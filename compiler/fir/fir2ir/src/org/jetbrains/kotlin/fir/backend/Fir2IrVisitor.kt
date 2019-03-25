/*
 * Copyright 2010-2019 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.fir.backend

import com.intellij.psi.PsiFile
import org.jetbrains.kotlin.backend.common.descriptors.*
import org.jetbrains.kotlin.descriptors.*
import org.jetbrains.kotlin.fir.FirElement
import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.declarations.*
import org.jetbrains.kotlin.fir.declarations.impl.FirDefaultPropertyGetter
import org.jetbrains.kotlin.fir.declarations.impl.FirDefaultPropertySetter
import org.jetbrains.kotlin.fir.descriptors.FirModuleDescriptor
import org.jetbrains.kotlin.fir.expressions.*
import org.jetbrains.kotlin.fir.render
import org.jetbrains.kotlin.fir.symbols.ConeClassLikeLookupTagImpl
import org.jetbrains.kotlin.fir.types.FirTypeRef
import org.jetbrains.kotlin.fir.types.impl.ConeClassTypeImpl
import org.jetbrains.kotlin.fir.types.impl.FirResolvedTypeRefImpl
import org.jetbrains.kotlin.fir.visitors.FirVisitor
import org.jetbrains.kotlin.ir.IrElement
import org.jetbrains.kotlin.ir.IrStatement
import org.jetbrains.kotlin.ir.declarations.*
import org.jetbrains.kotlin.ir.declarations.impl.*
import org.jetbrains.kotlin.ir.expressions.*
import org.jetbrains.kotlin.ir.expressions.impl.*
import org.jetbrains.kotlin.ir.symbols.IrFieldSymbol
import org.jetbrains.kotlin.ir.symbols.IrFunctionSymbol
import org.jetbrains.kotlin.ir.symbols.IrPropertySymbol
import org.jetbrains.kotlin.ir.types.IrType
import org.jetbrains.kotlin.ir.types.impl.IrErrorTypeImpl
import org.jetbrains.kotlin.ir.types.impl.IrSimpleTypeImpl
import org.jetbrains.kotlin.ir.util.SymbolTable
import org.jetbrains.kotlin.ir.util.constructedClassType
import org.jetbrains.kotlin.ir.util.constructors
import org.jetbrains.kotlin.name.ClassId
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.psi.KtSecondaryConstructor
import org.jetbrains.kotlin.psi2ir.PsiSourceManager
import org.jetbrains.kotlin.types.Variance

class Fir2IrVisitor(
    private val session: FirSession,
    private val moduleDescriptor: FirModuleDescriptor,
    private val symbolTable: SymbolTable
) : FirVisitor<IrElement, Any?>() {
    companion object {
        private val KOTLIN = FqName("kotlin")
    }

    private val declarationStorage = Fir2IrDeclarationStorage(session, symbolTable, moduleDescriptor)

    private fun FqName.simpleType(name: String): IrType =
        FirResolvedTypeRefImpl(
            session, null,
            ConeClassTypeImpl(
                ConeClassLikeLookupTagImpl(
                    ClassId(this, Name.identifier(name))
                ),
                typeArguments = emptyArray(),
                isNullable = false
            ),
            isMarkedNullable = false,
            annotations = emptyList()
        ).toIrType(session, declarationStorage)

    private val nothingType = KOTLIN.simpleType("Nothing")

    private fun ModuleDescriptor.findPackageFragmentForFile(file: FirFile): PackageFragmentDescriptor =
        getPackage(file.packageFqName).fragments.first()

    private val parentStack = mutableListOf<IrDeclarationParent>()

    private fun <T : IrDeclarationParent> T.withParent(f: T.() -> Unit): T {
        parentStack += this
        f()
        parentStack.removeAt(parentStack.size - 1)
        return this
    }

    private fun <T : IrDeclaration> T.setParent(parent: IrDeclarationParent = parentStack.last()): T {
        this.parent = parent
        return this
    }

    private val functionStack = mutableListOf<IrSimpleFunction>()

    private fun <T : IrSimpleFunction> T.withFunction(f: T.() -> Unit): T {
        functionStack += this
        f()
        functionStack.removeAt(functionStack.size - 1)
        return this
    }

    private val propertyStack = mutableListOf<IrProperty>()

    private fun IrProperty.withProperty(f: IrProperty.() -> Unit): IrProperty {
        propertyStack += this
        f()
        propertyStack.removeAt(propertyStack.size - 1)
        return this
    }

    private val classStack = mutableListOf<IrClass>()

    private fun IrClass.withClass(f: IrClass.() -> Unit): IrClass {
        classStack += this
        f()
        classStack.removeAt(classStack.size - 1)
        return this
    }

    override fun visitElement(element: FirElement, data: Any?): IrElement {
        TODO("Should not be here")
    }

    override fun visitFile(file: FirFile, data: Any?): IrFile {
        return IrFileImpl(
            PsiSourceManager.PsiFileEntry(file.psi as PsiFile),
            moduleDescriptor.findPackageFragmentForFile(file)
        ).withParent {
            file.declarations.forEach {
                declarations += it.accept(this@Fir2IrVisitor, data) as IrDeclaration
            }

            file.annotations.forEach {
                annotations += it.accept(this@Fir2IrVisitor, data) as IrCall
            }
        }
    }

    private fun IrClass.setClassContent(klass: FirClass) {
        for (superTypeRef in klass.superTypeRefs) {
            superTypes += superTypeRef.toIrType(session, declarationStorage)
        }
        symbolTable.enterScope(descriptor)
        val thisOrigin = IrDeclarationOrigin.INSTANCE_RECEIVER
        val thisType = IrSimpleTypeImpl(symbol, false, emptyList(), emptyList())
        thisReceiver = symbolTable.declareValueParameter(
            startOffset, endOffset, thisOrigin, WrappedValueParameterDescriptor(), thisType
        ) { symbol ->
            IrValueParameterImpl(
                startOffset, endOffset, thisOrigin, symbol,
                Name.special("<this>"), -1, thisType,
                varargElementType = null, isCrossinline = false, isNoinline = false
            ).setParent()
        }
        withClass {
            klass.declarations.forEach {
                declarations += it.accept(this@Fir2IrVisitor, null) as IrDeclaration
            }
            klass.annotations.forEach {
                annotations += it.accept(this@Fir2IrVisitor, null) as IrCall
            }
        }
        symbolTable.leaveScope(descriptor)
    }

    override fun visitRegularClass(regularClass: FirRegularClass, data: Any?): IrElement {
        return declarationStorage.getIrClass(regularClass, setParent = false)
            .setParent()
            .withParent {
                setClassContent(regularClass)
            }
    }

    private fun <T : IrFunction> T.setContent(descriptor: FunctionDescriptor, firFunction: FirFunction): T {
        setParent()
        withParent {
            symbolTable.enterScope(descriptor)
            val containingClass = classStack.lastOrNull()
            if (firFunction !is FirConstructor && containingClass != null) {
                val thisOrigin = IrDeclarationOrigin.DEFINED
                val thisType = containingClass.thisReceiver!!.type
                dispatchReceiverParameter = symbolTable.declareValueParameter(
                    startOffset, endOffset, thisOrigin, WrappedValueParameterDescriptor(),
                    thisType
                ) { symbol ->
                    IrValueParameterImpl(
                        startOffset, endOffset, thisOrigin, symbol,
                        Name.special("<this>"), -1, thisType,
                        varargElementType = null, isCrossinline = false, isNoinline = false
                    ).setParent()
                }
            }
            if (firFunction !is FirDefaultPropertySetter) {
                for ((index, valueParameter) in firFunction.valueParameters.withIndex()) {
                    valueParameters += valueParameter.accept(this@Fir2IrVisitor, index) as IrValueParameter
                }
            }
            body = firFunction.body?.accept(this@Fir2IrVisitor, null) as IrBody?
            symbolTable.leaveScope(descriptor)
        }
        return this
    }

    override fun visitConstructor(constructor: FirConstructor, data: Any?): IrElement {
        val ktConstructor = constructor.psi
        val descriptor = WrappedClassConstructorDescriptor()
        val origin = IrDeclarationOrigin.DEFINED
        val isPrimary = ktConstructor !is KtSecondaryConstructor
        return constructor.convertWithOffsets { startOffset, endOffset ->
            symbolTable.declareConstructor(startOffset, endOffset, origin, descriptor) { symbol ->
                IrConstructorImpl(
                    startOffset, endOffset, origin, symbol,
                    constructor.name, constructor.visibility,
                    constructor.returnTypeRef.toIrType(session, declarationStorage),
                    false, false, isPrimary
                ).setContent(descriptor, constructor).apply {
                    if (isPrimary) {
                        val body = this.body as IrBlockBody? ?: IrBlockBodyImpl(startOffset, endOffset)
                        val delegatedConstructor = constructor.delegatedConstructor
                        if (delegatedConstructor != null) {
                            //body.statements += delegatedConstructor.accept(this@Fir2IrVisitor, null) as IrStatement
                        }
                        val irClass = parent as IrClass
                        body.statements += IrInstanceInitializerCallImpl(
                            startOffset, endOffset, irClass.symbol, constructedClassType
                        )
                        this.body = body
                    }
                }
            }
        }
    }

    override fun visitAnonymousInitializer(anonymousInitializer: FirAnonymousInitializer, data: Any?): IrElement {
        val origin = IrDeclarationOrigin.DEFINED
        val parent = parentStack.last() as IrClass
        return anonymousInitializer.convertWithOffsets { startOffset, endOffset ->
            symbolTable.declareAnonymousInitializer(
                startOffset, endOffset, origin, parent.descriptor
            ).apply {
                symbolTable.enterScope(descriptor)
                body = anonymousInitializer.body!!.accept(this@Fir2IrVisitor, null) as IrBlockBody
                symbolTable.leaveScope(descriptor)
            }
        }
    }

    override fun visitDelegatedConstructorCall(delegatedConstructorCall: FirDelegatedConstructorCall, data: Any?): IrElement {
        val constructedTypeRef = delegatedConstructorCall.constructedTypeRef
        val constructedIrType = constructedTypeRef.toIrType(session, declarationStorage)
        TODO()
//        return IrDelegatingConstructorCallImpl(
//            startOffset, endOffset,
//            constructedIrType
//        )
    }

    override fun visitNamedFunction(namedFunction: FirNamedFunction, data: Any?): IrElement {
        val irFunction = declarationStorage.getIrFunction(namedFunction)
        return irFunction.withFunction { setContent(irFunction.descriptor, namedFunction) }
    }

    override fun visitValueParameter(valueParameter: FirValueParameter, data: Any?): IrElement {
        val descriptor = WrappedValueParameterDescriptor()
        val origin = IrDeclarationOrigin.DEFINED
        val type = valueParameter.returnTypeRef.toIrType(session, declarationStorage)
        return valueParameter.convertWithOffsets { startOffset, endOffset ->
            symbolTable.declareValueParameter(
                startOffset, endOffset, origin, descriptor, type
            ) { symbol ->
                IrValueParameterImpl(
                    startOffset, endOffset, origin, symbol,
                    valueParameter.name, data as Int, type,
                    null, valueParameter.isCrossinline, valueParameter.isNoinline
                )
            }
        }
    }

    override fun visitProperty(property: FirProperty, data: Any?): IrProperty {
        val descriptor = WrappedPropertyDescriptor()
        val origin = IrDeclarationOrigin.DEFINED
        return property.convertWithOffsets { startOffset, endOffset ->
            symbolTable.declareProperty(
                startOffset, endOffset,
                origin, descriptor, property.delegate != null
            ) { symbol ->
                IrPropertyImpl(
                    startOffset, endOffset, origin, symbol,
                    property.name, property.visibility, property.modality!!,
                    property.isVar, property.isConst, property.isLateInit,
                    property.delegate != null,
                    // TODO
                    isExternal = false
                )
            }.setParent().withProperty {
                descriptor.bind(this)
                val initializer = property.initializer
                // TODO: this check is very preliminary, FIR resolve should determine backing field presence itself
                if (initializer != null || property.getter is FirDefaultPropertyGetter ||
                    property.isVar && property.setter is FirDefaultPropertySetter
                ) {
                    val backingOrigin = IrDeclarationOrigin.PROPERTY_BACKING_FIELD
                    val type = property.returnTypeRef.toIrType(session, declarationStorage)
                    backingField = symbolTable.declareField(
                        startOffset, endOffset, backingOrigin, descriptor, type
                    ) { symbol ->
                        IrFieldImpl(
                            startOffset, endOffset, backingOrigin, symbol,
                            property.name, type, property.visibility,
                            isFinal = property.isVal, isExternal = false, isStatic = false
                        )
                    }.apply {
                        val initializerExpression = initializer?.accept(this@Fir2IrVisitor, data) as IrExpression?
                        this.initializer = initializerExpression?.let { IrExpressionBodyImpl(it) }
                    }
                }
                getter = property.getter.accept(this@Fir2IrVisitor, data) as IrSimpleFunction
                if (property.isVar) {
                    setter = property.setter.accept(this@Fir2IrVisitor, data) as IrSimpleFunction
                }
                property.annotations.forEach {
                    annotations += it.accept(this@Fir2IrVisitor, data) as IrCall
                }
            }
        }
    }

    private fun IrFieldAccessExpression.setReceiver(declaration: IrDeclaration): IrFieldAccessExpression {
        if (declaration is IrFunction) {
            val dispatchReceiver = declaration.dispatchReceiverParameter
            if (dispatchReceiver != null) {
                receiver = IrGetValueImpl(startOffset, endOffset, dispatchReceiver.symbol)
            }
        }
        return this
    }

    private fun createPropertyAccessor(
        propertyAccessor: FirPropertyAccessor, startOffset: Int, endOffset: Int,
        correspondingProperty: IrProperty, isDefault: Boolean
    ): IrSimpleFunction {
        val origin = when {
            isDefault -> IrDeclarationOrigin.DEFAULT_PROPERTY_ACCESSOR
            else -> IrDeclarationOrigin.DEFINED
        }
        val isSetter = propertyAccessor.isSetter
        val prefix = if (isSetter) "set" else "get"
        val descriptor = WrappedSimpleFunctionDescriptor()
        return symbolTable.declareSimpleFunction(
            startOffset, endOffset, origin, descriptor
        ) { symbol ->
            val accessorReturnType = propertyAccessor.returnTypeRef.toIrType(session, declarationStorage)
            IrFunctionImpl(
                startOffset, endOffset, origin, symbol,
                Name.special("<$prefix-${correspondingProperty.name}>"),
                propertyAccessor.visibility, correspondingProperty.modality, accessorReturnType,
                isInline = false, isExternal = false, isTailrec = false, isSuspend = false
            ).setContent(descriptor, propertyAccessor).apply {
                correspondingPropertySymbol = symbolTable.referenceProperty(correspondingProperty.descriptor)
                if (isDefault) {
                    withParent {
                        symbolTable.enterScope(descriptor)
                        val propertyType = correspondingProperty.backingField!!.type
                        if (isSetter) {
                            valueParameters += symbolTable.declareValueParameter(
                                startOffset, endOffset, origin, WrappedValueParameterDescriptor(), propertyType
                            ) { symbol ->
                                IrValueParameterImpl(
                                    startOffset, endOffset, IrDeclarationOrigin.DEFINED, symbol,
                                    Name.special("<set-?>"), 0, propertyType,
                                    varargElementType = null,
                                    isCrossinline = false, isNoinline = false
                                ).setParent()
                            }
                        }
                        val fieldSymbol = symbolTable.referenceField(correspondingProperty.descriptor)
                        val declaration = this
                        body = IrBlockBodyImpl(
                            startOffset, endOffset,
                            listOf(
                                if (isSetter) {
                                    IrSetFieldImpl(startOffset, endOffset, fieldSymbol, accessorReturnType).apply {
                                        setReceiver(declaration)
                                        value = IrGetValueImpl(startOffset, endOffset, propertyType, valueParameters.first().symbol)
                                    }
                                } else {
                                    IrReturnImpl(
                                        startOffset, endOffset, nothingType, symbol,
                                        IrGetFieldImpl(startOffset, endOffset, fieldSymbol, propertyType).setReceiver(declaration)
                                    )
                                }
                            )
                        )
                        symbolTable.leaveScope(descriptor)
                    }
                }
            }
        }
    }

    override fun visitPropertyAccessor(propertyAccessor: FirPropertyAccessor, data: Any?): IrElement {
        val correspondingProperty = propertyStack.last()
        return propertyAccessor.convertWithOffsets { startOffset, endOffset ->
            createPropertyAccessor(
                propertyAccessor, startOffset, endOffset, correspondingProperty,
                isDefault = propertyAccessor is FirDefaultPropertyGetter || propertyAccessor is FirDefaultPropertySetter
            )
        }
    }

    override fun visitBlock(block: FirBlock, data: Any?): IrElement {
        return block.convertWithOffsets { startOffset, endOffset ->
            IrBlockBodyImpl(
                startOffset, endOffset, block.statements.map { it.accept(this, data) as IrStatement }
            )
        }
    }

    override fun visitReturnExpression(returnExpression: FirReturnExpression, data: Any?): IrElement {
        val firTarget = returnExpression.target.labeledElement
        var irTarget = functionStack.last()
        for (potentialTarget in functionStack.asReversed()) {
            // TODO: remove comparison by name
            if (potentialTarget.name == (firTarget as? FirNamedFunction)?.name) {
                irTarget = potentialTarget
                break
            }
        }
        return returnExpression.convertWithOffsets { startOffset, endOffset ->
            IrReturnImpl(
                startOffset, endOffset, nothingType,
                symbolTable.referenceSimpleFunction(WrappedSimpleFunctionDescriptor().apply { bind(irTarget) }),
                returnExpression.result.accept(this, data) as IrExpression
            )
        }
    }

    private fun FirQualifiedAccess.toIrExpression(typeRef: FirTypeRef): IrExpression {
        val type = typeRef.toIrType(this@Fir2IrVisitor.session, declarationStorage)
        val symbol = calleeReference.toSymbol(declarationStorage)
        return typeRef.convertWithOffsets { startOffset, endOffset ->
            when {
                symbol is IrFunctionSymbol -> IrCallImpl(startOffset, endOffset, type, symbol)
                symbol is IrPropertySymbol && symbol.isBound -> IrCallImpl(startOffset, endOffset, type, symbol.owner.getter!!.symbol)
                else -> IrErrorCallExpressionImpl(startOffset, endOffset, type, "Unresolved reference: ${calleeReference.render()}")
            }
        }
    }

    override fun visitFunctionCall(functionCall: FirFunctionCall, data: Any?): IrElement {
        return when (val irCall = functionCall.toIrExpression(functionCall.typeRef)) {
            is IrCallImpl -> irCall.apply {
                for ((index, argument) in functionCall.arguments.withIndex()) {
                    val argumentExpression = argument.accept(this@Fir2IrVisitor, data) as IrExpression
                    putValueArgument(index, argumentExpression)
                }
            }
            is IrErrorCallExpressionImpl -> irCall.apply {
                for (argument in functionCall.arguments) {
                    addArgument(argument.accept(this@Fir2IrVisitor, data) as IrExpression)
                }
            }
            else -> irCall
        }
    }

    override fun visitQualifiedAccessExpression(qualifiedAccessExpression: FirQualifiedAccessExpression, data: Any?): IrElement {
        return qualifiedAccessExpression.toIrExpression(qualifiedAccessExpression.typeRef)
    }

    override fun visitVariableAssignment(variableAssignment: FirVariableAssignment, data: Any?): IrElement {
        val calleeReference = variableAssignment.calleeReference
        val symbol = calleeReference.toSymbol(declarationStorage)
        return variableAssignment.convertWithOffsets { startOffset, endOffset ->
            if (symbol is IrFieldSymbol && symbol.isBound) {
                IrSetFieldImpl(
                    startOffset, endOffset, symbol, symbol.owner.type
                ).apply {
                    value = variableAssignment.rValue.accept(this@Fir2IrVisitor, data) as IrExpression
                }
            } else {
                IrErrorCallExpressionImpl(
                    startOffset, endOffset, IrErrorTypeImpl(null, emptyList(), Variance.INVARIANT),
                    "Unresolved reference: ${calleeReference.render()}"
                )
            }
        }
    }

    override fun <T> visitConstExpression(constExpression: FirConstExpression<T>, data: Any?): IrElement {
        return constExpression.convertWithOffsets { startOffset, endOffset ->
            IrConstImpl(
                startOffset, endOffset,
                constExpression.typeRef.toIrType(session, declarationStorage),
                constExpression.kind, constExpression.value
            )
        }
    }

    override fun visitAnonymousObject(anonymousObject: FirAnonymousObject, data: Any?): IrElement {
        return anonymousObject.convertWithOffsets { startOffset, endOffset ->
            val descriptor = WrappedClassDescriptor()
            val origin = IrDeclarationOrigin.DEFINED
            val modality = Modality.FINAL
            val anonymousClass = symbolTable.declareClass(startOffset, endOffset, origin, descriptor, modality) { symbol ->
                IrClassImpl(
                    startOffset, endOffset, origin, symbol,
                    Name.special("<no name provided>"), anonymousObject.classKind,
                    Visibilities.LOCAL, modality,
                    isCompanion = false, isInner = false, isData = false, isExternal = false, isInline = false
                ).setParent().withParent {
                    setClassContent(anonymousObject)
                }
            }
            val anonymousClassType = anonymousClass.thisReceiver!!.type
            IrBlockImpl(
                startOffset, endOffset, anonymousClassType, IrStatementOrigin.OBJECT_LITERAL,
                listOf(
                    anonymousClass,
                    IrCallImpl(startOffset, endOffset, anonymousClassType, anonymousClass.constructors.first().symbol)
                )
            )
        }
    }
}