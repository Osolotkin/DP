#pragma once

#include "logger.h"
#include "error.h"
#include "syntax.h"
#include "utils.h"
#include "interpreter.h"

namespace Validator {

    int validateImplicitCast(const DataTypeEnum dtype, const DataTypeEnum dtypeRef);
    int validateAttributeCast(Variable* var, Variable* attribute);
    int validateTypeInitialization(TypeDefinition* dtype, TypeInitialization* dtypeInit);
    int validateTypeInitializations(TypeDefinition* dtype, Variable* var);
    
    int evaluateTypeInitialization(Variable* op, int attributesCount, TypeInitialization** tinitOut);
    int evaluateDataTypes(Variable* op, TypeDefinition** customDtype = NULL, DataTypeEnum lvalueType = DT_UNDEFINED, TypeDefinition* lvalueTypeDef = NULL);
    int evaluate(Variable* op, TypeDefinition** customDtype = NULL);

    void stripWrapperExpressions(Variable** op);
    void stripWrapperExpressions(Variable* op);

    void copy(Variable* dest, Variable* src);
    int copyExpression(Variable* src, Variable** dest, int* pidx, int* nextIdx);


    int validate() {

        Logger::log(Logger::INFO, "Validating...\n");

        // link user defined data types with definitions
        //

        for (int i = 0; i < (int) SyntaxNode::customDataTypesReferences.size(); i++) {

            VariableDefinition* const varDef = SyntaxNode::customDataTypesReferences[i];

            TypeDefinition* dtype = Utils::find<TypeDefinition>(varDef->scope, varDef->dtypeName, varDef->dtypeNameLen, &Scope::customDataTypes);
            if (!dtype) {

                Enumerator* en = Utils::find<Enumerator>(varDef->scope, varDef->dtypeName, varDef->dtypeNameLen, &Scope::enums);
                if (!en) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_DATA_TYPE), varDef->loc, varDef->dtypeNameLen);
                    return Err::UNKNOWN_DATA_TYPE;
                }

                varDef->var->cvalue.dtypeEnum = en->dtype;
                dtype = (TypeDefinition*) (dataTypes + en->dtype);
            
            }

            // if (varDef->var->expression) initializations.push_back(varDef);

            Pointer* ptr = NULL;
            int tdtypeEnum = varDef->var->cvalue.dtypeEnum;
            void* tdtype = varDef->var->dtype;
            while (tdtypeEnum == DT_POINTER || tdtypeEnum == DT_ARRAY) {
                ptr = (Pointer*) (tdtype);
                tdtypeEnum = ptr->pointsToEnum;
                tdtype = ptr->pointsTo;
            }

            if (ptr) {
                ptr->pointsTo = dtype;
            } else {
                varDef->var->dtype = dtype;
            }

        }



        // link variables with definitions
        // in case of scopeNames var is the last name (ex. point.x, var is then x)
        // scopeNames are sorted from left to right as written
        for (int i = 0; i < SyntaxNode::variables.size(); i++) {

            Variable* const var = SyntaxNode::variables[i];
            // const int scopeNamesLen = var->scopeNames.size();

            Scope* scope = var->scope;
            if (var->scopeNames.size() > 0) { 
                Namespace* nspace;
                const int err = validateScopeNames(var->scope, var->scopeNames, &nspace);
                if (err != Err::OK) return err;
                scope = nspace;
            }

            Variable* tmpVar = Utils::find<Variable>(scope, var->name, var->nameLen, &Scope::vars);
            if (!tmpVar) {

                Enumerator* en = Utils::find<Enumerator>(scope, var->name, var->nameLen, &Scope::enums);
                if (en) {
                    var->cvalue.dtypeEnum = DT_ENUM;
                    var->dtype = en;
                    continue;
                }

                tmpVar = Utils::find(internalVariables, internalVariablesCount, var->name, var->nameLen);
                if (!tmpVar) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_VARIABLE), var->loc, var->nameLen, var->nameLen, var->name);
                    return Err::UNKNOWN_VARIABLE;
                }
            
            }

            copy(var, tmpVar);
        
        }



        // link function calls with definitions
        for (int i = 0; i < (int) SyntaxNode::fcnCalls.size(); i++) {

            Variable* fcnCallOp = SyntaxNode::fcnCalls[i];
            FunctionCall* fcnCall = (FunctionCall*) (fcnCallOp->expression);

            if (fcnCall->fcn) continue;

            Scope* fcnCallScope = fcnCallOp->scope;
            if (fcnCall->scopeNames.size() > 0) {
                const int err = validateScopeNames(fcnCallOp->scope, fcnCall->scopeNames, (Namespace**) &fcnCallScope);
                if (err != Err::OK) return err;
            }

            Function* fcn = Utils::find<Function>(fcnCallScope, fcnCall->name, fcnCall->nameLen, &Scope::fcns);
            if (!fcn) {
                Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_FUNCTION), fcnCallOp->loc, fcnCall->nameLen, fcnCall->nameLen, fcnCall->name);
                return Err::UNKNOWN_FUNCTION;
            }

            fcnCall->fcn = fcn;
            fcnCall->outArg = new Variable();
            fcnCall->outArg->cvalue.hasValue = 0;
            if (fcn->outArgs.size() > 0) {
                fcnCall->outArg->cvalue.dtypeEnum = fcn->outArgs[0];
            } else {
                fcnCall->outArg->cvalue.dtypeEnum = DT_VOID;
            }

        }

        // Compute all cmptime vars
        //
        
        for (int i = 0; i < (int) SyntaxNode::cmpTimeVars.size(); i++) {

            Variable* var = SyntaxNode::cmpTimeVars[i];

            if (!(var->expression)) continue;
            
            const int err = evaluate(var);
            if (err < Err::OK) return err;
            
        }



        // compute arrays length
        //

        for (int i = 0; i < (int) SyntaxNode::arrays.size(); i++) {

            Variable* var = SyntaxNode::arrays[i];
            Array* arr = var->cvalue.arr;

            // var should directly represent definiton
            // therefore var->expression should be either by value initialization or allocation

            // arr->flags : info about length
            // var should directly represent definiton
            // therefore var->expression should be either by value initialization or allocation

            if (var->expression) {

                if (var->expression->type == EXT_FUNCTION_CALL) {
                    // alloc

                    Variable* alloc = ((FunctionCall*)(var->expression))->inArgs[0];

                    if (arr->flags & IS_CMP_TIME) {
                        Logger::log(Logger::ERROR, "TODO error: invalid use of alloc!");
                        return Err::CANNOT_EVALUATE_EXPRESION_AT_CMP_TIME;
                    }

                    if (!(arr->flags & IS_CONST)) {
                        if (arr->length->cvalue.hasValue) {
                            if (arr->length->cvalue.i64 != alloc->cvalue.arr->length->cvalue.i64) {
                                Logger::log(Logger::ERROR, "TODO error: array and alloc size missmatch, you can omit array size while initializationg array with rvalue...");
                                return Err::INVALID_LVALUE;
                            }
                        }
                        arr->flags |= IS_ARRAY_LIST;
                        var->def->flags |= IS_ARRAY_LIST;
                    }

                    var->cvalue.arr->length = alloc->cvalue.arr->length;
                    arr->flags |= IS_ALLOCATED;

                } else {
                    // init

                    // Variable* tmp = var;
                    // stripWrapperExpressions(&var);
                    stripWrapperExpressions(var);
                    Variable* tmp = var;

                    int len;
                    if (tmp->expression && tmp->expression->type == EXT_ARRAY_INITIALIZATION) {
                        
                        len = ((ArrayInitialization*) (tmp->expression))->attributes.size();

                        if (arr->length->cvalue.hasValue && arr->length->cvalue.i64 != len) {
                            Logger::log(Logger::ERROR, "TODO error: array and alloc size missmatch, you can omit array size while initializationg array with rvalue...");
                            return Err::INVALID_LVALUE;
                        }
                    
                    } else if (tmp->expression && tmp->expression->type == EXT_SLICE) {

                        Slice* slice = (Slice*) tmp->expression;
                        
                        evaluate(slice->eidx);
                        evaluate(slice->bidx);
                        
                        Value ans = slice->eidx->cvalue;
                        Interpreter::applyOperator(OP_SUBTRACTION, &ans, &(slice->bidx->cvalue));
                        
                        len = ans.i64 + 1;

                    } else {

                        Logger::log(Logger::ERROR, "TODO error: unexpected expression...");
                        return Err::UNEXPECTED_SYMBOL;

                    }

                    var->cvalue.arr->length->cvalue.i64 = len;
                    var->cvalue.arr->length->cvalue.dtypeEnum = DT_INT_64;
                    var->cvalue.arr->pointsTo = NULL;
                    var->cvalue.hasValue = 1;

                }

                continue;

            }

            if (arr->flags & IS_CONST || arr->flags & IS_CMP_TIME) {
                if (!(arr->length) || !(arr->length->expression)) {
                    arr->flags |= IS_ALLOCATED;
                }
                continue;
            }

            if (!(arr->length) || !(arr->length->expression)) {
                arr->flags |= IS_ARRAY_LIST | IS_ALLOCATED;
                var->def->flags |= IS_ARRAY_LIST;
                continue;
            }

            const int err = evaluate(arr->length);
            if (err < Err::OK) return err;

        }

        // link function calls with definitions, verify arguments etc.
        for (int i = 0; i < (int) SyntaxNode::fcnCalls.size(); i++) {

            Variable* fcnCallOp = SyntaxNode::fcnCalls[i];
            FunctionCall* fcnCall = (FunctionCall*) (fcnCallOp->expression);
            Function* fcn = fcnCall->fcn;

            const int fcnInCount = fcn->inArgs.size();
            // const int fcnCallInCount = fcnCall->inArgs.size();

            const int variableNumberOfArguments = fcnInCount > 0 && fcn->inArgs[fcnInCount - 1]->var->cvalue.dtypeEnum == DT_MULTIPLE_TYPES;

            // note: 
            //  array argument is parsed as two arguments (pointer and length) in definition

            int j = 0;
            for (int i = 0; i < fcnInCount - variableNumberOfArguments; i++) {

                if (j >= fcnCall->inArgs.size()) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::NOT_ENOUGH_ARGUMENTS), fcnCallOp->loc, fcnCall->nameLen);
                    return Err::NOT_ENOUGH_ARGUMENTS;
                }

                VariableDefinition* fcnVarDef = fcn->inArgs[i];
                Variable* fcnVar = fcnVarDef->var;
                Variable* fcnCallVar = fcnCall->inArgs[j];

                int fcnCallVarDtype;
                if (fcnCallVar->expression) {
                    // TODOD : TypeDefinition** customDtype, DataTypeEnum lvalueType, TypeDefinition* lvalueTypeDef
                    fcnCallVarDtype = evaluateDataTypes(fcnCallVar);
                    if (fcnCallVarDtype < Err::OK) return fcnCallVarDtype;
                } else {
                    fcnCallVarDtype = fcnCallVar->cvalue.dtypeEnum;
                }
                
                if (fcnVar->cvalue.dtypeEnum == DT_ARRAY) {
                    
                    if (!(fcnCallVar->cvalue.dtypeEnum == DT_ARRAY)) {
                        Logger::log(Logger::ERROR, ERR_STR(Err::ARRAY_EXPECTED), fcnCallVar->loc, 1);
                        return Err::ARRAY_EXPECTED;
                    }
                    
                    Variable* var = fcnCallVar;
                    stripWrapperExpressions((Variable**) & var);
                    if (!(var->def) || !(var->def->var->cvalue.arr)) {
                        Logger::log(Logger::ERROR, "TODO error: array expected!");
                        return Err::ARRAY_EXPECTED;
                    }

                    Array* arr = var->def->var->cvalue.arr;
                    if (arr->flags & IS_ARRAY_LIST) {
                        i++;
                        j++;
                        continue;
                    }

                    // fcnVar->cvalue.arr->length = arr->length;
                    fcnCall->inArgs.insert(fcnCall->inArgs.begin() + j + 1, arr->length); // fcnCallVar->def->var->cvalue.arr->length);
                    // j++;
                
                }
                
                if (!validateImplicitCast((DataTypeEnum) fcnCallVarDtype, fcnVar->cvalue.dtypeEnum)) {
                    // error : cannot cast
                    Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_TYPE_CONVERSION), fcnCallVar->loc, 1, (dataTypes + fcnVar->cvalue.dtypeEnum)->name, (dataTypes + fcnCallVarDtype)->name);
                    return Err::INVALID_TYPE_CONVERSION;
                }

                j++;
            
            }

            const int fcnCallInCount = fcnCall->inArgs.size();

            if (j < fcnCallInCount && !variableNumberOfArguments) {
                Logger::log(Logger::ERROR, ERR_STR(Err::TOO_MANY_ARGUMENTS), SyntaxNode::fcnCalls[i]->loc, fcnCall->nameLen);
                return Err::TOO_MANY_ARGUMENTS;
            }
            
            for (; j < fcnCallInCount; j++) {
                
                Variable* var = fcnCall->inArgs[j];
                
                int err;
                if (var->def) {
                    // TODO : ALLOC case for now
                    
                    Variable* tmp = var->def->var;
                    const DataTypeEnum tmpDtype = tmp->cvalue.dtypeEnum; 

                    err = evaluateDataTypes(var, NULL, tmp->cvalue.dtypeEnum, (TypeDefinition*) tmp->dtype);

                    if (tmp->cvalue.dtypeEnum == DT_CUSTOM) {
                        validateTypeInitialization((TypeDefinition*) (tmp->dtype), (TypeInitialization*) ((WrapperExpression*) (var->expression))->operand->expression);
                    } else {
                        if (!validateImplicitCast(var->cvalue.dtypeEnum, tmpDtype)) {
                            Logger::log(Logger::ERROR, "TODO error : DT_MULTIPLE_TYPES check in function call, wrong dtype...");
                            return Err::INVALID_DATA_TYPE;
                        }
                        tmp->cvalue.dtypeEnum = tmpDtype;
                    }

                } else {
                    err = evaluateDataTypes(var);
                }

                if (err < 0) return err;
            
            }

            fcnCall->fcn = fcn;
        
        }

        // validate return statements

        for (int i = 0; i < SyntaxNode::returnStatements.size(); i++) {

            ReturnStatement* rt = SyntaxNode::returnStatements[i];
            Function* fc = rt->fcn;

            if (!fc) {
                Logger::log(Logger::ERROR, "Unexpected return statement outside of the function!", rt->loc);
                return Err::UNEXPECTED_SYMBOL;
            }

            int j = 0;
            for (int i = 0; i < fc->outArgs.size(); i++) {
                
                if (j >= rt->vars.size()) {
                    Logger::log(Logger::ERROR, "Not enough return arguments!", rt->loc);
                    return Err::NOT_ENOUGH_ARGUMENTS;
                }

                Variable* var = rt->vars[j];
                const int rtType = evaluateDataTypes(var);
                if (rtType < 0) return rtType;

                if (!validateImplicitCast((DataTypeEnum) rtType, fc->outArgs[i])) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_TYPE_CONVERSION), rt->loc, 1, (dataTypes + rtType)->name, (dataTypes + fc->outArgs[i])->name);
                    return Err::INVALID_TYPE_CONVERSION;
                }

                j++;
            
            }

            if (j < rt->vars.size()) {
                Logger::log(Logger::ERROR, "More than enough return arguments, slow down!", rt->loc);
                return Err::TOO_MANY_ARGUMENTS;
            }
        
        }



        // var assignments
        //

        // so, if lvalue is
        //  1) just variable, then, after evaluation, variable is the answer,
        //  2) expression, then, after evaluation, the first expression has to be unary and operator be 'get value' or binary and operator be 'subscript'
        for (int i = 0; i < SyntaxNode::variableAssignments.size(); i++) {
            
            VariableAssignment* const varAss = SyntaxNode::variableAssignments[i];

            TypeDefinition* dtypeLDef = NULL;
            
            DataTypeEnum dtypeL = (DataTypeEnum) evaluateDataTypes(varAss->lvar, &dtypeLDef);
            if (dtypeL < Err::OK) return dtypeL;

            stripWrapperExpressions(&(varAss->lvar));
            //stripWrapperExpressions((varAss->lvar));

            Expression* lvalueEx = (varAss->lvar)->expression;

            // TODO : exclude binary expression check?
            //
            //
            if (!(
                    !(lvalueEx) || 
                    (lvalueEx->type == EXT_UNARY && ((UnaryExpression*) lvalueEx)->operType == OP_GET_VALUE) ||
                    (lvalueEx->type == EXT_BINARY && (((UnaryExpression*) lvalueEx)->operType == OP_SUBSCRIPT || ((UnaryExpression*) lvalueEx)->operType == OP_MEMBER_SELECTION)) ||
                    (lvalueEx->type == EXT_SLICE)
                )
            ) {
                Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_LVALUE), varAss->loc, varAss->loc->idx);// - varAss->lvar->loc->idx);
                return Err::INVALID_LVALUE;
            }

            if (varAss->lvar->def && varAss->lvar->def->flags & IS_CONST) {
                Logger::log(Logger::ERROR, ERR_STR(Err::CANNOT_ASSIGN_TO_CONST), varAss->loc, 1);
                return Err::CANNOT_ASSIGN_TO_CONST;
            }

            stripWrapperExpressions(&(varAss->rvar));

            DataTypeEnum dtypeR = (DataTypeEnum) evaluateDataTypes(varAss->rvar, NULL, dtypeL, dtypeLDef);
            if (dtypeR < 0) return dtypeR;

            if (dtypeL == DT_CUSTOM) {

                validateTypeInitializations(dtypeLDef, varAss->rvar);

                TypeInitialization* tinit;
                const int err = evaluateTypeInitialization(varAss->rvar, dtypeLDef->vars.size(), &tinit);

                // int err = validateCustomTypeInitialization(dtypeLDef, tinit);
                varAss->rvar->expression = tinit;

                if (err < 0) return err;

                continue;
            
            }

            if (lvalueEx && lvalueEx->type == EXT_SLICE && dtypeR != DT_ARRAY) {
                dtypeL = ((Slice*) lvalueEx)->arr->cvalue.arr->pointsToEnum;
            }

            if (!validateImplicitCast(dtypeR, dtypeL)) { //if (((dtypeL != dtypeR) && (dtypeL >= DT_POINTER || dtypeR >= DT_POINTER)) && (dtypeL != DT_POINTER && dtypeR != DT_ARRAY)) {
                // error : cannot cast
                Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_TYPE_CONVERSION), varAss->loc, 1, dataTypes[dtypeL].name, dataTypes[dtypeR].name);
                return Err::INVALID_TYPE_CONVERSION;
            }

        }



        // validationg loops
        for (int i = 0; i < SyntaxNode::loops.size(); i++) {
            Loop* loop = SyntaxNode::loops[i];
            DataTypeEnum dtype = (DataTypeEnum) evaluate(loop->array);
            if (dtype < Err::OK) return dtype;
            if (dtype != DT_ARRAY) {
                return Err::INVALID_DATA_TYPE;
            }
        }



        // initizlizations
        for (int i = 0; i < SyntaxNode::initializations.size(); i++) {

            VariableDefinition* const varDef = SyntaxNode::initializations[i];
            Variable* op = varDef->var;
            DataTypeEnum dtEnum = op->cvalue.dtypeEnum;
            void* dt = op->dtype;

            int dtype = evaluateDataTypes(op, NULL, op->cvalue.dtypeEnum, (TypeDefinition*) (op->dtype));
            if (dtype < 0) return dtype;

            // validateCustomTypeInitialization(dtype, dtypeInit);
            if (!validateImplicitCast((DataTypeEnum) dtype, dtEnum)) {
                Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_DATA_TYPE), varDef->loc, 1);
                return Err::INVALID_DATA_TYPE;                
            }

            op->cvalue.dtypeEnum = dtEnum;
            op->dtype = dt;

        }

        // branch expressions
        for (int i = 0; i < SyntaxNode::branchExpressions.size(); i++) {

            Variable* op = SyntaxNode::branchExpressions[i];
            int dtype = evaluateDataTypes(op);
            if (IS_INT(dtype)) continue;

            Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_DATA_TYPE), op->loc, 1);
            return Err::INVALID_DATA_TYPE;

        }

        // switch cases
        for (int i = 0; i < SyntaxNode::switchCases.size(); i++) {
            
            SwitchCase* const sw = SyntaxNode::switchCases[i];

            for (int i = 0; i < sw->casesExp.size(); i++) {
                
                int dtype = evaluate(sw->casesExp[i]);
                if (dtype < 0) return dtype;

            }

        }

        // statements
        for (int i = 0; i < SyntaxNode::statements.size(); i++) {

            Statement* st = SyntaxNode::statements[i];
            Variable* op = st->op;
            int dtype = evaluateDataTypes(op);
            if (dtype < 0) return dtype; 

        }

    }

    // the main thing is to glue initializations together
    // lvalueSize : number of elements
    // main initialization is the first one
    int processRValue(Variable* rvalue, int lvalueSize) {

        return Err::OK;
    
    }

    // TODO : to makro or constexpr
    int validateImplicitCast(const DataTypeEnum dtype, const DataTypeEnum dtypeRef) {

        const int basicTypes = (dtype != DT_VOID && dtypeRef != DT_VOID) && (dtype < DT_STRING && dtypeRef < DT_STRING);
        const int arrayToPointer = (dtype == DT_ARRAY && dtypeRef == DT_POINTER);
        const int pointerToArray = (dtypeRef == DT_ARRAY && dtype == DT_POINTER);
        const int sliceToArray = dtype == DT_SLICE && dtypeRef == DT_ARRAY;

        return (dtype == dtypeRef) || (basicTypes || arrayToPointer || pointerToArray || sliceToArray);

    }

    // used within validateTypeInitialization
    // think about better name
    int validateAttributeCast(Variable* var, Variable* attribute) {

        const int attributeDtype = evaluateDataTypes(attribute);
        if (attributeDtype == DT_CUSTOM) {

            if (var->cvalue.dtypeEnum != DT_CUSTOM) {
                // TODO : error;
                return Err::INVALID_DATA_TYPE;
            }

            if (!var->dtype) {
                // if no dtype, we have to find appropriate dtype and 'cash it'

                TypeDefinition *dtype = Utils::find<TypeDefinition>(var->scope, var->def->dtypeName, var->def->dtypeNameLen, &Scope::customDataTypes);
                if (!dtype) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_DATA_TYPE), var->def->loc, var->def->dtypeNameLen);
                    return Err::UNKNOWN_DATA_TYPE;
                }

                var->dtype = dtype;

            }

            const int err = validateTypeInitializations((TypeDefinition*) var->dtype, attribute);
            if (err < 0) return err;

        } else if (attributeDtype >= DT_MULTIPLE_TYPES) {

            Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_DATA_TYPE), attribute->loc, attribute->nameLen);
            return Err::INVALID_DATA_TYPE;

        } else {
            // basicly, if its any standard type, we should be able to do a cast

        }

        return Err::OK;
    
    }

    // assuming dtypeInit has at least one attribute
    // both TypeDefinition  has to be valid
    int validateTypeInitialization(TypeDefinition* dtype, TypeInitialization* dtypeInit) {
        
        if (dtype->vars.size() < dtypeInit->attributes.size()) {
            Logger::log(Logger::ERROR, ERR_STR(Err::TYPE_INIT_ATTRIBUTES_COUNT_MISSMATCH), dtype->loc, dtype->nameLen, dtypeInit->attributes.size(), dtype->vars.size());
            return Err::TYPE_INIT_ATTRIBUTES_COUNT_MISSMATCH;
        }

        const int count = dtype->vars.size();
        if (dtypeInit->attributes[0]->name) {
            // treating all as named
            
            for (int i = 0; i < dtypeInit->attributes.size(); i++) {

                Variable* attribute = dtypeInit->attributes[i];

                const int idx = Utils::findIdx(dtype->vars, attribute->name, attribute->nameLen);
                if (idx < 0) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_ATTRIBUTE_NAME), attribute->loc, attribute->nameLen, attribute->nameLen, attribute->name);
                    return Err::INVALID_ATTRIBUTE_NAME;    
                }

                dtypeInit->idxs[i] = idx;

                // typecheck
                if (!attribute->expression) continue;

                const int err = validateAttributeCast(dtype->vars[idx], attribute);
                if (err < 0) return err;
                
            }

            return Err::OK;
        
        }

        for (int i = 0; i < dtypeInit->attributes.size(); i++) {

            Variable* attribute = dtypeInit->attributes[i];

            dtypeInit->idxs[i] = i;

            // typecheck
            if (!attribute->expression) continue;

            const int err = validateAttributeCast(dtype->vars[i], attribute);
            if (err < 0) return err;
        
        }


    }

    int validateTypeInitializations(TypeDefinition* dtype, Variable* var) {

        Expression* ex = var->expression;
        if (!ex) {
            return Err::OK;
        }

        switch (ex->type) {

            case EXT_UNARY : {
                validateTypeInitializations(dtype, (Variable*) ((UnaryExpression*) ex)->operand);
                break;
            }

            case EXT_BINARY : {
                validateTypeInitializations(dtype, (Variable*) ((BinaryExpression*) ex)->operandA);
                validateTypeInitializations(dtype, (Variable*) ((BinaryExpression*) ex)->operandB);
                break;
            }

            case EXT_TERNARY : {
                break;
            }

            case EXT_WRAPPER : {
                validateTypeInitializations(dtype, (Variable*) ((WrapperExpression*)ex)->operand);
                break;
            }

            case EXT_FUNCTION_CALL : {
                break;
            }

            case EXT_TYPE_INITIALIZATION : {

                TypeInitialization* tinit = (TypeInitialization*) ex;
                
                int err = validateTypeInitialization(dtype, tinit);
                if (err < 0) return err;

            }

        }

        return Err::OK;

    }



    // makes sure, that first expression is meaningfull
    // ex.: ((1 + 2)) -> 1 + 2
    void stripWrapperExpressions(Variable** op) {

        const Expression* const ex = (*op)->expression;
        if (ex->type == EXT_WRAPPER) {
            Variable* tmp = ((WrapperExpression*) ex)->operand;
            if (tmp->expression) {
                (*op)->expression = tmp->expression;
            } else {
                *op = tmp;
            }
        }

    }

    void stripWrapperExpressions(Variable* op) {

        const Expression* const ex = op->expression;
        if (ex->type == EXT_WRAPPER) {
            op->expression = ((WrapperExpression*) ex)->operand->expression;
        }

    }



    int evaluateTypeInitialization(Variable* op, int attributesCount, TypeInitialization** tinitOut) {

        TypeInitialization* tinit = new TypeInitialization();
        
        int idx = -1;
        int nextIdx = -1;
        for (int i = 0; i < attributesCount; i++) {

            Variable* tmp = NULL;
            int err = copyExpression((Variable*) op, &tmp, &idx, &nextIdx);
            if (err < 0) return err;

            idx = nextIdx;
            nextIdx = -1;

            err = evaluateDataTypes(tmp);
            if (err < 0) { 
                Logger::log(Logger::ERROR, "TODO : Error, evaluateTypeInitialization -> evaluateDataTypes failed");
                return err;
            }
            
            tinit->attributes.push_back(tmp);
            tmp->name = NULL;
        
        }

        *tinitOut = tinit;

        return Err::OK;
        
    }



    int evaluateDataTypes(Variable* op, TypeDefinition** customDtype, DataTypeEnum lvalueType, TypeDefinition* lvalueTypeDef) {

        Expression* ex = op->expression;
        if (!ex) {

            if (op->cvalue.dtypeEnum == DT_CUSTOM && customDtype) {
                *customDtype = ((TypeDefinition*) (op->dtype));            
            } else if (customDtype) {
                *customDtype = (TypeDefinition*)(op->cvalue.any);
            }

            return op->cvalue.dtypeEnum;
        
        }
        
        int rdtype = DT_UNDEFINED;

        switch (ex->type) {
            
            case EXT_UNARY: {

                UnaryExpression* uex = (UnaryExpression*) ex;
                
                const int dtype = evaluateDataTypes(uex->operand, customDtype);
                if (dtype < Err::OK) return dtype;
                
                switch (uex->operType) {
                    
                    case OP_GET_ADDRESS:
                        op->cvalue.ptr = uex->operand->cvalue.ptr;
                        rdtype = DT_POINTER;
                        break;

                    case OP_GET_VALUE: 
                        op->cvalue.any = uex->operand->cvalue.ptr->pointsTo;
                        rdtype = uex->operand->cvalue.ptr->pointsToEnum;
                        break;

                    default:
                        op->cvalue.any = uex->operand->cvalue.any;
                        rdtype = dtype;

                }

                break;

            }

            case EXT_BINARY: {
                
                BinaryExpression* bex = (BinaryExpression*) ex;

                TypeDefinition* customDtypeA;
                const int dtypeA = evaluateDataTypes(bex->operandA, &customDtypeA, lvalueType, lvalueTypeDef);
                if (dtypeA < Err::OK) return dtypeA;

                const int dtypeB = evaluateDataTypes(bex->operandB, customDtype, lvalueType, lvalueTypeDef);
                if (dtypeB < Err::OK) return dtypeB;

                const int oper = bex->operType;
                switch (oper) {

                    case OP_SUBSCRIPT: {

                        if (dtypeA != DT_ARRAY) return Err::ARRAY_EXPECTED; // TODO : log error

                        Array* arr = bex->operandA->cvalue.arr;
                        if (arr->flags & IS_CMP_TIME) {


                        }

                        op->cvalue.any = arr->pointsTo;
                        rdtype = arr->pointsToEnum;

                        break;
                    }
                    
                    case OP_MEMBER_SELECTION: {
                        
                        if (dtypeA == DT_ARRAY) {

                            Variable* len = (Variable*) bex->operandA->cvalue.arr->length;

                            // TODO : 
                            op->name = len->name;
                            op->nameLen = len->nameLen;
                            op->scopeNames = len->scopeNames;
                            op->id = len->id;
                            op->expression = len->expression;
                            op->cvalue = len->cvalue;
                            return op->cvalue.dtypeEnum;
                            /*
                            if (var->cvalue.arr->length->cvalue.hasValue) {
                                op->cvalue = var->cvalue.arr->length->cvalue;
                                // var->cvalue = var->cvalue.arr->length->cvalue;
                                return DT_INT_64;
                            }
                            */

                        }

                        if (dtypeA != DT_CUSTOM) return DT_UNDEFINED;

                        
                        Variable* var = (Variable*) bex->operandB;
                        Variable* ans = Utils::find<Variable>(customDtypeA->vars, var->name, var->nameLen);
                        if (!ans) return DT_UNDEFINED; // TODO : change to error

                        copy(var, ans);

                        break;
                    
                    }

                    default:
                        
                        if (dtypeA >= dtypeB) {
                            op->cvalue.any = bex->operandA->cvalue.any;
                            rdtype = dtypeA;
                        } else {
                            op->cvalue.any = bex->operandB->cvalue.any;
                            rdtype = dtypeB;                        
                        }

                }

                break;
                
            }

            case EXT_TERNARY: {

            }

            case EXT_WRAPPER: {
                
                WrapperExpression* wex = (WrapperExpression*) ex;

                const int dtype = evaluateDataTypes(wex->operand, customDtype, lvalueType, lvalueTypeDef);
                if (dtype < Err::OK) return dtype;

                op->cvalue.any = wex->operand->cvalue.any;
                
                rdtype = dtype;
                break;

            }

            case EXT_FUNCTION_CALL: {

                FunctionCall* fex = (FunctionCall*) ex;
                
                // TODO : why i do even check it here...
                /*
                for (int i = 0; i < fex->inArgs.size(); i++) {
                    if (fex->fcn->inArgs[i]->var->cvalue.dtypeEnum == DT_MULTIPLE_TYPES) continue;
                    const int err = evaluateDataTypes(fex->inArgs[i], customDtype, lvalueType, lvalueTypeDef);
                    if (err < 0) return err;
                }
                */

                op->cvalue.any = fex->outArg->cvalue.any;
                rdtype = fex->outArg->cvalue.dtypeEnum;

                break;

            }

            case EXT_SLICE: {

                Slice* sex = (Slice*) ex;

                DataTypeEnum arrDtype = (DataTypeEnum) evaluateDataTypes(sex->arr);
                if (!validateImplicitCast(arrDtype, DT_ARRAY)) {
                    return DT_UNDEFINED;
                }

                DataTypeEnum idxDtype;

                idxDtype = (DataTypeEnum) evaluateDataTypes(sex->bidx);
                if (!validateImplicitCast(idxDtype, DT_INT)) {
                    return DT_UNDEFINED;
                }
                
                idxDtype = (DataTypeEnum) evaluateDataTypes(sex->eidx);
                if (!validateImplicitCast(idxDtype, DT_INT)) {
                    return DT_UNDEFINED;
                }

                if (customDtype) {
                    *customDtype = (TypeDefinition*) (sex->arr->cvalue.any);
                }

                return DT_ARRAY;
            
            }

            case EXT_STRING_INITIALIZATION: {
                
                return DT_STRING;

            }

            case EXT_ARRAY_INITIALIZATION: {

                ArrayInitialization* aex = (ArrayInitialization*) ex;

                if (lvalueType != DT_ARRAY) {
                    return DT_UNDEFINED;
                }

                Array* lvalueDtype = (Array*)lvalueTypeDef;
                DataTypeEnum dtypeB = (DataTypeEnum) lvalueDtype->pointsToEnum;

                for (int i = 0; i < aex->attributes.size(); i++) {

                    DataTypeEnum dtypeA = (DataTypeEnum) evaluateDataTypes(aex->attributes[i]);

                    if (!validateImplicitCast(dtypeB, dtypeA)) {
                        return DT_UNDEFINED;
                    }

                }

                if (customDtype) *customDtype = lvalueTypeDef;

                return lvalueType;


                return DT_STRING;

            }

            case EXT_TYPE_INITIALIZATION: {

                TypeInitialization* tex = (TypeInitialization*) ex;
                
                if (lvalueTypeDef->vars.size() < tex->attributes.size()) {
                    return DT_UNDEFINED;
                }


                for (int i = 0; i < tex->attributes.size(); i++) {

                    DataTypeEnum dtypeA = (DataTypeEnum) evaluateDataTypes(tex->attributes[i]);
                    DataTypeEnum dtypeB = (DataTypeEnum) evaluateDataTypes(lvalueTypeDef->vars[i]);

                    if (!validateImplicitCast(dtypeB, dtypeA)) {
                        return DT_UNDEFINED;
                    }

                }

                if (customDtype) *customDtype = lvalueTypeDef;
                
                return lvalueType; // (lvalueType == DT_ARRAY) ? DT_CUSTOM;

            }

        }

        op->cvalue.dtypeEnum = (DataTypeEnum) rdtype;
        return rdtype;

    }



    // returns DataTypeEnum of expression or error
    // assumings that each operand has defined dtype (evaluateDataTypes ran through branch)
    int evaluate(Variable* op, TypeDefinition** customDtype) {

        Expression* ex = op->expression;
        if (!ex) {
            
            const DataTypeEnum dtype = op->cvalue.dtypeEnum;
            if (dtype == DT_CUSTOM && customDtype) {
                *customDtype = (TypeDefinition*) op->def->var->dtype;
            }
            
            return dtype;
        
        }

        switch (ex->type) {
            
            case EXT_UNARY: {

                UnaryExpression* uex = (UnaryExpression*) ex;
                
                const int dtype = evaluate(uex->operand);
                if (dtype < Err::OK) return dtype;

                if (!(uex->operand->cvalue.hasValue)) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUARIED), uex->operand->loc);
                    return Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUARIED;
                }

                int x = Interpreter::applyOperator(uex->operType, &(uex->operand->cvalue));
                op->cvalue = uex->operand->cvalue;

                return op->cvalue.dtypeEnum;
                
            }

            case EXT_BINARY: {
                
                BinaryExpression* bex = (BinaryExpression*) ex;

                const int dtypeA = evaluate(bex->operandA);
                if (dtypeA < Err::OK) return dtypeA;

                if (!(bex->operandA->cvalue.hasValue)) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUARIED), bex->operandA->loc);
                    return Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUARIED;
                }

                const int dtypeB = evaluate(bex->operandB);
                if (dtypeB < Err::OK) return dtypeB;

                if (!(bex->operandB->cvalue.hasValue)) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUARIED), bex->operandB->loc);
                    return Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUARIED;
                }

                const int hasValueA = bex->operandA->cvalue.hasValue;
                const int hasValueB = bex->operandB->cvalue.hasValue;
                
                if (dtypeA < dtypeB) {
                    Variable* tmp = bex->operandA;
                    bex->operandA = bex->operandB;
                    bex->operandB = tmp;
                }

                
                Interpreter::applyOperator(bex->operType, &(bex->operandA->cvalue), &(bex->operandB->cvalue));
                op->cvalue = bex->operandA->cvalue;

                return op->cvalue.dtypeEnum;

            }

            case EXT_TERNARY: {

            }

            case EXT_WRAPPER: {
                
                WrapperExpression* wex = (WrapperExpression*) ex;
                
                const int dtype = evaluate(wex->operand);
                if (dtype < Err::OK) return dtype;

                if (op->cvalue.dtypeEnum == DT_ARRAY && dtype == Err::OK) {
                    // array initialization, we are, supposedly, ok with it
                    // computing all elements
                    return DT_ARRAY;
                }

                if (wex->operand->def) {
                    op->cvalue = wex->operand->def->var->cvalue;
                } else {
                    op->cvalue = wex->operand->cvalue;
                }

                return op->cvalue.dtypeEnum;

            }

            case EXT_FUNCTION_CALL: {

                FunctionCall* fex = (FunctionCall*) ex;

                for (int i = 0; i < fex->inArgs.size(); i++) {
                    const int err = evaluate(fex->inArgs[i]);
                    if (err < Err::OK) return err;
                    fex->fcn->inArgs[i]->var->cvalue.ptr = fex->inArgs[i]->cvalue.ptr;
                }

                const int err = Interpreter::execFunction(fex->fcn, op);
                if (err < 0) return err;

                op->cvalue.hasValue = 1;
                return err;

            }

            case EXT_TYPE_INITIALIZATION: {

                TypeInitialization* tex = (TypeInitialization*) ex;
                
                for (int i = 0; i < tex->attributes.size(); i++) {
                    const int err = evaluate(tex->attributes[i]);
                    if (err < Err::OK) return err;
                }

                return Err::OK;

            }

        }

    }



    void copy(Variable* dest, Variable* src) {

        Location* loc = dest->loc;
        Expression* ex = dest->expression;
        // Variable* aloc = dest->allocSize;
        // std::vector<ScopeName*> sc = dest->scopeNames;
        memcpy(dest, src, sizeof(Variable));
        dest->loc = loc;
        dest->expression = ex;
        // dest->allocSize = aloc;
        // dest->scopeNames = sc;
    
    }

    // better name?
    // if idx is < 0, function will use the first attribute of
    // the first initialization (expected at first call)
    // function returns index of the next avaliable attribute
    // the input index is returned if there is no attribute avaliable
    int copyExpression(Variable* src, Variable** dest, int* pidx, int* nextIdx) {
        
        Expression* ex = src->expression;
        if (!ex) {

            *dest = new Variable();
            copy(*dest, src);
            (*dest)->name = src->name;
            (*dest)->nameLen = src->nameLen;
            
            (*dest)->cvalue = src->cvalue;

            return Err::OK;

        }

        switch (ex->type) {

            case EXT_UNARY : {
                
                UnaryExpression* uex = (UnaryExpression*) ex;
                UnaryExpression* duex = new UnaryExpression();

                *dest = new Variable();
                (*dest)->expression = duex;

                duex->oper = uex->oper;
                duex->operType = uex->operType;
                
                return copyExpression((Variable*) uex->operand, (Variable**) &(duex->operand), pidx, nextIdx);
                
                break;

            }

            case EXT_BINARY: {

                BinaryExpression* bex = (BinaryExpression*) ex;
                BinaryExpression* dbex = new BinaryExpression();

                *dest = new Variable();
                (*dest)->expression = dbex;
                
                dbex->oper = bex->oper;
                dbex->operType = bex->operType;

                copyExpression((Variable*) bex->operandA, (Variable**) &(dbex->operandA), pidx, nextIdx);
                copyExpression((Variable*) bex->operandB, (Variable**) &(dbex->operandB), pidx, nextIdx);

                break;
            
            }

            case EXT_TERNARY: {

                break;
            }

            case EXT_WRAPPER: {

                WrapperExpression* wex = (WrapperExpression*) ex;

                *dest = new Variable();
                (*dest)->expression = new WrapperExpression();
                
                return copyExpression((Variable*) wex->operand, (Variable**) &(((WrapperExpression*) (*dest)->expression)->operand), pidx, nextIdx);

                break;

            }

            case EXT_FUNCTION_CALL: {

                FunctionCall* fex = (FunctionCall*) ex;
                FunctionCall* dfex = new FunctionCall();

                dfex->fcn = fex->fcn;
                dfex->name = fex->name;
                dfex->nameLen = fex->nameLen;
                dfex->scopeNames = fex->scopeNames;
                dfex->id = fex->id;

                for (int i = 0; i < fex->inArgs.size(); i++) {
                    copyExpression(fex->inArgs[i], &(dfex->inArgs[i]), pidx, nextIdx);
                }

                *dest = new Variable();
                (*dest)->expression = dfex;

                return copyExpression(fex->outArg, &(dfex->outArg), pidx, nextIdx);

                break;

            }

            case EXT_TYPE_INITIALIZATION: {

                TypeInitialization* tex = (TypeInitialization*) ex;
                
                *dest = new Variable();

                int idx = *pidx;

                if (tex->attributes[idx]->cvalue.dtypeEnum == DT_CUSTOM) {
                    // TODO : have to think even if allow inner initialization
                    // but if so, seems like it has to be done in itterative way
                    // or rethink everything

                }
                
                if (idx < 0) {

                    idx = 0;
                
                } else {

                    if (tex->attributes[0]->name) {

                        const int size = tex->attributes.size();

                        int i = 0;
                        for (; i < size; i++) { 
                            if (idx == tex->idxs[i]) {
                                idx = i;
                                break;
                            }
                        }

                        if (i >= size) {
                            Logger::log(Logger::ERROR, "TODO : error type initialization, no such attribute");
                        }
                    
                    }

                }

                int err = copyExpression(tex->attributes[idx], dest, pidx, nextIdx); // TODO : pidx and nextIdx are placeholders
                if (err < 0) return err;

                if (*nextIdx >= 0) return Err::OK;

                if (idx + 1 >= tex->attributes.size()) {
                    *nextIdx = idx;
                    break;
                }

                if (tex->attributes[0]->name) {
                    *nextIdx = tex->idxs[idx + 1];
                    break;
                }

                *nextIdx = idx + 1;
                // copy(*dest, tex->attributes[idx]);
                // (*dest)->cvalue = src->cvalue;

                break;

            }


        }

        return Err::OK;

    }

}