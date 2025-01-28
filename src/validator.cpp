#pragma once

#include "logger.h"
#include "error.h"
#include "syntax.h"
#include "utils.h"
#include "interpreter.h"
#include <iterator>
#include "c_translator.h"

namespace Validator {

    int validateImplicitCast(const DataTypeEnum dtype, const DataTypeEnum dtypeRef);
    int validateImplicitCast(void* dtype, void* dtypeRef, DataTypeEnum dtypeEnum, DataTypeEnum dtypeEnumRef);
    int validateAttributeCast(Variable* var, Variable* attribute);
    int validateTypeInitialization(TypeDefinition* dtype, TypeInitialization* dtypeInit);
    int validateTypeInitializations(TypeDefinition* dtype, Variable* var);
    inline int validatePointerAssignment(const Value* const val);
    
    int evaluateArrayLength(Variable* var, Variable* len);
    int evaluateTypeInitialization(Variable* op, int attributesCount, TypeInitialization** tinitOut);
    int evaluateDataTypes(Variable* op, TypeDefinition** customDtype = NULL, DataTypeEnum lvalueType = DT_UNDEFINED, TypeDefinition* lvalueTypeDef = NULL);
    int evaluate(Variable* op, TypeDefinition** customDtype = NULL);

    int findClosestFunction(Operand* callOp, Function** outFcn);

    int getFirstNonArrayDtype(Array* arr, int maxLevel = -1, int* level = NULL);

    void stripWrapperExpressions(Variable** op);
    void stripWrapperExpressions(Variable* op);

    void copy(Variable* dest, Variable* src);
    int copyExpression(Variable* src, Variable** dest, int* pidx, int* nextIdx);

    // TODO : better name?
    struct FunctionScore {
        Function* fcn;
        int score;
    };

    std::vector<FunctionScore> fCandidates; // f as function



    int validate() {

        Logger::log(Logger::INFO, "Validating...\n");

        // control that each initialization doesn't have same name in same scope
        for (int i = 0; i < SyntaxNode::initializations.size(); i++) {

            VariableDefinition* const varDef = SyntaxNode::initializations[i];
            Variable* const var = varDef->var;

            Scope* scope = varDef->scope;
            auto it = std::find(scope->defs.begin(), scope->defs.end(), varDef);
            if (it == scope->defs.end()) {
                Logger::log(Logger::ERROR, "Unexpected internal error! Required object of type VariableDefinition not found in expected collection Scope::defs!");
                return Err::UNEXPECTED_ERROR;
            }

            const int idx = std::distance(scope->defs.begin(), it);
            if (idx == 0) continue;

            auto ritBegin = scope->defs.rbegin() + (scope->defs.size() - idx);

            if (Utils::find(ritBegin, scope->defs.rend(), var->name, var->nameLen, &VariableDefinition::var)) {
                Logger::log(Logger::ERROR, ERR_STR(Err::VARIABLE_ALREADY_DEFINED), var->loc, var->nameLen, var->nameLen, var->name);
                return Err::VARIABLE_ALREADY_DEFINED;
            }

        }

        // link user defined data types with definitions
        //

        for (int i = 0; i < (int) SyntaxNode::customDataTypesReferences.size(); i++) {

            VariableDefinition* const varDef = SyntaxNode::customDataTypesReferences[i];

            void** dtype;
            int* dtypeEnum;
            
            if (varDef->lastPtr) {
                dtype = (void**) &(varDef->lastPtr->pointsTo);
                dtypeEnum = (int*) &(varDef->lastPtr->pointsToEnum);
            } else {
                dtype = (void**) &(varDef->var->cvalue.dtypeEnum);
                dtypeEnum = (int*) &(varDef->var->cvalue.any);
            }

            TypeDefinition* td = Utils::find<TypeDefinition>(varDef->scope, varDef->dtypeName, varDef->dtypeNameLen, &Scope::customDataTypes);
            if (td) {

                *dtype = (void*) td;
                *dtypeEnum = DT_CUSTOM;

                continue;

            }

            Enumerator* en = Utils::find<Enumerator>(varDef->scope, varDef->dtypeName, varDef->dtypeNameLen, &Scope::enums);
            if (en) {

                *dtype = (dataTypes + en->dtype);
                *dtypeEnum = en->dtype;

                continue;
            
            }

            Union* un = Utils::find<Union>(varDef->scope, varDef->dtypeName, varDef->dtypeNameLen, &Scope::unions);
            if (un) {
                
                *dtype = (void*) un;
                *dtypeEnum = DT_UNION;

                continue;
            
            }

            ErrorSet* er = Utils::find<ErrorSet>(varDef->scope, varDef->dtypeName, varDef->dtypeNameLen, &Scope::customErrors);
            if (er) {

                *dtype = (void*) er;
                *dtypeEnum = DT_ERROR;
                
                continue;

            }
            
            Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_DATA_TYPE), varDef->loc, varDef->dtypeNameLen);
            return Err::UNKNOWN_DATA_TYPE;
        
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
                ErrorSet* eset;
                const int err = validateScopeNames(var->scope, var->scopeNames, &nspace, &eset);
                if (err != Err::OK) return err;
                if (eset) {
                    continue;
                }
                scope = nspace;
            }

            Variable* tmpVar = Utils::find<Variable>(scope, var->name, var->nameLen, &Scope::vars);
            if (tmpVar) {
                copy(var, tmpVar);
                continue;
            }

            tmpVar = Utils::find(internalVariables, internalVariablesCount, var->name, var->nameLen);
            if (tmpVar) {
                copy(var, tmpVar);
                continue;
            }

            Enumerator* en = Utils::find<Enumerator>(scope, var->name, var->nameLen, &Scope::enums);
            if (en) {
                var->cvalue.dtypeEnum = DT_ENUM;
                var->cvalue.enm = en;
                continue;
            }
            
            Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_VARIABLE), var->loc, var->nameLen, var->nameLen, var->name);
            return Err::UNKNOWN_VARIABLE;
            
        }



        // link function calls with definitions
        for (int i = 0; i < (int) SyntaxNode::fcnCalls.size(); i++) {

            Variable* fcnCallOp = SyntaxNode::fcnCalls[i];
            FunctionCall* fcnCall = (FunctionCall*) (fcnCallOp->expression);

            if (fcnCall->fcn) continue;

            Function* fcn;
            const int err = findClosestFunction(fcnCallOp, &fcn);
            if (err < 0) return err;

            fcnCall->fcn = fcn;
            fcnCall->outArg = new Variable();
            fcnCall->outArg->cvalue.hasValue = 0;
            fcnCall->outArg->cvalue.dtypeEnum = fcn->outArg.dtypeEnum;

        }

        for (int i = 0; i < SyntaxNode::fcns.size(); i++) {

            Function* fcn = SyntaxNode::fcns[i];
            ErrorSet* errorSet = Utils::find<ErrorSet>(fcn->scope, fcn->errorSetName, fcn->errorSetNameLen, &Scope::customErrors);

            if (!errorSet) {
                Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_ERROR_SET), fcn->loc);
                return Err::UNKNOWN_ERROR_SET;
            }

            fcn->errorSet = errorSet;

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

            // arr->flags : info about length
            // var should directly represent definiton
            
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

                    if (alloc->cvalue.dtypeEnum != DT_ARRAY) {
                        Logger::log(Logger::ERROR, "TODO error: validating arrays!");
                        return Err::INVALID_DATA_TYPE;
                    }

                    var->cvalue.arr->length = alloc->cvalue.arr->length;

                    //var->cvalue.arr->length = len.cvalue.i64;
                    arr->flags |= IS_ALLOCATED;

                    continue;

                }

                if (arr->length && (arr->length->cvalue.hasValue || arr->length->expression)) {
                    const int dt = evaluateDataTypes(arr->length);
                    if (dt > 0 && dt <= DT_UINT_64) {
                        continue;
                    } else {
                        Logger::log(Logger::ERROR, "TODO error: array length has to be int!");
                        return Err::INVALID_DATA_TYPE;
                    }
                }

                int len;
                int err;
                Variable lenVar;

                stripWrapperExpressions(var);

                err = evaluateArrayLength(var, &lenVar);
                if (err < 0) return err;

                //translatorC.printVariable(stdout, 0, &lenVar, NULL);
                //printf("\n");

                len = lenVar.cvalue.i64;

                if (arr->length->cvalue.hasValue && arr->length->cvalue.i64 != len) {
                    Logger::log(Logger::ERROR, "TODO error: array and alloc size mismatch, you can omit array size while initializing array with rvalue...");
                    return Err::INVALID_LVALUE;
                }

                var->cvalue.arr->length->cvalue.hasValue = 1;
                var->cvalue.arr->length->cvalue.i64 = len;
                var->cvalue.arr->length->cvalue.dtypeEnum = DT_INT_64;
                var->cvalue.arr->pointsTo = NULL;
                var->cvalue.hasValue = 1;

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
                    const Value tmpValue = tmp->cvalue;
                    const DataTypeEnum tmpDtype = tmp->cvalue.dtypeEnum; 

                    err = evaluateDataTypes(var, NULL, tmp->cvalue.dtypeEnum, tmp->cvalue.def);

                    if (tmp->cvalue.dtypeEnum == DT_CUSTOM) {
                        validateTypeInitialization(tmp->cvalue.def, (TypeInitialization*) ((WrapperExpression*) (var->expression))->operand->expression);
                    } else {
                        const int err = validateImplicitCast(var->cvalue.any, tmpValue.any, var->cvalue.dtypeEnum, tmpValue.dtypeEnum);
                        if (err < 0) return err;
                        // tmp->cvalue.dtypeEnum = tmpDtype;
                    }

                    tmp->cvalue = tmpValue;

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

            if (fc->outArg.dtypeEnum != DT_VOID && !rt->var && !rt->err) {
                Logger::log(Logger::ERROR, "Not enough return arguments!", rt->loc); // TODO : better error message
                return Err::NOT_ENOUGH_ARGUMENTS;
            }

            if (fc->outArg.dtypeEnum == DT_VOID && rt->var) {
                Logger::log(Logger::ERROR, "Too many return arguments!", rt->loc); // TODO : better error message
                return Err::TOO_MANY_ARGUMENTS;
            }

            if (rt->var) {

                const int rtType = evaluateDataTypes(rt->var);
                if (rtType < 0) return rtType;

                const int err = validateImplicitCast(rt->var->cvalue.any, fc->outArg.any, (DataTypeEnum) rtType, fc->outArg.dtypeEnum);
                if (err < 0) {
                    if (fc->outArg.dtypeEnum == DT_POINTER) {
                        const int err = validatePointerAssignment(&(rt->var->cvalue));
                        if (err < 0) return err;
                    }
                        return err;
                }
                /*
                if (!validateImplicitCast((DataTypeEnum) rtType, fc->outArg)) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_TYPE_CONVERSION), rt->loc, 1, (dataTypes + rtType)->name, (dataTypes + fc->outArg)->name);
                    return Err::INVALID_TYPE_CONVERSION;
                }
                */
            
            }

            if (fc->errorSet && rt->err) {
                
                Namespace* nspace;
                ErrorSet* eset;

                const int err = validateScopeNames(fc->scope, rt->err->scopeNames, &nspace, &eset);
                if (!err) return err;

                if (!err) {
                    Logger::log(Logger::ERROR, "Expected error set, pure namespace given!", rt->loc);
                    return Err::UNKNOWN_ERROR_SET;
                }
            
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

            const int err = validateImplicitCast(varAss->rvar->cvalue.any, varAss->lvar->cvalue.any, dtypeR, dtypeL);
            if (err < 0) {
                if (dtypeL == DT_POINTER) {
                    const int err = validatePointerAssignment(&(varAss->rvar->cvalue));
                    if (err < 0) return err;
                }
                return err;
            }

            if (dtypeR == DT_ARRAY) {

                Variable lenVar;
                const int err = evaluateArrayLength(varAss->rvar, &lenVar);
                if (err < 0) return err;

                if (!lvalueEx) continue;

                if (lvalueEx->type == EXT_SLICE) {

                    Slice* sex = (Slice*)(varAss->lvar->expression);
                    if (sex->eidx->cvalue.dtypeEnum == DT_UNDEFINED) {
                        copy(sex->eidx, &lenVar);
                        sex->eidx->cvalue.dtypeEnum = DT_UNDEFINED;
                    }

                } else if (lvalueEx->type == EXT_BINARY) {

                    BinaryExpression* bex = (BinaryExpression*) lvalueEx;
                    if (bex->operType == OP_SUBSCRIPT && bex->operandA->cvalue.arr->length->cvalue.dtypeEnum == DT_UNDEFINED) {
                        copy(bex->operandA->cvalue.arr->length, &lenVar);
                        bex->operandA->cvalue.arr->length->cvalue.dtypeEnum = DT_UNDEFINED;
                    }

                }
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



        // initializations
        for (int i = 0; i < SyntaxNode::initializations.size(); i++) {

            VariableDefinition* const varDef = SyntaxNode::initializations[i];
            Variable* var = varDef->var;
            
            const Value value = var->cvalue;
            DataTypeEnum dtypeEnumRef = var->cvalue.dtypeEnum;
            void* dtypeRef = var->cvalue.any;

            void* dtype;
            int dtypeEnum = evaluateDataTypes(var, (TypeDefinition**) (&dtype), dtypeEnumRef, (TypeDefinition*) (dtypeRef));
            if (dtypeEnum < 0) return dtypeEnum;

            var->cvalue = value;

            // validateCustomTypeInitialization(dtype, dtypeInit);
            const int err = validateImplicitCast(dtype, dtypeRef, (DataTypeEnum) dtypeEnum, dtypeEnumRef);
            if (err < 0) {
                if (dtypeEnumRef == DT_POINTER) {
                    stripWrapperExpressions(&var);
                    const int err = validatePointerAssignment(&(var->cvalue));
                    if (err < 0) return err;
                } else {
                    return err;
                }
            }
            /*
            if (validateImplicitCast(dtype, dtypeRef, (DataTypeEnum) dtypeEnum, dtypeEnumRef) != Err::OK) {
                Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_DATA_TYPE), varDef->loc, 0);
                return Err::INVALID_DATA_TYPE;
            }
            */


            // if (!validateImplicitCast((DataTypeEnum) dtype, dtEnum)) {
                // Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_DATA_TYPE), varDef->loc, 0);
                // return Err::INVALID_DATA_TYPE;
            // }

            // var->cvalue.dtypeEnum = dtEnum;
            // var->dtype = dt;

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

        return Err::OK;

    }

    // len parameter is used to store expression that describes 
    // the length of the array
    // out represent internal variable representing length of array
    // defined by calculated expression
    // TODO : make evaluateDataType to also validate if
    //        operands of expression can interact with operator
    int evaluateArrayLength(Variable* var, Variable* len) {

        Expression* ex = var->expression;
        if (!ex) {

            if (var->cvalue.dtypeEnum < DT_STRING) {
                return Err::OK;
            }
            
            if (!(len->expression)) {
            
                Variable* lenVar = var->cvalue.arr->length;
                if (lenVar->expression) {
                    len->expression = lenVar->expression;
                } else {
                    len->cvalue = lenVar->cvalue;
                }

            } else if (len->expression->type == EXT_BINARY) {
                
                BinaryExpression* obex = (BinaryExpression*) len->expression;
                BinaryExpression* nbex = new BinaryExpression();

                nbex->operandA = var;
                nbex->operandB = new Variable();
                nbex->operandB->expression = obex;

                len->expression = nbex;

            } else {

            }
            
            return Err::OK;
        
        }

        switch (ex->type) {

            case EXT_UNARY : {
                
                UnaryExpression* uex = (UnaryExpression*) ex;
                
                const int err = evaluateArrayLength(uex->operand, len);
                if (err < 0) return err;

                return Err::OK;

            }

            case EXT_BINARY : {
                
                int err;
                BinaryExpression* bex = (BinaryExpression*) ex;
                
                Variable lenA;
                err = evaluateArrayLength(bex->operandA, &lenA);
                if (err < 0) return err;

                Variable lenB;
                err = evaluateArrayLength(bex->operandB, &lenB);
                if (err < 0) return err;
                
                if (bex->operType == OP_CONCATENATION) {
                    // the only thing that can increase size
                    
                    if (lenA.cvalue.hasValue && lenB.cvalue.hasValue) {
                        len->cvalue.hasValue = 1;
                        len->cvalue.dtypeEnum = DT_INT_64;
                        len->cvalue.i64 += lenA.cvalue.i64 + lenB.cvalue.i64;
                        return Err::OK;
                    }

                    BinaryExpression* bex = new BinaryExpression();
                    bex->oper = operators + OP_ADDITION;
                    bex->operType = OP_ADDITION;
                    bex->operandA = new Variable(&lenA);
                    bex->operandB = new Variable(&lenB);

                    len->expression = bex;
                    
                    return Err::OK;
                
                }

                if (!(len->expression)) {
                    Variable* lenVar = &lenA;
                    if (lenVar->expression) {
                        len->expression = lenVar->expression;
                    }
                    else {
                        len->cvalue = lenVar->cvalue;
                    }
                }

                return Err::OK;
            
            }

            case EXT_WRAPPER : {
                
                WrapperExpression* wex = (WrapperExpression*) ex;
                
                const int err = evaluateArrayLength(wex->operand, len);
                if (err < 0) return err;

                return Err::OK;

            }

            case EXT_FUNCTION_CALL : {

                FunctionCall* fex = (FunctionCall*) ex;
                Variable* lenVar = fex->outArg->cvalue.arr->length;

                if (!(len->expression)) {
            
                    len->cvalue = lenVar->cvalue;

                } else if (len->expression->type == EXT_BINARY) {
                    
                    BinaryExpression* obex = (BinaryExpression*) len->expression;
                    BinaryExpression* nbex = new BinaryExpression();

                    nbex->operandA = lenVar;
                    nbex->operandB = new Variable();
                    nbex->operandB->expression = obex;

                    len->expression = nbex;

                }

                return Err::OK;

            }

            case EXT_ARRAY_INITIALIZATION : {
                    
                if (len->expression) return Err::OK;

                len->cvalue.hasValue = 1;
                len->cvalue.dtypeEnum = DT_INT_64;
                len->cvalue.i64 = ((ArrayInitialization*) (var->expression))->attributes.size();

                return Err::OK;

            }
            
            case EXT_SLICE : {

                if (len->expression) return Err::OK;

                Slice* slice = (Slice*) var->expression;
                
                evaluate(slice->eidx);
                evaluate(slice->bidx);
                
                Value ans = slice->eidx->cvalue;
                ans.dtypeEnum = DT_INT_64;
                const int err = Interpreter::applyOperator(OP_SUBTRACTION, &ans, &(slice->bidx->cvalue));
                if (err < 0) {
                    slice->len = new Variable();

                    BinaryExpression* len = new BinaryExpression();
                    len->operandA = slice->eidx;
                    len->operandB = slice->bidx;
                    len->operType = OP_SUBTRACTION;

                    slice->len->expression = len;
                } else {
                    if (!slice->len) slice->len = new Variable();
                    slice->len->cvalue = ans;
                    slice->len->expression = NULL;
                }

                len->cvalue.hasValue = 1;
                len->cvalue.dtypeEnum = DT_INT_64;
                len->cvalue.i64 = ans.i64 + 1;

                return Err::OK;

            } 
            
            case EXT_STRING_INITIALIZATION : {

                if (len->expression) return Err::OK;

                StringInitialization* init = (StringInitialization*) var->expression;

                len->cvalue.hasValue = 1;
                len->cvalue.dtypeEnum = DT_INT_64;

                if (init->wideStr) {
                    len->cvalue.i64 = init->wideLen;
                } else {
                    len->cvalue.i64 = init->rawStr.size();
                }

                return Err::OK;

            }

        }

        return Err::OK;

    }

    enum Score {
        FOS_IMPLICIT_CAST,
        FOS_SAME_SUBTYPE_SIZE_DECREASE,
        FOS_SAME_SUBTYPE_NO_SIZE_DECREASE,
        FOS_EXACT_MATCH,
    };

    // working with global array fCandidates
    void findCandidateFunctions(Scope* scope, FunctionCall* call) {

        fCandidates.clear();

        const char* const name = call->name;
        const int nameLen = call->nameLen;

        while (scope) {
            
            const std::vector<Function*> fcns = scope->fcns;

            for (int i = 0; i < (int) fcns.size(); i++) {

                if (nameLen != fcns[i]->nameLen) continue;

                char* const itemName = fcns[i]->name;
                const int itemNameLen = fcns[i]->nameLen;

                int j = 0;
                for (; j < itemNameLen; j++) {
                    if (itemName[j] != name[j]) break;
                }

                if (j == itemNameLen) {
                    fCandidates.push_back({fcns[i], 0});
                }
            
            }

            scope = scope->scope;

        }

    }

    int findClosestFunction(Operand* callOp, Function** outFcn) {

        Scope* scope = callOp->scope;
        FunctionCall* call = (FunctionCall*) callOp->expression;

        findCandidateFunctions(scope, call);

        for (int j = 0; j < fCandidates.size(); j++) {
            
            int score = 0;
            Function* fcn = fCandidates[j].fcn;
            for (int i = 0; i < fcn->inArgs.size(); i++) {
                
                Variable* fArg = fcn->inArgs[i]->var;
                Variable* cArg = call->inArgs[i];

                const int fDtype = fArg->cvalue.dtypeEnum;
                const int cDtype = evaluateDataTypes(cArg);

                if (fDtype == cDtype) {
                    score += FOS_EXACT_MATCH;
                    continue;
                }

                if (IS_INT(cDtype) && IS_INT(fDtype)) {
                    if ((cDtype - fDtype <= DT_UINT_64 - DT_INT_64)) {
                        score += FOS_SAME_SUBTYPE_NO_SIZE_DECREASE;
                    } else {
                        score += FOS_SAME_SUBTYPE_SIZE_DECREASE;
                    }
                    continue;
                }

                if (IS_FLOAT(cDtype) && IS_FLOAT(fDtype)) {
                    if ((cDtype - fDtype <= DT_UINT_64 - DT_INT_64)) {
                        score += FOS_SAME_SUBTYPE_NO_SIZE_DECREASE;
                    } else {
                        score += FOS_SAME_SUBTYPE_SIZE_DECREASE;
                    }
                    continue;
                }

                if (validateImplicitCast((DataTypeEnum) cDtype, (DataTypeEnum) fDtype)) {
                    score += FOS_IMPLICIT_CAST;
                    continue;
                }

                FunctionScore tmp = fCandidates[fCandidates.size() - 1];
                fCandidates[j] = tmp;
                fCandidates.pop_back();
                j--;
                
                break;

            }
            fCandidates[j].score = score;

        }

        int bestIdx = 0;
        int bestScore = 0;
        int sameScoreCnt = 0;
        for (int i = 0; i < fCandidates.size(); i++) {
            const int score = fCandidates[i].score;
            if (score > bestScore) {
                bestScore = score;
                bestIdx = i;
            } else if (score == bestScore) {
                sameScoreCnt++;
                continue;
            }
            sameScoreCnt = 0;
        }

        if (bestScore > 0 && sameScoreCnt == 0) {
            *outFcn = fCandidates[bestIdx].fcn;
            return Err::OK;
        } else {

            if (bestScore <= 0 ) {
                Logger::log(Logger::ERROR, ERR_STR(Err::NO_MATCHING_FUNCTION_FOUND));
                return Err::NO_MATCHING_FUNCTION_FOUND;
            }

            Logger::log(Logger::ERROR, ERR_STR(Err::MORE_THAN_ONE_OVERLOAD_MATCH), callOp->loc, 1);
            return Err::MORE_THAN_ONE_OVERLOAD_MATCH;

        }

        return Err::OK;
        
    }

    // level has to be 0, if its output matters
    int getFirstNonArrayDtype(Array* arr, const int maxLevel, int* level) {
        
        const int dtype = arr->pointsToEnum;

        if (maxLevel > 0 && *level >= maxLevel) return dtype;
        if (dtype != DT_ARRAY) return dtype;

        if (level) *level = *level + 1;
        return getFirstNonArrayDtype((Array*) arr->pointsTo, maxLevel, level);
    
    }

    // TODO : to macro or constexpr
    int validateImplicitCast(const DataTypeEnum dtype, const DataTypeEnum dtypeRef) {

        const int basicTypes = (dtype != DT_VOID && dtypeRef != DT_VOID) && (dtype < DT_STRING && dtypeRef < DT_STRING);
        const int arrayToPointer = (dtype == DT_ARRAY && dtypeRef == DT_POINTER);
        const int pointerToArray = (dtypeRef == DT_ARRAY && dtype == DT_POINTER);
        const int sliceToArray = dtype == DT_SLICE && dtypeRef == DT_ARRAY;

        return (dtype == dtypeRef) || (basicTypes || arrayToPointer || pointerToArray || sliceToArray);

    }

    int validateImplicitCast(void* dtype, void* dtypeRef, DataTypeEnum dtypeEnum, DataTypeEnum dtypeEnumRef) {
        
        if (validateImplicitCast(dtypeEnum, dtypeEnumRef)) {
            return Err::OK;
        }

        if (dtypeEnumRef == DT_ARRAY && dtypeEnum == DT_STRING) {

            Array* arr = (Array*) dtypeRef;
            StringInitialization* str = (StringInitialization*) dtype;

            const int arrDtypeSize = dataTypes[arr->pointsToEnum].size;
            const int strDtypeSize = dataTypes[str->wideDtype].size;

            if (arrDtypeSize < strDtypeSize) {
                Logger::log(Logger::ERROR, "TODO error: ");
                return Err::INVALID_TYPE_CONVERSION;
            }

            if (!validateImplicitCast(arr->pointsToEnum, str->wideDtype)) {
                Logger::log(Logger::ERROR, "TODO error: ");
                return Err::INVALID_TYPE_CONVERSION;
            }

            if (arrDtypeSize < arrDtypeSize) {
                Logger::log(Logger::WARNING, "TODO warning: One can use smaller dtype");
            }

            return Err::OK;

        } else if (dtypeEnumRef == DT_ARRAY && dtypeEnum == DT_ARRAY) {
            
            Array* arrRef = (Array*) dtypeRef;
            Array* arr = (Array*) dtype;

            int levelRef = 0;
            const DataTypeEnum arrDtypeRef = (DataTypeEnum) getFirstNonArrayDtype(arrRef, -1, &levelRef);

            const int maxLevel = levelRef;
            levelRef = 0;
            const DataTypeEnum arrDtype = (DataTypeEnum) getFirstNonArrayDtype(arr, maxLevel, &levelRef);

            return validateImplicitCast(arrDtype, arrDtypeRef);

        
        } else if (dtypeEnumRef == DT_ARRAY) {

            Array* arr = (Array*) dtypeRef;
            const DataTypeEnum arrDtype = (DataTypeEnum) getFirstNonArrayDtype(arr);

            return validateImplicitCast(dtypeEnum, arrDtype);

        }

        return Err::INVALID_TYPE_CONVERSION;

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

            if (!var->cvalue.any) {
                // if no dtype, we have to find appropriate dtype and 'cash it'

                TypeDefinition *dtype = Utils::find<TypeDefinition>(var->scope, var->def->dtypeName, var->def->dtypeNameLen, &Scope::customDataTypes);
                if (!dtype) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_DATA_TYPE), var->def->loc, var->def->dtypeNameLen);
                    return Err::UNKNOWN_DATA_TYPE;
                }

                var->cvalue.def = dtype;

            }

            const int err = validateTypeInitializations(var->cvalue.def, attribute);
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
            Logger::log(Logger::ERROR, ERR_STR(Err::TYPE_INIT_ATTRIBUTES_COUNT_MISMATCH), dtype->loc, dtype->nameLen, dtypeInit->attributes.size(), dtype->vars.size());
            return Err::TYPE_INIT_ATTRIBUTES_COUNT_MISMATCH;
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

    inline int validatePointerAssignment(const Value* const val) {
        if (val->hasValue && val->u64 == 0) return Err::OK;
        Logger::log(Logger::ERROR, "Only 0 could be assigned to a pointer variable!");
        return Err::INVALID_RVALUE;
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
                *customDtype = op->cvalue.def;
            } else if (customDtype) {
                *customDtype = (TypeDefinition*) (op->cvalue.any);
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
                        if (op->cvalue.dtypeEnum == DT_CUSTOM) {
                            Logger::log(Logger::ERROR, "TODO error: unsupported operator with struct!");
                            return DT_UNDEFINED;
                        }
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

                        if (dtypeA == DT_ARRAY) {

                            Array* arr = bex->operandA->cvalue.arr;

                            if (dtypeB == DT_UNDEFINED) {
                                // appending to array, so returning array
                                op->cvalue.any = arr;
                                rdtype = DT_ARRAY;
                                break;
                            }

                            if (arr->flags & IS_CMP_TIME) {


                            }

                            op->cvalue.any = arr->pointsTo;
                            rdtype = arr->pointsToEnum;

                        } else if (dtypeA == DT_POINTER) {

                            Pointer* ptr = bex->operandA->cvalue.ptr;

                            op->cvalue.any = ptr->pointsTo;
                            rdtype = ptr->pointsToEnum;
                        
                        } else {

                            Logger::log(Logger::ERROR, "Only array and pointer can be indexed!");
                            return Err::ARRAY_EXPECTED;
                        
                        }

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
                if (idxDtype == DT_UNDEFINED && !sex->bidx->expression) {
                    sex->bidx->cvalue.dtypeEnum = DT_INT_64;
                    sex->bidx->cvalue.hasValue = 1;
                    sex->bidx->cvalue.i64 = 0;
                } else if (!validateImplicitCast(idxDtype, DT_INT)) {
                    return DT_UNDEFINED;
                }
                
                idxDtype = (DataTypeEnum) evaluateDataTypes(sex->eidx);
                if ((idxDtype != DT_UNDEFINED || sex->eidx->expression) && !validateImplicitCast(idxDtype, DT_INT)) {
                    return DT_UNDEFINED;
                }

                if (customDtype) {
                    *customDtype = (TypeDefinition*) (sex->arr->cvalue.any);
                }

                return DT_ARRAY;
            
            }

            case EXT_STRING_INITIALIZATION: {

                StringInitialization* init = (StringInitialization*) ex;

                if (customDtype) {
                    *customDtype = (TypeDefinition*) ex;
                }

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
                *customDtype = op->def->var->cvalue.def;
            }
            
            return dtype;
        
        }

        switch (ex->type) {
            
            case EXT_UNARY: {

                UnaryExpression* uex = (UnaryExpression*) ex;
                
                const int dtype = evaluate(uex->operand);
                if (dtype < Err::OK) return dtype;

                if (!(uex->operand->cvalue.hasValue)) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED), uex->operand->loc);
                    return Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED;
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
                    Logger::log(Logger::ERROR, ERR_STR(Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED), bex->operandA->loc);
                    return Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED;
                }

                const int dtypeB = evaluate(bex->operandB);
                if (dtypeB < Err::OK) return dtypeB;

                if (!(bex->operandB->cvalue.hasValue)) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED), bex->operandB->loc);
                    return Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED;
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
