#pragma once

#include "globals.h"

#include "syntax.h"
#include "interpreter.h"
#include "error.h"
#include "logger.h"
#include "utils.h"




Scope* SyntaxNode::root = NULL;

std::vector<LangDef*> SyntaxNode::langDefs;
std::vector<CodeBlock*> SyntaxNode::codeBlocks;
std::vector<ForeignFunction*> SyntaxNode::foreignFunctions;

std::vector<Variable*> SyntaxNode::variables;
std::vector<Variable*> SyntaxNode::fcnCalls;
std::vector<VariableDefinition*> SyntaxNode::customDataTypesReferences;
std::vector<VariableAssignment*> SyntaxNode::variableAssignments;
std::vector<Variable*> SyntaxNode::cmpTimeVars;
std::vector<Variable*> SyntaxNode::arrays;
std::vector<Loop*> SyntaxNode::loops;
std::vector<Variable*> SyntaxNode::branchExpressions;
std::vector<Statement*> SyntaxNode::statements;
std::vector<VariableDefinition*> SyntaxNode::initializations;
std::vector<ReturnStatement*> SyntaxNode::returnStatements;
std::vector<SwitchCase*> SyntaxNode::switchCases;

std::vector<Slice*> SyntaxNode::slices;

std::vector<VariableAssignment*> SyntaxNode::arraysAllocations;

std::vector<ImportStatement*> SyntaxNode::imports;

std::vector<TypeDefinition*> SyntaxNode::customDataTypes;
std::vector<Enumerator*> SyntaxNode::enumerators;



uint64_t internalFunctionUsed = 0;

Operator operators[] = {
    
    // OP_UNARY_PLUS
    {
        '+',
        4,
        IS_UNARY | IS_ONE_CHAR
    },

    // OP_UNARY_MINUS
    {
        '-',
        4,
        IS_UNARY | IS_ONE_CHAR
    },

    // OP_ADDITION
    {
        '+',
        4,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_SUBTRACTION
    {
        '-',
        4,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_MULTIPLICATION
    {
        '*',
        3,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_DIVISION
    {
        '/',
        3,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_MODULO
    {
        '%',
        3,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_GET_ADDRESS
    {
        '&',
        4,
        IS_UNARY | IS_ONE_CHAR
    },

    // OP_GET_VALUE
    {
        '*',
        4,
        IS_UNARY | IS_ONE_CHAR
    },

    // OP_BITWISE_AND
    {
        '&',
        4,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_LESS_THAN
    {
        '<',
        5,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_GREATER_THAN
    {
        '>',
        5,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_LESS_THAN_OR_EQUAL
    {
        '<=',
        5,
        IS_BINARY | IS_TWO_CHAR
    },

    // OP_GREATER_THAN_OR_EQUAL
    {
        '>=',
        5,
        IS_BINARY | IS_TWO_CHAR
    },

    // OP_EQUAL
    {
        '=',
        5,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_NOT_EQUAL
    {
        '!=',
        5,
        IS_BINARY | IS_TWO_CHAR
    },

    // OP_BOOL_AND
    {
        '&&',
        5,
        IS_BINARY | IS_TWO_CHAR
    },

    // OP_BOOL_OR
    {
        '||',
        5,
        IS_UNARY | IS_TWO_CHAR
    },

    // OP_INCREMENT
    {
        '++',
        5,
        IS_UNARY | IS_TWO_CHAR
    },

    // OP_DECREMENT
    {
        '--',
        5,
        IS_UNARY | IS_TWO_CHAR
    },

    // OP_SUBSCRIPT
    {
        '[',
        2,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_MEMBER_SELECTION
    {
        '.',
        1,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_DEREFERENCE_MEMBER_SELECTION
    {
        '.',
        4,
        IS_UNARY | IS_ONE_CHAR
    },

    // OP_NEGATION
    {
        '!',
        4,
        IS_UNARY | IS_ONE_CHAR
    }

};

const int OPERATORS_COUNT = sizeof(operators) / sizeof(Operator);



Variable* internalVariables[] = {
    VariableDefinition(new Variable(SyntaxNode::root, DT_POINTER, (char*) IVS_NULL, sizeof(IVS_NULL) - 1), IS_CMP_TIME).var
};

const int internalVariablesCount = sizeof(internalVariables) / sizeof(Variable*);

// helping functions (move to utils?)
// 
Location* getLocationStamp(Location* loc) {

    Location* stamp = (Location*) malloc(sizeof(Location));
    if (!stamp) return NULL;

    stamp->file = loc->file;
    stamp->idx = loc->idx;
    stamp->line = loc->line;

    return stamp;

}



// whatever
//

// assuming size > 0
int validateScopeNames(Scope* sc, std::vector<INamed*> names, Namespace** nspace) {

    Namespace* tmpNspace;
    for (int i = 0; i < names.size(); i++) {

        INamed* nm = names[i];
        Namespace* nspace = Utils::find<Namespace>(sc, nm->name, nm->nameLen, &Scope::namespaces);
        if (!nspace) {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_NAMESPACE));
            return Err::UNKNOWN_NAMESPACE;
        }

        sc = nspace;        
           
    }

    *nspace = (Namespace*) sc;
    return Err::OK;

}

int findMember(INamed* member, TypeDefinition* dtype) {

    return 0;

}



// Constructors
//

Operand::Operand() : SyntaxNode(NT_OPERAND) {

    // this->expression = NULL;
    // this->unrollExpression = 1;
    // this->value = NULL;
    // this->dataTypeEnum = DT_UNDEFINED;
    // this->loc = NULL;
    // this->hasValue = 0;
    this->def = NULL;

}

Operand::Operand(Scope* scope) : SyntaxNode(NT_OPERAND) {

    this->scope = scope;
    this->expression = NULL;
    this->unrollExpression = 1;
    this->cvalue.ptr = NULL;
    this->cvalue.dtypeEnum = DT_UNDEFINED;
    this->loc = NULL;
    this->cvalue.hasValue = 0;
    this->def = NULL;

}

VariableDefinition::VariableDefinition() : SyntaxNode(NT_VARIABLE_DEFINITION) {
    
    this->flags = 0;

}

VariableDefinition::VariableDefinition(Location* loc) : SyntaxNode(NT_VARIABLE_DEFINITION) {
    
    this->loc = getLocationStamp(loc);
    if (!this->loc) exit(1); // LOOK AT : maybe manage better

}

VariableDefinition::VariableDefinition(Variable* var, int flags) : SyntaxNode(NT_VARIABLE_DEFINITION) {
    
    this->var = var;
    this->flags = flags;
    var->def = this;
    // this->name = NULL;
    // this->nameLen = 0;
    // this->dtypeEnum = var->dataTypeEnum;

}

VariableAssignment::VariableAssignment() : SyntaxNode(NT_VARIABLE_ASSIGNMENT) {

    lvar = NULL;
    rvar = NULL;
    // offsetVar = NULL;

}

VariableAssignment::VariableAssignment(Location* loc) : VariableAssignment() {
    
    this->loc = getLocationStamp(loc);
    if (!this->loc) exit(1); // LOOK AT : maybe manage better

}

Variable::Variable() {

    unrollExpression = 1;
    scope = root;
    name = NULL;
    nameLen = 0;
    expression = NULL;
    cvalue.dtypeEnum = DT_UNDEFINED;
    cvalue.ptr = NULL;
    // parentStruct = NULL;
    // attribute = NULL;

}

Variable::Variable(Location* loc) : Variable() {
    
    this->loc = getLocationStamp(loc);
    if (!this->loc) exit(1); // LOOK AT : maybe manage better
    
}

Variable::Variable(Scope* loc) : Variable() {

    this->scope = scope;

}

Variable::Variable(Scope* const sc, DataTypeEnum dtype) {
    
    unrollExpression = 1;
    scope = sc;
    name = NULL;
    nameLen = 0;
    expression = NULL;
    cvalue.dtypeEnum = dtype;
    this->dtype = NULL;
    cvalue.ptr = NULL;
    // parentStruct = NULL;
    // attribute = NULL;

}

Variable::Variable(Scope* const sc, DataTypeEnum dtype, Location* loc) : Variable(sc, dtype) {
    
    this->loc = getLocationStamp(loc);
    if (!this->loc) exit(1); // LOOK AT : maybe manage better
    
}

Variable::Variable(Scope* const sc, DataTypeEnum dtype, char* name, int nameLen) : Variable(sc, dtype) {
    
    this->name = name;
    this->nameLen = nameLen;

}

Function::Function(Scope* sc, char* name, int nameLen, std::vector<VariableDefinition*> inArgs, std::vector<DataTypeEnum> outArgs, int internalIdx) : SyntaxNode(NT_FUNCTION) {
    this->scope = sc;
    this->name = name;
    this->nameLen = nameLen;
    this->inArgs = inArgs;
    this->outArgs = outArgs;
    this->internalIdx = internalIdx;
    this->bodyScope = NULL;
}



// print stuff
// LOOK AT: Ok, as ability to use any translator and use them in parallel 
//      or whatever seems reasonable, but it still looks and feels very clunky. 
//      So, what about dropping virtual functions and use enum as identifier 
//      of what type of SyntaxNode we are in but also use it as index/offset which 
//      allow translator to instantly call appropriate function, so we end up with 
//      more information and one less function call for the same amount of data. 

void Scope::print(Translator* const translator, FILE* file, int level) {
    translator->printScope(file, level, this, NULL);
};

void Enumerator::print(Translator* const translator, FILE* file, int level) {
    translator->printEnumerator(file, level, this, NULL);
};

void TypeDefinition::print(Translator* const translator, FILE* file, int level) {
    translator->printTypeDefinition(file, level, this, NULL);
};

void VariableDefinition::print(Translator* const translator, FILE* file, int level) {
    translator->printVariableDefinition(file, level, this, NULL);
}

void VariableAssignment::print(Translator* const translator, FILE* file, int level) {
    translator->printVariableAssignment(file, level, this, NULL);
}

void Variable::print(Translator* const translator, FILE* file, int level) {
    translator->printVariable(file, level, this, NULL);
};

void Function::print(Translator* const translator, FILE* file, int level) {
    translator->printFunction(file, level, this, NULL);
};

void Branch::print(Translator* const translator, FILE* file, int level) {
    translator->printBranch(file, level, this, NULL);
};

void SwitchCase::print(Translator* const translator, FILE* file, int level) {
    translator->printSwitchCase(file, level, this, NULL);
};

void WhileLoop::print(Translator* const translator, FILE* file, int level) {
    translator->printWhileLoop(file, level, this, NULL);
};

void ForLoop::print(Translator* const translator, FILE* file, int level) {
    translator->printForLoop(file, level, this, NULL);
};

void Loop::print(Translator* const translator, FILE* file, int level) {
    translator->printLoop(file, level, this, NULL);
};

void ReturnStatement::print(Translator* const translator, FILE* file, int level) {
    translator->printReturnStatement(file, level, this, NULL);
};

void ContinueStatement::print(Translator* const translator, FILE* file, int level) {
    translator->printContinueStatement(file, level, this, NULL);
};

void BreakStatement::print(Translator* const translator, FILE* file, int level) {
    translator->printBreakStatement(file, level, this, NULL);
};

void GotoStatement::print(Translator* const translator, FILE* file, int level) {
    translator->printGotoStatement(file, level, this, NULL);
};

void Label::print(Translator* const translator, FILE* file, int level) {
    translator->printLabel(file, level, this, NULL);
};

void Namespace::print(Translator* const translator, FILE* file, int level) {
    translator->printNamespace(file, level, this, NULL);
};

void ExpressionWrapper::print(Translator* const translator, FILE* file, int level) {
    translator->printExpressionWrapper(file, level, this, NULL);
}

void Operand::print(Translator* const translator, FILE* file, const int level) {
    translator->printOperand(file, level, this, NULL);
}

void FunctionCall::print(Translator* const translator, FILE* file, int level) {
    translator->printFunctionCall(file, level, this, NULL);
}

void TypeInitialization::print(Translator* const translator, FILE* file, int level) {
    translator->printTypeInitialization(file, level, this, NULL);
}

void StringInitialization::print(Translator* const translator, FILE* file, int level) {
    translator->printStringInitialization(file, level, this, NULL);
}

void ArrayInitialization::print(Translator* const translator, FILE* file, int level) {
    translator->printArrayInitialization(file, level, this, NULL);
}

void WrapperExpression::print(Translator* const translator, FILE* file, int level) {
    translator->printWrapperExpression(file, level, this, NULL);
}

void UnaryExpression::print(Translator* const translator, FILE* file, int level) {
    translator->printUnaryExpression(file, level, this, NULL);
}

void BinaryExpression::print(Translator* const translator, FILE* file, int level) {
    translator->printBinaryExpression(file, level, this, NULL);
}

void Statement::print(Translator* const translator, FILE* file, int level) {
    translator->printStatement(file, level, this, NULL);
}

void Slice::print(Translator* const translator, FILE* file, int level) {
}



void CodeBlock::print(Translator* const translator, FILE* file, int level) {
    
}



// expression evaluating stuff
//














int applyUnaryOperatorAddress(Operand* operand) {
    
    Pointer* ptr = new Pointer;
    ptr->pointsTo = operand->dtype;
    ptr->pointsToEnum = operand->cvalue.dtypeEnum;
    
    operand->cvalue.dtypeEnum = DT_POINTER;
    operand->dtype = (void*) ptr;
    
    return Err::OK;

}

int applyBinaryOperatorSubscript(Operand* a, Operand* b) {
    if (!IS_INT(b->cvalue.dtypeEnum)) return Err::INVALID_DATA_TYPE;
    return ((Pointer*) (a->dtype))->pointsToEnum; // TODO : do general solution!!!
}

int applyBinaryOperatorMemberSelection(Operand* ans, Operand* a, Operand* b) {
    
    if (b->cvalue.dtypeEnum != DT_MEMBER) return Err::INVALID_DATA_TYPE;

    Variable* var = Utils::find(((TypeDefinition*) a->dtype)->vars, ((Variable*) b)->name, ((Variable*) b)->nameLen);
    if (!var) return Err::INVALID_ATTRIBUTE_NAME;

    b->cvalue.dtypeEnum = var->cvalue.dtypeEnum; // dont know about this one
    b->dtype = var->dtype;
    ((Variable*) b)->id = var->id;
    ans->dtype = var->dtype;

    return Err::OK;

}

int applyBinaryOperatorMemberSelectionEnum(Operand* ans, Operand* a, Operand* b) {
    
    if (b->cvalue.dtypeEnum != DT_MEMBER) return Err::INVALID_DATA_TYPE;

    Variable* var = Utils::find(((Enumerator*) a->dtype)->vars, ((Variable*) b)->name, ((Variable*) b)->nameLen);
    if (!var) return Err::INVALID_ATTRIBUTE_NAME;

    ans->cvalue.dtypeEnum = var->cvalue.dtypeEnum; // dont know about this one
    ans->dtype = var->dtype;
    ans->cvalue.ptr = var->cvalue.ptr;
    ans->cvalue.hasValue = var->cvalue.hasValue;
    ans->def = var->def;
    ans->expression = NULL;
    ans->unrollExpression = 0;

    b->cvalue.dtypeEnum = var->cvalue.dtypeEnum;

    return Err::OK;

}

int applyBinaryOperatorMemberSelectionPointer(Operand* ans, Operand* a, Operand* b) {
    
    if (b->cvalue.dtypeEnum != DT_MEMBER) return Err::INVALID_DATA_TYPE;

    Pointer* ptr = (Pointer*) (a->dtype);
    if (ptr->pointsToEnum != DT_CUSTOM) return Err::INVALID_DATA_TYPE;

    TypeDefinition* td = (TypeDefinition*) (ptr->pointsTo);

    Variable* var = Utils::find(td->vars, ((Variable*) b)->name, ((Variable*) b)->nameLen);
    if (!var) return Err::INVALID_ATTRIBUTE_NAME;

    b->cvalue.dtypeEnum = var->cvalue.dtypeEnum; // dont know about this one
    b->dtype = var->dtype;
    ((Variable*) b)->id = var->id;
    ans->dtype = var->dtype;
    ((BinaryExpression*) (ans->expression))->operType = OP_DEREFERENCE_MEMBER_SELECTION;
    ((BinaryExpression*) (ans->expression))->oper = operators + OP_DEREFERENCE_MEMBER_SELECTION;

    return Err::OK;

}



// custom dypes
int applyUnaryOperatorPlusCustom(Operand* operand) {
    return Err::OK;
}


int applyUnaryOperatorMinusCustom(Operand* operand) {
    return Err::OK;
}









/*
int Syntax::evaluateDataTypes(Operand* op) {

    Expression* ex = op->expression;
    int rdtype = DT_UNDEFINED;

    switch (ex->type) {
        
        case EXT_UNARY: {

            UnaryExpression* uex = (UnaryExpression*) ex;
            
            const int dtype = evaluateDataTypes(uex->operand);
            if (dtype < Err::OK) return dtype;

            switch (uex->operType) {
                
                case OP_GET_ADDRESS: 
                    rdtype = DT_POINTER;
                    break;

                case OP_GET_VALUE: 
                    rdtype = uex->operand->cvalue.ptr->pointsToEnum;
                    break;

                default:
                    rdtype = dtype;

            }

        }

        case EXT_BINARY: {
            
            BinaryExpression* bex = (BinaryExpression*) ex;

            const int dtypeA = evaluateDataTypes(bex->operandA);
            if (dtypeA < Err::OK) return dtypeA;

            const int dtypeB = evaluateDataTypes(bex->operandB);
            if (dtypeB < Err::OK) return dtypeB;

            const int oper = bex->operType;
            switch (oper) {

                case OP_SUBSCRIPT:
                    if (dtypeA != DT_ARRAY) return Err::ARRAY_EXPECTED; // TODO : log error
                    rdtype = bex->operandA->cvalue.arr->pointsToEnum;
                    break;
                
                case OP_MEMBER_SELECTION:
                    break;

                default:
                    rdtype = (dtypeA >= dtypeB) ? dtypeA : dtypeB;

            }
            
        }

        case EXT_TERNARY: {

        }

        case EXT_WRAPPER: {
            
            WrapperExpression* wex = (WrapperExpression*) ex;

            const int dtype = evaluateDataTypes(wex->operand);
            if (dtype < Err::OK) return dtype;
            
            rdtype = dtype;

        }

        case EXT_FUNCTION_CALL: {

            FunctionCall* fex = (FunctionCall*) fex;
            
            for (int i = 0; i < fex->inArgs.size(); i++) {
                const int err = evaluateDataTypes(fex->inArgs[i]);
                if (err < 0) return err;
            }

            rdtype = fex->outArg->cvalue.dtypeEnum;

        }

        case EXT_TYPE_INITIALIZATION: {

            TypeInitialization* tex = (TypeInitialization*) ex;
            return DT_CUSTOM;

        }

    }

    op->cvalue.dtypeEnum = (DataTypeEnum) rdtype;
    return rdtype;

}





// returns DataTypeEnum of expression or error
// assumings that each operand has defined dtype (evaluateDataTypes ran through branch)
int Syntax::evaluate(Operand* op) {

    Expression* ex = op->expression;
    
    switch (ex->type) {
        
        case EXT_UNARY: {

            UnaryExpression* uex = (UnaryExpression*) ex;
            
            const int dtype = evaluate(uex->operand);
            if (dtype < Err::OK) return dtype;

            if (!(uex->operand->cvalue.hasValue)) {
                Logger::log(Logger::ERROR, ERR_STR(Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUARIED), uex->operand->loc);
                return Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUARIED;
            }

            Interpreter::applyOperator(uex->operType, &(uex->operand->cvalue));
            op->cvalue = uex->operand->cvalue;
            
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
                Operand* tmp = bex->operandA;
                bex->operandA = bex->operandB;
                bex->operandB = tmp;
            }

            
            Interpreter::applyOperator(bex->operType, &(bex->operandA->cvalue), &(bex->operandB->cvalue));
            op->cvalue = bex->operandA->cvalue;

        }

        case EXT_TERNARY: {

        }

        case EXT_WRAPPER: {
            
            WrapperExpression* wex = (WrapperExpression*) ex;
            op->cvalue = wex->operand->cvalue;

        }

        case EXT_FUNCTION_CALL: {

            FunctionCall* fex = (FunctionCall*) fex;

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
            return Err::OK;

        }

    }

    op->expression = NULL;
    return Err::OK;

}

*/



// on change update size!!!
DataType dataTypes[DATA_TYPES_COUNT] = {
        
    // VOID
    {
        (char*) KWS_VOID,
        sizeof(KWS_VOID) - 1,
        0,
        0
    },

    // INT
    {
        (char*) KWS_INT,
        sizeof(KWS_INT) - 1,
        4,
        1
    },

    // INT_32
    {
        (char*) KWS_INT_32,
        sizeof(KWS_INT_32) - 1,
        4,
        1
    },

    // INT_64
    {
        (char*) KWS_INT_64,
        sizeof(KWS_INT_64) - 1,
        8,
        2
    },

    // FLOAT_32
    {
        (char*) KWS_FLOAT_32,
        sizeof(KWS_FLOAT_32) - 1,
        4,
        3
    },

    // FLOAT_64
    {
        (char*) KWS_FLOAT_64,
        sizeof(KWS_FLOAT_64) - 1,
        8,
        4
    },

    // STRING
    {
        (char*) KWS_STRING,
        sizeof(KWS_STRING) - 1,
        8 * 2,
        5
    },

    // POINTER
    {
        (char*) KWS_POINTER,
        sizeof(KWS_POINTER) - 1,
        8 * 8,
        5 
    },

    // ARRAY
    {
        (char*) "array",
        5,
        8 * 8,
        5 
    },

    // SLICE
    {
        (char*) "slice",
        5,
        8 * 8,
        5
    },

    // MULTIPLE_TYPES
    {
        (char*) "...",
        3,
        0,
        0
    },

    // CUSTOM
    {
        (char*) "custom",
        6,
        0,
        10
    },

    // MEMBER
    {
        (char*) "member",
        6,
        0,
        0
    },

    // ENUM
    {
        (char*) "enum",
        4,
        0,
        0
    },

    // UNDEFINED
    {
        (char*) "undefined",
        9,
        0,
        0
    }

};
