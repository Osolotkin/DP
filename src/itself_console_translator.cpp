#pragma once

#include "itself_console_translator.h"



#define CSP_MAX_TAB_LEVEL 4
#define CSP_GEN_TAB(str, n) for (int i = 0; i < (n); i++) str[i] = '\t'; str[(n)] = '\0';



// maybe const?
Translator* const thisTranslator = &translatorItselfConsole;

void printForeignLangFunction(FILE* file, Function* node) {

}



void init (char* const dirName) {

}

void exit() {

}

void printDataType(const DataTypeEnum dtypeEnum) {

    DataType* const dtype = dataTypes + dtypeEnum;

    if (dtypeEnum == DT_CUSTOM) {

        printf("%.*s", dtype->nameLen, dtype->name);    
    
    } else if (dtypeEnum == DT_POINTER) {

        // cant do anything..
    
    } else {

        switch (dtypeEnum) {

            case DT_INT :
                printf("int");
                break;
                
            case DT_INT_32 :
                printf("i32");
                break;
                
            case DT_INT_64 :
                printf("i64");
                break;
                
            case DT_FLOAT_32 :
                printf("f32");
                break;
                
            case DT_FLOAT_64 :
                printf("f64");
                break;

            default :
                printf("%s", dtype->name);
        
        }
    
    }

}

void printDataType(const DataTypeEnum dtypeEnum, void* dtype) {
    
    if (dtypeEnum == DT_POINTER) {

        Pointer* const ptr = ((Pointer*) dtype);
        printDataType(ptr->pointsToEnum, ptr->pointsTo);
        putchar('^');
    
    } else if (dtypeEnum == DT_ARRAY) {
        
        Array* const arr = (Array*) dtype;
        printDataType(arr->pointsToEnum, arr->pointsTo);

    } else if (dtypeEnum == DT_CUSTOM) {
        
        TypeDefinition* td = (TypeDefinition*) (dtype);
        printf("%.*s", td->nameLen, td->name);
        // fprintf(stdout, "%.*s", td->nameLen, td->name)

    } else {

        printDataType(dtypeEnum);
    
    }

}

void printOperandValue(Operand* op) {

    switch (op->cvalue.dtypeEnum) {

        case DT_INT : {

        }

        case DT_INT_32 : {
            printf("%i", op->cvalue.i32);
            break;
        }

        case DT_INT_64 : {
            printf("%i", op->cvalue.i64);
            break;
        }

        case DT_FLOAT_32 : {
            printf("%.2f", op->cvalue.f32);
            break;
        }

        case DT_FLOAT_64 : {
            printf("%.2f", op->cvalue.f64);
            break;
        }

        case DT_STRING : {
            printf("\"%s\"", op->cvalue.str);
            break;
        }

        default : {
            printf("<unknown type>");
        }
    
    }

}

// TODO : maybe abstract??? will it benefit??
void printDataType(DataType* const dtype, const DataTypeEnum dtypeEnum) {

    if (dtypeEnum == DT_CUSTOM) {

        printf("%.*s", dtype->name, dtype->nameLen);    
    
    } else if (dtypeEnum == DT_POINTER) {

        Pointer* const ptr = (Pointer*) dtype;
        DataType* ptrDtype = dataTypes + DT_POINTER;

        printDataType((DataType*) ptr->pointsTo, ptr->pointsToEnum);
        printf("%s", ptrDtype->name);
    
    } else {

        printf("%s", dtype->name);
    
    }

}

void printOperator(OperatorEnum opType) {

    switch (opType) {
        
        case OP_UNARY_PLUS :
            putchar('+');    
            break;
        
        case OP_UNARY_MINUS :
            putchar('-');
            break;
        
        case OP_ADDITION :
            putchar('+');
            break;

        case OP_SUBTRACTION :
            putchar('-');
            break;

        case OP_MULTIPLICATION :
            putchar('*');
            break;

        case OP_DIVISION :
            putchar('/');;
            break;

        case OP_MODULO :
            putchar('%');
            break;

        case OP_GET_ADDRESS :
            putchar('&');
            break;

        case OP_GET_VALUE :
            putchar('*');            
            break;

        case OP_BITWISE_AND :
            putchar('&');
            break;

        case OP_EQUAL :
            putchar('=');
            putchar('=');
            break;
        
        case OP_NOT_EQUAL :
            putchar('!');
            putchar('=');
            break;

        case OP_LESS_THAN :
            putchar('<');
            break;

        case OP_GREATER_THAN :
            putchar('>');
            break;

        case OP_LESS_THAN_OR_EQUAL :
            putchar('<');
            putchar('=');
            break;
        
        case OP_BOOL_AND :
            putchar('&');
            putchar('&');
            break;

        case OP_BOOL_OR :
            putchar('|');
            putchar('|');
            break;

        case OP_GREATER_THAN_OR_EQUAL :
            putchar('>');
            putchar('=');
            break;

        case OP_INCREMENT :
            putchar('+');
            putchar('+');
            break;

        case OP_DECREMENT :
            putchar('-');
            putchar('-');
            break;

        case OP_SUBSCRIPT :
            putchar('[');
            break;

        case OP_MEMBER_SELECTION :
            putchar('.');
            break;
    }

}





void printScope(FILE* file, int level, Scope* const node, Variable* lvalue = NULL) {

    level = (level > CSP_MAX_TAB_LEVEL) ? CSP_MAX_TAB_LEVEL : level;
    char tab[CSP_MAX_TAB_LEVEL + 1];
    CSP_GEN_TAB(tab, level);
    
    printf("%s{\n", tab);
    for (int i = 0; i < (int) node->children.size(); i++) {
        node->children[i]->print(thisTranslator, file, level + 1);
    }
    printf("%s}\n", tab);

}

void printVariableDefinition(FILE* file, int level, VariableDefinition* const node, Variable* lvalue = NULL) {

    level = (level > CSP_MAX_TAB_LEVEL) ? CSP_MAX_TAB_LEVEL : level;
    char tab[CSP_MAX_TAB_LEVEL + 1];
    CSP_GEN_TAB(tab, level);

    printf("%s", tab);
    if (node->flags & IS_CONST) {
        printf("const ");
        printDataType((DataType*) node->var->cvalue.any, node->var->cvalue.dtypeEnum);
    } else {
        printDataType((DataType*) node->var->cvalue.any, node->var->cvalue.dtypeEnum);
    }

    printf(" %.*s = ", node->var->nameLen, node->var->name);
    
    // again, somehow have to get rid of this check
    if (node->var->expression) {
        
        Expression* ex = node->var->expression;
        ex->print(thisTranslator, file, level);

    } else {
        //var->print(level);
        printOperandValue(node->var);
    }

    printf(";\n");

}

void printVariableAssignment(FILE* file, int level, VariableAssignment* const node, Variable* lvalue = NULL) {

    level = (level > CSP_MAX_TAB_LEVEL) ? CSP_MAX_TAB_LEVEL : level;
    char tab[CSP_MAX_TAB_LEVEL + 1];
    CSP_GEN_TAB(tab, level);

    printf("%s", tab);
    node->lvar->print(thisTranslator, file, level);
    printf(" = ");
    //printf("%s%.*s = ", tab, var->nameLen, var->name);
    // again, somehow have to get rid of this check
    if (node->rvar->expression) {
        
        Expression* ex = node->rvar->expression;
        ex->print(thisTranslator, file);

    } else {
        printOperandValue(node->rvar);
    }

    printf(";\n");

}

void printTypeDefinition(FILE* file, int level, TypeDefinition* const node, Variable* lvalue = NULL) {

    level = (level > CSP_MAX_TAB_LEVEL) ? CSP_MAX_TAB_LEVEL : level;
    char tab[CSP_MAX_TAB_LEVEL + 1];
    CSP_GEN_TAB(tab, level);

    printf("%sdef %.*s \n%s{\n", tab, node->nameLen, node->name, tab);
    
    const int lastIdx = (int) node->vars.size() - 1;
    
    for (int i = 0; i < lastIdx; i++) {
        
        Variable* const var = node->vars[i];
        
        // TODO : cleanup
        if (var->cvalue.dtypeEnum != DT_CUSTOM) {
            DataType* const dtype = dataTypes + var->cvalue.dtypeEnum; 
            printf("%s\t%.*s %.*s = %i,\n", tab, dtype->nameLen, dtype->name, var->nameLen, var->name, var->cvalue.i64);
        } else {
            TypeDefinition* const dtype = var->cvalue.def; 
            printf("%s\t%.*s %.*s = %i,\n", tab, dtype->nameLen, dtype->name, var->nameLen, var->name, var->cvalue.i64);    
        }

    }

    if (lastIdx - 1 >= 0) {
        
        Variable* const var = node->vars[lastIdx];
        
        if (var->cvalue.dtypeEnum != DT_CUSTOM) {
            DataType* const dtype = dataTypes + var->cvalue.dtypeEnum; 
            printf("%s\t%.*s %.*s = %i\n", tab, dtype->nameLen, dtype->name, var->nameLen, var->name, var->cvalue.i64);
        } else {
            TypeDefinition* const dtype = var->cvalue.def; 
            printf("%s\t%.*s %.*s = %i\n", tab, dtype->nameLen, dtype->name, var->nameLen, var->name, var->cvalue.i64);
        }
    }
    
    printf("%s}\n", tab);

}

void printTypeInitialization(FILE* file, int level, TypeInitialization* const node, Variable* lvalue = NULL) {

    level = (level > CSP_MAX_TAB_LEVEL) ? CSP_MAX_TAB_LEVEL : level;
    char tab[CSP_MAX_TAB_LEVEL + 1];
    CSP_GEN_TAB(tab, level);

    printf("{\n");

    for (int i = 0; i < (int) node->attributes.size() - 1; i++) {
        
        Variable* const var = node->attributes[i];
        
        printf("%s\t", tab);

        if (var->nameLen > 0) {
            printf("%.*s = ", var->nameLen, var->name);
            if (var->expression) {
                var->expression->print(thisTranslator, file, level + 1);
            } else {
                //printf("%s", tab);
                printOperandValue(var);
            }
        } else {
            if (var->expression) {
                var->expression->print(thisTranslator, file, level + 1);
            } else {
                //printf("%s", tab);
                printOperandValue(var);
            }
        }

        printf(",\n");

    }

    if ((int) node->attributes.size() > 0) {
        
        Variable* const var = node->attributes[(int) node->attributes.size() - 1];
        
        printf("%s\t", tab);

        if (var->nameLen > 0) {
            printf("%.*s = ", var->nameLen, var->name);
            if (var->expression) {
                var->expression->print(thisTranslator, file, level + 1);
            } else {
                //printf("%s", tab);
                printOperandValue(var);
            }
        } else {
            if (var->expression) {
                var->expression->print(thisTranslator, file, level + 1);
            } else {
                //printf("%s", tab);
                printOperandValue(var);
            }
        }

    }

    printf("\n%s}", tab);

}

void printStringInitialization(FILE* file, int level, StringInitialization* const node, Variable* lvalue = NULL) {

}

void printArrayInitialization(FILE* file, int level, ArrayInitialization* const node, Variable* lvalue = NULL) {

}

void printEnumerator(FILE* file, int level, Enumerator* const node, Variable* lvalue = NULL) {

    level = (level > CSP_MAX_TAB_LEVEL) ? CSP_MAX_TAB_LEVEL : level;
    char tab[CSP_MAX_TAB_LEVEL + 1];
    CSP_GEN_TAB(tab, level);

    DataType* const dt = dataTypes + node->dtype;

    printf("%senum %.*s %.*s \n%s{\n", tab, node->nameLen, node->name, dt->nameLen, dt->name, tab);
    
    const int lastIdx = (int) node->vars.size() - 1;
    
    for (int i = 0; i < lastIdx; i++) {
        Variable* const var = node->vars[i];
        printf("%s\t%.*s = %d,\n", tab, var->nameLen, var->name, var->cvalue.i64);
    }

    if (lastIdx - 1 >= 0) {
        Variable* const var = node->vars[lastIdx];
        printf("%s\t%.*s = %i\n", tab, var->nameLen, var->name, var->cvalue.i64);
    }
    
    printf("%s}\n", tab);

}

void printVariable(FILE* file, int level, Variable* const node, Variable* lvalue = NULL) {
    
    if (node->def && node->def->flags & IS_CMP_TIME) {
        printOperandValue(node->def->var);
    } else if (node->nameLen > 0) {
        printf("%.*s_%i", node->nameLen, node->name, node->id);
    } else {
        if (node->expression) node->expression->print(&translatorItselfConsole, file);
        else printOperandValue(node);
    }

}


void printFunction(FILE* file, int level, Function* const node, Variable* lvalue = NULL) {

    level = (level > CSP_MAX_TAB_LEVEL) ? CSP_MAX_TAB_LEVEL : level;
    char tab[CSP_MAX_TAB_LEVEL + 1];
    CSP_GEN_TAB(tab, level);

    printf("%s%.*s(", tab, node->nameLen, node->name);

    // in args
    for (int i = 0; i < ((int) node->inArgs.size()) - 1; i++) {
        Variable* const var = node->inArgs[i]->var;
        printf("%s %.*s, ", (dataTypes + var->cvalue.dtypeEnum)->name, var->nameLen, var->name);
    }
    if ((int) node->inArgs.size() - 1 >= 0) {
        Variable* const var = node->inArgs[node->inArgs.size() - 1]->var;
        printf("%s %.*s) => (", (dataTypes + var->cvalue.dtypeEnum)->name, var->nameLen, var->name);
    } else {
        printf(") => (");
    
    }

    // out arg
    if (node->outArg.dtypeEnum == DT_VOID) {
        printf(")\n");
    } else {
        const DataType dtype = dataTypes[node->outArg.dtypeEnum];
        printf("%s)\n", dtype.name);
    }

    // TODO : empty body crash
    node->bodyScope->print(thisTranslator, file, level);

}


void printBranch(FILE* file, int level, Branch* const node, Variable* lvalue = NULL) {

    level = (level > CSP_MAX_TAB_LEVEL) ? CSP_MAX_TAB_LEVEL : level;
    char tab[CSP_MAX_TAB_LEVEL + 1];
    CSP_GEN_TAB(tab, level);

    // basic if branche
    printf("%s%s ", tab, KWS_IF);
    node->expressions[0]->print(thisTranslator, file, level);
    putchar('\n');
    node->scopes[0]->print(thisTranslator, file, level);
    
    // if elses
    int i = 1;
    for (; i < node->expressions.size(); i++) {
        printf("%s%s %s ", tab, KWS_ELSE, KWS_IF);
        node->expressions[i]->print(thisTranslator, file, level);
        putchar('\n');
        node->scopes[i]->print(thisTranslator, file, level);
    }

    // final else if present
    if (i < node->scopes.size()) {
        printf("%s%s\n", tab, KWS_ELSE);
        node->scopes[(int) node->scopes.size() - 1]->print(thisTranslator, file, level);
    }

}

void printSwitchCase(FILE* file, int level, SwitchCase* const node, Variable* lvalue = NULL) {

}

void printWhileLoop(FILE* file, int level, WhileLoop* const node, Variable* lvalue = NULL) {

    level = (level > CSP_MAX_TAB_LEVEL) ? CSP_MAX_TAB_LEVEL : level;
    char tab[CSP_MAX_TAB_LEVEL + 1];
    CSP_GEN_TAB(tab, level);

    printf("%s%s ", tab, KWS_WHILE);
    node->expression->print(thisTranslator, file, level);
    putchar('\n');
    node->bodyScope->print(thisTranslator, file, level);

}


void printForLoop(FILE* file, int level, ForLoop* const node, Variable* lvalue = NULL) {

}

void printLoop(FILE* file, int level, Loop* const node, Variable* lvalue = NULL) {

}

void printReturnStatement(FILE* file, int level, ReturnStatement* const node, Variable* lvalue = NULL) {

    printf("return ");
    node->var->print(&translatorItselfConsole, file, level);
    putchar(',');
    node->err->print(&translatorItselfConsole, file, level);

}

void printContinueStatement(FILE* file, int level, ContinueStatement* const node, Variable* lvalue = NULL) {

    printf("continue;");

}

void printBreakStatement(FILE* file, int level, BreakStatement* const node, Variable* lvalue = NULL) {

    printf("break;");

}

void printGotoStatement(FILE* file, int level, GotoStatement* const node, Variable* lvalue = NULL) {

    printf("goto %.*s;", node->nameLen, node->name);

}

void printLabel(FILE* file, int level, Label* const node, Variable* lvalue = NULL) {

    printf(">%.*s;", node->nameLen, node->name);

}

void printNamespace(FILE* file, int level, Namespace* const node, Variable* lvalue = NULL) {

}

void printExpression(FILE* file, int level, Expression* const node, Variable* lvalue = NULL) {

}

void printExpressionWrapper(FILE* file, int level, ExpressionWrapper* const node, Variable* lvalue = NULL) {

    level = (level > CSP_MAX_TAB_LEVEL) ? CSP_MAX_TAB_LEVEL : level;
    char tab[CSP_MAX_TAB_LEVEL + 1];
    CSP_GEN_TAB(tab, level);

    printf("%s", tab);
    node->operand->print(thisTranslator, file, level);
    printf(";\n");

}

void printWrapperExpression(FILE* file, int level, WrapperExpression* const node, Variable* lvalue = NULL) {

    node->operand->print(thisTranslator, file, level);

}

void printConstExpression(FILE* file, int level, ConstExpression* const node, Variable* lvalue = NULL) {

}

void printOperatorExpression(FILE* file, int level, OperatorExpression* const node, Variable* lvalue = NULL) {

}

void printUnaryExpression(FILE* file, int level, UnaryExpression* const node, Variable* lvalue = NULL) {

    printf("(");
    
    // node->oper->print(thisTranslator);
    printOperator(node->operType);

    if (node->operand->unrollExpression && node->operand->expression) node->operand->expression->print(thisTranslator, file);
    else node->operand->print(thisTranslator, file, level);
        // printOperandValue(operand);

    printf(")");

}

void printBinaryExpression(FILE* file, int level, BinaryExpression* const node, Variable* lvalue = NULL) {

    printf("(");
    
    if (node->operandA->unrollExpression && node->operandA->expression) node->operandA->expression->print(thisTranslator, file);
    else node->operandA->print(thisTranslator, file, level); //printOperandValue(operandA);

    // node->oper->print(thisTranslator, 1);
    printOperator(node->operType);

    if (node->operandB->unrollExpression && node->operandB->expression) node->operandB->expression->print(thisTranslator, file);
    else node->operandB->print(thisTranslator, file, level); //printOperandValue(operandB);

    if (node->operType == OP_SUBSCRIPT) putchar(']');

    printf(")");

}

void printTernaryExpression(FILE* file, int level, TernaryExpression* const node, Variable* lvalue = NULL) {

}

void printStatement(FILE* file, int level, Statement* const node, Variable* lvalue = NULL) {
    
    node->op->print(thisTranslator, file, level);
    putchar(';');

}

void printFunctionCall(FILE* file, int level, FunctionCall* const node, Variable* lvalue = NULL) {

    printf("%.*s(", node->nameLen, node->name);
    for (int i = 0; i < ((int) node->inArgs.size()) - 1; i++) {
        node->inArgs[i]->print(thisTranslator, file, level);
        putchar(',');
        putchar(' ');
        //Variable* const var = inArgs[i];
        //printf("%s %.*s, ", (dataTypes + var->dataTypeEnum)->word, var->nameLen, var->name);
    }
    if ((int) node->inArgs.size() - 1 >= 0) {
        node->inArgs[node->inArgs.size() - 1]->print(thisTranslator, file, 0);
        //Variable* const var = inArgs[inArgs.size() - 1];
        //printf("%s %.*s", (dataTypes + var->dataTypeEnum)->word, var->nameLen, var->name);
    }
    putchar(')');

}

void printOperand(FILE* file, int level, Operand* const node, Variable* lvalue = NULL) {

    // maybe separate type?
    if (node->expression) {
        node->expression->print(thisTranslator, file, level);
    } else {
        printOperandValue(node);
    }

}

void printOperator(FILE* file, int spaces, Operator* const node) {

    const char A = node->word;
    const char B = node->word >> 8;
    const char C = node->word >> 16;
    const char D = node->word >> 24;

    if (spaces) printf(" %c%c%c%c ", A, B, C, D);
    else printf("%c%c%c%c", A, B, C, D);

}

void printUnion(FILE* file, int level, Union* const node, Variable* lvalue) {

}

void printErrorSet(FILE* file, int level, ErrorSet* const node, Variable* lvalue) {

}

void printUnaryOperator(FILE* file, int level, UnaryOperator* const node, Variable* lvalue = NULL) {

}

void printBinaryOperator(FILE* file, int level, BinaryOperator* const node, Variable* lvalue = NULL) {

}

void printTernaryOperator(FILE* file, int level, TernaryOperator* const node, Variable* lvalue = NULL) {

}



Translator translatorItselfConsole{
    NULL,
    &init,
    &exit,
    &printScope,
    &printVariableDefinition,
    &printVariableAssignment,
    &printTypeDefinition,
    &printTypeInitialization,
    &printStringInitialization,
    &printArrayInitialization,
    &printUnion,
    &printErrorSet,
    &printEnumerator,
    &printVariable,
    &printFunction,
    &printBranch,
    &printSwitchCase,
    &printWhileLoop,
    &printForLoop,
    &printLoop,
    &printReturnStatement,
    &printContinueStatement,
    &printBreakStatement,
    &printGotoStatement,
    &printLabel,
    &printNamespace,
    &printExpression,
    &printWrapperExpression,
    &printExpressionWrapper,
    &printConstExpression,
    &printOperatorExpression,
    &printUnaryExpression,
    &printBinaryExpression,
    &printTernaryExpression,
    &printStatement,
    &printFunctionCall,
    &printOperand,
    &printUnaryOperator,
    &printBinaryOperator,
    &printTernaryOperator
};
