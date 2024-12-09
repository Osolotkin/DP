#pragma once

#include <stdio.h>

#include "c_translator.h"

#define MAX_FILE_SIZE 256
#define MAX_ARRAY_ID_SIZE 4

const char  MAIN_FILE_STR[]         = "main.c";
const char  FUNC_DEFS_FILE_STR[]    = "functions.h";
const char  FUNC_FILE_STR[]         = "functions.c";
const char  TYPE_FILE_STR[]         = "typedefs.h";
const char  GLOB_FILE_STR[]         = "global_scope.c";
const char  VARS_FILE_STR[]         = "variables.c";


FILE* mFile     = NULL;
FILE* fdFile    = NULL;
FILE* fFile     = NULL;
FILE* tFile     = NULL;
FILE* gFile     = NULL;
FILE* vFile     = NULL;


const char* const dtypePostfix[] = {
    "VOID",
    "I32",
    "I8",
    "I16",
    "I32",
    "I64",
    "U8",
    "U16",
    "U32",
    "U64",
    "F32",
    "F64",
    "Char",
    "Void",
    "DT_ARRAY",
    "DT_MULTIPLE_TYPES",
    "DT_CUSTOM",
    "DT_MEMBER",
    "DT_ENUM",
    "DT_UNDEFINED"
};



// TODO : move to utils or globals
//#define POSIX 0
#define WINDOWS 1

// 0 on success
#ifdef POSIX == 1
    int newdir(char* const path) {
        mkdir(path, 0777);
    }
#elif WINDOWS == 1
    #include <direct.h>
    int newDir(char* const path) {
        if (_mkdir(path)) {
            return (errno != EEXIST);
        }
        return 0;
    }
#endif





void c_printScope(FILE* file, int level, Scope* const node, Variable* lvalue = NULL);
void c_printVariableDefinition(FILE* file, int level, VariableDefinition* const node, Variable* lvalue = NULL);
void c_printVariableAssignment(FILE* file, int level, VariableAssignment* const node, Variable* lvalue = NULL);
void c_printTypeDefinition(FILE* file, int level, TypeDefinition* const node, Variable* lvalue = NULL);
void c_printTypeInitialization(FILE* file, int level, TypeInitialization* const node, Variable* lvalue = NULL);
void c_printEnumerator(FILE* file, int level, Enumerator* const node, Variable* lvalue = NULL);
void c_printVariable(FILE* file, int level, Variable* const node, Variable* lvalue = NULL);
void c_printFunction(FILE* file, int level, Function* const node, Variable* lvalue = NULL);
void c_printBranch(FILE* file, int level, Branch* const node, Variable* lvalue = NULL);
void c_printWhileLoop(FILE* file, int level, WhileLoop* const node, Variable* lvalue = NULL);
void c_printForLoop(FILE* file, int level, ForLoop* const node, Variable* lvalue = NULL);
void c_printExpression(FILE* file, int level, Expression* const node, Variable* lvalue = NULL);
void c_printWrapperExpression(FILE* file, int level, WrapperExpression* const node, Variable* lvalue = NULL);
void c_printExpressionWrapper(FILE* file, int level, ExpressionWrapper const node, Variable* lvalue = NULL);
void c_printConstExpression(FILE* file, int level, ConstExpression* const node, Variable* lvalue = NULL);
void c_printOperatorExpression(FILE* file, int level, OperatorExpression* const node, Variable* lvalue = NULL);
void c_printUnaryExpression(FILE* file, int level, UnaryExpression* const node, Variable* lvalue = NULL);
void c_printBinaryExpression(FILE* file, int level, BinaryExpression* const node, Variable* lvalue = NULL);
void c_printTernaryExpression(FILE* file, int level, TernaryExpression* const node, Variable* lvalue = NULL);
void c_printFunctionCall(FILE* file, int level, FunctionCall* const node, Variable* lvalue = NULL);
void c_printOperand(FILE* file, int level, Operand* const node, Variable* lvalue = NULL);
void c_printUnaryOperator(FILE* file, int level, UnaryOperator* const node, Variable* lvalue = NULL);
void c_printBinaryOperator(FILE* file, int level, BinaryOperator* const node, Variable* lvalue = NULL);
void c_printTernaryOperator(FILE* file, int level, TernaryOperator* const node, Variable* lvalue = NULL);

void c_printArrayInitialization(FILE* file, int level, ArrayInitialization* const node, Variable* lvalue = NULL);
void c_printStringInitialization(FILE* file, int level, StringInitialization* const node, Variable* lvalue = NULL);

void c_printForeignLangFunction(FILE* file, Function* node);






void printName(FILE* file, INamedEx* node) {
    fprintf(file, "%.*s_%i", node->nameLen, node->name, node->id);
}

void c_printOperandValue(FILE* file, Operand* op) {

    switch (op->cvalue.dtypeEnum) {

        case DT_INT : {

        }

        case DT_INT_32 : {
            fprintf(file, "%i", op->cvalue.i32);
            break;
        }

        case DT_POINTER :
        case DT_INT_64 : {
            fprintf(file, "%i", op->cvalue.i64);
            break;
        }

        case DT_UINT_8:
        case DT_UINT_16:
        case DT_UINT_32:
        case DT_UINT_64: {
            fprintf(file, "%llu", op->cvalue.i64);
            break;
        }

        case DT_FLOAT_32 : {
            fprintf(file, "%.9g", op->cvalue.f32);
            break;
        }

        case DT_FLOAT_64 : {
            fprintf(file, "%.17g", op->cvalue.f64);
            break;
        }

        case DT_STRING : {
            fprintf(file, "\"%s\"", (char*) op->cvalue.str);
            break;
        }

        default : {
            // fprintf(file, "<unknown type>");
        }
    
    }

}

void c_printDataType(FILE* file, const DataTypeEnum dtypeEnum) {

    switch (dtypeEnum) {

        case DT_INT :
            fprintf(file, "int");
            break;
            
        case DT_INT_8 :
            fprintf(file, "int8_t");
            break;
        
        case DT_INT_16:
            fprintf(file, "int16_t");
            break;

        case DT_INT_32 :
            fprintf(file, "int32_t");
            break;
            
        case DT_INT_64 :
            fprintf(file, "int64_t");
            break;

        case DT_UINT_8 :
            fprintf(file, "uint8_t");
            break;
        
        case DT_UINT_16 :
            fprintf(file, "uint16_t");
            break;

        case DT_UINT_32 :
            fprintf(file, "uint32_t");
            break;

        case DT_UINT_64 :
            fprintf(file, "uint64_t");
            break;

        case DT_FLOAT_32 :
            fprintf(file, "float");
            break;
            
        case DT_FLOAT_64 :
            fprintf(file, "double");
            break;

        case DT_POINTER :
            fprintf(file, "void*");
            break;

        default :
            fprintf(file, "%s", (dataTypes + dtypeEnum)->name);
    
    }

}

void c_printDataType(FILE* file, const DataTypeEnum dtypeEnum, void* dtype) {
    
    if (dtypeEnum == DT_POINTER) {

        Pointer* const ptr = ((Pointer*) dtype);
        c_printDataType(file, ptr->pointsToEnum, ptr->pointsTo);
        fputc('*', file);
    
    } else if (dtypeEnum == DT_ARRAY) {
        
        Array* const arr = (Array*) dtype;
        c_printDataType(file, arr->pointsToEnum, arr->pointsTo);

    } else if (dtypeEnum == DT_CUSTOM) {
        
        TypeDefinition* td = (TypeDefinition*) (dtype);
        fprintf(file, "%.*s", td->nameLen, td->name);
        // fprintf(stdout, "%.*s", td->nameLen, td->name)

    } else {

        c_printDataType(file, dtypeEnum);
    
    }

}

void c_printOperator(FILE* file, OperatorEnum opType) {

    switch (opType) {
        
        case OP_UNARY_PLUS :
            fputc('+', file);    
            break;
        
        case OP_UNARY_MINUS :
            fputc('-', file);
            break;
        
        case OP_ADDITION :
            fputc('+', file);
            break;

        case OP_SUBTRACTION :
            fputc('-', file);
            break;

        case OP_MULTIPLICATION :
            fputc('*', file);
            break;

        case OP_DIVISION :
            fputc('/', file);
            break;

        case OP_MODULO :
            fputc('%', file);
            break;

        case OP_GET_ADDRESS :
            fputc('&', file);
            break;

        case OP_GET_VALUE :
            fputc('*', file);            
            break;

        case OP_BITWISE_AND :
            fputc('&', file);
            break;

        case OP_NEGATION :
            fputc('!', file);
            break;

        case OP_EQUAL :
            fputc('=', file);
            fputc('=', file);
            break;
        
        case OP_NOT_EQUAL :
            fputc('!', file);
            fputc('=', file);
            break;

        case OP_LESS_THAN :
            fputc('<', file);
            break;

        case OP_GREATER_THAN :
            fputc('>', file);
            break;

        case OP_LESS_THAN_OR_EQUAL :
            fputc('<', file);
            fputc('=', file);
            break;
        
        case OP_BOOL_AND :
            fputc('&', file);
            fputc('&', file);
            break;

         case OP_BOOL_OR :
            fputc('|', file);
            fputc('|', file);
            break;

        case OP_GREATER_THAN_OR_EQUAL :
            fputc('>', file);
            fputc('=', file);
            break;

        case OP_INCREMENT :
            fputc('+', file);
            fputc('+', file);
            break;

        case OP_DECREMENT :
            fputc('-', file);
            fputc('-', file);
            break;

        case OP_SUBSCRIPT :
            fputc('[', file);
            break;

        case OP_MEMBER_SELECTION :
            fputc('.', file);
            break;

        case OP_DEREFERENCE_MEMBER_SELECTION :
            fputc('-', file);
            fputc('->', file);
            break;

    }

}




// TODO : handle errors
void c_init(char* const dirName) {

    if (newDir(dirName)) {
        return;
    }

    if (chdir(dirName)) {
        return;
    }

    mFile = fopen(MAIN_FILE_STR, "w");
    if (!mFile) {
        return;
    }
    translatorC.mainFile = mFile;

    fdFile = fopen(FUNC_DEFS_FILE_STR, "w");
    if (!fdFile) {
        return;
    }

    fFile = fopen(FUNC_FILE_STR, "w");
    if (!fFile) {
        return;
    }

    tFile = fopen(TYPE_FILE_STR, "w");
    if (!tFile) {
        return;
    }

    vFile = fopen(VARS_FILE_STR, "w");
    if (!vFile) {
        return;
    }

    // fprintf(fFile, "#include \"stdint.h\"\n");
    // fprintf(mFile, "#pragma once\n");

    if (internalFunctionUsed & (1 << (IF_PRINTF - 1))) {
        fprintf(mFile, "#include <stdio.h>\n");    
    }

    if (internalFunctionUsed & (1 << (IF_ALLOC - 1))) {
        fprintf(mFile, "#include <stdlib.h>\n");
    }

    if (1) {
        fprintf(mFile, "#include \"ArrayList.c\"\n");
    }

    fprintf(mFile, "#include \"stdint.h\"\n");
    fprintf(mFile, "#include \"typedefs.h\"\n");
    fprintf(mFile, "#include \"functions.h\"\n");
    fprintf(mFile, "#include \"variables.c\"\n");
    fprintf(mFile, "#include \"functions.c\"\n");
    fprintf(mFile, "int main(int argc, char** argv)");

    //fprintf(fFile, "#pragma once\n");
    //fprintf(fFile, "#include \"globals.h\"\n");

}

void c_exit() {

    fclose(mFile);
    fclose(fFile);
    // fclose(gFile);
    fclose(tFile);
    fclose(vFile);
    fclose(fdFile);

}

void c_printScope(FILE* file, int level, Scope* const node, Variable* lvalue) {

    const int size = (int) node->children.size();

    fputc('{', file);

    for(int i = 0; i < (int) node->children.size(); i++) {
        node->children[i]->print(&translatorC, file, level + 1);
    }

    fputc('}', file);

}

int c_printVariableDefinitionLValue(FILE* file, int level, VariableDefinition* const node) {

    const DataTypeEnum dtype = node->var->cvalue.dtypeEnum;
    if (dtype == DT_ARRAY) {

        Array* const arr = (Array*)node->var->dtype;

        if (arr->flags & IS_ARRAY_LIST) {

            char* tmp = (char*)dtypePostfix[arr->pointsToEnum];
            fprintf(file, "ArrayList%s* %.*s_%i = createArrayList%s(", tmp, node->var->nameLen, node->var->name, node->var->id, tmp);

            if (arr->length->cvalue.hasValue) {
                c_printVariable(file, level, arr->length);
            }

            fputc(')', file);
            fputc(';', file);

            return 1;

        } else if (arr->flags & IS_ALLOCATED) {

            c_printDataType(file, arr->pointsToEnum);
            fprintf(file, "* %.*s_%i", node->var->nameLen, node->var->name, node->var->id);

        } else {

            c_printDataType(file, dtype, arr);
            fprintf(file, " %.*s_%i[%i]", node->var->nameLen, node->var->name, node->var->id, arr->length->cvalue.i64);

        }

    } else {

        c_printDataType(file, node->var->cvalue.dtypeEnum, node->var->dtype);
        fprintf(file, " %.*s_%i", node->var->nameLen, node->var->name, node->var->id);

    }

    return 0;

}

void c_initArray(Expression* var) {
    // only EXT_ARRAY_INITIALIZATION expected or DT_SLICE


}

inline void c_printArrayListLength(FILE* file, Variable* arr) {
    fprintf(file, "%.*s_%i->len", arr->nameLen, arr->name, arr->id);
}

// returns 1 if IF_RENDERED was hit, otherwise 0
int c_printForExpression(FILE* file, Variable* var, Variable* lvalue, int id) {
    
    Expression* ex = var->expression;
    if (!ex) {
        c_printVariable(file, 0, var);
        if (var->cvalue.dtypeEnum == DT_ARRAY) {
            fprintf(file, "[i]");
        }
        return 0;
    }

    switch (ex->type) {

        case EXT_UNARY : {
            
            UnaryExpression* uex = (UnaryExpression*) (var->expression);
            
            c_printOperator(file, uex->operType);
            
            if (c_printForExpression(file, uex->operand, lvalue, id)) {
                c_printVariable(file, 0, lvalue);
                fprintf(file, "[off%i+i]", id);
            }
            
            break;

        }

        case EXT_BINARY : {

            BinaryExpression* bex = (BinaryExpression*)(var->expression);
            if (bex->operType == OP_CONCATENATION) return 1;

            if (c_printForExpression(file, bex->operandA, lvalue, id)) {
                c_printVariable(file, 0, lvalue);
                fprintf(file, "[off%i+i]", id);
            }

            c_printOperator(file, bex->operType);

            c_printForExpression(file, bex->operandB, lvalue, id);
            
            break;

        }

        case EXT_WRAPPER : {
            WrapperExpression* wex = (WrapperExpression*) ex;
            if (wex->operand->flags & IS_RENDERED) return 1;
            c_printForExpression(file, wex->operand, lvalue, id);
            break;
        }

        case EXT_ARRAY_INITIALIZATION : {
            fprintf(file, "%s[i]", var->name);
            break;

        }

        case EXT_STRING_INITIALIZATION : {
            fprintf(file, "%s[i]", var->name);
            break;
        }

        case EXT_SLICE: {
            fprintf(file, "%s[i]", var->name);
            break;
        }

    }

    return 0;

}

int c_printArrayRValue(FILE* file, Variable* lvalue, Variable* var, Variable** arrLen, int id, int* maxId) {

    Expression* ex = var->expression;
    if (!ex) {
        //c_printVariable(file, 0, var);
        if (var->cvalue.dtypeEnum == DT_ARRAY) {
            *arrLen = var->cvalue.arr->length;
        }
        return 0;
    }

    switch (ex->type) {

        case EXT_UNARY : {

            UnaryExpression* uex = (UnaryExpression*) (var->expression);
            
            c_printArrayRValue(file, lvalue, uex->operand, arrLen, id, maxId);
            
            break;
        
        }

        case EXT_BINARY : {
            BinaryExpression* bex = (BinaryExpression*) (var->expression);
            if (bex->operType == OP_CONCATENATION) {

                Variable* lenA = NULL;
                c_printArrayRValue(file, lvalue, bex->operandA, &lenA, id + 1, maxId);
                
                if (*maxId < 0) {
                    *maxId = id;
                    fprintf(file, "int off%i=off0;", id);
                } else {
                    fprintf(file, "int off%i=off%i;", id, id + 1);
                }

                if (lenA) {
                    fprintf(file, "int len%i=", id);
                    c_printVariable(file, 0, lenA);
                    fputc(';', file);

                    fprintf(file, "for (int i = 0; i <");
                    c_printVariable(file, 0, lenA);
                    fprintf(file, "; i++){");
                } else {
                    fprintf(file, "int len%i=len%i;", id, id + 1);
                    fprintf(file, "for (int i = 0; i < len%i; i++){", id + 1);
                }
                c_printVariable(file, 0, lvalue);
                fprintf(file, "[off%i + i]=", id);
                c_printForExpression(file, bex->operandA, lvalue, id);
                fprintf(file, ";}");



                if (lenA) {
                    fprintf(file, "off%i+=", id);
                    c_printVariable(file, 0, lenA);
                    fputc(';', file);
                } else {
                    fprintf(file, "off%i+=len%i;", id, id + 1);
                }



                Variable* lenB = NULL;
                c_printArrayRValue(file, lvalue, bex->operandB, &lenB, id + 1, maxId);

                if (lenB) {
                    fprintf(file, "len%i+=", id);
                    c_printVariable(file, 0, lenB);
                    fputc(';', file);

                    fprintf(file, "for (int i = 0; i <");
                    c_printVariable(file, 0, lenB);
                    fprintf(file, "; i++){");
                } else {
                    fprintf(file, "len%i+=len%i;", id, id + 1);
                    fprintf(file, "for (int i = 0; i < len%i; i++){", id + 1);
                }
                c_printVariable(file, 0, lvalue);
                fprintf(file, "[off%i+i]=", id);
                c_printForExpression(file, bex->operandB, lvalue, id);
                fprintf(file, ";}");



                if (lenA) {
                    fprintf(file, "off%i-=", id);
                    c_printVariable(file, 0, lenA);
                    fputc(';', file);
                } else {
                    fprintf(file, "off%i-=len%i;", id, id + 1);
                }



                var->flags |= IS_RENDERED;

                return 1;
                
            }

            c_printArrayRValue(file, lvalue, bex->operandA, arrLen, id, maxId);
            c_printArrayRValue(file, lvalue, bex->operandB, arrLen, id, maxId);

            break;
        }

        case EXT_WRAPPER : {
            WrapperExpression* wex = (WrapperExpression*) ex;
            return c_printArrayRValue(file, lvalue, wex->operand, arrLen, id, maxId);
            break;
        }

        case EXT_ARRAY_INITIALIZATION : {
            
            ArrayInitialization* init = (ArrayInitialization*) ex;
            
            const int nameLen = 1 + MAX_ARRAY_ID_SIZE + 1;
            var->name = (char*) malloc(nameLen);
            sprintf(var->name, "a%i", var->id);
            
            c_printDataType(file, var->cvalue.dtypeEnum, var->cvalue.arr);
            fprintf(file, " %s[]=", var->name);
            c_printArrayInitialization(file, 0, init);
            fputc(';', file);

            break;

        }

        case EXT_STRING_INITIALIZATION : {
            
            StringInitialization* init = (StringInitialization*) ex;
            
            const int nameLen = 1 + MAX_ARRAY_ID_SIZE + 1;
            var->name = (char*) malloc(nameLen);
            sprintf(var->name, "a%i", var->id);

            c_printDataType(file, init->wideDtype);
            fprintf(file, " %s[]=", var->name);
            c_printStringInitialization(file, 0, init);
            fputc(';', file);

            break;

        }

        case EXT_SLICE : {

            Slice* slice = (Slice*) ex;

            const int nameLen = 1 + MAX_ARRAY_ID_SIZE + 1;
            var->name = (char*) malloc(nameLen);
            sprintf(var->name, "a%i", var->id);

            c_printDataType(file, slice->arr->cvalue.dtypeEnum, slice->arr->cvalue.any);
            fprintf(file, "* %s=", var->name);
            c_printVariable(file, 0, slice->arr);
            fprintf(file, "+");
            c_printVariable(file, 0, slice->bidx);
            fputc(';', file);

            // maybe just add length attribute to Slice
            BinaryExpression* lenEx = new BinaryExpression();
            lenEx->operandA = slice->eidx;
            lenEx->operandB = slice->bidx;
            lenEx->operType = OP_SUBTRACTION;

            Variable* len = new Variable();            
            len->expression = lenEx;

            *arrLen = len;

        }


    }

    return 0;

}

void c_printArray(FILE* file, Variable* lvalue, Variable* rvalue) {

    Variable* var = lvalue->expression ? lvalue : rvalue;

    Expression* ex = lvalue->expression;
    const int exType = ex ? ex->type : EXT_WRAPPER;
    switch (exType) {
        
        case EXT_SLICE : {

            Slice* slice = (Slice*) ex;
            var = rvalue;
            lvalue = slice->arr;

            //fprintf(file, ";{int off0=""0;int len0=");

            fprintf(file, ";{int off0=");
            c_printVariable(file, 0, slice->bidx);

            
            fprintf(file, ";int len0=");
            if (slice->eidx->cvalue.dtypeEnum != DT_UNDEFINED) {
                c_printVariable(file, 0, slice->eidx);
                fputc('-', file);
            } else {
                slice->eidx->cvalue.dtypeEnum = DT_INT_64;
                c_printVariable(file, 0, slice->eidx);
                fputc('+', file);
            }
            c_printVariable(file, 0, slice->bidx);
            fputc(';', file);

            break;

        }

        case EXT_ARRAY_INITIALIZATION : {
            fputc('=', file);
            c_printArrayInitialization(file, 0, (ArrayInitialization*) ex);
            fputc(';', file);
            return;
        }

        case EXT_STRING_INITIALIZATION : {
            fputc('=', file);
            c_printStringInitialization(file, 0, (StringInitialization*) ex);
            fputc(';', file);
            return;
        }

        case EXT_FUNCTION_CALL : {
            // alloc
            fputc('=', file);
            c_printFunctionCall(file, 0, (FunctionCall*) ex, lvalue);
            fputc(';', file);
            return;
        }

        case EXT_BINARY: {
            
            BinaryExpression* bex = (BinaryExpression*) lvalue->expression;
            if (bex->operType == OP_SUBSCRIPT) {

                Array* arr = bex->operandA->cvalue.arr;

                fprintf(file, ";{int off0=");
                c_printArrayListLength(file, bex->operandA);
                fprintf(file, ";int len0=");
                arr->length->cvalue.dtypeEnum = DT_INT_64;
                c_printVariable(file, 0, arr->length);
                //arr->length->cvalue.dtypeEnum = DT_UNDEFINED;
                fputc(';', file);

                const char* postfix = dtypePostfix[arr->pointsToEnum];
                fprintf(file, "arrayListAppendAlloc%s(%.*s_%i,len0);", postfix, bex->operandA->nameLen, bex->operandA->name, bex->operandA->id);
                //c_printVariable(file, 0, arr->length);
                //fprintf(file, ");");

                var = rvalue;
                lvalue = bex->operandA;

                break;
            
            }

        }
    
        default:
            fprintf(file, ";{int off0=""0;int len0=");
            c_printVariable(file, 0, ((Array*) (lvalue->dtype))->length);
            fputc(';', file);
            break;

    }

    Variable* arrLen = NULL;
    int maxId = -2;
    c_printArrayRValue(file, lvalue, var, &arrLen, 1, &maxId);

    fprintf(file, "for (int i = 0; i < len0; i++){");
    c_printVariable(file, 0, lvalue);
    fprintf(file, "[off0+i]=");
    if (c_printForExpression(file, var, lvalue, 0)) {
        c_printVariable(file, 0, lvalue);
        fprintf(file, "[off0+i];");
    }
    fprintf(file, ";}");

    fputc('}', file);
            
    // for startIdx -> endIdx


}

void c_printVariableDefinition(FILE* file, int level, VariableDefinition* const node, Variable* lvalue) {

    if (node->flags & IS_CMP_TIME) return;

    // if (node->flags & IS_CONST) fprintf(file, "const "); TODO!!!

    if (c_printVariableDefinitionLValue(file, level, node)) return;
    
    if (node->var->cvalue.dtypeEnum == DT_ARRAY) {
        
        if (node->var->expression) {
            c_printArray(file, node->var, NULL);
        } else {
            fputc(';', file);
        }

        return;

    }

    if (node->var->expression) {
        fputc('=', file);
        c_printExpression(file, level, node->var->expression, node->var);
        // node->var->expression->print(&translatorC, file, level, node->var);
    } else if (node->var->cvalue.hasValue) {
        fputc('=', file);
        c_printOperandValue(file, node->var);
    }

    /*
    if (node->flags & IS_ARRAY) {
        
        fputc('[', file);
        
        if (node->flags & IS_CONST) {
            // c_printVariable(file, level, node->var->allocSize);
            fprintf(file, "%i", ((Array*) (node->var->dtype))->length);
        }
        
        fputc(']', file);
    
    }
    */

    fputc(';', file);
    
    const DataTypeEnum dtype = node->var->cvalue.dtypeEnum;
    if (dtype == DT_CUSTOM) {

        TypeDefinition* customDtype = (TypeDefinition*) node->var->dtype;
        for (int i = 0; i < customDtype->vars.size(); i++) {
            
            Variable* var = customDtype->vars[i];
           
            if (!(var->expression)) continue;

            fprintf(file, " %.*s_%i.", node->var->nameLen, node->var->name, node->var->id);
            c_printVariable(file, level, var);

            fputc('=', file);

            var->expression->print(&translatorC, file, level);

            fputc(';', file);
    
        }
    }
}

void c_printVariableAssignment(FILE* file, int level, VariableAssignment* const node, Variable* lvalue) {

    if (!node->rvar) {
        node->lvar->print(&translatorC, file, level);
        fputc(';', file);
    }

    if (node->lvar->cvalue.dtypeEnum == DT_ARRAY) {
        c_printArray(file, node->lvar, node->rvar);
        return;
    }

    const int lvalIsSlice = (node->lvar->expression) ? node->lvar->expression->type == EXT_SLICE : 0;
    const int rvarEType = (node->rvar->expression) ? node->rvar->expression->type : -1;

    if (lvalIsSlice) {

        Slice* sliceL = (Slice*)node->lvar->expression;

        if (rvarEType == EXT_SLICE) {
            
            Slice* sliceR = (Slice*)node->rvar->expression;

            fprintf(file, "for(int i=");
            c_printVariable(file, level, sliceL->bidx);
            fprintf(file, ",j=");
            c_printVariable(file, level, sliceR->bidx);
            fprintf(file, "; i <= ");
            c_printVariable(file, level, sliceL->eidx);
            fprintf(file, ";i++, j++){");
            c_printVariable(file, level, sliceL->arr);
            fprintf(file, "[i]=");
            c_printVariable(file, level, sliceR->arr);
            fprintf(file, "[j];}");
            
            return;
        
        } else if (rvarEType == EXT_ARRAY_INITIALIZATION) {
            
            ArrayInitialization* aex = (ArrayInitialization*) (node->rvar->expression);

            fprintf(file, "{int j=");
            c_printVariable(file, level, sliceL->bidx);
            fputc(';', file);

            const int size = aex->attributes.size();
            for (int i = 0; i < size; i++) {
                c_printVariable(file, level, sliceL->arr);
                fprintf(file, "[j]=");
                c_printVariable(file, level, aex->attributes[i]);
                if (i < size - 1) fprintf(file, ";j++;");
            }
            fprintf(file, ";}");

            return;
        
        } 

        fprintf(file, "for(int i=");
        c_printVariable(file, level, sliceL->bidx);
        fprintf(file, ";i<=");
        c_printVariable(file, level, sliceL->eidx);
        fprintf(file, ";i++){");
        c_printVariable(file, level, sliceL->arr);
        fprintf(file, "[i]=");


    } else if (node->rvar->expression->type == EXT_SLICE) {
        
        Slice* slice = (Slice*)node->rvar->expression;

        fprintf(file, "for(int i=");
        c_printVariable(file, level, slice->bidx, node->lvar);
        fprintf(file, ",j=0;i<=");
        c_printVariable(file, level, slice->eidx, node->lvar);
        fprintf(file, ";i++,j++){");
        c_printVariable(file, level, node->lvar);
        fprintf(file, "[j]=");
        c_printVariable(file, level, slice->arr);
        fprintf(file, "[i];}");

        return;

    } else {
    
        node->lvar->print(&translatorC, file, level);
        fputc('=', file);
    
    }

    if (rvarEType == EXT_TYPE_INITIALIZATION) {

        TypeInitialization* tinit = (TypeInitialization*) (node->rvar->expression);
        TypeDefinition* dtype = (TypeDefinition*) node->lvar->def->var->dtype;

        fprintf(file, "((%.*s){", dtype->nameLen, dtype->name);
        const int size = tinit->attributes.size();
        for (int i = 0; i < size; i++) {

            fputc('.', file);
            dtype->vars[i]->print(&translatorC, file, level);
            fputc('=', file);
            c_printVariable(file, level, tinit->attributes[i], node->lvar);
            if (i < size - 1) fputc(',', file);

        }
        fprintf(file, "})");

        return;    
    
    }

    // meh
    if (node->lvar->expression->type == EXT_BINARY) {

        BinaryExpression* bex = (BinaryExpression*) node->lvar->expression;
        Variable* opA = bex->operandA;

        const Value valA = bex->operandA->cvalue;
        if (valA.dtypeEnum == DT_ARRAY && valA.arr->flags & IS_ARRAY_LIST && bex->operType == OP_SUBSCRIPT) {
            fprintf(file, "arrayListInsert%s(%.*s_%i,", dtypePostfix[valA.arr->pointsToEnum], opA->nameLen, opA->name, opA->id);
            c_printVariable(file, level, bex->operandB);
            fputc(',', file);
            c_printVariable(file, level, node->rvar);
            fprintf(file, ");");
            return;
        }
        
    }

    // node->lvar->print(&translatorC, file, level);
    // fputc('=', file);
    c_printVariable(file, level, node->rvar, node->lvar);
    fputc(';', file);
    
    /*
    TypeInitialization* tinit = (TypeInitialization*) (node->rvar->expression);
    TypeDefinition* dtype = (TypeDefinition*) node->lvar->def->var->dtype;
        
    for (int i = 0; i < tinit->attributes.size(); i++) {
            
        node->lvar->print(&translatorC, file, level);
        fputc('.', file);
        dtype->vars[i]->print(&translatorC, file, level);

        fputc('=', file);

        c_printVariable(file, level, tinit->attributes[i], node->lvar);
        fputc(';', file);
        
    }
    */

}

void c_printTypeDefinition(FILE* file, int level, TypeDefinition* const node, Variable* lvalue) {

    fprintf(tFile, "typedef struct %.*s{", node->nameLen, node->name);
    
    const int size = (int) node->vars.size();
    
    for (int i = 0; i < size; i++) {
        
        Variable* const var = node->vars[i];

        const DataTypeEnum dtype = var->cvalue.dtypeEnum;
        if (dtype == DT_ARRAY) {

            Array* const arr = (Array*) var->dtype;

            c_printDataType(tFile, dtype, arr);
            fprintf(tFile, " %.*s_%i[%i]", var->nameLen, var->name, var->id, arr->length);
        
        } else {

            c_printDataType(tFile, var->cvalue.dtypeEnum, var->dtype);
            fprintf(tFile, " %.*s_%i", var->nameLen, var->name, var->id);

        }

        fputc(';', tFile);

    }
    
    fprintf(tFile, "}%.*s;", node->nameLen, node->name);

}

void c_printTypeInitialization(FILE* file, int level, TypeInitialization* const node, Variable* lvalue) {

    fputc('{', file);

    for (int i = 0; i < (int) node->attributes.size() - 1; i++) {
        
        Variable* const var = node->attributes[i];

        if (var->nameLen > 0) {
            fprintf(file, "%.*s_%i = ", var->nameLen, var->name, var->id);
            if (var->expression) {
                var->expression->print(&translatorC, file, level + 1);
            } else {
                c_printOperandValue(file, var);
            }
        } else {
            if (var->expression) {
                var->expression->print(&translatorC, file, level + 1);
            } else {
                c_printOperandValue(file, var);
            }
        }

        fputc(',', file);

    }

    if ((int) node->attributes.size() > 0) {
        
        Variable* const var = node->attributes[(int) node->attributes.size() - 1];

        if (var->nameLen > 0) {
            printf("%.*s_%i = ", var->nameLen, var->name, var->id);
            if (var->expression) {
                var->expression->print(&translatorC, file, level + 1);
            } else {
                c_printOperandValue(file, var);
            }
        } else {
            if (var->expression) {
                var->expression->print(&translatorC, file, level + 1);
            } else {
                c_printOperandValue(file, var);
            }
        }

    }

    fputc('}', file);

}

void c_printStringInitialization(FILE* file, int level, StringInitialization* const node, Variable* lvalue) {

    if (!(node->wideStr)) {
        fprintf(file, "\"%.*s\"", node->rawPtrLen, node->rawPtr);
        return;
    }

    fputc('{', file);

    switch (node->wideDtype) {

        case DT_UINT_8: {
            uint8_t* arr = (uint8_t*) node->wideStr;

            for (int i = 0; i < node->wideLen - 1; i++) {
                fprintf(file, "%llu,", arr[i]);
            }
            fprintf(file, "%llu", arr[node->wideLen - 1]);

            break;
        }

        case DT_UINT_16: {
            uint16_t* arr = (uint16_t*) node->wideStr;

            for (int i = 0; i < node->wideLen - 1; i++) {
                fprintf(file, "%llu,", arr[i]);
            }
            fprintf(file, "%llu", arr[node->wideLen - 1]);

            break;
        }

        case DT_UINT_32: {
            uint32_t* arr = (uint32_t*) node->wideStr;

            for (int i = 0; i < node->wideLen - 1; i++) {
                fprintf(file, "%llu,", arr[i]);
            }
            fprintf(file, "%llu", arr[node->wideLen - 1]);

            break;
        }

        case DT_UINT_64: {
            uint64_t* arr = (uint64_t*) node->wideStr;

            for (int i = 0; i < node->wideLen - 1; i++) {
                fprintf(file, "%llu,", arr[i]);
            }
            fprintf(file, "%llu", arr[node->wideLen - 1]);

            break;
        }

    }

    fputc('}', file);

}

void c_printArrayInitialization(FILE* file, int level, ArrayInitialization* const node, Variable* lvalue) {

    fputc('{', file);

    for (int i = 0; i < (int)node->attributes.size() - 1; i++) {

        Variable* const var = node->attributes[i];

        if (var->nameLen > 0) {
            fprintf(file, "%.*s_%i = ", var->nameLen, var->name, var->id);
            if (var->expression) {
                var->expression->print(&translatorC, file, level + 1);
            }
            else {
                c_printOperandValue(file, var);
            }
        }
        else {
            if (var->expression) {
                var->expression->print(&translatorC, file, level + 1);
            }
            else {
                c_printOperandValue(file, var);
            }
        }

        fputc(',', file);

    }

    if ((int)node->attributes.size() > 0) {

        Variable* const var = node->attributes[(int)node->attributes.size() - 1];

        if (var->nameLen > 0) {
            printf("%.*s_%i = ", var->nameLen, var->name, var->id);
            if (var->expression) {
                var->expression->print(&translatorC, file, level + 1);
            }
            else {
                c_printOperandValue(file, var);
            }
        }
        else {
            if (var->expression) {
                var->expression->print(&translatorC, file, level + 1);
            }
            else {
                c_printOperandValue(file, var);
            }
        }

    }

    fputc('}', file);

}

void c_printEnumerator(FILE* file, int level, Enumerator* const node, Variable* lvalue) {

    return;

    fprintf(tFile, "typedef enum %.*s_%id {", node->nameLen, node->name, node->id);
    
    for (int i = 0; i < node->vars.size(); i++) {
        
        Variable* var = node->vars[i];
        var->print(&translatorC, tFile, level);

        if (var->expression) {
            fputc('=', tFile);
            var->expression->print(&translatorC, tFile, level);
        }

        fputc(',', tFile);

    }

    fprintf(tFile, "} %.*s;", node->nameLen, node->name);

}

void c_printVariable(FILE* file, int level, Variable* const node, Variable* lvalue) {

    /*
    if (node->parentStruct) {
        node->parentStruct->print(&translatorC, file, level);
        fputc('.', file);
    }
    */

    /*
    for (int i = 0; i < node->scopeNames.size(); i++) {
        ScopeName* name = node->scopeNames[i];
        if (name->type != SC_ENUM) {
            fprintf(file, "%.*s_%i.", name->nameLen, name->name, name->id);
        }
    }
    */
    if (node->def && node->def->flags & IS_ARRAY_LIST) {
        fprintf(file, "(%.*s_%i->data)", node->nameLen, node->name, node->id);
        return;
    }
    
    // maybe separate type?
    if (node->def && node->def->flags & IS_CMP_TIME) {
        c_printOperandValue(file, node->def->var);
    } else if (node->nameLen > 0) {
        fprintf(file, "%.*s_%i", node->nameLen, node->name, node->id);
    } else {
        if (node->expression && !node->cvalue.hasValue) node->expression->print(&translatorC, file);
        else c_printOperandValue(file, node);

        // if (node->expression) node->expression->print(&translatorC, file);
        // else c_printOperandValue(file, node);
    }

}

void c_printFunctionDefinition(FILE* file, Function* const node) {
    
    if (node->outArgs.size() > 0) c_printDataType(file, node->outArgs[0]);
    else fprintf(file, "void");

    fprintf(file, " %.*s_%i(", node->nameLen, node->name, node->id);

    for (int i = 0; i < ((int) node->inArgs.size()) - 1; i++) {
        
        VariableDefinition* const varDef = node->inArgs[i];

        c_printVariableDefinitionLValue(file, 0, varDef);
        // c_printDataType(file, varDef->var->cvalue.dtypeEnum, varDef->var->dtype);
        // fprintf(file, " %.*s_%i", varDef->var->nameLen, varDef->var->name, varDef->var->id);

        if (varDef->flags & IS_ARRAY) {
            fputc('[', fFile);
            if (varDef->flags & IS_CMP_TIME) {
                c_printOperandValue(file, varDef->var->cvalue.arr->length);
            }
            fprintf(file, "]");
        }

        fputc(',', file);
    
    }

    if (node->inArgs.size() > 0) {

        VariableDefinition* const varDef = node->inArgs[((int) node->inArgs.size()) - 1];

        // c_printVariableDefinition(fFile, level, varDef);

        c_printDataType(file, varDef->var->cvalue.dtypeEnum, varDef->var->dtype);
        fprintf(file, " %.*s_%i", varDef->var->nameLen, varDef->var->name, varDef->var->id);

        if (varDef->flags & IS_ARRAY) {
            fputc('[', file);
            if (varDef->flags & IS_CMP_TIME) {
                c_printOperandValue(file, varDef->var->cvalue.arr->length);
            }
            fputc(']', file);
        }

    }

    fputc(')', file);

}

void c_printFunction(FILE* file, int level, Function* const node, Variable* lvalue) {

    if (node->internalIdx == -1) return;
    
    c_printFunctionDefinition(fdFile, node);
    fputc(';', fdFile);

    c_printFunctionDefinition(fFile, node);
    node->bodyScope->print(&translatorC, fFile, level);

}


void c_printBranch(FILE* file, int level, Branch* const node, Variable* lvalue) {

    // basic if branche
    // LOOK AT : maybe get rid of '()', as expression should have them already
    fprintf(file, "if (");
    node->expressions[0]->print(&translatorC, file, level);
    fputc(')', file);
    node->scopes[0]->print(&translatorC, file, level);
    
    // if elses
    int i = 1;
    for (; i < node->expressions.size(); i++) {
        fprintf(file, "else if (");
        node->expressions[i]->print(&translatorC, file, level);
        fputc(')', file);
        node->scopes[i]->print(&translatorC, file, level);
    }

    // final else if present
    if (i < node->scopes.size()) {
        fprintf(file, "else");
        node->scopes[(int) node->scopes.size() - 1]->print(&translatorC, file, level);
    }

}

void c_printSwitchCase(FILE* file, int level, SwitchCase* const node, Variable* lvalue) {
    
    fprintf(file, "switch (");
    node->switchExp->print(&translatorC, file, level);
    fprintf(file, "){");

    for (int i = 0; i < node->cases.size(); i++) {
        fprintf(file, "case ");
        node->casesExp[i]->print(&translatorC, file, level);
        fputc(':', file);
        node->cases[i]->print(&translatorC, file, level);
        fprintf(file, "break;");
    }

    fprintf(file, "default:");
    node->elseCase->print(&translatorC, file, level);

    fputc('}', file);

}

void c_printWhileLoop(FILE* file, int level, WhileLoop* const node, Variable* lvalue) {

    fprintf(file, "while(");
    c_printOperand(file, level, node->expression);
    fprintf(file, ")");
    
    c_printScope(file, level, node->bodyScope);

}


void c_printForLoop(FILE* file, int level, ForLoop* const node, Variable* lvalue) {

    fprintf(file, "for(");

    c_printOperand(file, level, node->initEx);
    fputc(';', file);
    
    c_printOperand(file, level, node->conditionEx);
    fputc(';', file);
    
    c_printOperand(file, level, node->actionEx);
    
    fputc(')', file);    
    c_printScope(file, level, node->bodyScope);

}

void c_printLoop(FILE* file, int level, Loop* node, Variable* lvalue) {

    fprintf(file, "for(");

    Variable* idxVar;
    Variable* sizeVar;

    if (node->idx) {
        idxVar = node->idx;
        c_printVariable(file, level, node->idx);
        fputc(';', file);
    } else {
        idxVar = node->idxDef->var;
        c_printVariableDefinition(file, level, node->idxDef);
    }

    c_printVariable(file, level, idxVar);
    fputc('<', file);
    // c_printOperand(file, level, node->array->allocSize);
    fprintf(file, "%i;", ((Array*) node->array->dtype)->length);
    //c_printVariable(file, level, ((Array*) node->array); 
    
    //fputc(';', file);
    
    c_printVariable(file, level, idxVar);
    fprintf(file, "++");
    
    fputc(')', file);    
    c_printScope(file, level, node->bodyScope);

}

void c_printReturnStatement(FILE* file, int level, ReturnStatement* const node, Variable* lvalue) {

    fprintf(file, "return ");
    
    if (node->vars.size() > 0) {
        node->vars[0]->print(&translatorC, file, level);
    }

    fputc(';', file);

}

void c_printContinueStatement(FILE* file, int level, ContinueStatement* const node, Variable* lvalue) {

    fprintf(file, "continue;");

}

void c_printBreakStatement(FILE* file, int level, BreakStatement* const node, Variable* lvalue) {

    fprintf(file, "break;");

}

void c_printGotoStatement(FILE* file, int level, GotoStatement* const node, Variable* lvalue) {

    fprintf(file, "goto %.*s;", node->nameLen, node->name);

}

void c_printLabel(FILE* file, int level, Label* const node, Variable* lvalue) {

    fprintf(file, "%.*s:", node->nameLen, node->name);

}

void c_printNamespace(FILE* file, int level, Namespace* const node, Variable* lvalue) {

    for (int i = 0; i < node->children.size(); i++) {
        node->children[i]->print(&translatorC, vFile, level);
    }

}

void c_printExpression(FILE* file, int level, Expression* const node, Variable* lvalue) {

    switch (node->type) {
        case EXT_WRAPPER:       
            c_printWrapperExpression(file, level, (WrapperExpression*) node, lvalue);
            break;
        case EXT_UNARY:
            c_printUnaryExpression(file, level, (UnaryExpression*) node, lvalue);
            break;
        case EXT_BINARY:
            c_printBinaryExpression(file, level, (BinaryExpression*) node, lvalue);
            break;
        case EXT_TERNARY:
            c_printTernaryExpression(file, level, (TernaryExpression*) node, lvalue);
            break;
        case EXT_FUNCTION_CALL: 
            c_printFunctionCall(file, level, (FunctionCall*) node, lvalue);
            break;
        case EXT_ARRAY_INITIALIZATION:
            c_printTypeInitialization(file, level, (TypeInitialization*)node, lvalue);
            break;
        case EXT_TYPE_INITIALIZATION: 
            c_printTypeInitialization(file, level, (TypeInitialization*) node, lvalue);
            break;
    }

}

void c_printWrapperExpression(FILE* file, int level, WrapperExpression* const node, Variable* lvalue) {
    
    node->operand->print(&translatorC, file, level);

}

void c_printExpressionWrapper(FILE* file, int level, ExpressionWrapper* const node, Variable* lvalue) {

    node->operand->print(&translatorC, file, level);
    fputc(';', file);

}

void c_printConstExpression(FILE* file, int level, ConstExpression* const node, Variable* lvalue) {

}

void c_printOperatorExpression(FILE* file, int level, OperatorExpression* const node, Variable* lvalue) {

}

void c_printUnaryExpression(FILE* file, int level, UnaryExpression* const node, Variable* lvalue) {
    
    fputc('(', file);

    c_printOperator(file, node->operType);
    // node->oper->print(&translatorC, level);

    if (node->operand->unrollExpression && node->operand->expression) node->operand->expression->print(&translatorC, file);
    else node->operand->print(&translatorC, file, level);

    fputc(')', file);

}

void c_printBinaryExpression(FILE* file, int level, BinaryExpression* const node, Variable* lvalue) {

    /*
    if (node->operandA->cvalue.dtypeEnum == DT_ARRAY && node->operandA->cvalue.arr->flags & IS_ARRAY_LIST) {
        
        if (node->operType == OP_SUBSCRIPT) {

            fputc('(', file);

            Variable* var = node->operandA;
            fprintf(file, "%.*s_%i->data[", var->nameLen, var->name, var->id);
            
            if (node->operandB->unrollExpression && node->operandB->expression) node->operandB->expression->print(&translatorC, file);
            else node->operandB->print(&translatorC, file, level);
            
            fputcf(']', file);
            fputc(')', file);
            
            return;
        
        }

    }
    */

    fputc('(', file);
    
    if (node->operandA->unrollExpression && node->operandA->expression) node->operandA->expression->print(&translatorC, file);
    else node->operandA->print(&translatorC, file, level);

    c_printOperator(file, node->operType);
    // node->oper->print(&translatorC, 1);
    
    if (node->operandB->unrollExpression && node->operandB->expression) node->operandB->expression->print(&translatorC, file);
    else node->operandB->print(&translatorC, file, level);

    if (node->operType == OP_SUBSCRIPT) fputc(']', file);

    fputc(')', file);

}

void c_printTernaryExpression(FILE* file, int level, TernaryExpression* const node, Variable* lvalue) {

}

void c_printStatement(FILE* file, int level, Statement* const node, Variable* lvalue) {

    node->op->print(&translatorC, file, level);
    fputc(';', file);

}

void c_printFunctionCall(FILE* file, int level, FunctionCall* const node, Variable* lvalue) {

    if (node->fcn->internalIdx <= 0) {
        
        fprintf(file, "%.*s_%i(", node->nameLen, node->name, node->fcn->id);
    
    } else if (node->fcn->internalIdx == IF_ALLOC) {
        // first

        Variable* var = node->inArgs[0];
        const DataTypeEnum dtype = var->cvalue.dtypeEnum;

        fprintf(file, "malloc(sizeof(");

        if (dtype == DT_CUSTOM) {

            TypeDefinition* customDtype = (TypeDefinition*) (var->dtype);
            fprintf(file, "%.*s", customDtype->nameLen, customDtype->name);


            fprintf(file, "));");

            TypeDefinition* lvalueDtype = (TypeDefinition*)((Pointer*)lvalue->dtype)->pointsTo;
            TypeInitialization* typeInit = (TypeInitialization*)((WrapperExpression*)(var->expression))->operand->expression;
            for (int i = 0; i < typeInit->attributes.size(); i++) {
                c_printVariable(file, level, lvalue);
                fputc('-', file);
                fputc('>', file);
                c_printVariable(file, level, lvalueDtype->vars[typeInit->idxs[i]]);
                fputc('=', file);
                c_printExpression(file, level, typeInit->attributes[i]->expression);
                fputc(';', file); // overprodeces ';', but who cares
            }

        } else if (dtype == DT_ARRAY) {

            Array* arr = var->cvalue.arr;
            c_printDataType(file, arr->pointsToEnum);
            
            fprintf(file, ")*");
            c_printVariable(file, level, arr->length);

            fprintf(file, ");");

            if (var->expression) {
                lvalue->expression = var->expression;
                c_printArray(file, lvalue, var);
            }

        } else {

            c_printDataType(file, dtype);
            fprintf(file, "));");
            if (var->cvalue.hasValue) {
                c_printVariable(file, level, lvalue);
                fputc('=', file);
                c_printVariable(file, level, var);
            }

        }
        
        return;
    
    } else {
        
        fprintf(file, "%.*s(", node->nameLen, node->name);
    
    }

    for (int i = 0; i < ((int) node->inArgs.size()) - 1; i++) {
        node->inArgs[i]->print(&translatorC, file, 0);
        fputc(',', file);
        fputc(' ', file);
        //Variable* const var = inArgs[i];
        //printf("%s %.*s, ", (dataTypes + var->dataTypeEnum)->word, var->nameLen, var->name);
    }
    if ((int) node->inArgs.size() - 1 >= 0) {
        node->inArgs[node->inArgs.size() - 1]->print(&translatorC, file, 0);
        //Variable* const var = inArgs[inArgs.size() - 1];
        //printf("%s %.*s", (dataTypes + var->dataTypeEnum)->word, var->nameLen, var->name);
    }
    fputc(')', file);

}

void c_printOperand(FILE* file, int level, Operand* const node, Variable* lvalue) {

    // maybe separate type?
    if (node->expression) {
        node->expression->print(&translatorC, file,  level);
    } else {
        c_printOperandValue(file, node);
    }

}

void c_printUnaryOperator(FILE* file, int level, UnaryOperator* const node, Variable* lvalue) {

}

void c_printBinaryOperator(FILE* file, int level, BinaryOperator* const node, Variable* lvalue) {

}

void c_printTernaryOperator(FILE* file, int level, TernaryOperator* const node, Variable* lvalue) {

}



Translator translatorC {
    mFile,
    &c_init,
    &c_exit,
    &c_printScope,
    &c_printVariableDefinition,
    &c_printVariableAssignment,
    &c_printTypeDefinition,
    &c_printTypeInitialization,
    &c_printStringInitialization,
    &c_printArrayInitialization,
    &c_printEnumerator,
    &c_printVariable,
    &c_printFunction,
    &c_printBranch,
    &c_printSwitchCase,
    &c_printWhileLoop,
    &c_printForLoop,
    &c_printLoop,
    &c_printReturnStatement,
    &c_printContinueStatement,
    &c_printBreakStatement,
    &c_printGotoStatement,
    &c_printLabel,
    &c_printNamespace,
    &c_printExpression,
    &c_printWrapperExpression,
    &c_printExpressionWrapper,
    &c_printConstExpression,
    &c_printOperatorExpression,
    &c_printUnaryExpression,
    &c_printBinaryExpression,
    &c_printTernaryExpression,
    &c_printStatement,
    &c_printFunctionCall,
    &c_printOperand,
    &c_printUnaryOperator,
    &c_printBinaryOperator,
    &c_printTernaryOperator
};
