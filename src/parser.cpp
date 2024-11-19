#pragma once

#include <ctype.h>
#include <vector>
#include <thread>

#include <stdlib.h>
#include <locale.h>
#include <stdio.h>

#include <iostream>
#include <string>
#include <locale>
#include <codecvt>
#include <wchar.h>

#include "globals.h"
#include "parser.h"
#include "syntax.h"
#include "interpreter.h"
#include "file_driver.h"
#include "utils.h"
#include "logger.h"
#include "error.h"

#define IS_HEX(ch) (((ch) >= '0' && (ch) <= '9') || ((ch) >= 'A' && (ch) <= 'F') || ((ch) >= 'a' && (ch) <= 'f'))
#define IS_NUMBER(ch) ((ch) >= '0' && (ch) <= '9')
#define IS_ALLOWED_VARIABLE_CHAR(ch) (((ch) >= '0' && (ch) <= '9') || (((ch) >= 'a' && (ch) <= 'z') || ((ch) >= 'A' && (ch) <= 'Z') || (ch) == '_'))
#define IS_ALLOWED_FIRST_VARIABLE_CHAR(ch) ((((ch) >= 'a' && (ch) <= 'z') || ((ch) >= 'A' && (ch) <= 'Z') || (ch) == '_'))
#define COPY_LOC(dest, src) {(dest)->idx = (src)->idx; (dest)->line = (src)->line;}
#define ASSIGN_ID(var) {varId++; (var)->id = varId;}
#define CHAR_CAT(chA, chB) ((chB) + ((chA) << 8 ))
#define CMP_TWO_CHARS(str, ch) ((*(str) == ((char) ch)) && (*((str) + 1) == ((char) (ch >> 8))))

namespace Parser {

    int selectKeyWord(const KeyWord* keyWords, const int keyWordsCount, char* const str, int* const idx);
    int selectDataTypeKeyWord(char* const str, int* const idx);

    int parseScope(Scope* scope, char* const str, Location* const loc, const ScopeType global = SC_COMMON, const char endWithStatement = 0);
    int parseKeyWord(KeyWordType keyWord, Scope* scope, char* const str, Location* const loc, uint64_t param = 0);
    int parseDirectiveKeyWord(KeyWordType keyWord, Scope* scope, char* const str, Location* const loc, uint64_t param = 0);
    int parseVariableDefinition(Scope* scope, char* const str, Location* const loc, uint64_t param, VariableDefinition** outVarDef);
    int parseExpression(Variable* var, char* const str, Location* const loc, const uint16_t endChar = STATEMENT_END, const int useKeyWordAsEnd = 0);
    int parseStringLiteral(Scope* scope, char* const str, Location* const loc);
    int parseStringLiteral(char* const str, Location* const loc, StringInitialization** initOut);
    int parseTypeInitialization(Scope* scope, char* const str, Location* loc, TypeInitialization** dtypeInit);
    DataTypeEnum parseNumberLiteral(char* const str, int* idx, uint64_t* out);
    int parseCharLiteral(char* const str, Location* idx, uint64_t* out);
    int parseLanguageTag(char* const str, Location* loc);
    int parseDataTypeDecorators(const DataTypeEnum dtype, Scope* scope, char* const str, Location* const loc, Variable* var);
    int parseRValue(Variable* outVar, char* str, Location* loc, Scope* scope, uint16_t endChar, DataTypeEnum mainDtype = DT_VOID);

    int processDataType(const DataTypeEnum dtype, Scope* scope, char* const str, Location* const loc, uint64_t param = 0, uint16_t endChar = STATEMENT_END, VariableDefinition** outVarDef = NULL, const int assignId = 1);

    const KeyWord* findKeyWord(const KeyWord* keyWords, const int keyWordsCount, char* const name, const int nameLen);
    uint32_t findDataType(char* const name, const int nameLen);

    int findBlockEnd(char* const str, Location* loc, const char beginCh, const char endCh);

    Location* getLocationStamp(Location* loc);

    void copy(Variable* dest, Variable* src);
    void copy(Variable* dest, Variable* src);

    void appendScope(Scope* scA, Scope* scB);
    void appendPrefixScope(Scope* scA, Scope* scB);

    void stripWrapperExpressions(Variable** op);



    // TODO : change when multi thread support will be added!!!
    //      used in ASSIGN_ID macro
    uint32_t varId = 0;
    // to assign each array/string initialization an id, so
    // render can easily create separate variable for them 
    uint32_t arrId = 0;



    // huh
    Function* currentFunction = NULL;
    SyntaxNode* currentLoop = NULL;
    Scope* fileRootScope = NULL; // maybe store it in Loacation
    

    // data types has to go first, if it will be not possible for any reason,
    // seek for DATA_TYPES_COUNT and update if necessary.
    const KeyWord keyWords[] = {
        {0, KWS_VOID},
        {1, KWS_INT},
        {2, KWS_INT_8},
        {3, KWS_INT_16},
        {4, KWS_INT_32},
        {5, KWS_INT_64},
        {6, KWS_UINT_8},
        {7, KWS_UINT_16},
        {8, KWS_UINT_32},
        {9, KWS_UINT_64},
        {10, KWS_FLOAT_32},
        {11, KWS_FLOAT_64},
        {12, KWS_STRING},
        {13, KWS_CMP_TIME},
        {14, KWS_CONST},
        {15, KWS_FUNCTION},
        {16, KWS_IF},
        {17, KWS_FOR},
        {18, KWS_WHILE},
        {19, KWS_GOTO},
        {20, KWS_ENUM},
        {21, KWS_TYPE_DEF},
        {22, KWS_RETURN},
        {23, KWS_CONTINUE},
        {24, KWS_LOOP},
        {25, KWS_BREAK},
        {26, KWS_USING},
        {27, KWS_NAMESPACE},
        {28, KWS_ELSE},
        {29, KWS_SWITCH_CASE},
        {30, KWS_SWITCH_CASE_CASE},
        {31, KWS_ALLOC},
    };

    const int KEY_WORDS_COUNT = sizeof(keyWords) / sizeof(KeyWord);



    const KeyWord directiveKeyWords[] = {
        {0, DKWS_LANG_DEF},
        {1, DKWS_IMPORT}
    };

    const int DIRECTIVE_KEY_WORDS_COUNT = sizeof(directiveKeyWords) / sizeof(KeyWord);



    // on edit update where used!!!
    const KeyWord langDefKeyWords[] = {
        {8, "command"},
        {9, "fcn_format"},
        {10, "fcn_format_in_args"},
        {11, "fcn_format_out_args"},
        {0, KWS_VOID},
        {1, KWS_INT},
        {2, KWS_INT_32},
        {3, KWS_INT_64},
        {4, KWS_FLOAT_32},
        {5, KWS_FLOAT_64},
        {6, KWS_STRING},
        {7, KWS_CMP_TIME}
    };

    const int LANG_DEF_KEY_WORDS_COUNT = sizeof(langDefKeyWords) / sizeof(KeyWord);






    // it's a bit weird, but let's keep functions here as it will be 
    // easier to adjust them in the future. There is an option to use
    // maps, but they are dynamic, so switch case will be the ?better?

    OperatorEnum findUnaryOperator(uint32_t word) {

        switch (word) {

            case '+'            : return OP_UNARY_PLUS;
            case '-'            : return OP_UNARY_MINUS;
            case POINTER_SYMBOL : return OP_GET_VALUE;
            case ADDRESS_SYMBOL : return OP_GET_ADDRESS;
            case '++'           : return OP_INCREMENT;
            case '--'           : return OP_DECREMENT;
            case '!'            : return OP_NEGATION;
            default             : return OP_NONE;
        
        }

    }

    OperatorEnum findPostfixUnaryOperator(uint32_t word) {

        switch (word) {

            case '++'   : return OP_INCREMENT;
            case '--'   : return OP_DECREMENT;
            default     : return OP_NONE;
        
        }

    }

    OperatorEnum findBinaryOperator(uint32_t word) {

        switch (word) {

            case '+'    : return OP_ADDITION;
            case '-'    : return OP_SUBTRACTION;
            case '*'    : return OP_MULTIPLICATION;
            case '/'    : return OP_DIVISION;
            case '%'    : return OP_MODULO;
            case '<'    : return OP_LESS_THAN;
            case '>'    : return OP_GREATER_THAN;
            case '<='   : return OP_LESS_THAN_OR_EQUAL;
            case '>='   : return OP_GREATER_THAN_OR_EQUAL;
            case '=='   : return OP_EQUAL;
            case '!='   : return OP_NOT_EQUAL;
            case '&&'   : return OP_BOOL_AND;
            case '||'   : return OP_BOOL_OR;
            case '..'   : return OP_CONCATENATION;
            case '.'    : return OP_MEMBER_SELECTION;
            default     : return OP_NONE;
        
        }

    }






    // TODO : make it 'static' if its even posible
    // LOOK AT: move to the syntax.h/.c ?
    Function internalFunctions[] = {
        
        Function(
            SyntaxNode::root,
            (char *) IFS_PRINTF,
            sizeof(IFS_PRINTF) - 1,
            std::vector<VariableDefinition*>({
                new VariableDefinition(new Variable(SyntaxNode::root, DT_STRING), 0),
                new VariableDefinition(new Variable(SyntaxNode::root, DT_MULTIPLE_TYPES), 0)
            }),
            std::vector<DataTypeEnum>(DT_VOID),
            IF_PRINTF
        ),

        Function(
            SyntaxNode::root,
            (char *) IFS_ALLOC,
            sizeof(IFS_ALLOC) - 1,
            std::vector<VariableDefinition*>({
                new VariableDefinition(new Variable(SyntaxNode::root, DT_MULTIPLE_TYPES), 0)
            }),
            std::vector<DataTypeEnum>(DT_POINTER),
            IF_ALLOC
        )
    
    };


    /*
    // internal Variables such as null, true, false etc...
    Variable* internalVariables[] = {
        VariableDefinition(new Variable(SyntaxNode::root, DT_POINTER, (char*) IVS_NULL, sizeof(IVS_NULL) - 1), IS_CMP_TIME).var
    };

    const int internalVariablesCount = sizeof(internalVariables) / sizeof(Variable*);
    */



    

    int parseFile(char* const flname, Scope* scope, const ScopeType globalScope, Scope* fileScope = NULL) {
        
        char* buffer;
        if (FileDriver::readFile(flname, &buffer)) {
            return 1;
        }

        Location location = {
            {flname, buffer},
            1,
            0
        };

        fileRootScope = fileScope ? fileScope : scope;
        return parseScope(scope, buffer, &location, globalScope);

    }

    int parse(char* const flname) {

        // NOTE : future parallelism in mind

        char* dirStr = flname;
        int dirLen = Utils::stripDir(flname);



        // Parsing itself
        //

        Logger::log(Logger::INFO, "Parsing...\n");

        SyntaxNode::root = new Scope;
        SyntaxNode::root->scope = NULL;

        internalFunctionUsed = 0;

        const int err = parseFile(flname, SyntaxNode::root, SC_GLOBAL);
        if (err != Err::OK) return err;

        while (SyntaxNode::imports.size() > 0) {
            
            ImportStatement* import = SyntaxNode::imports.back();
            SyntaxNode::imports.pop_back();

            String fname = import->fname;// imports[i]->fname;
            fname[fname.len] = EOS; // TODO : ?

            // ! TODO !
            char buff[256];
            
            int j = 0;
            for (; j < dirLen; j++) buff[j] = dirStr[j];
            for (; j < dirLen + fname.len; j++) buff[j] = fname[j - dirLen];
            buff[j] = EOS;
            
            switch (import->keyWord) {
                
                case -1 : {
                    
                    Scope* sc = new Scope;
                    sc->scope = import->root;
                    const int err = parseFile(buff, sc, SC_GLOBAL, import->root);
                    if (err != Err::OK) return err;

                    appendPrefixScope(import->root, sc);
                    // SyntaxNode::root = sc;

                    break;
                
                }

                case KW_NAMESPACE : {

                    Namespace* sc = new Namespace;
                    const int err = parseFile(buff, sc, SC_GLOBAL);
                    if (err != Err::OK) return err;

                    sc->name = import->param;
                    sc->nameLen = import->param.len;
                    sc->scope = import->root;

                    import->root->children.insert(import->root->children.begin(), sc);
                    import->root->namespaces.insert(import->root->namespaces.begin(), sc);

                    break;

                }


            }

        }



        // Assembling part
        // All parallel parsed nodes will be assembled into one tree
        // ...
        // ...
        //

        Logger::log(Logger::INFO, "Assembling...\n");

        return 0;
    
    }

    // the begin char has to be already skipped
    int parseScope(Scope* scope, char* const str, Location* const loc, const ScopeType scopeType, const char endAsStatement) {

        while (1) {

            const int oldIdx = loc->idx - 1;
            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) {

                if (scopeType == SC_GLOBAL) return Err::OK;

                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_END_OF_FILE), loc);
                return Err::UNEXPECTED_END_OF_FILE;

            }

            const char ch = str[loc->idx];

            if ((endAsStatement && str[oldIdx] == STATEMENT_END)) {

                return Err::OK;
                
            } else if (ch == STATEMENT_END) {

                loc->idx++;
            
            } else if (ch == SCOPE_BEGIN) {

                Scope *newScope = new Scope;
                newScope->scope = scope;

                loc->idx++;
                const int err = parseScope(newScope, str, loc);
                if (err < 0) return err;

                scope->children.push_back(newScope);
            
            } else if (ch == SCOPE_END) {
                
                if (scopeType != SC_GLOBAL) {
                    loc->idx++;
                    return Err::OK;
                }

                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 1, "");
                return Err::UNEXPECTED_SYMBOL;

            } else if (ch == STRING_LITERAL) {
                // two things can happen : print to stdout OR expression

                loc->idx++;
                const int err = parseStringLiteral(scope, str, loc);
                if (err < 0) {

                    Variable* var = new Variable(loc);
                    var->scope = scope;

                    const int err = parseExpression(var, str, loc);
                    if (err < 0) return err;

                    // LOOK AT : maybe add to vars as well
                    Scope::root->children.push_back(var);
                
                }
            
            } else if (ch == '#') {
                // compiler directives

                loc->idx++;

                const int keyWordIdx = selectKeyWord(directiveKeyWords, DIRECTIVE_KEY_WORDS_COUNT, str, &(loc->idx));
                if (keyWordIdx >= 0) {

                    const int err = parseDirectiveKeyWord((KeyWordType) directiveKeyWords[keyWordIdx].type, scope, str, loc);
                    if (err < 0) return err;
                
                }

            } else if (ch == '[') {
                // interoperability - scope
                
                const int tagLen = parseLanguageTag(str, loc);
                char* const tagStr = str + loc->idx - tagLen - 1;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                if (str[loc->idx] != SCOPE_BEGIN) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
                    return Err::UNEXPECTED_SYMBOL;
                }

                loc->idx++;

                char* const codeStr = str + loc->idx;
                if (findBlockEnd(str, loc, SCOPE_BEGIN, SCOPE_END) < 0) return Err::UNEXPECTED_END_OF_FILE;
                const int codeLen = str + loc->idx - codeStr - 1;

                CodeBlock* codeBlock = new CodeBlock();
                codeBlock->tagStr = tagStr;
                codeBlock->tagLen = tagLen;
                codeBlock->codeStr = codeStr;
                codeBlock->codeLen = codeLen;

                SyntaxNode::codeBlocks.push_back(codeBlock);

            } else if (ch == LABEL_BEGIN) {

                loc->idx++;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                Label* label = new Label;
                label->loc = getLocationStamp(loc);
                label->scope = scope;
                label->name = str + loc->idx;
                label->nameLen = Utils::findVarEnd(str + loc->idx);

                scope->children.push_back(label);

                loc->idx += label->nameLen;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                if (str[loc->idx] != LABEL_END) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 1);
                    return Err::UNEXPECTED_SYMBOL;
                }

                loc->idx++;

            } else {

                const int keyWordIdx = selectKeyWord(keyWords, KEY_WORDS_COUNT, str, &(loc->idx));
                if (keyWordIdx >= 0) {

                    const int err = parseKeyWord((KeyWordType)keyWords[keyWordIdx].type, scope, str, loc);
                    if (err < 0) return err;
                
                } else {
                    // if not keyword maybe expression, variable assignment or custom dataType

                    int err;
                    int linesSkipped = 0;
                    Location *const startLoc = getLocationStamp(loc);

                    char *const word = str + loc->idx;
                    const int wordLen = Utils::findVarEnd(word);

                    loc->idx += wordLen;

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                    const char tmpCh = str[loc->idx];
                    if (!IS_ALLOWED_VARIABLE_CHAR(tmpCh) && tmpCh != POINTER_SYMBOL) {

                        COPY_LOC(loc, startLoc);

                        Variable* const var = new Variable;
                        var->scope = scope;
                        
                        const int err = parseExpression(var, str, loc, (1 << 8) | '=' + (STATEMENT_END << 8 ));
                        if (err < 0) return err;

                        if (str[loc->idx - 1] == '=') {
                            // assignment

                            VariableAssignment* const varAssignment = new VariableAssignment(loc);
                            varAssignment->loc->idx--;
                            varAssignment->scope = scope;
                            varAssignment->lvar = var;
                            varAssignment->rvar = new Variable;
                            varAssignment->rvar->scope = scope;

                            const int err = parseExpression(varAssignment->rvar, str, loc);
                            if (err < 0) return err;

                            if (var->cvalue.dtypeEnum == DT_ARRAY) {
                                SyntaxNode::arraysAllocations.push_back(varAssignment);
                            }

                            SyntaxNode::variableAssignments.push_back(varAssignment);
                            scope->children.push_back(varAssignment);

                        } else {
                            // just expression

                            Statement* st = new Statement;
                            st->op = var;
                            st->loc = getLocationStamp(loc);

                            scope->children.push_back(st);
                            SyntaxNode::statements.push_back(st);

                        }

                    } else {

                        VariableDefinition* varDef;
                        varDef->dtypeName = word;
                        varDef->dtypeNameLen = wordLen;

                        err = processDataType(DT_CUSTOM, scope, str, loc, 0, STATEMENT_END, &varDef, DT_CUSTOM);
                        if (err < 0) return err;

                        varDef->dtypeName = word;
                        varDef->dtypeNameLen = wordLen;
                        varDef->loc = startLoc;

                        scope->children.push_back(varDef);
                        // scope->defs.push_back(varDef);
                        SyntaxNode::customDataTypesReferences.push_back(varDef);
                    
                    }
                
                }
            
            }
        
        }
    
    }

    // returns tag length
    int parseLanguageTag(char* const str, Location* loc) {

        loc->idx++;

        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

        const int idx = Utils::findWordEndOrChar(str + loc->idx, ']');
        loc->idx += idx;
        
        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
        
        if (str[loc->idx] != ']') {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
            return Err::UNEXPECTED_SYMBOL;
        }

        loc->idx++;

        return idx;

    }

    int findBlockEnd(char* const str, Location* loc, const char beginCh, const char endCh) {
        
        int toClose = 1;

        while (1) {

            const char ch = str[loc->idx];

            if (ch == '\n') {
                loc->line++;
            } else if (ch == beginCh) {
                toClose++;
            } else if (ch == endCh) {
                    toClose--;
                    if (toClose <= 0) break;            
            } else if (ch == '\0') {
                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_END_OF_FILE), loc);
                return Err::UNEXPECTED_END_OF_FILE;
            }

            loc->idx++;
        
        }

        loc->idx++;
        return Err::OK;

    }

    int parseVariableDefinition(Scope* scope, char* const str, Location* const loc, uint64_t param, uint16_t endChar, VariableDefinition **outVarDef) {

        // [*qualifire] [datatype] [name] = [expression / type definition];

        Location* defLoc;
        int dtype;
        char* dtypeName;
        int dtypeLen;

        const int startIdx = loc->idx;
        const int keyWord = selectKeyWord(keyWords, KEY_WORDS_COUNT, str, &(loc->idx));
        if (keyWord == KW_CONST || keyWord == KW_CMP_TIME) {
            
            param = param | ((keyWord == KW_CONST) ? KW_CONST : KW_CMP_TIME);
            
            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

            defLoc = getLocationStamp(loc);

            dtypeName = str + loc->idx;
            dtypeLen = Utils::findVarEnd(dtypeName);

            loc->idx += dtypeLen;

        } else {

            defLoc = getLocationStamp(loc);

            dtypeName = str + startIdx;

            if (keyWord < 0) {
                dtypeLen = Utils::findVarEnd(dtypeName);
                loc->idx += dtypeLen;
            } else {
                dtypeLen = loc->idx - startIdx;
            }

        }

        if (dtypeLen == 0) {
            // TODO : error
            return Err::UNEXPECTED_SYMBOL;
        }

        dtype = findDataType(dtypeName, dtypeLen);
        dtype = (dtype == DT_UNDEFINED) ? DT_CUSTOM : dtype;

        VariableDefinition* varDef;
        const int err = processDataType((DataTypeEnum) dtype, scope, str, loc, param, endChar, &varDef);
        if (err) return err;

        varDef->loc = defLoc;

        if (dtype == DT_CUSTOM) {
            // custom dtype
            
            varDef->dtypeName = dtypeName;
            varDef->dtypeNameLen = dtypeLen;

            SyntaxNode::customDataTypesReferences.push_back(varDef);
            
        }

        *outVarDef = varDef;
        return Err::OK;

    }

    // already without starting '"'
    int parseStringLiteral(Scope *scope, char *const str, Location *const loc) {

        // "asdasd";
        // "%i\n" 3;

        const int startIdx = loc->idx;

        while (1) {

            const char ch = str[loc->idx];
            if (ch == '"') {

                internalFunctionUsed = internalFunctionUsed | (1 << (IF_PRINTF - 1));

                Function* const fcn = internalFunctions + (IF_PRINTF - 1);

                FunctionCall* fcnCall = new FunctionCall;
                fcnCall->fcn = fcn;
                fcnCall->name = fcn->name;
                fcnCall->nameLen = fcn->nameLen;

                Variable* strVar = new Variable(scope, DT_STRING, loc);
                const int strVarNameLen = loc->idx - startIdx;
                strVar->cvalue.str = malloc(strVarNameLen + 1);
                if (!strVar->cvalue.str) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::MALLOC));
                    return Err::MALLOC;
                }
                memcpy(strVar->cvalue.str, str + startIdx, strVarNameLen);
                ((char *)(strVar->cvalue.str))[strVarNameLen] = '\0';

                fcnCall->inArgs.push_back(strVar);

                Variable *operand = new Variable;
                operand->scope = scope;
                operand->expression = fcnCall;
                operand->unrollExpression = 1;
                operand->cvalue.str = NULL;
                operand->loc = getLocationStamp(loc);

                SyntaxNode::fcnCalls.push_back(operand);

                ExpressionWrapper *exWrapper = new ExpressionWrapper;
                exWrapper->operand = operand;
                exWrapper->operand->expression = fcnCall;

                scope->children.push_back(exWrapper);

                loc->idx++;

                while (1) {

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                    char ch = str[loc->idx];
                    if (ch == STATEMENT_END) {
                        loc->idx++;
                        return Err::OK;
                    }

                    // LOOK AT : separate function?
                    Variable* newOperand = new Variable(scope, DT_UNDEFINED, loc);

                    int err = parseExpression(newOperand, str, loc, CHAR_CAT(',', STATEMENT_END));
                    if (err < 0) return err;

                    fcnCall->inArgs.push_back(newOperand);

                    loc->idx--;
                    ch = str[loc->idx]; // LOOK AT : could be zero?
                    if (ch == ',') {
                        loc->idx++;
                    } else if (ch == STATEMENT_END) {
                        loc->idx++;
                        return Err::OK;
                    } else {
                        Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
                        return Err::UNEXPECTED_SYMBOL;
                    }
                
                }
            
            } else if (ch == '\0') {

                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_END_OF_FILE), loc);
                return Err::UNEXPECTED_END_OF_FILE;
            
            }

            loc->idx++;
        
        }

        return Err::OK;
    
    }

    // already without starting '"'
    int parseStringLiteral(char* const str, Location* const loc, StringInitialization** initOut) {

        const int startIdx = loc->idx;
        while (1) {

            const char ch = str[loc->idx];

            if (ch == STRING_END) {
                break;
            } else if (ch == EOF) {
                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_END_OF_FILE), loc);
                return Err::UNEXPECTED_END_OF_FILE;
            }

            loc->idx++;
        
        }

        const int strLen = loc->idx - startIdx;

        StringInitialization* init = new StringInitialization;
        init->rawStr = std::string(str + startIdx, strLen);
        init->rawPtr = str + startIdx;
        init->rawPtrLen = strLen;

        int utf8Len;
        int utf8BytesPerChar;
        char* utf8Str = Utils::encodeUtf8(init->rawStr.c_str(), strLen, &utf8Len, &utf8BytesPerChar);
        
        if (utf8BytesPerChar != 1) {
            init->wideStr = utf8Str;
            init->wideDtype = (DataTypeEnum) (DT_UINT_8 + utf8BytesPerChar - 1);
            init->wideLen = utf8Len;
        } else {
            init->wideStr = NULL;
            init->wideDtype = DT_UINT_8;
        }

        *initOut = init;
        loc->idx++;

        return Err::OK;

    }

    // already without starting '['
    int parseArrayInitialization(char* const str, Location* loc, ArrayInitialization** initOut) {

        *initOut = new ArrayInitialization();

        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
        const int startIdx = loc->idx;

        while (1) {

            Variable* var = new Variable();

            const int err = parseExpression(var, str, loc, ',]');
            if (err < 0) return err;

            (*initOut)->attributes.push_back(var);

            if (str[loc->idx - 1] == ARRAY_END) break;

            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

        }

        return Err::OK;
    
    }

    // TODO: optimize - use precompile hash or something
    // null-terminated
    int selectKeyWord(const KeyWord keyWords[], const int keyWordsCount, char* const str, int* const idx) {

        const int startStrIdx = *idx;
        for (int i = 0; i < keyWordsCount; i++) {

            int strIdx = startStrIdx;
            const char* keyWord = keyWords[i].str;
            while (1) {

                const char ch = *keyWord;

                if (ch == '\0') {
                    if (!IS_ALLOWED_VARIABLE_CHAR(str[strIdx])) {
                        *idx = strIdx;
                        return i;
                    }
                    break;
                }

                if (ch != str[strIdx]) {
                    // *idx = strIdx;
                    break;
                }

                strIdx++;
                keyWord++;
            }
        
        }

        return -1;

    }

    int selectDataTypeKeyWord(char *const str, int *const idx) {

        const int startStrIdx = *idx;
        for (int i = 0; i < DATA_TYPES_COUNT; i++) {

            int strIdx = startStrIdx;
            const char *keyWord = keyWords[i].str;
            while (1) {

                const char ch = *keyWord;

                if (ch == '\0') {
                    *idx = strIdx;
                    return i;
                }

                if (ch != str[strIdx]) {
                    *idx = strIdx;
                    break;
                }

                strIdx++;
                keyWord++;
            
            }
        
        }

        return -1;
    
    }

    int parseTypeInitialization(Scope *scope, char* const str, Location* loc, TypeInitialization** dtypeInit) {

        *dtypeInit = new TypeInitialization();

        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
        const int startIdx = loc->idx;

        if (IS_ALLOWED_FIRST_VARIABLE_CHAR(str[loc->idx])) {
            loc->idx += Utils::findWordEnd(str + loc->idx);
        }
        
        // TODO : ':' to const ref
        if (startIdx != loc->idx || str[loc->idx] == ':') {

            if (startIdx != loc->idx && str[loc->idx - 1] == ':') {
                loc->idx--;
            } else {
                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
            }

            if (str[loc->idx] != ':') {
                // TODO: ERROR
            } 
            
            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

            Variable* var = new Variable();
            var->name = str + startIdx;
            var->nameLen = loc->idx - startIdx;

            while (1) {
                
                loc->idx++;

                const int err = parseExpression(var, str, loc, ',}');
                if (err < 0) return err;

                (*dtypeInit)->attributes.push_back(var);

                if (str[loc->idx - 1] == SCOPE_END) break;
                
                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                var = new Variable();
                var->name = str + loc->idx;
                var->nameLen = Utils::findVarEnd(var->name);
                
                loc->idx += var->nameLen;
                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                if (str[loc->idx] != ':') {
                    // TODO Error
                }

            }

        } else {

            while (1) {
                
                Variable* var = new Variable();

                const int err = parseExpression(var, str, loc, ',}');
                if (err < 0) return err;

                (*dtypeInit)->attributes.push_back(var);

                if (str[loc->idx - 1] == SCOPE_END) break;
                
                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                
            }

        }

        (*dtypeInit)->idxs = (int*) malloc((*dtypeInit)->attributes.size() * sizeof(int));
        if (!(*dtypeInit)->idxs) {
            Logger::log(Logger::ERROR, Err::str[-Err::MALLOC]);
            return Err::MALLOC;
        }

        return Err::OK;
    
    }

    // either alloc or expression
    // alloc [DtypeName] [DtypeInit]
    int parseRValue(Variable* outVar, char* str, Location* loc, Scope* scope, uint16_t endChar, DataTypeEnum mainDtype) {
        
        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

        // TODO : remove alloc from keyWords to own collection or just harcode it here
        const int keyWordIdx = selectKeyWord(keyWords, KEY_WORDS_COUNT, str, &(loc->idx));
        if (keyWords[keyWordIdx].type == KW_ALLOC) {
        
            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

            DataTypeEnum dtype = mainDtype;
            char* dtypeName = outVar->def->dtypeName;
            int dtypeNameLen = outVar->def->dtypeNameLen;
            if (mainDtype <= 0) {

                dtypeName = str + loc->idx;
                dtypeNameLen = Utils::findVarEnd(dtypeName);
                if (dtypeNameLen <= 0) {
                    Logger::log(Logger::ERROR, "TODO : error parseRValue alloc requaries dtype name!");
                    return Err::INVALID_DATA_TYPE;
                }

                loc->idx += dtypeNameLen;

                dtype = (DataTypeEnum) findDataType(dtypeName, dtypeNameLen);
                if (dtype == DT_UNDEFINED) {
                    dtype = DT_CUSTOM;
                }

            }

            Function* const fcn = internalFunctions + (IF_ALLOC - 1);

            FunctionCall* fcnCall = new FunctionCall();
            fcnCall->fcn = fcn;
            fcnCall->name = fcn->name;
            fcnCall->nameLen = fcn->nameLen;
            fcnCall->outArg = new Variable();
            fcnCall->outArg->cvalue.dtypeEnum = DT_POINTER;

            VariableDefinition* varDef = new VariableDefinition();
            varDef->dtypeName = dtypeName;
            varDef->dtypeNameLen = dtypeNameLen;
            varDef->scope = scope;
            varDef->loc = getLocationStamp(loc);

            Variable* var = new Variable();
            var->def = varDef;
            var->cvalue.dtypeEnum = dtype;

            varDef->var = var;

            parseDataTypeDecorators(dtype, scope, str, loc, var);

            const int err = parseExpression(var, str, loc, endChar);
            if (err < 0) return err;
            
            fcnCall->inArgs.push_back(var);

            outVar->expression = fcnCall;
            SyntaxNode::fcnCalls.push_back(outVar);
            // initializations.push_back(varDef);
            
            if (var->cvalue.dtypeEnum == DT_CUSTOM) SyntaxNode::customDataTypesReferences.push_back(varDef);
            
        } else {
            
            const int err = parseExpression(outVar, str, loc, endChar);
            if (err < 0) return err;

        }

        return Err::OK;

    }
    

    int parseDataTypeDecorators(const DataTypeEnum dtype, Scope* scope, char* const str, Location* const loc, Variable* var) {

        var->dtype = (dtype == DT_CUSTOM) ? NULL : dataTypes + dtype;

        Pointer* mainPtr = NULL;
        while (1) {

            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
            
            const char ch = str[loc->idx];
            if (ch == POINTER_SYMBOL) {
            
                Pointer* ptr = new Pointer();

                if (mainPtr) {

                    ptr->pointsTo = (DataType*) mainPtr; // dataTypes + DT_POINTER;
                    ptr->pointsToEnum = DT_POINTER;

                } else {

                    ptr->pointsTo = (dtype == DT_CUSTOM) ? NULL : dataTypes + dtype;
                    ptr->pointsToEnum = dtype;
                    var->cvalue.dtypeEnum = DT_POINTER;
                    // var->dtype = (DataType*) ptr;

                }

                var->cvalue.ptr = ptr;
                var->dtype = (void*) ptr;

                loc->idx++;

                mainPtr = ptr;

            } else if (ch == ARRAY_BEGIN) {
                // either const / embed or expression
                
                loc->idx++;

                Array* arr = new Array;
                arr->pointsTo = (dtype == DT_CUSTOM) ? NULL : dataTypes + dtype;
                arr->pointsToEnum = dtype;

                Variable* lenVar = new Variable(loc);
                lenVar->scope = scope;
                lenVar->nameLen = 0;
                // lenVar->parentStruct = NULL;
                
                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                const int keyWordIdx = selectKeyWord(keyWords, KEY_WORDS_COUNT, str, &(loc->idx));
                const int kword = keyWords[keyWordIdx].type;
                if (kword == KW_CONST || kword == KW_CMP_TIME) {

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                    if (str[loc->idx] != ARRAY_END) {
                        Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL));
                        return Err::UNEXPECTED_SYMBOL;
                    }

                    loc->idx++;

                    arr->flags = (kword == KW_CONST) ? IS_CONST : IS_CMP_TIME;
                    arr->length = NULL; // var->allocSize = NULL;
                
                } else {

                    const int err = parseExpression(lenVar, str, loc, ARRAY_END);
                    if (err < 0) return err;

                    arr->flags = 0;
                    arr->length = lenVar; // var->allocSize = lenVar;
                
                }
                
                // var->flags = varDef->flags ^ IS_ARRAY;
                var->cvalue.dtypeEnum = DT_ARRAY;
                var->cvalue.arr = arr;
                var->dtype = (void*) arr;
                var->flags = 0;

                SyntaxNode::arrays.push_back(var);

            } else {

                break;
            
            }

        }

    }

    // LOOK AT : dont like it, maybe rework at the later stage when more info will be better defined
    // if outVarDef is NULL, then new VariableDefinition will be added to scope.children, otherwise it will be returned
    // endChar is used as two wchars (first 16 bits and last 16 bits) that can end parsing
    int processDataType(
        const DataTypeEnum dtype, 
        Scope* scope, 
        char* const str, 
        Location* const loc, 
        uint64_t param, 
        uint16_t endChar,
        VariableDefinition** outVarDef,
        const int assignId
    ) {

        const uint8_t fEndCh = endChar;
        const uint8_t sEndCh = endChar >> 8;

        VariableDefinition* varDef = new VariableDefinition(loc);
        Variable* var = new Variable(scope, dtype);

        varDef->scope = scope;
        varDef->var = var;
        varDef->flags = param;

        parseDataTypeDecorators(dtype, scope, str, loc, var);

        var->def = varDef;
        var->unrollExpression = 0;
        var->loc = getLocationStamp(loc);
        var->name = str + loc->idx;
        var->nameLen = Utils::findVarEnd(var->name);
        
        if (assignId) ASSIGN_ID(var);

        loc->idx += var->nameLen;

        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
        
        const char ch = str[loc->idx];
        if (ch == '=') {
            
            loc->idx++;

            const int err = parseRValue(var, str, loc, scope, endChar, dtype);
            if (err < 0) return err;

            if (param & IS_CMP_TIME) SyntaxNode::cmpTimeVars.push_back(var);
            else SyntaxNode::initializations.push_back(varDef);

        } else if (ch == fEndCh || ch == sEndCh) {

            loc->idx++;
        
        } else {
            
            if (ch == '\0') {
                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_END_OF_FILE), loc);
                return Err::UNEXPECTED_END_OF_FILE;
            } else {
                // TODO : fix error
                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 1);
                return Err::UNEXPECTED_SYMBOL;
            }

        }

        if (outVarDef) {
            *outVarDef = varDef;
        } else {
            scope->children.push_back(varDef);
            scope->defs.push_back(varDef);
        }
        scope->vars.push_back(var);

        return Err::OK;

    }

    int parseKeyWord(KeyWordType keyWord, Scope *scope, char *const str, Location *loc, uint64_t param) {

        switch (keyWord) {

            case KW_INT: 
                return processDataType(DT_INT, scope, str, loc, param);

            case KW_INT_8:
                return processDataType(DT_INT_8, scope, str, loc, param);

            case KW_INT_16:
                return processDataType(DT_INT_16, scope, str, loc, param);

            case KW_INT_32:
                return processDataType(DT_INT_32, scope, str, loc, param);

            case KW_INT_64:
                return processDataType(DT_INT_64, scope, str, loc, param);

            case KW_UINT_8:
                return processDataType(DT_UINT_8, scope, str, loc, param);

            case KW_UINT_16:
                return processDataType(DT_UINT_16, scope, str, loc, param);

            case KW_UINT_32:
                return processDataType(DT_UINT_32, scope, str, loc, param);

            case KW_UINT_64:
                return processDataType(DT_UINT_64, scope, str, loc, param);

            case KW_FLOAT_32:
                return processDataType(DT_FLOAT_32, scope, str, loc, param);

            case KW_FLOAT_64:
                return processDataType(DT_FLOAT_64, scope, str, loc, param);

            case KW_CMP_TIME: {

                param = param | IS_CMP_TIME;

            }

            case KW_CONST: {

                // var or function

                param = param | IS_CONST;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                int wordLen = loc->idx;

                int keyWordIdx = selectKeyWord(keyWords, KEY_WORDS_COUNT, str, &(loc->idx));
                wordLen = loc->idx - wordLen + 1;

                if (keyWordIdx < 0) {
                    // maybe custom data type
                    
                    if (str[loc->idx] < 32) {
                        Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, wordLen, "Variable or function definition");
                        return Err::UNEXPECTED_SYMBOL;
                    }

                    Location* defLoc = getLocationStamp(loc);
                    char* const dtypeName = str + loc->idx;
                    const int dtypeNameLen = Utils::findVarEnd(dtypeName);

                    loc->idx += dtypeNameLen;

                    VariableDefinition* varDef;
                    const int err = processDataType(DT_CUSTOM, scope, str, loc, param, STATEMENT_END, &varDef);
                    if (err < 0) return err;

                    varDef->loc = defLoc;

                    varDef->dtypeName = dtypeName;
                    varDef->dtypeNameLen = dtypeNameLen;

                    scope->children.push_back(varDef);
                    SyntaxNode::customDataTypesReferences.push_back(varDef);

                } else {

                    const KeyWordType ktype = (KeyWordType) (keyWords + keyWordIdx)->type;
                    if (ktype > KW_STRING && ktype != KW_FUNCTION) {
                        Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, wordLen, "Variable or function definition");
                        return Err::UNEXPECTED_SYMBOL;
                    }

                    const int err = parseKeyWord(ktype, scope, str, loc, param);
                    if (err < 0) return err;
                
                }
                
                return Err::OK;

            }

            case KW_FUNCTION: {

                Function* fcn;

                Scope* outerScope = new Scope();
                outerScope->scope = scope;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                int foreignLang = 0;
                if (str[loc->idx] == '[') {
                    // 
                    
                    fcn = new ForeignFunction;

                    foreignLang = 1;
                    const int tagLen = parseLanguageTag(str, loc);

                    ((ForeignFunction*) fcn)->tagLen = tagLen;
                    ((ForeignFunction*) fcn)->tagStr = str + loc->idx - tagLen - 1;

                    ASSIGN_ID(fcn);

                } else {
                    fcn = new Function;
                }
                fcn->scope = scope;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                const int wordLen = Utils::findVarEnd(str + loc->idx); // TODO : search till char or space

                fcn->name = str + loc->idx;
                fcn->nameLen = wordLen;

                ASSIGN_ID(fcn);

                // TODO : remove?
                /*
                Function *prevDefFcn = findFunction(scope, fcn->name, fcn->nameLen);
                if (prevDefFcn) {
                    // fcn allready defined
                    // TODO : store in variable last position to report

                    Logger::log(Logger::ERROR, ERR_STR(Err::FUNCTION_ALREADY_DEFINED), loc, fcn->nameLen);
                    return Err::FUNCTION_ALREADY_DEFINED;
                
                }
                */

                loc->idx += wordLen;

                int withoutClosure = 0;
                int linesSkipped = 0;

                if (str[loc->idx] != FUNCTION_START) {

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                    if (str[loc->idx] != FUNCTION_START) {

                        if (str[loc->idx] == SCOPE_BEGIN) {
                            goto fcnParseScope;
                        }

                        Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
                        return Err::UNEXPECTED_SYMBOL;
                    
                    }
                
                }

                loc->idx++;

                // parse input
                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                while (1) {

                    if (str[loc->idx] == FUNCTION_END) {
                        loc->idx++;
                        break;
                    }

                    VariableDefinition* varDef;
                    const int err = parseVariableDefinition(outerScope, str, loc, param, CHAR_CAT(')',','), &varDef);
                    if (err) return err;

                    if (varDef->var->cvalue.dtypeEnum == DT_ARRAY) {

                        VariableDefinition* lenVarDef = new VariableDefinition;
                        lenVarDef->var = new Variable(outerScope, DT_INT);
                        lenVarDef->flags = 0;

                        const int nameLen = varDef->var->nameLen + 3;
                        char* name = (char*) malloc(nameLen * sizeof(char));
                        if (!name) {
                            Logger::log(Logger::ERROR, ERR_STR(Err::MALLOC));
                            return Err::MALLOC;
                        }

                        // WARNING : doing asumption, that name len will never be zero!!!
                        // TODO : get rid of it as we now have id?
                        memcpy(name, varDef->var->name, varDef->var->nameLen);
                        *((uint32_t*) (name + (nameLen - 3))) = 0 + 'L' + (((int) 'e') << 8) + (((int) 'n') << 16);
                       
                        lenVarDef->var->name = name;
                        lenVarDef->var->nameLen = nameLen;
                        lenVarDef->var->def = lenVarDef;

                        ASSIGN_ID(lenVarDef->var);

                        lenVarDef->var->cvalue.dtypeEnum = DT_INT;

                        varDef->var->cvalue.arr->length = lenVarDef->var;// varDef->var->allocSize = lenVarDef->var;

                        fcn->inArgs.push_back(varDef);
                        fcn->inArgs.push_back(lenVarDef);

                    } else {

                        fcn->inArgs.push_back(varDef);
                    
                    }

                    if (str[loc->idx - 1] == ',') {
                        //loc->idx++;
                    } else if (str[loc->idx - 1] == FUNCTION_END) {
                        //loc->idx++;
                        break;
                    }

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                
                }

                // =>
                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                {
                    uint16_t tmp = *((uint16_t *)(str + loc->idx));
                    if (tmp != ('=' | ('>' << 8))) {

                        if ((tmp & 0xFF) == SCOPE_BEGIN) {
                            if (foreignLang) loc->idx++;
                            goto fcnParseScope;
                        }

                        // LOOK AT : maybe better name for this situation, something like "unexpected char sequence"
                        Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
                        return Err::UNEXPECTED_SYMBOL;

                    }
                }

                loc->idx += 2;

                if (str[loc->idx] != FUNCTION_START) {

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                    if (str[loc->idx] != FUNCTION_START) {
                        withoutClosure = 1;
                        // Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL));
                        // return Err::UNEXPECTED_SYMBOL;
                    } else {
                        loc->idx++;
                    }
                
                } else {
                    loc->idx++;
                }

                // parse output

                while (1) {

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                    // LOOK AT : process this out of the loop for the first time
                    if (str[loc->idx] == FUNCTION_END) {

                        if (withoutClosure) {
                            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
                            return Err::UNEXPECTED_SYMBOL;
                        }

                        loc->idx++;
                        break;

                    } else if (withoutClosure && str[loc->idx] == SCOPE_BEGIN) {

                        Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
                        return Err::UNEXPECTED_SYMBOL;
                    
                    }

                    // data type
                    char *const dtypeName = str + loc->idx;
                    const int dtypeLen = Utils::findVarEnd(dtypeName);
                    const int dtype = findDataType(dtypeName, dtypeLen);

                    loc->idx += dtypeLen;

                    fcn->outArgs.push_back((DataTypeEnum) ((dtype == DT_UNDEFINED) ? DT_CUSTOM : dtype));

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                    const char ch = str[loc->idx];
                    if (ch == ',') {
                        // TODO : handle ERROR
                        loc->idx++;
                    } else if (ch == (withoutClosure ? SCOPE_BEGIN : FUNCTION_END)) {
                        loc->idx++;
                        break;
                    } else {
                        Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
                        return Err::UNEXPECTED_SYMBOL;
                    }

                }

            fcnParseScope:
                // scope
                if (foreignLang) {
                    
                    ForeignFunction* ffcn = (ForeignFunction*) fcn;
                    ffcn->codeStr = str + loc->idx;
                    if (findBlockEnd(str, loc, SCOPE_BEGIN, SCOPE_END) < 0) return Err::UNEXPECTED_END_OF_FILE;
                    ffcn->codeLen = str + loc->idx - ffcn->codeStr - 1;
                    

                    ffcn->internalIdx = -3443431;
                    scope->fcns.push_back(ffcn);
                    SyntaxNode::foreignFunctions.push_back(ffcn);

                    return Err::OK;
                
                }

                fcn->internalIdx = 0;

                if (!withoutClosure) {

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                    if (str[loc->idx] != SCOPE_BEGIN) {
                        Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
                        return Err::UNEXPECTED_SYMBOL;
                    }

                    loc->idx++;
                
                }

                Scope* newScope = new Scope;
                newScope->scope = outerScope;

                currentFunction = fcn;
                const int err = parseScope(newScope, str, loc);
                if (err < 0) return err;
                currentFunction = NULL;

                fcn->bodyScope = newScope;

                scope->children.push_back(fcn);
                scope->fcns.push_back(fcn);


                break;
            
            }

            case KW_IF: {

                int err = 0;
                // int linesSkipped = 0;

                Scope* newScope = new Scope();
                newScope->scope = scope;

                Variable* newOperand = new Variable(scope);
                newOperand->loc = getLocationStamp(loc);

                err = parseExpression(newOperand, str, loc, CHAR_CAT(STATEMENT_BEGIN, SCOPE_BEGIN));
                if (err < 0) return err;

                err = parseScope(newScope, str, loc, SC_COMMON, (str[loc->idx - 1] == STATEMENT_BEGIN) ? 1 : 0);
                if (err < 0) return err;

                Branch* branch = new Branch();
                branch->scope = scope;
                branch->scopes.push_back(newScope);
                branch->expressions.push_back(newOperand);

                scope->children.push_back(branch);
                SyntaxNode::branchExpressions.push_back(newOperand);

                const int ifWordLen = strlen(KWS_IF);
                const int elseWordLen = strlen(KWS_ELSE);
                while (1) {

                    err = Utils::skipWhiteSpacesAndComments(str, loc);
                    if (err < 0) return Err::OK;

                    char *const word = str + loc->idx;
                    const int wordLen = Utils::findWordEnd(word);

                    if (wordLen != elseWordLen) return Err::OK;

                    for (int i = 0; i < wordLen; i++) {
                        if (word[i] != KWS_ELSE[i])
                            return Err::OK;
                    }

                    loc->idx += wordLen;

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                    if (str[loc->idx] == SCOPE_BEGIN || str[loc->idx] == ':') {
                        // else case

                        loc->idx++;

                        Scope *newScope = new Scope();
                        newScope->scope = scope;

                        err = parseScope(newScope, str, loc, SC_COMMON, (str[loc->idx - 1] == ':') ? 1 : 0);
                        if (err < 0) return err;

                        branch->scopes.push_back(newScope);

                        return Err::OK;

                    } else {
                        // else if case

                        char *const word = str + loc->idx;
                        const int wordLen = Utils::findWordEnd(word);

                        if (wordLen != ifWordLen) {
                            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, wordLen);
                            return Err::UNEXPECTED_SYMBOL;
                        }

                        for (int i = 0; i < wordLen; i++) {
                            if (word[i] != KWS_IF[i]) {
                                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, wordLen);
                                return Err::UNEXPECTED_SYMBOL;
                            }
                        }

                        loc->idx += wordLen;

                        Scope *newScope = new Scope();
                        newScope->scope = scope;

                        Variable* newOperand = new Variable(scope);
                        newOperand->loc = getLocationStamp(loc);

                        err = parseExpression(newOperand, str, loc, CHAR_CAT(STATEMENT_BEGIN, SCOPE_BEGIN));
                        if (err < 0) return err;

                        err = parseScope(newScope, str, loc, SC_COMMON, (str[loc->idx - 1] == ':') ? 1 : 0);
                        if (err < 0) return err;

                        branch->scopes.push_back(newScope);
                        branch->expressions.push_back(newOperand);
                        SyntaxNode::branchExpressions.push_back(newOperand);
                    
                    }
                
                }

                break;
            
            }

            case KW_SWITCH_CASE: {
                                     
                // case x :
                // is cmp_exp :
                // is cmp_exp_2 { .. }
                // [else :] 


                // when x :
                // case cmp_exp :
                // case cmp_exp_2 : {..}
                // [else :]
                //
                SwitchCase* switchCase = new SwitchCase;
                switchCase->loc = getLocationStamp(loc);
                switchCase->scope = scope;

                Variable* var = new Variable;
                var->loc = getLocationStamp(loc);
                var->scope = scope;

                int err = parseExpression(var, str, loc, CHAR_CAT(STATEMENT_BEGIN, SCOPE_BEGIN));
                if (err < 0) return err;

                int statementBegining = 0;
                int ch = str[loc->idx - 1];
                if (ch == STATEMENT_BEGIN) statementBegining = 1;
                else if (ch == SCOPE_BEGIN) statementBegining = 0;
                else {
                    Logger::log(Logger::ERROR, "TODO");
                    return Err::UNEXPECTED_SYMBOL;
                }

                switchCase->switchExp = var;

                loc->idx++;

                while (1) {

                    if (Utils::skipWhiteSpacesAndComments(str, loc)) return Err::UNEXPECTED_END_OF_FILE;

                    char* const word = str + loc->idx;
                    const int wordLen = Utils::findVarEnd(word);
                    const KeyWord* const keyWord = findKeyWord(keyWords, KEY_WORDS_COUNT, word, wordLen);

                    if (keyWord->type != KW_SWITCH_CASE_CASE && keyWord->type != KW_ELSE) {
                        Logger::log(Logger::ERROR, "TODO");
                        return Err::UNEXPECTED_SYMBOL;
                    }

                    loc->idx += wordLen + 1;

                    Variable* cmpExp = new Variable;
                    cmpExp->scope = scope;
                    err = parseExpression(cmpExp, str, loc, CHAR_CAT(STATEMENT_BEGIN, SCOPE_BEGIN));
                    if (err < 0) return err;

                    ch = str[loc->idx - 1];
                    Scope* sc = new Scope;
                    sc->scope = scope;
                    if (ch == STATEMENT_BEGIN) {
                        parseScope(sc, str, loc, SC_COMMON, 1); 
                    } else if (ch == SCOPE_BEGIN) {
                        parseScope(sc, str, loc);
                    } else {
                        Logger::log(Logger::ERROR, "TODO");
                        return Err::UNEXPECTED_SYMBOL;
                    }

                    if (keyWord->type == KW_ELSE) {
                        switchCase->elseCase = sc;
                        break;
                    }

                    switchCase->casesExp.push_back(cmpExp);
                    switchCase->cases.push_back(sc);

                }

                SyntaxNode::switchCases.push_back(switchCase);
                scope->children.push_back(switchCase);
                
                break;

            }

            case KW_FOR: {

                ForLoop *loop = new ForLoop();

                int err = 0;
                int linesSkipped = 0;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                Scope* outerScope = new Scope();
                outerScope->scope = scope;

                Scope* bodyScope = new Scope();
                bodyScope->scope = outerScope;

                Variable *initEx = new Variable(outerScope);

                // can be either variable initialization or expression
                err = selectDataTypeKeyWord(str, &(loc->idx));
                if (err < 0) {
                    err = parseExpression(initEx, str, loc, STATEMENT_END);
                    if (err < 0) return err;
                } else {
                    err = parseKeyWord((KeyWordType)err, outerScope, str, loc);
                    if (err < 0) return err;
                }

                Variable* conditionEx = new Variable(outerScope);
                conditionEx->loc = getLocationStamp(loc);

                err = parseExpression(conditionEx, str, loc, STATEMENT_END);
                if (err < 0) return err;

                Variable *actionEx = new Variable(outerScope);

                err = parseExpression(actionEx, str, loc, '{');
                if (err < 0) return err;

                currentLoop = loop;
                err = parseScope(bodyScope, str, loc);
                if (err < 0) return err;
                currentLoop = NULL;

                loop->scope = scope;
                loop->bodyScope = bodyScope;
                loop->initEx = initEx;
                loop->conditionEx = conditionEx;
                loop->actionEx = actionEx;

                outerScope->children.push_back(loop);
                scope->children.push_back(outerScope);
                SyntaxNode::branchExpressions.push_back(conditionEx);

                break;
            
            }

            case KW_WHILE: {

                WhileLoop* loop = new WhileLoop();

                int err = 0;
                int linesSkipped = 0;

                Scope* newScope = new Scope();
                newScope->scope = scope;

                Variable* newOperand = new Variable(newScope);

                err = parseExpression(newOperand, str, loc, SCOPE_BEGIN);
                if (err < 0) return err;

                currentLoop = loop;
                err = parseScope(newScope, str, loc);
                if (err < 0) return err;
                currentLoop = NULL;

                loop->scope = scope;
                loop->bodyScope = newScope;
                loop->expression = newOperand;

                scope->children.push_back(loop);

                break;
            
            }

            case KW_GOTO: {

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                GotoStatement* gt = new GotoStatement;
                gt->loc = getLocationStamp(loc);
                gt->scope = scope;
                gt->name = str + loc->idx;
                gt->nameLen = Utils::findVarEnd(str + loc->idx);

                loc->idx += gt->nameLen;

                scope->children.push_back(gt);

                break;
            
            }

            case KW_ENUM: {

                // enum <name> : <type> { .. }

                int err = 0;
                int linesSkipped = 0;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                char* const name = str + loc->idx;
                const int nameLen = Utils::findVarEnd(name);

                loc->idx += nameLen;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                DataTypeEnum dtype;
                const char tmpCh = str[loc->idx];
                if (tmpCh == ':') {

                    loc->idx++;

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                    char* const dtypeName = str + loc->idx;
                    const int dtypeNameLen = Utils::findVarEnd(dtypeName);

                    loc->idx += dtypeNameLen;

                    dtype = (DataTypeEnum)findDataType(dtypeName, dtypeNameLen);
                    if (dtype == DT_UNDEFINED) {
                        Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_DATA_TYPE), loc);
                        return Err::UNKNOWN_DATA_TYPE;
                    } else if (dtype < DT_INT || dtype > DT_INT_64) {
                        Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_DATA_TYPE), loc);
                        return Err::INVALID_DATA_TYPE;
                    }

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                    if (str[loc->idx] != '{') {
                        Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
                        return Err::UNEXPECTED_SYMBOL;
                    }

                    loc->idx++;
                
                } else if (tmpCh == '{') {

                    dtype = DT_INT;
                    loc->idx++;
                
                } else {

                    Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
                    return Err::UNEXPECTED_SYMBOL;
                
                }

                Enumerator* enumerator = new Enumerator();
                enumerator->name = name;
                enumerator->nameLen = nameLen;
                enumerator->dtype = dtype;
                enumerator->loc = getLocationStamp(loc);

                ASSIGN_ID(enumerator);

                uint64_t lastValue = -1; //((uint64_t) 0) - ((uint64_t) 1);
                while (1) {

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                    if (str[loc->idx] == SCOPE_END) {
                        loc->idx++;
                        break;
                    }

                    char* const word = str + loc->idx;
                    const int wordLen = Utils::findVarEnd(word);

                    loc->idx += wordLen;

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                    VariableDefinition* newVarDef = new VariableDefinition();
                    Variable* newVar = new Variable(scope, dtype, loc);

                    newVarDef->var = newVar;
                    newVarDef->var->cvalue.dtypeEnum = dtype;
                    newVarDef->loc = getLocationStamp(loc);
                    newVarDef->flags = IS_CMP_TIME;

                    newVar->def = newVarDef;
                    newVar->name = word;
                    newVar->nameLen = wordLen;
                    newVar->unrollExpression = 1;

                    ASSIGN_ID(newVar);

                    SyntaxNode::cmpTimeVars.push_back(newVar);

                    enumerator->vars.push_back(newVar);

                    const char ch = str[loc->idx];
                    if (ch == '=') {

                        loc->idx++;

                        err = parseExpression(newVar, str, loc, CHAR_CAT(',', SCOPE_END));
                        if (err < 0) return err;

                        if (str[loc->idx - 1] == SCOPE_END) loc->idx--;

                        lastValue = newVar->cvalue.i64;
                    
                    } else if (ch == ',') {

                        lastValue++;
                        newVar->cvalue.i64 = lastValue;

                        loc->idx++;
                    
                    } else if (ch == SCOPE_END) {

                        lastValue++;
                        newVar->cvalue.i64 = lastValue;

                        loc->idx++;
                        break;
                    
                    } else {

                        Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
                        return Err::UNEXPECTED_SYMBOL;
                    
                    }
                
                }

                SyntaxNode::enumerators.push_back(enumerator);
                scope->children.push_back(enumerator);
                scope->enums.push_back(enumerator);

                break;
            
            }

            case KW_TYPE_DEF: {

                int err = 0;
                // int linesSkipped = 0;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                char* const name = str + loc->idx;
                const int nameLen = Utils::findVarEnd(name);

                loc->idx += nameLen;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                if (str[loc->idx] != SCOPE_BEGIN) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
                    return Err::UNEXPECTED_SYMBOL;
                }

                TypeDefinition* newTypeDefinition = new TypeDefinition();
                newTypeDefinition->scope = scope;
                newTypeDefinition->name = name;
                newTypeDefinition->nameLen = nameLen;

                SyntaxNode::customDataTypes.push_back(newTypeDefinition);
                scope->customDataTypes.push_back(newTypeDefinition);
                scope->children.push_back(newTypeDefinition);

                loc->idx++;
                while (1) {

                    int err;
                    char ch;

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                    if (str[loc->idx] == SCOPE_END) {
                        // TODO : out of the loop?
                        // is it even necessary?
                        loc->idx++;
                        break;
                    }

                    VariableDefinition* varDef;
                    err = parseVariableDefinition(scope, str, loc, param, '};', &varDef);
                    if (err) return err;
                    
                    newTypeDefinition->vars.push_back(varDef->var);

                    ch = str[loc->idx - 1];
                    if (ch == STATEMENT_END) {

                        continue;

                    } else if (ch == SCOPE_END) {
                        
                        break;
                    
                    } else { // TODO
                        
                        Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
                        return Err::UNEXPECTED_SYMBOL;

                    }

                }

                break;

            }

            case KW_RETURN : {

                ReturnStatement* ret = new ReturnStatement();
                ret->fcn = currentFunction;
                ret->loc = getLocationStamp(loc);
                ret->scope = scope;

                ret->idx = currentFunction->returns.size();
                currentFunction->returns.push_back(ret);

                while (1) {

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                    Variable *newVar = new Variable(scope, DT_INT_64, loc);

                    const int err = parseExpression(newVar, str, loc, CHAR_CAT(',', STATEMENT_END));
                    if (err < 0) return err;

                    ret->vars.push_back(newVar);

                    if (str[loc->idx - 1] == STATEMENT_END) break;

                }

                scope->children.push_back(ret);
                SyntaxNode::returnStatements.push_back(ret);

                break;
            
            }

            case KW_CONTINUE : {

                if (!currentLoop) {
                    Logger::log(Logger::ERROR, "Continue statement used outside of the loop!");
                    return Err::UNEXPECTED_SYMBOL;
                }

                ContinueStatement* cnt = new ContinueStatement();
                cnt->loc = getLocationStamp(loc);
                cnt->scope = scope;

                scope->children.push_back(cnt);

                break;

            }

            case KW_BREAK : {

                if (!currentLoop) {
                    Logger::log(Logger::ERROR, "Break statement used outside of the loop!");
                    return Err::UNEXPECTED_SYMBOL;
                }

                BreakStatement* cnt = new BreakStatement();
                cnt->loc = getLocationStamp(loc);
                cnt->scope = scope;

                scope->children.push_back(cnt);

                break;

            }

            case KW_LOOP : {
                // loop <<array_name>> using <<int i>> :
                // or
                // int i = 0;
                // loop <<array_name>> using <<i>> : 

                int linesSkipped;

                Loop* loop = new Loop();
                loop->loc = getLocationStamp(loc);

                Scope* outerScope = new Scope();
                outerScope->scope = scope;

                loop->scope = outerScope;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                Variable* newVar = new Variable(loop->scope, DT_UNDEFINED, loc);
                int keyWord = parseExpression(newVar, str, loc, STATEMENT_END, 1);
                if (keyWord < 0) {
                    return keyWord;
                } else if (keyWord != KW_USING) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_VARIABLE_NAME), loc, 1, "Variable name is matching key word name!");
                    return Err::INVALID_VARIABLE_NAME;
                }

                loop->array = newVar;

                loc->idx += strlen(KWS_USING);

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                // var or var def
                int dtypeKeyWord = selectDataTypeKeyWord(str, &(loc->idx));
                if (dtypeKeyWord > 0) {
                    // var def case
                    loop->idx = NULL;
                    
                    const int err = processDataType((DataTypeEnum) dtypeKeyWord, loop->scope, str, loc, param, '{', &(loop->idxDef));
                    if (err < 0) return err;

                    loc->idx++;

                    if (loop->idxDef->var->expression == NULL) {
                        loop->idxDef->var->cvalue.hasValue = 1;
                        loop->idxDef->var->cvalue.i64 = 0;
                        loop->idxDef->var->cvalue.dtypeEnum = DT_INT;
                    }

                } else {
                    // var case
                    loop->idxDef = NULL;

                    const int err = parseExpression(loop->idx, str, loc, SCOPE_BEGIN);
                    if (err < 0) return err;
                
                }

                loop->bodyScope = new Scope();
                loop->bodyScope->scope = loop->scope;
                
                currentLoop = loop;
                const int err = parseScope(loop->bodyScope, str, loc);
                if (err < 0) return err;
                currentLoop = NULL;

                SyntaxNode::loops.push_back(loop);
                scope->children.push_back(loop);

                break;

            }

            case KW_NAMESPACE : {

                if (currentFunction) {
                    Logger::log(Logger::ERROR, "Namespace cannot be declared inside function!");
                    return Err::INVALID_DECLARATION_PLACE;
                }

                Namespace* nsc = new Namespace;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                
                nsc->name = str + loc->idx;
                nsc->nameLen = Utils::findVarEnd(str);

                loc->idx += nsc->nameLen;

                parseScope(nsc, str, loc, SC_COMMON);

                scope->children.push_back(nsc);
                scope->namespaces.push_back(nsc);

                break;

            }

            case KW_ALLOC : {

                                        

                int x = 1;
                x = 2;

            }

        }

        return Err::OK;

    }

    int parseDirectiveKeyWord(KeyWordType keyWord, Scope* scope, char* const str, Location* loc, uint64_t param) { 
        
        switch (keyWord) {

            case DKW_LANG_DEF : {

                LangDef* langDef = new LangDef;
                langDef->dtypeMap = (KeyWord*) malloc(sizeof(langDefKeyWords));
                if (!(langDef->dtypeMap)) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::MALLOC));
                    return Err::MALLOC;
                }
                langDef->dtypeMapLen = LANG_DEF_KEY_WORDS_COUNT;

                const int cmdType               = langDefKeyWords[0].type;
                const int fcnFormatType         = langDefKeyWords[1].type;
                const int fcnFormatInArgsType   = langDefKeyWords[2].type;
                const int fcnFormatOutArgsType  = langDefKeyWords[3].type;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                langDef->tag = {str + loc->idx, Utils::findWordEndOrChar(str + loc->idx, SCOPE_BEGIN)};
                if (langDef->tag.len <= 0) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
                    return Err::UNEXPECTED_SYMBOL;
                }
                loc->idx += langDef->tag.len;
                
                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                if (str[loc->idx] != SCOPE_BEGIN) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
                    return Err::UNEXPECTED_SYMBOL;
                }
                loc->idx++;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                while (1) {

                    const int keyWordIdx = selectKeyWord(langDefKeyWords, LANG_DEF_KEY_WORDS_COUNT, str, &(loc->idx));
                    const int keyWordStartIdx = loc->idx;
                    if (keyWordIdx < 0) {

                        if (keyWordIdx < 0) {
                            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, loc->idx - keyWordStartIdx, "keyword");
                            return Err::UNEXPECTED_SYMBOL;
                        }

                    }

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                    if (str[loc->idx] != STATEMENT_BEGIN) {
                        Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 1, ":");
                        return Err::UNEXPECTED_SYMBOL;
                    }
                    loc->idx++;

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
               
                    /*
                    if (langDefKeyWords[keyWordIdx].type == cmdType) {
                        langDef->cmpCommand = str + loc->idx;
                    } else {
                        langDef->dtypeMap[keyWordIdx].str = str + loc->idx;
                    }
                    */

                    int idx = loc->idx;
                    while (1) {
                        
                        const char ch = str[idx];
                        
                        if (ch == STATEMENT_END) {
                            
                            // TODO : switch case
                            const String finalString = {str + loc->idx, idx - loc->idx};
                            if (langDefKeyWords[keyWordIdx].type == cmdType) {
                                langDef->cmpCommand = finalString;
                            } else if (langDefKeyWords[keyWordIdx].type == fcnFormatType) {
                                langDef->fcnFormat = finalString;
                            } else if (langDefKeyWords[keyWordIdx].type == fcnFormatInArgsType) {
                                langDef->fcnFormatInArgs = finalString;
                            } else if (langDefKeyWords[keyWordIdx].type == fcnFormatOutArgsType) {
                                langDef->fcnFormatOutArgs = finalString;
                            } else {
                                const int tmp = langDefKeyWords[keyWordIdx].type;
                                langDef->dtypeMap[tmp].str = str + loc->idx;
                                langDef->dtypeMap[tmp].type = idx - loc->idx;
                            }

                            loc->idx = idx + 1;

                            break;
                        
                        } else if (ch == EOL) {
                            
                            loc->line++;
                        
                        } else if (ch == EOF) {
                            
                            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_END_OF_FILE), loc);
                            return Err::UNEXPECTED_END_OF_FILE;
                        
                        }

                        idx++;
                    
                    }

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                    if (str[loc->idx] == SCOPE_END) {
                        loc->idx++;
                        break;
                    }

                }

                SyntaxNode::langDefs.push_back(langDef);

                break;

            }

            case DKW_IMPORT : {

                // #import x;
                //      something like include in c, just copy-paste in place

                // #import x as namespace n;
                //      wraps content of file x into namespace n

                // maybe later???
                // #import x as scope;
                // #import x as function foo;

                ImportStatement* import = new ImportStatement;
                import->root = fileRootScope;
                
                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                
                char* const fname = str + loc->idx;
                const int fnameLen = Utils::findWordEnd(fname);

                loc->idx += fnameLen;
                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                
                if (str[loc->idx] == STATEMENT_END) {
                    import->fname = { fname, fnameLen };
                    import->keyWord = (KeyWordType) -1;
                    SyntaxNode::imports.push_back(import);
                    return Err::OK;
                }

                if (str[loc->idx] != 'a' && str[loc->idx + 1] != 's') {
                    Logger::log(Logger::ERROR, "Unexpected word, 'as' expected!", loc, 1);
                    return Err::UNEXPECTED_SYMBOL;
                }

                loc->idx += 2;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                
                const int keyWordIdx = selectKeyWord(keyWords, KEY_WORDS_COUNT, str, &(loc->idx));
                if (keyWords[keyWordIdx].type != KW_NAMESPACE) {
                    Logger::log(Logger::ERROR, "Unsupported value is used! Use one of the following : 'namespace'\n");
                    return Err::UNEXPECTED_SYMBOL;
                }

                import->keyWord = (KeyWordType) keyWords[keyWordIdx].type;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                
                char* const pname = str + loc->idx;;
                const int pnameLen = Utils::findVarEnd(pname);

                loc->idx += pnameLen;
                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                
                if (str[loc->idx] != STATEMENT_END) {
                    Logger::log(Logger::ERROR, "Unexpexted symbol, %c expected", loc, 1, STATEMENT_END);
                    return Err::UNEXPECTED_SYMBOL;
                }

                import->fname = { fname, fnameLen };
                import->keyWord = (KeyWordType) keyWordIdx;
                import->param = { pname, pnameLen };
                SyntaxNode::imports.push_back(import);
                
                break;

            }

        }

        return Err::OK;

    }

    // G as gneral
    enum {
        G_NONE,
        G_VARIABLE,
        G_STRING_LITERAL,
        G_NUMBER_LITERAL,
        G_OPERATOR,
        G_SUB_EXPRESSION
    };

    // expression is something like this
    // x = 10 * 1 + y + z * sin(u) + tmp[0];
    // states
    // 0 -> unary operator or value/var is expected
    // 1 -> binary operator or value/var is expected
    // expresion has to have at least one operator, otherwise it has to be NULL and value is represented directly in variable
    // if second half (endChar >> 8) of endChar is 1 'exclusive end' is enabled -> in this case only first half of endChar is used as end char
    // in case that 1 is requaried as end check, can be used in first part
    // if useKeyWordAsEnd is true, then possitive return value is KeyWordType of the found key word
    int parseExpression(Variable* operand, char* const str, Location* const loc, const uint16_t endChar, const int useKeyWordAsEnd) {

        int lastOperatorRank = 0;
        UnaryExpression* lastUnaryExpression = NULL;
        BinaryExpression* lastBinaryExpression = NULL;
        int lastType = G_NONE;
        int lastOperator = -1;
        int lastOperandType = DT_UNDEFINED;
        Variable* lastVariable = NULL;

        const int exclusiveEnd = (endChar >> 8) == 1;

        const char endCharA = (char) endChar;
        const char endCharB = (endChar > 255) ? endChar >> 8 : endCharA;

        while (1) {

            // TODO : do better
            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

            const char ch = str[loc->idx];

            if ( ( ch == endCharA && ((!exclusiveEnd || (exclusiveEnd && str[loc->idx + 1] != endCharA))) ) || ch == endCharB) {
                // end of expression

                if (lastType == G_OPERATOR) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_END_OF_EXPRESSION), loc);
                    return Err::UNEXPECTED_END_OF_EXPRESSION;
                }

                loc->idx++;

                return Err::OK;
            
            }
            else if (ch == '(') {
                // subexpression start

                Variable* newOperand = new Variable();
                newOperand->cvalue.dtypeEnum = DT_UNDEFINED; // TODO : exchange for UNDEFINED or whatever!!!!!
                newOperand->scope = operand->scope;
                newOperand->unrollExpression = 1;
                newOperand->cvalue.ptr = NULL;

                loc->idx++;
                parseExpression(newOperand, str, loc, ')');

                if (lastType == G_NONE) {

                    // TODO: typecheck
                    WrapperExpression* ex = new WrapperExpression();
                    ex->operand = newOperand;
                    operand->expression = ex;

                }
                else {

                    if (lastUnaryExpression) lastUnaryExpression->operand = newOperand;
                    else ((BinaryExpression*)operand->expression)->operandB = newOperand;

                }

                loc->idx--;
                lastType = G_VARIABLE;

            } else if (ch == STRING_BEGIN) {

                Variable* newOperand = new Variable();
                newOperand->cvalue.dtypeEnum = DT_STRING;
                newOperand->unrollExpression = 0;
                
                newOperand->id = arrId;
                arrId++;

                loc->idx++;

                const int err = parseStringLiteral(str, loc, (StringInitialization**) &(newOperand->expression));
                if (err < 0) return err;

                if (lastType == G_NONE) {
                    WrapperExpression* ex = new WrapperExpression();
                    ex->operand = newOperand;
                    operand->expression = ex;
                } else {
                    if (lastUnaryExpression) lastUnaryExpression->operand = newOperand;
                    else ((BinaryExpression*)operand->expression)->operandB = newOperand;
                }

                loc->idx--;
                lastType = G_VARIABLE;

            } else if (ch == CHAR_BEGIN) {

                loc->idx++;

                uint64_t charInt = 0;
                const int size = parseCharLiteral(str, loc, &charInt);
                if (size < 0) return size;

                DataTypeEnum dtype = DT_UINT_64;
                if (size <= 1) {
                    charInt &= 0x00000000000000FF;
                    dtype = DT_UINT_8;
                } else if (size <= 2) {
                    charInt &= 0x000000000000FFFF;
                    dtype = DT_UINT_16;
                } else if (size <= 4) {
                    charInt &= 0x00000000FFFFFFFF;
                    dtype = DT_UINT_32;
                }

                Variable* var = new Variable();
                var->unrollExpression = 0;
                var->cvalue.dtypeEnum = dtype;
                var->cvalue.i64 = charInt;
                var->cvalue.hasValue = 1;
                var->expression = NULL;

                // TODO : refactor so can be used generaly
                if (lastType == G_NONE) {
                    WrapperExpression* ex = new WrapperExpression();
                    ex->operand = var;
                    operand->expression = ex;
                } else {
                    if (lastUnaryExpression) lastUnaryExpression->operand = var;
                    else ((BinaryExpression*)operand->expression)->operandB = var;
                }

                loc->idx--;
                lastType = G_VARIABLE;

            } else if (ch == ARRAY_BEGIN && lastType != G_VARIABLE) {

                Variable* newOperand = new Variable();
                newOperand->cvalue.dtypeEnum = DT_ARRAY;
                newOperand->unrollExpression = 0;

                newOperand->id = arrId;
                arrId++;
                
                loc->idx++;

                const int err = parseArrayInitialization(str, loc, (ArrayInitialization**) &(newOperand->expression));
                if (err < 0) return err;

                if (lastType == G_NONE) {
                    // TODO: typecheck

                    WrapperExpression* ex = new WrapperExpression();
                    ex->operand = newOperand;
                    operand->expression = ex;

                }
                else {

                    if (lastUnaryExpression) lastUnaryExpression->operand = newOperand;
                    else ((BinaryExpression*)operand->expression)->operandB = newOperand;

                }

                loc->idx--;

                lastType = G_VARIABLE;
            
            } else if (ch == SCOPE_BEGIN) {
                // custom type

                // TypeInitialization* typeInit;

                Variable* newOperand = new Variable();
                // newOperand->expression = typeInit;
                newOperand->cvalue.dtypeEnum = DT_CUSTOM;
                newOperand->unrollExpression = 0;

                loc->idx++;

                const int err = parseTypeInitialization(operand->scope, str, loc, (TypeInitialization **)&(newOperand->expression));
                if (err < 0) return err;

                if (lastType == G_NONE) {
                    // TODO: typecheck
                    
                    WrapperExpression* ex = new WrapperExpression();
                    ex->operand = newOperand;
                    operand->expression = ex;

                } else {

                    if (lastUnaryExpression) lastUnaryExpression->operand = newOperand;
                    else ((BinaryExpression*) operand->expression)->operandB = newOperand;
                
                }

                loc->idx--;

                lastType = G_VARIABLE;
            
            } else {
                // word found

                // LOOK AT : check also for '_'?
                if (ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z') {
                    // variable or function call (or alloc expression)

                    Location* startLocation = getLocationStamp(loc);

                    char* word = str + loc->idx;
                    int wordLen = Utils::findVarEnd(word + 1) + 1;

                    const KeyWord* keyWord = findKeyWord(keyWords, KEY_WORDS_COUNT, word, wordLen);
                    if (keyWord) {
                        if (useKeyWordAsEnd) {
                            if (lastType == G_OPERATOR) {
                                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_END_OF_EXPRESSION), loc);
                                return Err::UNEXPECTED_END_OF_EXPRESSION;
                            }

                            // loc->idx++;

                            return keyWord->type;
                        }

                        Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_VARIABLE_NAME), loc, wordLen, "Variable name is matching key word name!");
                        return Err::INVALID_VARIABLE_NAME;
                    }

                    // decide var or fcn

                    loc->idx += wordLen;
                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                    std::vector<INamed*> scopeNames;
                    while (CMP_TWO_CHARS(str + loc->idx, SCOPE_RESOLUTION_SYMBOL)) {

                        INamed* scName = new INamed(word, wordLen);
                        scopeNames.push_back(scName);

                        loc->idx += 2;
                        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                        word = str + loc->idx;
                        wordLen = Utils::findVarEnd(word);

                        loc->idx += wordLen;
                        
                    }

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                    const int isVar = str[loc->idx] != FUNCTION_START;

                    if (isVar) {

                        Variable* var = new Variable();
                        var->scope = operand->scope;
                        var->loc = startLocation;
                        var->flags = 0;
                        var->unrollExpression = 0;
                        var->scopeNames = scopeNames;

                        var->name = word;
                        var->nameLen = wordLen;

                        if (lastOperator != OP_MEMBER_SELECTION) { 
                            lastVariable = var;
                            SyntaxNode::variables.push_back(var);
                        } else {
                            var->cvalue.dtypeEnum = DT_MEMBER;
                            var->id = 0;
                        }

                        if (lastType == G_NONE) {

                            WrapperExpression* ex = new WrapperExpression();
                            ex->operand = var;
                            operand->expression = ex;
                        
                        } else {

                            if (lastUnaryExpression) {
                                lastUnaryExpression->operand = var;
                            } else {
                                ((BinaryExpression *)operand->expression)->operandB = var;
                            }
                        
                        }

                        loc->idx += - 1;
                    
                    } else {

                        FunctionCall* fcnCall = new FunctionCall;
                        fcnCall->fcn = NULL;
                        fcnCall->name = word;
                        fcnCall->nameLen = wordLen;
                        fcnCall->scopeNames = scopeNames;

                        // parse input
                        loc->idx += 1;
                        while (1) {

                            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                            // LOOK AT : process this out of the loop for the first time
                            if (str[loc->idx] == FUNCTION_END) break;

                            // LOOK AT : is Variable needed? maybe use just Variable
                            Variable* newOperand = new Variable(operand->scope, DT_UNDEFINED, loc);
                            newOperand->flags = 0;

                            int err = parseExpression(newOperand, str, loc, CHAR_CAT(',', FUNCTION_END));
                            if (err < 0) return err;

                            fcnCall->inArgs.push_back(newOperand);

                            loc->idx--;
                            const char ch = str[loc->idx]; // LOOK AT : could be zero?
                            if (ch == ',') {
                                loc->idx++;
                            } else if (ch == FUNCTION_END) {
                                break;
                            } else {
                                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
                                return Err::UNEXPECTED_SYMBOL;
                            }

                        }

                        Variable* newOperand = new Variable;
                        newOperand->cvalue.dtypeEnum = DT_UNDEFINED;
                        newOperand->expression = fcnCall;
                        newOperand->scope = operand->scope;
                        newOperand->unrollExpression = 1;
                        newOperand->cvalue.ptr = NULL;
                        newOperand->loc = startLocation;

                        if (lastType == G_NONE) {

                            WrapperExpression* ex = new WrapperExpression();
                            ex->operand = newOperand;
                            operand->expression = ex;
                        
                        } else {

                            if (lastUnaryExpression) lastUnaryExpression->operand = newOperand;
                            else ((BinaryExpression*) operand->expression)->operandB = newOperand;
                        
                        }

                        SyntaxNode::fcnCalls.push_back(newOperand);
                    
                    }

                    lastType = G_VARIABLE;
                
                } else if (ch >= '0' && ch <= '9' || (ch == '.' && lastType != G_VARIABLE)) {
                    // number literal

                    if (lastType != G_OPERATOR && lastType != G_NONE) {
                        Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_NUMBER_LITERAL), loc);
                        return Err::INVALID_NUMBER_LITERAL;
                    }

                    uint64_t value;
                    DataTypeEnum dataType = parseNumberLiteral(str + loc->idx, &(loc->idx), &value);
                    if (dataType == DT_VOID) {
                        Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_NUMBER_LITERAL), loc);
                        return Err::INVALID_NUMBER_LITERAL;
                    }

                    Variable* newVar = new Variable(operand->scope, dataType, loc);
                    newVar->cvalue.i64 = value;
                    newVar->cvalue.hasValue = 1;

                    if (lastType == G_NONE) {

                        newVar->unrollExpression = 1;

                        WrapperExpression* ex = new WrapperExpression();
                        ex->operand = newVar;
                        operand->expression = ex;
                    
                    } else {

                        newVar->unrollExpression = 0; // LOOK AT : do we have to set it?

                        if (lastUnaryExpression) lastUnaryExpression->operand = newVar;
                        else ((BinaryExpression*) operand->expression)->operandB = newVar;
                    
                    }

                    lastType = G_NUMBER_LITERAL;

                    loc->idx -= 1;

                } else if (ch == '"') {
                    // string

                    lastType = G_STRING_LITERAL;

                } else if (ch == ARRAY_BEGIN) {
                    // lets keep it hardcoded for now
                    
                    // either slice or single access

                    if (lastType == G_NONE || lastType == G_OPERATOR) {
                        Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_USAGE_OF_OPERATOR), loc, 1, 5, "[...]");
                        return Err::INVALID_USAGE_OF_OPERATOR;
                    }

                    Variable* idxOperand = new Variable();
                    idxOperand->cvalue.dtypeEnum = DT_UNDEFINED;
                    idxOperand->scope = operand->scope;
                    idxOperand->unrollExpression = 1;
                    idxOperand->cvalue.ptr = NULL;
                    
                    loc->idx++;

                    const int err = parseExpression(idxOperand, str, loc, CHAR_CAT(ARRAY_END, ':'));
                    if (err < 0) return err;

                    Variable* operandB = idxOperand;

                    if (str[loc->idx - 1] == ':') {
                        // slice

                        Variable* idxOperand2 = new Variable();
                        idxOperand2->cvalue.dtypeEnum = DT_UNDEFINED;
                        idxOperand2->scope = operand->scope;
                        idxOperand2->unrollExpression = 1;
                        idxOperand2->cvalue.ptr = NULL;

                        const int err = parseExpression(idxOperand2, str, loc, ARRAY_END);
                        if (err < 0) return err;

                        Slice* slice = new Slice;
                        slice->bidx = idxOperand;
                        slice->eidx = idxOperand2;

                        Variable* newOperand = new Variable();
                        newOperand->cvalue.dtypeEnum = DT_UNDEFINED;
                        newOperand->scope = operand->scope;
                        newOperand->unrollExpression = 1;
                        newOperand->expression = slice;

                        Variable* arr;
                        if (lastUnaryExpression) {
                            arr = ((UnaryExpression*) (operand->expression))->operand;
                            ((UnaryExpression*) (operand->expression))->operand = newOperand;
                        } else if (lastBinaryExpression) {
                            arr = ((BinaryExpression*) (operand->expression))->operandB;
                            ((BinaryExpression*) (operand->expression))->operandB = newOperand;
                        } else {
                            arr = ((WrapperExpression*) (operand->expression))->operand;
                            ((WrapperExpression*) (operand->expression))->operand = newOperand;
                        }

                        slice->arr = arr;

                        // SyntaxNode::slices.push_back(slice);

                        lastVariable = newOperand;
                        lastType = G_VARIABLE;

                        continue;

                    }

                    BinaryExpression* bEx = new BinaryExpression;
                    bEx->operandB = idxOperand;
                    bEx->operType = OP_SUBSCRIPT;
                    bEx->oper = operators + OP_SUBSCRIPT;

                    Variable* newOperand = new Variable();
                    newOperand->cvalue.dtypeEnum = DT_UNDEFINED;
                    newOperand->scope = operand->scope;
                    newOperand->unrollExpression = 1;
                    newOperand->cvalue.ptr = NULL;
                    newOperand->expression = bEx;

                    if (lastUnaryExpression) {
                        bEx->operandA = ((UnaryExpression*) (operand->expression))->operand;
                        ((UnaryExpression*) (operand->expression))->operand = newOperand;
                    } else if (lastBinaryExpression) {
                        if (lastOperatorRank >= (operators + OP_SUBSCRIPT)->rank) {
                            bEx->operandA = ((BinaryExpression*) (operand->expression))->operandB;
                            ((BinaryExpression*) (operand->expression))->operandB = newOperand;
                        } else {
                            newOperand->expression = operand->expression;
                            bEx->operandA = newOperand;
                            operand->expression = bEx;
                        }
                    } else {
                        bEx->operandA = ((WrapperExpression*) (operand->expression))->operand;
                        ((WrapperExpression*) (operand->expression))->operand = newOperand;
                    }

                    // operand->expression = bEx;

                    lastVariable = newOperand;

                    loc->idx--;
                    lastType = G_VARIABLE;

                } else {
                    // operator

                    // operator can be merged with other word...
                    uint32_t word = ch;
                    int wordLen = 1;
                    for (int i = loc->idx + 1; i < loc->idx + 4; i++) {
                        const char ch = str[i];
                        // TODO : has to be overall better way
                        if (ch <= ' ' || ch == ';' || ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z' || ch >= '0' && ch <= '9' || ch == '(' || ch == ')') {
                            break;
                        }
                        wordLen++;
                        word = (word << 8) + ch;
                    }

                    OperatorEnum opType;

                    if (lastType == G_NONE) {

                        opType = findUnaryOperator(word);
                        if (opType <= OP_NONE) {
                            Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_OPERATOR), loc, wordLen, wordLen, &word);
                            return Err::INVALID_OPERATOR;
                        }

                        UnaryExpression* uEx = new UnaryExpression;
                        uEx->operand = NULL;
                        uEx->operType = opType;
                        uEx->oper = operators + opType;

                        operand->expression = uEx;
                        lastUnaryExpression = uEx;
                        lastType = G_OPERATOR;

                    } else if (lastType == G_OPERATOR) {

                        opType = findUnaryOperator(word);
                        if (!opType <= OP_NONE) {
                            Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_OPERATOR), loc, wordLen, wordLen, &word);
                            return Err::INVALID_OPERATOR;
                        }

                        UnaryExpression* uEx = new UnaryExpression;
                        uEx->operand = NULL;
                        uEx->operType = opType;
                        uEx->oper = operators + opType;

                        Variable* newVar = new Variable(loc);
                        newVar->scope = operand->scope;
                        newVar->expression = uEx;

                        if (lastUnaryExpression == NULL) {
                            // LOOK_AT: is it allways binary at this point?
                            ((BinaryExpression*) operand->expression)->operandB = newVar;
                        } else {
                            lastUnaryExpression->operand = newVar;
                        }

                        lastUnaryExpression = uEx;
                    
                    } else if (opType = findPostfixUnaryOperator(word), opType > OP_NONE) {

                        Variable* newVar = new Variable(loc);
                        newVar->scope = operand->scope;
                        newVar->expression = operand->expression;

                        UnaryExpression* uEx = new UnaryExpression;
                        uEx->operand = newVar;
                        uEx->operType = opType;
                        uEx->oper = operators + opType;

                        operand->expression = uEx;

                        lastUnaryExpression = uEx;
                        lastType = G_VARIABLE;
                    
                    } else {

                        opType = findBinaryOperator(word);
                        if (opType <= OP_NONE) {
                            Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_OPERATOR), loc, wordLen, wordLen, &word);
                            return Err::INVALID_OPERATOR;
                        }

                        Operator* op = operators + opType;

                        lastUnaryExpression = NULL;

                        BinaryExpression* bEx = new BinaryExpression;

                        Variable* newVar = new Variable(loc);
                        newVar->scope = operand->scope;

                        // LOOK_AT: maybe change design to allways somehow have root as expression, so we dont have to check or something
                        const int operatorRank = op->rank;
                        if (lastOperatorRank > operatorRank) {

                            newVar->expression = bEx;

                            Variable* tmpVar = (Variable*) (((BinaryExpression *)operand->expression)->operandB);
                            ((BinaryExpression*) operand->expression)->operandB = newVar;
                            bEx->operandA = tmpVar;

                            operand = newVar;

                        } else if (operand->expression) {

                            newVar->expression = operand->expression;
                            bEx->operandA = newVar;
                        
                        } else {

                            newVar->cvalue.dtypeEnum = operand->cvalue.dtypeEnum;
                            // newVar->dataType = operand->dataType;
                            newVar->cvalue.i64 = operand->cvalue.i64;
                            newVar->expression = NULL;
                            bEx->operandA = newVar;
                        
                        }

                        operand->expression = bEx;
                        bEx->operandB = NULL;
                        bEx->operType = opType;
                        bEx->oper = op;

                        lastOperatorRank = operatorRank;
                        lastBinaryExpression = bEx;
                        lastType = G_OPERATOR;

                    }

                    lastOperator = opType; // LOOK AT : is it any usefull
                    // lastType = G_OPERATOR;

                    loc->idx += wordLen - 1;

                }
            
            }

            loc->idx++; // LOOK AT : get rid of it???
        
        }
    
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

    int parseEscapeChar(char* str, int* idx) {

        const char ch = str[*idx];
        switch(ch) {

            case 'a':
                return 0x07;
            case 'b':
                return 0x08;
            case 'f':
                return 0x0C;
            case 'n':
                return 0x0A;
            case 'r':
                return 0x0D;
            case 't':
                return 0x09;
            case 'v':
                return 0x0B;
            case '\\':
                return 0x5C;
            case '\'':
                return 0x27;
            case '\"':
                return 0x22;
            case '\?':
                return 0x3F;
            case '\0':
                return 0;
            default:
                return -1;

        }

    };

    //
    int parseCharLiteral(char* const str, Location* loc, uint64_t* out) {
        
        const int startIdx = loc->idx;

        uint64_t tmpOut = 0;

        int size = 0;
        while (1) {

            char ch = str[loc->idx];
            if (ch == CHAR_END) {
                loc->idx++;
                break;
            } else if (ch == ESCAPE_CHAR) {
                loc->idx++;
                ch = parseEscapeChar(str, &(loc->idx));
                if (ch == -1) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNSUPPORTED_ESCAPE_SEQUENCE), loc, 1);
                    return Err::UNSUPPORTED_ESCAPE_SEQUENCE;
                }
            }
            
            if (ch == EOS) {
                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_END_OF_FILE));
                return Err::UNEXPECTED_END_OF_FILE;
            }

            uint64_t tmp = ch;
            tmp <<= 56;

            tmpOut >>= 8;
            tmpOut |= tmp;

            size++;

            loc->idx++;

        }

        if (size > 8) {
            const int len = loc->idx - startIdx;
            loc->idx -= startIdx - 1;
            Logger::log(Logger::ERROR, ERR_STR(Err::DATA_TYPE_SIZE_EXCEEDED), loc, len);
            return Err::DATA_TYPE_SIZE_EXCEEDED;
        }

        tmpOut >>= (8 - size) * 8;

        *out = tmpOut;
        return size;

    }

    // returns data type (DT_VOID on error)
    DataTypeEnum parseNumberLiteral(char *const str, int *idx, uint64_t *out) {

        const int16_t tmp = ((int16_t *) str)[0];
        if (tmp == (('x' << 8) + '0')) {
            // int-hex

            uint64_t num = 0;
            for (int i = 2;; i++) {

                const char ch = str[i];

                if (ch >= '0' && ch <= '9') num = num * 16 + (ch - '0');
                else if (ch >= 'A' && ch <= 'F') num = num * 16 + (10 + ch - 'A');
                else if (ch >= 'a' && ch <= 'f') num = num * 16 + (10 + ch - 'a');
                else {
                    *idx += i;
                    *out = num;
                    return DT_INT_64;
                }
            
            }
        
        } else if (tmp == (('b' << 8) + '0')) {
            // int-bin

            uint64_t num = 0;
            for (int i = 2;; i++) {

                const char ch = str[i];
                if (ch == '0' || ch == '1') num = num * 2 + (ch - '0');
                else {
                    *idx += i;
                    *out = num;
                    return DT_INT_64;
                }
            
            }
        
        } else {

            uint64_t integer = 0;
            for (int i = 0;; i++) {

                const char ch = str[i];

                if (ch >= '0' && ch <= '9') integer = integer * 10 + (ch - '0');
                else if (ch == '.') {
                    // float

                    uint64_t decimal = 0;
                    uint64_t base = 1;
                    for (int j = i + 1;; j++) {

                        const char ch = str[j];

                        if (ch >= '0' && ch <= '9') decimal = decimal * 10 + (ch - '0');
                        else if (ch == 'f') {

                            *idx += j + 1;
                            *((float_t *)out) = (float_t) (integer + (decimal / (double) base));
                            return DT_FLOAT_32;
                            
                        } else {

                            *idx += j;
                            *((double_t *)out) = (double_t) (integer + (decimal / (double) base));
                            return DT_FLOAT_64;                                
                        
                        }

                        base *= 10;
                    
                    }
                    
                } else if (ch == 'f') {

                    *idx += i + 1;
                    *((float_t *) out) = (float_t) integer;
                    return DT_FLOAT_32;
                    
                } else {

                    *idx += i;
                    *out = integer;
                    return DT_INT_64;
                
                }
            
            }
        
        }

        return DT_VOID;
    
    }

    const KeyWord* findKeyWord(const KeyWord* keyWords, const int keyWordsCount, char* const name, const int nameLen) {

        for (int i = 0; i < keyWordsCount; i++) {

            const char* kstr = keyWords[i].str;
            const int klen = strlen(kstr);

            if (nameLen != klen) continue;

            int j = 0;
            for (; j < klen; j++) {
                if (kstr[j] != name[j]) break;
            }

            if (j != klen) continue;
            else return keyWords + i;
        
        }

        return NULL;
    
    }

    uint32_t findDataType(char* const name, const int nameLen) {

        for (int i = 0; i < DATA_TYPES_COUNT; i++) {

            const DataType *dtype = dataTypes + i;
            if (dtype->nameLen != nameLen) continue;

            const char* word = dtype->name;

            int j = 0;
            for (; j < nameLen; j++) {
                if (word[j] != name[j]) break;
            }

            if (j != nameLen) continue;
            else return i;
        
        }

        return DT_UNDEFINED;
    }

    Location* getLocationStamp(Location* loc) {

        Location* stamp = (Location*) malloc(sizeof(Location));
        if (!stamp)
            return NULL;

        stamp->file = loc->file;
        stamp->idx = loc->idx;
        stamp->line = loc->line;

        return stamp;
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

    /*
    void copy(Variable* dest, Variable* src) {

        dest->cvalue.dtypeEnum = src->cvalue.dtypeEnum;
        dest->expression = src->expression;
        dest->scope = src->scope;
        dest->unrollExpression = src->unrollExpression;
        dest->cvalue.i64 = src->cvalue.i64;
    
    }
    */

    // appends scB to scA
    void appendScope(Scope* scA, Scope* scB) {
        #define appendVectors(a, b) (a).insert(std::end(a), std::begin(b), std::end(b))
        appendVectors(scA->children, scB->children);
        appendVectors(scA->codeBlocks, scB->codeBlocks);
        appendVectors(scA->customDataTypes, scB->customDataTypes);
        appendVectors(scA->enums, scB->enums);
        appendVectors(scA->fcns, scB->fcns);
        appendVectors(scA->foreignFunctions, scB->foreignFunctions);
        appendVectors(scA->langDefs, scB->langDefs);
        appendVectors(scA->namespaces, scB->namespaces);
    }

    // appends scB  infront of scA
    void appendPrefixScope(Scope* scA, Scope* scB) {
        #define appendVectors(a, b) (a).insert(std::begin(a), std::begin(b), std::end(b))
        appendVectors(scA->children, scB->children);
        appendVectors(scA->codeBlocks, scB->codeBlocks);
        appendVectors(scA->customDataTypes, scB->customDataTypes);
        appendVectors(scA->enums, scB->enums);
        appendVectors(scA->vars, scB->vars);
        appendVectors(scA->fcns, scB->fcns);
        appendVectors(scA->foreignFunctions, scB->foreignFunctions);
        appendVectors(scA->langDefs, scB->langDefs);
        appendVectors(scA->namespaces, scB->namespaces);
    }

}
