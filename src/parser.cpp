#include <ctype.h>
#include <vector>
#include <thread>

#include <stdlib.h>
#include <locale.h>
#include <stdio.h>
#include <string.h>

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
#include <map>
#include <filesystem>

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
    int parseVariableDefinition(Scope* scope, char* const str, Location* const loc, uint64_t param, uint16_t endChar, VariableDefinition** outVarDef);
    int parseExpression(Variable* var, char* const str, Location* const loc, const uint16_t endChar = STATEMENT_END, const int useKeyWordAsEnd = 0, const int emptyExpressionAllowed = 0, const int defIdx = -1);
    int parseStringLiteral(Scope* scope, char* const str, Location* const loc);
    int parseStringLiteral(char* const str, Location* const loc, StringInitialization** initOut);
    int parseTypeInitialization(Scope* scope, char* const str, Location* loc, TypeInitialization** dtypeInit);
    uint64_t parseHexInt(char* const str, int* idx);
    DataTypeEnum parseNumberLiteral(char* const str, int* idx, uint64_t* out);
    int parseCharLiteral(char* const str, Location* idx, uint64_t* out);
    int parseLanguageTag(char* const str, Location* loc);
    int parseDataTypeDecorators(Scope* scope, char* const str, Location* const loc, Variable* var, Pointer** lastPointer, int include);
    int parseVariableDefinitionLValue(Scope* scope, char* const str, Location* const loc, uint64_t param = 0);
    int parseRValue(Variable* outVar, char* str, Location* loc, Scope* scope, uint16_t endChar);
    int parseFunctionPointer(Scope* scope, char* const str, Location* loc, FunctionPrototype** fcnOut);
    int parseDefinitionAssignment(Scope* scope, char* const str, Location* const loc, VariableDefinition* const def, uint16_t endChar, int includeToScope);
    int parseDataType(Scope* scope, char* str, Location* loc, const int expectQualifier, VariableDefinition* def);
    int parseScopeNames(INamedVar* var, char* const str, Location* const loc);
    int parseImport(Scope* sc, char* const str, Location* const loc);

    int processDataType(const DataTypeEnum dtype, Scope* scope, char* const str, Location* const loc, uint64_t param = 0, uint16_t endChar = STATEMENT_END, VariableDefinition** outVarDef = NULL, const int include = 1, const int alloc = 1);
    int skipScopeNames(char* const str, Location* const loc);
    
    const KeyWord* findKeyWord(const KeyWord* keyWords, const int keyWordsCount, char* const name, const int nameLen);
    uint32_t findDataType(char* const name, const int nameLen);

    int findBlockEnd(char* const str, Location* loc, const char beginCh, const char endCh);

    Location* getLocationStamp(Location* loc);
    void freeLocationStamp(Location* loc);

    void copy(Variable* dest, Variable* src);
    void copy(Variable* dest, Variable* src);
    void copy(Scope* scA, Scope* scB);

    Namespace* getCopy(Namespace* nspace);
    Function* getCopy(Function* nspace);

    void appendScope(Scope* scA, Scope* scB);
    void appendPrefixScope(Scope* scA, Scope* scB);

    void stripWrapperExpressions(Variable** op);

    inline void offsetParentIdx(std::vector<SyntaxNode*> vec, const int offset);

    VariableDefinition* createEmptyVariableDefinition();



    // TODO : change when multi thread support will be added!!!
    //      used in ASSIGN_ID macro
    uint32_t varId = 0;
    // to assign each array/string initialization an id, so
    // render can easily create separate variable for them 
    uint32_t arrId = 0;

    uint64_t errId = 1;

    uint64_t defId = 0;
    



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
        {32, KWS_FREE},
        {33, KWS_ERROR},
        {34, KWS_UNION},
        {35, KWS_CATCH},
        {36, KWS_IMPORT},
        {37, KWS_SCOPE},
    };

    const int KEY_WORDS_COUNT = sizeof(keyWords) / sizeof(KeyWord);



    const KeyWord typedefKeyWords[] = {
        {0, KWS_STRUCT},
        {1, KWS_UNION},
    };

    const int TYPEDEF_KEY_WORDS_COUNT = sizeof(typedefKeyWords) / sizeof(KeyWord);



    const KeyWord directiveKeyWords[] = {
        {0, DKWS_LANG_DEF}
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

            case operators[OP_UNARY_PLUS].word  : return OP_UNARY_PLUS;
            case operators[OP_UNARY_MINUS].word : return OP_UNARY_MINUS;
            case POINTER_SYMBOL                 : return OP_GET_VALUE;
            case ADDRESS_SYMBOL                 : return OP_GET_ADDRESS;
            case operators[OP_INCREMENT].word   : return OP_INCREMENT;
            case operators[OP_DECREMENT].word   : return OP_DECREMENT;
            case operators[OP_NEGATION].word    : return OP_NEGATION;
            default                             : return OP_NONE;
        
        }

    }

    OperatorEnum findPostfixUnaryOperator(uint32_t word) {

        switch (word) {

            case operators[OP_INCREMENT].word   : return OP_INCREMENT;
            case operators[OP_DECREMENT].word   : return OP_DECREMENT;
            default                             : return OP_NONE;
        
        }

    }

    OperatorEnum findBinaryOperator(uint32_t word) {

        switch (word) {

            case operators[OP_ADDITION].word            : return OP_ADDITION;
            case operators[OP_SUBTRACTION].word         : return OP_SUBTRACTION;
            case operators[OP_MULTIPLICATION].word      : return OP_MULTIPLICATION;
            case operators[OP_DIVISION].word            : return OP_DIVISION;
            case operators[OP_MODULO].word              : return OP_MODULO;
            case operators[OP_LESS_THAN].word           : return OP_LESS_THAN;
            case operators[OP_GREATER_THAN].word        : return OP_GREATER_THAN;
            case operators[OP_LESS_THAN_OR_EQUAL].word  : return OP_LESS_THAN_OR_EQUAL;
            case operators[OP_GREATER_THAN_OR_EQUAL].word   : return OP_GREATER_THAN_OR_EQUAL;
            case operators[OP_EQUAL].word               : return OP_EQUAL;
            case operators[OP_NOT_EQUAL].word           : return OP_NOT_EQUAL;
            case operators[OP_BOOL_AND].word            : return OP_BOOL_AND;
            case operators[OP_BOOL_OR].word             : return OP_BOOL_OR;
            case operators[OP_CONCATENATION].word       : return OP_CONCATENATION;
            case operators[OP_MEMBER_SELECTION].word    : return OP_MEMBER_SELECTION;
            case operators[OP_BITWISE_AND].word         : return OP_BITWISE_AND;
            case operators[OP_BITWISE_OR].word          : return OP_BITWISE_OR;
            case operators[OP_BITWISE_XOR].word         : return OP_BITWISE_XOR;
            case operators[OP_BITWISE_NEGATION].word    : return OP_BITWISE_NEGATION;
            case operators[OP_SHIFT_RIGHT].word         : return OP_SHIFT_RIGHT;
            case operators[OP_SHIFT_LEFT].word          : return OP_SHIFT_LEFT;
            default     : return OP_NONE;
        
        }

    }






    // TODO : make it 'static' if its even possible
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
            new VariableDefinition(new Variable(SyntaxNode::root, DT_VOID), 0),
            IF_PRINTF
        ),

        Function(
            SyntaxNode::root,
            (char *) IFS_ALLOC,
            sizeof(IFS_ALLOC) - 1,
            std::vector<VariableDefinition*>({
                new VariableDefinition(new Variable(SyntaxNode::root, DT_MULTIPLE_TYPES), 0)
            }),
            new VariableDefinition(new Variable(SyntaxNode::root, DT_POINTER), 0),
            IF_ALLOC
        ),

        Function(
            SyntaxNode::root,
            (char *) IFS_FREE,
            sizeof(IFS_FREE) - 1,
            std::vector<VariableDefinition*>({
                new VariableDefinition(new Variable(SyntaxNode::root, DT_POINTER), 0)
            }),
            new VariableDefinition(new Variable(SyntaxNode::root, DT_VOID), 0),
            IF_FREE
        ),
    
    };

    // import related stuff
    
    // abstracted, so we can change the way we identify file
    struct FileId {
        uint64_t size;
        std::filesystem::file_time_type time;

        //FileId() = default;
        //FileId(const FileId&) = default;
        //FileId& operator=(const FileId&) = default;
        //FileId(FileId&&) noexcept = default;
        //FileId& operator=(FileId&&) noexcept = default;

        bool operator<(const FileId& other) const {
            return std::tie(size, time) < std::tie(other.size, other.time);
        }
    };

    // to represent tree of imports
    // so we can check for circular imports and log user full path
    struct ImportNode {
        FileId* fileId;
        Scope* fileScope;
        ImportStatement* import;
        ImportNode* parent;
        std::vector<ImportNode*> children;
    };

    ImportNode* importRoot = new ImportNode;
    ImportNode* importCurrent = new ImportNode;
    std::map<FileId, Namespace*> parsedFiles;

    int areFileIdEqual(FileId* a, FileId* b) {
        return a->size == b->size && a->time == b->time;
    }

    int doesImportExistInPath(ImportNode* pathNode, ImportNode* checkNode) {
        
        ImportNode* node = pathNode;
        while (node) {
            if (areFileIdEqual(node->fileId, checkNode->fileId)) {
                return 1;
            }
            node = node->parent;
        }

        return 0;
    
    }

    void logImportPath(ImportNode* node) {
        
        if (node->parent) {
            logImportPath(node->parent);
        }

        if (node->import) {
            Logger::log(Logger::PLAIN, " -> %.*s", NULL, 0, node->import->fname.len, node->import->fname.buff);
        } else {
            Logger::log(Logger::PLAIN, " import path: MAIN FILE");
        }

    }

    // meh
    std::filesystem::path* getPath(String fpath, String fname) {
        std::string strA(fpath.buff, fpath.len);
        std::string strB(fname.buff, fname.len);
        return new std::filesystem::path(std::filesystem::path(strA) / std::filesystem::path(strB));
    }

    // meh
    FileId* genFileId(std::filesystem::path* const path) {
        
        if (!std::filesystem::exists(*path)) return NULL;

        FileId* id = new FileId;
            
        id->size = std::filesystem::file_size(*path);
        id->time = std::filesystem::last_write_time(*path);
        
        return id;
    
    }


   
    inline void setParentIdx(SyntaxNode* node) {
        node->parentIdx = node->scope->children.size();
    }




    int parseFile(char* const flname, ImportNode* import) {
        
        char* buffer;
        if (FileDriver::readFile(flname, &buffer)) {
            return 1;
        }

        std::filesystem::path absPath = std::filesystem::absolute({flname});
        Location location = {
            new File { absPath, NULL, flname, buffer },
            1,
            0
        };

        importCurrent = import;

        // fileRootScope = fileScope ? fileScope : scope;
        return parseScope(import->fileScope, buffer, &location, SC_GLOBAL);

    }

    int processImport(ImportNode* currentNode, String fpath) {

        // 1) process all children imports in current file
        for (int i = 0; i < currentNode->children.size(); i++) {
            
            ImportNode* const importNode = currentNode->children[i];
            ImportStatement* import = importNode->import;

            // maybe store also file path in ImportNode
            std::filesystem::path* filePath = getPath(fpath, import->fname);
            importNode->fileId = genFileId(filePath);
            if (!importNode->fileId) {
                Logger::log(Logger::ERROR, "File %.*s does not exists!", import->loc, import->fname.len, import->fname.len, import->fname.buff);
                return Err::FILE_DOES_NOT_EXISTS;
            }

            if (parsedFiles.find(*(importNode->fileId)) != parsedFiles.end()) {

                // LOOK AT: reusing already parsed stuff, for now as reference
                // may be copy will be needed
                importNode->fileScope = parsedFiles[*(importNode->fileId)];
            
            } else {
                
                // as namespace, so we can easier switch in the future
                importNode->fileScope = new Namespace;
                importNode->fileScope->type = NT_SCOPE;
                
                const int err = parseFile((char*) strdup(filePath->string().c_str()), importNode);
                if (err != Err::OK) {
                    printf("return err;\n");
                    return err;
                }

                // parsedFiles.insert(std::make_pair(*(importNode->fileId), (Namespace*) importNode->fileScope));
                parsedFiles.emplace(*(importNode->fileId), (Namespace*) importNode->fileScope);
            
            }

            if (doesImportExistInPath(currentNode, importNode)) {
                Logger::log(Logger::ERROR, ERR_STR(Err::CIRCULAR_IMPORT), import->loc, import->fname.len);
                logImportPath(importNode);
                return Err::CIRCULAR_IMPORT;
            }            
            
            Scope* root = importNode->parent->fileScope ? importNode->parent->fileScope : SyntaxNode::root;
            switch (import->keyWord) {
                
                case -1 : {

                    // import foo from file

                    int symbolType = -1;
                    SyntaxNode* symbol = NULL;

                    Namespace* nsc = (Namespace*) (importNode->fileScope);
                    for (int i = 0; i < nsc->children.size(); i++) {
                        
                        SyntaxNode* node = nsc->children[i];
                        if (node->type == NT_NAMESPACE) {
                            
                            Namespace* nspace = (Namespace*) node;
                            if (nspace->nameLen == import->param.len && strncmp(nspace->name, import->param.buff, import->param.len) == 0) {
                                symbolType = NT_NAMESPACE;
                                symbol = nspace;
                                break;
                            }

                        } else if (node->type == NT_FUNCTION) {
                            
                            Function* fcn = (Function*) node;
                            if (fcn->nameLen == import->param.len && strncmp(fcn->name, import->param.buff, import->param.len) == 0) {
                                symbolType = NT_FUNCTION;
                                symbol = fcn;
                                break;
                            }

                        }

                    }

                    if (symbolType < 0) {
                        Logger::log(Logger::ERROR, "Aprepritee symbol not found! Note, only namespaces and functions can be exported.", import->loc, import->param.len);
                        return Err::UNEXPECTED_SYMBOL;
                    }

                    switch (symbolType) {
                        
                        case NT_NAMESPACE : {

                            Namespace* sc = getCopy((Namespace*) symbol);

                            sc->scope = root;
                            sc->parentIdx = 0;
        
                            Utils::pushFornt<SyntaxNode*>(root->children, sc);
                            Utils::pushFornt<Namespace*>(root->namespaces, sc);
                            
                            break;

                        }

                        case NT_FUNCTION : {
                            
                            Function* fcn = getCopy((Function*) symbol);

                            fcn->scope = root;
                            fcn->parentIdx = 0;
        
                            Utils::pushFornt<SyntaxNode*>(root->children, fcn);
                            Utils::pushFornt<Function*>(root->fcns, fcn); 
                            
                            break;

                        }
                    
                        default:
                            Logger::log(Logger::ERROR, "Aprepritee symbol not found! Note, only namespaces and functions can be exported.", import->loc, import->param.len);
                            return Err::UNEXPECTED_SYMBOL;
                    
                    }

                    break;
                
                }

                case KW_SCOPE : {

                    Scope* sc = (Namespace*) (importNode->fileScope);

                    sc->scope = root;
                    sc->type = NT_SCOPE;
                    sc->parentIdx = 0;

                    Utils::pushFornt<SyntaxNode*>(root->children, sc);

                    break;

                }

                case KW_FUNCTION : {

                    Function* fcn = new Function();
                    
                    fcn->bodyScope = (Namespace*) (importNode->fileScope);
                    fcn->name = import->param;
                    fcn->nameLen = import->param.len;
                    fcn->scope = root;
                    fcn->type = NT_FUNCTION;
                    fcn->outArg = createEmptyVariableDefinition();
                    fcn->outArg->var->cvalue.dtypeEnum = DT_VOID;
                    fcn->parentIdx = 0;
                    fcn->snFlags = 0;

                    // as we importing function, order doesn't matter, so we can push it back
                    root->children.push_back(fcn);
                    root->fcns.push_back(fcn);

                    break;

                }

                case KW_NAMESPACE : {

                    Namespace* sc = (Namespace*) (importNode->fileScope);

                    sc->name = import->param;
                    sc->nameLen = import->param.len;
                    sc->scope = root;
                    sc->type = NT_NAMESPACE;
                    sc->parentIdx = 0;
                    sc->snFlags = 0;

                    Utils::pushFornt<SyntaxNode*>(root->children, sc);
                    Utils::pushFornt<Namespace*>(root->namespaces, sc);
                    
                    // in case of namespace we dont need to update parentIdx for searchDefs
                    // but may be wrong..

                    break;

                }

            }

        }

        // 2) process new imports of each children
        for (int i = 0; i < currentNode->children.size(); i++) {
            ImportNode* node = currentNode->children[i];
            const int err = processImport(node, fpath);
            if (err != Err::OK) return err;
        }

        return Err::OK;

    }

    int parse(char* const flname) {

        // NOTE : future parallelism in mind

        int err;

        char* dirStr = flname;
        int dirLen = Utils::stripDir(flname);

        Logger::log(Logger::INFO, "Parsing...\n");

        SyntaxNode::root = new Scope;
        SyntaxNode::root->fcn = NULL;
        SyntaxNode::root->scope = NULL;
        SyntaxNode::root->parentIdx = 0;
        SyntaxNode::dir = new INamed(dirStr, dirLen);

        std::filesystem::path flpath(flname);
        importRoot->fileId = genFileId(&flpath);
        if (!importRoot->fileId) {
            Logger::log(Logger::ERROR, ERR_STR(Err::FILE_DOES_NOT_EXISTS), NULL, 0, flname);
            return Err::FILE_DOES_NOT_EXISTS;
        }

        importRoot->fileScope = SyntaxNode::root;
        importRoot->import = NULL;
        importRoot->parent = NULL;

        internalFunctionUsed = 0;

        err = parseFile(flname, importRoot);
        if (err != Err::OK) return err;

        err = processImport(importRoot, { dirStr, dirLen });
        if (err != Err::OK) return err;

        return 0;
    
    }

    // the begin char has to be already skipped
    int parseScope(Scope* scope, char* const str, Location* const loc, const ScopeType scopeType, const char endAsStatement) {
 
        while (1) {

            const int oldIdx = loc->idx - 1;
            
            if ((endAsStatement && str[oldIdx] == STATEMENT_END)) {
                return Err::OK;
            }

            const int err = Utils::skipWhiteSpacesAndComments(str, loc);
            if (err < 0) {

                if (err == Err::UNTERMINATED_COMMENT) return Err::UNTERMINATED_COMMENT;
                if (scopeType == SC_GLOBAL) return Err::OK;

                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_END_OF_FILE), loc);
                return Err::UNEXPECTED_END_OF_FILE;

            }

            const char ch = str[loc->idx];

            if (ch == STATEMENT_END) {

                loc->idx++;
            
            } else if (ch == SCOPE_BEGIN) {

                Scope* newScope = new Scope;
                newScope->fcn = currentFunction;
                newScope->scope = scope;
                setParentIdx(newScope);
                
                // pushDefLike(scope->defSearch, newScope);

                loc->idx++;
                const int err = parseScope(newScope, str, loc);
                if (err < 0) return err;

                scope->children.push_back(newScope);
                //pushDefLike(scope->defSearch, newScope);

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
                if (err == Err::UNEXPECTED_SYMBOL) {

                    Variable* var = new Variable(loc);
                    var->scope = scope;

                    const int err = parseExpression(var, str, loc);
                    if (err < 0) return err;

                    // LOOK AT : maybe add to vars as well
                    Scope::root->children.push_back(var);
                
                } else if (err < 0) {

                    return err;
                
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
                
                Location* tagLoc = getLocationStamp(loc);
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

                Label* label = new Label();
                label->loc = getLocationStamp(loc);
                label->scope = scope;
                label->name = str + loc->idx;
                label->nameLen = Utils::findVarEnd(str + loc->idx);
                setParentIdx(label);

                scope->children.push_back(label);
                scope->labels.push_back(label);
                SyntaxNode::labels.push_back(label);

                auto res = scope->defSearch.insert({std::string_view(label->name, label->nameLen), label});
                if (!res.second) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::SYMBOL_ALREADY_DEFINED), label->loc, label->nameLen);
                    return Err::SYMBOL_ALREADY_DEFINED;
                }
                //pushDefLike(scope->defSearch, label);

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
                    // TODO : mem leak probably
                    Location *const startLoc = getLocationStamp(loc);

                    char *const word = str + loc->idx;
                    err = skipScopeNames(str, loc);
                    if (err < 0) return err;
                    const int wordLen = loc->idx - startLoc->idx;
                    // const int wordLen = Utils::findVarEnd(word);

                    //loc->idx += wordLen;

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                    const char tmpCh = str[loc->idx];
                    if (wordLen <= 0 || (!IS_ALLOWED_VARIABLE_CHAR(tmpCh) && tmpCh != POINTER_SYMBOL)) {

                        COPY_LOC(loc, startLoc);

                        Variable* const var = new Variable(scope);
                        var->loc = startLoc;
                        
                        const int err = parseExpression(var, str, loc, (1 << 8) | '=' + (STATEMENT_END << 8 ));
                        if (err < 0) return err;

                        if (str[loc->idx - 1] == '=') {
                            // assignment

                            VariableAssignment* const varAssignment = new VariableAssignment(loc);
                            varAssignment->loc->idx--;
                            varAssignment->scope = scope;
                            varAssignment->lvar = var;
                            varAssignment->rvar = new Variable(scope);
                            varAssignment->rvar->loc = getLocationStamp(loc);

                            const int err = parseRValue(varAssignment->rvar, str, loc, scope, STATEMENT_END);
                            // const int err = parseExpression(varAssignment->rvar, str, loc);
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

                        VariableDefinition* varDef = new VariableDefinition(startLoc);

                        varDef->dtype = new INamedVar();
                        parseScopeNames(varDef->dtype, str, startLoc);                        
                        varDef->loc->idx -= varDef->dtype->nameLen + 1;

                        err = processDataType(DT_CUSTOM, scope, str, loc, 0, STATEMENT_END, &varDef, 1, 0);
                        if (err < 0) return err;

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

    /*
    int parseVariableDefinitionLValue(Scope* scope, char* const str, Location* const loc, uint16_t endChar, VariableDefinition** outVarDef, uint64_t param) {
        
        const uint8_t fEndCh = endChar;
        const uint8_t sEndCh = endChar >> 8;

        Location* defLoc;
        DataTypeEnum dtype;
        char* dtypeName;
        int dtypeNameLen;

        const int startIdx = loc->idx;
        const int keyWord = selectKeyWord(keyWords, KEY_WORDS_COUNT, str, &(loc->idx));
        
        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

        if (keyWord == KW_CONST || keyWord == KW_CMP_TIME) {
            
            param = param | ((keyWord == KW_CONST) ? KW_CONST : KW_CMP_TIME);
            
            // if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

            defLoc = getLocationStamp(loc);

            dtypeName = str + loc->idx;
            dtypeNameLen = Utils::findVarEnd(dtypeName);

            loc->idx += dtypeNameLen;

        } else {

            defLoc = getLocationStamp(loc);

            dtypeName = str + startIdx;

            if (keyWord < 0) {
                dtypeNameLen = Utils::findVarEnd(dtypeName);
                loc->idx += dtypeNameLen;
            } else {
                dtypeNameLen = loc->idx - startIdx;
            }

        }

        if (dtypeNameLen == 0) {
            // TODO : error
            return Err::UNEXPECTED_SYMBOL;
        }

        dtype = (DataTypeEnum) findDataType(dtypeName, dtypeNameLen);
        dtype = (dtype == DT_UNDEFINED) ? DT_CUSTOM : dtype;

        VariableDefinition* varDef = new VariableDefinition(loc);
        Variable* var = new Variable(scope, dtype);

        varDef->scope = scope;
        varDef->var = var;
        varDef->flags = param;

        Pointer* lastPtr = NULL;
        var->cvalue.dtypeEnum = dtype;
        var->cvalue.any = (dtype == DT_CUSTOM) ? NULL : dataTypes + dtype;
        parseDataTypeDecorators(scope, str, loc, var, &lastPtr, 1);

        var->def = varDef;
        var->unrollExpression = 0;
        var->loc = getLocationStamp(loc);
        var->name = str + loc->idx;
        var->nameLen = Utils::findVarEnd(var->name);
        
        ASSIGN_ID(var);

        loc->idx += var->nameLen;

        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
        
        const char ch = str[loc->idx];
        if (ch != fEndCh || ch != sEndCh) {
            // TODO : error
            return Err::UNEXPECTED_SYMBOL;
        }

        if (outVarDef) {
            *outVarDef = varDef;
        } else {
            scope->children.push_back(varDef);
            // pushDefLike(scope->defSearch, varDef->var);
            //scope->defSearch.push_back(varDef);
            //scope->defs.push_back(varDef);
        }
        
        //scope->defs.push_back(var);

        //scope->vars.push_back(var);

        return Err::OK;

    }
        */

    // returns DataTypeEnum or error
    // def->flags will be rewritten
    int parseDataType(Scope* scope, char* str, Location* loc, const int expectQualifier, VariableDefinition* def) {

        DataTypeEnum dtype;
        char* dtypeName;
        int dtypeLen;

        const int startIdx = loc->idx;
        const int keyWord = selectKeyWord(keyWords, KEY_WORDS_COUNT, str, &(loc->idx));
        if (keyWord == KW_CONST || keyWord == KW_CMP_TIME) {
            
            if (!expectQualifier) {
                Logger::log(Logger::ERROR, "Qualifier not expected here!", loc, 1);
                return Err::UNEXPECTED_SYMBOL;
            }

            def->flags = ((keyWord == KW_CONST) ? KW_CONST : KW_CMP_TIME);

            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

            def->loc = getLocationStamp(loc);

            dtypeName = str + loc->idx;
            dtypeLen = Utils::findVarEnd(dtypeName);

            loc->idx += dtypeLen;

        } else {

            def->loc = getLocationStamp(loc);
            def->loc->idx = startIdx;

            dtypeName = str + startIdx;

            if (keyWord < 0) {
                dtypeLen = Utils::findVarEnd(dtypeName);
                loc->idx += dtypeLen;
            } else {
                dtypeLen = loc->idx - startIdx;
            }

        }

        if (dtypeLen == 0) {
            Logger::log(Logger::ERROR, "Data type expected!", loc, 1);
            return Err::UNEXPECTED_SYMBOL;
        }

        def->var = new Variable(scope);
        def->var->def = def;
        def->var->unrollExpression = 0;
        def->var->expression = NULL;
        def->scope = scope;

        dtype = (DataTypeEnum) findDataType(dtypeName, dtypeLen);
        if (dtype == DT_UNDEFINED) {
            const int klen = strlen(KWS_FUNCTION);
            if (strncmp(KWS_FUNCTION, dtypeName, dtypeLen > klen ? dtypeLen : klen) == 0) {
                
                FunctionPrototype* fptr;
                const int err = parseFunctionPointer(scope, str, loc, &fptr);
                if (err < 0) return err;

                def->var->cvalue.fcn = fptr;
                def->var->cvalue.dtypeEnum = (DataTypeEnum) DT_FUNCTION;

            } else {

                if (dtype == DT_UNDEFINED) {
                    // dt_custom case
                    def->var->cvalue.dtypeEnum = DT_CUSTOM;
                    def->var->cvalue.any = NULL;
                    def->dtype = new INamedVar();
                    def->dtype->name = dtypeName;
                    def->dtype->nameLen = dtypeLen;

                    SyntaxNode::customDataTypesReferences.push_back(def);
                } else {
                    def->var->cvalue.dtypeEnum = (DataTypeEnum) dtype;
                    def->var->cvalue.any = NULL;
                }
            
            }
        } else {
            def->var->cvalue.dtypeEnum = dtype;
            def->var->cvalue.any = (dtype == DT_CUSTOM) ? NULL : dataTypes + dtype;
        }

        Pointer* lastPtr = NULL;
        const int err = parseDataTypeDecorators(scope, str, loc, def->var, &lastPtr, 1);
        if (err < 0) return err;

        def->lastPtr = lastPtr;
        def->var->loc = getLocationStamp(loc);

        return Err::OK;

    }

    int parseFunctionPointer(Scope* scope, char* const str, Location* loc, FunctionPrototype** fcnOut) {

        FunctionPrototype* fcn = new FunctionPrototype;

        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

        if (str[loc->idx] != FUNCTION_START) {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 1);            
            return Err::UNEXPECTED_SYMBOL;
        }

        loc->idx++;
        
        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
        
        const char ch = str[loc->idx];
        if (ch == '-' && str[loc->idx + 1] == '>') {
            loc->idx += 2;
            goto fOutArgs;
        }

        while (1) {
            
            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

            VariableDefinition* def = new VariableDefinition;
            
            DataTypeEnum dtype = (DataTypeEnum) parseDataType(scope, str, loc, 1, def);
            if (dtype < 0) return dtype;

            fcn->inArgs.push_back(def);
            
            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

            const char ch = str[loc->idx];
            if (ch == ',') {
                loc->idx++;
                continue;
            } else if (ch == '-' && str[loc->idx + 1] == '>') {
                loc->idx += 2;
                break;
            } else {
                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 1);
                return Err::UNEXPECTED_SYMBOL;
            }

        }

        fOutArgs:

        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

        VariableDefinition* def;
        if (str[loc->idx] != FUNCTION_END) {
            
            def = new VariableDefinition;
            DataTypeEnum dtype = (DataTypeEnum) parseDataType(scope, str, loc, 0, def);
            if (dtype < 0) return dtype;

            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
            if (str[loc->idx] != FUNCTION_END) {
                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 1);
                return Err::UNEXPECTED_SYMBOL;
            }

        } else {

            def = createEmptyVariableDefinition();
        
        }

        fcn->outArg = def;
        *fcnOut = fcn;

        loc->idx++;

        return Err::OK;

    }

    // part starting with variable name in definition
    // ex: const int^ x ... from x
    int parseDefinitionAssignment(Scope* scope, char* const str, Location* const loc, VariableDefinition* const def, uint16_t endChar, int includeToScope) {

        const uint8_t fEndCh = endChar;
        const uint8_t sEndCh = endChar >> 8;

        def->var->name = str + loc->idx;
        def->var->nameLen = Utils::findVarEnd(def->var->name);
        
        ASSIGN_ID(def->var);

        loc->idx += def->var->nameLen;

        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
        
        const char ch = str[loc->idx];
        if (ch == '=') {
            
            loc->idx++;

            const int err = parseRValue(def->var, str, loc, scope, endChar);
            if (err < 0) return err;

            if (def->flags & IS_CMP_TIME) SyntaxNode::cmpTimeVars.push_back(def->var);
            else SyntaxNode::initializations.push_back(def);

        } else if (ch == fEndCh || ch == sEndCh) {

            loc->idx++;
            def->var->expression = NULL;
        
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

        if (includeToScope) {
            //scope->children.push_back(def);
            // pushDefLike(scope->defSearch, def->var);
            
            setParentIdx(def->var);

            auto res = scope->defSearch.insert({std::string_view(def->var->name, def->var->nameLen), def->var});
            if (!res.second) {
                Logger::log(Logger::ERROR, ERR_STR(Err::SYMBOL_ALREADY_DEFINED), def->var->loc, def->var->nameLen);
                return Err::SYMBOL_ALREADY_DEFINED;
            }

            scope->children.push_back(def);
            SyntaxNode::variableDefinitions.push_back(def);
            scope->defs.push_back(def->var);
        }

        return Err::OK;

    }

    int parseVariableDefinition(Scope* scope, char* const str, Location* const loc, uint64_t param, uint16_t endChar, VariableDefinition** outVarDef) {       

        VariableDefinition* varDef = new VariableDefinition;
        DataTypeEnum dtype = (DataTypeEnum) parseDataType(scope, str, loc, 1, varDef);
        if (dtype < 0) return dtype;

        const int err = parseDefinitionAssignment(scope, str, loc, varDef, endChar, 0);
        if (err) return err;

        *outVarDef = varDef;
        return Err::OK;

    }

    int processDataType(
        const DataTypeEnum dtype, 
        Scope* const scope, 
        char* const str, 
        Location* const loc, 
        const uint64_t param, 
        const uint16_t endChar,
        VariableDefinition** outVarDef,
        const int include,
        const int alloc
    ) {

        VariableDefinition* def;
        if (alloc) {
            def = new VariableDefinition(loc);
        } else {
            def = *outVarDef;
        }
        
        def->var = new Variable(scope);
        def->var->def = def;
        def->var->cvalue.dtypeEnum = dtype;
        def->var->cvalue.any = (dtype == DT_CUSTOM) ? NULL : dataTypes + dtype;
        def->var->unrollExpression = 0;
        def->var->expression = NULL;
        def->flags = param;
        def->scope = scope;

        Pointer* lastPtr = NULL;
        int err = parseDataTypeDecorators(scope, str, loc, def->var, &lastPtr, 1);
        if (err < 0) return err;

        def->lastPtr = lastPtr;
        def->var->loc = getLocationStamp(loc);

        err = parseDefinitionAssignment(scope, str, loc, def, endChar, include);
        if (err < 0) return err;

        if (outVarDef) *outVarDef = def;
        return Err::OK;

    }

    // already without starting '"'
    int parseStringLiteral(Scope *scope, char *const str, Location *const loc) {

        // "asdasd";
        // "asdasd"b;
        // "%i\n" 3;

        const int startIdx = loc->idx;

        while (1) {

            const char ch = str[loc->idx];
            if (ch == '"') {

                internalFunctionUsed = internalFunctionUsed | (1 << (IF_PRINTF - 1));

                Function* const fcn = internalFunctions + (IF_PRINTF - 1);

                FunctionCall* fcnCall = new FunctionCall;
                fcnCall->fptr = NULL;
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

                // TODO : wtf even is this
                ExpressionWrapper *exWrapper = new ExpressionWrapper;
                exWrapper->operand = operand;
                exWrapper->operand->expression = fcnCall;

                loc->idx++;

                while (1) {

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                    char ch = str[loc->idx];
                    if (ch == STATEMENT_END) {
                        break;
                        //loc->idx++;
                        //return Err::OK;
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
                        break;
                        //loc->idx++;
                        //scope->children.push_back(exWrapper);
                        //return Err::OK;
                    } else {
                        Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
                        return Err::UNEXPECTED_SYMBOL;
                    }
                
                }

                loc->idx++;
                scope->children.push_back(exWrapper);
                return Err::OK;

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
        const int rawStringRequired = (str[loc->idx + 1] == 'b') ? 1 : 0;

        StringInitialization* init = new StringInitialization;
        init->rawStr = std::string(str + startIdx, strLen);
        init->rawPtr = str + startIdx;
        init->rawPtrLen = strLen;

        // meh but whatever
        if (rawStringRequired) {
            init->wideStr = NULL;
            init->wideDtype = DT_UINT_8;
            loc->idx++;
        } else {

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

            const int err = parseExpression(var, str, loc, toDoubleChar(',', ']'));
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
        const int startLine = loc->line;

        loc->idx += Utils::findVarEnd(str + loc->idx);
        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

        if (str[loc->idx] == ':' || CMP_TWO_CHARS(str + loc->idx, FILL_THE_REST_SYMBOL)) {
            // names assumed

            loc->idx = startIdx;
            loc->line = startLine;

            int hasFillVar = 0;
            (*dtypeInit)->fillVar = NULL;

            while (1) {

                Variable* var = new Variable(scope);
                var->name = str + loc->idx;
                var->nameLen = Utils::findVarEnd(var->name);
                var->loc = getLocationStamp(loc);

                if (var->nameLen == 0) {
                    
                    if (!CMP_TWO_CHARS(str + loc->idx, FILL_THE_REST_SYMBOL)) {
                        Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_ATTRIBUTE_NAME), loc, 1);
                        return Err::INVALID_ATTRIBUTE_NAME;
                    }
                    
                    if (hasFillVar) {
                        Logger::log(Logger::ERROR, "Fill the rest symbol can be used only once per initialization!", loc, 2);
                        return Err::UNEXPECTED_SYMBOL;
                    }

                    loc->idx += 2;
                    (*dtypeInit)->fillVar = var;
                    hasFillVar = 1;
                
                } else {
                    
                    (*dtypeInit)->attributes.push_back(var);
                    loc->idx += var->nameLen;
                
                }

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                if (str[loc->idx] != ':') {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 1);
                    return Err::UNEXPECTED_SYMBOL;
                }

                loc->idx++;

                const int err = parseExpression(var, str, loc, toDoubleChar(',', '}'));
                if (err < 0) return err;

                if (str[loc->idx - 1] == SCOPE_END) break;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

            }

        } else {

            loc->idx = startIdx;
            loc->line = startLine;

            while (1) {
                
                Variable* var = new Variable();

                const int err = parseExpression(var, str, loc, toDoubleChar(',', '}'));
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
    // alloc [DtypeName, omitted if mainDtype >= 0] ['[' Expression defining length ']'] [:] [DtypeInit]
    int parseRValue(Variable* outVar, char* str, Location* loc, Scope* scope, uint16_t endChar) {
        
        DataTypeEnum mainDtype = outVar->cvalue.dtypeEnum;
        if (mainDtype == DT_POINTER) {
            mainDtype = outVar->cvalue.ptr->pointsToEnum;
        } else if (mainDtype == DT_ARRAY) {
            mainDtype = outVar->cvalue.arr->pointsToEnum;
        }

        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

        // TODO : remove alloc from keyWords to own collection or just harcode it here
        const int keyWordIdx = selectKeyWord(keyWords, KEY_WORDS_COUNT, str, &(loc->idx));
        if (keyWords[keyWordIdx].type == KW_ALLOC) {
        
            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

            DataTypeEnum dtype = mainDtype;
            INamedVar* dtypeFullName = NULL;
            char* dtypeName = str + loc->idx; // outVar->def->dtypeName;
            int dtypeNameLen = Utils::findVarEnd(dtypeName); // outVar->def->dtypeNameLen;
            
            if (dtypeNameLen > 0) {
                
                loc->idx += dtypeNameLen;

                dtype = (DataTypeEnum)findDataType(dtypeName, dtypeNameLen);
                if (dtype == DT_UNDEFINED) {
                    dtype = DT_CUSTOM;
                } else {
                    dtypeName = NULL;
                    dtypeNameLen = 0;
                }
            
            } else {

                if (outVar->def) {
                    dtypeFullName = outVar->def->dtype;
                }
                
                if (mainDtype <= 0) {
                    Logger::log(Logger::ERROR, "TODO : error parseRValue alloc requires dtype name! Can be omitted only in definition!");
                    return Err::INVALID_DATA_TYPE;
                }

            }

            Function* const fcn = internalFunctions + (IF_ALLOC - 1);

            FunctionCall* fcnCall = new FunctionCall();
            fcnCall->fptr = NULL;
            fcnCall->fcn = fcn;
            fcnCall->name = fcn->name;
            fcnCall->nameLen = fcn->nameLen;
            fcnCall->outArg = new Variable();
            fcnCall->outArg->cvalue.dtypeEnum = DT_POINTER;

            VariableDefinition* varDef = new VariableDefinition();
            if (!dtypeFullName) {
                varDef->dtype = new INamedVar();
                varDef->dtype->name = dtypeName;
                varDef->dtype->nameLen = dtypeNameLen;
            } else {
                varDef->dtype = dtypeFullName;
            }
            varDef->scope = scope;
            varDef->loc = getLocationStamp(loc);

            Variable* var = new Variable();
            var->def = varDef;
            var->cvalue.dtypeEnum = dtype;
            var->cvalue.any = (dtype == DT_CUSTOM) ? NULL : dataTypes + dtype;

            varDef->var = var;

            Pointer* lastPtr = NULL;
            parseDataTypeDecorators(scope, str, loc, var, &lastPtr, 0);
            varDef->lastPtr = lastPtr;

            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
            if (str[loc->idx] == STATEMENT_BEGIN) {
                loc->idx++;
                const int err = parseExpression(var, str, loc, endChar);
                if (err < 0) return err;
            } else if (str[loc->idx] != STATEMENT_END) {
                Logger::log(Logger::ERROR, "TODO error: parseRValue unexpected symbol!");
                return Err::UNEXPECTED_SYMBOL;
            } else {
                loc->idx;
            }
            
            fcnCall->inArgs.push_back(var);

            outVar->expression = fcnCall;
            SyntaxNode::fcnCalls.push_back(outVar);
            // initializations.push_back(varDef);
            
            // ...
            if (mainDtype == DT_CUSTOM) SyntaxNode::customDataTypesReferences.push_back(varDef);
            else if (var->cvalue.dtypeEnum == DT_CUSTOM) SyntaxNode::customDataTypesReferences.push_back(varDef);

            outVar->snFlags |= IS_ALLOCATION;

        } else {
            
            const int err = parseExpression(outVar, str, loc, endChar);
            if (err < 0) return err;

        }

        return Err::OK;

    }
    

    // pointers can ocur only before arrays
    // for now only one array
    int parseDataTypeDecorators(Scope* scope, char* const str, Location* const loc, Variable* var, Pointer** lastPointer, int include) {

        //var->cvalue.dtypeEnum = dtype;
        //var->cvalue.any = (dtype == DT_CUSTOM) ? NULL : dataTypes + dtype;

        int wasArray = 0;

        Pointer* mainPtr = NULL;
        while (1) {

            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
            
            const char ch = str[loc->idx];
            if (ch == POINTER_SYMBOL) {

                if (wasArray) {
                    Logger::log(Logger::ERROR, "Pointer can't be used after array declaration!", loc, 1);
                    return Err::UNEXPECTED_SYMBOL;
                }
            
                Pointer* ptr = new Pointer();
                if (!mainPtr) *lastPointer = ptr;
                else mainPtr->parentPointer = ptr;

                ptr->parentPointer = mainPtr;
                ptr->pointsTo = var->cvalue.any;
                ptr->pointsToEnum = var->cvalue.dtypeEnum;

                var->cvalue.dtypeEnum = DT_POINTER;
                var->cvalue.ptr = ptr;
                // var->dtype = (void*) ptr;

                loc->idx++;

                mainPtr = ptr;

            } else if (ch == ARRAY_BEGIN) {
                // either const / embed or expression
                
                if (wasArray) {
                    Logger::log(Logger::ERROR, "Multidimensional arrays not allowed!", loc, 1);
                    return Err::UNEXPECTED_SYMBOL;
                }

                wasArray = 1;

                loc->idx++;

                Array* arr = new Array;
                if (!mainPtr) *lastPointer = arr;
                else mainPtr->parentPointer = arr;

                arr->pointsTo = var->cvalue.any;
                arr->pointsToEnum = var->cvalue.dtypeEnum;

                Variable* lenVar = new Variable(loc);
                lenVar->scope = scope;
                lenVar->nameLen = 0;
                // lenVar->parentStruct = NULL;
                
                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                const int keyWordIdx = selectKeyWord(keyWords, KEY_WORDS_COUNT, str, &(loc->idx));
                const int kword = (keyWordIdx < 0) ? -1 : keyWords[keyWordIdx].type;
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

                    const int err = parseExpression(lenVar, str, loc, ARRAY_END, 0, 1);
                    if (err < 0 && err != Err::UNEXPECTED_END_OF_EXPRESSION) return err;

                    arr->flags = 0;
                    arr->length = lenVar; // var->allocSize = lenVar;
                
                }
                
                // var->flags = varDef->flags ^ IS_ARRAY;
                var->cvalue.dtypeEnum = DT_ARRAY;
                var->cvalue.arr = arr;
                // var->dtype = (void*) arr;
                var->flags = 0;

                if (include) SyntaxNode::arrays.push_back(var);

            } else {

                break;
            
            }

        }

        return Err::OK;

    }

    
    // LOOK AT : dont like it, maybe rework at the later stage when more info will be better defined
    // if outVarDef is NULL, then new VariableDefinition will be added to scope.children, otherwise it will be returned
    // endChar is used as two wchars (first 16 bits and last 16 bits) that can end parsing
    /*
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

        Pointer* lastPtr = NULL;
        const int err = parseDataTypeDecorators(dtype, scope, str, loc, var, &lastPtr);
        if (err < 0) return err;

        varDef->lastPtr = lastPtr;

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
            var->expression = NULL;
        
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
            pushDefLike(scope->defSearch, varDef->var);
            //scope->defs.push_back(varDef);
        }
        //scope->vars.push_back(var);
        scope->defs.push_back(var);

        return Err::OK;

    }
    */

    /*
    int parseStructLikeDefinition(Scope* scope, char* const str, Location* loc, std::vector<Variable*> &attributes) {

        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

        char* name = str + loc->idx;
        int nameLen = Utils::findVarEnd(name);

        if (str[loc->idx] != SCOPE_BEGIN) {
            return Err::UNEXPECTED_SYMBOL;
        }

        newUnion->scope = scope;
        newUnion->name = name;
        newUnion->nameLen = nameLen;
        newUnion->loc = getLocationStamp(loc);
        
        newUnion->id = defId;
        defId++;

        loc->idx++;
        while (1) {

            int err;
            char ch;

            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

            if (str[loc->idx] == SCOPE_END) {
                loc->idx++;
                break;
            }

            VariableDefinition* varDef;
            err = parseVariableDefinition(scope, str, loc, param, '};', &varDef);
            if (err) return err;
            
            if (varDef->var->expression) {
                return Err::UNEXPECTED_RVALUE;
                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_RVALUE), loc);
            }
            
            newUnion->vars.push_back(varDef->var);

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
    
    }
        */

    int parseKeyWord(KeyWordType keyWord, Scope *scope, char *const str, Location *loc, uint64_t param) {

        switch (keyWord) {

            case KW_INT: 
                return processDataType(DT_INT, scope, str, loc, param, STATEMENT_END, NULL, 1);

            case KW_INT_8:
                return processDataType(DT_INT_8, scope, str, loc, param, STATEMENT_END, NULL, 1);

            case KW_INT_16:
                return processDataType(DT_INT_16, scope, str, loc, param, STATEMENT_END, NULL, 1);

            case KW_INT_32:
                return processDataType(DT_INT_32, scope, str, loc, param, STATEMENT_END, NULL, 1);

            case KW_INT_64:
                return processDataType(DT_INT_64, scope, str, loc, param, STATEMENT_END, NULL, 1);

            case KW_UINT_8:
                return processDataType(DT_UINT_8, scope, str, loc, param, STATEMENT_END, NULL, 1);

            case KW_UINT_16:
                return processDataType(DT_UINT_16, scope, str, loc, param, STATEMENT_END, NULL, 1);

            case KW_UINT_32:
                return processDataType(DT_UINT_32, scope, str, loc, param, STATEMENT_END, NULL, 1);

            case KW_UINT_64:
                return processDataType(DT_UINT_64, scope, str, loc, param, STATEMENT_END, NULL, 1);

            case KW_FLOAT_32:
                return processDataType(DT_FLOAT_32, scope, str, loc, param, STATEMENT_END, NULL, 1);

            case KW_FLOAT_64:
                return processDataType(DT_FLOAT_64, scope, str, loc, param, STATEMENT_END, NULL, 1);

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
                    const int err = processDataType(DT_CUSTOM, scope, str, loc, param, STATEMENT_END, &varDef, 1);
                    if (err < 0) return err;

                    varDef->loc = defLoc;

                    varDef->dtype = new INamedVar();
                    varDef->dtype->name = dtypeName;
                    varDef->dtype->nameLen = dtypeNameLen;
                    varDef->parentIdx = varDef->var->parentIdx;

                    //scope->children.push_back(varDef);
                    //pushDefLike(scope->defSearch, varDef);
                    auto res = scope->defSearch.insert({std::string_view(varDef->var->name, varDef->var->nameLen), varDef});
                    if (!res.second) {
                        Logger::log(Logger::ERROR, ERR_STR(Err::SYMBOL_ALREADY_DEFINED), varDef->var->loc, varDef->var->nameLen);
                        return Err::SYMBOL_ALREADY_DEFINED;
                    }
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
                
                const int startIdx = loc->idx;

                // first test if it could bee fcn pointer
                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                if (str[loc->idx] == FUNCTION_START) {
                    
                    loc->idx = startIdx - 3; // TODO
                    
                    VariableDefinition* def;
                    const int err = parseVariableDefinition(scope, str, loc, param, STATEMENT_END, &def);
                    if (err < 0) return err;
                    
                    // pushDefLike(scope->defSearch, def->var);
                    auto res = scope->defSearch.insert({std::string_view(def->var->name, def->var->nameLen), def->var});
                    if (!res.second) {
                        Logger::log(Logger::ERROR, ERR_STR(Err::SYMBOL_ALREADY_DEFINED), def->var->loc, def->var->nameLen);
                        return Err::SYMBOL_ALREADY_DEFINED;
                    }

                    scope->children.push_back(def);
                    SyntaxNode::variableDefinitions.push_back(def);
                    scope->defs.push_back(def->var);

                    return Err::OK;
                
                }

                Function* fcn;

                Scope* newScope = new Scope();
                newScope->fcn = currentFunction;
                newScope->scope = scope;
                setParentIdx(newScope);

                //pushDefLike(scope->defSearch, newScope);
                
                //Scope* outerScope = new Scope();
                //outerScope->fcn = currentFunction;
                //outerScope->scope = scope;

                //if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                int foreignLang = 0;
                if (str[loc->idx] == '[') {
                    // 
                    
                    fcn = new ForeignFunction();

                    foreignLang = 1;
                    Location* tagLoc = getLocationStamp(loc);
                    const int tagLen = parseLanguageTag(str, loc);

                    ((ForeignFunction*) fcn)->tagLen = tagLen;
                    ((ForeignFunction*) fcn)->tagStr = str + loc->idx - tagLen - 1;
                    ((ForeignFunction*) fcn)->tagLoc = tagLoc;
                    tagLoc->idx++;
                    
                    ASSIGN_ID(fcn);

                } else {
                    fcn = new Function();
                }

                fcn->inArgsCnt = 0;
                fcn->scope = scope;
                fcn->errorSetName = NULL;
                
                fcn->outArg = createEmptyVariableDefinition();

                fcn->loc = getLocationStamp(loc);

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

                    fcn->inArgsCnt++;

                    VariableDefinition* varDef;
                    const int err = parseVariableDefinition(newScope, str, loc, param, CHAR_CAT(')',','), &varDef);
                    if (err) return err;

                    //pushDefLike(newScope->defSearch, varDef->var);
                    //setParentIdx(varDef->var);
                    // :)
                    varDef->var->parentIdx = -1;
                    varDef->parentIdx = -1;

                    auto res = newScope->defSearch.insert({std::string_view(varDef->var->name, varDef->var->nameLen), varDef->var});
                    if (!res.second) {
                        Logger::log(Logger::ERROR, ERR_STR(Err::SYMBOL_ALREADY_DEFINED), varDef->var->loc, varDef->var->nameLen);
                        return Err::SYMBOL_ALREADY_DEFINED;
                    }
                    
                    SyntaxNode::variableDefinitions.push_back(varDef);
                    newScope->defs.push_back(varDef->var);

                    fcn->inArgs.push_back(varDef);

                    if (str[loc->idx - 1] == ',') {
                        //loc->idx++;
                    } else if (str[loc->idx - 1] == FUNCTION_END) {
                        //loc->idx++;
                        break;
                    }

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                
                }

                // [using 'Error Set'] ->
                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                
                {
                    const int len = Utils::findVarEnd(str + loc->idx);
                    if (len > 0 && strncmp(str + loc->idx, KWS_USING, len) == 0) {

                        loc->idx += strlen(KWS_USING);
                        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                        
                        fcn->errorSetName = new INamedVar();
                        const int err = parseScopeNames(fcn->errorSetName, str, loc);
                        if (err < 0) {
                            return err;
                        }

                        //char* name = str + loc->idx;
                        //int len = Utils::findVarEnd(name);
                        
                        /*
                        if (len <= 0) {
                            Logger::log(Logger::ERROR, "Error set name expected!", loc, 1);
                            return Err::INVALID_VARIABLE_NAME;
                        }

                        fcn->errorSetName = name;
                        fcn->errorSetNameLen = len;
                        */

                        Using* tmp = new Using();
                        tmp->var = fcn;
                        
                        newScope->usings.push_back(tmp);
                        //pushDefLike(newScope->defSearch, tmp);

                        //loc->idx += len;
                        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                        
                    }

                }
                
                {
                    uint16_t tmp = *((uint16_t *)(str + loc->idx));
                    if (tmp != ('-' | ('>' << 8))) {

                        if ((tmp & 0xFF) == SCOPE_BEGIN) {
                            if (foreignLang) loc->idx++;
                            goto fcnParseScope;
                        }

                        // LOOK AT : maybe better name for this situation, something like "unexpected char sequence"
                        Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 2);
                        Logger::log(Logger::HINT, "'->' expected\n");
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

                    fcn->outArg->var->cvalue.dtypeEnum = (DataTypeEnum) ((dtype == DT_UNDEFINED) ? DT_CUSTOM : dtype);

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

                //Scope* newScope = new Scope;
                //newScope->fcn = currentFunction;
                //newScope->scope = outerScope;

                currentFunction = fcn;
                const int err = parseScope(newScope, str, loc);
                if (err < 0) return err;
                currentFunction = NULL;

                fcn->bodyScope = newScope;

                scope->children.push_back(fcn);
                scope->fcns.push_back(fcn);
                SyntaxNode::fcns.push_back(fcn);

                break;
            
            }

            case KW_IF: {

                const int parentIdx = scope->children.size();

                int err = 0;
                // int linesSkipped = 0;

                Scope* newScope = new Scope();
                newScope->fcn = currentFunction;
                newScope->scope = scope;
                newScope->parentIdx = parentIdx;

                Variable* newOperand = new Variable(scope);
                newOperand->loc = getLocationStamp(loc);

                err = parseExpression(newOperand, str, loc, CHAR_CAT(STATEMENT_BEGIN, SCOPE_BEGIN));
                if (err < 0) return err;

                //pushDefLike(scope->defSearch, newScope);

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

                        Scope* newScope = new Scope();
                        newScope->fcn = currentFunction;
                        newScope->scope = scope;
                        newScope->parentIdx = parentIdx;

                        //pushDefLike(scope->defSearch, newScope);

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

                        Variable* newOperand = new Variable(scope);
                        newOperand->loc = getLocationStamp(loc);

                        err = parseExpression(newOperand, str, loc, CHAR_CAT(STATEMENT_BEGIN, SCOPE_BEGIN));
                        if (err < 0) return err;

                        Scope *newScope = new Scope();
                        newScope->fcn = currentFunction;
                        newScope->scope = scope;
                        newScope->parentIdx = parentIdx;

                        //pushDefLike(scope->defSearch, newScope);

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
                switchCase->elseCase = NULL;

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

                int atLeastOneCase = 0;
                while (1) {

                    if (Utils::skipWhiteSpacesAndComments(str, loc)) {
                        if (atLeastOneCase) return Err::OK;
                        return Err::UNEXPECTED_END_OF_FILE;
                    }

                    char* const word = str + loc->idx;
                    const int wordLen = Utils::findVarEnd(word);
                    const KeyWord* const keyWord = findKeyWord(keyWords, KEY_WORDS_COUNT, word, wordLen);

                    if (!keyWord || (keyWord->type != KW_SWITCH_CASE_CASE && keyWord->type != KW_ELSE)) {
                        break;
                        //Logger::log(Logger::ERROR, "Keyword 'case' or 'else' expected!", loc, 1);
                        //return Err::UNEXPECTED_SYMBOL;
                    }

                    loc->idx += wordLen;

                    // whatever
                    Variable* cmpExp;
                    if (keyWord->type != KW_ELSE) {
                        cmpExp = new Variable;
                        cmpExp->scope = scope;
                        err = parseExpression(cmpExp, str, loc, CHAR_CAT(STATEMENT_BEGIN, SCOPE_BEGIN));
                        if (err < 0) return err;
                    } else {
                        if (Utils::skipWhiteSpacesAndComments(str, loc)) return Err::UNEXPECTED_END_OF_FILE;
                        loc->idx++;
                    }

                    ch = str[loc->idx - 1];
                    Scope* sc = new Scope;
                    sc->fcn = currentFunction;
                    sc->scope = scope;
                    setParentIdx(sc);

                    atLeastOneCase = 1;

                    //pushDefLike(scope->defSearch, sc);

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

                // TODO : something about empty expressions, maybe make them null or something...

                ForLoop *loop = new ForLoop();

                int err = 0;
                int linesSkipped = 0;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                Scope* outerScope = new Scope();
                outerScope->fcn = currentFunction;
                outerScope->scope = scope;
                setParentIdx(outerScope);

                //pushDefLike(scope->defSearch, outerScope);

                Variable* initEx = new Variable(outerScope);

                // can be either variable initialization or expression
                err = selectDataTypeKeyWord(str, &(loc->idx));
                if (err < 0) {
                    err = parseExpression(initEx, str, loc, STATEMENT_END, 0, 1);
                    if (err < 0) return err;
                } else {
                    err = parseKeyWord((KeyWordType)err, outerScope, str, loc);
                    if (err < 0) return err;
                }

                Scope* bodyScope = new Scope();
                bodyScope->fcn = currentFunction;
                bodyScope->scope = outerScope;
                setParentIdx(bodyScope);

                Variable* conditionEx = new Variable(outerScope);
                conditionEx->loc = getLocationStamp(loc);

                err = parseExpression(conditionEx, str, loc, STATEMENT_END, 0, 1);
                if (err < 0) return err;

                // can be assignment
                Variable *actionEx = new Variable(outerScope);
                
                VariableAssignment* ass;
                int assignment = 0;
                {
                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                    
                    const int startIdx = loc->idx;
                    const int startLn = loc->line;

                    char* const name = str + loc->idx;
                    const int len = Utils::findVarEnd(name);

                    loc->idx += len;

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                    assignment = str[loc->idx] == EQUAL_SYMBOL;
                    if (assignment) {

                        ass = new VariableAssignment();
                        
                        ass->lvar = new Variable(bodyScope);
                        ass->lvar->loc = getLocationStamp(loc);
                        ass->lvar->loc->idx = startIdx;
                        ass->lvar->loc->line = startLn;
                        ass->lvar->parentIdx = 0;
                        ass->lvar->name = name;
                        ass->lvar->nameLen = len;

                        ASSIGN_ID(ass->lvar);

                        ass->rvar = new Variable(bodyScope);

                        loc->idx++;
                        err = parseExpression(ass->rvar, str, loc, SCOPE_BEGIN, 0, 0);
                        if (err < 0) return err;

                    } else {
                        
                        loc->idx = startIdx;
                        loc->line = startLn;

                        err = parseExpression(actionEx, str, loc, SCOPE_BEGIN, 0, 1);
                        if (err < 0) return err;

                    }

                }

                
                //pushDefLike(bodyScope->scope->defSearch, bodyScope);

                currentLoop = loop;
                
                err = parseScope(bodyScope, str, loc);
                if (err < 0) return err;

                if (assignment) {
                    bodyScope->children.push_back(ass);
                    bodyScope->variableAssignments.push_back(ass);
                    bodyScope->variables.push_back(ass->lvar);
                }
                
                currentLoop = NULL;

                loop->scope = scope;
                loop->bodyScope = bodyScope;
                loop->initEx = initEx;
                loop->conditionEx = conditionEx;
                loop->actionEx = actionEx;

                outerScope->children.push_back(loop);
                scope->children.push_back(outerScope);
                // pushDefLike(scope->defSearch, outerScope);
                if (conditionEx->expression) {
                    loop->conditionEx = conditionEx;
                    SyntaxNode::branchExpressions.push_back(conditionEx);
                } else {
                    loop->conditionEx = NULL;
                    freeLocationStamp(conditionEx->loc);
                    delete conditionEx;
                }

                break;
            
            }

            case KW_WHILE: {

                WhileLoop* loop = new WhileLoop();

                int err = 0;
                int linesSkipped = 0;

                Scope* newScope = new Scope();
                newScope->fcn = currentFunction;
                newScope->scope = scope;
                setParentIdx(newScope);

                //pushDefLike(scope->defSearch, newScope);

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
                gt->label = NULL;

                loc->idx += gt->nameLen;

                scope->children.push_back(gt);
                scope->gotos.push_back(gt);
                SyntaxNode::gotos.push_back(gt);

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

                enumerator->id = defId;
                defId++;

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
                    newVarDef->var->cvalue.hasValue = 1;
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
                // Utils::push(scope->defSearch, enumerator);
                scope->enums.push_back(enumerator);

                break;
            
            }

            case KW_TYPE_DEF: {

                int err = 0;
                // int linesSkipped = 0;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                Location* defLoc = getLocationStamp(loc);
                char* name = str + loc->idx;
                int nameLen = Utils::findVarEnd(name);

                const KeyWord* keywordType = findKeyWord(typedefKeyWords, TYPEDEF_KEY_WORDS_COUNT, name, nameLen);
                int keyword = keywordType ? keywordType->type : TKW_STRUCT;
                if (keyword == TKW_UNION || keywordType) {
                    
                    loc->idx += nameLen;
                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                    
                    name = str + loc->idx;
                    nameLen = Utils::findVarEnd(name);
                
                } else {
                    
                    keyword = TKW_STRUCT;
                
                }

                loc->idx += nameLen;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                if (str[loc->idx] != SCOPE_BEGIN) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
                    return Err::UNEXPECTED_SYMBOL;
                }

                TypeDefinition* newTypeDefinition;
                if (keyword == TKW_STRUCT) {
                    newTypeDefinition = new TypeDefinition();
                } else {
                    newTypeDefinition = new Union();
                }

                newTypeDefinition->scope = scope;
                newTypeDefinition->name = name;
                newTypeDefinition->nameLen = nameLen;
                newTypeDefinition->parentIdx = SyntaxNode::customDataTypes.size();
                newTypeDefinition->loc = defLoc;
                
                newTypeDefinition->id = defId;
                defId++;

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
                    err = parseVariableDefinition(scope, str, loc, param, toDoubleChar('}', ';'), &varDef);
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

                ret->var = NULL;
                ret->err = NULL;

                ret->idx = currentFunction->returns.size();
                currentFunction->returns.push_back(ret);

                scope->children.push_back(ret);
                SyntaxNode::returnStatements.push_back(ret);

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                if (str[loc->idx] == STATEMENT_END) {
                    loc->idx++;
                    break;
                }

                // var case
                if (str[loc->idx] == SKIP_VARIABLE) {
                    loc->idx++;
                } else {
                    Variable *newVar = new Variable(scope, DT_INT_64, loc);

                    const int err = parseExpression(newVar, str, loc, CHAR_CAT(',', STATEMENT_END));
                    if (err < 0) return err;

                    ret->var = newVar;
                    loc->idx--;
                }

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                if (str[loc->idx] == STATEMENT_END) {
                    loc->idx++;
                    break;
                }
                
                if (str[loc->idx] == LIST_SEPARATOR) {
                    loc->idx++;
                } else {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
                    return Err::UNEXPECTED_SYMBOL;
                }

                // error case
                if (str[loc->idx] == SKIP_VARIABLE) {
                    loc->idx++;
                } else {

                    Variable* newVar = new Variable(scope, DT_INT_64, loc);

                    const int err = parseExpression(newVar, str, loc, CHAR_CAT(',', STATEMENT_END));
                    if (err < 0) return err;

                    ret->err = newVar;
                    loc->idx--;

                    /*
                    ErrorSet* err = new ErrorSet();

                    err->name = str + loc->idx;
                    err->nameLen = Utils::findVarEnd(err->name);
                    */
                    // loc->idx += err->nameLen;
                }

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                if (str[loc->idx] == STATEMENT_END) {
                    loc->idx++;
                    break;
                }
                
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
                outerScope->fcn = currentFunction;
                outerScope->scope = scope;
                setParentIdx(outerScope);

                //pushDefLike(scope->defSearch, outerScope);

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
                    loop->idx = new Variable(outerScope);

                    const int err = parseExpression(loop->idx, str, loc, SCOPE_BEGIN);
                    if (err < 0) return err;
                
                }

                loop->bodyScope = new Scope();
                loop->bodyScope->fcn = currentFunction;
                loop->bodyScope->scope = loop->scope;
                setParentIdx(loop->bodyScope);

                //pushDefLike(loop->scope->defSearch, loop->bodyScope);
                
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

                Namespace* nsc = new Namespace();
                nsc->type = NT_NAMESPACE;
                nsc->scope = scope;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                
                nsc->loc = getLocationStamp(loc);
                nsc->name = str + loc->idx;
                nsc->nameLen = Utils::findVarEnd(nsc->name);
                setParentIdx(nsc);
                
                //pushDefLike(scope->defSearch, nsc);

                loc->idx += nsc->nameLen;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                if (str[loc->idx] != SCOPE_BEGIN) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 1);
                    return Err::UNEXPECTED_SYMBOL;
                }

                loc->idx++;

                parseScope(nsc, str, loc, SC_COMMON);
                
                scope->children.push_back(nsc);
                scope->namespaces.push_back(nsc);

                break;

            }

            case KW_ALLOC : {

                // TODO : error?
                break;

            }

            case KW_FREE : {

                Function* const fcn = internalFunctions + (IF_FREE - 1);

                FunctionCall* fcnCall = new FunctionCall();
                fcnCall->fptr = NULL;
                fcnCall->fcn = fcn;
                fcnCall->name = fcn->name;
                fcnCall->nameLen = fcn->nameLen;
                fcnCall->outArg = new Variable();
                fcnCall->outArg->cvalue.dtypeEnum = DT_VOID;
                
                Variable* inVar = new Variable();
                const int err = parseExpression(inVar, str, loc, STATEMENT_END);
                if (err < 0) return err;

                fcnCall->inArgsCnt = 1;
                fcnCall->inArgs.push_back(inVar);

                Variable* wrapper = new Variable(scope);
                wrapper->loc = getLocationStamp(loc);
                wrapper->expression = fcnCall;

                Statement* st = new Statement();
                st->scope = scope;
                st->op = wrapper;

                SyntaxNode::fcnCalls.push_back(wrapper);
                scope->children.push_back(st);

                break;

            }

            case KW_IMPORT : {

                const int err = parseImport(scope, str, loc);
                if (err < 0) return err;

                break;
            
            }

            case KW_ERROR : {

                ErrorSet* errorSet = new ErrorSet();

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                char* name = str + loc->idx;
                int nameLen = Utils::findVarEnd(name);

                if (nameLen <= 0) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::MISSING_VARIABLE_NAME), loc, 1);
                    return Err::MISSING_VARIABLE_NAME;
                }

                loc->idx += nameLen;

                if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                if (str[loc->idx] != SCOPE_BEGIN) {
                    // maybe declaration + initialization

                    VariableDefinition* def = createEmptyVariableDefinition();
                    def->var->name = name;
                    def->var->nameLen = nameLen;
                    def->var->cvalue.dtypeEnum = DT_ERROR;
                    def->var->loc = getLocationStamp(loc);
                    def->scope = scope;
                    setParentIdx(def->var);

                    auto res = def->scope->defSearch.insert({std::string_view(name, nameLen), def->var});
                    if (!res.second) {
                        Logger::log(Logger::ERROR, ERR_STR(Err::SYMBOL_ALREADY_DEFINED), loc, nameLen);
                        return Err::SYMBOL_ALREADY_DEFINED;
                    }
                    //pushDefLike(scope->defSearch, def->var);
                    
                    ASSIGN_ID(def->var);
                        
                    if (str[loc->idx] == STATEMENT_END) {
                        
                        loc->idx++;

                    } else if (str[loc->idx] == '=') {
                        
                        loc->idx++;
                        const int err = parseExpression(def->var, str, loc, STATEMENT_END);
                        if (err < 0) return err;
                    
                    } else {

                        Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 1);
                        return Err::UNEXPECTED_SYMBOL;
                    
                    }

                    SyntaxNode::variableDefinitions.push_back(def);
                    scope->defs.push_back(def->var);
                    scope->children.push_back(def);

                    break;

                }

                errorSet->scope = scope;
                errorSet->name = name;
                errorSet->nameLen = nameLen;
                errorSet->value = errId;
                errId++;

                ASSIGN_ID(errorSet);

                loc->idx++;
                while (1) {

                    int err;
                    char ch;

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                    if (str[loc->idx] == SCOPE_END) {
                        loc->idx++;
                        break;
                    }

                    Variable* var = new Variable();
                    var->scope = scope;
                    var->name = str + loc->idx;
                    var->nameLen = Utils::findVarEnd(var->name);
                    var->loc = getLocationStamp(loc);
                    var->cvalue.hasValue = 0;
                    var->cvalue.dtypeEnum = DT_ERROR;
                    var->cvalue.u64 = errId;
                    errId++;

                    // ASSIGN_ID(var);

                    loc->idx += var->nameLen;

                    errorSet->vars.push_back(var);

                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                    ch = str[loc->idx];
                    if (ch == STATEMENT_END) {
                        loc->idx++;
                        continue;
                    } else if (ch == SCOPE_END) {
                        loc->idx++;
                        break;
                    } else {
                        Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
                        return Err::UNEXPECTED_SYMBOL;
                    }

                }

                SyntaxNode::customErrors.push_back(errorSet);
                scope->customErrors.push_back(errorSet);
                scope->children.push_back(errorSet);
                
                break;

            }

            case KW_UNION : {

                Logger::log(Logger::ERROR, "TODO error : Use 'def union' to define union!", loc, 1);
                return Err::UNEXPECTED_SYMBOL;
                break;
                
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

        }

        return Err::OK;

    }

    int parseImport(Scope* sc, char* const str, Location* const loc) {

        // import x as namespace n;
        //      wraps content of file x into namespace n

        // import x as scope;
        // import x as function foo;

        // import Foo from file;

        ImportStatement* import = new ImportStatement();
        // import->root = fileRootScope;
        import->scope = sc;
        
        ImportNode* importNode = new ImportNode;
        importNode->import = import;
        importNode->parent = importCurrent;

        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
        import->loc = getLocationStamp(loc);
        
        char* const pnameA = str + loc->idx;
        const int pnameALen = Utils::findWordEnd(pnameA);

        loc->idx += pnameALen;
        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
        
        if (str[loc->idx] == STATEMENT_END) {
            import->fname = { pnameA, pnameALen };
            import->keyWord = (KeyWordType) -1;
            importCurrent->children.push_back(importNode);
            // SyntaxNode::imports.push_back(import);
            return Err::OK;
        }

        char* const word = str + loc->idx;
        const int wordLen = Utils::findVarEnd(word);

        // 0 for as, 1 for from
        int wordType = 0;

        if (strncmp("as", word, wordLen > 2 ? wordLen : 2) == 0) {
            
            loc->idx += 2;
            wordType = 0;

            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

            const int keyWordIdx = selectKeyWord(keyWords, KEY_WORDS_COUNT, str, &(loc->idx));
            const int keyWordType = keyWords[keyWordIdx].type;
            if (keyWordType != KW_NAMESPACE && keyWordType != KW_SCOPE && keyWordType != KW_FUNCTION) {
                Logger::log(Logger::ERROR, "Unsupported value is used! Use one of the following : 'namespace', 'fcn', 'scope'\n", loc, Utils::findVarEnd(str + loc->idx));
                return Err::UNEXPECTED_SYMBOL;
            }
    
            import->keyWord = (KeyWordType) keyWords[keyWordIdx].type;

        } else if (strncmp("from", word, wordLen > 4 ? wordLen : 4) == 0) {
            
            loc->idx += 4;
            wordType = 1;

        } else {

            Logger::log(Logger::ERROR, "Unexpected word, 'as' or 'from' expected!", loc, 1);
            return Err::UNEXPECTED_SYMBOL;
        
        }

        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
            
        char* const pnameB = str + loc->idx;;
        const int pnameBLen = Utils::findVarEnd(pnameB);

        loc->idx += pnameBLen;
        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
        
        if (str[loc->idx] != STATEMENT_END) {
            Logger::log(Logger::ERROR, "Unexpexted symbol, %c expected", loc, 1, STATEMENT_END);
            return Err::UNEXPECTED_SYMBOL;
        }

        if (wordType == 0) {
            import->fname = { pnameA, pnameALen };
            import->param = { pnameB, pnameBLen };
        } else if (wordType == 1) {
            import->fname = { pnameB, pnameBLen };
            import->param = { pnameA, pnameALen };
            import->keyWord = (KeyWordType) -1;
        }

        importCurrent->children.push_back(importNode);

        loc->idx++;

        return Err::OK;
                
    }

    // expects that str starts at word
    // TODO: better name, as it skips also variable name
    int skipScopeNames(char* const str, Location* const loc) {

        while (1) {

            char* const word = str + loc->idx;
            const int wordLen = Utils::findVarEnd(word);

            loc->idx += wordLen;
            
            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

            if (CMP_TWO_CHARS(str + loc->idx, SCOPE_RESOLUTION_SYMBOL)) {
                loc->idx += 2;
            } else {
                loc->idx = (word - str) + wordLen;
                return Err::OK;
            }

            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

        }

    }

    // already at word start
    int parseScopeNames(INamedVar* var, char* const str, Location* const loc) {

        Location startLocation = *loc;
        
        char* word = str + loc->idx;
        int wordLen = Utils::findVarEnd(word);

        loc->idx += wordLen;

        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

        while (CMP_TWO_CHARS(str + loc->idx, SCOPE_RESOLUTION_SYMBOL)) {

            INamedLoc* scName = new INamedLoc(word, wordLen, getLocationStamp(&startLocation));
            var->scopeNames.push_back(scName);

            loc->idx += 2;
            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

            word = str + loc->idx;
            wordLen = Utils::findVarEnd(word);

            startLocation = *loc;
            loc->idx += wordLen;
            
        }

        var->name = word;
        var->nameLen = wordLen;

        return Err::OK;

    }

    // G as gneral
    enum {
        G_NONE,
        G_VARIABLE,
        G_STRING_LITERAL,
        G_NUMBER_LITERAL,
        G_OPERATOR,
        G_CATCH,
        G_SUB_EXPRESSION,
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
    int parseExpression(Variable* operand, char* const str, Location* const loc, const uint16_t endChar, const int useKeyWordAsEnd, const int emptyExpressionAllowed, const int defIdx) {

        const int lastDefIdx = (defIdx < 0) ? operand->scope->children.size() : defIdx;

        int lastOperatorRank = 0;
        UnaryExpression* lastUnaryExpression = NULL;
        BinaryExpression* lastBinaryExpression = NULL;
        int lastType = G_NONE;
        int lastOperator = -1;
        int lastOperandType = DT_UNDEFINED;
        Variable* lastVariable = NULL;
        FunctionCall* lastFunctionCall = NULL;

        const int exclusiveEnd = (endChar >> 8) == 1;

        const char endCharA = (char) endChar;
        const char endCharB = (endChar > 255) ? endChar >> 8 : endCharA;

        while (1) {

            // TODO : do better
            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

            const char ch = str[loc->idx];

            if ( ( ch == endCharA && ((!exclusiveEnd || (exclusiveEnd && str[loc->idx + 1] != endCharA))) ) || ch == endCharB) {
                // end of expression

                if (!emptyExpressionAllowed && lastType == G_NONE) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_END_OF_EXPRESSION), loc, 1);
                    loc->idx++;
                    return Err::UNEXPECTED_END_OF_EXPRESSION;
                }

                if (lastType == G_OPERATOR) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_END_OF_EXPRESSION), loc);
                    return Err::UNEXPECTED_END_OF_EXPRESSION;
                }

                loc->idx++;

                return Err::OK;
            
            } else if (lastType == G_CATCH) {
                
                Logger::log(Logger::ERROR, "TODO error : catch expression cannot be used within other expression!", loc, strlen(KWS_CATCH));
                return Err::UNEXPECTED_SYMBOL;
            
            } else if (ch == '(') {
                // subexpression start

                Variable* newOperand = new Variable();
                newOperand->cvalue.dtypeEnum = DT_UNDEFINED; // TODO : exchange for UNDEFINED or whatever!!!!!
                newOperand->scope = operand->scope;
                newOperand->unrollExpression = 1;
                newOperand->cvalue.ptr = NULL;

                loc->idx++;
                const int err = parseExpression(newOperand, str, loc, ')', 0, 0, lastDefIdx);
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

                        if (keyWord->type == KW_CATCH) {
                            
                            if (lastUnaryExpression || lastBinaryExpression) {
                                Logger::log(Logger::ERROR, "TODO error : catch expression cannot be used within other expression!", loc, strlen(KWS_CATCH));
                                return Err::UNEXPECTED_SYMBOL;
                            }

                            if (!lastFunctionCall) {
                                Logger::log(Logger::ERROR, "TODO error : 'catch' keyword can only be used after function call!", loc, strlen(KWS_CATCH));
                                return Err::UNEXPECTED_SYMBOL;
                            }

                            loc->idx += strlen(KWS_CATCH);
                            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                            Catch* cex = new Catch();
                            cex->call = (FunctionCall*) lastFunctionCall;
                            
                            char* name = str + loc->idx;
                            int nameLen = Utils::findVarEnd(name);
                            Location* varLoc = getLocationStamp(loc);

                            loc->idx += nameLen;

                            const int retLen = strlen(KWS_RETURN);
                            const int returnErr = strncmp(KWS_RETURN, name, nameLen > retLen ? nameLen : retLen) == 0;
                            
                            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                            if (str[loc->idx] == SCOPE_BEGIN || returnErr) {
                                // local var def

                                Scope* newScope = new Scope();
                                newScope->scope = operand->scope;
                                setParentIdx(newScope);
                                //pushDefLike(operand->scope->defSearch, newScope);

                                VariableDefinition* errDef = createEmptyVariableDefinition();
                                errDef->scope = newScope;
                                errDef->var->name = name;
                                errDef->var->nameLen = nameLen;
                                errDef->var->scope = newScope;
                                errDef->var->cvalue.dtypeEnum = DT_ERROR;
                                errDef->var->loc = varLoc;
                                errDef->var->parentIdx = -1;

                                ASSIGN_ID(errDef->var);

                                newScope->defs.push_back(errDef->var);
                                SyntaxNode::variableDefinitions.push_back(errDef);

                                auto res = newScope->defSearch.insert({std::string_view(name, nameLen), errDef->var});
                                if (!res.second) {
                                    Logger::log(Logger::ERROR, ERR_STR(Err::SYMBOL_ALREADY_DEFINED), varLoc, nameLen);
                                    return Err::SYMBOL_ALREADY_DEFINED;
                                }
                                //pushDefLike(newScope->defSearch, errDef->var);

                                cex->err = errDef->var;
                                cex->scope = newScope;

                                operand->expression = cex;

                                if (returnErr) {
                                    // basicly just emulate following
                                    // .. .. catch err { return _, err; }
                                    
                                    ReturnStatement* ret = new ReturnStatement();
                                    ret->err = errDef->var;
                                    ret->var = NULL;
                                    ret->scope = newScope;
                                    ret->fcn = currentFunction;

                                    newScope->children.push_back(ret);
                                    SyntaxNode::returnStatements.push_back(ret);

                                    cex->err = errDef->var;
                                    cex->scope = newScope;

                                    operand->expression = cex;

                                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
                                    if (str[loc->idx] != STATEMENT_END) {
                                        Logger::log(Logger::ERROR, "'catch return' expression has to be terminated with ';'!", startLocation);
                                        return Err::UNEXPECTED_SYMBOL;
                                    }

                                    loc->idx++;

                                    return Err::OK;

                                }

                                loc->idx++;
                                parseScope(newScope, str, loc, SC_COMMON);
                            
                            } else {
                                
                                Variable* var = new Variable(operand->scope, DT_ERROR, loc);
                                var->name = name;
                                var->nameLen = nameLen;
                                var->loc = varLoc;
                                var->parentIdx = lastDefIdx;

                                SyntaxNode::variables.push_back(var);

                                cex->err = var;
                                cex->scope = NULL;

                            }

                            operand->expression = cex;

                            // lastType = G_CATCH;
                            // continue;

                            return Err::OK;
                            
                        }

                        Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_VARIABLE_NAME), loc, wordLen, "Variable name is matching key word name!");
                        return Err::INVALID_VARIABLE_NAME;
                    }

                    // decide var or fcn

                    loc->idx += wordLen;
                    if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                    std::vector<INamedLoc*> scopeNames;
                    while (CMP_TWO_CHARS(str + loc->idx, SCOPE_RESOLUTION_SYMBOL)) {

                        INamedLoc* scName = new INamedLoc(word, wordLen, startLocation);
                        scopeNames.push_back(scName);

                        loc->idx += 2;
                        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                        word = str + loc->idx;
                        wordLen = Utils::findVarEnd(word);

                        startLocation = getLocationStamp(loc);
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
                            var->parentIdx = lastDefIdx;
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
                        fcnCall->fptr = NULL;
                        fcnCall->fcn = NULL;
                        fcnCall->name = word;
                        fcnCall->nameLen = wordLen;
                        fcnCall->scopeNames = scopeNames;

                        lastFunctionCall = fcnCall;

                        // parse input
                        loc->idx += 1;
                        while (1) {

                            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

                            // LOOK AT : process this out of the loop for the first time
                            if (str[loc->idx] == FUNCTION_END) break;

                            // LOOK AT : is Variable needed? maybe use just Variable
                            Variable* newOperand = new Variable(operand->scope, DT_UNDEFINED, loc);
                            newOperand->flags = 0;

                            int err = parseExpression(newOperand, str, loc, CHAR_CAT(',', FUNCTION_END), 0, 0, lastDefIdx);
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

                        fcnCall->inArgsCnt = fcnCall->inArgs.size();

                        Variable* newOperand = new Variable;
                        newOperand->cvalue.dtypeEnum = DT_UNDEFINED;
                        newOperand->expression = fcnCall;
                        newOperand->scope = operand->scope;
                        newOperand->unrollExpression = 1;
                        newOperand->cvalue.ptr = NULL;
                        newOperand->loc = startLocation;
                        newOperand->parentIdx = lastDefIdx;

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

                    const int err = parseExpression(idxOperand, str, loc, CHAR_CAT(ARRAY_END, ':'), 0, 1, lastDefIdx);
                    if (err < 0) return err;

                    Variable* operandB = idxOperand;

                    if (str[loc->idx - 1] == ':') {
                        // slice

                        Variable* idxOperand2 = new Variable();
                        idxOperand2->cvalue.dtypeEnum = DT_UNDEFINED;
                        idxOperand2->scope = operand->scope;
                        idxOperand2->unrollExpression = 1;
                        idxOperand2->cvalue.ptr = NULL;

                        const int err = parseExpression(idxOperand2, str, loc, ARRAY_END, 0, 1, lastDefIdx);
                        if (err < 0 && err != Err::UNEXPECTED_END_OF_EXPRESSION) return err;

                        Slice* slice = new Slice;
                        slice->bidx = idxOperand;
                        slice->eidx = idxOperand2;

                        Variable* newOperand = new Variable();
                        newOperand->cvalue.dtypeEnum = DT_UNDEFINED;
                        newOperand->scope = operand->scope;
                        newOperand->unrollExpression = 1;
                        newOperand->expression = slice;
                        
                        newOperand->id = arrId;
                        arrId++;

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
                    // bEx->oper = operators + OP_SUBSCRIPT;

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
                        if (lastOperatorRank >= operators[OP_SUBSCRIPT].rank) {
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
                        // uEx->oper = operators + opType;

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
                        // uEx->oper = operators + opType;

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
                        // uEx->oper = operators + opType;

                        operand->expression = uEx;

                        lastUnaryExpression = uEx;
                        lastType = G_VARIABLE;
                    
                    } else {

                        opType = findBinaryOperator(word);
                        if (opType <= OP_NONE) {
                            Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_OPERATOR), loc, wordLen, wordLen, &word);
                            return Err::INVALID_OPERATOR;
                        }

                        Operator op = operators[opType];

                        lastUnaryExpression = NULL;

                        BinaryExpression* bEx = new BinaryExpression;

                        Variable* newVar = new Variable(loc);
                        newVar->scope = operand->scope;

                        // LOOK_AT: maybe change design to allways somehow have root as expression, so we dont have to check or something
                        const int operatorRank = op.rank;
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
                        // bEx->oper = op;

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

    VariableDefinition* createEmptyVariableDefinition() {

        VariableDefinition* def = new VariableDefinition();
        def->flags = 0;
        def->lastPtr = NULL;
        def->dtype = NULL;

        def->var = new Variable();
        def->var->cvalue.any = NULL;
        def->var->cvalue.dtypeEnum = DT_VOID;
        def->var->cvalue.hasValue = 0;

        def->var->def = def;

        return def;

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

                // duex->oper = uex->oper;
                duex->operType = uex->operType;
                
                return copyExpression((Variable*) uex->operand, (Variable**) &(duex->operand), pidx, nextIdx);
                
                break;

            }

            case EXT_BINARY: {

                BinaryExpression* bex = (BinaryExpression*) ex;
                BinaryExpression* dbex = new BinaryExpression();

                *dest = new Variable();
                (*dest)->expression = dbex;
                
                // dbex->oper = bex->oper;
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

                dfex->fptr = fex->fptr;
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
                if (str[loc->idx] == 'x') {

                    int idx;
                    ch = parseHexInt(str + loc->idx + 1, &idx);
                    if (idx == 0) {
                        Logger::log(Logger::ERROR, "At least one hex digit requaried!", loc, 1);
                        return Err::UNEXPECTED_SYMBOL;
                    }
                    loc->idx += idx;
                
                } else {
                    
                    ch = parseEscapeChar(str, &(loc->idx));
                    if (ch == -1) {
                        parseHexInt(str, &(loc->idx));
                        Logger::log(Logger::ERROR, ERR_STR(Err::UNSUPPORTED_ESCAPE_SEQUENCE), loc, 1);
                        return Err::UNSUPPORTED_ESCAPE_SEQUENCE;
                    }
                
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


    uint64_t parseHexInt(char* const str, int* idx) {

        uint64_t num = 0;
        for (int i = 0;; i++) {

            const char ch = str[i];

            if (ch >= '0' && ch <= '9') num = num * 16 + (ch - '0');
            else if (ch >= 'A' && ch <= 'F') num = num * 16 + (10 + ch - 'A');
            else if (ch >= 'a' && ch <= 'f') num = num * 16 + (10 + ch - 'a');
            else {
                *idx = i;
                return num;
            }

        }

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
                else if (ch == '_') {
                    if (str[i + 1] == '_') return DT_VOID;
                } else {
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
                else if (ch == '_') {
                    if (str[i + 1] == '_') return DT_VOID;
                } else {
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
                            
                        }   else if (ch == '_') {
                            
                            if (str[i + 1] == '_') return DT_VOID;
                        
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
                    
                } else if (ch == '_') {
                    
                    if (str[i + 1] == '_') return DT_VOID;
                    
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

    void freeLocationStamp(Location* loc) {
        free(loc);
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

    inline void offsetParentIdx(std::vector<SyntaxNode*> vec, const int offset) {
        for (int i = 0; i < vec.size(); i++) {
            vec[i]->parentIdx += offset;
        }
    }

    Namespace* getCopy(Namespace* nspace) {

        Namespace* newNspace = new Namespace();
        memcpy(newNspace, nspace, sizeof(Namespace));

        return newNspace;

    }

    Function* getCopy(Function* fcn) {

        Function* newFcn = new Function();
        memcpy(newFcn, fcn, sizeof(Function));

        return newFcn;

    }

    void copy(Scope* scA, Scope* scB) {
        
        Scope* parent = scB->scope;

        scA->type = scB->type;
        scA->scope = scB->scope;
        scA->loc = scB->loc;
        scA->parentIdx = scB->parentIdx;
        scA->snFlags = scB->snFlags;

        scA->children = scB->children;
        
        scA->fcn = scB->fcn;

        scA->defSearch = scB->defSearch;
        scA->defs = scB->defs;
        scA->fcns = scB->fcns;
        scA->unions = scB->unions;
        scA->labels = scB->labels;
        scA->customErrors = scB->customErrors;
        scA->customDataTypes = scB->customDataTypes;
        scA->enums = scB->enums;
        scA->namespaces = scB->namespaces;
        scA->gotos = scB->gotos;

    }

    #define appendVectors(a, b) (a).insert(std::begin(a), std::begin(b), std::end(b))
        
    // appends scB to scA
    void appendScope(Scope* scA, Scope* scB) {
        // #define appendVectors(a, b) (a).insert(std::end(a), std::begin(b), std::end(b))
        appendVectors(scA->children, scB->children);
        appendVectors(scA->codeBlocks, scB->codeBlocks);
        appendVectors(scA->customDataTypes, scB->customDataTypes);
        appendVectors(scA->enums, scB->enums);
        appendVectors(scA->fcns, scB->fcns);
        appendVectors(scA->foreignFunctions, scB->foreignFunctions);
        appendVectors(scA->langDefs, scB->langDefs);
        appendVectors(scA->namespaces, scB->namespaces);

        //appendVectors(scA->defSearch, scB->defSearch);
        //offsetParentIdx(scB->defSearch, scA->defSearch.size());
    }

    // appends scB  infront of scA
    void appendPrefixScope(Scope* scA, Scope* scB) {
        // #define appendVectors(a, b) (a).insert(std::begin(a), std::begin(b), std::end(b))
        appendVectors(scA->children, scB->children);
        appendVectors(scA->codeBlocks, scB->codeBlocks);
        appendVectors(scA->customDataTypes, scB->customDataTypes);
        appendVectors(scA->enums, scB->enums);
        //appendVectors(scA->vars, scB->vars);
        appendVectors(scA->defs, scB->defs);
        appendVectors(scA->fcns, scB->fcns);
        appendVectors(scA->foreignFunctions, scB->foreignFunctions);
        appendVectors(scA->langDefs, scB->langDefs);
        appendVectors(scA->namespaces, scB->namespaces);

        //appendVectors(scB->defSearch, scA->defSearch);
        //offsetParentIdx(scA->defSearch, scB->defSearch.size());
    }

}
