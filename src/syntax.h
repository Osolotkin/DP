// TODO : maybe exchange FILE for something generic and custom...
// NOTE : for now std::vectors/arrays are main collections as 
//        it's easy to work with and debug them. Later they will
//        be exchanged for more appropriate collections.

#pragma once

#include <vector>
#include <string>

#include "globals.h"


struct Translator;

enum InternalFunction : uint32_t;

enum KeyWordType : int;
struct KeyWord;

struct SyntaxNode;
struct Scope;
struct Namespace;
struct VariableDefinition;
struct VariableAssignment;
struct TypeDefinition;
struct TypeInitialization;
struct StringInitialization;
struct ArrayInitialization;
struct Enumerator;
struct Variable;
struct Function;
struct Branch;
struct SwitchCase;
struct WhileLoop;
struct ForLoop;
struct Loop;
struct ReturnStatement;
struct ContinueStatement;
struct BreakStatement;
struct GotoStatement;
struct Label;
struct WrapperExpression;
struct ExpressionWrapper;
struct Expression;
struct ConstExpression;
struct OperatorExpression;
struct UnaryExpression;
struct BinaryExpression;
struct TernaryExpression;
struct Statement;
struct Operator;
struct Operand;
struct FunctionCall;
struct UnaryOperator;
struct BinaryOperator;
struct TernaryOperator;
struct ImportStatement;

struct ScopeName;
enum ScopeType;

struct IForeignCode;
struct CodeBlock;
struct ForeignFunction;
struct LangDef;

enum DataTypeEnum : int;
struct DataType;
struct TypeDefinition;
struct Pointer;
struct Array;
struct Slice;

enum ExpressionType;



// for now here because of cross reference stuff 
struct Translator {

    FILE* mainFile;

    void (*init)                        (char* const dirName);
    void (*printScope)                  (FILE* file, int level, Scope* const node, Variable* lvalue);
    void (*printVariableDefinition)     (FILE* file, int level, VariableDefinition* const node, Variable* lvalue);
    void (*printVariableAssignment)     (FILE* file, int level, VariableAssignment* const node, Variable* lvalue);
    void (*printTypeDefinition)         (FILE* file, int level, TypeDefinition* const node, Variable* lvalue);
    void (*printTypeInitialization)     (FILE* file, int level, TypeInitialization* const node, Variable* lvalue);
    void (*printStringInitialization)   (FILE* file, int level, StringInitialization* const node, Variable* lvalue);
    void (*printArrayInitialization)    (FILE* file, int level, ArrayInitialization* const node, Variable* lvalue);
    void (*printEnumerator)             (FILE* file, int level, Enumerator* const node, Variable* lvalue);
    void (*printVariable)               (FILE* file, int level, Variable* const node, Variable* lvalue);
    void (*printFunction)               (FILE* file, int level, Function* const node, Variable* lvalue);
    void (*printBranch)                 (FILE* file, int level, Branch* const node, Variable* lvalue);
    void (*printSwitchCase)             (FILE* file, int level, SwitchCase* const node, Variable* lvalue);
    void (*printWhileLoop)              (FILE* file, int level, WhileLoop* const node, Variable* lvalue);
    void (*printForLoop)                (FILE* file, int level, ForLoop* const node, Variable* lvalue);
    void (*printLoop)                   (FILE* file, int level, Loop* const node, Variable* lvalue);
    void (*printReturnStatement)        (FILE* file, int level, ReturnStatement* const node, Variable* lvalue);
    void (*printContinueStatement)      (FILE* file, int level, ContinueStatement* const node, Variable* lvalue);
    void (*printBreakStatement)         (FILE* file, int level, BreakStatement* const node, Variable* lvalue);
    void (*printGotoStatement)          (FILE* file, int level, GotoStatement* const node, Variable* lvalue);
    void (*printLabel)                  (FILE* file, int level, Label* const node, Variable* lvalue);
    void (*printNamespace)              (FILE* file, int level, Namespace* const node, Variable* lvalue);
    void (*printExpression)             (FILE* file, int level, Expression* const node, Variable* lvalue);
    void (*printWrapperExpression)      (FILE* file, int level, WrapperExpression* const node, Variable* lvalue);
    void (*printExpressionWrapper)      (FILE* file, int level, ExpressionWrapper* const node, Variable* lvalue);
    void (*printConstExpression)        (FILE* file, int level, ConstExpression* const node, Variable* lvalue);
    void (*printOperatorExpression)     (FILE* file, int level, OperatorExpression* const node, Variable* lvalue);
    void (*printUnaryExpression)        (FILE* file, int level, UnaryExpression* const node, Variable* lvalue);
    void (*printBinaryExpression)       (FILE* file, int level, BinaryExpression* const node, Variable* lvalue);
    void (*printTernaryExpression)      (FILE* file, int level, TernaryExpression* const node, Variable* lvalue);
    void (*printStatement)              (FILE* file, int level, Statement* const node, Variable* lvalue);
    void (*printFunctionCall)           (FILE* file, int level, FunctionCall* const node, Variable* lvalue);
    void (*printOperand)                (FILE* file, int level, Operand* const node, Variable* lvalue);
    void (*printUnaryOperator)          (FILE* file, int level, UnaryOperator* const node, Variable* lvalue);
    void (*printBinaryOperator)         (FILE* file, int level, BinaryOperator* const node, Variable* lvalue);
    void (*printTernaryOperator)        (FILE* file, int level, TernaryOperator* const node, Variable* lvalue);

};



enum NodeType : int {
    NT_SCOPE,
    NT_VARIABLE_DEFINITION,
    NT_VARIABLE_ASSIGNMENT,
    NT_TYPE_DEFINITION,
    NT_TYPE_INITIALIZATION,
    NT_ENUMERATOR,
    NT_VARIABLE,
    NT_FUNCTION,
    NT_BRANCH,
    NT_SWITCH_CASE,
    NT_WHILE_LOOP,
    NT_FOR_LOOP,
    NT_LOOP,
    NT_RETURN_STATEMENT,
    NT_CONTINUE_STATEMENT,
    NT_BREAK_STATEMENT,
    NT_GOTO_STATEMENT,
    NT_LABEL,
    NT_NAMESPACE,
    NT_STATEMENT,
    NT_FUNCTION_CALL,
    NT_OPERAND,
    NT_UNARY_OPERATOR,
    NT_BINARY_OPERATOR,
    NT_TERNARY_OPERATOR,
    NT_CODE_BLOCK,
    NT_EXPRESSION_WRAPPER
};



// each bit corresponds with the InternaFunction enum
// indicates if function was used at least once in code
extern uint64_t internalFunctionUsed;

enum InternalFunction : uint32_t {
    IF_PRINTF = 1,
    IF_ALLOC  = 2,
};



// ================================= //
//  Section:
//    KEY WORDS
// ================================= //

enum KeyWordType : int {
    KW_VOID,
    KW_INT,
    KW_INT_8,
    KW_INT_16,
    KW_INT_32,
    KW_INT_64,
    KW_UINT_8,
    KW_UINT_16,
    KW_UINT_32,
    KW_UINT_64,
    KW_FLOAT_32,
    KW_FLOAT_64,
    KW_STRING,
    KW_CMP_TIME,
    KW_CONST,
    KW_FUNCTION,
    KW_IF,
    KW_FOR,
    KW_WHILE,
    KW_GOTO,
    KW_ENUM,
    KW_TYPE_DEF,
    KW_RETURN,
    KW_CONTINUE,
    KW_LOOP,
    KW_BREAK,
    KW_USING,
    KW_NAMESPACE,
    KW_ELSE,
    KW_SWITCH_CASE,
    KW_SWITCH_CASE_CASE,
    KW_ALLOC,
};

enum DirectiveKeyWord : int {
    DKW_LANG_DEF,
    DKW_IMPORT
};

struct KeyWord {
    int type;
    const char* str;
};

// internal Variables such as null, true, false etc...
extern Variable* internalVariables[];
extern const int internalVariablesCount;

// ================================= //
//  Section:
//    OPERATORS
// ================================= //

// also indexes operators array
enum OperatorEnum {
    OP_NONE = -1,
    OP_UNARY_PLUS = 0,
    OP_UNARY_MINUS,
    OP_ADDITION,
    OP_SUBTRACTION,
    OP_MULTIPLICATION,
    OP_DIVISION,
    OP_MODULO,
    OP_GET_ADDRESS,
    OP_GET_VALUE,
    OP_BITWISE_AND,
    OP_EQUAL,
    OP_NOT_EQUAL,
    OP_LESS_THAN,
    OP_GREATER_THAN,
    OP_LESS_THAN_OR_EQUAL,
    OP_GREATER_THAN_OR_EQUAL,
    OP_BOOL_AND,
    OP_BOOL_OR,
    OP_INCREMENT,
    OP_DECREMENT,
    OP_SUBSCRIPT,
    OP_MEMBER_SELECTION,
    OP_DEREFERENCE_MEMBER_SELECTION,
    OP_NEGATION,
    // OP_CAST // to tie cast to dtype, not sure about it as operator, but lets see
};
/*
struct OperatorMap {

    int (*unaryPlus) (Operand* op);
    int (*unaryMinus) (Operand* op);
    int (*addition) (Operand* a, Operand* b);
    int (*subtraction) (Operand* a, Operand* b);
    int (*multiplication) (Operand* a, Operand* b);
    int (*division) (Operand* a, Operand* b);
    int (*modulo) (Operand* a, Operand* b);
    int (*address) (Operand* op);
    int (*subscript) (Operand* a, Operand* b);
    int (*memberSelection) (Operand* ans, Operand* a, Operand* b);
    int (*equal) (Operand* a, Operand* b);
    int (*notEqual) (Operand* a, Operand* b);
    int (*lessThan) (Operand* a, Operand* b);
    int (*greaterThan) (Operand* a, Operand* b);
    int (*lessThanOrEqual) (Operand* a, Operand* b);
    int (*greaterThanOrEqual) (Operand* a, Operand* b);
    int (*boolAnd) (Operand* a, Operand* b);
    int (*boolOr) (Operand* a, Operand* b);

    constexpr OperatorMap() : 
        unaryPlus(NULL), 
        unaryMinus(NULL),
        addition(NULL), 
        subtraction(NULL),
        multiplication(NULL), 
        division(NULL),
        modulo(NULL),
        address(NULL),
        subscript(NULL),
        memberSelection(NULL),
        equal(NULL),
        notEqual(NULL),
        lessThan(NULL),
        greaterThan(NULL),
        lessThanOrEqual(NULL),
        greaterThanOrEqual(NULL),
        boolAnd(NULL),
        boolOr(NULL)
    {
    };
    
};
*/

// indexed by OperatorEnum
extern Operator operators[];

extern const int OPERATORS_COUNT;



// ================================= //
//  Section:
//    SYNTAX NODES
// ================================= //

struct SyntaxNode {

    static Scope* root;

    static std::vector<LangDef*> langDefs;
    static std::vector<CodeBlock*> codeBlocks;
    static std::vector<ForeignFunction*> foreignFunctions;

    static std::vector<Variable*> variables;
    static std::vector<Variable*> fcnCalls;
    static std::vector<VariableDefinition*> customDataTypesReferences;
    static std::vector<VariableAssignment*> variableAssignments;
    static std::vector<Variable*> cmpTimeVars;
    static std::vector<Variable*> arrays;
    static std::vector<Loop*> loops;
    static std::vector<Variable*> branchExpressions;
    static std::vector<Statement*> statements;
    static std::vector<VariableDefinition*> initializations;
    static std::vector<ReturnStatement*> returnStatements;
    static std::vector<SwitchCase*> switchCases;

    static std::vector<Slice*> slices;

    static std::vector<VariableAssignment*> arraysAllocations;

    static std::vector<ImportStatement*> imports;

    static std::vector<TypeDefinition*> customDataTypes;
    static std::vector<Enumerator*> enumerators;

    NodeType type;
    Scope* scope;
    Location* loc;

    SyntaxNode(NodeType nodeType) { type = nodeType; };

    virtual void print(Translator* const translator, FILE* file, int level) = 0;
    
};

enum ScopeType {
    SC_GLOBAL = 1,
    SC_COMMON = 0
};

struct Scope : SyntaxNode {

    std::vector<SyntaxNode*> children;

    // LOOK AT : unite?
    std::vector<Variable*> vars; // LOOK AT : maybe exchange for an hash, depends, how many vars are here typicaly per scope
    std::vector<Function*> fcns;
    std::vector<TypeDefinition*> customDataTypes; // TODO : do we need it?
    std::vector<Enumerator*> enums; // LOOK AT : maybe unite enum under Variable interface or something
    std::vector<Namespace*> namespaces;

    Scope() : SyntaxNode(NT_SCOPE) {};

    virtual void print(Translator* const translator, FILE* file, int level);

};

struct Namespace : Scope, INamedEx {

    virtual void print(Translator* const translator, FILE* file, int level);

};

// TODO : think of better name
struct INamedVar : INamedEx {
    std::vector<INamed*> scopeNames;
};

int validateScopeNames(Scope* sc, std::vector<INamed*> names, Namespace** nspace);
/*
struct ScopeName : INamedEx {
    // we need to know which scope name we are dealing with to adjust render as 
    // different languages can have different symbols for accessing different 
    // things, or dont have any at all (enums in C)
    ScopeType type;

    // hmm actualy dont know where to put this function and if its needed
    // but for now lets keep it here, so its not waving around as lil lost kitty
    static int validateScopeNames(std::vector<ScopeName*> names, TypeDefinition** dtype);
};

enum ScopeType : int {
    SC_ENUM,
    SC_STRUCT,
    SC_NAMESPACE
};
*/

// used for interoperability to keep code of the foreign language
struct IForeignCode {
    
    char* tagStr;
    int tagLen;

    char* codeStr;
    int codeLen;

};

struct CodeBlock : IForeignCode, SyntaxNode {

    CodeBlock() : SyntaxNode(NT_CODE_BLOCK) {};

    virtual void print(Translator* const translator, FILE* file, int level);

};

struct Enumerator : SyntaxNode, INamedEx {
    
    DataTypeEnum dtype;
    std::vector<Variable*> vars;

    Enumerator() : SyntaxNode(NT_ENUMERATOR) {};

    virtual void print(Translator* const translator, FILE* file, int level);

};

struct Value {

    DataTypeEnum dtypeEnum;
    int hasValue = 0;
    union {
        int32_t     i32;
        int64_t     i64;
        uint32_t    u32;
        uint64_t    u64;
        float_t     f32;
        double_t    f64;
        Pointer*    ptr;
        Array*      arr;
        Slice*      slc;
        void*       str;
        void*       any;
    };

};

struct Operand : SyntaxNode {

    VariableDefinition* def;

    Value cvalue; // c as compiler
    Value ivalue; // i as interpreter
    // TODO : isnt it part of cvalue?
    
    void* dtype; // !!! if DT_POINTER points to Pointer struct, if DT_ARRAY points to Array struct, if DT_CUSTOM points to TypeDefinition otherwise points to DataType !!!

    std::vector<Value> istack;
    // std::vector<ScopeName*> scopeNames;

    int unrollExpression; // LOOk AT : maybe get rid of Operand itself and use Variable instead as before? or have two types of operand constant and dynamic?
    Expression* expression;

    Operand();
    Operand(Scope* scope);
    virtual void print(Translator* const translator, FILE* file, int level);
    
};

enum ExpressionType {
    EXT_FUNCTION_CALL,
    EXT_UNARY,
    EXT_BINARY,
    EXT_TERNARY,
    EXT_TYPE_INITIALIZATION,
    EXT_STRING_INITIALIZATION,
    EXT_ARRAY_INITIALIZATION,
    EXT_SLICE,
    EXT_WRAPPER,
};

enum ExpressionQualifire {
    EXQ_VARIABLE,
    EXQ_CONSTANT,
    EXQ_COMPILE_TIME
};

struct Expression {

    ExpressionType type;

    virtual void print(Translator* const translator, FILE* file, int level = 0) = 0;

};

struct Statement : SyntaxNode {
    Variable* op;
    Statement() : SyntaxNode(NT_STATEMENT) {};
    virtual void print(Translator* const translator, FILE* file, int level);
};
// LOOK AT : is there a better way
/*
struct VariableDefiniton : SyntaxNode {
    
    Variable* var;
    int flags;

    VariableDefiniton() {};
    VariableDefiniton(Location* loc);
    virtual void print(int level);

};
*/

struct TypeInitialization : Expression {

    TypeInitialization() {
        type = EXT_TYPE_INITIALIZATION;
    };

    std::vector<Variable*> attributes;
    int* idxs; // maps attributes to og indicies in TypeDefinition 

    virtual void print(Translator* const translator, FILE* file, int level = 0);

};

struct StringInitialization : Expression {

    StringInitialization() {
        type = EXT_STRING_INITIALIZATION;
    };

    // already escaped
    std::string rawStr;

    void* wideStr;
    DataTypeEnum wideDtype;
    
    virtual void print(Translator* const translator, FILE* file, int level = 0);

};

struct ArrayInitialization : Expression {

    ArrayInitialization() {
        type = EXT_ARRAY_INITIALIZATION;
    };

    std::vector<Variable*> attributes;

    virtual void print(Translator* const translator, FILE* file, int level = 0);

};

struct VariableDefinition : SyntaxNode {
     
    Variable* var; // it may be enough
    int flags;

    // only for custom data types as they will be linked at the end
    char* dtypeName;
    int dtypeNameLen;
    
    // DataTypeEnum dtypeEnum;
    // TypeDefinition* dtype;

    VariableDefinition();
    VariableDefinition(Location* loc);
    VariableDefinition(Variable* var, int flags);
    virtual void print(Translator* const translator, FILE* file, int level);

};

struct VariableAssignment : SyntaxNode {
    
    Variable* lvar;
    Variable* rvar;
    // Operand* offsetVar; // for arrays and maybe something else

    VariableAssignment();
    VariableAssignment(Location* loc);
    virtual void print(Translator* const translator, FILE* file, int level);

};

struct Variable : INamedVar, Operand {

    // Variable* parentStruct;
    // Variable* attribute;
    // Variable* allocSize; // LOOK AT : dunno about this

    // std::vector<INamed*> scopeNames;

    uint64_t flags; // TODO : get rid of, didn't helped to solve the problem

    Variable();
    Variable(Location* loc);
    Variable(Scope* scope);
    Variable(Scope* const sc, DataTypeEnum dtype);
    Variable(Scope* const sc, DataTypeEnum dtype, Location* loc);
    Variable(Scope* const sc, DataTypeEnum dtype, char* name, int nameLen);
    virtual void print(Translator* const translator, FILE* file, int level);

};

struct Function : SyntaxNode, INamedEx {
    
    std::vector<VariableDefinition*> inArgs;
    std::vector<DataTypeEnum> outArgs; // TODO : !!!!
    std::vector<ReturnStatement*> returns;

    Scope* bodyScope;
    
    int internalIdx; // if it is > 0, then its internal function, and value represents unique id, otherwise should be ignored ***** TODO : for now value: -1 is used as identifer to not render function, fix it later ***** 
    
    int icnt = 0; // counter of function usage by interpreter
    int istackIdx = 0; // 0 is neutral value, no additional stack is used, so indexing is from 1
    /*
    char* tagStr;
    int tagLen;

    char* codeStr;
    int codeLen;
    */

    Function() : SyntaxNode(NT_FUNCTION) {};
    Function(Scope* sc, char* name, int nameLen, std::vector<VariableDefinition*> inArgs, std::vector<DataTypeEnum> outArgs, int internalIdx);
    virtual void print(Translator* const translator, FILE* file, int level);

};

struct ForeignFunction : Function, IForeignCode {

};

struct FunctionCall : Expression, INamedVar {

    FunctionCall() {
        type = EXT_FUNCTION_CALL;
    };

    Function* fcn;
    std::vector<Variable*> inArgs;
    Variable* outArg;

    virtual void print(Translator* const translator, FILE* file, int level = 0);

};

struct Branch : SyntaxNode {

    std::vector<Scope*> scopes;
    std::vector<Variable*> expressions;
    
    Branch() : SyntaxNode(NT_BRANCH) {};
    virtual void print(Translator* const translator, FILE* file, int level);

};

struct SwitchCase : SyntaxNode {
   
    Variable* switchExp;
    std::vector<Variable*> casesExp;

    std::vector<Scope*> cases;
    Scope* elseCase;

    SwitchCase() : SyntaxNode(NT_SWITCH_CASE) {};
    virtual void print(Translator* const translator, FILE* file, int level);

};

struct WhileLoop : SyntaxNode {

    Scope* bodyScope;
    Variable* expression;

    WhileLoop() : SyntaxNode(NT_WHILE_LOOP) {};
    
    virtual void print(Translator* const translator, FILE* file, int level);

};

struct ForLoop : SyntaxNode {

    Scope* bodyScope;

    Variable* initEx;
    Variable* conditionEx;
    Variable* actionEx;

    ForLoop() : SyntaxNode(NT_FOR_LOOP) {};
    
    virtual void print(Translator* const translator, FILE* file, int level);

};

struct Loop : SyntaxNode {
    
    Scope* bodyScope;

    Variable* array;
    Variable* idx;
    VariableDefinition* idxDef;

    Loop() : SyntaxNode(NT_LOOP) {};
    virtual void print(Translator* const translator, FILE* file, int level);

};

struct ReturnStatement : SyntaxNode {

    Function* fcn;
    std::vector<Variable*> vars;

    int idx; // indexes itself in Function.returns

    ReturnStatement() : SyntaxNode(NT_RETURN_STATEMENT) {};
    
    virtual void print(Translator* const translator, FILE* file, int level);

};

struct ContinueStatement : SyntaxNode {

    ContinueStatement() : SyntaxNode(NT_CONTINUE_STATEMENT) {};
    
    virtual void print(Translator* const translator, FILE* file, int level);

};

struct BreakStatement : SyntaxNode {

    BreakStatement() : SyntaxNode(NT_BREAK_STATEMENT) {};

    virtual void print(Translator* const translator, FILE* file, int level);

};

struct GotoStatement : SyntaxNode, INamed {

    Location* loc;
    
    GotoStatement() : SyntaxNode(NT_GOTO_STATEMENT) {};

    virtual void print(Translator* const translator, FILE* file, int level);

};

struct Label : SyntaxNode, INamed {

    Location* loc;

    Label() : SyntaxNode(NT_LABEL) {};

    virtual void print(Translator* const translator, FILE* file, int level);

};

struct Operator {
    
    uint32_t word;
    int rank; // precedence of operator, zero->positive-whatever, high->low *(precedence seems too long for usage)
    uint64_t flag;
    // int (*compare) (Operator* op, uint32_t word);

    // void print(Translator* const translator, const int spaces = 0); // maybe separate function

};

// LOOK AT : think about better name
// what about ArithmeticExpression??
struct OperatorExpression : Expression {
    OperatorEnum operType;
    Operator* oper;
};

struct ExpressionWrapper : SyntaxNode {
    Variable* operand;
    ExpressionWrapper() : SyntaxNode(NT_EXPRESSION_WRAPPER) {};
    virtual void print(Translator* const translator, FILE* file, int lines);
};

// LOOK AT : think about better name
struct WrapperExpression : Expression {
    
    WrapperExpression() {
        type = EXT_WRAPPER;
    };

    Variable* operand;
    
    virtual void print(Translator* const translator, FILE* file, int level = 0);

};

struct UnaryExpression : OperatorExpression {
    
    UnaryExpression() {
        type = EXT_UNARY;
    };

    Variable* operand;

    virtual void print(Translator* const translator, FILE* file, int level = 0);

};

struct BinaryExpression : OperatorExpression {

    BinaryExpression() {
        type = EXT_BINARY;
    };

    Variable* operandA;
    Variable* operandB;

    virtual void print(Translator* const translator, FILE* file, int level = 0);

};

struct TernaryExpression : BinaryExpression {

    TernaryExpression() {
        type = EXT_TERNARY;
    };

    Variable* operandA;
    Variable* operandB;
    Variable* operandC;

    virtual void print(Translator* const translator, FILE* file, int level = 0);

};



// ================================= //
//  Section:
//    DATA TYPES
// ================================= //

enum DataTypeEnum : int {
    DT_VOID = 0,
    DT_INT,
    DT_INT_8,
    DT_INT_16,
    DT_INT_32,
    DT_INT_64,
    DT_UINT_8,
    DT_UINT_16,
    DT_UINT_32,
    DT_UINT_64,
    DT_FLOAT_32,
    DT_FLOAT_64,
    DT_STRING,
    DT_POINTER,
    DT_ARRAY,
    DT_SLICE,
    DT_MULTIPLE_TYPES,
    DT_CUSTOM,
    DT_MEMBER, // dont know about this one, represents the right side of member reference operator 'point . x'
    DT_ENUM,
    DT_UNDEFINED
};

const int DATA_TYPES_COUNT = DT_UNDEFINED + 1;

#define IS_INT(x) ((x) >= DT_INT && (x) <= DT_INT_64)

struct DataType : INamed {
    
    int size; // in bytes
    int rank;

    constexpr DataType() : 
            
        size(0), 
        rank(0),
        
        INamed(NULL, 0)

    {
    
    };

    constexpr DataType(char* const wd, const int wdLen, const int sz, const int rk) : 
        
        size(sz), 
        rank(rk),
        
        INamed(wd, wdLen)

    {

    };

    ~DataType() {};

};

extern DataType dataTypes[DATA_TYPES_COUNT];

struct TypeDefinition : DataType, SyntaxNode {
    
    std::vector<Variable*> vars;

    Function* unaryPlus;
    Function* unaryMinus;
    Function* addition;
    Function* subtraction;
    Function* multiplication;
    Function* division;
    Function* modulo;
    Function* address;
    Function* subscript;

    TypeDefinition() : SyntaxNode(NT_TYPE_DEFINITION) {};
    virtual void print(Translator* const translator, FILE* file, int level);

};

struct Pointer {
    void* pointsTo;
    DataTypeEnum pointsToEnum;
};

struct Array : Pointer {
    Variable* length;
    int flags;
};

struct Slice : Expression {

    Slice() {
        type = EXT_SLICE;
    }

    Variable* arr;
    Variable* bidx;
    Variable* eidx;

    void print(Translator* const translator, FILE* file, int level);

};





struct LangDef {

    String tag;
    
    // compile command used to compile foreign language
    String cmpCommand;

    // 
    String fcnFormat;

    // 
    String fcnFormatInArgs;

    // 
    String fcnFormatOutArgs;

    // for now just simple solution, type is used as length
    KeyWord* dtypeMap;
    int dtypeMapLen;

};

struct ImportStatement {

    // name of the importing file
    String fname;

    // defines how the file file content is wrapped
    // for now only KW_NAMESPACE, -1 is used for no wrapp
    KeyWordType keyWord;

    // additional parrameter for ketWord
    // for KW_NAMESPACE its the name of the namespace
    String param;

    // root scope of the imports file
    Scope* root;

};

