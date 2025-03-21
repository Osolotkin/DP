#pragma once

#include "stdint.h"
#include <filesystem>

constexpr uint16_t toDoubleChar(char first, char second) {
    return (static_cast<uint16_t>(first) << 8) | static_cast<uint16_t>(second);
}

struct Location;
struct LogInfo;
struct File;
struct INamed;

const int EOL = '\n';
const int EOS = '\0'; // dont know if it is meaningful to do so...

const char SCOPE_BEGIN      = '{';
const char SCOPE_END        = '}';
const char FUNCTION_START   = '(';
const char FUNCTION_END     = ')';
const char STRING_LITERAL   = '"';
const char ARRAY_BEGIN      = '[';
const char ARRAY_END        = ']';
const char LABEL_BEGIN      = '>';
const char LABEL_END        = ':';
const char STATEMENT_BEGIN  = ':';
const char STATEMENT_END    = ';';
const char SKIP_VARIABLE    = '_';
const char LIST_SEPARATOR   = ',';
const char STRING_BEGIN     = '"';
const char STRING_END       = '"';
const char CHAR_BEGIN       = '\'';
const char CHAR_END         = '\'';

const char ESCAPE_CHAR = '\\';

const char POINTER_SYMBOL = '^';
const char ADDRESS_SYMBOL = '&';

const uint16_t SCOPE_RESOLUTION_SYMBOL = toDoubleChar(':', ':');
const uint16_t FILL_THE_REST_SYMBOL = toDoubleChar('.', '.');

const char KWS_VOID[]       = "void";
const char KWS_INT[]        = "int";
const char KWS_INT_8[]      = "i8";
const char KWS_INT_16[]     = "i16";
const char KWS_INT_32[]     = "i32";
const char KWS_INT_64[]     = "i64";
const char KWS_UINT_8[]     = "u8";
const char KWS_UINT_16[]    = "u16";
const char KWS_UINT_32[]    = "u32";
const char KWS_UINT_64[]    = "u64";
const char KWS_FLOAT_32[]   = "f32";
const char KWS_FLOAT_64[]   = "f64";
const char KWS_STRING[]     = "string";
const char KWS_POINTER[]    = "^";

const char KWS_CONST[]      = "const";
const char KWS_CMP_TIME[]   = "embed";
const char KWS_FUNCTION[]   = "fcn";
const char KWS_ENUM[]       = "enum";
const char KWS_TYPE_DEF[]   = "def";
const char KWS_SCOPE[]      = "scope";
const char KWS_NAMESPACE[]  = "namespace";
const char KWS_ERROR[]      = "error";
const char KWS_STRUCT[]     = "struct";
const char KWS_UNION[]      = "union";
const char KWS_CATCH[]      = "catch";

const char KWS_IF[]     = "if";
const char KWS_ELSE[]   = "else";
const char KWS_FOR[]    = "for";
const char KWS_WHILE[]  = "while";
const char KWS_LOOP[]   = "loop";
const char KWS_GOTO[]   = "goto";

const char KWS_SWITCH_CASE[]        = "when";
const char KWS_SWITCH_CASE_CASE[]   = "case";

const char KWS_USING[]   = "using";

const char KWS_RETURN[]     = "return";
const char KWS_CONTINUE[]   = "continue";
const char KWS_BREAK[]      = "break";

const char KWS_ALLOC[]      = "alloc";
const char KWS_FREE[]       = "free";

const char KWS_ARRAY_LENGTH[]   = "length";
const char KWS_ARRAY_SIZE[]     = "size";

const char KWS_IMPORT[]    = "import";

const char DKWS_LANG_DEF[]  = "lang_def";

const char IFS_PRINTF[] = "printf";
const char IFS_ALLOC[]  = "malloc";
const char IFS_FREE[]   = "free";

const char IVS_NULL[] = "null";
const char IVS_TRUE[] = "true";
const char IVS_FALSE[] = "false";

const char IS_FILL = '@';

// TODO : better name
enum State : uint64_t {
    IS_CONST = 1 << 0,
    IS_CMP_TIME = 1 << 1,
    IS_EMBEDED = 1 << 2,

    IS_UNARY = 1 << 3,
    IS_BINARY = 1 << 4,
    IS_TERNARY = 1 << 5,

    IS_ONE_CHAR = 1 << 7,
    IS_TWO_CHAR = 1 << 8,
    IS_THREE_CHAR = 1 << 9,
    IS_FOUR_CHAR = 1 << 10,

    IS_ARRAY = 1 << 11,
    IS_ARRAY_LIST = 1 << 12,
    
    IS_ALLOCATED = 1 << 13,

    IS_STRING  = 1 << 14,

    IS_ALLOCATION = 1 << 15,
    IS_LENGTH = 1 << 16,
    IS_SIZE = 1 << 17,

    IS_RENDERED = 1 << 24,
    /*
    IS_RESERVED_16  = 1 << 18,
    IS_RESERVED_17  = 1 << 19,
    IS_RESERVED_18  = 1 << 20,
    IS_RESERVED_19  = 1 << 21,
    IS_RESERVED_20  = 1 << 22,
    IS_RESERVED_21  = 1 << 23,
    IS_RESERVED_23  = 1 << 25,
    IS_RESERVED_24  = 1 << 26,
    IS_RESERVED_25  = 1 << 27,
    IS_RESERVED_26  = 1 << 28,
    IS_RESERVED_27  = 1 << 29,

    */

    IS_UNIQUE  = 1 << 30, // used in parser while checking for unique names in scope

};

struct File {
    std::filesystem::path absPath;
    char* absPathRaw;
    char* name;
    char* buff;
};

struct Location {
    File* file;
    int line; // to track current line
    int idx; // current position in buffer
};

struct LogInfo {
    Location loc;
};

struct INamed {
	    
    char* name;
    int nameLen;

    INamed() {};

    constexpr INamed(char* const nm, const int nmLn) : 
        name(nm), 
        nameLen(nmLn)
    {

    };

};

// sometimes we need to store locations of INamed stuff that 
// is not describing advanced stuff
// stuff... stuff...
struct INamedLoc : INamed {

    Location* loc;

    constexpr INamedLoc(char* const nm, const int nmLn, Location* loc) : 
        INamed(nm, nmLn),
        loc(loc)
    {

    };

};

// LOOK AT : maybe better name
struct INamedEx : INamed {
    
    uint32_t id; // LOOK AT : change to macro for 32bit/64bit

    INamedEx() {};
    
    constexpr INamedEx(char* const nm, const int nmLn, const uint32_t id) :
        INamed(nm, nmLn),
        id(id)
    {

    };

};

struct String {
    char* buff;
    int len;

    inline char& operator [] (const int idx) {
		return buff[idx];
	}

    inline operator char* () {
		return buff;
	}

    inline void operator = (char* str) {
		buff = str;
	}

    inline void operator += (int offset) {
		buff += offset;
        len -= offset;
	}
};
