#pragma once

#include <stdint.h>
//#include <stdarg.h>
#include "globals.h"

namespace Logger {

    enum Level {
        INFO    = 0x1,
        WARNING = 0x2,
        ERROR   = 0x4
    };

    // better name
    // loc->idx -1 has to point to start closure '(', '{', '<', '[', anything else is not supported for now
    enum UnderlineEnd {
        STATEMENT   = -1,
        LINE        = -2,
        CLOSURE     = -3
    };

    // use Level enum to define bits
    extern uint32_t verbosity;

    void log(const uint32_t type, const char* const message, Location* loc, const int len = 0, ...);
    void log(const uint32_t type, const char* const message);

}
