#pragma once

#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include "logger.h"
#include "globals.h"
#include "Utils.h"

#define DEFAULT_MESSAGE_LENGTH 256
#define TAB_SIZE 4

#define RED_ESC "\033[1;31m"
#define COLOR_RESET_ESC "\033[0m"

namespace Logger {

    uint32_t verbosity = HINT | INFO | WARNING | ERROR;

    int getEndClosure(const char ch) {
        switch (ch) {
            case '(' : return ')';
            case '<' : return '>';
            case '[' : return ']';
            case '{' : return '}';
            default: return ')';
        }
    }

    void log(const uint32_t type, const char* const message, Location* loc, int len, ...) {

        if (!(verbosity & type)) return;

        char* const body = loc->file.buff;

        // TODO : buggy when \0
        int tabCount;
        const int lnStartIdx = Utils::findLineStart(body, loc->idx, &tabCount);
        const int lnEndIdx = Utils::findLineEnd(body, loc->idx);
        const int lnLength = lnEndIdx - lnStartIdx + 1;

        switch (len) {

            case STATEMENT : {
                char* strOff = Utils::findChar(body + loc->idx, STATEMENT_END);
                len = (strOff) ? strOff - (body + loc->idx) : 0;
                break;
            }
            
            case LINE :
                len = 0;
                break;

            case CLOSURE : {
                len = Utils::findClosureEnd(body + loc->idx - 1, getEndClosure(body[loc->idx - 1])) - 1;
                break;
            }
        
        }

        const char* underlineEscColor = "";
        switch (type) {

            case INFO : {
                printf("INFO : ");
                break;
            }

            case WARNING : {
                printf("WARNING : ");
                break;
            }

            case ERROR : {
                underlineEscColor = RED_ESC;
                printf(RED_ESC "\nERROR" COLOR_RESET_ESC);
                printf("(%i, %i) : ", loc->line, loc->idx - lnStartIdx + 1);
                break;
            }
        
            default :
                break;
        
        }

        va_list args;
        va_start(args, len);
        vprintf(message, args);
        va_end (args);
        putchar('\n');

        // enough?
        char numbuff[32];
        sprintf(numbuff, "%i | ", loc->line);

        printf("%s%.*s\n", numbuff, lnLength, body + lnStartIdx);
        
        // owful
        const int tabOffset = tabCount * (TAB_SIZE - 1);
        int i = lnStartIdx;
        for (int i = 0; i < strlen(numbuff); i++) putchar(' ');
        for (; i < lnStartIdx + tabCount; i++) putchar('\t');
        for (; i < loc->idx; i++) putchar(' ');
        printf(underlineEscColor);
        for (; i < loc->idx + len; i++) putchar('^');
        printf(COLOR_RESET_ESC);
        for (; i < lnEndIdx; i++) putchar(' ');
        
        putchar('\n');

    }

    void log(const uint32_t type, const char* const message) {

        if (!(verbosity & type)) return;

        switch (type) {

            case HINT : {
                break;
            }

            case INFO : {
                printf("INFO : ");
                break;
            }

            case WARNING : {
                printf("WARNING : ");
                break;
            }

            case ERROR : {
                printf("ERROR : ");
                break;
            }
        
            default :
                break;
        
        }
        printf(message);

    }

}
