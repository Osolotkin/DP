#pragma once

namespace Compiler {

    enum OutLangs {
        ITSELF_CONSOLE_LANG = 1 << 0,
        C_LANG              = 1 << 1,
    };

    extern char* mainFile;
    extern char* outFile;
    extern char* outDir;

    extern int outLangs;
    
    int compile();

}
