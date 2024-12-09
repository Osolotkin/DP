#pragma once

#include "compiler.h"
#include "parser.h"
#include "validator.h"
#include "logger.h"
#include "syntax.h"
#include "error.h"
#include "utils.h"

#include "file_driver.h"

#include "itself_console_translator.h"
#include "c_translator.h"

#include <map>
#include <string>

#define NULL 0

#define runTranslator(t) {t.init(Compiler::outDir); SyntaxNode::root->print(&(t), t.mainFile, 0); t.exit();}



char* Compiler::mainFile = NULL;
char* Compiler::outFile = NULL;
char* Compiler::outDir = NULL;

int Compiler::outLangs = 0;



struct ForeignLangsData {
    LangDef* data;
    FILE* file;    
};
std::map<std::string, ForeignLangsData*> foreignLangsMap;

template <typename T>
int printForeignCode(std::vector<T*> codeBlocks, std::vector<LangDef*>, char* outDir);
int printForeignFunction(std::vector<ForeignFunction*> foreignFcn, std::vector<LangDef*> langDefs, char* outDir);

int compileForeignCode(char* path);




int Compiler::compile() {

    Logger::log(Logger::INFO, "Compilation started...\n");

    int err;

    err = Parser::parse(mainFile);
    if (err) return err;
    
    Logger::log(Logger::INFO, "Parsing completed...\n");

    err = Validator::validate();
    if (err) return err;

    Logger::log(Logger::INFO, "Validating completed...\n");

    if (printForeignCode(SyntaxNode::codeBlocks, SyntaxNode::langDefs, Compiler::outDir)) return 1;
    if (printForeignFunction(SyntaxNode::foreignFunctions, SyntaxNode::langDefs, Compiler::outDir)) return 1;
    if (compileForeignCode(Compiler::outDir)) return 1;
    
    Logger::log(Logger::INFO, "Foreign block of codes assembled...\n");

    // TODO : make paralel
    if (outLangs & ITSELF_CONSOLE_LANG) runTranslator(translatorItselfConsole);
    if (outLangs & C_LANG) runTranslator(translatorC);

    Logger::log(Logger::INFO, "Translation completed...\n");

    return 0;

}

// for now here
// TODO : proper errors
template <typename T>
int printForeignCode(std::vector<T*> foreignCode, std::vector<LangDef*> langDefs, char* outDir) {

    for (int i = 0; i < foreignCode.size(); i++) {

        const std::string str = std::string(foreignCode[i]->tagStr, foreignCode[i]->tagLen);
        
        if (!foreignLangsMap.count(str)) {

            FILE* file = FileDriver::openFile(foreignCode[i]->tagStr, foreignCode[i]->tagLen, outDir, "wb");
            if (!file) return 1;
            
            foreignLangsMap[str] = new ForeignLangsData;
            foreignLangsMap[str]->file = file;

            LangDef* langDef = NULL;
            for (int i = 0; i < langDefs.size(); i++) {
                
                char* tag = langDefs[i]->tag;
                const int tagLen = langDefs[i]->tag.len;
                
                int areTheSame = 1;
                
                if (str.size() != tagLen) continue;
                for (int i = 0; i < str.size(); i++) {
                    if (str[i] != tag[i]) {
                        areTheSame = 0;
                        break;
                    }
                }

                if (areTheSame) {
                    langDef = langDefs[i];
                    break;
                }

            }

            if (!langDef) {
                Logger:log(Logger::ERROR, ERR_STR(Err::APPROPRIATE_TAG_DOES_NOT_EXISTS), foreignCode[i]->loc, str.size(), str.c_str());
                return Err::APPROPRIATE_TAG_DOES_NOT_EXISTS;
            }

            foreignLangsMap[str]->data = langDef;

        }

        fwrite(foreignCode[i]->codeStr, sizeof(char), foreignCode[i]->codeLen, foreignLangsMap[str]->file);

    }

    return 0;

}

int printForeignFunction(std::vector<ForeignFunction*> foreignFcn, std::vector<LangDef*> langDefs, char* outDir) {

    for (int i = 0; i < foreignFcn.size(); i++) {

        const std::string str = std::string(foreignFcn[i]->tagStr, foreignFcn[i]->tagLen);
        
        if (!foreignLangsMap.count(str)) {

            FILE* file = FileDriver::openFile(foreignFcn[i]->tagStr, foreignFcn[i]->tagLen, outDir, "wb");
            if (!file) return 1;
            
            foreignLangsMap[str] = new ForeignLangsData;
            foreignLangsMap[str]->file = file;

            LangDef* langDef = NULL;
            for (int i = 0; i < langDefs.size(); i++) {
                
                char* tag = langDefs[i]->tag;
                const int tagLen = langDefs[i]->tag.len;
                
                int areTheSame = 1;
                
                if (str.size() != tagLen) continue;
                for (int i = 0; i < str.size(); i++) {
                    if (str[i] != tag[i]) {
                        areTheSame = 0;
                        break;
                    }
                }

                if (areTheSame) {
                    langDef = langDefs[i];
                    break;
                }

            }

            if (!langDef) {
                Logger:log(Logger::ERROR, ERR_STR(Err::APPROPRIATE_TAG_DOES_NOT_EXISTS), foreignFcn[i]->loc, str.size(), str.c_str());
                return Err::APPROPRIATE_TAG_DOES_NOT_EXISTS;
            }

            foreignLangsMap[str]->data = langDef;

        }

        FILE* const file = foreignLangsMap[str]->file;// stdout;
        ForeignFunction* ff = foreignFcn[i];

        LangDef* langDef = foreignLangsMap[str]->data;
        String fcnFormat = langDef->fcnFormat;
        String inArgsFormat = langDef->fcnFormatInArgs;
        String outArgsFormat = langDef->fcnFormatOutArgs;

        int startIdx = 0;
        for (int i = startIdx; i < fcnFormat.len; i++) {
            
            char ch = fcnFormat[i];
            if (ch != '%') continue;

            fwrite(fcnFormat.buff + startIdx, sizeof(char), (i <= 1) ? 0 : i - startIdx, file);

            // !!!
            // as all format options are one char long for now
            startIdx = i + 2;
            i++;
            // !!!

            ch = fcnFormat[i];
            if (ch == 'n') {
                fwrite(ff->name, sizeof(char), ff->nameLen, file);
                fprintf(file, "_%i", ff->id);
            } else if (ch == 'c') {
                fwrite(ff->codeStr, sizeof(char), ff->codeLen, file);
            } else if (ch == 'i') {

                std::vector<VariableDefinition*> variables = ff->inArgs;

                int idx = 0;
                Utils::skipWhiteSpaces(inArgsFormat, &idx);

                String delimiter = {(char*) "", 0};
                if (inArgsFormat[idx] == '%' && (inArgsFormat[idx + 1] != 'n' && inArgsFormat[idx + 1] != 't')) {
                    delimiter = {
                        inArgsFormat + idx + 1,
                        Utils::skipTillWhiteSpace(inArgsFormat + idx + 1)
                    };
                    inArgsFormat += delimiter.len + 1;
                }

                int varIdx = 0;
                while (idx < variables.size()) {
                    
                    Variable* var = variables[idx]->var;
                    DataType* dtype = dataTypes + variables[idx]->var->cvalue.dtypeEnum;

                    int startIdx = 0;
                    for (int i = 0; i < inArgsFormat.len; i++) {

                        char ch = inArgsFormat[i];
                        if (ch != '%') continue;

                        fwrite(inArgsFormat.buff + startIdx, sizeof(char), (i <= 1) ? 0 : i - startIdx, file);

                        startIdx = i + 2;
                        i++;
                        
                        ch = inArgsFormat[i];
                        if (ch == 'n') {
                            fwrite(var->name, sizeof(char), var->nameLen, file);
                            // fprintf(file, "_%i", var->id);
                        } else if (ch == 't') {
                            fwrite(dtype->name, sizeof(char), dtype->nameLen, file);
                        }

                    }
                    
                    if (idx < variables.size() - 1) {
                        fwrite(delimiter.buff, sizeof(char), delimiter.len, file);
                    }

                    idx++;

                }

            } else if (ch == 'o') {

                std::vector<DataTypeEnum> dtypes = ff->outArgs;

                int idx = 0;
                Utils::skipWhiteSpaces(outArgsFormat, &idx);

                String delimiter = {(char*) "", 0};
                if (outArgsFormat[idx] == '%' && (outArgsFormat[idx + 1] != 'n' && outArgsFormat[idx + 1] != 't')) {
                    delimiter = {
                        outArgsFormat + idx + 1,
                        Utils::skipTillWhiteSpace(outArgsFormat + idx + 1)
                    };
                    outArgsFormat += delimiter.len + 1;
                }

                int dtypeIdx = 0;
                while (idx < dtypes.size()) {
                    
                    DataType* dtype = dataTypes + dtypes[idx];

                    int startIdx = 0;
                    for (int i = 0; i < outArgsFormat.len; i++) {

                        char ch = outArgsFormat[i];
                        if (ch != '%') continue;

                        fwrite(outArgsFormat.buff + startIdx, sizeof(char), (i <= 1) ? 0 : i - startIdx, file);

                        startIdx = i + 2;
                        i++;
                        
                        fwrite(dtype->name, sizeof(char), dtype->nameLen, file);

                    }

                    if (idx < dtypes.size() - 1) {
                        fwrite(delimiter.buff, sizeof(char), delimiter.len, file);
                    }

                    idx++;

                }

                if (idx == 0) {

                    KeyWord dtype = langDef->dtypeMap[DT_VOID];

                    int startIdx = 0;
                    for (int i = 0; i < outArgsFormat.len; i++) {

                        char ch = outArgsFormat[i];
                        if (ch != '%') continue;

                        fwrite(outArgsFormat.buff + startIdx, sizeof(char), (i <= 1) ? 0 : i - startIdx, file);

                        startIdx = i + 2;
                        i++;
                        
                        fwrite(dtype.str, sizeof(char), dtype.type, file);

                    }

                    if (idx < dtypes.size() - 1) {
                        fwrite(delimiter.buff, sizeof(char), delimiter.len, file);
                    }

                }

            }
        
        }

        fwrite(fcnFormat.buff + startIdx, sizeof(char), (fcnFormat.len <= 1) ? 0 : fcnFormat.len - startIdx, file);

    }

    return 0;

}

int compileForeignCode(char* path) {

    for (auto const& item : foreignLangsMap) {

        fclose(item.second->file);
        
        char* cmd = item.second->data->cmpCommand;
        const int cmdLen = item.second->data->cmpCommand.len;

        char* tag = item.second->data->tag;
        const int tagLen = item.second->data->tag.len;

        for (int i = 0; i < cmdLen; i++) {
            
            if (cmd[i] == '%') {
                    
                char* oldCmd = cmd;

                cmd = (char*) malloc((cmdLen + tagLen) * sizeof(char));
                if (!cmd) return Err::MALLOC;
                
                const int offset = i;
                for (; i < offset + tagLen; i++) {
                    cmd[i] = tag[i - offset];
                };
                
                for (; i < cmdLen + tagLen - 1; i++) {
                    cmd[i] = oldCmd[i - tagLen + 1];

                }

                cmd[i] = '\0';

                for (i = 0; i < offset; i++) {
                    cmd[i] = oldCmd[i];
                }

                break;
            
            }
        
        }

        char buffer[256];
        const int len = snprintf(buffer, sizeof(buffer), "cd %s && %s", path, cmd);
        if (len < 0 || len > sizeof(buffer)) {
            Logger::log(Logger::ERROR, ERR_STR(Err::BUFFER_OVERRUN));
            return Err::BUFFER_OVERRUN;
        }

        if (system(buffer) == -1) {            
            Logger::log(Logger::ERROR, ERR_STR(Err::SYSTEM_COMMAND_EXECUTION_FAILED));
            return Err::SYSTEM_COMMAND_EXECUTION_FAILED;
        }

    }

    return 0;

}
