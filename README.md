# Whatever-named-language

## How to build
using `cl` (Visual Studio)
    run either `build.bat` or `build_debug.bat`
    note, use developer powershell to have cl available

using `g++`
    run either `build_gcc.bat` or `build_gcc_debug.bat`
    note, not that much tested

builded file should be located in `./build`

## Usage
Lets say our compiler exe is compiler.exe and its available.
Lets say our main file is main.vi.

To compile and run use
    `compiler.exe run main.vi`
To just compile use
    `compiler.exe build main.vi`
To generate just C code use
    `compiler.exe translate main.vi`

In all cases `./out` folder will be generated. 
There will be located generated C files and exe file if builded.
Note, these files are note compilable as it is, one have to include headers from `./resources`.

There is list of all command line options that can be used with any of run, build and translate commands:

## Documentation
Documentation is located in `./doc/doc.html`

## Language support
Some useful plugins for VS-Code and Nvim are located in `./tools/`

## Dependencies
Internally uses tcc (Tiny C Compiler):
    - Runtime dependencies are:
        `./tcc`, have to be accessed as `../tcc` from whatever compiler location.
        `libtcc.dll` located in `./build`
    - Build dependencies
        `./lib` folder with bunch of tcc related files, not all may be needed for specific compilation.
