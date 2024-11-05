@mkdir .\build\debug
@cd .\build\debug

@cl -Zi ..\..\src\*.cpp /std:c++20 /W0 /wd4530 /Fe"./compiler" | Findstr /v "note:" 

@cd ..\..\
