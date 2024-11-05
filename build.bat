@mkdir .\build
@cd .\build

@cl ..\src\*.cpp /std:c++20 /W0 /wd4530 /Fe"./compiler" | Findstr /v "note:" 
@del .\*.obj

@cd ..\
