@echo off
del *.pdb > NUL 2> NUL
del *.rdi > NUL 2> NUL

set include_dir="C:/Program Files/RadDebugger"

REM nameless union/struct
set warnings=           -wd4201

REM unused param
set warnings=%warnings% -wd4100

REM unused local var (initialized)
REM set warnings=%warnings% -wd4189

REM unused local var
REM set warnings=%warnings% -wd4101

REM i64 to i32 conversion
set warnings=%warnings% -wd4244

REM constant conditional expression
set warnings=%warnings% -wd4127

cl src/el.c -nologo -Fe:el.exe -I %include_dir% -Z7 -D_CRT_SECURE_NO_WARNINGS -W4 -external:anglebrackets -external:W0 %warnings% -link -incremental:no -opt:ref
del *.obj > NUL 2> NUL
del *.ilk > NUL 2> NUL
