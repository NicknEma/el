@echo off
del *.pdb > NUL 2> NUL
del *.rdi > NUL 2> NUL
cl src/el.c -nologo -Fe:el.exe -Z7 -D_CRT_SECURE_NO_WARNINGS -link -incremental:no -opt:ref
del *.obj > NUL 2> NUL
del *.ilk > NUL 2> NUL
