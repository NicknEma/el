@echo off
del *.pdb > NUL 2> NUL
del *.rdi > NUL 2> NUL
ml64 generated/generated.asm /nologo /Fegenerated/generated.exe /W4 /WX /Zi /link /incremental:no /opt:ref
del *.obj > NUL 2> NUL
del *.ilk > NUL 2> NUL
del mllink$* > NUL 2> NUL
