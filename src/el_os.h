#ifndef EL_OS_H
#define EL_OS_H

typedef struct String_And_Bool String_And_Bool;
struct String_And_Bool { String str; bool ok; };

internal String_And_Bool load_file_string(Arena *arena, char *name);

#endif
