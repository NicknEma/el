#ifndef EL_PRINT_H
#define EL_PRINT_H

#define WRITER_PROC(name) void *name(void *dest, char *source, usize count)
typedef WRITER_PROC(Writer_Proc);

internal WRITER_PROC(string_builder_writer_proc);
internal WRITER_PROC(libc_file_writer_proc);
internal WRITER_PROC(debugger_writer_proc);
internal WRITER_PROC(print_counter_proc);

internal String bsprint(SliceU8 buffer, char *format, ...);
internal String asprint(Arena *arena,   char *format, ...);
internal void    fprint(FILE *stream,   char *format, ...);
internal void     print(char *format, ...);
internal void    eprint(char *format, ...);
internal void    dprint(char *format, ...);
internal usize   nprint(char *format, ...);

////////////////////////////////
//~ Compiler-specific printing utilities

global   int   indent_level;
global   char *indent_chars = "  ";

internal void  inc_indent(void);
internal void  dec_indent(void);

internal void fprint_indent(FILE *stream);
internal void  print_indent(void);

#endif
