#ifndef EL_PRINT_H
#define EL_PRINT_H

////////////////////////////////
//~ Compiler-specific printing utilities

global   int   indent_level;
global   char *indent_chars = "  ";

internal void  inc_indent(void);
internal void  dec_indent(void);

internal void fprint_indent(FILE *stream);
internal void  print_indent(void);

#endif
