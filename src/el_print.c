#ifndef EL_PRINT_C
#define EL_PRINT_C

////////////////////////////////
//~ Generic

global int   indent_level;
global char *indent_chars = "  ";

internal void inc_indent(void) { indent_level += 1; }

internal void dec_indent(void) { indent_level = max(indent_level - 1, 0); }

internal void fprint_indent(FILE *stream) {
	for (int i = 0; i < indent_level; i += 1) fputs(indent_chars, stream);
}

internal void print_indent(void) { fprint_indent(stdout); }

#endif
