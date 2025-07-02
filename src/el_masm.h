#ifndef EL_MASM_H
#define EL_MASM_H

////////////////////////////////
//~ MASM Source

typedef struct MASM_Context MASM_Context;
struct MASM_Context {
	Arena  arena;
	String current_label;
	
	String_List lines;
	int    indent_level;
	String indent_string;
};

global MASM_Context masm_context = {
	.indent_string = string_from_lit_const("\t"),
};

#endif
