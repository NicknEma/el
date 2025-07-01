#ifndef EL_CHECK_SINGLE_PASS_H
#define EL_CHECK_SINGLE_PASS_H

typedef struct Typechecker Typechecker;
typedef struct Symbol_Table Symbol_Table;
typedef struct Symbol Symbol;
typedef struct Scope_Stack Scope_Stack;

struct Symbol_Table {
	Scope_Stack *current_scope_stack;
};

struct Typechecker {
	Arena *arena;
	
	Symbol_Table symbol_table;
	
	Ast_Declaration *first_decl;
	int error_count;
};

struct Scope_Stack {
	Scope_Stack *next;
	Scope *inner;
};

struct Scope {
	Scope *parent;
	Symbol *first_symbol;
	Symbol *last_symbol;
};

struct Symbol {
	Symbol *next;
	
	String ident;
	Type type;
};

#endif
