#ifndef EL_CHECK_SINGLE_PASS_H
#define EL_CHECK_SINGLE_PASS_H

typedef struct Typechecker Typechecker;
typedef struct Symbol_Table Symbol_Table;
typedef struct Symbol Symbol;

struct Symbol_Table {
	Scope *global_scope;
	Scope *current_scope;
	
	// Temporary stack for nested procedures
	Scope *check_top;
};

struct Typechecker {
	Arena *arena;
	Arena *name_arena;
	
	Symbol_Table symbol_table;
	
	Ast_Declaration *first_decl;
	int error_count;
	
	// bool check_shadowing;
};

struct Scope {
	// For tree construction
	Scope  *parent;
	Scope  *next_sibling;
	Scope  *prev_sibling;
	Scope  *first_child;
	Scope  *last_child;
	
	// For name-mangling (?)
	Scope  *lexical_parent;
	
	// Temporary stack for nested procedures
	Scope  *check_next;
	
	Symbol *first_symbol;
	Symbol *last_symbol;
	
	String  name;
};

struct Symbol {
	Symbol *next;
	
	String  ident;
	Type   *type;
	int     size;
};

#endif
