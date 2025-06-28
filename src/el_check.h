#ifndef EL_CHECK_H
#define EL_CHECK_H

////////////////////////////////
//~ Errors

typedef struct Check_Context Check_Context;

internal void report_error(String message);

////////////////////////////////
//~ Scope and Symbols

typedef struct Symbol Symbol;
struct Symbol {
	// Data structure management:
	Symbol *next;
	Symbol *prev;
	Symbol *next_free;
	
	// Parser handles:
	Ast_Declaration *decl;
	i64 entity_index;
	
	// Scope-building results:
	String ident;
	Location location_declared;
	Location locations_used[32]; // Arbitrary number for now
	i64 locations_used_count;
	
	// Type-checking results...? TODO.
	Initter *initter;
	
	// Runtime info:
	Type type;
	i64  size;
	i64  addr;
	// Calling_Convention conv;
	// Storage_Class storage_class;
};

global Symbol *first_free_symbol;


typedef struct Scope Scope;
struct Scope {
	Scope *parent;
	Scope *next_sibling;
	Scope *prev_sibling;
	Scope *first_child;
	Scope *last_child;
	
	Symbol *first_symbol;
	Symbol *last_symbol;
};

global Scope scope_global;
global Arena scope_arena;

////////////////////////////////
//~ Type-checking

#endif
