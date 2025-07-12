#ifndef EL_CHECK_SINGLE_PASS_H
#define EL_CHECK_SINGLE_PASS_H

typedef struct Typechecker Typechecker;
typedef struct Symbol_Table Symbol_Table;
typedef struct Symbol Symbol;

#define SYMBOL_KIND_NAMES \
X(SYMBOL_NONE) \
X(SYMBOL_LOCAL_VAR) \
X(SYMBOL_GLOBAL_VAR) \
X(SYMBOL_PROC) \
X(SYMBOL_TYPE) \
X(SYMBOL_COUNT)

typedef enum Symbol_Kind {
#define X(name) name,
	SYMBOL_KIND_NAMES
#undef  X
} Symbol_Kind;

global read_only String symbol_kind_names[] = {
#define X(name) string_from_lit_const(#name),
	SYMBOL_KIND_NAMES
#undef  X
};

struct Symbol_Table {
	Scope *global_scope;
	Scope *current_scope;
	
	// Temporary stack for nested procedures
	Scope *check_top;
	
	int global_var_count;
	int proc_count;
};

struct Typechecker {
	Arena *arena;
	Arena *name_arena;
	
	Symbol_Table symbol_table;
	Type_Table type_table;
	
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
	
	Symbol_Kind kind;
	union {
		int bcode_address;
		int bcode_reg;
		int address;
		int offset;
	};
	
	String  ident;
	Type_Id type;
};

#endif
