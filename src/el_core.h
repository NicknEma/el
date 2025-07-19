#ifndef EL_CORE_H
#define EL_CORE_H

////////////////////////////////
//~ Misc types

typedef enum Unary_Operator {
	Unary_Operator_NONE = 0,
	Unary_Operator_PLUS,
	Unary_Operator_MINUS,
	Unary_Operator_DEREFERENCE,
	Unary_Operator_COUNT,
} Unary_Operator;

typedef enum Binary_Operator {
	Binary_Operator_NONE = 0,
	Binary_Operator_PLUS,
	Binary_Operator_MINUS,
	Binary_Operator_TIMES,
	Binary_Operator_DIVIDE,
	Binary_Operator_MODULUS,
	Binary_Operator_TERNARY,
	// Binary_Operator_COMMA,
	// Binary_Operator_ASSIGNMENT,
	Binary_Operator_MEMBER,
	Binary_Operator_CALL,
	Binary_Operator_ARRAY_ACCESS,
	Binary_Operator_COUNT,
} Binary_Operator;

typedef enum Type_Modifier {
	Type_Modifier_NONE = 0,
	Type_Modifier_POINTER,
	Type_Modifier_SLICE,
	Type_Modifier_COUNT,
} Type_Modifier;

typedef struct Symbol Symbol;
typedef struct Scope  Scope;

////////////////////////////////
//~ AST

typedef struct Ast_Expression Ast_Expression;
typedef struct Ast_Statement Ast_Statement;
typedef struct Ast_Declaration Ast_Declaration;

typedef struct Expr_List Expr_List;
typedef struct Stmt_List Stmt_List;
typedef struct Decl_List Decl_List;

//- Expressions

// Some expr representations:
//
// 1 + 2
// +
//   1
//   2
//
// foo(x, y)
// CALL
//   IDENT
//   x -> y
//
// Foo{0}
// COMPOUND
//   IDENT
//   0
//
// ^int
// TYPE_MODIFIER of kind POINTER
//   IDENT
//
// []int{0,0,}
// COMPOUND
//   TYPE_MODIFIER of kind SLICE
//     IDENT
//   0 -> 0
//
// struct{x:int}{0}
// COMPOUND
//   STRUCT_DEFN
//   0 -> 0
//
// struct{x:int}
// STRUCT_DEFN
//
// proc(x: int) -> float { return x * 0.5 }
// PROC_DEFN

typedef enum Ast_Expression_Kind {
	Ast_Expression_Kind_NULL = 0,
	
	// "Regular" expressions
	Ast_Expression_Kind_BOOL_LITERAL,
	Ast_Expression_Kind_INT_LITERAL,
	Ast_Expression_Kind_STRING_LITERAL,
	Ast_Expression_Kind_COMPOUND_LITERAL,
	Ast_Expression_Kind_IDENT,
	Ast_Expression_Kind_UNARY,
	Ast_Expression_Kind_BINARY,
	
	// Type annotations
	Ast_Expression_Kind_TYPE_MODIFIER,
	Ast_Expression_Kind_STRUCT_DEFN,
	// Ast_Expression_Kind_PROC_DEFN,
	
	// Other
	Ast_Expression_Kind_ASSIGNMENT,
	
	Ast_Expression_Kind_COUNT,
} Ast_Expression_Kind;

typedef enum Ast_Expression_Flags {
	Ast_Expression_Flag_CONSTANT = (1<<0),
} Ast_Expression_Flags;

struct Expr_List {
	Ast_Expression *first;
	Ast_Expression *last;
};

struct Ast_Expression {
	Ast_Expression_Kind  kind;
	Ast_Expression_Flags flags;
	
	union { Unary_Operator unary; Binary_Operator binary; Type_Modifier modifier; Token_Kind assigner; };
	
	Ast_Expression  *next;
	Ast_Expression  *prev;
	Ast_Expression  *expr_first;
	Ast_Expression  *expr_last;
	Ast_Expression  *expr_sep;
	Ast_Statement   *stmt_first;
	Ast_Statement   *stmt_last;
	Ast_Declaration *dec1_first; // First list of declarations
	Ast_Declaration *dec1_last;
	Ast_Declaration *dec2_first; // Second list of declarations
	Ast_Declaration *dec2_last;
	
	Range1DI32 location;
	
	union {
		struct { String ident; Symbol *symbol; };
		String string_value;
		i64    i64_value;
		bool   bool_value;
	};
	
	Type_Array types;
};

typedef struct Ast_Expression_Array Ast_Expression_Array;
struct Ast_Expression_Array {
	Ast_Expression *data;
	i64 count;
};

//- Statements

typedef enum Ast_Statement_Kind {
	Ast_Statement_Kind_NULL = 0,
	Ast_Statement_Kind_EXPR,
	Ast_Statement_Kind_BLOCK,
	Ast_Statement_Kind_RETURN,
	Ast_Statement_Kind_ASSIGNMENT,
	Ast_Statement_Kind_DECLARATION,
	Ast_Statement_Kind_COUNT,
} Ast_Statement_Kind;

struct Stmt_List {
	Ast_Statement *first, *last;
};

struct Ast_Statement {
	Ast_Statement_Kind kind;
	Token_Kind assigner;
	
	Range1DI32 location;
	
	union {
		// For block statements
		struct {
			Ast_Statement *stmt_first;
			Ast_Statement *stmt_last;
		};
		
		// For assignments and return statements
		struct {
			Ast_Expression *expr_first;
			Ast_Expression *expr_last;
			Ast_Expression *expr_sep;
		};
		
		Ast_Declaration *decl; // For decl statements
	};
	
	Scope *scope;
	
	Ast_Statement *next;
};

//- Declarations

typedef enum Ast_Declaration_Flags {
	Ast_Declaration_Flag_TYPE_ANNOTATION = (1<<0),
	Ast_Declaration_Flag_CONSTANT = (1<<1),
} Ast_Declaration_Flags;

	Ast_Declaration *first, *last;
};

struct Ast_Declaration {
	Ast_Declaration_Flags flags;
	
	Ast_Expression *expr_first;
	Ast_Expression *expr_last;
	Ast_Expression *expr_sep;
	
	Ast_Expression *type_annotation;
	Range1DI32 location;
	
	Ast_Declaration *next;
};

////////////////////////////////
//~ Nil objects
//- Nil objects

global read_only Ast_Expression  nil_expression;
global read_only Ast_Statement   nil_statement;
global read_only Ast_Declaration nil_declaration;

global read_only Ast_Expression nil_expression = {
	.next = &nil_expression,
	.prev = &nil_expression,
	.expr_first = &nil_expression,
	.expr_last = &nil_expression,
	.expr_sep = &nil_expression,
	.stmt_first = &nil_statement,
	.stmt_last = &nil_statement,
	.dec1_first = &nil_declaration,
	.dec1_last = &nil_declaration,
	.dec2_first = &nil_declaration,
	.dec2_last = &nil_declaration,
};

#define check_nil_expression(p) ((p)==0||(p)==&nil_expression)
#define set_nil_expression(p) ((p)=&nil_expression)

global read_only Ast_Statement nil_statement = {
	.stmt_first = &nil_statement,
	.stmt_last = &nil_statement,
	.expr_first = &nil_expression,
	.expr_last = &nil_expression,
	.expr_sep = &nil_expression,
	.decl = &nil_declaration,
	.next = &nil_statement,
};

#define check_nil_statement(p) ((p)==0||(p)==&nil_statement)
#define set_nil_statement(p) ((p)=&nil_statement)

global read_only Ast_Declaration nil_declaration = {
	.expr_first = &nil_expression,
	.expr_last = &nil_expression,
	.expr_sep = &nil_expression,
	.type_annotation = &nil_expression,
	.next = &nil_declaration,
};

#define check_nil_declaration(p) ((p)==0||(p)==&nil_declaration)
#define set_nil_declaration(p) ((p)=&nil_declaration)

#endif
