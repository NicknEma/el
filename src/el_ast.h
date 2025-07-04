#ifndef EL_AST_H
#define EL_AST_H

////////////////////////////////
//~ Operators

//- Operators types

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
	// Binary_Operator_COMMA,
	Binary_Operator_MEMBER,
	Binary_Operator_CALL,
	Binary_Operator_ARRAY_ACCESS,
	Binary_Operator_COUNT,
} Binary_Operator;

////////////////////////////////
//~ Types

typedef enum Type_Kind {
	Type_Kind_UNKNOWN = 0,
	Type_Kind_VOID,
	Type_Kind_TYPE,
	Type_Kind_BOOLEAN,
	Type_Kind_INTEGER,
	Type_Kind_STRING,
	Type_Kind_STRUCT,
	Type_Kind_POINTER,
	Type_Kind_PROC,
	Type_Kind_COUNT,
} Type_Kind;

typedef struct Type Type;
struct Type {
	Type_Kind kind;
	
	// For pointers
	Type *pointed;
	
	// For structs
	Type *members;
	i64   member_count;
	
	// For procedures
	Type *params;
	i64   param_count;
	Type *retvals;
	i64   retval_count;
};

////////////////////////////////
//~ AST

typedef struct Ast_Expression Ast_Expression;
typedef struct Ast_Statement Ast_Statement;
typedef struct Ast_Declaration Ast_Declaration;

typedef struct Scope Scope;

//- Expressions

typedef enum Ast_Expression_Kind {
	Ast_Expression_Kind_NULL = 0,
	Ast_Expression_Kind_INT_LITERAL,
	Ast_Expression_Kind_STRING_LITERAL,
	Ast_Expression_Kind_IDENT,
	Ast_Expression_Kind_UNARY,
	Ast_Expression_Kind_BINARY,
	Ast_Expression_Kind_TERNARY,
	Ast_Expression_Kind_COUNT,
} Ast_Expression_Kind;

struct Ast_Expression {
	Ast_Expression_Kind kind;
	Unary_Operator      unary;
	Binary_Operator     binary;
	
	union { Ast_Expression *left; Ast_Expression *subexpr; };
	Ast_Expression *middle; // For ternaries
	Ast_Expression *right;
	
	Range1DI32 location;
	
	String lexeme;
	String ident;
	i64    value;
	
	Type   type;
	
	Ast_Expression *next;
	void  *user;
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

struct Ast_Statement {
	Ast_Statement_Kind kind;
	Token_Kind assigner;
	
	Range1DI32 location;
	
	Ast_Statement   *block;
	union { Ast_Expression *expr; Ast_Expression *lhs; };
	Ast_Expression  *rhs;
	Ast_Declaration *decl;
	
	Scope *scope;
	
	bool typechecked;
	Ast_Statement *next;
};

//- Declarations

// TODO: In the type-checking phase, decls must either have an explicit type or an initializer, or something...
typedef enum Type_Ann_Kind {
	Type_Ann_Kind_NONE = 0,
	Type_Ann_Kind_IDENT,
	Type_Ann_Kind_COUNT,
} Type_Ann_Kind;

typedef struct Type_Ann Type_Ann;
struct Type_Ann {
	Type_Ann_Kind kind;
	String ident;
};


typedef enum Entity_Kind {
	Entity_Kind_NULL = 0,
	Entity_Kind_UNKNOWN, // For things that don't have an initializer but just a type annotation, or for when the initializer is a generic expression (of which we obviously don't know the type yet)
	Entity_Kind_STRUCT,
	Entity_Kind_VALUE,
	Entity_Kind_PROCEDURE,
	Entity_Kind_PROCEDURE_TYPE,
	Entity_Kind_PROCEDURE_PROTO,
	Entity_Kind_COUNT,
} Entity_Kind;

typedef struct Entity Entity;
struct Entity {
	Entity_Kind kind;
	
	String   ident;
	Range1DI32 location;
};


typedef enum Initter_Kind {
	Initter_Kind_NONE = 0,
	Initter_Kind_EXPR,
	Initter_Kind_STRUCT,
	Initter_Kind_PROCEDURE,
	Initter_Kind_PROCEDURE_TYPE,
	Initter_Kind_PROCEDURE_PROTO,
	Initter_Kind_COUNT,
} Initter_Kind;

typedef struct Initter Initter;
struct Initter {
	Initter_Kind kind;
	
	union { Ast_Declaration *first_param; Ast_Declaration *first_member; };
	Ast_Statement  *body;
	Ast_Expression *expr;
};


typedef enum Ast_Declaration_Flags {
	Ast_Declaration_Flag_TYPE_ANNOTATION = (1<<0),
	Ast_Declaration_Flag_CONSTANT = (1<<1),
} Ast_Declaration_Flags;

struct Ast_Declaration {
	Ast_Declaration_Flags flags;
	
	i64      entity_count;
	Entity  *entities;
	
	i64      initter_count;
	Initter *initters;
	
	Range1DI32 location;
	Type_Ann type_annotation;
	
	Ast_Declaration *next;
};

////////////////////////////////
//~ Nil objects

global read_only Ast_Expression nil_expression = {
	.left   = &nil_expression,
	.middle = &nil_expression,
	.right  = &nil_expression,
	.next   = &nil_expression,
};

#define check_nil_expression(p) ((p)==0||(p)==&nil_expression)
#define set_nil_expression(p) ((p)=&nil_expression)

global read_only Ast_Declaration nil_declaration;
global read_only Ast_Statement nil_statement = {
	.block = &nil_statement,
	.lhs   = &nil_expression,
	.rhs   = &nil_expression,
	.decl  = &nil_declaration,
	.next  = &nil_statement,
};

#define check_nil_statement(p) ((p)==0||(p)==&nil_statement)
#define set_nil_statement(p) ((p)=&nil_statement)

global read_only Ast_Declaration nil_declaration = {
	.next = &nil_declaration,
};

#define check_nil_declaration(p) ((p)==0||(p)==&nil_declaration)
#define set_nil_declaration(p) ((p)=&nil_declaration)

global read_only Initter nil_initter = {
	.first_param = &nil_declaration,
	.body = &nil_statement,
	.expr = &nil_expression,
};


#if 0
// TODO: Parsing declarations in parameter lists, struct bodies or in a generic contexts have
// very similar control flow. I wonder if it's convenient to abstract them all into one
// implementation and switch on the details based on a context param.
typedef enum Ast_Declaration_List_Context {
	Ast_Declaration_List_Context_NONE = 0,
	Ast_Declaration_List_Context_GENERIC,
	Ast_Declaration_List_Context_STRUCT_OR_UNION_BODY,
	Ast_Declaration_List_Context_PROC_HEADER,
	Ast_Declaration_List_Context_COUNT,
} Ast_Declaration_List_Context;
#endif

#endif
