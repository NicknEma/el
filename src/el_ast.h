#ifndef EL_AST_H
#define EL_AST_H

////////////////////////////////
//~ Location

typedef struct Location Location;
struct Location {
	i64 l0, l1;
	i64 c0, c1;
	i64 b0, b1;
};

internal bool location_is_valid(Location location);

internal bool location_is_zero(Location location);
internal bool location_is_greater_than(Location location1, Location location2);

internal Location locations_merge(Location location1, Location location2);

////////////////////////////////
//~ Tokens

//- Token types

typedef enum Token_Kind {
	Token_Kind_INVALID = 0,
	
	/*  33 ..  47 */
	
	Token_Kind_EMARK   = '!',
	Token_Kind_QUOTE   = '"',
	Token_Kind_HASH    = '#',
	Token_Kind_DOLLAR  = '$',
	Token_Kind_PERCENT = '%',
	Token_Kind_AMPER   = '&',
	Token_Kind_APEX    = '\'',
	Token_Kind_LPAREN  = '(',
	Token_Kind_RPAREN  = ')',
	Token_Kind_STAR    = '*',
	Token_Kind_PLUS    = '+',
	Token_Kind_COMMA   = ',',
	Token_Kind_DASH    = '-',
	Token_Kind_DOT     = '.',
	Token_Kind_SLASH   = '/',
	
	/*  58 ..  64 */
	
	Token_Kind_COLON     = ':',
	Token_Kind_SEMICOLON = ';',
	Token_Kind_LTHAN     = '<',
	Token_Kind_EQUALS    = '=',
	Token_Kind_GTHAN     = '>',
	Token_Kind_QMARK     = '?',
	Token_Kind_AT        = '@',
	
	/*  91 ..  96 */
	
	Token_Kind_LBRACK = '[',
	Token_Kind_BSLASH = '\\',
	Token_Kind_RBRACK = ']',
	Token_Kind_HAT    = '^',
	Token_Kind_USCORE = '_',
	Token_Kind_TICK   = '`',
	
	/* 123 .. 126 */
	
	Token_Kind_LBRACE = '{',
	Token_Kind_PIPE   = '|',
	Token_Kind_RBRACE = '}',
	Token_Kind_TILDE  = '~',
	
	/* 127 .. */
	
	Token_Kind_DOUBLE_EQUALS = 127,
	Token_Kind_PLUS_EQUALS,
	Token_Kind_DASH_EQUALS,
	Token_Kind_STAR_EQUALS,
	Token_Kind_SLASH_EQUALS,
	Token_Kind_PERCENT_EQUALS,
	Token_Kind_EMARK_EQUALS,
	Token_Kind_PIPE_EQUALS,
	Token_Kind_AMPER_EQUALS,
	Token_Kind_TILDE_EQUALS,
	Token_Kind_COLON_EQUALS,
	
	Token_Kind_DOUBLE_COLON,
	Token_Kind_TRIPLE_DASH,
	Token_Kind_FORWARD_ARROW,
	
	Token_Kind_INTEGER,
	Token_Kind_STRING,
	Token_Kind_IDENT,
	Token_Kind_KEYWORD,
	
	Token_Kind_EOI,
	Token_Kind_COUNT,
} Token_Kind;

typedef enum Keyword {
	Keyword_NONE = 0,
	Keyword_RETURN,
	Keyword_PROC,
	Keyword_STRUCT,
	Keyword_BREAK,
	Keyword_CONTINUE,
	Keyword_COUNT,
} Keyword;

typedef struct Token Token;
struct Token {
	Token_Kind kind;
	Keyword keyword;
	
	Location location;
	
	String string_val;
	i64    int_val;
};

//- Token functions

typedef struct Parse_Context Parse_Context;

internal Token peek_token(Parse_Context *parser);
internal Token make_token(Parse_Context *parser);
internal void  consume_token(Parse_Context *parser);
internal void  consume_all_tokens(Parse_Context *parser);

internal bool  expect_and_consume_token(Parse_Context *parser, Token_Kind kind);

////////////////////////////////
//~ Operators

//- Operators types

typedef enum Precedence {
	Precedence_NONE = 0,
	// Precedence_COMMA,
	// Precedence_ASSIGNMENT,
	Precedence_TERNARY,
	Precedence_LOGICAL,
	Precedence_RELATIONAL,
	Precedence_ADDITIVE,
	Precedence_MULTIPLICATIVE,
	Precedence_UNARY_PREFIX,
	Precedence_UNARY_POSTFIX,
	Precedence_CALL_OR_ARRAY_ACCESS,
	Precedence_MEMBER,
	Precedence_COUNT,
} Precedence;

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

internal bool token_is_prefix(Token token);
internal bool token_is_postfix(Token token);
internal bool token_is_infix(Token token);
#define token_is_unary(token)  (token_is_prefix(token) || token_is_postfix(token))
#define token_is_binary(token) (token_is_infix(token))

internal Unary_Operator  unary_from_token(Token token);
internal Unary_Operator  unary_from_token_kind(Token_Kind kind);
internal Binary_Operator binary_from_token(Token token);
internal Binary_Operator binary_from_token_kind(Token_Kind kind);

internal bool token_is_expression_terminator(Token token);
internal bool token_is_expression_atom(Token token);

internal bool token_is_declarator(Token token);
internal bool token_is_assigner(Token token);

internal String lexeme_from_token(Parse_Context *parser, Token token);
internal String lexeme_from_token_or_not_printable(Parse_Context *parser, Token token);

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
	Ast_Expression_Kind_LITERAL,
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
	
	Location location;
	
	String lexeme;
	String ident;
	i64    value;
	
	Type   type;
	
	Ast_Expression *next;
	void  *user;
};

internal Ast_Expression *make_atom_expression(Parse_Context *parser, Token token);
internal Ast_Expression *make_unary_expression(Parse_Context *parser, Token unary, Ast_Expression *subexpr);
internal Ast_Expression *make_binary_expression(Parse_Context *parser, Token binary, Ast_Expression *left, Ast_Expression *right);
internal Ast_Expression *make_ternary_expression(Parse_Context *parser, Ast_Expression *left, Ast_Expression *middle, Ast_Expression *right);

internal Ast_Expression *parse_expression(Parse_Context *parser, Precedence caller_precedence, bool required);

internal String string_from_expression_tree(Arena *arena, Ast_Expression *root);
internal void print_expression_tree(Ast_Expression *root);

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
	
	Location location;
	
	Ast_Statement   *block;
	union { Ast_Expression *expr; Ast_Expression *lhs; };
	Ast_Expression  *rhs;
	Ast_Declaration *decl;
	
	Scope *scope;
	
	bool typechecked;
	Ast_Statement *next;
};

internal Ast_Statement *parse_statement(Parse_Context *parser);

internal String string_from_statement_tree(Arena *arena, Ast_Statement *root);
internal void print_statement_tree(Ast_Statement *root);

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
	Entity_Kind_PROCEDURE,
	Entity_Kind_PROCEDURE_TYPE,
	Entity_Kind_PROCEDURE_PROTO,
	Entity_Kind_COUNT,
} Entity_Kind;

typedef struct Entity Entity;
struct Entity {
	Entity_Kind kind;
	
	String   ident;
	Location location;
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
	
	Location location;
	Type_Ann type_annotation;
	
	Ast_Declaration *next;
};

internal Ast_Declaration *parse_declaration(Parse_Context *parser);
internal Ast_Declaration *parse_declaration_after_lhs(Parse_Context *parser, String *idents, i64 ident_count);
internal Initter          parse_declaration_rhs(Parse_Context *parser);

internal Ast_Declaration *parse_proc_header(Parse_Context *parser);
internal Type_Ann         parse_type_annotation(Parse_Context *parser);

internal String string_from_declaration_tree(Arena *arena, Ast_Declaration *root);
internal void print_declaration_tree(Ast_Declaration *root);

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

global read_only Initter nil_initializer = {
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

////////////////////////////////
//~ Context

struct Parse_Context {
	Arena *arena;
	
	String source;
	i64    index;
	
	i64    rollback_index;
	Token  token;
	
	int error_count;
};

global   i64  max_printed_lex_errors   = I64_MAX;
global   i64  max_printed_parse_errors = 1;

internal void parser_init(Parse_Context *parser, Arena *arena, String source);

internal void report_lex_error(Parse_Context *parser, char *message);
internal void report_lex_errorf(Parse_Context *parser, char *format, ...);

internal void report_parse_error(Parse_Context *parser, char *message);
internal void report_parse_errorf(Parse_Context *parser, char *format, ...);

internal void expect_token_kind(Parse_Context *parser, Token_Kind kind, char *message);

internal bool there_were_parse_errors(Parse_Context *parser);

#endif
