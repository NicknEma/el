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
	
	Token_Kind_PERCENT = '%',
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
	Token_Kind_QMARK     = '?',
	
	/*  91 ..  96 */
	
	Token_Kind_LBRACK = '[',
	Token_Kind_RBRACK = ']',
	Token_Kind_HAT    = '^',
	
	/* 123 .. 126 */
	
	Token_Kind_LBRACE = '{',
	Token_Kind_RBRACE = '}',
	
	/* 127 .. */
	
	Token_Kind_DOUBLE_COLON = 127,
	
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
	Keyword_COUNT,
} Keyword;

global read_only String keywords[] = {
	string_from_lit_const(""),
	string_from_lit_const("return"),
};

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
internal bool  expect_and_consume_token(Parse_Context *parser, Token_Kind kind);

////////////////////////////////
//~ Operators

//- Operators types

typedef enum Precedence {
	Precedence_NONE = 0,
	Precedence_COMMA,
	Precedence_ASSIGNMENT,
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
	Binary_Operator_COMMA,
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

////////////////////////////////
//~ AST

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

typedef struct Ast_Expression Ast_Expression;
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
	
	void  *user;
};

internal Ast_Expression *make_atom_expression(Parse_Context *parser, Token token);
internal Ast_Expression *make_unary_expression(Parse_Context *parser, Token unary, Ast_Expression *subexpr);
internal Ast_Expression *make_binary_expression(Parse_Context *parser, Token binary, Ast_Expression *left, Ast_Expression *right);
internal Ast_Expression *make_ternary_expression(Parse_Context *parser, Ast_Expression *left, Ast_Expression *middle, Ast_Expression *right);

internal Ast_Expression *parse_expression(Parse_Context *parser, Precedence caller_precedence, bool required);

internal String string_from_expression_tree(Arena *arena, Ast_Expression *root);
internal void print_expression_tree(Ast_Expression *root);

//- Ast_Statements

typedef enum Ast_Statement_Kind {
	Ast_Statement_Kind_NULL = 0,
	Ast_Statement_Kind_EXPR,
	Ast_Statement_Kind_BLOCK,
	Ast_Statement_Kind_RETURN,
	Ast_Statement_Kind_COUNT,
} Ast_Statement_Kind;

typedef struct Ast_Statement Ast_Statement;
struct Ast_Statement {
	Ast_Statement_Kind kind;
	
	Location location;
	
	Ast_Statement  *block;
	Ast_Expression *expr;
	
	Ast_Statement *next;
};

internal Ast_Statement *parse_statement(Parse_Context *parser);

//- Ast_Declarations

typedef enum Ast_Declaration_Kind {
	Ast_Declaration_Kind_VARIABLE,
	Ast_Declaration_Kind_PROCEDURE,
	Ast_Declaration_Kind_COUNT,
} Ast_Declaration_Kind;

typedef struct Ast_Declaration Ast_Declaration;
struct Ast_Declaration {
	Ast_Declaration_Kind kind;
	
	String   ident;
	Location location;
	
	Ast_Statement *body;
	
	Ast_Declaration *next;
};

internal Ast_Declaration *parse_declaration(Parse_Context *parser);

////////////////////////////////
//~ Context

struct Parse_Context {
	Arena *arena;
	
	String source;
	i64    index;
	
	Token  token;
	
	int error_count;
};

internal void parser_init(Parse_Context *parser, Arena *arena, String source);

internal void report_parse_error(Parse_Context *parser, String message);
internal void report_parse_errorf(Parse_Context *parser, char *format, ...);

internal void expect_token_kind(Parse_Context *parser, Token_Kind kind, char *message);

#endif
