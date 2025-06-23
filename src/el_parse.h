#ifndef EL_PARSE_H
#define EL_PARSE_H

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
	// int l0, l1, c0, c1;
	i64 b0, b1; // Byte range
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

////////////////////////////////
//~ AST

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

//- Expressions

typedef enum Expression_Kind {
	Expression_Kind_NULL = 0,
	Expression_Kind_LITERAL,
	Expression_Kind_IDENT,
	Expression_Kind_UNARY,
	Expression_Kind_BINARY,
	Expression_Kind_TERNARY,
	Expression_Kind_COUNT,
} Expression_Kind;

// TODO: Add a Runtime_Type field
typedef struct Expression Expression;
struct Expression {
	Expression_Kind kind;
	Unary_Operator  unary;
	Binary_Operator binary;
	
	union { Expression *left; Expression *subexpr; };
	Expression *middle; // For ternaries
	Expression *right;
	
	String lexeme;
	String ident;
	i64    value;
	
	void  *user;
};

internal Expression *make_atom_expression(Parse_Context *parser, Token token);
internal Expression *make_unary_expression(Parse_Context *parser, Token unary, Expression *subexpr);
internal Expression *make_binary_expression(Parse_Context *parser, Token binary, Expression *left, Expression *right);
internal Expression *make_ternary_expression(Parse_Context *parser, Expression *left, Expression *middle, Expression *right);

internal Expression *parse_expression(Parse_Context *parser, Precedence caller_precedence);

//- Statements

typedef enum Statement_Kind {
	Statement_Kind_NULL = 0,
	Statement_Kind_EXPR,
	Statement_Kind_BLOCK,
	Statement_Kind_RETURN,
	Statement_Kind_COUNT,
} Statement_Kind;

typedef struct Statement Statement;
struct Statement {
	Statement_Kind kind;
	Statement     *next;
	Statement     *block;
	
	Expression *expr;
};

internal Statement *parse_statement(Parse_Context *parser);

//- Declarations

typedef enum Declaration_Kind {
	Declaration_Kind_PROCEDURE,
	Declaration_Kind_COUNT,
} Declaration_Kind;

typedef struct Declaration Declaration;
struct Declaration {
	Declaration_Kind kind;
	Declaration     *next;
	String           ident;
	
	Statement *body;
};

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
