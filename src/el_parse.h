#ifndef EL_PARSE_H
#define EL_PARSE_H

//- Parser

typedef struct Parser Parser;
struct Parser {
	// The arena is stored as a pointer because its lifetime should extend beyound the one of the
	// parser. Its purpose is to be a "default allocator" for AST nodes, so the memory shall
	// live for the entire compilation process and not just for the parsing phase.
	Arena *arena;
	
	Lexer  lexer;
	int error_count;
};

//- Expression helpers

typedef enum Precedence {
	PREC_NONE = 0,
	
	// The comma "operator" is not the same as the C comma operator; it is just a way
	// to encode a list of expressions without requiring an additional *next member
	// and additional code paths.  :CommaOperator
	// PREC_COMMA,
	// PREC_ASSIGNMENT,
	PREC_TERNARY,
	PREC_LOGICAL,
	PREC_RELATIONAL,
	PREC_ADDITIVE,
	PREC_MULTIPLICATIVE,
	PREC_UNARY_PREFIX,
	PREC_UNARY_POSTFIX,
	PREC_CALL_OR_ARRAY_ACCESS,
	PREC_MEMBER,
	PREC_COUNT,
} Precedence;

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

//- Expressions

// There's only 1 type of parse flags, valid for all parsing routines. The flags that do not apply to that
// node kind are simply ignored. This allows for flags to be passed down eterogeneous routines without
// weird context shenanigans.
typedef enum Parse_Flags {
	// expr flags
	Parse_Flags_EXPR_REQUIRED                  = 1 << 0,
	Parse_Flags_EXPR_ALLOW_ASSIGNMENT          = 1 << 1,
	
	// expr lists flags
	Parse_Flags_EXPR_LIST_ALLOW_TRAILING_COMMA = 1 << 2,
	
	// useful combinations
	Parse_Flags_EXPR_DEFAULT = Parse_Flags_EXPR_REQUIRED,
	Parse_Flags_RETURN_VALS  = 0,
	Parse_Flags_CALL_ARGS    = Parse_Flags_EXPR_LIST_ALLOW_TRAILING_COMMA|Parse_Flags_EXPR_ALLOW_ASSIGNMENT,
	Parse_Flags_COMPOUND_LIT = Parse_Flags_EXPR_LIST_ALLOW_TRAILING_COMMA|Parse_Flags_EXPR_ALLOW_ASSIGNMENT,
} Parse_Flags;

internal Ast_Expression *make_atom_expression(Parser *parser, Token token);
internal Ast_Expression *make_unary_expression(Parser *parser, Token unary, Ast_Expression *subexpr);
internal Ast_Expression *make_binary_expression(Parser *parser, Token binary, Ast_Expression *left, Ast_Expression *right);
internal Ast_Expression *make_ternary_expression(Parser *parser, Ast_Expression *left, Ast_Expression *middle, Ast_Expression *right);

internal Ast_Expression *parse_expression(Parser *parser, Precedence caller_precedence, Parse_Flags parse_flags);

//- Statements

typedef struct Stmt_List Stmt_List;
struct Stmt_List {
	Ast_Statement *first, *last;
};

internal Ast_Statement *make_expr_stmt(Parser *parser, Ast_Expression *expr, Range1DI32 loc);
internal Ast_Statement *make_block_stmt(Parser *parser, Stmt_List block, Range1DI32 loc);
internal Ast_Statement *make_return_stmt(Parser *parser, Expr_List retvals, Range1DI32 loc);
internal Ast_Statement *make_assignment_stmt(Parser *parser, Expr_List lhs, Expr_List rhs, Token_Kind assigner, Range1DI32 loc);
internal Ast_Statement *make_decl_stmt(Parser *parser, Ast_Declaration *decl, Range1DI32 loc);

internal Ast_Statement *parse_statement(Parser *parser);

//- Declarations

internal Ast_Declaration *parse_declaration(Parser *parser);
internal Ast_Declaration *parse_declaration_after_lhs(Parser *parser, Ast_Expression *lhs);
internal Ast_Expression  *parse_declaration_rhs(Parser *parser);

internal Ast_Declaration *parse_proc_header(Parser *parser);

//- General/errors

global   i64  max_printed_parse_errors = 1;

internal void report_parse_error(Parser *parser, char *message);
internal void report_parse_errorf(Parser *parser, char *format, ...);

internal void expect_token_kind(Parser *parser, Token_Kind kind, char *message);

internal bool there_were_parse_errors(Parser *parser);

typedef struct Parser_Init_Params Parser_Init_Params;
struct Parser_Init_Params { String text; String file_name; };

internal void parser_init_(Parser *parser, Arena *ast_arena, Parser_Init_Params init_params);
#define parser_init(parser, arena, ...) \
parser_init_(parser, arena, (Parser_Init_Params){ .text = {0}, .file_name = {0}, __VA_ARGS__ });

internal void parser_set_source(Parser *parser, String source);
internal u64  estimate_ast_arena_size(String source);

#endif
