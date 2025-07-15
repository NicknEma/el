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

typedef enum Parse_Expr_Flags {
	Parse_Expr_Flags_REQUIRED             = 1 << 0,
	Parse_Expr_Flags_ALLOW_COMMA          = 1 << 1,
	Parse_Expr_Flags_ALLOW_TRAILING_COMMA = 1 << 2,
} Parse_Expr_Flags;

internal Ast_Expression *make_atom_expression(Parser *parser, Token token);
internal Ast_Expression *make_unary_expression(Parser *parser, Token unary, Ast_Expression *subexpr);
internal Ast_Expression *make_binary_expression(Parser *parser, Token binary, Ast_Expression *left, Ast_Expression *right);
internal Ast_Expression *make_ternary_expression(Parser *parser, Ast_Expression *left, Ast_Expression *middle, Ast_Expression *right);

internal Ast_Expression *parse_expression(Parser *parser, Arena *arena, Precedence caller_precedence, Parse_Expr_Flags parse_flags);

//- Statements

typedef struct Make_Statement_Params Make_Statement_Params;
struct Make_Statement_Params {
	Token_Kind assigner;
	
	Ast_Statement   *block;
	union { Ast_Expression *expr; Ast_Expression *lhs; };
	Ast_Expression  *rhs;
	Ast_Declaration *decl;
};

#define make_statement(parser, kind, location, ...) \
make_statement_(parser, kind, location, (Make_Statement_Params){ \
.assigner = 0,\
.block    = &nil_statement,  \
.lhs      = &nil_expression, \
.rhs      = &nil_expression, \
.decl     = &nil_declaration,\
__VA_ARGS__\
})
internal Ast_Statement *make_statement_(Parser *parser, Ast_Statement_Kind kind, Range1DI32 location, Make_Statement_Params params);

internal Ast_Statement *parse_statement(Parser *parser);

//- Declarations

internal Ast_Declaration *parse_declaration(Parser *parser);
internal Ast_Declaration *parse_declaration_after_lhs(Parser *parser, Ast_Expression *lhs);
internal Ast_Expression  *parse_declaration_rhs(Parser *parser);

internal Ast_Declaration *parse_proc_header(Parser *parser);
internal Type_Ann        *parse_type_annotation(Parser *parser, Arena *arena);

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
