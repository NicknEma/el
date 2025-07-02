#ifndef EL_PARSE_H
#define EL_PARSE_H

//- Parser

typedef struct Parser Parser;
struct Parser {
	Arena *arena;
	Lexer *lexer;
	
	int error_count;
};

//- Expression helpers

typedef enum Precedence {
	PREC_NONE = 0,
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

internal Ast_Expression *make_atom_expression(Parser *parser, Token token);
internal Ast_Expression *make_unary_expression(Parser *parser, Token unary, Ast_Expression *subexpr);
internal Ast_Expression *make_binary_expression(Parser *parser, Token binary, Ast_Expression *left, Ast_Expression *right);
internal Ast_Expression *make_ternary_expression(Parser *parser, Ast_Expression *left, Ast_Expression *middle, Ast_Expression *right);

internal Ast_Expression *parse_expression(Parser *parser, Precedence caller_precedence, bool required);

internal String string_from_expression_tree(Arena *arena, Ast_Expression *root);
internal void print_expression_tree(Ast_Expression *root);

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

internal String string_from_statement_tree(Arena *arena, Ast_Statement *root);
internal void print_statement_tree(Ast_Statement *root);

//- Declarations

internal Ast_Declaration *parse_declaration(Parser *parser);
internal Ast_Declaration *parse_declaration_after_lhs(Parser *parser, String *idents, Range1DI32 *ident_locations, i64 ident_count);
internal Initter          parse_declaration_rhs(Parser *parser);

internal Ast_Declaration *parse_proc_header(Parser *parser);
internal Type_Ann         parse_type_annotation(Parser *parser);

internal String string_from_declaration_tree(Arena *arena, Ast_Declaration *root);
internal void print_declaration_tree(Ast_Declaration *root);

//- General/errors

global   i64  max_printed_parse_errors = 1;

internal void parser_init(Parser *parser, Arena *arena, String source);

internal void report_parse_error(Parser *parser, char *message);
internal void report_parse_errorf(Parser *parser, char *format, ...);

internal void expect_token_kind(Parser *parser, Token_Kind kind, char *message);

internal bool there_were_parse_errors(Parser *parser);

#endif
