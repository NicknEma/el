#ifndef EL_LEX_H
#define EL_LEX_H

typedef struct Token Token;
typedef struct Lexer Lexer;

typedef enum Token_Kind {
	TOKEN_INVALID = 0,
	
	/*  33 ..  47 */
	
	TOKEN_EMARK     = '!',
	TOKEN_QUOTE     = '"',
	TOKEN_HASH      = '#',
	TOKEN_DOLLAR    = '$',
	TOKEN_PERCENT   = '%',
	TOKEN_AMPER     = '&',
	TOKEN_APEX      = '\'',
	TOKEN_LPAREN    = '(',
	TOKEN_RPAREN    = ')',
	TOKEN_STAR      = '*',
	TOKEN_PLUS      = '+',
	TOKEN_COMMA     = ',',
	TOKEN_DASH      = '-',
	TOKEN_DOT       = '.',
	TOKEN_SLASH     = '/',
	
	/*  58 ..  64 */
	
	TOKEN_COLON     = ':',
	TOKEN_SEMICOLON = ';',
	TOKEN_LTHAN     = '<',
	TOKEN_EQUALS    = '=',
	TOKEN_GTHAN     = '>',
	TOKEN_QMARK     = '?',
	TOKEN_AT        = '@',
	
	/*  91 ..  96 */
	
	TOKEN_LBRACK    = '[',
	TOKEN_BSLASH    = '\\',
	TOKEN_RBRACK    = ']',
	TOKEN_HAT       = '^',
	TOKEN_USCORE    = '_',
	TOKEN_TICK      = '`',
	
	/* 123 .. 126 */
	
	TOKEN_LBRACE    = '{',
	TOKEN_PIPE      = '|',
	TOKEN_RBRACE    = '}',
	TOKEN_TILDE     = '~',
	
	/* 127 .. */
	
	TOKEN_DOUBLE_EQUALS = 127,
	TOKEN_DOUBLE_AMPER,
	TOKEN_DOUBLE_PIPE,
	TOKEN_DOUBLE_DOT,
	
	TOKEN_PLUS_EQUALS,
	TOKEN_DASH_EQUALS,
	TOKEN_STAR_EQUALS,
	TOKEN_SLASH_EQUALS,
	TOKEN_PERCENT_EQUALS,
	TOKEN_EMARK_EQUALS,
	TOKEN_PIPE_EQUALS,
	TOKEN_AMPER_EQUALS,
	TOKEN_TILDE_EQUALS,
	TOKEN_COLON_EQUALS,
	
	TOKEN_LESS_THAN_EQUALS,
	TOKEN_GREATER_THAN_EQUALS,
	
	TOKEN_DOUBLE_COLON,
	TOKEN_TRIPLE_DASH,
	TOKEN_FORWARD_ARROW,
	
	TOKEN_BOOLEAN,
	TOKEN_INTEGER,
	TOKEN_STRING,
	TOKEN_IDENT,
	TOKEN_KEYWORD,
	
	TOKEN_EOI,
	TOKEN_COUNT,
} Token_Kind;

typedef enum Keyword {
	KEYWORD_NONE = 0,
	KEYWORD_PROC,
	KEYWORD_IF,
	KEYWORD_FOR,
	KEYWORD_WHILE,
	KEYWORD_DO,
	KEYWORD_RETURN,
	KEYWORD_STRUCT,
	KEYWORD_BREAK,
	KEYWORD_CONTINUE,
	KEYWORD_COUNT,
} Keyword;

struct Token {
	Token_Kind kind;
	Keyword keyword;
	Range1DI32  loc;
	
	union {
		String string_val;
		i64    int_val;
		bool   bool_val;
	};
};

struct Lexer {
	String source_name;
	
	String source;
	i64    index;
	
	i64 lookahead_index;
	Token  token;
	
	int error_count;
};

global   i64   max_printed_lex_errors = I64_MAX;

internal i64   skip_whitespace_and_comments(String source, i64 index);

internal Token peek_token(Lexer *lexer);
internal Token make_token(Lexer *lexer);
internal void  consume_token(Lexer *lexer);
internal void  consume_all_tokens(Lexer *lexer);

internal void  begin_lookahead(Lexer *lexer);
internal void  end_lookahead(Lexer *lexer);

internal bool  expect_and_consume_token(Lexer *lexer, Token_Kind kind);

internal void  report_lex_error(Lexer *lexer, char *message);
internal void  report_lex_errorf(Lexer *lexer, char *format, ...);

internal String lexeme_from_token(Lexer *lexer, Token token);
internal String lexeme_from_token_or_not_printable(Lexer *lexer, Token token);

#endif
