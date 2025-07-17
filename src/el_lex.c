#ifndef EL_LEX_C
#define EL_LEX_C

internal Token peek_token(Lexer *lexer) {
	if (lexer->index == 0)
		consume_token(lexer);
	return lexer->token;
}

internal void consume_token(Lexer *lexer) {
	lexer->token = make_token(lexer);
}

internal void consume_all_tokens(Lexer *lexer) {
	for (Token token = peek_token(lexer); token.kind != TOKEN_EOI; token = peek_token(lexer))
		consume_token(lexer);
}

internal bool expect_and_consume_token(Lexer *lexer, Token_Kind kind) {
	bool kinds_match = peek_token(lexer).kind == kind;
	if (kinds_match)
		consume_token(lexer);
	return kinds_match;
}

internal i64 skip_whitespace_and_comments(String source, i64 index) {
	bool should_continue = true;
	while (should_continue) {
		should_continue = false;
		while (index < source.len && isspace(source.data[index])) {
			should_continue = true;
			index += 1;
		}
		
		// TODO: Comments
	}
	
	return index;
}

internal Token make_token(Lexer *lexer) {
	Token  token  = {0};
	
	String source = lexer->source;
	i64    index  = skip_whitespace_and_comments(source, lexer->index);
	
	token.loc.start = index;
	
	// TODO: A more data-oriented way to store these pairs would be
	// to use parallel arrays.
	
	typedef struct Lexeme_Token_Pair Lexeme_Token_Pair;
	struct Lexeme_Token_Pair { String lexeme; Token_Kind kind; };
	
	static read_only Lexeme_Token_Pair misc_pairs[] = {
		{ string_from_lit_const("!"),  '!',  },
		{ string_from_lit_const("\""), '"',  },
		{ string_from_lit_const("#"),  '#',  },
		{ string_from_lit_const("$"),  '$',  },
		{ string_from_lit_const("%"),  '%',  },
		{ string_from_lit_const("&"),  '&',  },
		{ string_from_lit_const("'"),  '\'', },
		{ string_from_lit_const("("),  '(',  },
		{ string_from_lit_const(")"),  ')',  },
		{ string_from_lit_const("*"),  '*',  },
		{ string_from_lit_const("+"),  '+',  },
		{ string_from_lit_const(","),  ',',  },
		{ string_from_lit_const("-"),  '-',  },
		{ string_from_lit_const("."),  '.',  },
		{ string_from_lit_const("/"),  '/',  },
		
		{ string_from_lit_const(":"),  ':',  },
		{ string_from_lit_const(";"),  ';',  },
		{ string_from_lit_const("<"),  '<',  },
		{ string_from_lit_const("="),  '=',  },
		{ string_from_lit_const(">"),  '>',  },
		{ string_from_lit_const("?"),  '?',  },
		{ string_from_lit_const("@"),  '@',  },
		
		{ string_from_lit_const("["),  '[',  },
		{ string_from_lit_const("\\"), '\\', },
		{ string_from_lit_const("]"),  ']',  },
		{ string_from_lit_const("^"),  '^',  },
		{ string_from_lit_const("_"),  '_',  },
		{ string_from_lit_const("`"),  '`',  },
		
		{ string_from_lit_const("{"),  '{',  },
		{ string_from_lit_const("|"),  '|',  },
		{ string_from_lit_const("}"),  '}',  },
		{ string_from_lit_const("~"),  '~',  },
		
		{ string_from_lit_const("=="),  TOKEN_DOUBLE_EQUALS  },
		{ string_from_lit_const("+="),  TOKEN_PLUS_EQUALS    },
		{ string_from_lit_const("-="),  TOKEN_DASH_EQUALS    },
		{ string_from_lit_const("*="),  TOKEN_STAR_EQUALS    },
		{ string_from_lit_const("/="),  TOKEN_SLASH_EQUALS   },
		{ string_from_lit_const("%="),  TOKEN_PERCENT_EQUALS },
		{ string_from_lit_const("!="),  TOKEN_EMARK_EQUALS   },
		{ string_from_lit_const("|="),  TOKEN_PIPE_EQUALS    },
		{ string_from_lit_const("&="),  TOKEN_AMPER_EQUALS   },
		{ string_from_lit_const("~="),  TOKEN_TILDE_EQUALS   },
		{ string_from_lit_const(":="),  TOKEN_COLON_EQUALS   },
		
		{ string_from_lit_const("::"),  TOKEN_DOUBLE_COLON   },
		{ string_from_lit_const("---"), TOKEN_TRIPLE_DASH    },
		{ string_from_lit_const("->"),  TOKEN_FORWARD_ARROW  },
	};
	
	typedef struct Lexeme_KEYWORD_Pair Lexeme_KEYWORD_Pair;
	struct Lexeme_KEYWORD_Pair { String lexeme; Keyword keyword; };
	
	static read_only Lexeme_KEYWORD_Pair keyword_pairs[] = {
		{ string_from_lit_const("return"),   KEYWORD_RETURN   },
		{ string_from_lit_const("proc"),     KEYWORD_PROC     },
		{ string_from_lit_const("struct"),   KEYWORD_STRUCT   },
		{ string_from_lit_const("break"),    KEYWORD_BREAK    },
		{ string_from_lit_const("continue"), KEYWORD_CONTINUE },
	};
	
	if (index < source.len) {
		if (token.kind == TOKEN_INVALID && isdigit(source.data[index])) {
			i64 value = 0;
			
			while (index < source.len && (isdigit(source.data[index]) || source.data[index] == '_')) {
				if (source.data[index] != '_') {
					i64 digit = source.data[index] - '0';
					
					value *= 10;
					value += digit;
				}
				
				index += 1;
			}
			
			token.kind    = TOKEN_INTEGER;
			token.int_val = value;
			token.loc.end = index;
		}
		
		if (token.kind == TOKEN_INVALID && source.data[index] == '"') {
			token.kind = TOKEN_STRING;
			
			index += 1;
			while (index < source.len && !(source.data[index] == '"' && source.data[index-1] != '\\')) {
				index += 1;
			}
			index += 1;
			
			token.string_val = string_slice(source, token.loc.start+1, index-1);
			token.loc.end    = index;
		}
		
		if (token.kind == TOKEN_INVALID) {
			i64    best_match_len = 0;
			Token_Kind best_match = TOKEN_INVALID;
			
			for (i64 i = 0; i < array_count(misc_pairs); i += 1) {
				if (string_starts_with(string_skip(source, index), misc_pairs[i].lexeme) &&
					misc_pairs[i].lexeme.len > best_match_len) {
					best_match_len = misc_pairs[i].lexeme.len;
					best_match     = misc_pairs[i].kind;
				}
			}
			
			token.kind    = best_match;
			token.loc.end = index + best_match_len;
			
			index += best_match_len;
		}
		
		if (token.kind == TOKEN_INVALID) {
			i64 best_match_len = 0;
			Keyword best_match = KEYWORD_NONE;
			
			for (i64 i = 0; i < array_count(keyword_pairs); i += 1) {
				if (string_starts_with(string_skip(source, index), keyword_pairs[i].lexeme) &&
					keyword_pairs[i].lexeme.len > best_match_len) {
					best_match_len = keyword_pairs[i].lexeme.len;
					best_match     = keyword_pairs[i].keyword;
				}
			}
			
			if (best_match != KEYWORD_NONE) {
				token.kind    = TOKEN_KEYWORD;
				token.keyword = best_match;
				token.loc.end = index + best_match_len;
				
				index += best_match_len;
			}
		}
		
		if (token.kind == TOKEN_INVALID) {
			if (string_starts_with(string_skip(source, index), string_from_lit("true"))) {
				token.kind     = TOKEN_BOOLEAN;
				token.loc.end  = index + 4;
				token.bool_val = true;
				
				index += 4;
			}
			
			if (string_starts_with(string_skip(source, index), string_from_lit("false"))) {
				token.kind     = TOKEN_BOOLEAN;
				token.loc.end  = index + 5;
				token.bool_val = false;
				
				index += 5;
			}
		}
		
		if (token.kind == TOKEN_INVALID && (isalpha(source.data[index]) || source.data[index] == '_')) {
			token.kind = TOKEN_IDENT;
			
			while (isalpha(source.data[index]) || isdigit(source.data[index]) || source.data[index] == '_') {
				index += 1;
			}
			
			token.loc.end = index;
		}
		
		if (token.kind == TOKEN_INVALID) {
			token.loc.end = index + 1;
			index += 1;
			
			report_lex_errorf(lexer, "Invalid token '%.*s'", string_expand(lexeme_from_token_or_not_printable(lexer, token)));
		}
		
	} else {
		token.kind = TOKEN_EOI;
	}
	
	if (token.loc.end == 0) {
		token.loc.end = index;
	}
	
	lexer->source = source;
	lexer->index  = index;
	
	return token;
}

internal void begin_lookahead(Lexer *lexer) {
	Token token = peek_token(lexer);
	lexer->lookahead_index = token.loc.start;
}

internal void end_lookahead(Lexer *lexer) {
	assert(lexer->index >= lexer->lookahead_index, "Lexer index went *back* during lookahead, somehow");
	lexer->index = lexer->lookahead_index;
}

internal void report_lex_error(Lexer *lexer, char *message) {
	if (lexer->error_count < max_printed_lex_errors) {
		// String span = lexeme_from_token(lexer, lexer->token);
		fprintf(stderr, "Syntax error (%i..%i): %s.\n", lexer->token.loc.start, lexer->token.loc.end, message);
	}
	lexer->error_count += 1;
}

internal void report_lex_errorf(Lexer *lexer, char *format, ...) {
	if (lexer->error_count < max_printed_lex_errors) {
		va_list args;
		va_start(args, format);
		Scratch scratch = scratch_begin(0, 0);
		
		String formatted_message = push_stringf_va_list(scratch.arena, format, args);
		report_lex_error(lexer, cstring_from_string(scratch.arena, formatted_message));
		
		scratch_end(scratch);
		va_end(args);
	} else {
		lexer->error_count += 1;
	}
}

internal String lexeme_from_token(Lexer *lexer, Token token) {
	return string_slice(lexer->source, token.loc.start, token.loc.end);
}

internal String lexeme_from_token_or_not_printable(Lexer *lexer, Token token) {
	String lexeme = lexeme_from_token(lexer, token);
	for (i64 i = 0; i < lexeme.len; i += 1) {
		if (!isprint(lexeme.data[i])) {
			lexeme = string_from_lit("(not printable)");
			break;
		}
	}
	return lexeme;
}

#endif
