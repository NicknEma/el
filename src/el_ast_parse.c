#ifndef EL_AST_PARSE_C
#define EL_AST_PARSE_C

////////////////////////////////
//~ Location

internal bool
location_is_valid(Location location) {
	return (location.b0 <= location.b1 && location.l0 <= location.l1 &&
			(location.l0 < location.l1 || location.c0 <= location.c1));
}

internal bool
location_is_zero(Location location) {
	Location zero_location = {0};
	return memcmp(&location, &zero_location, sizeof(Location)) == 0;
}

internal bool
location_is_greater_than(Location location1, Location location2) {
	return location1.b0 > location2.b0;
}

internal Location
locations_merge(Location location1, Location location2) {
	if (location_is_greater_than(location1, location2)) {
		Location t = location1; location1 = location2; location2 = t;
	}
	
	Location result = {
		.l0 = location1.l0,
		.l1 = location2.l1,
		.c0 = location1.c0,
		.c1 = location2.c1,
		.b0 = location1.b0,
		.b1 = location2.b1,
	};
	
	assert(location_is_valid(result));
	return result;
}

////////////////////////////////
//~ Tokens

internal Token
peek_token(Parse_Context *parser) {
	if (parser->index == 0) {
		consume_token(parser);
	}
	return parser->token;
}

internal void
consume_token(Parse_Context *parser) {
	parser->token = make_token(parser);
}

internal void
consume_all_tokens(Parse_Context *parser) {
	for (Token token = peek_token(parser); token.kind != Token_Kind_EOI; token = peek_token(parser)) {
		consume_token(parser);
	}
}

internal bool
expect_and_consume_token(Parse_Context *parser, Token_Kind kind) {
	bool kinds_match = peek_token(parser).kind == kind;
	if (kinds_match) {
		consume_token(parser);
	}
	return kinds_match;
}

// This procedure assumes that its input represents a number (aka. is in the range '0'..'9').
internal i64
i64_from_char(u8 c) {
	return (c - '0');
}

internal Token
make_token(Parse_Context *parser) {
	Token  token  = {0};
	
	String source = parser->source;
	i64    index  = parser->index;
	
	// Skip whitespace
	while (index < source.len && isspace(source.data[index])) {
		index += 1;
	}
	
	token.location.b0 = index;
	
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
		
		{ string_from_lit_const("=="),  Token_Kind_DOUBLE_EQUALS  },
		{ string_from_lit_const("+="),  Token_Kind_PLUS_EQUALS    },
		{ string_from_lit_const("-="),  Token_Kind_DASH_EQUALS    },
		{ string_from_lit_const("*="),  Token_Kind_STAR_EQUALS    },
		{ string_from_lit_const("/="),  Token_Kind_SLASH_EQUALS   },
		{ string_from_lit_const("%="),  Token_Kind_PERCENT_EQUALS },
		{ string_from_lit_const("!="),  Token_Kind_EMARK_EQUALS   },
		{ string_from_lit_const("|="),  Token_Kind_PIPE_EQUALS    },
		{ string_from_lit_const("&="),  Token_Kind_AMPER_EQUALS   },
		{ string_from_lit_const("~="),  Token_Kind_TILDE_EQUALS   },
		{ string_from_lit_const(":="),  Token_Kind_COLON_EQUALS   },
		
		{ string_from_lit_const("::"),  Token_Kind_DOUBLE_COLON   },
		{ string_from_lit_const("---"), Token_Kind_TRIPLE_DASH    },
		{ string_from_lit_const("->"),  Token_Kind_FORWARD_ARROW  },
	};
	
	typedef struct Lexeme_Keyword_Pair Lexeme_Keyword_Pair;
	struct Lexeme_Keyword_Pair { String lexeme; Keyword keyword; };
	
	static read_only Lexeme_Keyword_Pair keyword_pairs[] = {
		{ string_from_lit_const("return"),   Keyword_RETURN   },
		{ string_from_lit_const("proc"),     Keyword_PROC     },
		{ string_from_lit_const("struct"),   Keyword_STRUCT   },
		{ string_from_lit_const("break"),    Keyword_BREAK    },
		{ string_from_lit_const("continue"), Keyword_CONTINUE },
	};
	
	if (index < source.len) {
		
		if (token.kind == Token_Kind_INVALID && isdigit(source.data[index])) {
			i64 value = 0;
			
			while (index < source.len && (isdigit(source.data[index]) || source.data[index] == '_')) {
				if (source.data[index] != '_') {
					i64 digit = i64_from_char(source.data[index]);
					
					value *= 10;
					value += digit;
				}
				
				index += 1;
			}
			
			token.kind        = Token_Kind_INTEGER;
			token.int_val     = value;
			token.location.b1 = index;
		}
		
		if (token.kind == Token_Kind_INVALID) {
			i64    best_match_len = 0;
			Token_Kind best_match = Token_Kind_INVALID;
			
			for (i64 i = 0; i < array_count(misc_pairs); i += 1) {
				if (string_starts_with(string_skip(parser->source, index), misc_pairs[i].lexeme) &&
					misc_pairs[i].lexeme.len > best_match_len) {
					best_match_len = misc_pairs[i].lexeme.len;
					best_match     = misc_pairs[i].kind;
				}
			}
			
			token.kind        = best_match;
			token.location.b1 = index + best_match_len;
			
			index += best_match_len;
		}
		
		if (token.kind == Token_Kind_INVALID) {
			i64 best_match_len = 0;
			Keyword best_match = Keyword_NONE;
			
			for (i64 i = 0; i < array_count(keyword_pairs); i += 1) {
				if (string_starts_with(string_skip(parser->source, index), keyword_pairs[i].lexeme) &&
					keyword_pairs[i].lexeme.len > best_match_len) {
					best_match_len = keyword_pairs[i].lexeme.len;
					best_match     = keyword_pairs[i].keyword;
				}
			}
			
			if (best_match != Keyword_NONE) {
				token.kind        = Token_Kind_KEYWORD;
				token.keyword     = best_match;
				token.location.b1 = index + best_match_len;
				
				index += best_match_len;
			}
		}
		
		if (token.kind == Token_Kind_INVALID && (isalpha(source.data[index]) || source.data[index] == '_')) {
			token.kind = Token_Kind_IDENT;
			
			i64 start = index;
			while (isalpha(source.data[index]) || isdigit(source.data[index]) || source.data[index] == '_') {
				index += 1;
			}
			
			token.location.b1 = index;
		}
		
		if (token.kind == Token_Kind_INVALID) {
			token.location.b1 = index + 1;
			index += 1;
			
			report_lex_errorf(parser, "Invalid token '%.*s'", string_expand(lexeme_from_token_or_not_printable(parser, token)));
		}
		
	} else {
		token.kind = Token_Kind_EOI;
	}
	
	if (token.location.b1 == 0) {
		token.location.b1 = index;
	}
	
	parser->source = source;
	parser->index  = index;
	
	return token;
}

////////////////////////////////
//~ Operators

//- Parsing helpers: Prefix/Infix/Postfix

internal bool
token_is_prefix(Token token) {
	bool result = false;
	switch (token.kind) {
		case Token_Kind_PLUS:
		case Token_Kind_DASH: result = true; break;
		default: break;
	}
	return result;
}

internal bool
token_is_postfix(Token token) {
	bool result = false;
	switch (token.kind) {
		case Token_Kind_HAT: result = true; break;
		default: break;
	}
	return result;
}

internal bool
token_is_infix(Token token) {
	bool result = false;
	if (token.kind == Token_Kind_QMARK || binary_from_token(token) != Binary_Operator_NONE) {
		result = true;
	}
	return result;
}

//- Parsing helpers: Unary/Binary

internal Unary_Operator
unary_from_token(Token token) {
	return unary_from_token_kind(token.kind);
}

internal Unary_Operator
unary_from_token_kind(Token_Kind kind) {
	Unary_Operator unary = Unary_Operator_NONE;
	switch (kind) {
		case Token_Kind_PLUS: { unary = Unary_Operator_PLUS; } break;
		case Token_Kind_DASH: { unary = Unary_Operator_MINUS; } break;
		case Token_Kind_HAT:  { unary = Unary_Operator_DEREFERENCE; } break;
		default: break;
	}
	return unary;
}

internal Binary_Operator
binary_from_token(Token token) {
	return binary_from_token_kind(token.kind);
}

internal Binary_Operator
binary_from_token_kind(Token_Kind kind) {
	Binary_Operator binary = Binary_Operator_NONE;
	switch (kind) {
		case Token_Kind_PLUS:    { binary = Binary_Operator_PLUS; } break;
		case Token_Kind_DASH:    { binary = Binary_Operator_MINUS; } break;
		case Token_Kind_STAR:    { binary = Binary_Operator_TIMES; } break;
		case Token_Kind_SLASH:   { binary = Binary_Operator_DIVIDE; } break;
		case Token_Kind_PERCENT: { binary = Binary_Operator_MODULUS; } break;
		// case Token_Kind_COMMA:   { binary = Binary_Operator_COMMA; } break;
		case Token_Kind_DOT:     { binary = Binary_Operator_MEMBER; } break;
		case Token_Kind_LPAREN:  { binary = Binary_Operator_CALL; } break;
		case Token_Kind_LBRACK:  { binary = Binary_Operator_ARRAY_ACCESS; } break;
		default: break;
	}
	return binary;
}

//- Parsing helpers: Precedence

internal Precedence
infix_precedence_from_token(Token token) {
	Precedence precedence = Precedence_NONE;
	switch (token.kind) {
		// case Token_Kind_COMMA: precedence = Precedence_COMMA; break;
		
		// case Token_Kind_EQUALS: precedence = Precedence_ASSIGNMENT; break;
		
		case Token_Kind_QMARK: precedence = Precedence_TERNARY; break;
		
		// case Token_Kind_LOGICAL_AND: precedence = Precedence_LOGICAL; break;
		
		// case Token_Kind_DOUBLE_EQUALS: precedence = Precedence_RELATIONAL; break;
		
		case Token_Kind_PLUS:
		case Token_Kind_DASH: precedence = Precedence_ADDITIVE; break;
		
		case Token_Kind_STAR:
		case Token_Kind_SLASH:
		case Token_Kind_PERCENT: precedence = Precedence_MULTIPLICATIVE; break;
		
		case Token_Kind_LPAREN: precedence = Precedence_CALL_OR_ARRAY_ACCESS; break;
		
		// case Token_Kind_DOT: precedence = Precedence_MEMBER; break;
		
		default: break;
	}
	return precedence;
}

internal Precedence
prefix_precedence_from_token(Token token) {
	Precedence precedence = Precedence_NONE;
	if (token_is_prefix(token)) {
		precedence = Precedence_UNARY_PREFIX;
	}
	return precedence;
}

internal Precedence
postfix_precedence_from_token(Token token) {
	Precedence precedence = Precedence_NONE;
	if (token_is_postfix(token)) {
		precedence = Precedence_UNARY_POSTFIX;
	}
	return precedence;
}

//- Parsing helpers: Misc

internal bool
token_is_expression_terminator(Token token) {
	Token_Kind k = token.kind;
	return (k == Token_Kind_EOI || k == Token_Kind_RPAREN || k == Token_Kind_RBRACK || k == Token_Kind_RBRACE || k == Token_Kind_SEMICOLON);
}

internal bool
token_is_expression_atom(Token token) {
	Token_Kind k = token.kind;
	return k == Token_Kind_INTEGER || k == Token_Kind_STRING || k == Token_Kind_IDENT;
}

internal bool
token_is_declarator(Token token) {
	Token_Kind k = token.kind;
	return k == ':' || k == Token_Kind_COLON_EQUALS || k == Token_Kind_DOUBLE_COLON;
}

internal bool
token_is_assigner(Token token) {
	Token_Kind k = token.kind;
	return k == '=' || k == Token_Kind_PLUS_EQUALS || k == Token_Kind_DASH_EQUALS || k == Token_Kind_STAR_EQUALS ||
		k == Token_Kind_SLASH_EQUALS || k == Token_Kind_PERCENT_EQUALS || k == Token_Kind_AMPER_EQUALS ||
		k == Token_Kind_EMARK_EQUALS;
}

internal String
lexeme_from_token(Parse_Context *parser, Token token) {
	return string_slice(parser->source, token.location.b0, token.location.b1);
}

internal String
lexeme_from_token_or_not_printable(Parse_Context *parser, Token token) {
	String lexeme = lexeme_from_token(parser, token);
	for (i64 i = 0; i < lexeme.len; i += 1) {
		if (!isprint(lexeme.data[i])) {
			lexeme = string_from_lit("(not printable)");
			break;
		}
	}
	return lexeme;
}

////////////////////////////////
//~ AST

internal Ast_Expression *
ast_expression_alloc(Arena *arena) {
	Ast_Expression *expr = push_type(arena, Ast_Expression);
	memcpy(expr, &nil_expression, sizeof(Ast_Expression));
	
	return expr;
}

internal Ast_Statement *
ast_statement_alloc(Arena *arena) {
	Ast_Statement *stat = push_type(arena, Ast_Statement);
	memcpy(stat, &nil_statement, sizeof(Ast_Statement));
	
	return stat;
}

internal Ast_Declaration *
ast_declaration_alloc(Arena *arena) {
	Ast_Declaration *decl = push_type(arena, Ast_Declaration);
	memcpy(decl, &nil_declaration, sizeof(Ast_Declaration));
	
	return decl;
}

//- Parser: Expressions

internal Ast_Expression *
make_atom_expression(Parse_Context *parser, Token token) {
	Ast_Expression *node = ast_expression_alloc(parser->arena);
	
	if (node != NULL) {
		if (token.kind == Token_Kind_INTEGER) {
			node->kind     = Ast_Expression_Kind_LITERAL;
			node->lexeme   = lexeme_from_token(parser, token);
			node->value    = token.int_val;
			node->location = token.location;
		} else if (token.kind == Token_Kind_IDENT) {
			node->kind     = Ast_Expression_Kind_IDENT;
			node->lexeme   = lexeme_from_token(parser, token);
			node->ident    = node->lexeme;
			node->location = token.location;
		} else { panic(); }
	}
	
	return node;
}

internal Ast_Expression *
make_unary_expression(Parse_Context *parser, Token unary, Ast_Expression *subexpr) {
	Ast_Expression *node = ast_expression_alloc(parser->arena);
	
	if (node != NULL) {
		node->kind     = Ast_Expression_Kind_UNARY;
		node->lexeme   = lexeme_from_token(parser, unary);
		node->unary    = unary_from_token(unary);
		node->subexpr  = subexpr;
		node->location = unary.location;
		node->location = locations_merge(node->location, subexpr->location);
	}
	
	return node;
}

internal Ast_Expression *
make_binary_expression(Parse_Context *parser, Token binary, Ast_Expression *left, Ast_Expression *right) {
	Ast_Expression *node = ast_expression_alloc(parser->arena);
	
	if (node != NULL) {
		node->kind     = Ast_Expression_Kind_BINARY;
		node->lexeme   = lexeme_from_token(parser, binary);
		node->binary   = binary_from_token(binary);
		node->left     = left;
		node->right    = right;
		node->location = binary.location;
		node->location = locations_merge(node->location, left->location);
		if (right != NULL && right != &nil_expression)
			node->location = locations_merge(node->location, right->location);
	}
	
	return node;
}

internal Ast_Expression *
make_ternary_expression(Parse_Context *parser, Ast_Expression *left, Ast_Expression *middle, Ast_Expression *right) {
	Ast_Expression *node = ast_expression_alloc(parser->arena);
	
	if (node != NULL) {
		node->kind     = Ast_Expression_Kind_TERNARY;
		node->lexeme   = string_from_lit("?:");
		node->left     = left;
		node->middle   = middle;
		node->right    = right;
		node->location = left->location;
		node->location = locations_merge(node->location, middle->location);
		node->location = locations_merge(node->location, right->location);
	}
	
	return node;
}

internal Ast_Expression *
parse_expression(Parse_Context *parser, Precedence caller_precedence, bool required) {
	Ast_Expression *left = &nil_expression;
	
	Token token = peek_token(parser);
	if (token_is_expression_atom(token)) {
		consume_token(parser);
		
		left = make_atom_expression(parser, token);
	} else if (token_is_prefix(token)) {
		consume_token(parser);
		Ast_Expression *right = parse_expression(parser, prefix_precedence_from_token(token), true);
		
		left = make_unary_expression(parser, token, right);
	} else if (token.kind == Token_Kind_LPAREN) {
		consume_token(parser);
		Ast_Expression *grouped = parse_expression(parser, Precedence_NONE, true);
		expect_token_kind(parser, Token_Kind_RPAREN, "Expected )");
		
		left = grouped;
	} else {
		if (required) {
			report_parse_error(parser, "Expected an expression");
		}
	}
	
	for (;;) {
		token = peek_token(parser);
		
		if (token_is_postfix(token)) {
			Precedence precedence = postfix_precedence_from_token(token);
			if (precedence < caller_precedence)  break;
			
			consume_token(parser);
			
			left = make_unary_expression(parser, token, left);
		} else if (token_is_infix(token)) {
			Precedence precedence = infix_precedence_from_token(token);
			if (precedence < caller_precedence)  break;
			
			consume_token(parser);
			
			if (token.kind == Token_Kind_QMARK) {
				Ast_Expression *middle = parse_expression(parser, Precedence_NONE, true);
				
				expect_token_kind(parser, Token_Kind_COLON, "Expected :");
				Ast_Expression *right = parse_expression(parser, precedence, true);
				
				left = make_ternary_expression(parser, left, middle, right);
			} else {
				bool subexpr_required = true;
				if (token.kind == Token_Kind_LPAREN || token.kind == Token_Kind_LBRACK) {
					precedence = 0;
					
					if (token.kind == Token_Kind_LPAREN) {
						subexpr_required = false;
					}
				}
				
				Ast_Expression *right = parse_expression(parser, precedence, subexpr_required);
				
				left = make_binary_expression(parser, token, left, right);
				
				if (token.kind == Token_Kind_LPAREN)  expect_token_kind(parser, Token_Kind_RPAREN, "Expected )");
				if (token.kind == Token_Kind_LBRACK)  expect_token_kind(parser, Token_Kind_RBRACK, "Expected ]");
			}
		} else {
			break;
		}
	}
	
	assert(left != NULL);
	return left;
}

//- Parser: Statements

internal Ast_Statement *
make_statement_(Parse_Context *parser, Ast_Statement_Kind kind, Location location, Make_Statement_Params params) {
	Ast_Statement *result = ast_statement_alloc(parser->arena);
	
	if (result != NULL) {
		result->kind     = kind;
		result->location = location;
		result->assigner = params.assigner;
		result->block    = params.block;
		result->lhs      = params.lhs;
		result->rhs      = params.rhs;
		result->decl     = params.decl;
	}
	
	return result;
}

internal Ast_Statement *
parse_statement(Parse_Context *parser) {
	Ast_Statement *result = &nil_statement;
	
	Token token = peek_token(parser);
	if (token.kind == Token_Kind_LBRACE) {
		consume_token(parser); // {
		
		Location lbrace_location = token.location;
		Location rbrace_location = {0};
		
		// Parse statement list inside the braces
		Ast_Statement *first = NULL;
		Ast_Statement *last  = NULL;
		
		token = peek_token(parser);
		if (token.kind != '}') { // Allow empty blocks like {} (without a semicolon in the middle)
			for (;;) {
				Ast_Statement *stat = parse_statement(parser);
				if (stat != &nil_statement) {
					// Avoid writing to read-only memory (*but continue trying,
					// since nil-statements can also mean empty statements and not
					// only failed ones).
					queue_push_nz(first, last, stat, next, check_nil_statement, set_nil_statement);
				}
				
				token = peek_token(parser);
				if (token.kind == Token_Kind_RBRACE) {
					rbrace_location = token.location;
					
					consume_token(parser);
					break;
				} else if (token.kind == Token_Kind_EOI) {
					report_parse_error(parser, "Expected }");
					break;
				}
			}
		} else {
			rbrace_location = token.location;
		}
		
		if (first != NULL) {
			result = make_statement(parser, Ast_Statement_Kind_BLOCK, locations_merge(lbrace_location, rbrace_location),
									.block = first);
		}
	} else if (token.keyword == Keyword_RETURN) {
		consume_token(parser); // return
		
		Location location = token.location;
		
		// Parse return values: either nothing, or an expression list.
		//
		// Do not require an expression to begin with (it could be a "void" return),
		// but require one more expression after each comma.
		Ast_Expression *first = NULL;
		Ast_Expression *last  = NULL;
		
		for (bool is_expr_list = false; ; is_expr_list = true) {
			Ast_Expression *expr = parse_expression(parser, Precedence_NONE, false);
			if (expr == &nil_expression) {
				if (is_expr_list) {
					assert(there_were_parse_errors(parser));
				}
				
				break; // Avoid writing to read-only memory
			}
			
			queue_push_nz(first, last, expr, next, check_nil_expression, set_nil_expression);
			
			token = peek_token(parser);
			if (token.kind != ',') break; // That was the last expr in the list
		}
		
		if (first != NULL) {
			result = make_statement(parser, Ast_Statement_Kind_RETURN, location, .expr = first);
		}
	} else {
		// Do *NOT* consume the first token as it will be part of the first expression/lhs.
		
		Location location = token.location;
		
		Ast_Statement_Kind kind = Ast_Statement_Kind_EXPR;
		
		// Parse an expression list, stopping at any 'assigner' or 'declarator' token,
		// or when there are no more expressions.
		//
		// Do not require an expression to begin with (it could be the empty statement),
		// but require one more expression after each comma.
		Ast_Expression *first = NULL;
		Ast_Expression *last  = NULL;
		i64 count = 0;
		
		for (bool is_expr_list = false; ; is_expr_list = true) {
			Ast_Expression *expr = parse_expression(parser, Precedence_NONE, is_expr_list);
			if (expr == &nil_expression) {
				if (is_expr_list) {
					assert(there_were_parse_errors(parser));
				}
				
				break; // Avoid writing to read-only memory
			}
			
			count += 1;
			queue_push_nz(first, last, expr, next, check_nil_expression, set_nil_expression);
			location = locations_merge(location, expr->location);
			
			token = peek_token(parser);
			if (token_is_assigner(token)) {
				kind = Ast_Statement_Kind_ASSIGNMENT;
				break;
			}
			
			if (token_is_declarator(token)) {
				kind = Ast_Statement_Kind_DECLARATION;
				break;
			}
			
			if (token.kind == ',') {
				consume_token(parser);
			} else {
				break; // That was the last expr in the list
			}
		}
		
		if (kind == Ast_Statement_Kind_EXPR) {
			
			if (first != NULL) {
				result = make_statement(parser, kind, location, .expr = first);
			}
			
		} else if (kind == Ast_Statement_Kind_ASSIGNMENT) {
			
			if (count == 0) {
				report_parse_error(parser, "Cannot assign to nothing. At least one expression must be on the left of the assignment");
			}
			
			Token assigner = peek_token(parser);
			consume_token(parser); // assigner
			
			// Parse assignment right-hand-side, stopping when there are no more expressions.
			//
			// At least one is required, and every comma requires another one after, so
			// they are all required. The list ends when there are no more commas after
			// the expression.
			Ast_Expression *lhs_first = first;
			Ast_Expression *lhs_last  = last;
			i64 lhs_count = count;
			first = NULL;
			last  = NULL;
			count = 0;
			
			for (;;) {
				Ast_Expression *expr = parse_expression(parser, Precedence_NONE, true);
				if (expr == &nil_expression) {
					assert(there_were_parse_errors(parser));
					break; // Avoid writing to read-only memory
				}
				
				count += 1;
				queue_push_nz(first, last, expr, next, check_nil_expression, set_nil_expression);
				
				token = peek_token(parser);
				if (token.kind == ',') {
					consume_token(parser);
				} else {
					break; // That was the last expr in the list
				}
			}
			
			// Do *NOT* report an error if lhs_count != count: it's not the number of expressions
			// that matters, but the number of values. One expression could yield multiple values,
			// e.g. a function call with multiple returns.
			
			if (lhs_first != NULL && first != NULL) {
				result = make_statement(parser, kind, assigner.location, .assigner = assigner.kind,
										.lhs = lhs_first, .rhs = first);
			}
			
		} else if (kind == Ast_Statement_Kind_DECLARATION) {
			
			if (count == 0) {
				report_parse_error(parser, "Cannot declare nothing. At least one identifier must be on the left of the declaration");
			}
			
			// Check that all expressions on the left of the declarator are identifiers
			// (you can only declare identifiers), while also storing them in an array
			// for passing them to parse_declaration_after_lhs().
			Scratch scratch = scratch_begin(&parser->arena, 1);
			
			String *idents  = push_array(scratch.arena, String, count);
			i64 ident_count = 0;
			for (Ast_Expression *expr = first; expr != NULL && expr != &nil_expression; expr = expr->next) {
				if (expr->kind == Ast_Expression_Kind_IDENT) {
					idents[ident_count] = expr->ident;
					ident_count += 1;
				} else {
					report_parse_error(parser, "Only identifiers are allowed on the left of a declaration");
				}
			}
			
			if (count == ident_count) {
				// Do *NOT* consume the declarator as it will be used in parse_declaration_after_lhs().
				
				Token declarator = peek_token(parser);
				assert(token_is_declarator(declarator));
				
				Ast_Declaration *decl = parse_declaration_after_lhs(parser, idents, ident_count);
				result = make_statement(parser, kind, declarator.location, .decl = decl);
			} else {
				assert(there_were_parse_errors(parser));
			}
			
			scratch_end(scratch);
		} else {
			panic("Invalid codepath");
		}
		
		allow_break();
	}
	
	if (result->kind != Ast_Statement_Kind_BLOCK) {
		expect_token_kind(parser, Token_Kind_SEMICOLON, "Expected ; after statement");
	}
	
	if (there_were_parse_errors(parser)) {
		result = &nil_statement; // TODO: @Leak, @Hack
	}
	
	assert(result != NULL && result->next != NULL);
	return result;
}

//- Parser: Declarations

internal Ast_Declaration *
parse_declaration(Parse_Context *parser) {
	Ast_Declaration *result = &nil_declaration;
	Scratch scratch = scratch_begin(&parser->arena, 1);
	
	String_List ident_list = {0};
	
	Token token = {0};
	for (;;) {
		token = peek_token(parser);
		if (token.kind == Token_Kind_IDENT) {
			consume_token(parser); // ident
			
			string_list_push(scratch.arena, &ident_list, lexeme_from_token(parser, token));
			token = peek_token(parser);
		} else {
			report_parse_errorf(parser, "Expected identifier, got '%.*s'", string_expand(lexeme_from_token(parser, token)));
		}
		
		if (token_is_declarator(token)) break;
		if (token.kind != ',') {
			report_parse_errorf(parser, "Unexpected token '%.*s'", string_expand(lexeme_from_token(parser, token)));
			break;
		}
	}
	
	if (token_is_declarator(token)) {
		i64 ident_count = 0;
		String *idents = push_array(scratch.arena, String, ident_list.node_count);
		for (String_Node *node = ident_list.first; node != NULL; node = node->next) {
			idents[ident_count] = node->str;
			ident_count += 1;
		}
		
		result = parse_declaration_after_lhs(parser, idents, ident_count);
	}
	
	if (there_were_parse_errors(parser)) {
		assert(result == &nil_declaration);
	}
	
	scratch_end(scratch);
	return result;
}

internal Ast_Declaration *
parse_declaration_after_lhs(Parse_Context *parser, String *idents, i64 ident_count) {
	Ast_Declaration *result = &nil_declaration;
	
	// Determine the kind of declaration
	bool     parse_initializers = false;
	Type_Ann    type_annotation = {0};
	Ast_Declaration_Flags flags = 0;
	
	Token token = peek_token(parser);
	if (token.kind == ':') {
		consume_token(parser); // :
		
		// ':' alone (NOT ':=' or '::') means there MUST be an explicit type annotation,
		// so we parse that.
		// TODO: For now the only valid type annotations are identifiers.
		
		Token next_token = peek_token(parser);
		if (next_token.kind == Token_Kind_IDENT) {
			consume_token(parser); // type ident
			
			flags |= Ast_Declaration_Flag_TYPE_ANNOTATION;
			type_annotation.ident = lexeme_from_token(parser, next_token);
		} else {
			report_parse_error(parser, "Expected type annotation after :");
		}
		
		next_token = peek_token(parser);
		if (next_token.kind == '=' || next_token.kind == ':') {
			consume_token(parser); // = or :
			parse_initializers = true;
			
			if (next_token.kind == ':') flags |= Ast_Declaration_Flag_CONSTANT;
		}
	} else if (token.kind == Token_Kind_COLON_EQUALS ||
			   token.kind == Token_Kind_DOUBLE_COLON) {
		consume_token(parser); // := or ::
		parse_initializers = true;
		
		if (token.kind == Token_Kind_DOUBLE_COLON) flags |= Ast_Declaration_Flag_CONSTANT;
	} else {
		fprintf(stderr, "Internal error: parse_declaration_after_lhs() expects a declarator to be the first token, got %d\n", token.kind);
	}
	
	if (token_is_declarator(token)) {
		// Build the declaration: either by parsing the initializers,
		// or by looking at the type annotation.
		
		result = ast_declaration_alloc(parser->arena);
		result->flags           = flags;
		result->entity_count    = ident_count;
		result->entities        = push_array(parser->arena, Entity, ident_count);
		result->type_annotation = type_annotation;
		
		for (i64 i = 0; i < ident_count; i += 1) {
			result->entities[i].kind  = Entity_Kind_UNKNOWN;
			result->entities[i].ident = idents[i];
			// result->entities[i].location = ; // TODO.
		}
		
		if (parse_initializers) {
			Scratch scratch = scratch_begin(&parser->arena, 1);
			
			typedef struct Initter_Node Initter_Node;
			struct Initter_Node { Initter initter; Initter_Node *next; };
			Initter_Node *first = NULL;
			Initter_Node *last  = NULL;
			
			i64 count = 0;
			for (;;) {
				Initter initter = parse_declaration_rhs(parser);
				if (initter.kind == Initter_Kind_NONE) {
					assert(there_were_parse_errors(parser));
					break;
				}
				
				Initter_Node *node = push_type(scratch.arena, Initter_Node);
				node->initter = initter;
				
				count += 1;
				queue_push(first, last, node);
				
				token = peek_token(parser);
				if (token.kind == ',') {
					consume_token(parser);
				} else {
					break; // That was the last decl in the list
				}
			}
			
			// Do *NOT* report an error if ident_count != count: it's not the number of initializers
			// that matters, but the number of values. One expression could yield multiple values,
			// e.g. a function call with multiple returns.
			
			result->initter_count = count;
			result->initters      = push_array(parser->arena, Initter, result->entity_count);
			
			i64 i = 0;
			for (Initter_Node *node = first; node != NULL; node = node->next) {
				result->initters[i] = node->initter;
				i += 1;
			}
			
			// TODO: location?
			
			scratch_end(scratch);
		} else {
			assert((flags & Ast_Declaration_Flag_TYPE_ANNOTATION) || there_were_parse_errors(parser));
			
			// The declaration only has a type annotation, without initializers.
			// Make new declaration nodes from the given idents.
			
			// See if we can infer the entity/entities being declared by looking at the
			// type annotation.
			// If we can't (e.g. the annotation is a plain identifier), we'll infer it
			// later during type-checking.
			Entity_Kind entity = Entity_Kind_NULL;
			switch (type_annotation.kind) {
				case Type_Ann_Kind_IDENT: {
					entity = Entity_Kind_UNKNOWN;
				} break;
				
				// TODO: Other kinds of type annotations
				
				default: break;
			}
			
			result->initter_count = 0;
			result->initters      = NULL;
			
			// TODO: location?
		}
	}
	
	if (there_were_parse_errors(parser)) {
		result = &nil_declaration; // TODO: @Leak, @Hack
	}
	
	return result;
}

internal Initter
parse_declaration_rhs(Parse_Context *parser) {
	Initter result = {0};
	
	Token token = peek_token(parser);
	if (token.keyword == Keyword_PROC) {
		// Starting with the keyword 'proc', this is a
		// proc definition OR a proc TYPE definition.
		
		consume_token(parser); // proc
		
		result.kind = Initter_Kind_PROCEDURE_TYPE;
		result.first_param = parse_proc_header(parser);
		
		token = peek_token(parser);
		if (token.kind == Token_Kind_TRIPLE_DASH) {
			// This is a procedure prototype.
			
			result.kind = Initter_Kind_PROCEDURE_PROTO;
		} else if (token.kind == '{') {
			// This is a procedure definition.
			
			result.kind = Initter_Kind_PROCEDURE;
			result.body = parse_statement(parser);
		}
		
	} else if (token.keyword == Keyword_STRUCT) {
		// Starting with the keyword 'struct', this is a
		// struct definition.
		
		consume_token(parser); // struct
		
		result.kind = Initter_Kind_STRUCT;
		// result.initializer.first_member = parse_struct_definition(parser);
		
	} else {
		// Starting in any other way, we treat is as a generic expression,
		// and we figure out later what exactly that is.
		
		result.kind = Initter_Kind_EXPR;
		result.expr = parse_expression(parser, Precedence_NONE, true);
	}
	
	// TODO: 'distinct'
	// [
	// ^
	// 'enum'
	// 'union'
	
	return result;
}

internal Type_Ann
parse_type_annotation(Parse_Context *parser) {
	Type_Ann result = {0};
	
	Token token = peek_token(parser);
	
	// case ident    ->
	// case "proc"   ->
	// case "struct" ->
	// case          -> error
	
	return result;
}

internal Ast_Declaration *
parse_proc_header(Parse_Context *parser) {
	Ast_Declaration *result = &nil_declaration;
	Scratch scratch = scratch_begin(&parser->arena, 1);
	
	// Parse argument list
	
	expect_token_kind(parser, '(', "Expected (");
	
#if 0 // No arguments for now
	Ast_Declaration *first = NULL;
	Ast_Declaration *last  = NULL;
	
	for (;;) {
		String_List arg_names = {0};
		
		Token token = {0};
		for (;;) {
			token = peek_token(parser);
			if (token.kind == Token_Kind_IDENT) {
				consume_token(parser); // ident
				string_list_push(scratch.arena, &arg_names, lexeme_from_token(parser, token));
			} else {
				report_parse_error(parser, "Unexpected token");
			}
			
			token = peek_token(parser);
			if (token.kind == ':' || token.kind == Token_Kind_COLON_EQUALS) break;
			if (token.kind != ',') {
				report_parse_error(parser, "Unexpected token");
				break;
			}
		}
		
		bool parse_default_values = false;
		Type_Ann  type_annotation = {0};
		bool type_annotation_present = false;
		
		if (token.kind == ':') {
			consume_token(parser); // :
			
			token = peek_token(parser);
			if (token.kind == Token_Kind_IDENT) {
				consume_token(parser); // type ident
				
				type_annotation_present = true;
				type_annotation.ident = lexeme_from_token(parser, token);
			} else {
				report_parse_error(parser, "Expected type annotation");
			}
			
			if (token.kind == '=') {
				parse_default_values = true;
			}
		} else if (token.kind == Token_Kind_COLON_EQUALS) {
			parse_default_values = true;
		} else {
			report_parse_error(parser, "Unexpected token");
			break;
		}
		
		if (parse_default_values) {
#if 0
			// TODO: Copy explicit_type into all the args in the list
			// TODO: Copy arg names into the list
			Ast_Declaration *first_arg = parse_declaration_rhs_list(parser);
			if (first_arg != &nil_declaration) {
				queue_push(first, last, first_arg); // TODO: Wrong though... first_arg could have a next ptr
			}
#else
			report_parse_error(parser, "Default values not supported");
#endif
			
		} else {
			assert(type_annotation_present);
			
			for (String_Node *arg_name = arg_names.first; arg_name != NULL; arg_name = arg_name->next) {
				Ast_Declaration *decl = ast_declaration_alloc(parser->arena);
				decl->entity = Ast_Declaration_Entity_UNKNOWN;
				decl->ident  = arg_name->str;
				decl->type_annotation = type_annotation;
				
				queue_push_nz(first, last, decl, next, check_nil_declaration, set_nil_declaration);
			}
			
		}
		
		token = peek_token(parser);
		if (token.kind == ')') break;
		if (token.kind != ',') {
			report_parse_error(parser, "Unexpected token");
			break;
		}
		
		arena_reset(scratch.arena);
	}
	
	if (first != NULL) result = first;
#endif
	
	expect_token_kind(parser, ')', "Expected )");
	
	// Parse return types
	
	Token token = peek_token(parser);
	if (token.kind == Token_Kind_FORWARD_ARROW) {
		consume_token(parser); // ->
		
		String_List ret_types = {0};
		
		for (;;) {
			token = peek_token(parser);
			if (token.kind == Token_Kind_IDENT) {
				consume_token(parser); // type ident
				string_list_push(scratch.arena, &ret_types, lexeme_from_token(parser, token));
			}
			
			token = peek_token(parser);
			if (token.kind != ',') break;
		}
		
		// TODO: Add to explicit_type
	}
	
	// TODO: Eventually parse other annotations
	
	scratch_end(scratch);
	return result;
}

////////////////////////////////
//~ Program

internal Ast_Declaration *
parse_program(Parse_Context *parser) {
	Ast_Declaration *result = &nil_declaration;
	
	Ast_Declaration *first_decl = NULL;
	Ast_Declaration *last_decl  = NULL;
	
	for (Token token = peek_token(parser); token.kind != Token_Kind_EOI; token = peek_token(parser)) {
		Ast_Declaration *decl = parse_declaration(parser);
		if (decl == NULL || decl == &nil_declaration) break;
		
		queue_push_nz(first_decl, last_decl, decl, next, check_nil_declaration, set_nil_declaration);
	}
	
	if (first_decl != NULL) result = first_decl;
	
	consume_all_tokens(parser);
	
	return result;
}

////////////////////////////////
//~ Context

internal void
report_lex_error(Parse_Context *parser, char *message) {
	if (parser->error_count < max_printed_lex_errors) {
		String span = lexeme_from_token(parser, parser->token);
		fprintf(stderr, "Syntax error (%lli..%lli): %s.\n\t%.*s\n\n", parser->token.location.b0, parser->token.location.b1, message, string_expand(span));
	}
	parser->error_count += 1;
}

internal void
report_lex_errorf(Parse_Context *parser, char *format, ...) {
	va_list args;
	va_start(args, format);
	Scratch scratch = scratch_begin(0, 0);
	
	String formatted_message = push_stringf_va_list(scratch.arena, format, args);
	report_lex_error(parser, cstring_from_string(scratch.arena, formatted_message));
	
	scratch_end(scratch);
	va_end(args);
}

internal void
report_parse_error(Parse_Context *parser, char *message) {
	if (parser->error_count < max_printed_parse_errors) {
		String span = lexeme_from_token(parser, parser->token);
		fprintf(stderr, "Syntax error (%lli..%lli): %s.\n\t%.*s\n\n", parser->token.location.b0, parser->token.location.b1, message, string_expand(span));
	}
	parser->error_count += 1;
}

internal void
report_parse_errorf(Parse_Context *parser, char *format, ...) {
	va_list args;
	va_start(args, format);
	Scratch scratch = scratch_begin(0, 0);
	
	String formatted_message = push_stringf_va_list(scratch.arena, format, args);
	report_parse_error(parser, cstring_from_string(scratch.arena, formatted_message));
	
	scratch_end(scratch);
	va_end(args);
}

internal void
parser_init(Parse_Context *parser, Arena *arena, String source) {
	memset(parser, 0, sizeof(*parser));
	parser->arena  = arena;
	parser->source = source;
}

internal void
expect_token_kind(Parse_Context *parser, Token_Kind kind, char *message) {
	if (peek_token(parser).kind != kind) {
		report_parse_error(parser, message);
	}
	consume_token(parser);
}

internal bool
there_were_parse_errors(Parse_Context *parser) {
	return parser->error_count;
}

//- Wrappers

internal Ast_Expression *
parse_expression_string(Arena *arena, String source) {
	Parse_Context context = {0};
	parser_init(&context, arena, source);
	
	return parse_expression(&context, Precedence_NONE, true);
}

internal Ast_Statement *
parse_statement_string(Arena *arena, String source) {
	Parse_Context context = {0};
	parser_init(&context, arena, source);
	
	return parse_statement(&context);
}

internal Ast_Declaration *
parse_declaration_string(Arena *arena, String source) {
	Parse_Context context = {0};
	parser_init(&context, arena, source);
	
	return parse_declaration(&context);
}

internal Ast_Declaration *
parse_program_string(Arena *arena, String source) {
	Parse_Context context = {0};
	parser_init(&context, arena, source);
	
	return parse_program(&context);
}

#endif
