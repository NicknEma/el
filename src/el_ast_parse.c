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
	
	if (index < source.len) {
		switch (source.data[index]) {
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9': {
				i64 value = 0;
				
				while (index < source.len && (isdigit(source.data[index]) || source.data[index] == '_')) {
					if (source.data[index] != '_') {
						i64 digit = i64_from_char(source.data[index]);
						
						value *= 10;
						value += digit;
					}
					
					index += 1;
				}
				
				token.kind = Token_Kind_INTEGER;
				token.int_val = value;
				token.location.b1 = index;
			} break;
			
			case '+': {
				token.kind = Token_Kind_PLUS;
				token.location.b1 = index + 1;
				
				index += 1;
				
				if (index < source.len && source.data[index] == '=') {
					token.kind = Token_Kind_PLUS_EQUALS;
					token.location.b1 = index + 1;
					
					index += 1;
				}
			} break;
			
			case '-': {
				token.kind = Token_Kind_DASH;
				token.location.b1 = index + 1;
				
				index += 1;
				
				if (index < source.len && source.data[index] == '=') {
					token.kind = Token_Kind_DASH_EQUALS;
					token.location.b1 = index + 1;
					
					index += 1;
				} else if (index < source.len && source.data[index] == '>') {
					token.kind = Token_Kind_FORWARD_ARROW;
					token.location.b1 = index + 1;
					
					index += 1;
				} else if (index+1 < source.len && source.data[index] == '-' && source.data[index+1] == '-') {
					token.kind = Token_Kind_TRIPLE_DASH;
					token.location.b1 = index + 2;
					
					index += 2;
				}
			} break;
			
			case '*': {
				token.kind = Token_Kind_STAR;
				token.location.b1 = index + 1;
				
				index += 1;
				
				if (index < source.len && source.data[index] == '=') {
					token.kind = Token_Kind_STAR_EQUALS;
					token.location.b1 = index + 1;
					
					index += 1;
				}
			} break;
			
			case '/': {
				token.kind = Token_Kind_SLASH;
				token.location.b1 = index + 1;
				
				index += 1;
				
				if (index < source.len && source.data[index] == '=') {
					token.kind = Token_Kind_SLASH_EQUALS;
					token.location.b1 = index + 1;
					
					index += 1;
				}
			} break;
			
			case '%': {
				token.kind = Token_Kind_PERCENT;
				token.location.b1 = index + 1;
				
				index += 1;
				
				if (index < source.len && source.data[index] == '=') {
					token.kind = Token_Kind_PERCENT_EQUALS;
					token.location.b1 = index + 1;
					
					index += 1;
				}
			} break;
			
			case ':': {
				token.kind = Token_Kind_COLON;
				token.location.b1 = index + 1;
				
				index += 1;
				
				if (index < source.len && source.data[index] == ':') {
					token.kind = Token_Kind_DOUBLE_COLON;
					token.location.b1 = index + 1;
					
					index += 1;
				}
			} break;
			
			case '?': {
				token.kind = Token_Kind_QMARK;
				token.location.b1 = index + 1;
				
				index += 1;
			} break;
			
			case '(': {
				token.kind = Token_Kind_LPAREN;
				token.location.b1 = index + 1;
				
				index += 1;
			} break;
			
			case ')': {
				token.kind = Token_Kind_RPAREN;
				token.location.b1 = index + 1;
				
				index += 1;
			} break;
			
			case '[': {
				token.kind = Token_Kind_LBRACK;
				token.location.b1 = index + 1;
				
				index += 1;
			} break;
			
			case ']': {
				token.kind = Token_Kind_RBRACK;
				token.location.b1 = index + 1;
				
				index += 1;
			} break;
			
			case '{': {
				token.kind = Token_Kind_LBRACE;
				token.location.b1 = index + 1;
				
				index += 1;
			} break;
			
			case '}': {
				token.kind = Token_Kind_RBRACE;
				token.location.b1 = index + 1;
				
				index += 1;
			} break;
			
			case '.': {
				token.kind = Token_Kind_DOT;
				token.location.b1 = index + 1;
				
				index += 1;
			} break;
			
			case ',': {
				token.kind = Token_Kind_COMMA;
				token.location.b1 = index + 1;
				
				index += 1;
			} break;
			
			case ';': {
				token.kind = Token_Kind_SEMICOLON;
				token.location.b1 = index + 1;
				
				index += 1;
			} break;
			
			case '^': {
				token.kind = Token_Kind_HAT;
				token.location.b1 = index + 1;
				
				index += 1;
			} break;
			
			default: {
				if (isalpha(source.data[index]) || source.data[index] == '_') {
					token.kind = Token_Kind_IDENT;
					
					i64 start = index;
					while (isalpha(source.data[index]) || isdigit(source.data[index]) || source.data[index] == '_') {
						index += 1;
					}
					
					token.location.b1 = index;
					
					String ident = string_slice(source, start, index);
					for (int i = 1; i < array_count(keywords); i += 1) {
						if (string_equals(ident, keywords[i])) {
							token.kind = Token_Kind_KEYWORD;
							token.keyword = i;
							break;
						}
					}
				} else {
					token.kind = Token_Kind_INVALID;
					token.location.b1 = index + 1;
					
					index += 1;
				}
			} break;
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

//- Parser: Ast_Statements

internal Ast_Statement *
parse_statement(Parse_Context *parser) {
	Ast_Statement *result = &nil_statement;
	
	Token token = peek_token(parser);
	if (token.kind == Token_Kind_LBRACE) {
		consume_token(parser); // {
		
		result = ast_statement_alloc(parser->arena);
		result->kind     = Ast_Statement_Kind_BLOCK;
		result->location = token.location;
		
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
					// only that the parse failed).
					queue_push(first, last, stat);
				}
				
				token = peek_token(parser);
				if (token.kind == Token_Kind_RBRACE) {
					consume_token(parser);
					break;
				} else if (token.kind == Token_Kind_EOI) {
					report_parse_error(parser, "Expected }");
					break;
				}
			}
		}
		
		if (first != NULL) {
			result->block    = first;
			result->location = locations_merge(result->location, peek_token(parser).location);
		}
	} else if (token.keyword == Keyword_RETURN) {
		consume_token(parser); // return
		
		result = ast_statement_alloc(parser->arena);
		result->kind     = Ast_Statement_Kind_RETURN;
		result->location = token.location;
		
		// Parse return values: either nothing, or an expression list.
		//
		// Do not require an expression to begin with (it could be a "void" return),
		// but require one more expression after each comma.
		Ast_Expression *first = NULL;
		Ast_Expression *last  = NULL;
		
		// TODO: Good candidate for a do-while loop
		for (bool is_expr_list = false; ; is_expr_list = true) {
			Ast_Expression *expr = parse_expression(parser, Precedence_NONE, false);
			if (expr == &nil_expression) {
				if (is_expr_list) {
					assert(parser->error_count > 0);
				}
				
				break; // Avoid writing to read-only memory
			}
			
			queue_push(first, last, expr);
			result->location = locations_merge(result->location, expr->location);
			
			token = peek_token(parser);
			if (token.kind != ',') break; // That was the last expr in the list
		}
		
		if (first != NULL) result->expr = first;
		
	} else {
		// Do *NOT* consume the first token as it will be part of the first expression/lhs.
		
		result = ast_statement_alloc(parser->arena);
		result->kind     = Ast_Statement_Kind_EXPR;
		result->location = token.location;
		
		// Parse an expression list, stopping at any 'assigner' or 'declarator' token,
		// or when there are no more expressions.
		//
		// Do not require an expression to begin with (it could be the empty statement),
		// but require one more expression after each comma.
		Ast_Expression *first = NULL;
		Ast_Expression *last  = NULL;
		i64 count = 0;
		
		// TODO: Good candidate for a do-while loop
		for (bool is_expr_list = false; ; is_expr_list = true) {
			Ast_Expression *expr = parse_expression(parser, Precedence_NONE, is_expr_list);
			if (expr == &nil_expression) {
				if (is_expr_list) {
					assert(parser->error_count > 0);
				}
				
				break; // Avoid writing to read-only memory
			}
			
			count += 1;
			queue_push(first, last, expr);
			result->location = locations_merge(result->location, expr->location);
			
			token = peek_token(parser);
			if (token.kind != ',') break; // That was the last expr in the list
			
			if (token_is_assigner(token)) {
				result->kind = Ast_Statement_Kind_ASSIGNMENT;
				break;
			}
			
			if (token_is_declarator(token)) {
				result->kind = Ast_Statement_Kind_DECLARATION;
				break;
			}
		}
		
		if (result->kind == Ast_Statement_Kind_EXPR) {
			
			if (first != NULL) result->expr = first;
			
		} else if (result->kind == Ast_Statement_Kind_ASSIGNMENT) {
			
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
			
			// TODO: Good candidate for a do-while loop
			for (;;) {
				Ast_Expression *expr = parse_expression(parser, Precedence_NONE, true);
				if (expr == &nil_expression) {
					assert(parser->error_count > 0);
					break; // Avoid writing to read-only memory
				}
				
				count += 1;
				queue_push(first, last, expr);
				
				token = peek_token(parser);
				if (token.kind != ',') break; // That was the last expr in the list
			}
			
			// Do *NOT* report an error if lhs_count != count: it's not the number of expressions
			// that matters, but the number of values. One expression could yield multiple values,
			// e.g. a function call with multiple returns.
			
			result->lhs      = lhs_first;
			result->rhs      = first;
			result->location = assigner.location;
			result->assigner = assigner.kind;
			
		} else if (result->kind == Ast_Statement_Kind_DECLARATION) {
			
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
					ident_count += 1;
				} else {
					report_parse_error(parser, "Only identifiers are allowed on the left of a declaration");
				}
			}
			
			if (count == ident_count) {
				Token declarator = peek_token(parser);
				consume_token(parser); // declarator
				
				result->kind     = Ast_Statement_Kind_DECLARATION;
				result->decl     = parse_declaration_after_lhs(parser, idents, ident_count);
				result->location = declarator.location;
			} else {
				assert(parser->error_count > 0);
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
	
	return result;
}

//- Parser: Ast_Declarations

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
		} else {
			report_parse_error(parser, "Unexpected token");
		}
		
		token = peek_token(parser);
		if (token_is_declarator(token)) break;
		if (token.kind != ',') {
			report_parse_error(parser, "Unexpected token");
			break;
		}
	}
	
	i64 ident_count = 0;
	String *idents = push_array(scratch.arena, String, ident_list.node_count);
	for (String_Node *node = ident_list.first; node != NULL; node = node->next) {
		idents[ident_count] = node->str;
		ident_count += 1;
	}
	
	result = parse_declaration_after_lhs(parser, idents, ident_count);
	
	scratch_end(scratch);
	return result;
}

internal Ast_Declaration *
parse_declaration_after_lhs(Parse_Context *parser, String *idents, i64 ident_count) {
	Ast_Declaration *result = &nil_declaration;
	
	bool parse_initializers = false;
	bool type_annotation_present = false;
	bool is_constant = false;
	Type_Ann type_annotation = {0};
	
	Token token = peek_token(parser);
	if (token.kind == ':') {
		consume_token(parser); // :
		
		// ':' alone (NOT ':=' or '::') means there MUST be an explicit type annotation,
		// so we parse that.
		// TODO: For now the only valid type annotations are identifiers.
		
		token = peek_token(parser);
		if (token.kind == Token_Kind_IDENT) {
			consume_token(parser); // type ident
			
			type_annotation_present = true;
			type_annotation.ident = lexeme_from_token(parser, token);
		} else {
			report_parse_error(parser, "Expected type annotation");
		}
		
		if (token.kind == '=' || token.kind == ':') {
			consume_token(parser); // = or :
			parse_initializers = true;
			
			if (token.kind == ':') is_constant = true;
		}
	} else if (token.kind == Token_Kind_COLON_EQUALS ||
			   token.kind == Token_Kind_DOUBLE_COLON) {
		consume_token(parser); // := or ::
		parse_initializers = true;
		
		if (token.kind == Token_Kind_DOUBLE_COLON) is_constant = true;
	} else {
		fprintf(stderr, "Internal error, expected declarator\n");
	}
	
	Ast_Declaration *first = NULL;
	Ast_Declaration *last  = NULL;
	
	if (parse_initializers) {
		
		Ast_Declaration_Flags decl_flags = 0;
		if (type_annotation_present) decl_flags |= Ast_Declaration_Flag_TYPE_ANNOTATION;
		if (is_constant) decl_flags |= Ast_Declaration_Flag_CONSTANT;
		
		i64 count = 0;
		for (;;) {
			Ast_Declaration *decl = parse_declaration_rhs(parser, string_from_lit(""));
			if (decl != &nil_declaration) {
				queue_push(first, last, decl);
			}
			
			count += 1;
			
			if (token.kind != ',') break;
		}
		
		if (count == ident_count) {
			Ast_Declaration *decl = first;
			for (i64 ident_index = 0; ident_index < ident_count; ident_index += 1) {
				String ident = idents[ident_index];
				
				decl->ident = ident;
				decl->flags = decl_flags;
				decl->type_annotation = type_annotation;
				
				decl = decl->next;
			}
		} else {
			report_parse_error(parser, "Incorrect number of initializers for declaration");
		}
		
	} else {
		assert(type_annotation_present);
		
		// The declaration only has a type annotation, without initializers.
		// Make new declaration nodes from the given idents.
		
		Ast_Declaration_Flags decl_flags = Ast_Declaration_Flag_TYPE_ANNOTATION;
		if (is_constant) decl_flags |= Ast_Declaration_Flag_CONSTANT;
		
		Ast_Declaration_Entity entity = 0;
		switch (type_annotation.kind) {
			case Type_Ann_Kind_IDENT: {
				entity = Ast_Declaration_Entity_UNKNOWN;
			} break;
			
			default: break;
		}
		
		for (i64 ident_index = 0; ident_index < ident_count; ident_index += 1) {
			String ident = idents[ident_index];
			
			Ast_Declaration *decl = ast_declaration_alloc(parser->arena);
			decl->ident  = ident;
			decl->flags  = decl_flags;
			decl->entity = entity;
			decl->type_annotation = type_annotation;
			
			// TODO: location
			
			queue_push(first, last, decl);
		}
		
	}
	
	if (first != NULL) result = first;
	
	return result;
}

internal Ast_Declaration *
parse_declaration_rhs(Parse_Context *parser, String ident) {
	Ast_Declaration *decl = &nil_declaration;
	
	Token token = peek_token(parser);
	if (token.keyword == Keyword_PROC) {
		// Starting with the keyword 'proc', this is a
		// proc definition OR a proc TYPE definition.
		
		consume_token(parser); // proc
		
		decl = ast_declaration_alloc(parser->arena);
		decl->entity        = Ast_Declaration_Entity_PROCEDURE_TYPE;
		decl->first_param   = parse_proc_header(parser);
		
		token = peek_token(parser);
		
		if (token.kind == Token_Kind_TRIPLE_DASH) {
			// This is a procedure prototype.
			
			decl->entity = Ast_Declaration_Entity_PROCEDURE_PROTO;
		} else if (token.kind == '{') {
			// This is a procedure definition.
			
			decl->entity = Ast_Declaration_Entity_PROCEDURE;
			decl->body   = parse_statement(parser);
		} else {
			report_parse_error(parser, "Unexpected token after proc declaration, did you miss --- ?");
		}
		
	} else if (token.keyword == Keyword_STRUCT) {
		// Starting with the keyword 'struct', this is a
		// struct definition.
		
		consume_token(parser); // struct
		
		decl = ast_declaration_alloc(parser->arena);
		decl->entity        = Ast_Declaration_Entity_STRUCT;
		// decl->first_member  = parse_struct_definition(parser);
		
	} else {
		// start_lookahead(parser);
		// end_lookahead(parser);
		
		decl = ast_declaration_alloc(parser->arena);
		decl->entity      = Ast_Declaration_Entity_UNKNOWN;
		decl->initializer = parse_expression(parser, Precedence_NONE, true);
		
		// TODO: Remember to fixup the decl kind later in case the "expression" is just an ident
		// and that ident is a proc or a type
	}
	
	// TODO: 'distinct'
	// [
	// ^
	// 'enum'
	// 'union'
	
	return decl;
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
	
	Ast_Declaration *first = NULL;
	Ast_Declaration *last  = NULL;
	
	// TODO: Good candidates for do-while loops
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
				
				queue_push(first, last, decl);
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
	
	for (;;) {
		Ast_Declaration *decl = parse_declaration(parser);
		if (decl == NULL || decl == &nil_declaration) break;
		
		queue_push(first_decl, last_decl, decl);
	}
	
	if (first_decl != NULL) result = first_decl;
	
	return result;
}

////////////////////////////////
//~ Context

internal void
report_parse_error(Parse_Context *parser, char *message) {
	if (parser->error_count == 0) {
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

//- Testing

/*
** TODO(ema):
** [ ] Printer
** [ ] Input reading
*/

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

internal void
test_expression_parser(void) {
	printf("### Testing expression parser ###\n\n");
	
	Arena arena = {0};
	arena_init(&arena);
	
	Ast_Expression *tree = &nil_expression;
	
	String expr_1 = string_from_lit("1+2");                    // 3
	String expr_2 = string_from_lit("5 - 4");                  // 1
	String expr_3 = string_from_lit("(3 * 4) - (10 / 2) + 1"); // 8
	String expr_4 = string_from_lit("7 + (-2) - (3 - 1)");     // 3
	String expr_5 = string_from_lit("5 + (+4)");               // 9
	String expr_6 = string_from_lit("1++2");                   // 3
	String expr_7 = string_from_lit("foo()");
	
	String expr_8 = string_from_lit("+");
	String expr_9 = string_from_lit("(4 + 1");
	String expr_0 = string_from_lit("foo(");
	
	arena_reset(&arena);
	printf("Parsing sample expression %.*s:\n", string_expand(expr_1));
	tree = parse_expression_string(&arena, expr_1);
	print_expression_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample expression %.*s:\n", string_expand(expr_2));
	tree = parse_expression_string(&arena, expr_2);
	print_expression_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample expression %.*s:\n", string_expand(expr_3));
	tree = parse_expression_string(&arena, expr_3);
	print_expression_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample expression %.*s:\n", string_expand(expr_4));
	tree = parse_expression_string(&arena, expr_4);
	print_expression_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample expression %.*s:\n", string_expand(expr_5));
	tree = parse_expression_string(&arena, expr_5);
	print_expression_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample expression %.*s:\n", string_expand(expr_6));
	tree = parse_expression_string(&arena, expr_6);
	print_expression_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample expression %.*s:\n", string_expand(expr_7));
	tree = parse_expression_string(&arena, expr_7);
	print_expression_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample expression %.*s:\n", string_expand(expr_8));
	tree = parse_expression_string(&arena, expr_8);
	print_expression_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample expression %.*s:\n", string_expand(expr_9));
	tree = parse_expression_string(&arena, expr_9);
	print_expression_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample expression %.*s:\n", string_expand(expr_0));
	tree = parse_expression_string(&arena, expr_0);
	print_expression_tree(tree);
	
	arena_fini(&arena);
	
	printf("\n");
}

internal void
test_statement_parser(void) {
	printf("### Testing statement parser ###\n\n");
	
	Arena arena = {0};
	arena_init(&arena);
	
	Ast_Statement *tree = &nil_statement;
	
	String stat_1 = string_from_lit_const("1 + 2;");
	String stat_2 = string_from_lit_const("return;");
	String stat_3 = string_from_lit_const("return 0;");
	String stat_4 = string_from_lit_const("return 1 + 2;");
	String stat_5 = string_from_lit_const("return (1 + 2);");
	String stat_6 = string_from_lit_const("{ return; return; }");
	String stat_7 = string_from_lit_const("{ ;; }");
	
	String stat_8 = string_from_lit_const("1 + 2");
	String stat_9 = string_from_lit_const("{ return } ");
	String stat_0 = string_from_lit_const("{ return; ");
	
	arena_reset(&arena);
	printf("Parsing sample statement %.*s:\n", string_expand(stat_1));
	tree = parse_statement_string(&arena, stat_1);
	print_statement_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample statement %.*s:\n", string_expand(stat_2));
	tree = parse_statement_string(&arena, stat_2);
	print_statement_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample statement %.*s:\n", string_expand(stat_3));
	tree = parse_statement_string(&arena, stat_3);
	print_statement_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample statement %.*s:\n", string_expand(stat_4));
	tree = parse_statement_string(&arena, stat_4);
	print_statement_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample statement %.*s:\n", string_expand(stat_5));
	tree = parse_statement_string(&arena, stat_5);
	print_statement_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample statement %.*s:\n", string_expand(stat_6));
	tree = parse_statement_string(&arena, stat_6);
	print_statement_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample statement %.*s:\n", string_expand(stat_7));
	tree = parse_statement_string(&arena, stat_7);
	print_statement_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample statement %.*s:\n", string_expand(stat_8));
	tree = parse_statement_string(&arena, stat_8);
	print_statement_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample statement %.*s:\n", string_expand(stat_9));
	tree = parse_statement_string(&arena, stat_9);
	print_statement_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample statement %.*s:\n", string_expand(stat_0));
	tree = parse_statement_string(&arena, stat_0);
	print_statement_tree(tree);
	
	arena_fini(&arena);
	
	printf("\n");
}

internal void
test_declaration_parser(void) {
	printf("### Testing declaration parser ###\n\n");
	
	Arena arena = {0};
	arena_init(&arena);
	
	Ast_Declaration *tree = &nil_declaration;
	
	String decl_1 = string_from_lit_const("foo :: () { }");
	String decl_2 = string_from_lit_const("foo :: () { ; }");
	String decl_3 = string_from_lit_const("foo :: 4;");
	String decl_4 = string_from_lit_const("foo : : 4;");
	String decl_5 = string_from_lit_const("foo : int;");
#if 0
	String decl_6 = string_from_lit_const("{ return; return; }");
	String decl_7 = string_from_lit_const("{ ;; }");
#endif
	
	String decl_8 = string_from_lit_const("foo :: (");
#if 0
	String decl_9 = string_from_lit_const("{ return } ");
	String decl_0 = string_from_lit_const("{ return; ");
#endif
	
	arena_reset(&arena);
	printf("Parsing sample declaration 1:\n");
	tree = parse_declaration_string(&arena, decl_1);
	print_declaration_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample declaration 2:\n");
	tree = parse_declaration_string(&arena, decl_2);
	print_declaration_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample declaration 3:\n");
	tree = parse_declaration_string(&arena, decl_3);
	print_declaration_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample declaration 4:\n");
	tree = parse_declaration_string(&arena, decl_4);
	print_declaration_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample declaration 5:\n");
	tree = parse_declaration_string(&arena, decl_5);
	print_declaration_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample declaration 8:\n");
	tree = parse_declaration_string(&arena, decl_8);
	print_declaration_tree(tree);
	
	arena_fini(&arena);
	
	printf("\n");
}

#endif
