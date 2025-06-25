#ifndef EL_AST_PARSE_C
#define EL_AST_PARSE_C

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
	
	token.b0 = index;
	
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
				token.b1 = index;
			} break;
			
			case '+': {
				token.kind = Token_Kind_PLUS;
				token.b1 = index + 1;
				
				index += 1;
			} break;
			
			case '-': {
				token.kind = Token_Kind_DASH;
				token.b1 = index + 1;
				
				index += 1;
			} break;
			
			case '*': {
				token.kind = Token_Kind_STAR;
				token.b1 = index + 1;
				
				index += 1;
			} break;
			
			case '/': {
				token.kind = Token_Kind_SLASH;
				token.b1 = index + 1;
				
				index += 1;
			} break;
			
			case '%': {
				token.kind = Token_Kind_PERCENT;
				token.b1 = index + 1;
				
				index += 1;
			} break;
			
			case ':': {
				token.kind = Token_Kind_COLON;
				token.b1 = index + 1;
				
				index += 1;
				
				if (index < source.len && source.data[index] == ':') {
					token.kind = Token_Kind_DOUBLE_COLON;
					token.b1 = index + 1;
					
					index += 1;
				}
			} break;
			
			case '?': {
				token.kind = Token_Kind_QMARK;
				token.b1 = index + 1;
				
				index += 1;
			} break;
			
			case '(': {
				token.kind = Token_Kind_LPAREN;
				token.b1 = index + 1;
				
				index += 1;
			} break;
			
			case ')': {
				token.kind = Token_Kind_RPAREN;
				token.b1 = index + 1;
				
				index += 1;
			} break;
			
			case '[': {
				token.kind = Token_Kind_LBRACK;
				token.b1 = index + 1;
				
				index += 1;
			} break;
			
			case ']': {
				token.kind = Token_Kind_RBRACK;
				token.b1 = index + 1;
				
				index += 1;
			} break;
			
			case '{': {
				token.kind = Token_Kind_LBRACE;
				token.b1 = index + 1;
				
				index += 1;
			} break;
			
			case '}': {
				token.kind = Token_Kind_RBRACE;
				token.b1 = index + 1;
				
				index += 1;
			} break;
			
			case '.': {
				token.kind = Token_Kind_DOT;
				token.b1 = index + 1;
				
				index += 1;
			} break;
			
			case ',': {
				token.kind = Token_Kind_COMMA;
				token.b1 = index + 1;
				
				index += 1;
			} break;
			
			case ';': {
				token.kind = Token_Kind_SEMICOLON;
				token.b1 = index + 1;
				
				index += 1;
			} break;
			
			case '^': {
				token.kind = Token_Kind_HAT;
				token.b1 = index + 1;
				
				index += 1;
			} break;
			
			default: {
				if (isalpha(source.data[index]) || source.data[index] == '_') {
					token.kind = Token_Kind_IDENT;
					
					i64 start = index;
					while (isalpha(source.data[index]) || isdigit(source.data[index]) || source.data[index] == '_') {
						index += 1;
					}
					
					token.b1 = index;
					
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
					token.b1 = index + 1;
					
					index += 1;
				}
			} break;
		}
	} else {
		token.kind = Token_Kind_EOI;
	}
	
	if (token.b1 == 0) {
		token.b1 = index;
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
		case Token_Kind_COMMA:   { binary = Binary_Operator_COMMA; } break;
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
		case Token_Kind_COMMA: precedence = Precedence_COMMA; break;
		
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

internal String
lexeme_from_token(Parse_Context *parser, Token token) {
	return string_slice(parser->source, token.b0, token.b1);
}

////////////////////////////////
//~ Location

internal bool
location_is_zero(Location location) {
	Location zero_location = {0};
	return memcmp(&location, &zero_location, sizeof(Location)) == 0;
}

internal bool
location_is_greater_than(Location location1, Location location2) {
	return location1.b0 > location2.b0;
}

////////////////////////////////
//~ AST

//- Node constructors

internal Ast_Expression *
make_atom_expression(Parse_Context *parser, Token token) {
	Ast_Expression *node = push_type(parser->arena, Ast_Expression);
	
	if (node != NULL) {
		if (token.kind == Token_Kind_INTEGER) {
			node->kind   = Ast_Expression_Kind_LITERAL;
			node->lexeme = lexeme_from_token(parser, token);
			node->value  = token.int_val;
		} else if (token.kind == Token_Kind_IDENT) {
			node->kind   = Ast_Expression_Kind_IDENT;
			node->lexeme = lexeme_from_token(parser, token);
			node->ident  = node->lexeme;
		} else { panic(); }
	}
	
	return node;
}

internal Ast_Expression *
make_unary_expression(Parse_Context *parser, Token unary, Ast_Expression *subexpr) {
	Ast_Expression *node = push_type(parser->arena, Ast_Expression);
	
	if (node != NULL) {
		node->kind    = Ast_Expression_Kind_UNARY;
		node->lexeme  = lexeme_from_token(parser, unary);
		node->unary   = unary_from_token(unary);
		node->subexpr = subexpr;
	}
	
	return node;
}

internal Ast_Expression *
make_binary_expression(Parse_Context *parser, Token binary, Ast_Expression *left, Ast_Expression *right) {
	Ast_Expression *node = push_type(parser->arena, Ast_Expression);
	
	if (node != NULL) {
		node->kind   = Ast_Expression_Kind_BINARY;
		node->lexeme = lexeme_from_token(parser, binary);
		node->binary = binary_from_token(binary);
		node->left   = left;
		node->right  = right;
	}
	
	return node;
}

internal Ast_Expression *
make_ternary_expression(Parse_Context *parser, Ast_Expression *left, Ast_Expression *middle, Ast_Expression *right) {
	Ast_Expression *node = push_type(parser->arena, Ast_Expression);
	
	if (node != NULL) {
		node->kind   = Ast_Expression_Kind_TERNARY;
		node->lexeme = string_from_lit("?:");
		node->left   = left;
		node->middle = middle;
		node->right  = right;
	}
	
	return node;
}

//- Parser: Ast_Expressions

global read_only Ast_Expression nil_expression = {
	.left   = &nil_expression,
	.middle = &nil_expression,
	.right  = &nil_expression,
};

internal Ast_Expression *
parse_expression(Parse_Context *parser, Precedence caller_precedence) {
	Ast_Expression *left = &nil_expression;
	
	Token token = peek_token(parser);
	if (token_is_expression_atom(token)) {
		consume_token(parser);
		
		left = make_atom_expression(parser, token);
	} else if (token_is_prefix(token)) {
		consume_token(parser);
		Ast_Expression *right = parse_expression(parser, prefix_precedence_from_token(token));
		
		left = make_unary_expression(parser, token, right);
	} else if (token.kind == Token_Kind_LPAREN) {
		consume_token(parser);
		Ast_Expression *grouped = parse_expression(parser, Precedence_NONE);
		expect_token_kind(parser, Token_Kind_RPAREN, "Expected )");
		
		left = grouped;
	} else {
		report_parse_error(parser, string_from_lit("Expected an expression"));
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
				Ast_Expression *middle = parse_expression(parser, Precedence_NONE);
				
				expect_token_kind(parser, Token_Kind_COLON, "Expected :");
				Ast_Expression *right = parse_expression(parser, precedence);
				
				left = make_ternary_expression(parser, left, middle, right);
			} else {
				if (token.kind == Token_Kind_LPAREN || token.kind == Token_Kind_LBRACK) {
					precedence = 0;
				}
				
				Ast_Expression *right = parse_expression(parser, precedence);
				
				left = make_binary_expression(parser, token, left, right);
				
				if (token.kind == Token_Kind_LPAREN)  expect_token_kind(parser, Token_Kind_RPAREN, "Expected )");
				if (token.kind == Token_Kind_LBRACK)  expect_token_kind(parser, Token_Kind_RBRACK, "Expected ]");
			}
		} else if (token_is_expression_terminator(token)) {
			break;
		} else {
			report_parse_error(parser, string_from_lit("Unexpected character"));
			consume_token(parser);
		}
	}
	
	return left;
}

//- Parser: Ast_Statements

global read_only Ast_Statement nil_statement = {
	.block = &nil_statement,
	.next  = &nil_statement,
	.expr  = &nil_expression,
};

internal Ast_Statement *
parse_statement(Parse_Context *parser) {
	Ast_Statement *result = &nil_statement;
	
	Token token = peek_token(parser);
	if (token.kind == Token_Kind_LBRACE) {
		consume_token(parser); // {
		
		result = push_type(parser->arena, Ast_Statement);
		result->kind  = Ast_Statement_Kind_BLOCK;
		result->next  = &nil_statement;
		result->block = &nil_statement;
		result->expr  = &nil_expression;
		
		// Parse statement list inside the braces
		Ast_Statement *block_first = NULL;
		Ast_Statement *block_last  = NULL;
		
		for (;;) {
			Ast_Statement *stat = parse_statement(parser);
			if (stat != &nil_statement) { // Otherwise queue_push() will write to read-only memory
				queue_push(block_first, block_last, stat);
			}
			
			token = peek_token(parser);
			if (token.kind == Token_Kind_RBRACE) {
				consume_token(parser);
				break;
			} else if (token.kind == Token_Kind_EOI) {
				report_parse_error(parser, string_from_lit("Expected }"));
				break;
			}
		}
		
		if (block_first != NULL) {
			result->block = block_first;
		}
	} else if (token.keyword == Keyword_RETURN) {
		consume_token(parser); // return
		
		result = push_type(parser->arena, Ast_Statement);
		result->kind  = Ast_Statement_Kind_RETURN;
		result->next  = &nil_statement;
		result->block = &nil_statement;
		result->expr  = &nil_expression;
		
		token = peek_token(parser);
		if (token.kind != Token_Kind_SEMICOLON) {
			result->expr = parse_expression(parser, Precedence_NONE);
		}
	} else if (token.kind == Token_Kind_SEMICOLON) {
		;
	} else {
		result = push_type(parser->arena, Ast_Statement);
		result->kind  = Ast_Statement_Kind_EXPR;
		result->next  = &nil_statement;
		result->block = &nil_statement;
		result->expr  = parse_expression(parser, Precedence_NONE);
	}
	
	if (result->kind != Ast_Statement_Kind_BLOCK) {
		expect_token_kind(parser, Token_Kind_SEMICOLON, "Expected ; after statement");
	}
	
	return result;
}

//- Parser: Ast_Declarations

global read_only Ast_Declaration nil_declaration = {
	.next = &nil_declaration,
	.body = &nil_statement,
};

////////////////////////////////
//~ Context

internal void
report_parse_error(Parse_Context *parser, String message) {
	if (parser->error_count == 0) {
		fprintf(stderr, "Error [%lli..%lli]: %.*s.\n", parser->token.b0, parser->token.b1, string_expand(message));
	}
	parser->error_count += 1;
}

internal void
report_parse_errorf(Parse_Context *parser, char *format, ...) {
	va_list args;
	va_start(args, format);
	Scratch scratch = scratch_begin(0, 0);
	
	String formatted_message = push_stringf_va_list(scratch.arena, format, args);
	report_parse_error(parser, formatted_message);
	
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
		report_parse_error(parser, string_from_cstring(message));
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
	
	return parse_expression(&context, Precedence_NONE);
}

internal Ast_Statement *
parse_statement_string(Arena *arena, String source) {
	Parse_Context context = {0};
	parser_init(&context, arena, source);
	
	return parse_statement(&context);
}

internal void
test_expression_parser(void) {
	Arena arena = {0};
	arena_init(&arena);
	
	Ast_Expression *tree = &nil_expression;
	
	String expr_1 = string_from_lit("1+2");                    // 3
	String expr_2 = string_from_lit("5 - 4");                  // 1
	String expr_3 = string_from_lit("(3 * 4) - (10 / 2) + 1"); // 8
	String expr_4 = string_from_lit("7 + (-2) - (3 - 1)");     // 3
	String expr_5 = string_from_lit("100_000 * 2");            // 200000
	String expr_6 = string_from_lit("5 + (+4)");               // 9
	String expr_7 = string_from_lit("1++2");                   // 3
	
	String expr_8 = string_from_lit("1 2");
	String expr_9 = string_from_lit("2 (4 + 1)");
	String expr_0 = string_from_lit("3 * (2 (4 + 1))");
	
	arena_reset(&arena);
	printf("Parsing sample expression 1:\n");
	tree = parse_expression_string(&arena, expr_1);
	print_expression_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample expression 2:\n");
	tree = parse_expression_string(&arena, expr_2);
	print_expression_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample expression 3:\n");
	tree = parse_expression_string(&arena, expr_3);
	print_expression_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample expression 4:\n");
	tree = parse_expression_string(&arena, expr_4);
	print_expression_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample expression 5:\n");
	tree = parse_expression_string(&arena, expr_5);
	print_expression_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample expression 6:\n");
	tree = parse_expression_string(&arena, expr_6);
	print_expression_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample expression 7:\n");
	tree = parse_expression_string(&arena, expr_7);
	print_expression_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample expression 8:\n");
	tree = parse_expression_string(&arena, expr_8);
	print_expression_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample expression 9:\n");
	tree = parse_expression_string(&arena, expr_9);
	print_expression_tree(tree);
	
	arena_reset(&arena);
	printf("Parsing sample expression 0:\n");
	tree = parse_expression_string(&arena, expr_0);
	print_expression_tree(tree);
	
	arena_fini(&arena);
}

internal void
test_statement_parser(void) {
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
	printf("Parsing sample statement 1:\n");
	tree = parse_statement_string(&arena, stat_1);
	
	arena_reset(&arena);
	printf("Parsing sample statement 2:\n");
	tree = parse_statement_string(&arena, stat_2);
	
	arena_reset(&arena);
	printf("Parsing sample statement 3:\n");
	tree = parse_statement_string(&arena, stat_3);
	
	arena_reset(&arena);
	printf("Parsing sample statement 4:\n");
	tree = parse_statement_string(&arena, stat_4);
	
	arena_reset(&arena);
	printf("Parsing sample statement 5:\n");
	tree = parse_statement_string(&arena, stat_5);
	
	arena_reset(&arena);
	printf("Parsing sample statement 6:\n");
	tree = parse_statement_string(&arena, stat_6);
	
	arena_reset(&arena);
	printf("Parsing sample statement 7:\n");
	tree = parse_statement_string(&arena, stat_7);
	
	arena_reset(&arena);
	printf("Parsing sample statement 8:\n");
	tree = parse_statement_string(&arena, stat_8);
	
	arena_reset(&arena);
	printf("Parsing sample statement 9:\n");
	tree = parse_statement_string(&arena, stat_9);
	
	arena_reset(&arena);
	printf("Parsing sample statement 0:\n");
	tree = parse_statement_string(&arena, stat_0);
	
	arena_fini(&arena);
}

internal Ast_Declaration *
hardcode_a_declaration(Arena *arena) {
	Ast_Declaration *main_decl = push_type(arena, Ast_Declaration);
	main_decl->kind = Ast_Declaration_Kind_PROCEDURE;
	main_decl->ident = string_from_lit("main");
	main_decl->body = parse_statement_string(arena, string_from_lit("{ return other(); }"));
	
	{
		main_decl->next = push_type(arena, Ast_Declaration);
		
		Ast_Declaration *next_decl = main_decl->next;
		next_decl->kind = Ast_Declaration_Kind_PROCEDURE;
		next_decl->ident = string_from_lit("other");
		next_decl->body = parse_statement_string(arena, string_from_lit("{ return 2*3 + 10/(4+1); 7-0; }"));
	}
	
	return main_decl;
}

#endif
