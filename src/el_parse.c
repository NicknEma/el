#ifndef EL_PARSE_C
#define EL_PARSE_C

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
//~ AST

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

//- Node constructors

internal Expression *
make_atom_expression(Parse_Context *parser, Token token) {
	Expression *node = push_type(parser->arena, Expression);
	
	if (node != NULL) {
		if (token.kind == Token_Kind_INTEGER) {
			node->kind   = Expression_Kind_LITERAL;
			node->lexeme = lexeme_from_token(parser, token);
			node->value  = token.int_val;
		} else if (token.kind == Token_Kind_IDENT) {
			node->kind   = Expression_Kind_IDENT;
			node->lexeme = lexeme_from_token(parser, token);
			node->ident  = node->lexeme;
		} else { panic(); }
	}
	
	return node;
}

internal Expression *
make_unary_expression(Parse_Context *parser, Token unary, Expression *subexpr) {
	Expression *node = push_type(parser->arena, Expression);
	
	if (node != NULL) {
		node->kind    = Expression_Kind_UNARY;
		node->lexeme  = lexeme_from_token(parser, unary);
		node->unary   = unary_from_token(unary);
		node->subexpr = subexpr;
	}
	
	return node;
}

internal Expression *
make_binary_expression(Parse_Context *parser, Token binary, Expression *left, Expression *right) {
	Expression *node = push_type(parser->arena, Expression);
	
	if (node != NULL) {
		node->kind   = Expression_Kind_BINARY;
		node->lexeme = lexeme_from_token(parser, binary);
		node->binary = binary_from_token(binary);
		node->left   = left;
		node->right  = right;
	}
	
	return node;
}

internal Expression *
make_ternary_expression(Parse_Context *parser, Expression *left, Expression *middle, Expression *right) {
	Expression *node = push_type(parser->arena, Expression);
	
	if (node != NULL) {
		node->kind   = Expression_Kind_TERNARY;
		node->lexeme = string_from_lit("?:");
		node->left   = left;
		node->middle = middle;
		node->right  = right;
	}
	
	return node;
}

//- Parser: Expressions

global read_only Expression nil_expression = {
	.left   = &nil_expression,
	.middle = &nil_expression,
	.right  = &nil_expression,
};

internal Expression *
parse_expression(Parse_Context *parser, Precedence caller_precedence) {
	Expression *left = &nil_expression;
	
	Token token = peek_token(parser);
	if (token_is_expression_atom(token)) {
		consume_token(parser);
		
		left = make_atom_expression(parser, token);
	} else if (token_is_prefix(token)) {
		consume_token(parser);
		Expression *right = parse_expression(parser, prefix_precedence_from_token(token));
		
		left = make_unary_expression(parser, token, right);
	} else if (token.kind == Token_Kind_LPAREN) {
		consume_token(parser);
		Expression *grouped = parse_expression(parser, Precedence_NONE);
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
				Expression *middle = parse_expression(parser, Precedence_NONE);
				
				expect_token_kind(parser, Token_Kind_COLON, "Expected :");
				Expression *right = parse_expression(parser, precedence);
				
				left = make_ternary_expression(parser, left, middle, right);
			} else {
				if (token.kind == Token_Kind_LPAREN || token.kind == Token_Kind_LBRACK) {
					precedence = 0;
				}
				
				Expression *right = parse_expression(parser, precedence);
				
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

//- Parser: Statements

global read_only Statement nil_statement = {
	.block = &nil_statement,
	.next  = &nil_statement,
	.expr  = &nil_expression,
};

internal Statement *
parse_statement(Parse_Context *parser) {
	Statement *result = &nil_statement;
	
	Token token = peek_token(parser);
	if (token.kind == Token_Kind_LBRACE) {
		consume_token(parser); // {
		
		result = push_type(parser->arena, Statement);
		result->kind  = Statement_Kind_BLOCK;
		result->next  = &nil_statement;
		result->block = &nil_statement;
		result->expr  = &nil_expression;
		
		// Parse statement list inside the braces
		Statement *block_first = NULL;
		Statement *block_last  = NULL;
		
		for (;;) {
			Statement *stat = parse_statement(parser);
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
		
		result = push_type(parser->arena, Statement);
		result->kind  = Statement_Kind_RETURN;
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
		result = push_type(parser->arena, Statement);
		result->kind  = Statement_Kind_EXPR;
		result->next  = &nil_statement;
		result->block = &nil_statement;
		result->expr  = parse_expression(parser, Precedence_NONE);
	}
	
	if (result->kind != Statement_Kind_BLOCK) {
		expect_token_kind(parser, Token_Kind_SEMICOLON, "Expected ; after statement");
	}
	
	return result;
}

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

char *sample_expression_1 = "1+2";                    // 3
char *sample_expression_2 = "5 - 4";                  // 1
char *sample_expression_3 = "(3 * 4) - (10 / 2) + 1"; // 8
char *sample_expression_4 = "7 + (-2) - (3 - 1)";     // 3
char *sample_expression_5 = "100_000 * 2";            // 200000
char *sample_expression_6 = "5 + (+4)";               // 9
char *sample_expression_7 = "1++2";                   // 3

char *sample_expression_8 = "1 2";
char *sample_expression_9 = "2 (4 + 1)";
char *sample_expression_0 = "3 * (2 (4 + 1))";

internal void
test_sample_expressions(void) {
	Scratch scratch = scratch_begin(0, 0);
	
	Arena arena = {0};
	arena_init(&arena);
	
	Parse_Context parser = {0};
	Expression *sample_program_tree = NULL;
	
	arena_reset(&arena);
	parser_init(&parser, &arena, string_from_cstring(sample_expression_1));
	sample_program_tree = parse_expression(&parser, Precedence_NONE);
	
	printf("%.*s\n", string_expand(string_from_expression_tree(scratch.arena, sample_program_tree)));
	
	arena_reset(&arena);
	parser_init(&parser, &arena, string_from_cstring(sample_expression_2));
	sample_program_tree = parse_expression(&parser, Precedence_NONE);
	
	printf("%.*s\n", string_expand(string_from_expression_tree(scratch.arena, sample_program_tree)));
	
	arena_reset(&arena);
	parser_init(&parser, &arena, string_from_cstring(sample_expression_3));
	sample_program_tree = parse_expression(&parser, Precedence_NONE);
	
	printf("%.*s\n", string_expand(string_from_expression_tree(scratch.arena, sample_program_tree)));
	
	arena_reset(&arena);
	parser_init(&parser, &arena, string_from_cstring(sample_expression_4));
	sample_program_tree = parse_expression(&parser, Precedence_NONE);
	
	printf("%.*s\n", string_expand(string_from_expression_tree(scratch.arena, sample_program_tree)));
	
	arena_reset(&arena);
	parser_init(&parser, &arena, string_from_cstring(sample_expression_5));
	sample_program_tree = parse_expression(&parser, Precedence_NONE);
	
	printf("%.*s\n", string_expand(string_from_expression_tree(scratch.arena, sample_program_tree)));
	
	arena_reset(&arena);
	parser_init(&parser, &arena, string_from_cstring(sample_expression_6));
	sample_program_tree = parse_expression(&parser, Precedence_NONE);
	
	printf("%.*s\n", string_expand(string_from_expression_tree(scratch.arena, sample_program_tree)));
	
	arena_reset(&arena);
	parser_init(&parser, &arena, string_from_cstring(sample_expression_7));
	sample_program_tree = parse_expression(&parser, Precedence_NONE);
	
	printf("%.*s\n", string_expand(string_from_expression_tree(scratch.arena, sample_program_tree)));
	
	arena_reset(&arena);
	parser_init(&parser, &arena, string_from_cstring(sample_expression_8));
	sample_program_tree = parse_expression(&parser, Precedence_NONE);
	
	printf("%.*s\n", string_expand(string_from_expression_tree(scratch.arena, sample_program_tree)));
	
	arena_reset(&arena);
	parser_init(&parser, &arena, string_from_cstring(sample_expression_9));
	sample_program_tree = parse_expression(&parser, Precedence_NONE);
	
	printf("%.*s\n", string_expand(string_from_expression_tree(scratch.arena, sample_program_tree)));
	
	arena_fini(&arena);
	
	scratch_end(scratch);
}

internal Expression *
parse_expression_string(Arena *arena, String source) {
	Parse_Context context = {0};
	parser_init(&context, arena, source);
	
	return parse_expression(&context, 0);
}

String sample_statement_1 = string_from_lit_const("1 + 2;");
String sample_statement_2 = string_from_lit_const("return;");
String sample_statement_3 = string_from_lit_const("return 0;");
String sample_statement_4 = string_from_lit_const("return 1 + 2;");
String sample_statement_5 = string_from_lit_const("return (1 + 2);");
String sample_statement_6 = string_from_lit_const("{ return; return; }");
String sample_statement_7 = string_from_lit_const("{ ;; }");

String sample_statement_8 = string_from_lit_const("1 + 2");
String sample_statement_9 = string_from_lit_const("{ return } ");
String sample_statement_0 = string_from_lit_const("{ return; ");

internal void
test_sample_statements(void) {
	Arena arena = {0};
	arena_init(&arena);
	
	Parse_Context parser = {0};
	Statement *sample_program_tree = NULL;
	
	arena_reset(&arena);
	parser_init(&parser, &arena, sample_statement_1);
	sample_program_tree = parse_statement(&parser);
	
	arena_reset(&arena);
	parser_init(&parser, &arena, sample_statement_2);
	sample_program_tree = parse_statement(&parser);
	
	arena_reset(&arena);
	parser_init(&parser, &arena, sample_statement_3);
	sample_program_tree = parse_statement(&parser);
	
	arena_reset(&arena);
	parser_init(&parser, &arena, sample_statement_4);
	sample_program_tree = parse_statement(&parser);
	
	arena_reset(&arena);
	parser_init(&parser, &arena, sample_statement_5);
	sample_program_tree = parse_statement(&parser);
	
	arena_reset(&arena);
	parser_init(&parser, &arena, sample_statement_6);
	sample_program_tree = parse_statement(&parser);
	
	arena_reset(&arena);
	parser_init(&parser, &arena, sample_statement_7);
	sample_program_tree = parse_statement(&parser);
	
	arena_reset(&arena);
	parser_init(&parser, &arena, sample_statement_8);
	sample_program_tree = parse_statement(&parser);
	
	arena_reset(&arena);
	parser_init(&parser, &arena, sample_statement_9);
	sample_program_tree = parse_statement(&parser);
	
	arena_reset(&arena);
	parser_init(&parser, &arena, sample_statement_0);
	sample_program_tree = parse_statement(&parser);
	
	arena_fini(&arena);
}

internal Statement *
parse_statement_string(Arena *arena, String source) {
	Parse_Context context = {0};
	parser_init(&context, arena, source);
	
	return parse_statement(&context);
}

internal Declaration *
hardcode_a_declaration(Arena *arena) {
	Declaration *main_decl = push_type(arena, Declaration);
	main_decl->kind = Declaration_Kind_PROCEDURE;
	main_decl->ident = string_from_lit("main");
	main_decl->body = parse_statement_string(arena, string_from_lit("{ return other(); }"));
	
	{
		main_decl->next = push_type(arena, Declaration);
		
		Declaration *next_decl = main_decl->next;
		next_decl->kind = Declaration_Kind_PROCEDURE;
		next_decl->ident = string_from_lit("other");
		next_decl->body = parse_statement_string(arena, string_from_lit("{ return 2*3 + 10/(4+1); 7-0; }"));
	}
	
	return main_decl;
}

#endif
