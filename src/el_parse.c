#ifndef EL_PARSE_C
#define EL_PARSE_C

////////////////////////////////
//~ Tokens

static Token
peek_token(Parse_Context *parse_context) {
	if (parse_context->source_index == 0) {
		consume_token(parse_context);
	}
	return parse_context->token;
}

static void
consume_token(Parse_Context *parse_context) {
	parse_context->token = make_token(parse_context);
}

static bool
expect_and_consume_token(Parse_Context *parse_context, Token_Kind kind) {
	bool kinds_match = peek_token(parse_context).kind == kind;
	if (kinds_match) {
		consume_token(parse_context);
	}
	return kinds_match;
}

// This procedure assumes that its input represents a number (aka. is in the range '0'..'9').
static i64
i64_from_char(u8 c) {
	return (c - '0');
}

static Token
make_token(Parse_Context *parse_context) {
	Token token;
	
	// Skip whitespace
	while (parse_context->source_index < parse_context->source.len && isspace(parse_context->source.data[parse_context->source_index])) {
		parse_context->source_index += 1;
	}
	
	token.b0 = parse_context->source_index;
	
	if (parse_context->source_index < parse_context->source.len) {
		switch (parse_context->source.data[parse_context->source_index]) {
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
				i64 index = parse_context->source_index;
				
				while (index < parse_context->source.len && (isdigit(parse_context->source.data[index]) || parse_context->source.data[index] == '_')) {
					if (parse_context->source.data[index] != '_') {
						i64 digit = i64_from_char(parse_context->source.data[index]);
						
						value *= 10;
						value += digit;
					}
					
					index += 1;
				}
				
				token.kind = Token_Kind_INTEGER;
				token.int_val = value;
				token.b1 = index;
				
				parse_context->source_index = index;
			} break;
			
			case '+': {
				token.kind = Token_Kind_PLUS;
				token.b1 = parse_context->source_index + 1;
				
				parse_context->source_index += 1;
			} break;
			
			case '-': {
				token.kind = Token_Kind_DASH;
				token.b1 = parse_context->source_index + 1;
				
				parse_context->source_index += 1;
			} break;
			
			case '*': {
				token.kind = Token_Kind_STAR;
				token.b1 = parse_context->source_index + 1;
				
				parse_context->source_index += 1;
			} break;
			
			case '/': {
				token.kind = Token_Kind_SLASH;
				token.b1 = parse_context->source_index + 1;
				
				parse_context->source_index += 1;
			} break;
			
			case '%': {
				token.kind = Token_Kind_PERCENT;
				token.b1 = parse_context->source_index + 1;
				
				parse_context->source_index += 1;
			} break;
			
			case ':': {
				token.kind = Token_Kind_COLON;
				token.b1 = parse_context->source_index + 1;
				
				parse_context->source_index += 1;
				
				if (parse_context->source_index < parse_context->source.len && parse_context->source.data[parse_context->source_index] == ':') {
					token.kind = Token_Kind_DOUBLE_COLON;
					token.b1 = parse_context->source_index + 1;
					
					parse_context->source_index += 1;
				}
			} break;
			
			case '?': {
				token.kind = Token_Kind_QMARK;
				token.b1 = parse_context->source_index + 1;
				
				parse_context->source_index += 1;
			} break;
			
			case '(': {
				token.kind = Token_Kind_LPAREN;
				token.b1 = parse_context->source_index + 1;
				
				parse_context->source_index += 1;
			} break;
			
			case ')': {
				token.kind = Token_Kind_RPAREN;
				token.b1 = parse_context->source_index + 1;
				
				parse_context->source_index += 1;
			} break;
			
			case '[': {
				token.kind = Token_Kind_LBRACK;
				token.b1 = parse_context->source_index + 1;
				
				parse_context->source_index += 1;
			} break;
			
			case ']': {
				token.kind = Token_Kind_RBRACK;
				token.b1 = parse_context->source_index + 1;
				
				parse_context->source_index += 1;
			} break;
			
			case '{': {
				token.kind = Token_Kind_LBRACE;
				token.b1 = parse_context->source_index + 1;
				
				parse_context->source_index += 1;
			} break;
			
			case '}': {
				token.kind = Token_Kind_RBRACE;
				token.b1 = parse_context->source_index + 1;
				
				parse_context->source_index += 1;
			} break;
			
			case '.': {
				token.kind = Token_Kind_DOT;
				token.b1 = parse_context->source_index + 1;
				
				parse_context->source_index += 1;
			} break;
			
			case ',': {
				token.kind = Token_Kind_COMMA;
				token.b1 = parse_context->source_index + 1;
				
				parse_context->source_index += 1;
			} break;
			
			case ';': {
				token.kind = Token_Kind_SEMICOLON;
				token.b1 = parse_context->source_index + 1;
				
				parse_context->source_index += 1;
			} break;
			
			case '^': {
				token.kind = Token_Kind_HAT;
				token.b1 = parse_context->source_index + 1;
				
				parse_context->source_index += 1;
			} break;
			
			default: {
				token.kind = Token_Kind_INVALID;
				token.b1 = parse_context->source_index + 1;
				
				parse_context->source_index += 1;
			} break;
		}
	} else {
		token.kind = Token_Kind_EOI;
	}
	
	if (token.b1 == 0) {
		token.b1 = parse_context->source_index;
	}
	
	token.lexeme = string_slice(parse_context->source, token.b0, token.b1);
	
	return token;
}

////////////////////////////////
//~ AST

//- Parsing helpers: Prefix/Infix/Postfix

static bool
token_is_prefix(Token token) {
	bool result = false;
	switch (token.kind) {
		case Token_Kind_PLUS:
		case Token_Kind_DASH: result = true; break;
		default: break;
	}
	return result;
}

static bool
token_is_postfix(Token token) {
	bool result = false;
	switch (token.kind) {
		case Token_Kind_HAT: result = true; break;
		default: break;
	}
	return result;
}

static bool
token_is_infix(Token token) {
	bool result = false;
	if (token.kind == Token_Kind_QMARK || binary_from_token(token) != Binary_Operator_NONE) {
		result = true;
	}
	return result;
}

//- Parsing helpers: Unary/Binary

static Unary_Operator
unary_from_token(Token token) {
	return unary_from_token_kind(token.kind);
}

static Unary_Operator
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

static Binary_Operator
binary_from_token(Token token) {
	return binary_from_token_kind(token.kind);
}

static Binary_Operator
binary_from_token_kind(Token_Kind kind) {
	Binary_Operator binary = Binary_Operator_NONE;
	switch (kind) {
		case Token_Kind_PLUS:    { binary = Binary_Operator_PLUS; } break;
		case Token_Kind_DASH:    { binary = Binary_Operator_MINUS; } break;
		case Token_Kind_STAR:    { binary = Binary_Operator_TIMES; } break;
		case Token_Kind_SLASH:   { binary = Binary_Operator_DIVIDE; } break;
		case Token_Kind_PERCENT: { binary = Binary_Operator_MODULUS; } break;
		case Token_Kind_COMMA: { binary = Binary_Operator_COMMA; } break;
		case Token_Kind_DOT: { binary = Binary_Operator_MEMBER; } break;
		case Token_Kind_LPAREN:  { binary = Binary_Operator_CALL; } break;
		case Token_Kind_LBRACK:  { binary = Binary_Operator_ARRAY_ACCESS; } break;
		default: break;
	}
	return binary;
}

//- Parsing helpers: Precedence

static Precedence
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

static Precedence
prefix_precedence_from_token(Token token) {
	Precedence precedence = Precedence_NONE;
	if (token_is_prefix(token)) {
		precedence = Precedence_UNARY_PREFIX;
	}
	return precedence;
}

static Precedence
postfix_precedence_from_token(Token token) {
	Precedence precedence = Precedence_NONE;
	if (token_is_postfix(token)) {
		precedence = Precedence_UNARY_POSTFIX;
	}
	return precedence;
}

//- Parsing helpers: Misc

static bool
token_is_expression_terminator(Token token) {
	Token_Kind k = token.kind;
	return (k == Token_Kind_EOI || k == Token_Kind_RPAREN || k == Token_Kind_RBRACK || k == Token_Kind_SEMICOLON);
}

static bool
token_is_expression_atom(Token token) {
	Token_Kind k = token.kind;
	return k == Token_Kind_INTEGER || k == Token_Kind_STRING || k == Token_Kind_IDENT;
}

//- Node constructors

static Expression *
make_atom_expression(Parse_Context *parse_context, Token token) {
	Expression *node = push_type(parse_context->arena, Expression);
	
	if (node != NULL) {
		node->kind   = Expression_Kind_LITERAL;
		node->lexeme = token.lexeme;
		node->value  = token.int_val;
	}
	
	return node;
}

static Expression *
make_unary_expression(Parse_Context *parse_context, Token unary, Expression *subexpr) {
	Expression *node = push_type(parse_context->arena, Expression);
	
	if (node != NULL) {
		node->kind   = Expression_Kind_UNARY;
		node->lexeme = unary.lexeme;
		node->unary  = unary_from_token(unary);
		node->right  = subexpr;
	}
	
	return node;
}

static Expression *
make_binary_expression(Parse_Context *parse_context, Token binary, Expression *left, Expression *right) {
	Expression *node = push_type(parse_context->arena, Expression);
	
	if (node != NULL) {
		node->kind   = Expression_Kind_BINARY;
		node->lexeme = binary.lexeme;
		node->binary = binary_from_token(binary);
		node->left   = left;
		node->right  = right;
	}
	
	return node;
}

static Expression *
make_ternary_expression(Parse_Context *parse_context, Expression *left, Expression *middle, Expression *right) {
	Expression *node = push_type(parse_context->arena, Expression);
	
	if (node != NULL) {
		node->kind   = Expression_Kind_TERNARY;
		node->lexeme = string_from_lit("?:");
		node->left   = left;
		node->middle = middle;
		node->right  = right;
	}
	
	return node;
}

//- Parser

static read_only Expression nil_expression = {
	.left = &nil_expression,
	.middle = &nil_expression,
	.right = &nil_expression,
};

static Expression *
parse_expression(Parse_Context *parse_context, Precedence caller_precedence) {
	Expression *left = &nil_expression;
	
	Token token = peek_token(parse_context);
	if (token_is_expression_atom(token)) {
		consume_token(parse_context);
		
		left = make_atom_expression(parse_context, token);
	} else if (token_is_prefix(token)) {
		consume_token(parse_context);
		Expression *right = parse_expression(parse_context, prefix_precedence_from_token(token));
		
		left = make_unary_expression(parse_context, token, right);
	} else if (token.kind == Token_Kind_LPAREN) {
		consume_token(parse_context);
		Expression *grouped = parse_expression(parse_context, Precedence_NONE);
		expect_token_kind(parse_context, Token_Kind_RPAREN, "Expected )");
		
		left = grouped;
	} else {
		report_parse_error(parse_context, string_from_lit("Expected an expression"));
	}
	
	for (;;) {
		token = peek_token(parse_context);
		
		if (token_is_postfix(token)) {
			Precedence precedence = postfix_precedence_from_token(token);
			if (precedence < caller_precedence)  break;
			
			consume_token(parse_context);
			
			left = make_unary_expression(parse_context, token, left);
		} else if (token_is_infix(token)) {
			Precedence precedence = infix_precedence_from_token(token);
			if (precedence < caller_precedence)  break;
			
			consume_token(parse_context);
			
			if (token.kind == Token_Kind_QMARK) {
				Expression *middle = parse_expression(parse_context, Precedence_NONE);
				
				expect_token_kind(parse_context, Token_Kind_COLON, "Expected :");
				Expression *right = parse_expression(parse_context, precedence);
				
				left = make_ternary_expression(parse_context, left, middle, right);
			} else {
				if (token.kind == Token_Kind_LPAREN || token.kind == Token_Kind_LBRACK) {
					precedence = 0;
				}
				
				Expression *right = parse_expression(parse_context, precedence);
				
				left = make_binary_expression(parse_context, token, left, right);
				
				if (token.kind == Token_Kind_LPAREN)  expect_token_kind(parse_context, Token_Kind_RPAREN, "Expected )");
				if (token.kind == Token_Kind_LBRACK)  expect_token_kind(parse_context, Token_Kind_RBRACK, "Expected ]");
			}
		} else if (token_is_expression_terminator(token)) {
			break;
		} else {
			report_parse_error(parse_context, string_from_lit("Unexpected character"));
			consume_token(parse_context);
		}
	}
	
	return left;
}

////////////////////////////////
//~ Context

static void
report_parse_error(Parse_Context *parse_context, String message) {
	if (parse_context->error_count == 0) {
		fprintf(stderr, "Error [%lli..%lli]: %.*s.\n", parse_context->token.b0, parse_context->token.b1, string_expand(message));
	}
	parse_context->error_count += 1;
}

static void
report_parse_errorf(Parse_Context *parse_context, char *format, ...) {
	va_list args;
	va_start(args, format);
	Scratch scratch = scratch_begin(0, 0);
	
	String formatted_message = push_stringf_va_list(scratch.arena, format, args);
	report_parse_error(parse_context, formatted_message);
	
	scratch_end(scratch);
	va_end(args);
}

static void
parse_context_init(Parse_Context *parse_context, Arena *arena, String source) {
	memset(parse_context, 0, sizeof(*parse_context));
	parse_context->arena  = arena;
	parse_context->source = source;
}

static void
expect_token_kind(Parse_Context *parse_context, Token_Kind kind, char *message) {
	if (peek_token(parse_context).kind != kind) {
		report_parse_error(parse_context, string_from_cstring(message));
	}
	consume_token(parse_context);
}

//- Tree printing

static int
i64_digit_count(i64 x) {
	int count = 0;
	
	if (x != 0) {
		while (x != 0) {
			count += 1;
			x /= 10;
		}
	} else {
		count = 1;
	}
	
	return count;
}

#if 0
static int
compute_expression_tree_width(Expression *root) {
	assert(root != NULL);
	
	int root_width = 0;
	
	if (root->kind == Expression_Kind_LITERAL) {
		root_width  = i64_digit_count(root->value);
	} else if (root->kind == Expression_Kind_IDENT) {
		root_width  = root->ident.len;
	} else if (root->kind == Expression_Kind_UNARY) {
		root_width  = root->token.len;
		
		root_width += compute_expression_tree_width(root->right);
		root_width += 1;
	} else if (root->kind == Expression_Kind_BINARY) {
		root_width  = root->token.len;
		
		root_width += compute_expression_tree_width(root->left);
		root_width += compute_expression_tree_width(root->right);
		root_width += 2;
	}
	
	return root_width;
}
#endif

static void
print_spaces(int count) {
	for (int i = 0; i < count; i += 1) putchar(' ');
}

static void
print_expression_tree(Expression *root) {
	Scratch scratch = scratch_begin(0, 0);
	
	typedef struct Expr_Node Expr_Node;
	struct Expr_Node {
		Expression *expr;
		Expr_Node *next, *prev;
		Expr_Node *left, *middle, *right;
		int self_width, left_width, middle_width, right_width;
		
		int left_sibling_width, parent_width;
		int level;
	};
	
	Expr_Node *root_node = push_type(scratch.arena, Expr_Node);
	root_node->expr  = root;
	root_node->level = 0;
	
	{
		// Compute widths: Depth-first, post-order
		// https://en.wikipedia.org/wiki/Tree_traversal
		// NOTE: The pseudocode on Wikipedia is written using a pre-conditional loop instead of
		// a post-conditional like here, but given the initial conditions we know that 1
		// iteration is always going to happen... I think.
		
		Expr_Node *stack_first = NULL;
		Expr_Node *last_node_visited = NULL;
		Expr_Node *node = root_node;
		for (;;) {
			if (node->expr != NULL && node->expr != &nil_expression) {
				stack_push(stack_first, node);
				
				// node = node->left;
				{
					assert(node->left == NULL);
					node->left = push_type(scratch.arena, Expr_Node);
					node->left->expr = node->expr->left;
					
					node = node->left;
				}
			} else {
				Expr_Node *peek_node = stack_first;
				
				// If right child exists and traversing node
				// from left child, then move right
				if ((peek_node->expr->right != NULL && peek_node->expr->right != &nil_expression) &&
					last_node_visited != peek_node->right) {
					// node = peek_node->right;
					{
						assert(peek_node->right == NULL);
						peek_node->right  = push_type(scratch.arena, Expr_Node);
						peek_node->right->expr = peek_node->expr->right;
						
						node = peek_node->right;
					}
				} else {
					// visit(peek_node)
					{
						
						if (peek_node->left   != NULL) {
							peek_node->left_width   = peek_node->left->self_width;
						}
						
						if (peek_node->middle != NULL) {
							peek_node->middle_width = peek_node->middle->self_width;
						}
						
						if (peek_node->right  != NULL) {
							peek_node->right_width  = peek_node->right->self_width;
						}
						
						peek_node->self_width = max(peek_node->expr->lexeme.len, peek_node->middle_width);
						
#if 0
						
						if (peek_node->kind == Expression_Kind_LITERAL) {
							peek_node->self_width = i64_digit_count(peek_node->value);
						} else if (peek_node->kind == Expression_Kind_IDENT) {
							peek_node->self_width = peek_node->ident.len;
						} else if (peek_node->kind == Expression_Kind_UNARY) {
							peek_node->self_width = peek_node->token.len;
							peek_node->
								root_width += compute_expression_tree_width(peek_node->right);
						} else if (peek_node->kind == Expression_Kind_BINARY) {
							peek_node->self_width = peek_node->token.len;
							
							root_width += compute_expression_tree_width(peek_node->left);
							root_width += compute_expression_tree_width(peek_node->right);
							root_width += 2;
						}
#endif
						
					}
					
					last_node_visited = stack_first;
					stack_pop(stack_first);
				}
				
			}
			
			if (stack_first == NULL && (node->expr == NULL || node->expr == &nil_expression)) break;
		}
	}
	
	{
		// Print the tree: Breadth-first
		
		Expr_Node *level_first = NULL; // Queue
		Expr_Node *level_last  = NULL;
		
		queue_push(level_first, level_last, root_node);
		
		int level = 0;
		
		for (;;) {
			Expr_Node *node = level_first;
			queue_pop(level_first, level_last);
			
			if (node->level > level) {
				level = node->level;
				printf("\n\n");
			}
			
			int curr_width = node->self_width;
			
			// print_spaces(node->left_sibling_width);
			print_spaces(node->parent_width);
			print_spaces(node->left_width);
			printf("%.*s", string_expand(node->expr->lexeme));
			print_spaces(curr_width - node->expr->lexeme.len);
			print_spaces(node->right_width);
			
			// Push left child
			if (node->expr->left != NULL && node->expr->left != &nil_expression) {
				node->left->level = node->level + 1;
				queue_push(level_first, level_last, node->left);
			}
			
			// Push right child
			if (node->expr->right != NULL && node->expr->right != &nil_expression) {
				node->right->level = node->level + 1;
				node->right->parent_width = curr_width;
				node->right->left_sibling_width = node->left->self_width;
				queue_push(level_first, level_last, node->right);
			}
			
			if (level_first == NULL) break;
		}
	}
	
	printf("\n\n");
	
	scratch_end(scratch);
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

static void
test_sample_expressions(void) {
	Arena arena = {0};
	arena_init(&arena);
	
	Parse_Context parse_context = {0};
	Expression *sample_program_tree = NULL;
	
	arena_reset(&arena);
	parse_context_init(&parse_context, &arena, string_from_cstring(sample_expression_1));
	sample_program_tree = parse_expression(&parse_context, Precedence_NONE);
	
	print_expression_tree(sample_program_tree);
	
	arena_reset(&arena);
	parse_context_init(&parse_context, &arena, string_from_cstring(sample_expression_2));
	sample_program_tree = parse_expression(&parse_context, Precedence_NONE);
	
	print_expression_tree(sample_program_tree);
	
	arena_reset(&arena);
	parse_context_init(&parse_context, &arena, string_from_cstring(sample_expression_3));
	sample_program_tree = parse_expression(&parse_context, Precedence_NONE);
	
	print_expression_tree(sample_program_tree);
	
	arena_reset(&arena);
	parse_context_init(&parse_context, &arena, string_from_cstring(sample_expression_4));
	sample_program_tree = parse_expression(&parse_context, Precedence_NONE);
	
	print_expression_tree(sample_program_tree);
	
	arena_reset(&arena);
	parse_context_init(&parse_context, &arena, string_from_cstring(sample_expression_5));
	sample_program_tree = parse_expression(&parse_context, Precedence_NONE);
	
	print_expression_tree(sample_program_tree);
	
	arena_reset(&arena);
	parse_context_init(&parse_context, &arena, string_from_cstring(sample_expression_6));
	sample_program_tree = parse_expression(&parse_context, Precedence_NONE);
	
	arena_reset(&arena);
	parse_context_init(&parse_context, &arena, string_from_cstring(sample_expression_7));
	sample_program_tree = parse_expression(&parse_context, Precedence_NONE);
	
	arena_reset(&arena);
	parse_context_init(&parse_context, &arena, string_from_cstring(sample_expression_8));
	sample_program_tree = parse_expression(&parse_context, Precedence_NONE);
	
	arena_reset(&arena);
	parse_context_init(&parse_context, &arena, string_from_cstring(sample_expression_9));
	sample_program_tree = parse_expression(&parse_context, Precedence_NONE);
	
	arena_fini(&arena);
}

static Expression *
parse_expression_string(Arena *arena, String source) {
	Parse_Context context = {0};
	parse_context_init(&context, arena, source);
	
	return parse_expression(&context, 0);
}

static Statement *
hardcode_a_statement(Arena *arena) {
	Statement *statement = push_type(arena, Statement);
	statement->kind = Statement_Kind_EXPR;
	statement->expr = parse_expression_string(arena, string_from_lit("2*3 + 10/(4+1)"));
	// statement->expr = parse_expression_string("7-0");
	
	statement->next = push_type(arena, Statement);
	statement->next->kind = Statement_Kind_RETURN;
	statement->next->expr = statement->expr;
	statement->next->next = NULL;
	
	return statement;
}

static Declaration *
hardcode_a_declaration(Arena *arena) {
	Declaration *main_decl = push_type(arena, Declaration);
	main_decl->kind = Declaration_Kind_PROCEDURE;
	main_decl->next = push_type(arena, Declaration);
	main_decl->ident = string_from_lit("main");
	
	{
		main_decl->body = push_type(arena, Statement);
		main_decl->body->kind = Statement_Kind_RETURN;
		main_decl->body->expr = push_type(arena, Expression);
		main_decl->body->expr->kind = Expression_Kind_BINARY;
		main_decl->body->expr->binary = Binary_Operator_CALL;
		main_decl->body->expr->right = &nil_expression;
		main_decl->body->expr->left = push_type(arena, Expression);
		
		{
			main_decl->body->expr->left->kind = Expression_Kind_IDENT;
			main_decl->body->expr->left->ident = string_from_lit("other");
		}
	}
	
	{
		Declaration *next_decl = main_decl->next;
		next_decl->kind = Declaration_Kind_PROCEDURE;
		next_decl->ident = string_from_lit("other");
		
		next_decl->body = hardcode_a_statement(arena);
	}
	
	return main_decl;
}

#endif
