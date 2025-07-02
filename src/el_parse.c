#ifndef EL_PARSE_C
#define EL_PARSE_C

////////////////////////////////
//~ Operators

//- Parsing helpers: Prefix/Infix/Postfix

internal bool token_is_prefix(Token token) {
	bool result = false;
	switch (token.kind) {
		case '+':
		case '-': result = true; break;
		default: break;
	}
	return result;
}

internal bool token_is_postfix(Token token) {
	bool result = false;
	switch (token.kind) {
		case '^': result = true; break;
		default: break;
	}
	return result;
}

internal bool token_is_infix(Token token) {
	bool result = false;
	if (token.kind == '?' || binary_from_token(token) != Binary_Operator_NONE) {
		result = true;
	}
	return result;
}

//- Parsing helpers: Unary/Binary

internal Unary_Operator unary_from_token(Token token) {
	return unary_from_token_kind(token.kind);
}

internal Unary_Operator unary_from_token_kind(Token_Kind kind) {
	Unary_Operator unary = Unary_Operator_NONE;
	switch (kind) {
		case '+': { unary = Unary_Operator_PLUS; } break;
		case '-': { unary = Unary_Operator_MINUS; } break;
		case '^': { unary = Unary_Operator_DEREFERENCE; } break;
		default: break;
	}
	return unary;
}

internal Binary_Operator binary_from_token(Token token) {
	return binary_from_token_kind(token.kind);
}

internal Binary_Operator binary_from_token_kind(Token_Kind kind) {
	Binary_Operator binary = Binary_Operator_NONE;
	switch (kind) {
		case '+':    { binary = Binary_Operator_PLUS; } break;
		case '-':    { binary = Binary_Operator_MINUS; } break;
		case '*':    { binary = Binary_Operator_TIMES; } break;
		case '/':   { binary = Binary_Operator_DIVIDE; } break;
		case '%': { binary = Binary_Operator_MODULUS; } break;
		// case ',':   { binary = Binary_Operator_COMMA; } break;
		case '.':     { binary = Binary_Operator_MEMBER; } break;
		case '(':  { binary = Binary_Operator_CALL; } break;
		case '[':  { binary = Binary_Operator_ARRAY_ACCESS; } break;
		default: break;
	}
	return binary;
}

//- Parsing helpers: Precedence

internal Precedence infix_precedence_from_token(Token token) {
	Precedence precedence = PREC_NONE;
	switch (token.kind) {
		// case ',': precedence = PREC_COMMA; break;
		
		// case '=': precedence = PREC_ASSIGNMENT; break;
		
		case '?': precedence = PREC_TERNARY; break;
		
		// case TOKEN_LOGICAL_AND: precedence = PREC_LOGICAL; break;
		
		// case TOKEN_DOUBLE_EQUALS: precedence = PREC_RELATIONAL; break;
		
		case '+':
		case '-': precedence = PREC_ADDITIVE; break;
		
		case '*':
		case '/':
		case '%': precedence = PREC_MULTIPLICATIVE; break;
		
		case '(': precedence = PREC_CALL_OR_ARRAY_ACCESS; break;
		
		// case '.': precedence = PREC_MEMBER; break;
		
		default: break;
	}
	return precedence;
}

internal Precedence prefix_precedence_from_token(Token token) {
	Precedence precedence = PREC_NONE;
	if (token_is_prefix(token)) {
		precedence = PREC_UNARY_PREFIX;
	}
	return precedence;
}

internal Precedence postfix_precedence_from_token(Token token) {
	Precedence precedence = PREC_NONE;
	if (token_is_postfix(token)) {
		precedence = PREC_UNARY_POSTFIX;
	}
	return precedence;
}

//- Parsing helpers: Misc

internal bool token_is_expression_terminator(Token token) {
	Token_Kind k = token.kind;
	return (k == TOKEN_EOI || k == ')' || k == ']' || k == '}' || k == ';');
}

internal bool token_is_expression_atom(Token token) {
	Token_Kind k = token.kind;
	return k == TOKEN_INTEGER || k == TOKEN_STRING || k == TOKEN_IDENT;
}

internal bool token_is_declarator(Token token) {
	Token_Kind k = token.kind;
	return k == ':' || k == TOKEN_COLON_EQUALS || k == TOKEN_DOUBLE_COLON;
}

internal bool token_is_assigner(Token token) {
	Token_Kind k = token.kind;
	return k == '=' || k == TOKEN_PLUS_EQUALS || k == TOKEN_DASH_EQUALS || k == TOKEN_STAR_EQUALS ||
		k == TOKEN_SLASH_EQUALS || k == TOKEN_PERCENT_EQUALS || k == TOKEN_AMPER_EQUALS ||
		k == TOKEN_EMARK_EQUALS;
}

////////////////////////////////
//~ AST

internal Ast_Expression *ast_expression_alloc(Arena *arena) {
	Ast_Expression *expr = push_type(arena, Ast_Expression);
	memcpy(expr, &nil_expression, sizeof(Ast_Expression));
	
	return expr;
}

internal Ast_Statement *ast_statement_alloc(Arena *arena) {
	Ast_Statement *stat = push_type(arena, Ast_Statement);
	memcpy(stat, &nil_statement, sizeof(Ast_Statement));
	
	return stat;
}

internal Ast_Declaration *ast_declaration_alloc(Arena *arena) {
	Ast_Declaration *decl = push_type(arena, Ast_Declaration);
	memcpy(decl, &nil_declaration, sizeof(Ast_Declaration));
	
	return decl;
}

//- Parser: Expressions

internal Ast_Expression *make_atom_expression(Parser *parser, Token token) {
	Ast_Expression *node = ast_expression_alloc(parser->arena);
	
	if (node != NULL) {
		if (token.kind == TOKEN_INTEGER) {
			node->kind     = Ast_Expression_Kind_INT_LITERAL;
			node->lexeme   = lexeme_from_token(parser->lexer, token);
			node->value    = token.int_val;
			node->location = token.loc;
		} else if (token.kind == TOKEN_STRING) {
			node->kind     = Ast_Expression_Kind_STRING_LITERAL;
			node->lexeme   = lexeme_from_token(parser->lexer, token);
			// node->value    = token.string_val;
			node->location = token.loc;
		} else if (token.kind == TOKEN_IDENT) {
			node->kind     = Ast_Expression_Kind_IDENT;
			node->lexeme   = lexeme_from_token(parser->lexer, token);
			node->ident    = node->lexeme;
			node->location = token.loc;
		} else { panic(); }
	}
	
	return node;
}

internal Ast_Expression *make_unary_expression(Parser *parser, Token unary, Ast_Expression *subexpr) {
	Ast_Expression *node = ast_expression_alloc(parser->arena);
	
	if (node != NULL) {
		node->kind     = Ast_Expression_Kind_UNARY;
		node->lexeme   = lexeme_from_token(parser->lexer, unary);
		node->unary    = unary_from_token(unary);
		node->subexpr  = subexpr;
		node->location = unary.loc;
		node->location = range1di32_merge(node->location, subexpr->location);
	}
	
	return node;
}

internal Ast_Expression *make_binary_expression(Parser *parser, Token binary, Ast_Expression *left, Ast_Expression *right) {
	Ast_Expression *node = ast_expression_alloc(parser->arena);
	
	if (node != NULL) {
		node->kind     = Ast_Expression_Kind_BINARY;
		node->lexeme   = lexeme_from_token(parser->lexer, binary);
		node->binary   = binary_from_token(binary);
		node->left     = left;
		node->right    = right;
		node->location = binary.loc;
		node->location = range1di32_merge(node->location, left->location);
		if (right != NULL && right != &nil_expression)
			node->location = range1di32_merge(node->location, right->location);
	}
	
	return node;
}

internal Ast_Expression *make_ternary_expression(Parser *parser, Ast_Expression *left, Ast_Expression *middle, Ast_Expression *right) {
	Ast_Expression *node = ast_expression_alloc(parser->arena);
	
	if (node != NULL) {
		node->kind     = Ast_Expression_Kind_TERNARY;
		node->lexeme   = string_from_lit("?:");
		node->left     = left;
		node->middle   = middle;
		node->right    = right;
		node->location = left->location;
		node->location = range1di32_merge(node->location, middle->location);
		node->location = range1di32_merge(node->location, right->location);
	}
	
	return node;
}

internal Ast_Expression *parse_expression(Parser *parser, Precedence caller_precedence, bool required) {
	Ast_Expression *left = &nil_expression;
	
	Token token = peek_token(parser->lexer);
	if (token_is_expression_atom(token)) {
		consume_token(parser->lexer); // atom
		
		left = make_atom_expression(parser, token);
	} else if (token_is_prefix(token)) {
		consume_token(parser->lexer); // prefix
		Ast_Expression *right = parse_expression(parser, prefix_precedence_from_token(token), true);
		
		left = make_unary_expression(parser, token, right);
	} else if (token.kind == '(') {
		consume_token(parser->lexer); // (
		Ast_Expression *grouped = parse_expression(parser, PREC_NONE, true);
		expect_token_kind(parser, ')', "Expected )");
		
		left = grouped;
	} else {
		// This token is not the beginning of any expression.
		if (required) {
			report_parse_error(parser, "Expected an expression");
		}
	}
	
	for (;;) {
		token = peek_token(parser->lexer);
		
		if (token_is_postfix(token)) {
			Precedence precedence = postfix_precedence_from_token(token);
			if (precedence < caller_precedence)  break;
			
			consume_token(parser->lexer); // postfix
			
			left = make_unary_expression(parser, token, left);
		} else if (token_is_infix(token)) {
			Precedence precedence = infix_precedence_from_token(token);
			if (precedence < caller_precedence)  break;
			
			consume_token(parser->lexer); // infix
			
			if (token.kind == '?') {
				Ast_Expression *middle = parse_expression(parser, PREC_NONE, true);
				
				expect_token_kind(parser, ':', "Expected :");
				Ast_Expression *right = parse_expression(parser, precedence, true);
				
				left = make_ternary_expression(parser, left, middle, right);
			} else {
				bool subexpr_required = true;
				if (token.kind == '(' || token.kind == '[') {
					precedence = 0;
					
					if (token.kind == '(') {
						subexpr_required = false;
					}
				}
				
				Ast_Expression *right = parse_expression(parser, precedence, subexpr_required);
				
				left = make_binary_expression(parser, token, left, right);
				
				if (token.kind == '(')  expect_token_kind(parser, ')', "Expected )");
				if (token.kind == '[')  expect_token_kind(parser, ']', "Expected ]");
			}
		} else {
			break;
		}
	}
	
	assert(left != NULL);
	return left;
}

//- Parser: Statements

internal Ast_Statement *make_statement_(Parser *parser, Ast_Statement_Kind kind, Range1DI32 location, Make_Statement_Params params) {
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

internal Ast_Statement *parse_statement(Parser *parser) {
	Ast_Statement *result = &nil_statement;
	
	Token token = peek_token(parser->lexer);
	if (token.kind == '{') {
		consume_token(parser->lexer); // {
		
		Range1DI32 lbrace_location = token.loc;
		Range1DI32 rbrace_location = {0};
		
		// Parse statement list inside the braces
		Ast_Statement *first = NULL;
		Ast_Statement *last  = NULL;
		
		token = peek_token(parser->lexer);
		if (token.kind != '}') { // Allow empty blocks like {} (without a semicolon in the middle)
			for (;;) {
				Ast_Statement *stat = parse_statement(parser);
				if (stat != &nil_statement) {
					// Avoid writing to read-only memory (*but continue trying,
					// since nil-statements can also mean empty statements and not
					// only failed ones).
					// TODO: Review :NilStatements
					queue_push_nz(first, last, stat, next, check_nil_statement, set_nil_statement);
				}
				
				token = peek_token(parser->lexer);
				if (token.kind == '}') {
					rbrace_location = token.loc;
					
					// We set it to &nil_statement always for now, so that {;} works.
					// TODO: Review :NilStatements
					if (first == NULL) {
						first = &nil_statement;
					}
					
					consume_token(parser->lexer);
					break;
				} else if (token.kind == TOKEN_EOI) {
					report_parse_error(parser, "Expected }");
					break;
				}
			}
		} else {
			rbrace_location = token.loc;
			first = &nil_statement;
		}
		
		if (first != NULL) {
			result = make_statement(parser, Ast_Statement_Kind_BLOCK, range1di32_merge(lbrace_location, rbrace_location),
									.block = first);
		}
	} else if (token.keyword == KEYWORD_RETURN) {
		consume_token(parser->lexer); // return
		
		Range1DI32 location = token.loc;
		
		// Parse return values: either nothing, or an expression list.
		//
		// Do not require an expression to begin with (it could be a "void" return),
		// but require one more expression after each comma.
		Ast_Expression *first = NULL;
		Ast_Expression *last  = NULL;
		
		for (bool is_expr_list = false; ; is_expr_list = true) {
			Ast_Expression *expr = parse_expression(parser, PREC_NONE, false);
			if (expr == &nil_expression) {
				if (is_expr_list) {
					assert(there_were_parse_errors(parser));
				}
				
				break; // Avoid writing to read-only memory
			}
			
			queue_push_nz(first, last, expr, next, check_nil_expression, set_nil_expression);
			
			token = peek_token(parser->lexer);
			if (token.kind != ',') break; // That was the last expr in the list
		}
		
		if (first != NULL) {
			result = make_statement(parser, Ast_Statement_Kind_RETURN, location, .expr = first);
		}
	} else {
		// Do *NOT* consume the first token as it will be part of the first expression/lhs.
		
		Range1DI32 location = token.loc;
		
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
			Ast_Expression *expr = parse_expression(parser, PREC_NONE, is_expr_list);
			if (expr == &nil_expression) {
				if (is_expr_list) {
					assert(there_were_parse_errors(parser));
				}
				
				break; // Avoid writing to read-only memory
			}
			
			count += 1;
			queue_push_nz(first, last, expr, next, check_nil_expression, set_nil_expression);
			location = range1di32_merge(location, expr->location);
			
			token = peek_token(parser->lexer);
			if (token_is_assigner(token)) {
				kind = Ast_Statement_Kind_ASSIGNMENT;
				break;
			}
			
			if (token_is_declarator(token)) {
				kind = Ast_Statement_Kind_DECLARATION;
				break;
			}
			
			if (token.kind == ',') {
				consume_token(parser->lexer);
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
			
			Token assigner = peek_token(parser->lexer);
			consume_token(parser->lexer); // assigner
			
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
				Ast_Expression *expr = parse_expression(parser, PREC_NONE, true);
				if (expr == &nil_expression) {
					assert(there_were_parse_errors(parser));
					break; // Avoid writing to read-only memory
				}
				
				count += 1;
				queue_push_nz(first, last, expr, next, check_nil_expression, set_nil_expression);
				
				token = peek_token(parser->lexer);
				if (token.kind == ',') {
					consume_token(parser->lexer);
				} else {
					break; // That was the last expr in the list
				}
			}
			
			// Do *NOT* report an error if lhs_count != count: it's not the number of expressions
			// that matters, but the number of values. One expression could yield multiple values,
			// e.g. a function call with multiple returns.
			
			if (lhs_first != NULL && first != NULL) {
				result = make_statement(parser, kind, assigner.loc, .assigner = assigner.kind,
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
			
			i64 ident_count = 0;
			String *idents  = push_array(scratch.arena, String, count);
			Range1DI32 *ident_locations = push_array(scratch.arena, Range1DI32, count);
			for (Ast_Expression *expr = first; expr != NULL && expr != &nil_expression; expr = expr->next) {
				if (expr->kind == Ast_Expression_Kind_IDENT) {
					ident_locations[ident_count] = expr->location;
					idents[ident_count] = expr->ident;
					ident_count += 1;
				} else {
					report_parse_error(parser, "Only identifiers are allowed on the left of a declaration");
				}
			}
			
			if (count == ident_count) {
				// Do *NOT* consume the declarator as it will be used in parse_declaration_after_lhs().
				
				Token declarator = peek_token(parser->lexer);
				assert(token_is_declarator(declarator));
				
				Ast_Declaration *decl = parse_declaration_after_lhs(parser, idents, ident_locations, ident_count);
				result = make_statement(parser, kind, declarator.loc, .decl = decl);
			} else {
				assert(there_were_parse_errors(parser));
			}
			
			scratch_end(scratch);
		} else {
			panic("Invalid codepath");
		}
		
		allow_break();
	}
	
	{
		bool require_semicolon = true;
		if (result->kind == Ast_Statement_Kind_BLOCK) require_semicolon = false;
		if (result->kind == Ast_Statement_Kind_DECLARATION && result->decl->initter_count > 0 &&
			result->decl->initters[result->decl->initter_count-1].kind == Initter_Kind_PROCEDURE) {
			require_semicolon = false;
		}
		
		if (require_semicolon) {
			expect_token_kind(parser, TOKEN_SEMICOLON, "Expected ; after statement");
		}
	}
	
	if (there_were_parse_errors(parser)) {
		result = &nil_statement; // TODO: @Leak, @Hack
	}
	
	assert(result != NULL && result->next != NULL);
	return result;
}

//- Parser: Declarations

internal Ast_Declaration *parse_declaration(Parser *parser) {
	Ast_Declaration *result = &nil_declaration;
	Scratch scratch = scratch_begin(&parser->arena, 1);
	
	typedef struct Token_Node Token_Node;
	struct Token_Node { Token token; Token_Node *next; };
	
	Token_Node *ident_first = NULL;
	Token_Node *ident_last  = NULL;
	i64 ident_count = 0;
	
	Token token = {0};
	for (;;) {
		token = peek_token(parser->lexer);
		if (token.kind == TOKEN_IDENT) {
			consume_token(parser->lexer); // ident
			
			Token_Node *node = push_type(scratch.arena, Token_Node);
			node->token = token;
			
			ident_count += 1;
			queue_push(ident_first, ident_last, node);
			token = peek_token(parser->lexer);
		} else {
			report_parse_errorf(parser, "Expected identifier, got '%.*s'", string_expand(lexeme_from_token(parser->lexer, token)));
		}
		
		if (token_is_declarator(token)) break;
		if (token.kind == ',') {
			consume_token(parser->lexer);
		} else {
			report_parse_errorf(parser, "Unexpected token '%.*s'", string_expand(lexeme_from_token(parser->lexer, token)));
			break;
		}
	}
	
	if (token_is_declarator(token)) {
		String   *idents = push_array(scratch.arena, String, ident_count);
		Range1DI32 *ident_locations = push_array(scratch.arena, Range1DI32, ident_count);
		ident_count = 0;
		for (Token_Node *node = ident_first; node != NULL; node = node->next) {
			ident_locations[ident_count] = node->token.loc;
			idents[ident_count] = lexeme_from_token(parser->lexer, node->token);
			ident_count += 1;
		}
		
		result = parse_declaration_after_lhs(parser, idents, ident_locations, ident_count);
	}
	
	if (there_were_parse_errors(parser)) {
		assert(result == &nil_declaration);
	}
	
	scratch_end(scratch);
	return result;
}

internal Ast_Declaration *parse_declaration_after_lhs(Parser *parser, String *idents, Range1DI32 *ident_locations, i64 ident_count) {
	Ast_Declaration *result = &nil_declaration;
	
	// Determine the kind of declaration
	bool     parse_initializers = false;
	Type_Ann    type_annotation = {0};
	Ast_Declaration_Flags flags = 0;
	
	Token token = peek_token(parser->lexer);
	if (token.kind == ':') {
		consume_token(parser->lexer); // :
		
		// ':' alone (NOT ':=' or '::') means there MUST be an explicit type annotation,
		// so we parse that.
		// TODO: For now the only valid type annotations are identifiers.
		
		Token next_token = peek_token(parser->lexer);
		if (next_token.kind == TOKEN_IDENT) {
			consume_token(parser->lexer); // type ident
			
			flags |= Ast_Declaration_Flag_TYPE_ANNOTATION;
			type_annotation.ident = lexeme_from_token(parser->lexer, next_token);
		} else {
			report_parse_error(parser, "Expected type annotation after :");
		}
		
		next_token = peek_token(parser->lexer);
		if (next_token.kind == '=' || next_token.kind == ':') {
			consume_token(parser->lexer); // = or :
			parse_initializers = true;
			
			if (next_token.kind == ':') flags |= Ast_Declaration_Flag_CONSTANT;
		}
	} else if (token.kind == TOKEN_COLON_EQUALS ||
			   token.kind == TOKEN_DOUBLE_COLON) {
		consume_token(parser->lexer); // := or ::
		parse_initializers = true;
		
		if (token.kind == TOKEN_DOUBLE_COLON) flags |= Ast_Declaration_Flag_CONSTANT;
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
			result->entities[i].location = ident_locations[i];
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
				
				token = peek_token(parser->lexer);
				if (token.kind == ',') {
					consume_token(parser->lexer);
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

internal Initter parse_declaration_rhs(Parser *parser) {
	Initter result = nil_initter;
	
	Token token = peek_token(parser->lexer);
	if (token.keyword == KEYWORD_PROC) {
		// Starting with the keyword 'proc', this is a
		// proc definition OR a proc TYPE definition.
		
		consume_token(parser->lexer); // proc
		
		result.kind = Initter_Kind_PROCEDURE_TYPE;
		result.first_param = parse_proc_header(parser);
		
		token = peek_token(parser->lexer);
		if (token.kind == TOKEN_TRIPLE_DASH) {
			// This is a procedure prototype.
			consume_token(parser->lexer); // ---
			
			result.kind = Initter_Kind_PROCEDURE_PROTO;
		} else if (token.kind == '{') {
			// This is a procedure definition.
			//
			// Do *NOT* consume { as it will be used by parse_statement to
			// know it's a block statement.
			
			result.kind = Initter_Kind_PROCEDURE;
			result.body = parse_statement(parser);
		}
		
	} else if (token.keyword == KEYWORD_STRUCT) {
		// Starting with the keyword 'struct', this is a
		// struct definition.
		
		consume_token(parser->lexer); // struct
		
		result.kind = Initter_Kind_STRUCT;
		// result.initializer.first_member = parse_struct_definition(parser);
		
	} else {
		// Starting in any other way, we treat is as a generic expression,
		// and we figure out later what exactly that is.
		
		result.kind = Initter_Kind_EXPR;
		result.expr = parse_expression(parser, PREC_NONE, true);
	}
	
	// TODO: 'distinct'
	// [
	// ^
	// 'enum'
	// 'union'
	
	return result;
}

internal Type_Ann parse_type_annotation(Parser *parser) {
	Type_Ann result = {0};
	
	Token token = peek_token(parser->lexer);
	
	// case ident    ->
	// case "proc"   ->
	// case "struct" ->
	// case          -> error
	
	return result;
}

internal Ast_Declaration *parse_proc_header(Parser *parser) {
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
			token = peek_token(parser->lexer);
			if (token.kind == TOKEN_IDENT) {
				consume_token(parser->lexer); // ident
				string_list_push(scratch.arena, &arg_names, lexeme_from_token(parser->lexer, token));
			} else {
				report_parse_error(parser, "Unexpected token");
			}
			
			token = peek_token(parser->lexer);
			if (token.kind == ':' || token.kind == TOKEN_COLON_EQUALS) break;
			if (token.kind != ',') {
				report_parse_error(parser, "Unexpected token");
				break;
			}
		}
		
		bool parse_default_values = false;
		Type_Ann  type_annotation = {0};
		bool type_annotation_present = false;
		
		if (token.kind == ':') {
			consume_token(parser->lexer); // :
			
			token = peek_token(parser->lexer);
			if (token.kind == TOKEN_IDENT) {
				consume_token(parser->lexer); // type ident
				
				type_annotation_present = true;
				type_annotation.ident = lexeme_from_token(parser->lexer, token);
			} else {
				report_parse_error(parser, "Expected type annotation");
			}
			
			if (token.kind == '=') {
				parse_default_values = true;
			}
		} else if (token.kind == TOKEN_COLON_EQUALS) {
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
		
		token = peek_token(parser->lexer);
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
	
	Token token = peek_token(parser->lexer);
	if (token.kind == TOKEN_FORWARD_ARROW) {
		consume_token(parser->lexer); // ->
		
		String_List ret_types = {0};
		
		for (;;) {
			token = peek_token(parser->lexer);
			if (token.kind == TOKEN_IDENT) {
				consume_token(parser->lexer); // type ident
				string_list_push(scratch.arena, &ret_types, lexeme_from_token(parser->lexer, token));
			}
			
			token = peek_token(parser->lexer);
			if (token.kind == ',') {
				consume_token(parser->lexer);
			} else {
				break;
			}
		}
		
		// TODO: Add to explicit_type
	}
	
	// TODO: Eventually parse other annotations
	
	scratch_end(scratch);
	return result;
}

////////////////////////////////
//~ Program

internal Ast_Declaration *parse_program(Parser *parser) {
	Ast_Declaration *result = &nil_declaration;
	
	Ast_Declaration *first_decl = NULL;
	Ast_Declaration *last_decl  = NULL;
	
	for (Token token = peek_token(parser->lexer); token.kind != TOKEN_EOI; token = peek_token(parser->lexer)) {
		Ast_Declaration *decl = parse_declaration(parser);
		if (decl == NULL || decl == &nil_declaration) break;
		
		queue_push_nz(first_decl, last_decl, decl, next, check_nil_declaration, set_nil_declaration);
	}
	
	if (first_decl != NULL) result = first_decl;
	
	consume_all_tokens(parser->lexer);
	
	return result;
}

////////////////////////////////
//~ Context

internal void expect_token_kind(Parser *parser, Token_Kind kind, char *message) {
	if (peek_token(parser->lexer).kind != kind)
		report_parse_error(parser, message);
	consume_token(parser->lexer);
}

internal void report_parse_error(Parser *parser, char *message) {
	if (parser->error_count < max_printed_parse_errors) {
		String span = lexeme_from_token(parser->lexer, parser->lexer->token);
		fprintf(stderr, "Syntax error (%i..%i): %s.\n", parser->lexer->token.loc.start, parser->lexer->token.loc.end, message, string_expand(span));
	}
	parser->error_count += 1;
}

internal void report_parse_errorf(Parser *parser, char *format, ...) {
	if (parser->error_count < max_printed_parse_errors) {
		va_list args;
		va_start(args, format);
		Scratch scratch = scratch_begin(0, 0);
		
		String formatted_message = push_stringf_va_list(scratch.arena, format, args);
		report_parse_error(parser, cstring_from_string(scratch.arena, formatted_message));
		
		scratch_end(scratch);
		va_end(args);
	}
	parser->error_count += 1;
}

internal void parser_init(Parser *parser, Arena *arena, String source) {
	memset(parser, 0, sizeof(*parser));
	parser->arena = arena;
	parser->lexer = push_type(arena, Lexer);
	parser->lexer->source = source;
}

internal bool there_were_parse_errors(Parser *parser) {
	return parser->error_count > 0;
}

//- Wrappers

internal Ast_Expression *parse_expression_string(Arena *arena, String source) {
	Parser context = {0};
	parser_init(&context, arena, source);
	
	return parse_expression(&context, PREC_NONE, true);
}

internal Ast_Statement *parse_statement_string(Arena *arena, String source) {
	Parser context = {0};
	parser_init(&context, arena, source);
	
	return parse_statement(&context);
}

internal Ast_Declaration *parse_declaration_string(Arena *arena, String source) {
	Parser context = {0};
	parser_init(&context, arena, source);
	
	return parse_declaration(&context);
}

internal Ast_Declaration *parse_program_string(Arena *arena, String source) {
	Parser context = {0};
	parser_init(&context, arena, source);
	
	return parse_program(&context);
}

#endif
