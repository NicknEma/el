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
		case '?': { binary = Binary_Operator_TERNARY; } break;
		// case ',':   { binary = Binary_Operator_COMMA; } break;
		case '.':     { binary = Binary_Operator_MEMBER; } break;
		case '(':  { binary = Binary_Operator_CALL; } break;
		case '[':  { binary = Binary_Operator_ARRAY_ACCESS; } break;
		
#if 0
		case '=':
		case TOKEN_PLUS_EQUALS:
		case TOKEN_DASH_EQUALS:
		case TOKEN_STAR_EQUALS:
		case TOKEN_SLASH_EQUALS: {
			binary = Binary_Operator_ASSIGNMENT;
		} break;
#endif
		
		default: break;
	}
	return binary;
}

//- Parsing helpers: Precedence

internal Precedence infix_precedence_from_token(Token token) {
	Precedence precedence = PREC_NONE;
	switch (token.kind) {
		// case ',': precedence = PREC_COMMA; break;
		
#if 0
		case '=':
		case TOKEN_PLUS_EQUALS:
		case TOKEN_DASH_EQUALS:
		case TOKEN_STAR_EQUALS:
		case TOKEN_SLASH_EQUALS: precedence = PREC_ASSIGNMENT; break;
#endif
		
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
	return k == TOKEN_BOOLEAN || k == TOKEN_INTEGER || k == TOKEN_STRING || k == TOKEN_IDENT;
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
	
	node->location = token.loc;
	
	if (token.kind == TOKEN_BOOLEAN) {
		node->kind         = Ast_Expression_Kind_BOOL_LITERAL;
		node->bool_value   = token.int_val;
	} else if (token.kind == TOKEN_INTEGER) {
		node->kind         = Ast_Expression_Kind_INT_LITERAL;
		node->i64_value    = token.int_val;
	} else if (token.kind == TOKEN_STRING) {
		node->kind         = Ast_Expression_Kind_STRING_LITERAL;
		node->string_value = token.string_val;
	} else if (token.kind == TOKEN_IDENT) {
		node->kind         = Ast_Expression_Kind_IDENT;
		node->ident        = lexeme_from_token(&parser->lexer, token);
	} else { panic(); }
	
	return node;
}

internal Ast_Expression *make_unary_expression(Parser *parser, Token unary, Ast_Expression *subexpr) {
	Ast_Expression *node = ast_expression_alloc(parser->arena);
	
	node->kind       = Ast_Expression_Kind_UNARY;
	node->unary      = unary_from_token(unary);
	node->expr_first = subexpr;
	node->expr_last  = subexpr;
	node->location   = unary.loc;
	node->location   = range1di32_merge(node->location, subexpr->location);
	
	return node;
}

internal Ast_Expression *make_binary_expression(Parser *parser, Token binary, Ast_Expression *left, Ast_Expression *right) {
	Ast_Expression *node = ast_expression_alloc(parser->arena);
	
	node->kind       = Ast_Expression_Kind_BINARY;
	node->binary     = binary_from_token(binary);
	node->expr_first = left;
	node->expr_last  = left;
	dll_push_back_npz(node->expr_first, node->expr_last, right, next, prev, check_nil_expression, set_nil_expression);
	node->location   = binary.loc;
	node->location   = range1di32_merge(node->location, left->location);
	if (!check_nil_expression(right))
		node->location = range1di32_merge(node->location, right->location);
	
	return node;
}

internal Ast_Expression *make_ternary_expression(Parser *parser, Ast_Expression *left, Ast_Expression *middle, Ast_Expression *right) {
	Ast_Expression *node = ast_expression_alloc(parser->arena);
	
	node->kind       = Ast_Expression_Kind_BINARY;
	node->binary     = Binary_Operator_TERNARY;
	node->expr_first = left;
	node->expr_last  = left;
	dll_push_back_npz(node->expr_first, node->expr_last, middle, next, prev, check_nil_expression, set_nil_expression);
	dll_push_back_npz(node->expr_first, node->expr_last, right, next, prev, check_nil_expression, set_nil_expression);
	node->location   = left->location;
	node->location   = range1di32_merge(node->location, middle->location);
	node->location   = range1di32_merge(node->location, right->location);
	
	return node;
}

internal Ast_Expression *make_compound_literal_expression(Parser *parser, Ast_Expression *type_annotation, Expr_List exprs, Range1DI32 loc) {
	Ast_Expression *expr = ast_expression_alloc(parser->arena);
	
	expr->kind       = Ast_Expression_Kind_COMPOUND_LITERAL;
	expr->expr_first = exprs.first;
	expr->expr_last  = exprs.last;
	expr->expr_sep   = exprs.first;
	dll_push_front_npz(expr->expr_first, expr->expr_last, type_annotation, next, prev, check_nil_expression, set_nil_expression); // Put the type annotation as the rightmost child
	expr->location   = loc;
	
	return expr;
}

internal Ast_Expression *make_type_modifier(Parser *parser, Ast_Expression *subexpr, Type_Modifier mod, Range1DI32 loc) {
	Ast_Expression *expr = ast_expression_alloc(parser->arena);
	
	expr->kind       = Ast_Expression_Kind_TYPE_MODIFIER;
	expr->modifier   = mod;
	expr->expr_first = subexpr;
	expr->expr_last  = subexpr;
	expr->location   = loc;
	
	return expr;
}

internal Ast_Expression *make_call_expr(Parser *parser, Ast_Expression *callee, Ast_Expression *first_arg, Ast_Expression *last_arg, Range1DI32 loc) {
	Ast_Expression *expr = ast_expression_alloc(parser->arena);
	
	expr->kind       = Ast_Expression_Kind_BINARY;
	expr->binary     = Binary_Operator_CALL;
	expr->expr_first = first_arg;
	expr->expr_last  = last_arg;
	dll_push_front_npz(expr->expr_first, expr->expr_last, callee, next, prev, check_nil_expression, set_nil_expression); // Put the callee as the leftmost child
	expr->location   = loc;
	
	return expr;
}

internal Expr_List parse_expr_list(Parser *parser, Parse_Flags parse_flags) {
	Expr_List exprs = {&nil_expression, &nil_expression};
	
	exprs.first = parse_expression(parser, PREC_NONE, parse_flags);
	exprs.last  = exprs.first;
	
	Parse_Flags subexpr_parse_flags = parse_flags;
	if (parse_flags & Parse_Flags_EXPR_LIST_ALLOW_TRAILING_COMMA) {
		subexpr_parse_flags &= ~Parse_Flags_EXPR_REQUIRED;
	}
	
	for (Token token = peek_token(&parser->lexer);; token = peek_token(&parser->lexer)) {
		if (token.kind == ',') {
			consume_token(&parser->lexer);
			
			Ast_Expression *subexpr = parse_expression(parser, PREC_NONE, subexpr_parse_flags);
			
			if (!check_nil_expression(subexpr)) {
				dll_push_back_npz(exprs.first, exprs.last, subexpr, next, prev, check_nil_expression, set_nil_expression);
			} else {
				// break;
			}
		} else {
			break;
		}
	}
	
	return exprs;
}

internal Ast_Expression *parse_expression(Parser *parser, Precedence caller_precedence, Parse_Flags parse_flags) {
	Ast_Expression *left = &nil_expression;
	
	Token token = peek_token(&parser->lexer);
	if (token_is_expression_atom(token)) {
		Token atom = token;
		consume_token(&parser->lexer); // atom
		
		if (token.kind == TOKEN_IDENT) { // It could be a compound literal
			Token ident = token;
			token = peek_token(&parser->lexer);
			
			if (token.kind == '{') { // It is a compound literal
				consume_token(&parser->lexer); // {
				
				Ast_Expression *type_annotation = make_atom_expression(parser, ident);
				Expr_List compound_exprs = parse_expr_list(parser, Parse_Flags_COMPOUND_LIT);
				
				token = peek_token(&parser->lexer); // Save copy of } so we have the loc
				expect_token_kind(parser, '}', "Expected '}' closing compound literal");
				
				if (!there_were_parse_errors(parser)) {
					Range1DI32 loc = range1di32_merge(ident.loc, token.loc);
					left = make_compound_literal_expression(parser, type_annotation, compound_exprs, loc);
				}
			} else {
				left = make_atom_expression(parser, atom);
			}
		} else {
			left = make_atom_expression(parser, atom);
		}
	} else if (token.kind == '[' || token.keyword == KEYWORD_STRUCT ||
			   token.kind == '^') { // This is the beginning of a (nested) type annotation
		
		// Consume the first token(s) so we can make the top node of the nest, then recursively call
		// parse_expression
		
		Ast_Expression *type_annotation = &nil_expression;
		if (token.kind == '^') {
			consume_token(&parser->lexer); // ^
			
			Ast_Expression *pointed = parse_expression(parser, PREC_NONE, Parse_Flags_EXPR_REQUIRED);
			type_annotation = make_type_modifier(parser, pointed, Type_Modifier_POINTER, token.loc);
		} else if (token.kind == '[') {
			consume_token(&parser->lexer); // [
			
			Token lbrack = token;
			token = peek_token(&parser->lexer);
			
			if (token.kind == ']') {
				consume_token(&parser->lexer); // ]
				
				Ast_Expression *elements = parse_expression(parser, PREC_NONE, Parse_Flags_EXPR_REQUIRED);
				type_annotation = make_type_modifier(parser, elements, Type_Modifier_SLICE, lbrack.loc);
			} else {
				report_parse_error(parser, "Expected ']' after '['"); // TODO: Other array kinds
			}
		} else {
			consume_token(&parser->lexer); // struct
			
			Token struct_token = token;
			token = peek_token(&parser->lexer);
			if (token.kind == '{') {
				// TODO: Temporarily consume all sturct fields. Implement actual struct parsing
				while (peek_token(&parser->lexer).kind != '}') consume_token(&parser->lexer);
				consume_token(&parser->lexer);
				
				type_annotation = ast_expression_alloc(parser->arena);
				type_annotation->kind     = Ast_Expression_Kind_STRUCT_DEFN;
				type_annotation->location = struct_token.loc;
			} else {
				report_parse_error(parser, "Expected '{' after 'struct' keyword");
			}
		}
		
		// We just parsed a type annotation. If the next token is a {, it means there's a compound literal
		// and this was the annotation for that. Otherwise, it was a lone type annotation and we
		// proceed as normal.
		token = peek_token(&parser->lexer);
		if (token.kind == '{') {
			consume_token(&parser->lexer);
			
			Expr_List compound_exprs = parse_expr_list(parser, Parse_Flags_COMPOUND_LIT);
			
			token = peek_token(&parser->lexer); // Save copy of } so we have the loc
			expect_token_kind(parser, '}', "Expected '}' closing compound literal");
			
			if (!there_were_parse_errors(parser)) {
				Range1DI32 loc = range1di32_merge(type_annotation->location, token.loc);
				left = make_compound_literal_expression(parser, type_annotation, compound_exprs, loc);
			}
		} else {
			left = type_annotation;
		}
		
	} else if (token_is_prefix(token)) {
		consume_token(&parser->lexer);
		Ast_Expression *right = parse_expression(parser, prefix_precedence_from_token(token), Parse_Flags_EXPR_REQUIRED);
		
		left = make_unary_expression(parser, token, right);
	} else if (token.kind == '(') {
		consume_token(&parser->lexer);
		
		Ast_Expression *grouped = parse_expression(parser, PREC_NONE, Parse_Flags_EXPR_REQUIRED);
		expect_token_kind(parser, ')', "Expected )");
		
		left = grouped;
	} else if (parse_flags & Parse_Flags_EXPR_REQUIRED) {
		report_parse_errorf(parser, "Expected an expression; token '%.*s' is not the beginning of any expression", string_expand(lexeme_from_token(&parser->lexer, token)));
	}
	
	for (;!check_nil_expression(left);) {
		token = peek_token(&parser->lexer);
		
		if (token_is_postfix(token)) {
			Precedence precedence = postfix_precedence_from_token(token);
			if (precedence < caller_precedence)  break;
			
			consume_token(&parser->lexer); // postfix
			
			left = make_unary_expression(parser, token, left);
		} else if (token_is_infix(token) && (token.kind != '=' || (parse_flags & Parse_Flags_EXPR_ALLOW_ASSIGNMENT))) {
			Precedence precedence = infix_precedence_from_token(token);
			if (precedence < caller_precedence)  break;
			
			consume_token(&parser->lexer); // infix
			
			if (token.kind == '?') {
				Ast_Expression *middle = parse_expression(parser, PREC_NONE, Parse_Flags_EXPR_REQUIRED);
				
				expect_token_kind(parser, ':', "Expected :");
				Ast_Expression *right = parse_expression(parser, precedence, Parse_Flags_EXPR_REQUIRED);
				
				left = make_ternary_expression(parser, left, middle, right);
			} else {
				Parse_Flags subexpr_parse_flags = Parse_Flags_EXPR_REQUIRED;
				if (token.kind == '(' || token.kind == '[') {
					precedence = PREC_NONE;
					
					if (token.kind == '(') {
						subexpr_parse_flags &= ~Parse_Flags_EXPR_REQUIRED;
					}
				}
				
				if (token.kind == '(') {
					Expr_List args = parse_expr_list(parser, subexpr_parse_flags);
					left = make_call_expr(parser, left, args.first, args.last, token.loc);
				} else {
					Ast_Expression *right = parse_expression(parser, precedence, subexpr_parse_flags);
					left = make_binary_expression(parser, token, left, right);
				}
				
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

internal Ast_Statement *make_expr_stmt(Parser *parser, Ast_Expression *expr, Range1DI32 loc) {
	Ast_Statement *stmt = ast_statement_alloc(parser->arena);
	
	stmt->kind       = Ast_Statement_Kind_EXPR;
	stmt->expr_first = expr;
	stmt->expr_last  = expr;
	stmt->location   = loc;
	
	return stmt;
}

internal Ast_Statement *make_block_stmt(Parser *parser, Stmt_List block, Range1DI32 loc) {
	Ast_Statement *stmt = ast_statement_alloc(parser->arena);
	
	stmt->kind       = Ast_Statement_Kind_BLOCK;
	stmt->stmt_first = block.first;
	stmt->stmt_last  = block.last;
	stmt->location   = loc;
	
	return stmt;
}

internal Ast_Statement *make_return_stmt(Parser *parser, Expr_List retvals, Range1DI32 loc) {
	Ast_Statement *stmt = ast_statement_alloc(parser->arena);
	
	stmt->kind       = Ast_Statement_Kind_RETURN;
	stmt->expr_first = retvals.first;
	stmt->expr_last  = retvals.last;
	stmt->location   = loc;
	
	return stmt;
}

#if 0
internal Expr_List expr_list_join(Parser *parser, Expr_List a, Expr_List b) {
	Ast_Expression *sep = ast_expression_alloc(parser->arena);
	memset(sep, 0, sizeof(*sep));
	
	sep->prev = a.last;
	if (!check_nil_expression(a.last)) {
		a.last->next = sep;
	}
	
	sep->next = b.first;
	if (!check_nil_expression(b.first)) {
		b.first->prev = sep;
	}
	
	return a;
}
#else
internal Expr_List expr_list_join(Expr_List a, Expr_List b) {
	if (!check_nil_expression(a.last))
		a.last->next = b.first;
	if (!check_nil_expression(b.first))
		b.first->prev = a.last;
	a.last = b.last;
	return a;
}
#endif

internal Ast_Statement *make_assignment_stmt(Parser *parser, Expr_List lhs, Expr_List rhs, Token_Kind assigner, Range1DI32 loc) {
	Ast_Statement *stmt = ast_statement_alloc(parser->arena);
	
	// Expr_List joined = expr_list_join(parser, lhs, rhs);
	Expr_List joined = expr_list_join(lhs, rhs);
	
	stmt->kind       = Ast_Statement_Kind_ASSIGNMENT;
	stmt->assigner   = assigner;
	stmt->expr_first = joined.first;
	stmt->expr_last  = joined.last;
	stmt->expr_sep   = rhs.first;
	stmt->location   = loc;
	
	return stmt;
}

internal Ast_Statement *make_decl_stmt(Parser *parser, Ast_Declaration *decl, Range1DI32 loc) {
	Ast_Statement *stmt = ast_statement_alloc(parser->arena);
	
	stmt->kind       = Ast_Statement_Kind_DECLARATION;
	stmt->decl       = decl;
	stmt->location   = loc;
	
	return stmt;
}

internal Ast_Statement *parse_statement(Parser *parser) {
	Ast_Statement *result = &nil_statement;
	
	bool require_semicolon = true;
	
	Token token = peek_token(&parser->lexer);
	if (token.kind == '{') {
		consume_token(&parser->lexer); // {
		
		Range1DI32 lbrace_location = token.loc;
		Range1DI32 rbrace_location = {0};
		
		// Parse statement list inside the braces
		Ast_Statement *first = NULL;
		Ast_Statement *last  = NULL;
		
		token = peek_token(&parser->lexer);
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
				
				token = peek_token(&parser->lexer);
				if (token.kind == '}') {
					rbrace_location = token.loc;
					
					// We set it to &nil_statement always for now, so that {;} works.
					// TODO: Review :NilStatements
					if (first == NULL) {
						first = &nil_statement;
					}
					
					consume_token(&parser->lexer);
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
			Stmt_List list = {first, last};
			result = make_block_stmt(parser, list, range1di32_merge(lbrace_location, rbrace_location));
		}
		
		require_semicolon = false;
	} else if (token.keyword == KEYWORD_RETURN) {
		consume_token(&parser->lexer); // return
		
		Range1DI32 location = token.loc;
		
		// Parse return values: either nothing, or an expression list.
		//
		// Do not require an expression to begin with (it could be a "void" return),
		// but require one more expression after each comma.
		Expr_List retvals = parse_expr_list(parser, Parse_Flags_RETURN_VALS);
		result = make_return_stmt(parser, retvals, location);
	} else {
		
#if 1
		// Do *NOT* consume the first token as it will be part of the first expression/lhs.
		
		Range1DI32 location = token.loc;
		
		Ast_Statement_Kind kind = Ast_Statement_Kind_EXPR;
		
		// Parse an expression list, stopping at any 'assigner' or 'declarator' token,
		// or when there are no more expressions.
		//
		// Do not require an expression to begin with (it could be the empty statement),
		// but require one more expression after each comma.
		Expr_List lhs = parse_expr_list(parser, 0);
		
		token = peek_token(&parser->lexer);
		if (token_is_assigner(token))   kind = Ast_Statement_Kind_ASSIGNMENT;
		if (token_is_declarator(token)) kind = Ast_Statement_Kind_DECLARATION;
		
		if (kind == Ast_Statement_Kind_EXPR) {
			if (lhs.first != lhs.last)
				report_parse_error(parser, "Expression lists are not allowed at the statement level");
			
			result = make_expr_stmt(parser, lhs.first, location);
		} else if (kind == Ast_Statement_Kind_ASSIGNMENT) {
			// The parse_expression flags don't include REQUIRED because we want to allow
			// the empty statement. This means that in order to check wether there were any expressions
			// on the left of the assigner, check the root against NULL/nil_expression
			// :AssignToNothing
			if (check_nil_expression(lhs.first)) {
				report_parse_error(parser, "Cannot assign to nothing. At least one expression must be on the left of the assignment");
			}
			
			Token assigner = peek_token(&parser->lexer);
			consume_token(&parser->lexer); // assigner
			
			// Parse assignment right-hand-side, stopping when there are no more expressions.
			//
			// At least one is required, and every comma requires another one after, so
			// they are all required. The list ends when there are no more commas after
			// the expression.
			Expr_List rhs = parse_expr_list(parser, Parse_Flags_EXPR_REQUIRED);
			result = make_assignment_stmt(parser, lhs, rhs, assigner.kind, assigner.loc);
		} else if (kind == Ast_Statement_Kind_DECLARATION) {
			// See above comment about this check
			// :AssignToNothing
			if (check_nil_expression(lhs.first)) {
				report_parse_error(parser, "Cannot declare nothing. At least one identifier must be on the left of the declaration");
			}
			
			Token declarator = token;
			
			Ast_Declaration *decl = &nil_declaration;
			
			// Determine the kind of declaration
			bool parse_initializers = false;
			Ast_Expression *type_annotation = NULL;
			Ast_Declaration_Flags flags = 0;
			
			if (declarator.kind == ':') {
				consume_token(&parser->lexer); // :
				
				// ':' alone (NOT ':=' or '::') means there MUST be an explicit type annotation,
				// so we parse that.
				//
				// We don't pass the REQUIRED flag so that we can print a customized
				// error message later.
				
				type_annotation = parse_expression(parser, PREC_NONE, 0);
				if (!check_nil_expression(type_annotation)) { // There was a type annotation
					flags |= Ast_Declaration_Flag_TYPE_ANNOTATION;
				}
				
				token = peek_token(&parser->lexer);
				if (token.kind == '=' || token.kind == ':') {
					consume_token(&parser->lexer); // = or :
					parse_initializers = true;
					
					if (token.kind == ':') flags |= Ast_Declaration_Flag_CONSTANT;
				} else if ((flags & Ast_Declaration_Flag_TYPE_ANNOTATION) == 0) {
					// The declarator was : (not := nor ::) and there is no initializer:
					// Report an error
					
					report_parse_error(parser, "Expected type annotation after :");
				}
				
			} else if (declarator.kind == TOKEN_COLON_EQUALS ||
					   declarator.kind == TOKEN_DOUBLE_COLON) {
				consume_token(&parser->lexer); // := or ::
				parse_initializers = true;
				
				if (declarator.kind == TOKEN_DOUBLE_COLON) flags |= Ast_Declaration_Flag_CONSTANT;
			} else {
				panic("Attempt to parse a declaration but 'declarator' was not a declarator");
			}
			
			{
				// Build the declaration: either by parsing the initializers,
				// or by looking at the type annotation.
				
				decl = ast_declaration_alloc(parser->arena);
				decl->flags           = flags;
				decl->expr_first      = lhs.first;
				decl->expr_last       = lhs.last;
				if (!check_nil_expression(type_annotation)) {
					dll_push_back_npz(decl->expr_first, decl->expr_last, type_annotation, next, prev, check_nil_expression, set_nil_expression);
				}
				
				if (parse_initializers) {
					
					Expr_List rhs = parse_expr_list(parser, Parse_Flags_EXPR_REQUIRED);
					
					{
						// Concatenate the lists
						// TODO: dll_cat & dll_cat_npz
						
						if (!check_nil_expression(decl->expr_last))
							decl->expr_last->next = rhs.first;
						if (!check_nil_expression(rhs.first))
							rhs.first->prev = decl->expr_last;
						
						decl->expr_sep = rhs.first;
					}
					
					// Do *NOT* report an error if ident_count != count: it's not the number of initializers
					// that matters, but the number of values. One expression could yield multiple values,
					// e.g. a function call with multiple returns.
				} else {
					assert((flags & Ast_Declaration_Flag_TYPE_ANNOTATION) || there_were_parse_errors(parser));
					
					// The declaration only has a type annotation, without initializers.
					// Leave rhs empty.
				}
			}
			
			result = make_decl_stmt(parser, decl, declarator.loc);
			
			for (Ast_Expression *rhs = decl->expr_sep; !check_nil_expression(rhs); rhs = rhs->next) {
				if (check_nil_expression(rhs->next)) { // This is the last
#if 0
					if (rhs->kind == Ast_Expression_Kind_PROC_DEFN) {
						require_semicolon = false;
					}
#endif
				}
			}
			
#endif
			
		} else {
			panic("Invalid assignment to the statement kind");
		}
		
		allow_break();
	}
	
	if (require_semicolon)
		expect_token_kind(parser, TOKEN_SEMICOLON, "Expected ; after statement");
	
	if (there_were_parse_errors(parser)) {
		result = &nil_statement; // TODO: @Leak, @Hack
	}
	
	assert(result != NULL && result->next != NULL);
	return result;
}

//- Parser: Declarations

#if 0
internal Ast_Declaration *parse_decl(Parser *parser, Parse_Flags parse_flags) {
	Ast_Declaration *result = &nil_declaration;
	
	// TODO: Parse possible "attribute" or something
	
	Expr_List lhs = parse_expr_list(parser, 0);
	
	
	return result;
}
#endif

#if 0
internal Ast_Declaration *parse_declaration_after_lhs(Parser *parser, Ast_Expression *lhs) {
	Ast_Declaration *result = &nil_declaration;
	
	// Determine the kind of declaration
	bool     parse_initializers = false;
	Type_Ann   *type_annotation = NULL;
	Ast_Declaration_Flags flags = 0;
	
	Token token = peek_token(&parser->lexer);
	if (token.kind == ':') {
		consume_token(&parser->lexer); // :
		
		// ':' alone (NOT ':=' or '::') means there MUST be an explicit type annotation,
		// so we parse that.
		// TODO: For now the only valid type annotations are identifiers.
		
		Token next_token = peek_token(&parser->lexer);
		if (next_token.kind == TOKEN_IDENT ||
			next_token.kind == '^' ||
			next_token.kind == '[') {
			
			flags |= Ast_Declaration_Flag_TYPE_ANNOTATION;
			type_annotation = parse_type_annotation(parser, parser->arena);
		} else {
			report_parse_error(parser, "Expected type annotation after :");
		}
		
		next_token = peek_token(&parser->lexer);
		if (next_token.kind == '=' || next_token.kind == ':') {
			consume_token(&parser->lexer); // = or :
			parse_initializers = true;
			
			if (next_token.kind == ':') flags |= Ast_Declaration_Flag_CONSTANT;
		}
	} else if (token.kind == TOKEN_COLON_EQUALS ||
			   token.kind == TOKEN_DOUBLE_COLON) {
		consume_token(&parser->lexer); // := or ::
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
		result->lhs             = lhs;
		result->type_annotation = type_annotation;
		
		if (parse_initializers) {
			
			result->rhs = parse_expression(parser, parser->arena, PREC_NONE, Parse_Expr_Flags_REQUIRED|Parse_Expr_Flags_ALLOW_COMMA);
			
			// Do *NOT* report an error if ident_count != count: it's not the number of initializers
			// that matters, but the number of values. One expression could yield multiple values,
			// e.g. a function call with multiple returns.
		} else {
			assert((flags & Ast_Declaration_Flag_TYPE_ANNOTATION) || there_were_parse_errors(parser));
			
			// The declaration only has a type annotation, without initializers.
			// Leave rhs empty.
		}
	}
	
	if (there_were_parse_errors(parser)) {
		result = &nil_declaration; // TODO: @Leak, @Hack
	}
	
	return result;
}
#endif

#if 0
internal Ast_Expression *parse_declaration_rhs(Parser *parser) {
	Initter result = nil_initter;
	
	Token token = peek_token(&parser->lexer);
	if (token.keyword == KEYWORD_PROC) {
		// Starting with the keyword 'proc', this is a
		// proc definition OR a proc TYPE definition.
		
		consume_token(&parser->lexer); // proc
		
		result.kind = Initter_Kind_PROCEDURE_TYPE;
		result.first_param = parse_proc_header(parser);
		
		token = peek_token(&parser->lexer);
		if (token.kind == TOKEN_TRIPLE_DASH) {
			// This is a procedure prototype.
			consume_token(&parser->lexer); // ---
			
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
		
		consume_token(&parser->lexer); // struct
		
		result.kind = Initter_Kind_STRUCT;
		// result.initializer.first_member = parse_struct_definition(parser);
		
	} else {
		// Starting in any other way, we treat is as a generic expression,
		// and we figure out later what exactly that is.
		
		result.kind = Initter_Kind_EXPR;
		result.expr = parse_expression(parser, parser->arena, PREC_NONE, Parse_Expr_Flags_REQUIRED);
	}
	
	// TODO: 'distinct'
	// [
	// ^
	// 'enum'
	// 'union'
	
	return result;
}
#endif

// TODO: This will be called by parse_type_annotation...
#if 0
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
			token = peek_token(&parser->lexer);
			if (token.kind == TOKEN_IDENT) {
				consume_token(&parser->lexer); // ident
				string_list_push(scratch.arena, &arg_names, lexeme_from_token(&parser->lexer, token));
			} else {
				report_parse_error(parser, "Unexpected token");
			}
			
			token = peek_token(&parser->lexer);
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
			consume_token(&parser->lexer); // :
			
			token = peek_token(&parser->lexer);
			if (token.kind == TOKEN_IDENT) {
				consume_token(&parser->lexer); // type ident
				
				type_annotation_present = true;
				type_annotation.ident = lexeme_from_token(&parser->lexer, token);
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
		
		token = peek_token(&parser->lexer);
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
	
	Token token = peek_token(&parser->lexer);
	if (token.kind == TOKEN_FORWARD_ARROW) {
		consume_token(&parser->lexer); // ->
		
		String_List ret_types = {0};
		
		for (;;) {
			token = peek_token(&parser->lexer);
			if (token.kind == TOKEN_IDENT) {
				consume_token(&parser->lexer); // type ident
				string_list_push(scratch.arena, &ret_types, lexeme_from_token(&parser->lexer, token));
			}
			
			token = peek_token(&parser->lexer);
			if (token.kind == ',') {
				consume_token(&parser->lexer);
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
#endif

////////////////////////////////
//~ Program

internal Ast_Statement *parse_program(Parser *parser) {
	Ast_Statement *result = &nil_statement;
	
	Ast_Statement *first = NULL;
	Ast_Statement *last  = NULL;
	
	for (Token token = peek_token(&parser->lexer); token.kind != TOKEN_EOI; token = peek_token(&parser->lexer)) {
		Ast_Statement *stmt = parse_statement(parser);
		if (check_nil_statement(stmt)) break;
		
		queue_push_nz(first, last, stmt, next, check_nil_statement, set_nil_statement);
	}
	
	if (first != NULL) result = first;
	
	consume_all_tokens(&parser->lexer);
	return result;
}

////////////////////////////////
//~ Context

internal void expect_token_kind(Parser *parser, Token_Kind kind, char *message) {
	if (peek_token(&parser->lexer).kind != kind)
		report_parse_error(parser, message);
	consume_token(&parser->lexer);
}

internal void report_parse_error(Parser *parser, char *message) {
	if (parser->error_count < max_printed_parse_errors) {
		// String span = lexeme_from_token(&parser->lexer, parser->lexer->token);
		fprintf(stderr, "Syntax error (%i..%i): %s.\n", parser->lexer.token.loc.start, parser->lexer.token.loc.end, message);
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
	} else {
		parser->error_count += 1;
	}
}

internal bool there_were_parse_errors(Parser *parser) {
	return parser->error_count > 0;
}

internal void parser_init(Parser *parser, Arena *arena, String source, String name) {
	assert(arena_initted(*arena), "Ast arena must be manually initialized");
	
	memset(parser, 0, sizeof(*parser));
	parser->lexer.source_name = name;
	parser->arena = arena;
	
	if (source.len > 0) {
		parser_set_source(parser, source);
	}
}

internal void parser_set_source(Parser *parser, String source) {
	assert(parser->arena && arena_initted(*parser->arena), "Parser not initialized when setting source");
	
	u64 ast_size_cap = estimate_ast_arena_size(source);
	
	if (parser->arena->cap < ast_size_cap) {
		arena_fini(parser->arena);
		arena_init(parser->arena, .reserve_size = ast_size_cap);
	}
	
	parser->lexer.source = source;
}

internal u64 estimate_ast_arena_size(String source) {
	u64 max_node_size = max(sizeof(Ast_Expression), max(sizeof(Ast_Statement), sizeof(Ast_Declaration)));
	u64 ast_size_cap  = max_node_size * source.len;
	return max(ast_size_cap, DEFAULT_ARENA_RESERVE_SIZE);
}

//- Wrappers

internal Ast_Expression *expr_from_string(Arena *arena, String source, String name, Parse_Flags flags) {
	Parser parser = {0};
	parser_init(&parser, arena, source, name);
	
	Arena_Restore_Point temp = arena_begin_temp_region(arena);
	Ast_Expression *expr = parse_expression(&parser, PREC_NONE, flags);
	
	if (there_were_parse_errors(&parser)) {
		arena_end_temp_region(temp);
		expr = &nil_expression;
	}
	
	return expr;
}

internal Ast_Statement *stmt_from_string(Arena *arena, String source, String name) {
	Parser parser = {0};
	parser_init(&parser, arena, source, name);
	
	Arena_Restore_Point temp = arena_begin_temp_region(arena);
	Ast_Statement *stmt = parse_statement(&parser);
	
	if (there_were_parse_errors(&parser)) {
		arena_end_temp_region(temp);
		stmt = &nil_statement;
	}
	
	return stmt;
}

#if 0
internal Ast_Declaration *decl_from_string(Arena *arena, String source, String name) {
	Parser parser = {0};
	parser_init(&parser, arena, source, name);
	
	Arena_Restore_Point temp = arena_begin_temp_region(arena);
	Ast_Declaration *decl = parse_declaration(&parser);
	
	if (there_were_parse_errors(&parser)) {
		arena_end_temp_region(temp);
		decl = &nil_declaration;
	}
	
	return decl;
}
#endif

#if 0
internal Ast_Statement *unit_from_string(Arena *arena, String source, String name) {
	Parser parser = {0};
	parser_init(&parser, arena, source, name);
	
	return parse_program(&parser);
}
#endif

#endif
