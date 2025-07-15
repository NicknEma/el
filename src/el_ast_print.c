#ifndef EL_AST_PRINT_C
#define EL_AST_PRINT_C

////////////////////////////////
//~ Expressions

internal String string_from_expression_tree(Arena *arena, Ast_Expression *root);
internal void print_expression_tree(Ast_Expression *root);

internal char char_from_unary_op(Unary_Operator op) {
	switch (op) {
		case Unary_Operator_PLUS:         return '+';
		case Unary_Operator_MINUS:        return '-';
		case Unary_Operator_DEREFERENCE:  return '^';
		default: panic("Invalid codepath");
	}
	
	return 0;
}

internal char char_from_binary_op(Binary_Operator op) {
	switch (op) {
		case Binary_Operator_PLUS:         return '+';
		case Binary_Operator_MINUS:        return '-';
		case Binary_Operator_TIMES:        return '*';
		case Binary_Operator_DIVIDE:       return '/';
		case Binary_Operator_MODULUS:      return '%';
		case Binary_Operator_TERNARY:      return '?';
		// case Binary_Operator_COMMA:        return ',';
		case Binary_Operator_MEMBER:       return '.';
		case Binary_Operator_CALL:         return '(';
		case Binary_Operator_ARRAY_ACCESS: return '[';
		default: panic("Invalid codepath");
	}
	
	return 0;
}

internal void string_from_declaration_tree_internal(Arena *arena, Ast_Declaration *root, String_List *builder);

internal void string_from_expression_tree_internal(Arena *arena, Ast_Expression *root, String_List *builder) {
	switch (root->kind) {
		case Ast_Expression_Kind_INT_LITERAL: {
			string_list_pushf(arena, builder, "%lld", root->i64_value);
		} break;
		
		case Ast_Expression_Kind_STRING_LITERAL: {
			string_list_push(arena, builder, root->string_value);
		} break;
		
		case Ast_Expression_Kind_BOOL_LITERAL: {
			string_list_push(arena, builder, root->bool_value ? string_from_lit("true") : string_from_lit("false"));
		} break;
		
		case Ast_Expression_Kind_COMPOUND_LITERAL: {
			// TODO: Print type annotation
			
			string_list_push(arena, builder, string_from_lit("{"));
			string_from_expression_tree_internal(arena, root->subexpr, builder);
			string_list_push(arena, builder, string_from_lit("}"));
		} break;
		
		case Ast_Expression_Kind_IDENT: {
			string_list_push(arena, builder, root->ident);
		} break;
		
		case Ast_Expression_Kind_UNARY: {
			switch (root->unary) {
				case Unary_Operator_PLUS:
				case Unary_Operator_MINUS: {
					string_list_pushf(arena, builder, "%c(", char_from_unary_op(root->unary));
					string_from_expression_tree_internal(arena, root->left, builder);
					string_list_push(arena, builder, string_from_lit(")"));
				} break;
				
				case Unary_Operator_DEREFERENCE: {
					string_list_push(arena, builder, string_from_lit("("));
					string_from_expression_tree_internal(arena, root->left, builder);
					string_list_pushf(arena, builder, ")^");
				} break;
				
				default: break;
			}
		} break;
		
		case Ast_Expression_Kind_BINARY: {
			string_list_push(arena, builder, string_from_lit("("));
			string_from_expression_tree_internal(arena, root->left, builder);
			string_list_pushf(arena, builder, ")%c(", char_from_binary_op(root->binary));
			
			if (root->binary != Binary_Operator_TERNARY) {
				string_from_expression_tree_internal(arena, root->right, builder);
			} else {
				string_list_push(arena, builder, string_from_lit("("));
				string_from_expression_tree_internal(arena, root->middle, builder);
				string_list_push(arena, builder, string_from_lit("):("));
				string_from_expression_tree_internal(arena, root->right, builder);
				string_list_push(arena, builder, string_from_lit(")"));
			}
			
			string_list_push(arena, builder, string_from_lit(")"));
		} break;
		
		default: break;
	}
	
	if (!check_nil_expression(root->next)) {
		string_list_push(arena, builder, string_from_lit(","));
		string_from_expression_tree_internal(arena, root->next, builder);
	}
}

internal String string_from_expression_tree(Arena *arena, Ast_Expression *root) {
	Scratch scratch = scratch_begin(&arena, 1);
	String_List builder = {0};
	
	string_from_expression_tree_internal(scratch.arena, root, &builder);
	String result = string_from_list(arena, builder);
	
	scratch_end(scratch);
	return result;
}

internal void print_expression_tree(Ast_Expression *root) {
	Scratch scratch = scratch_begin(0, 0);
	printf("%.*s\n", string_expand(string_from_expression_tree(scratch.arena, root)));
	
	scratch_end(scratch);
}

////////////////////////////////
//~ Statements

internal String string_from_statement_tree(Arena *arena, Ast_Statement *root);
internal void print_statement_tree(Ast_Statement *root);

internal void string_from_statement_tree_internal(Arena *arena, Ast_Statement *root, String_List *builder) {
	switch (root->kind) {
		case Ast_Statement_Kind_EXPR: {
			string_from_expression_tree_internal(arena, root->expr, builder);
		} break;
		
		case Ast_Statement_Kind_RETURN: {
			string_list_push(arena, builder, string_from_lit("return "));
			string_from_expression_tree_internal(arena, root->expr, builder);
		} break;
		
		case Ast_Statement_Kind_ASSIGNMENT: {
			string_from_expression_tree_internal(arena, root->lhs, builder);
			string_list_push(arena, builder, string_from_lit(" = "));
			string_from_expression_tree_internal(arena, root->rhs, builder);
		} break;
		
		case Ast_Statement_Kind_DECLARATION: {
			string_from_declaration_tree_internal(arena, root->decl, builder);
		} break;
		
		case Ast_Statement_Kind_BLOCK: {
			string_list_push(arena, builder, string_from_lit("{ "));
			
			for (Ast_Statement *stat = root->block; stat != NULL && stat != &nil_statement; stat = stat->next) {
				string_from_statement_tree_internal(arena, stat, builder);
			}
			
			string_list_push(arena, builder, string_from_lit("}"));
		} break;
		
		default: break;
	}
	
	if (root->kind != Ast_Statement_Kind_BLOCK) {
		string_list_push(arena, builder, string_from_lit("; "));
	}
}

internal String string_from_statement_tree(Arena *arena, Ast_Statement *root) {
	Scratch scratch = scratch_begin(&arena, 1);
	String_List builder = {0};
	
	string_from_statement_tree_internal(scratch.arena, root, &builder);
	String result = string_from_list(arena, builder);
	
	scratch_end(scratch);
	return result;
}

internal void print_statement_tree(Ast_Statement *root) {
	Scratch scratch = scratch_begin(0, 0);
	printf("%.*s\n", string_expand(string_from_statement_tree(scratch.arena, root)));
	
	scratch_end(scratch);
}

////////////////////////////////
//~ Declarations

internal String string_from_declaration_tree(Arena *arena, Ast_Declaration *root);
internal void print_declaration_tree(Ast_Declaration *root);

internal void string_from_declaration_tree_internal(Arena *arena, Ast_Declaration *root, String_List *builder) {
	string_from_expression_tree_internal(arena, root->lhs, builder);
	
	if (root->flags & Ast_Declaration_Flag_TYPE_ANNOTATION) {
		string_list_push(arena, builder, string_from_lit(":"));
		string_list_push(arena, builder, string_from_lit("<type>")); // TODO: Actual type
		string_list_push(arena, builder, string_from_lit("="));
	} else {
		string_list_push(arena, builder, string_from_lit(":="));
	}
	
	if (!check_nil_expression(root->rhs)) {
		string_from_expression_tree_internal(arena, root->rhs, builder);
	}
	
#if 0
	switch (root->entity) {
		case Ast_Declaration_Entity_PROCEDURE: {
			string_list_pushf(arena, builder, "%.*s :: (", string_expand(root->ident));
			
			// TODO: Parameter list
			
			string_list_push(arena, builder, string_from_lit(") "));
			
			string_from_statement_tree_internal(arena, root->body, builder);
		} break;
		
		default: break;
	}
#endif
	
	// string_list_push(arena, builder, string_from_lit("\n"));
}

internal String string_from_declaration_tree(Arena *arena, Ast_Declaration *root) {
	Scratch scratch = scratch_begin(&arena, 1);
	String_List builder = {0};
	
	string_from_declaration_tree_internal(scratch.arena, root, &builder);
	String result = string_from_list(arena, builder);
	
	scratch_end(scratch);
	return result;
}

internal void print_declaration_tree(Ast_Declaration *root) {
	Scratch scratch = scratch_begin(0, 0);
	printf("%.*s\n", string_expand(string_from_declaration_tree(scratch.arena, root)));
	
	scratch_end(scratch);
}

#if 0
internal int compute_expression_tree_width(Expression *root) {
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


internal void print_spaces(int count) {
	for (int i = 0; i < count; i += 1) putchar(' ');
}

internal void print_expression_tree(Expression *root) {
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
#endif

#endif
