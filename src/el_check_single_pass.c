#ifndef EL_CHECK_SINGLE_PASS_C
#define EL_CHECK_SINGLE_PASS_C

////////////////////////////////
//~ Error reporting

global i64 max_printed_type_errors = I64_MAX;

internal void report_type_error(Typechecker *checker, char *message) {
	if (checker->error_count < max_printed_type_errors) {
		fprintf(stderr, "Error: %s.\n", message);
	}
	checker->error_count += 1;
}

internal void report_type_errorf(Typechecker *checker, char *format, ...) {
	va_list args;
	va_start(args, format);
	Scratch scratch = scratch_begin(0, 0);
	
	String formatted_message = push_stringf_va_list(scratch.arena, format, args);
	report_type_error(checker, cstring_from_string(scratch.arena, formatted_message));
	
	scratch_end(scratch);
	va_end(args);
}

////////////////////////////////
//~ Scope building

internal void enter_nested_scope(Typechecker *checker) {
	Scope *scope = checker->symbol_table.current_scope;
	
	Scope *new_scope  = push_type(checker->arena, Scope);
	new_scope->parent = scope;
	
	String temp[] = {new_scope->parent->name, string_from_lit_const("+")};
	new_scope->name   = strings_concat(checker->name_arena, temp, array_count(temp));
	dll_push_back_npz(scope->first_child, scope->last_child, new_scope, next_sibling, prev_sibling, check_null, set_null);
	
	return;
}

internal void leave_nested_scope(Typechecker *checker) {
	if (checker->symbol_table.current_scope != checker->symbol_table.global_scope) {
		checker->symbol_table.current_scope = checker->symbol_table.current_scope->parent;
	} else {
		panic("Invalid codepath, tried to leave global scope");
	}
	
	return;
}

internal void enter_procedure_scope(Typechecker *checker, String name) {
	Scope *global_scope = checker->symbol_table.global_scope;
	Scope *scope = checker->symbol_table.current_scope;
	
	Scope *new_scope  = push_type(checker->arena, Scope);
	new_scope->parent = global_scope;
	new_scope->lexical_parent = scope;
	new_scope->name   = name;
	dll_push_back_npz(global_scope->first_child, global_scope->last_child, new_scope, next_sibling, prev_sibling, check_null, set_null);
	
	stack_push_n(checker->symbol_table.check_top, scope, check_next);
	checker->symbol_table.current_scope = new_scope;
	
	return;
}

internal void leave_procedure_scope(Typechecker *checker) {
	Scope *scope = checker->symbol_table.check_top;
	stack_pop_nz(checker->symbol_table.check_top, check_next, check_null);
	
	checker->symbol_table.current_scope = scope;
	
	return;
}

////////////////////////////////
//~ Symbol resolving

internal void declare_symbol(Typechecker *checker, Entity entity, Type type) {
	Scope *inner = checker->symbol_table.current_scope;
	
	bool can_declare = true;
	for (Symbol *entry = inner->first_symbol; entry != NULL; entry = entry->next) {
		if (string_equals(entry->ident, entity.ident)) {
			// Found! You'd be redefining the same symbol, so report an error instead
			report_type_errorf(checker, "Redefinition of identifier '%.*s'", string_expand(entity.ident));
			
			can_declare = false;
			break;
		}
	}
	
	if (can_declare) {
		Symbol *entry = push_type(checker->arena, Symbol);
		queue_push(inner->first_symbol, inner->last_symbol, entry);
		
		entry->ident = entity.ident;
		entry->type  = type;
	}
}

internal Symbol *lookup_symbol(Typechecker *checker, String ident) {
	Scope *inner = checker->symbol_table.current_scope;
	
	Symbol *result = NULL;
	for (Scope *scope = inner; scope != NULL; scope = scope->parent) {
		for (Symbol *entry = scope->first_symbol; entry != NULL; entry = entry->next) {
			if (string_equals(entry->ident, ident)) {
				result = entry;
				goto found_label;
			}
		}
	}
	found_label:;
	
	return result;
}

////////////////////////////////
//~ Typechecking

internal void typecheck_expr(Typechecker *checker, Ast_Expression *expr) {
	assert(!check_nil_expression(expr));
	
	switch (expr->kind) {
		case Ast_Expression_Kind_INT_LITERAL: {
			expr->types = push_type_array(checker->arena, 1);
			expr->types.data[0].atoms[0].kind = TYPE_INTEGER;
		} break;
		
		case Ast_Expression_Kind_STRING_LITERAL: {
			expr->types = push_type_array(checker->arena, 1);
			expr->types.data[0].atoms[0].kind = TYPE_STRING;
		} break;
		
		case Ast_Expression_Kind_IDENT: {
			Symbol *entry = lookup_symbol(checker, expr->ident);
			if (entry != NULL) {
				expr->types = push_type_array(checker->arena, 1);
				expr->types.data[0] = entry->type;
			} else {
				report_type_errorf(checker, "Undeclared identifier '%.*s'", string_expand(expr->ident));
			}
		} break;
		
		case Ast_Expression_Kind_UNARY: {
			assert(!check_nil_expression(expr->subexpr));
			typecheck_expr(checker, expr->subexpr);
			
			switch (expr->unary) {
				case Unary_Operator_PLUS:
				case Unary_Operator_MINUS: {
					// These operators only work on single expressions, not lists:
					// If the sub-expr is a list, it's an error.
					//
					// TODO: This is very unlikely to happen and it probabily means there's
					// a bug in the parser...
					
					bool ok = true;
					if (!check_nil_expression(expr->subexpr->next)) {
						report_type_error(checker, "Cannot apply + or - to multiple expressions");
						ok = false;
					}
					
					if (ok && expr->subexpr->types.count > 1) {
						report_type_error(checker, "Cannot apply + or - to an expression with multiple types");
						ok = false;
					}
					
					if (ok && type_is_atom_kind(expr->subexpr->types.data[0], TYPE_INTEGER)) {
						report_type_error(checker, "Cannot apply + or - to this type");
						ok = false;
					}
					
					if (ok) {
						expr->types = expr->subexpr->types;
					}
				} break;
				
				case Unary_Operator_DEREFERENCE: {
					// Same as above.
					
					bool ok = true;
					if (expr->subexpr->next != &nil_expression) {
						report_type_error(checker, "Cannot apply ^ to multiple expressions");
						ok = false;
					}
					
					if (ok && expr->subexpr->types.count > 1) {
						report_type_error(checker, "Cannot apply ^to an expression with multiple types");
						ok = false;
					}
					
					if (ok && type_is_atom_kind(expr->subexpr->types.data[0], TYPE_POINTER)) {
						report_type_error(checker, "Cannot apply ^ to this type");
						ok = false;
					}
					
					if (ok) {
						expr->types = type_array_slice(expr->subexpr->types, 1, 2);
					}
				} break;
				
				default: break;
			}
			
		} break;
		
		case Ast_Expression_Kind_BINARY: {
			assert(!check_nil_expression(expr->left) && !check_nil_expression(expr->right));
			typecheck_expr(checker, expr->left);
			typecheck_expr(checker, expr->right);
			
			switch (expr->binary) {
				case Binary_Operator_PLUS:
				case Binary_Operator_MINUS:
				case Binary_Operator_TIMES:
				case Binary_Operator_DIVIDE:
				case Binary_Operator_MODULUS: {
					// These operators only work on single expressions, not lists:
					// If either left or right are lists, it's an error.
					//
					// TODO: This is very unlikely to happen and it probabily means there's
					// a bug in the parser...
					
					bool ok = true;
					if (expr->left->next != &nil_expression || expr->right->next != &nil_expression) {
						if (expr->left->next  != &nil_expression) {
							report_type_error(checker, "Too many expressions on the left of this operator");
						}
						
						if (expr->right->next != &nil_expression) {
							report_type_error(checker, "Too many expressions on the right of this operator");
						}
						
						ok = false;
					}
					
					// These operators only work on integers. If either side is not an integer
					// (but it has another known type), it's an error.
					//
					// TODO: Types here can never be UNKNOWN if we 'break;' right when we check
					// the result of previous analyse_expression()...
					
					if (ok && !type_is_atom_kind(expr->left->types.data[0], TYPE_INTEGER) && !type_is_atom_kind(expr->left->types.data[0], TYPE_UNKNOWN) &&
						!type_is_atom_kind(expr->right->types.data[0], TYPE_INTEGER) && !type_is_atom_kind(expr->right->types.data[0], TYPE_UNKNOWN)) {
						report_type_error(checker, "Cannot apply operator to this type");
						ok = false;
					}
					
					if (ok) {
						expr->types = type_array_slice(expr->left->types, 1, 2);
					}
				} break;
				
#if 0
				case Binary_Operator_CALL: {
					// TODO: For now, every call's left expr is an identifier
					
					Decl *found = NULL;
					for (Scope *s = scope; s != NULL; s = s->next) {
						for (Decl *decl = scope->procs->first; decl != NULL; decl = decl->next) {
							if (string_equals(decl->ident, expr->ident)) {
								found = decl;
								goto after_scope_loop;
							}
						}
					}
					
					after_scope_loop:;
					if (!found) {
						string_list_push(arena, unresolved, expr->ident);
					} else {
						result = typed_expression_from_ast_expression(expr);
						result->type_count = found->ret_type_count;
						result->types = push_array(arena, Type, result->type_count);
						for (i64 i = 0; i < result->type_count; i += 1) {
							result->types[0] = found->ret_types[0];
						}
						result->ident = expr->ident;
					}
				} break;
#endif
				
				default: break;
			}
			
		} break;
		
#if 0
		case Ast_Expression_Kind_TERNARY: {
			if (!analyse_expression(arena, expr->left,   scope, unresolved)) break;
			if (!analyse_expression(arena, expr->middle, scope, unresolved)) break;
			if (!analyse_expression(arena, expr->right,  scope, unresolved)) break;
			
			// The ternary operator only works on single expressions, not lists:
			// If either of the subexprs are lists, it's an error.
			//
			// TODO: This is very unlikely to happen and it probabily means there's
			// a bug in the parser...
			
			
			bool ok = true;
			if (expr->left->next != &nil_expression || expr->middle->next != &nil_expression || expr->right->next != &nil_expression) {
				// TODO: Better error messages...
				if (expr->left->next   != &nil_expression) {
					report_type_error(checker, "Too many expressions on the left of this operator");
				}
				
				if (expr->middle->next != &nil_expression) {
					report_type_error(checker, "Too many expressions in the middle of this operator");
				}
				
				if (expr->right->next  != &nil_expression) {
					report_type_error(checker, "Too many expressions on the right of this operator");
				}
				
				ok = false;
			}
			
			if (ok && (expr->left->type.kind != Type_Kind_BOOLEAN && expr->left->type.kind != Type_Kind_POINTER)) {
				report_type_error(checker, "Cannot apply operator to this type");
				ok = false;
			}
			
			if (ok && expr->middle->type.kind != expr->right->type.kind) { // TODO: Exact match, not only the kinds
				report_type_error(checker, "Types must match");
				ok = false;
			}
			
			if (ok) {
				expr->type = expr->middle->type;
			}
		} break;
#endif
		
		default: break;
	}
	
	return;
}

internal void typecheck_decl(Typechecker *checker, Ast_Declaration *decl);

internal void typecheck_stat(Typechecker *checker, Ast_Statement *stat) {
	assert(!check_nil_statement(stat));
	
	if (stat->kind == Ast_Statement_Kind_EXPR) {
		typecheck_expr(checker, stat->expr);
	} else if (stat->kind == Ast_Statement_Kind_RETURN) {
		if (!check_nil_expression(stat->expr))
			typecheck_expr(checker, stat->expr);
	} else if (stat->kind == Ast_Statement_Kind_BLOCK) {
		enter_nested_scope(checker);
		
		for (Ast_Statement *substat = stat->block; !check_nil_statement(substat); substat = substat->next) {
			typecheck_stat(checker, substat);
		}
		
		leave_nested_scope(checker);
	} else if (stat->kind == Ast_Statement_Kind_ASSIGNMENT) {
		
		for (Ast_Expression *lhs = stat->lhs; !check_nil_expression(lhs); lhs = lhs->next) {
			typecheck_expr(checker, lhs);
			
			// TODO: Check that it will evaluate to a memory location
		}
		
		for (Ast_Expression *rhs = stat->rhs; !check_nil_expression(rhs); rhs = rhs->next) {
			typecheck_expr(checker, rhs);
			
			// TODO: Check the type against the lhss types, and their count
		}
		
	} else if (stat->kind == Ast_Statement_Kind_DECLARATION) {
		typecheck_decl(checker, stat->decl);
	}
	
	return;
}

internal void typecheck_decl(Typechecker *checker, Ast_Declaration *decl) {
	assert(!check_nil_declaration(decl));
	
	int entities_done = 0;
	for (int i = 0; i < decl->initter_count; i += 1) {
		if (decl->initters[i].kind == Initter_Kind_EXPR) {
			assert(!check_nil_expression(decl->initters[i].expr));
			typecheck_expr(checker, decl->initters[i].expr);
			
			if (!type_is_atom_kind(decl->initters[i].expr->types.data[0], TYPE_UNKNOWN)) {
				// Initializer expression has a type.
				
				Type_Array types = decl->initters[i].expr->types;
				for (int e = entities_done; e < types.count; e += 1) {
					if (e >= decl->entity_count) {
						report_type_error(checker, "Too many initializers on the right side of the declaration");
						break;
					}
					
					declare_symbol(checker, decl->entities[e], types.data[e]);
				}
				
				entities_done += types.count;
			} else {
				assert(checker->error_count > 0, "Could not resolve the type of an expression, but no errors were reported");
			}
		} else if (decl->initters[i].kind == Initter_Kind_PROCEDURE) {
#if 1
			assert(!check_nil_statement(decl->initters[i].body));
			assert(decl->initters[i].body->kind == Ast_Statement_Kind_BLOCK);
			
			declare_symbol(checker, decl->entities[entities_done], make_type_from_proc_defn(checker, decl->initters[i].first_param, decl->initters[i].body));
			entities_done += 1;
			
			{
				enter_procedure_scope(checker, decl->entities[entities_done].ident);
				
				// TODO: Check params
				
				Ast_Statement *body = decl->initters[i].body;
				for (Ast_Statement *stat = body->block; !check_nil_statement(stat); stat = stat->next) {
					typecheck_stat(checker, stat);
				}
				
				leave_procedure_scope(checker);
			}
#else
			unimplemented();
#endif
		} else {
			unimplemented();
		}
	}
	
	return;
}

internal void do_all_checks(Ast_Declaration *prog) {
	Typechecker checker_ = {0};
	Typechecker *checker = &checker_;
	
	Arena arena = {0};
	arena_init(&arena);
	
	Arena name_arena = {0};
	arena_init(&name_arena);
	
	checker->first_decl = prog;
	checker->arena = &arena;
	checker->name_arena = &name_arena;
	
	// Init global scope
	checker->symbol_table.global_scope = push_type(checker->arena, Scope);
	checker->symbol_table.current_scope = checker->symbol_table.global_scope;
	
	// Typecheck
	for (Ast_Declaration *decl = checker->first_decl; !check_nil_declaration(decl); decl = decl->next) {
		typecheck_decl(checker, decl);
	}
	
	return;
}

#endif
