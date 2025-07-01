#ifndef EL_CHECK_SINGLE_PASS_C
#define EL_CHECK_SINGLE_PASS_C

internal bool type_is_atomic(Type type) {
	Type_Kind k = type.kind;
	return k == Type_Kind_VOID || k == Type_Kind_BOOLEAN || k == Type_Kind_INTEGER || k == Type_Kind_STRING;
}

typedef struct Type_Array Type_Array;
struct Type_Array {
	Type *data;
	i64   count;
};

internal Type_Array get_inferred_types(Typechecker *checker, Type type) {
	Type_Array result = {0};
	
	if (type_is_atomic(type)) {
		result.count = 1;
		result.data  = push_array(checker->arena, Type, 1);
		result.data[0] = type;
	} else {
		unimplemented();
	}
	
	return result;
}

internal Type make_type_from_proc_defn(Typechecker *checker, Ast_Declaration *first_param, Ast_Statement *body) {
	Type type = {0};
	type.kind = Type_Kind_PROC;
	
#if 0 // No params for now
	int param_count = 0;
	for (Ast_Declaration *param = first_param; !check_nil_declaration(param); param = param->next) {
		param_count += param->entity_count;
	}
	
	type->param_count = param_count;
	type->params = push_array(checker->arena, Type, param_count);
	
	int param_index = 0;
	for (Ast_Declaration *param = first_param; !check_nil_declaration(param); param = param->next) {
		param_index += 1;
	}
#endif
	
	return type;
}

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

internal void declare_symbol(Typechecker *checker, Entity entity, Type type) {
	Scope *inner = checker->symbol_table.current_scope_stack->inner;
	
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
		entry->type = type;
	}
}

internal Symbol *lookup_symbol(Typechecker *checker, String ident) {
	Scope *inner = checker->symbol_table.current_scope_stack->inner;
	
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

internal void typecheck_expr(Typechecker *checker, Ast_Expression *expr) {
	assert(!check_nil_expression(expr));
	
	switch (expr->kind) {
		case Ast_Expression_Kind_INT_LITERAL: {
			expr->type.kind = Type_Kind_INTEGER;
		} break;
		
		case Ast_Expression_Kind_STRING_LITERAL: {
			expr->type.kind = Type_Kind_STRING;
		} break;
		
		case Ast_Expression_Kind_IDENT: {
			Symbol *entry = lookup_symbol(checker, expr->ident);
			if (entry != NULL) {
				expr->type = entry->type;
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
					if (expr->subexpr->next != &nil_expression) {
						report_type_error(checker, "Cannot apply + or - to multiple expressions");
						ok = false;
					}
					
					if (ok && expr->subexpr->type.kind != Type_Kind_INTEGER) {
						report_type_error(checker, "Cannot apply + or - to this type");
						ok = false;
					}
					
					if (ok) {
						expr->type.kind = Type_Kind_INTEGER;
					}
				} break;
				
				case Unary_Operator_DEREFERENCE: {
					// Same as above.
					
					bool ok = true;
					if (expr->subexpr->next != &nil_expression) {
						report_type_error(checker, "Cannot apply ^ to multiple expressions");
						ok = false;
					}
					
					if (ok && expr->subexpr->type.kind != Type_Kind_POINTER) {
						report_type_error(checker, "Cannot apply ^ to this type");
						ok = false;
					}
					
					if (ok) {
						expr->type = *expr->subexpr->type.pointed;
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
					
					if (ok && expr->left->type.kind != Type_Kind_INTEGER && expr->left->type.kind != Type_Kind_UNKNOWN &&
						expr->right->type.kind != Type_Kind_INTEGER && expr->right->type.kind != Type_Kind_UNKNOWN) {
						report_type_error(checker, "Cannot apply operator to this type");
						ok = false;
					}
					
					if (ok) {
						expr->type.kind = Type_Kind_INTEGER;
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

internal void enter_scope(Typechecker *checker) {
	Scope *new_inner = push_type(checker->arena, Scope);
	stack_push_n(checker->symbol_table.current_scope_stack->inner, new_inner, parent);
}

internal void leave_scope(Typechecker *checker) {
	Scope *old_inner = checker->symbol_table.current_scope_stack->inner; // @Leak
	stack_pop_nz(checker->symbol_table.current_scope_stack->inner, parent, check_null);
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
		enter_scope(checker);
		
		for (Ast_Statement *substat = stat->block; !check_nil_statement(substat); substat = substat->next) {
			typecheck_stat(checker, substat);
		}
		
		leave_scope(checker);
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
			
			if (decl->initters[i].expr->type.kind != Type_Kind_UNKNOWN) {
				// Initializer expression has a type.
				
				Type_Array types = get_inferred_types(checker, decl->initters[i].expr->type);
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
				enter_scope(checker);
				
				// TODO: Check params
				
				Ast_Statement *body = decl->initters[i].body;
				for (Ast_Statement *stat = body->block; !check_nil_statement(stat); stat = stat->next) {
					typecheck_stat(checker, stat);
				}
				
				leave_scope(checker);
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
	
	checker->first_decl = prog;
	checker->arena = &arena;
	
	// Init global scope
	checker->symbol_table.current_scope_stack = push_type(checker->arena, Scope_Stack);
	checker->symbol_table.current_scope_stack->inner = push_type(checker->arena, Scope);
	
	// Typecheck
	for (Ast_Declaration *decl = checker->first_decl; !check_nil_declaration(decl); decl = decl->next) {
		typecheck_decl(checker, decl);
	}
	
	return;
}

#endif
