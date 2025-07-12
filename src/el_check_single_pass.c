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
	if (checker->error_count < max_printed_type_errors) {
		va_list args;
		va_start(args, format);
		Scratch scratch = scratch_begin(0, 0);
		
		String formatted_message = push_stringf_va_list(scratch.arena, format, args);
		report_type_error(checker, cstring_from_string(scratch.arena, formatted_message));
		
		scratch_end(scratch);
		va_end(args);
	} else {
		checker->error_count += 1;
	}
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

internal void declare_symbol(Typechecker *checker, Entity *entity, Type_Id type, Symbol_Kind kind) {
	Scope *inner = checker->symbol_table.current_scope;
	
	bool can_declare = true;
	for (Symbol *entry = inner->first_symbol; entry != NULL; entry = entry->next) {
		if (string_equals(entry->ident, entity->ident)) {
			// Found! You'd be redefining the same symbol, so report an error instead
			report_type_errorf(checker, "Redefinition of identifier '%.*s'", string_expand(entity->ident));
			
			can_declare = false;
			break;
		}
	}
	
	if (can_declare) {
		Symbol *entry = push_type(checker->arena, Symbol);
		queue_push(inner->first_symbol, inner->last_symbol, entry);
		
		entry->ident = entity->ident;
		entry->type  = type;
		entry->kind  = kind;
		
		entity->symbol = entry;
	}
}

internal Symbol *lookup_symbol(Symbol_Table *table, String ident) {
	Scope *inner = table->current_scope;
	
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

internal void infer_expr_type(Typechecker *checker, Ast_Expression *expr) {
	assert(!check_nil_expression(expr));
	
	switch (expr->kind) {
		case Ast_Expression_Kind_INT_LITERAL: {
			expr->flags |= Ast_Expression_Flag_CONSTANT;
			
			expr->types = push_type_array(checker->arena, 1);
			expr->types.data[0] = Type_Id_UNTYPED_INT;
		} break;
		
		case Ast_Expression_Kind_STRING_LITERAL: {
			expr->flags |= Ast_Expression_Flag_CONSTANT;
			
			expr->types = push_type_array(checker->arena, 1);
			expr->types.data[0] = Type_Id_UNTYPED_STRING;
		} break;
		
		case Ast_Expression_Kind_BOOL_LITERAL: {
			expr->flags |= Ast_Expression_Flag_CONSTANT;
			
			expr->types = push_type_array(checker->arena, 1);
			expr->types.data[0] = Type_Id_UNTYPED_BOOL;
		} break;
		
		case Ast_Expression_Kind_IDENT: {
			Symbol *entry = lookup_symbol(&checker->symbol_table, expr->ident);
			if (entry != NULL) {
				expr->symbol = entry;
				expr->types = push_type_array(checker->arena, 1);
				expr->types.data[0] = entry->type;
			} else {
				report_type_errorf(checker, "Undeclared identifier '%.*s'", string_expand(expr->ident));
			}
		} break;
		
		case Ast_Expression_Kind_UNARY: {
			assert(!check_nil_expression(expr->subexpr));
			infer_expr_type(checker, expr->subexpr);
			
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
					
					Type *type = type_from_id(expr->subexpr->types.data[0]);
					
					if (ok && type->kind != TYPE_INTEGER) {
						report_type_error(checker, "Cannot apply + or - to this type");
						ok = false;
					}
					
					if (ok) {
						expr->types = expr->subexpr->types;
						
						if (expr->subexpr->flags & Ast_Expression_Flag_CONSTANT)
							expr->flags |= Ast_Expression_Flag_CONSTANT;
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
					
					Type *type = type_from_id(expr->subexpr->types.data[0]);
					
					if (ok && type->kind != TYPE_POINTER) {
						report_type_error(checker, "Cannot apply ^ to this type");
						ok = false;
					}
					
					if (ok) {
						expr->types = expr->subexpr->types;
					}
				} break;
				
				default: break;
			}
			
		} break;
		
		case Ast_Expression_Kind_BINARY: {
			assert(!check_nil_expression(expr->left) && !check_nil_expression(expr->right));
			infer_expr_type(checker, expr->left);
			infer_expr_type(checker, expr->right);
			
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
					
					Type *ltype = type_from_id(expr->left->types.data[0]);
					Type *rtype = type_from_id(expr->right->types.data[0]);
					
					if (ok && ltype->kind != TYPE_INTEGER && rtype->kind != TYPE_INTEGER) {
						report_type_error(checker, "Cannot apply operator to this type");
						ok = false;
					}
					
					// TODO: Check type sizes or convert
					
					if (ok) {
						expr->types = expr->left->types;
						
						if (expr->left->flags & expr->right->flags & Ast_Expression_Flag_CONSTANT)
							expr->flags |= Ast_Expression_Flag_CONSTANT;
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
		infer_expr_type(checker, stat->expr);
	} else if (stat->kind == Ast_Statement_Kind_RETURN) {
		if (!check_nil_expression(stat->expr))
			infer_expr_type(checker, stat->expr);
	} else if (stat->kind == Ast_Statement_Kind_BLOCK) {
		enter_nested_scope(checker);
		
		for (Ast_Statement *substat = stat->block; !check_nil_statement(substat); substat = substat->next) {
			typecheck_stat(checker, substat);
		}
		
		leave_nested_scope(checker);
	} else if (stat->kind == Ast_Statement_Kind_ASSIGNMENT) {
		
		for (Ast_Expression *lhs = stat->lhs; !check_nil_expression(lhs); lhs = lhs->next) {
			infer_expr_type(checker, lhs);
			
			// TODO: Check that it will evaluate to a memory location
		}
		
		for (Ast_Expression *rhs = stat->rhs; !check_nil_expression(rhs); rhs = rhs->next) {
			infer_expr_type(checker, rhs);
			
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
	for (int initter_index = 0; initter_index < decl->initter_count; initter_index += 1) {
		if (decl->initters[initter_index].kind == Initter_Kind_EXPR) {
			assert(!check_nil_expression(decl->initters[initter_index].expr));
			infer_expr_type(checker, decl->initters[initter_index].expr);
			
			Type_Array types = decl->initters[initter_index].expr->types;
			
			for (int type_index = 0, entity_index = entities_done; type_index < types.count; type_index += 1, entity_index += 1) {
				if (entity_index >= decl->entity_count) {
					report_type_error(checker, "Too many initializers on the right side of the declaration");
					break;
				}
				
				Type *type = type_from_id(types.data[type_index]);
				
				if (type->kind != TYPE_UNKNOWN) {  // Initializer expression has a type.
					
					Symbol_Kind symbol_kind = SYMBOL_NONE;
					if (type->kind == TYPE_TYPE) {
						symbol_kind = SYMBOL_TYPE;
					} else if (type->kind == TYPE_PROC) {
						symbol_kind = SYMBOL_PROC;
					} else {
						symbol_kind = SYMBOL_LOCAL_VAR;
						if (checker->symbol_table.current_scope == checker->symbol_table.global_scope) {
							symbol_kind = SYMBOL_GLOBAL_VAR;
							checker->symbol_table.global_var_count += 1;
						}
					}
					
					
					decl->entities[entities_done].initter = &decl->initters[initter_index];
					decl->entities[entities_done].initter_value_index = type_index;
					
					declare_symbol(checker, &decl->entities[entity_index], types.data[type_index], symbol_kind);
				} else {
					assert(checker->error_count > 0, "Could not resolve the type of an expression, but no errors were reported");
					break;  // Even if the assertion didn't fire, all the errors were already reported in infer_expr_type() so we can stop
				}
			}
			
			entities_done += types.count;
		} else if (decl->initters[initter_index].kind == Initter_Kind_PROCEDURE) {
			assert(!check_nil_statement(decl->initters[initter_index].body));
			assert(decl->initters[initter_index].body->kind == Ast_Statement_Kind_BLOCK);
			
			
			decl->entities[entities_done].initter = &decl->initters[initter_index];
			decl->entities[entities_done].initter_value_index = 0;
			
			Type proc_defn_type = make_proc_defn_type(checker, decl->initters[initter_index].first_param, decl->initters[initter_index].body);
			Type_Id id = define_type(proc_defn_type);
			
			declare_symbol(checker, &decl->entities[entities_done], id, SYMBOL_PROC);
			checker->symbol_table.proc_count += 1;
			
			{
				enter_procedure_scope(checker, decl->entities[entities_done].ident);
				
				// TODO: Check params
				
				Ast_Statement *body = decl->initters[initter_index].body;
				for (Ast_Statement *stat = body->block; !check_nil_statement(stat); stat = stat->next) {
					typecheck_stat(checker, stat);
				}
				
				leave_procedure_scope(checker);
			}
			
			entities_done += 1;
		} else {
			unimplemented();
		}
	}
	
	return;
}

#if 0
internal Entity_Group *group_entities(Arena *arena, Initter *initters, i64 initter_count) {
	Entity_Group *groups = push_array(arena, Entity_Group, initter_count);
	
	int entities_done = 0;
	for (int i = 0; i < initter_count; i += 1) {
		if (initters[i].kind == Initter_Kind_EXPR) {
			assert(!check_nil_expression(initters[i].expr));
			
			Type_Array types = initters[i].expr->types;
			
			groups[i].first = entities_done;
			groups[i].opl   = entities_done + types.count;
		} else {
			groups[i].first = entities_done;
			groups[i].opl   = entities_done + 1;
		}
		
		entities_done = groups[i].opl;
	}
	
	return groups;
}
#endif

////////////////////////////////
//~ Printing

internal void print_symbol(Symbol *symbol) {
	printf("%.*s: ", string_expand(symbol->ident));
	print_type(type_from_id(symbol->type));
	printf(" (%.*s)", string_expand(symbol_kind_names[symbol->kind]));
}

internal void print_scope(Scope *scope) {
	Scratch scratch = scratch_begin(0, 0);
	
	for (Symbol *symbol = scope->first_symbol; symbol != NULL; symbol = symbol->next) {
		print_indent();
		print_symbol(symbol);
		printf("\n");
	}
	
	inc_indent();
	for (Scope *child = scope->first_child; child != NULL; child = child->next_sibling) {
		print_scope(child);
	}
	dec_indent();
	
	scratch_end(scratch);
}

////////////////////////////////
//~ All

internal void typechecker_init(Typechecker *checker, Arena *arena, Arena *name_arena) {
	checker->arena = arena;
	checker->name_arena = name_arena;
	
	// Init global scope
	checker->symbol_table.global_scope = push_type(checker->arena, Scope);
	checker->symbol_table.current_scope = checker->symbol_table.global_scope;
	
	define_type(make_void_type(string_from_lit("void")));
	define_type(make_int_type(INT_SUBTYPE_UNTYPED, string_from_lit("untyped-int")));
	define_type(make_int_type(INT_SUBTYPE_UINT,    string_from_lit("uint")));
	define_type(make_int_type(INT_SUBTYPE_INT,     string_from_lit("int")));
	define_type((Type){.kind = TYPE_STRING, .string_subtype = STRING_SUBTYPE_UNTYPED, .name = string_from_lit("untyped-string")});
	define_type((Type){.kind = TYPE_BOOLEAN, .name = string_from_lit("untyped-bool")});
}

internal void do_all_checks(Typechecker *checker, Ast_Declaration *prog) {
	checker->first_decl = prog;
	
	// Typecheck
	for (Ast_Declaration *decl = checker->first_decl; !check_nil_declaration(decl); decl = decl->next) {
		typecheck_decl(checker, decl);
	}
	
	printf("Global var count: %d\nProc count: %d\n", checker->symbol_table.global_var_count, checker->symbol_table.proc_count);
	print_scope(checker->symbol_table.global_scope);
	printf("\n");
}

#endif
