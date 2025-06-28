#ifndef EL_CHECK_C
#define EL_CHECK_C

////////////////////////////////
//~ Errors

internal void
report_error(String message) {
	fprintf(stderr, "Error: %.*s\n", string_expand(message));
}

////////////////////////////////
//~ Scope and Symbols

internal Symbol *
symbol_alloc(Arena *arena) {
	Symbol *result = first_free_symbol;
	
	if (result != NULL) {
		stack_pop_nz(first_free_symbol, next_free, check_null);
		memset(result, 0, sizeof(Symbol));
	} else {
		result = push_type(arena, Symbol);
	}
	
	return result;
}

internal void
symbol_free(Symbol *symbol) {
	stack_push_n(first_free_symbol, symbol, next_free);
}

internal void
symbol_add_location_used(Arena *arena, Symbol *symbol, Location location) {
	(void) arena;
	
	assert(symbol->locations_used_count < array_count(symbol->locations_used));
	symbol->locations_used[symbol->locations_used_count] = location;
	symbol->locations_used_count += 1;
}


internal Scope *
scope_enter(Arena *arena, Scope *scope) {
	Scope *new_scope = push_type(arena, Scope);
	
	dll_push_back_npz(scope->first_child, scope->last_child, new_scope, next_sibling, prev_sibling, check_null, set_null);
	new_scope->parent = scope;
	
	return new_scope;
}

internal Scope *
scope_leave(Scope *scope) {
	return scope->parent;
}

internal Symbol *
scope_lookup(Scope *inner, String ident) {
	Symbol *result = NULL;
	
	for (Scope *current = inner; current != NULL; current = current->parent) {
		for (Symbol *entry = current->first_symbol; entry != NULL; entry = entry->next) {
			if (string_equals(entry->ident, ident)) {
				result = entry;
				goto found;
			}
		}
	}
	found:;
	
	return result;
}

internal Symbol *
scope_lookup_inner(Scope *scope, String ident) {
	Symbol *result = NULL;
	
	for (Symbol *entry = scope->first_symbol; entry != NULL; entry = entry->next) {
		if (string_equals(entry->ident, ident)) {
			result = entry;
			break;
		}
	}
	
	return result;
}

internal Symbol *
scope_insert_ident(Arena *arena, Scope *scope, String ident) {
	Symbol *entry = symbol_alloc(arena);
	
	dll_push_back(scope->first_symbol, scope->last_symbol, entry);
	entry->ident = ident;
	
	return entry;
}

////////////////////////////////
//~ Symbol table building

internal void
build_scope_for_expression(Arena *arena, Scope *scope, Ast_Expression *expr) {
	
	switch (expr->kind) {
		case Ast_Expression_Kind_LITERAL: break;
		
		case Ast_Expression_Kind_IDENT: {
			Symbol *symbol = scope_lookup(scope, expr->ident);
			if (symbol == NULL) {
				// We insert the symbol even if this is just a usage site.
				// We register the location_used, so later we can check if there was
				// at least one location_declared.
				symbol = scope_insert_ident(arena, scope, expr->ident);
			}
			
			symbol_add_location_used(arena, symbol, expr->location);
		} break;
		
		case Ast_Expression_Kind_UNARY: {
			build_scope_for_expression(arena, scope, expr->subexpr);
		} break;
		
		case Ast_Expression_Kind_BINARY: {
			build_scope_for_expression(arena, scope, expr->left);
			build_scope_for_expression(arena, scope, expr->right);
		} break;
		
		case Ast_Expression_Kind_TERNARY: {
			build_scope_for_expression(arena, scope, expr->left);
			build_scope_for_expression(arena, scope, expr->middle);
			build_scope_for_expression(arena, scope, expr->right);
		} break;
		
		default: break;
	}
	
	return;
}

internal void
build_scope_for_statement(Arena *arena, Scope *scope, Ast_Statement *stat) {
	stat->scope = scope;
	
	switch (stat->kind) {
		case Ast_Statement_Kind_EXPR: {
			build_scope_for_expression(arena, scope, stat->expr);
		} break;
		
		case Ast_Statement_Kind_RETURN: {
			build_scope_for_expression(arena, scope, stat->expr);
		} break;
		
		case Ast_Statement_Kind_BLOCK: {
			scope = scope_enter(arena, scope);
			
			for (Ast_Statement *curr = stat->block; curr != NULL && curr != &nil_statement; curr = curr->next) {
				build_scope_for_statement(arena, scope, curr);
			}
			
			scope = scope_leave(scope);
		} break;
		
		default: break;
	}
	
	return;
}

internal void
build_scope_for_declaration(Arena *arena, Scope *scope, Ast_Declaration *decl) {
	
	for (i64 i = 0; i < decl->entity_count; i += 1) {
		Entity e = decl->entities[i];
		
		Symbol *symbol = scope_lookup_inner(scope, e.ident);
		if (symbol == NULL) {
			symbol = scope_insert_ident(arena, scope, e.ident);
			symbol->location_declared = e.location;
		} else {
			report_error(string_from_lit("Redefinition of Disco"));
		}
		
		// TODO: Make this less of a hack
		symbol->decl = decl;
		symbol->entity_index = i;
	}
	
	for (i64 i = 0; i < decl->initter_count; i += 1) {
		switch (decl->initters[i].kind) {
			case Initter_Kind_PROCEDURE: {
				scope = scope_enter(arena, scope);
				
				// TODO:
				// for each decl in the procedure's formal parameter list:
				//   build_scope_for_declaration()
				
				build_scope_for_statement(arena, scope, decl->initters[i].body);
				
				scope = scope_leave(scope);
			} break;
			
			case Initter_Kind_STRUCT: {
				// TODO:
				// For each of the members, you need to add its type_annotation ident to the symbol table,
				// as a usage.
				// If its type_annotation is not an ident, I have no idea.
			} break;
			
			case Initter_Kind_PROCEDURE_TYPE: {
				// TODO:
				// For each of the params, you need to add its type_annotation ident to the symbol table,
				// as a usage.
				// If its type_annotation is not an ident, I have no idea.
			} break;
			
			case Initter_Kind_PROCEDURE_PROTO: {
				// TODO:
				// For each of the params, you need to add its type_annotation ident to the symbol table,
				// as a usage.
				// If its type_annotation is not an ident, I have no idea.
			} break;
			
			case Initter_Kind_EXPR: {
				build_scope_for_expression(arena, scope, decl->initters[i].expr);
			} break;
			
			default: break;
		}
	}
	
	return;
}

internal void
rearrange_scope(Arena *arena, Scope *scope) {
	for (Symbol *entry = scope->first_symbol; entry != NULL; entry = entry->next) {
		if (entry->locations_used_count > 1) {
			bool declared = !location_is_zero(entry->location_declared);
			i64  uses_before_decl = 0;
			for (; uses_before_decl < entry->locations_used_count; uses_before_decl += 1) {
				Location used = entry->locations_used[uses_before_decl];
				if (location_is_greater_than(used, entry->location_declared)) {
					break;
				}
			}
			
			// If it's not declared at all, then technically every use is before the declaration.
			assert(implies(!declared, uses_before_decl == entry->locations_used_count));
			
			if (uses_before_decl < entry->locations_used_count || !declared) {
				Symbol *found_symbol = NULL;
				Scope  *found_scope  = NULL;
				
				for (Scope *current = scope; current != NULL; current = current->parent) {
					for (Symbol *other_entry = current->first_symbol; other_entry != NULL; other_entry = other_entry->next) {
						if (string_equals(other_entry->ident, entry->ident)) {
							found_symbol = other_entry;
							found_scope  = current;
							goto found;
						}
					}
				}
				found:;
				
				if (found_symbol != NULL) {
					// Split the inner entry between locations_used that are before and after
					// the location_declared.
					Symbol *before_part = entry;
					Symbol *after_part  = symbol_alloc(arena);
					
					after_part->ident = before_part->ident;
					after_part->type  = before_part->type;
					after_part->size  = before_part->size;
					after_part->addr  = before_part->addr;
					
					after_part->location_declared = before_part->location_declared;
					
					i64 uses_after_decl = before_part->locations_used_count - uses_before_decl;
					after_part->locations_used_count  = uses_after_decl;
					before_part->locations_used_count = uses_before_decl;
					
					for (i64 i = 0; i < uses_after_decl; i += 1) {
						after_part->locations_used[i] = before_part->locations_used[uses_before_decl + i];
					}
					
					// Merge the first entry with the outer entry.
					assert(found_symbol->locations_used_count + before_part->locations_used_count <
						   array_count(found_symbol->locations_used));
					
					for (i64 i = 0; i < before_part->locations_used_count; i += 1) {
						found_symbol->locations_used[found_symbol->locations_used_count + i] = before_part->locations_used[i];
					}
					
					found_symbol->locations_used_count += before_part->locations_used_count;
					symbol_free(before_part);
					
					// The uses after the current location_declared are fine as they are and
					// should be left untouched.
					
					allow_break();
				} else {
					// The incriminated identifier was not found in any of the parent scopes,
					// so the only valid one is the one defined in the current scope.
					//
					// Since the identifier is used before its declaration (or it was not declared),
					// react accordingly depending on the type of declaration.
					
					bool is_local_var = ((entry->decl->flags & Ast_Declaration_Flag_CONSTANT) == 0) && (scope->parent != NULL);
					if (is_local_var || !declared) {
						// If it's a local variable (meaning we don't care about out-of-order
						// declarations) or the ident wasn't declared at all: report error.
						
						for (i64 i = 0; i < uses_before_decl; i += 1) {
							Location used = entry->locations_used[uses_before_decl];
							(void) used;
							
							report_error(string_from_lit("Undeclared identifier"));
						}
					} else {
						// All good.
					}
				}
			}
		}
	}
}

internal void
build_scope_for_all_declarations(Arena *arena, Scope *scope, Ast_Declaration *first) {
	
	for (Ast_Declaration *decl = first; decl != NULL && decl != &nil_declaration; decl = decl->next) {
		build_scope_for_declaration(arena, scope, decl);
	}
	
	// TODO: Recursively do this for all scopes
	rearrange_scope(arena, scope);
	
	// TODO: Check for shadowing
	
	// TODO: If there are entries in the table with no locations_used, you can report a warning and
	// remove them
	
	return;
}

////////////////////////////////
//~ Type-checking

internal bool
analyse_expression(Arena *arena, Ast_Expression *expr, Scope *scope, String_List *unresolved) {
	bool progress = false;
	
	if (expr->type.kind != Type_Kind_UNKNOWN) {
		return false; // We already have a type
	}
	
	switch (expr->kind) {
		case Ast_Expression_Kind_LITERAL: {
			expr->type.kind = Type_Kind_INTEGER;
		} break;
		
#if 0
		case Ast_Expression_Kind_IDENT: {
			Decl *found = NULL;
			Scope *where = scope;
			for (Scope *s = scope; s != NULL; s = s->next) {
				for (Decl *decl = scope->resolved_decls->first; decl != NULL; decl = decl->next) {
					if (string_equals(decl->ident, expr->ident)) {
						found = decl;
						where = s;
						goto after_scope_loop;
					}
				}
			}
			
			after_scope_loop:;
			if (!found) {
				string_list_push(arena, unresolved, expr->ident);
			} else {
				result = typed_expression_from_ast_expression(expr);
				result->type_count = 1;
				result->types = push_array(arena, Type, result->type_count);
				result->types[0] = found->type;
				
				if (where == scope) {
					// It's a global: remember the identifier
				} else {
					// It's a local or parameter: remember the stack offset
				}
			}
		} break;
#endif
		
		case Ast_Expression_Kind_UNARY: {
			if (!analyse_expression(arena, expr->subexpr, scope, unresolved)) break;
			
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
						report_error(string_from_lit("Cannot apply + or - to multiple expressions"));
						ok = false;
					}
					
					if (ok && expr->subexpr->type.kind != Type_Kind_INTEGER) {
						report_error(string_from_lit("Cannot apply + or - to this type"));
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
						report_error(string_from_lit("Cannot apply ^ to multiple expressions"));
						ok = false;
					}
					
					if (ok && expr->subexpr->type.kind != Type_Kind_POINTER) {
						report_error(string_from_lit("Cannot apply ^ to this type"));
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
			if (!analyse_expression(arena, expr->left,  scope, unresolved)) break;
			if (!analyse_expression(arena, expr->right, scope, unresolved)) break;
			
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
							report_error(string_from_lit("Too many expressions on the left of this operator"));
						}
						
						if (expr->right->next != &nil_expression) {
							report_error(string_from_lit("Too many expressions on the right of this operator"));
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
						report_error(string_from_lit("Cannot apply operator to this type"));
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
					report_error(string_from_lit("Too many expressions on the left of this operator"));
				}
				
				if (expr->middle->next != &nil_expression) {
					report_error(string_from_lit("Too many expressions in the middle of this operator"));
				}
				
				if (expr->right->next  != &nil_expression) {
					report_error(string_from_lit("Too many expressions on the right of this operator"));
				}
				
				ok = false;
			}
			
			if (ok && (expr->left->type.kind != Type_Kind_BOOLEAN && expr->left->type.kind != Type_Kind_POINTER)) {
				report_error(string_from_lit("Cannot apply operator to this type"));
				ok = false;
			}
			
			if (ok && expr->middle->type.kind != expr->right->type.kind) { // TODO: Exact match, not only the kinds
				report_error(string_from_lit("Types must match"));
				ok = false;
			}
			
			if (ok) {
				expr->type = expr->middle->type;
			}
		} break;
		
		default: break;
	}
	
	progress = expr->type.kind != Type_Kind_UNKNOWN;
	return progress;
}

internal bool
analyse_statement(Arena *arena, Ast_Statement *stat, String_List *unresolved) {
	bool progress = false;
	
	if (!stat->typechecked) {
		switch (stat->kind) {
			case Ast_Statement_Kind_EXPR: {
				progress = analyse_expression(arena, stat->expr, stat->scope, unresolved);
				if (stat->expr->type.kind != Type_Kind_UNKNOWN) {
					stat->typechecked = true;
				}
			} break;
			
			case Ast_Statement_Kind_RETURN: {
				progress = analyse_expression(arena, stat->expr, stat->scope, unresolved);
				if (stat->expr->type.kind != Type_Kind_UNKNOWN) {
					stat->typechecked = true;
				}
			} break;
			
			case Ast_Statement_Kind_BLOCK: {
				for (Ast_Statement *curr = stat->block; curr != NULL && curr != &nil_statement; curr = curr->next) {
					progress = progress || analyse_statement(arena, curr, unresolved);
					stat->typechecked = stat->typechecked && curr->typechecked;
				}
			} break;
			
			case Ast_Statement_Kind_NULL: {
				stat->typechecked = true;
			} break;
			
			default: break;
		}
	}
	
	return progress;
}

#if 0
internal void
analyse_declarations(Ast_Declaration *first) {
	Scratch scratch = scratch_begin(0, 0);
	
	Typed_Declaration *first = NULL;
	
	Scope scope = {0};
	String_List unresolved = {0};
	
	for (Ast_Declaration *decl; decl != NULL && decl != &nil_declaration; decl = decl->next) {
		String_List dependencies = {0};
		
		if (decl->kind == Ast_Declaration_Kind_PROCEDURE) {
			Typed_Declaration *typed_decl = analyse_declaration(scratch.arena, decl, &scope, &dependencies);
			// push to the Typed_Declaration list
		}
	}
	
	for (;unresolved->first != NULL;) {
		bool made_progress = false;
		
		for (Typed_Declaration *decl = first; decl != NULL; decl = decl->next) {
			
			bool resolved = false;
			for (String_Node *dep = dependencies->first; dep != NULL; dep = dep->next) {
				
				
				if (!found) {
					string_list_push(arena, &unresolved_symbols, dep->s);
				}
			}
			
			if (resolved) {
				decl_list_push(&scope->decls, decl);
				
				made_progress = true;
			} else {
				decl_list_push(&unresolved, decl);
			}
		}
		
		if (!made_progress) {
			fprintf(stderr, "Error!\n");
			break;
		}
	}
	
	scratch_end(scratch);
}
#endif

////////////////////////////////
//~ Checking

internal void
do_all_checks(Ast_Declaration *prog) {
	if (!arena_initted(scope_arena)) {
		arena_init(&scope_arena);
	}
	
	build_scope_for_declaration(&scope_arena, &scope_global, prog);
	
	return;
}

////////////////////////////////
//~ Utils

#if 0
internal int
count_expressions_in_list(Ast_Expression *expr, int acc) {
	if (expr->kind == Ast_Expression_Kind_COMMA) {
		acc += count_expressions_in_list(expr->left);
		acc += count_expressions_in_list(expr->right);
	} else {
		acc += 1;
	}
	return acc;
}
#endif

#endif
