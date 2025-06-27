#include "el_ctx_crack.h"
#include "el_base.h"

#include "el_base.c"

#include "el_ast.h"

#include "el_ast_parse.c"
#include "el_ast_print.c"

#include "el_x64.c"

////////////////////////////////
//~ Errors

internal void
report_error(String message) {
	fprintf(stderr, "Error: %.*s\n", string_expand(message));
}

////////////////////////////////
//~ Scope and Symbols

typedef struct Symbol Symbol;
struct Symbol {
	Symbol *next;
	Symbol *prev;
	Symbol *next_free;
	
	String ident;
	Ast_Declaration_Entity decl_kind;
	Ast_Declaration_Flags decl_flags;
	Location location_declared;
	Location locations_used[32]; // Arbitrary number for now
	i64 locations_used_count;
	Type type;
	i64  size;
	i64  addr;
	// Calling_Convention conv;
	// Storage_Class storage_class;
};

global Symbol *first_free_symbol;

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


typedef struct Scope Scope;
struct Scope {
	Scope *parent;
	Scope *next_sibling;
	Scope *prev_sibling;
	Scope *first_child;
	Scope *last_child;
	
	Symbol *first_symbol;
	Symbol *last_symbol;
};

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
	
	switch (decl->entity) {
		case Ast_Declaration_Entity_PROCEDURE: {
			Symbol *symbol = scope_lookup_inner(scope, decl->ident);
			if (symbol == NULL) {
				symbol = scope_insert_ident(arena, scope, decl->ident);
				symbol->location_declared = decl->location;
			} else {
				report_error(string_from_lit("Redefinition of Disco"));
			}
			
			scope = scope_enter(arena, scope);
			
			// for each decl in the procedure's formal parameter list:
			//   build_scope_for_declaration()
			
			build_scope_for_statement(arena, scope, decl->body);
			
			scope = scope_leave(scope);
		} break;
		
		default: break;
	}
	
	return;
}

global Scope scope_global;
global Arena scope_arena;

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
					
					bool is_local_var = ((entry->decl_flags & Ast_Declaration_Flag_CONSTANT) == 0) && (scope->parent != NULL);
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
build_scope(Ast_Declaration *root) {
	arena_init(&scope_arena);
	
	for (Ast_Declaration *decl = root; decl != NULL && decl != &nil_declaration; decl = decl->next) {
		build_scope_for_declaration(&scope_arena, &scope_global, decl);
	}
	
	// TODO: Recursively do this for all scopes
	rearrange_scope(&scope_arena, &scope_global);
	
	// TODO: Check for shadowing
	
	// TODO: If there are entries in the table with no locations_used, you can report a warning and
	// remove them
	
	return;
}

////////////////////////////////
//~ Type-checking

#if 0
internal Typed_Expression *
analyse_expression(Arena *arena, Ast_Expression *expr, Scope *scope, String_List *unresolved) {
	Typed_Expression *result = &nil_typed_expression;
	
	switch (expr->kind) {
		case Ast_Expression_Kind_LITERAL: {
			result = typed_expression_from_ast_expression(expr);
			result->value = expr->value;
			result->types = push_array(arena, Type, 1);
			result->type_count = 1;
			result->types[0].kind = Type_Kind_INTEGER;
		} break;
		
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
		
		case Ast_Expression_Kind_UNARY: {
			Typed_Expression *subresult = analyse_expression(arena, expr->subexpr, scope, unresolved);
			
			switch (expr->unary) {
				case Unary_Operator_PLUS:
				case Unary_Operator_MINUS: {
					if (subresult->type_count != 1) {
						report_error(string_from_lit("Cannot apply + or - to multiple types"));
					} else if (subresult->types[0] != Type_Kind_INTEGER) {
						report_error(string_from_lit("Cannot apply + or - to this type"));
					} else {
						result = typed_expression_from_ast_expression(expr);
						result->type_count = 1;
						result->types = push_array(arena, Type, result->type_count);
						result->types[0] = subresult->types[0];
					}
				} break;
				
				case Unary_Operator_DEREFERENCE: {
					if (subresult->type_count != 1) {
						report_error(string_from_lit("Cannot apply ^ to multiple types"));
					} else if (subresult->types[0] != Type_Kind_INTEGER) {
						report_error(string_from_lit("Cannot apply ^ to this type"));
					} else {
						result = typed_expression_from_ast_expression(expr);
						result->type_count = 1;
						result->types = push_array(arena, Type, result->type_count);
						result->types[0] = subresult->types[0];
					}
				} break;
				
				default: break;
			}
			
		} break;
		
		case Ast_Expression_Kind_BINARY: {
			Typed_Expression *typed_left  = analyse_expression(arena, expr->left,    scope, unresolved);
			Typed_Expression *typed_right = analyse_expression(arena, expr->right,   scope, unresolved);
			
			switch (expr->binary) {
				case Binary_Operator_PLUS:
				case Binary_Operator_MINUS:
				case Binary_Operator_TIMES:
				case Binary_Operator_DIVIDE:
				case Binary_Operator_MODULUS: {
					if (typed_left->type_count == 1 && typed_right->type_count == 1) {
						report_error(string_from_lit("Too many expressions on left or right of this operator"));
					} else if (typed_left->types[0] != Type_Kind_INTEGER && typed_right->types[0] != Type_Kind_INTEGER) {
						report_error(string_from_lit("Cannot apply operator to this type"));
					} else {
						result = typed_expression_from_ast_expression(expr);
						result->type_count = 1;
						result->types = push_array(arena, Type, result->type_count);
						result->types[0] = typed_left->types[0];
					}
				} break;
				
				case Binary_Operator_COMMA: {
					result = typed_expression_from_ast_expression(expr);
					result->type_count = typed_left->type_count + typed_right->type_count;
					result->types = push_array(arena, Type, result->type_count);
					for (i64 i = 0; i < typed_left->type_count; i += 1) {
						result->types[i] = typed_left->types[i];
					}
					for (i64 i = typed_left->type_count; i < typed_right->type_count; i += 1) {
						result->types[i] = typed_right->types[i];
					}
				} break;
				
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
				
				default: break;
			}
			
		} break;
		
#if 0
		case Ast_Expression_Kind_TERNARY: {
			analyse_expression(arena, expr->left,    scope, unresolved);
			analyse_expression(arena, expr->middle,  scope, unresolved);
			analyse_expression(arena, expr->right,   scope, unresolved);
		} break;
#endif
		
		default: break;
	}
	
	return result;
}

typedef struct Typed_Statement Typed_Statement;
struct Typed_Statement {
	Ast_Statement_Kind kind;
	Typed_Statement   *next;
	Typed_Statement   *block;
	
	Typed_Expression *expr;
};

internal Typed_Statement *
analyse_statement(Arena *arena, Ast_Statement *stat, Scope *scope, String_List *unresolved) {
	Typed_Statement *result = &nil_typed_statement;
	
	switch (stat->kind) {
		case Ast_Statement_Kind_EXPR: {
			(void) analyse_expression(arena, stat->expr, scope, unresolved);
		} break;
		
		case Ast_Statement_Kind_RETURN: {
			(void) analyse_expression(arena, stat->expr, scope, unresolved);
		} break;
		
		case Ast_Statement_Kind_BLOCK: {
			scope = push_scope(scope);
			
			// TODO: First do all DECL statements so that local functions and types are added to the
			// scope;
			// then do all other statements.
			
			for (Ast_Statement *curr = stat->block; curr != NULL && curr != &nil_statement; curr = curr->next) {
				(void) analyse_statement(arena, curr, scope, unresolved);
			}
			
			scope = pop_scope(scope);
		} break;
	}
	
	return result;
}

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
//~ Tree

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

////////////////////////////////
//~ Bytecode

typedef enum Instr_Operation {
	Instr_Operation_NULL,
	
	Instr_Operation_NOP,
	Instr_Operation_SET,
	Instr_Operation_NEG,
	Instr_Operation_ADD,
	Instr_Operation_SUB,
	Instr_Operation_MUL,
	Instr_Operation_DIV,
	
	Instr_Operation_CALL,
	Instr_Operation_RETURN,
	
	Instr_Operation_SWAP,
	
	Instr_Operation_COUNT,
} Instr_Operation;

typedef enum Addressing_Mode {
	Addressing_Mode_CONSTANT,
	Addressing_Mode_REGISTER,
	Addressing_Mode_COUNT,
} Addressing_Mode;

#define BYTECODE_RETURN_REGISTER_0 0

typedef enum Label_Kind {
	Label_Kind_NULL = 0,
	Label_Kind_PROCEDURE,
	Label_Kind_INTERNAL,
	Label_Kind_COUNT,
} Label_Kind;

// NOTE: jump_dest_label is only useful when translating bytecode to actual assembly, not when
// running the bytecode directly (if we ever do that). Before running the bytecode, do an additional pass
// over the generated instructions where each jump target name is changed to its actual address
// (or index of the instruction in the array). There was a paper somewhere on the internet about building
// an assembler which explained the algorithm to do just that.

typedef struct Reg_Group Reg_Group; // Temporary
struct Reg_Group {
	int regs[8];
	int reg_count;
};

typedef struct Instr Instr;
struct Instr {
	String label;
	Label_Kind label_kind;
	String jump_dest_label; // For jumps and procedure calls.
	Instr_Operation operation;
	Addressing_Mode mode;
	int source; // Register
	int dest;   // Register
	
	// int ret_regs[8]; // Arbitrary number for now
	int ret_reg_count;
	
	int arg_regs[8]; // Arbitrary number for now
	int arg_reg_count;
};

global Instr instructions[256];
global int instruction_count;

global int registers_used;

internal Instr_Operation
instr_operation_from_expr_unary(Unary_Operator expr_op) {
	Instr_Operation instr_op = 0;
	
	switch (expr_op) {
		case Unary_Operator_PLUS: { instr_op = Instr_Operation_NOP; } break;
		case Unary_Operator_MINUS: { instr_op = Instr_Operation_NEG; } break;
		// case Unary_Operator_DEREFERENCE: { instr_op = Instr_Operation_DEREFERENCE; } break;
		default: break;
	}
	
	return instr_op;
}

internal Instr_Operation
instr_operation_from_expr_binary(Binary_Operator expr_op) {
	Instr_Operation instr_op = 0;
	
	switch (expr_op) {
		case Binary_Operator_PLUS: { instr_op = Instr_Operation_ADD; } break;
		case Binary_Operator_MINUS: { instr_op = Instr_Operation_SUB; } break;
		case Binary_Operator_TIMES: { instr_op = Instr_Operation_MUL; } break;
		case Binary_Operator_DIVIDE: { instr_op = Instr_Operation_DIV; } break;
		// case Binary_Operator_MEMBER: { instr_op = Instr_Operation_COMMA; } break;
		case Binary_Operator_CALL: { instr_op = Instr_Operation_CALL; } break;
		// case Binary_Operator_ARRAY_ACCESS: { instr_op = Instr_Operation_ARRAY_ACCESS; } break;
		default: break;
	}
	
	return instr_op;
}

internal Reg_Group
generate_bytecode_for_expression(Ast_Expression *expr) {
	Instr instr = {0};
	Reg_Group dests = {0};
	
	switch (expr->kind) {
		case Ast_Expression_Kind_LITERAL: {
			instr.operation = Instr_Operation_SET;
			instr.mode      = Addressing_Mode_CONSTANT;
			instr.source    = expr->value;
			instr.dest      = registers_used;
			registers_used += 1;
			
			instructions[instruction_count] = instr;
			instruction_count += 1;
			
			dests.regs[0] = instr.dest;
			dests.reg_count = 1;
		} break;
		
		// -x        => imul x, -1
		// -(x, y)   => 
		// foo(x)    => mov rdi, x, call foo
		// foo(x, y) => mov rdi, x; mov rsi, y; call foo
		
		case Ast_Expression_Kind_UNARY: {
			Reg_Group sub_dests = generate_bytecode_for_expression(expr->left);
			assert(sub_dests.reg_count >= 1); // Should be ==, but first decide how to treat comma expressions
			
			instr.operation = instr_operation_from_expr_unary(expr->unary);
			instr.mode      = Addressing_Mode_REGISTER;
			instr.source    = sub_dests.regs[0];
			instr.dest      = sub_dests.regs[0];
			
			instructions[instruction_count] = instr;
			instruction_count += 1;
			
			dests.regs[0] = instr.dest;
			dests.reg_count = 1;
		} break;
		
		case Ast_Expression_Kind_BINARY: {
			Binary_Operator binary = expr->binary;
			
			if (binary == Binary_Operator_CALL) {
				// For now only identifiers can be lhs of calls
				assert(expr->left->kind == Ast_Expression_Kind_IDENT);
				instr.jump_dest_label = expr->left->ident;
				
				Reg_Group right_dests = generate_bytecode_for_expression(expr->right);
				// assert(right_dests.reg_count >= 1); // Could have no arguments
				
				instr.operation = instr_operation_from_expr_binary(expr->binary);
				instr.mode      = Addressing_Mode_REGISTER;
				
				// TODO: For now do nothing: when executing the next instruction you will be
				// inside the function call and are going to be using the saved registers.
				// When a more robust approach to calling conventions is defined (registers vs stack),
				// maybe some registers could be freed here.
				// registers_used -= 1;
				
				// Remember which registers are used to store the evaluated arguments
				for (int si = 0; si < right_dests.reg_count; si += 1) {
					instr.arg_regs[si] = right_dests.regs[si];
					instr.arg_reg_count += 1;
				}
				
				// Ast_Declaration *callee = decl_list_find_ident(&proc_list, instr.jump_dest_label);
				// assert(callee != NULL); // Typechecking should've failed
				
				instructions[instruction_count] = instr;
				instruction_count += 1;
				
				// Pretend that every function has 1 return value and that it is put in register 0.
				// TODO: This is very bad.
				// This is why we should use the stack:
				// before inserting the call instruction, push all live registers onto the stack
				// after the call instruction, pop all registers that were pushed before
				dests.regs[0] = 0;
				dests.reg_count = 1;
			} else if (0 /*&& binary == Binary_Operator_COMMA*/) {
				Reg_Group left_dests  = generate_bytecode_for_expression(expr->left);
				Reg_Group right_dests = generate_bytecode_for_expression(expr->right);
				assert(left_dests.reg_count >= 1); // Same as above
				assert(right_dests.reg_count >= 1);
				
				// No new instructions generated; a COMMA operator simply evaluates both lhs and rhs.
				
				assert(left_dests.reg_count + right_dests.reg_count <= array_count(dests.regs));
				for (int si = 0; si < left_dests.reg_count; si += 1) {
					dests.regs[dests.reg_count] = left_dests.regs[si];
					dests.reg_count += 1;
				}
				for (int si = 0; si < right_dests.reg_count; si += 1) {
					dests.regs[dests.reg_count] = right_dests.regs[si];
					dests.reg_count += 1;
				}
			} else {
				Reg_Group left_dests  = generate_bytecode_for_expression(expr->left);
				Reg_Group right_dests = generate_bytecode_for_expression(expr->right);
				assert(left_dests.reg_count >= 1); // Same as above
				assert(right_dests.reg_count >= 1);
				
				instr.operation = instr_operation_from_expr_binary(expr->binary);
				instr.mode      = Addressing_Mode_REGISTER;
				instr.source    = right_dests.regs[0];
				instr.dest      = left_dests.regs[0];
				registers_used -= 1;         // This instruction puts the result in left.dest;  right.dest can be used by the next instruction
				
				instructions[instruction_count] = instr;
				instruction_count += 1;
				
				dests.regs[0] = instr.dest;
				dests.reg_count = 1;
			}
		} break;
		
		default: break;
	}
	
	return dests;
}

internal void
generate_bytecode_for_statement(Ast_Statement *statement) {
	switch (statement->kind) {
		case Ast_Statement_Kind_EXPR: {
			generate_bytecode_for_expression(statement->expr);
		} break;
		
		case Ast_Statement_Kind_BLOCK: {
			for (Ast_Statement *s = statement->block; s != NULL && s != &nil_statement; s = s->next) {
				generate_bytecode_for_statement(s);
			}
		} break;
		
		case Ast_Statement_Kind_RETURN: {
			
			Instr ret = {0};
			
#if 0
			{
				// Remember the destination register of each of the returned expressions,
				// as well as how many there are.
				// int i = 0;
				for (Ast_Expression *expr = statement->expr; expr != NULL; expr = expr->next) {
					Reg_Group dests = generate_bytecode_for_expression(expr);
					
					for (int di = 0; di < dests.reg_count; di += 1) {
						assert(ret.ret_reg_count < array_count(ret.ret_regs));
						ret.ret_regs[ret.ret_reg_count] = dests.regs[di];
						ret.ret_reg_count += 1;
					}
				}
			}
#else
			
			int retval_count = 0;
			
			{
				// Walk the expression list and map each current destination register to
				// the correct return register for the calling convention
				
				int unmapped_ret_regs[8]; // 8 arbitrary size for now, TODO
				int unmapped_ret_reg_count = 0;
				
#if 0
				typedef struct Expr_Node Expr_Node;
				struct Expr_Node { Ast_Expression *expr; Expr_Node *next; };
				
				Expr_Node *expr = statement->expr;
				
				for (;expr != NULL;) {
					if (expr->kind == Ast_Expression_Kind_COMMA) {
						stack_push(expr->right);
						stack_push(expr->left);
					} else {
						Reg_Group dests = generate_bytecode_for_expression(expr);
						stack_pop(expr);
						
						for (int di = 0; di < dests.reg_count; di += 1) {
							unmapped_ret_regs[unmapped_ret_reg_count] = dests.regs[di];
							unmapped_ret_reg_count += 1;
						}
					}
				}
#else
				Reg_Group dests = generate_bytecode_for_expression(statement->expr);
				for (int di = 0; di < dests.reg_count; di += 1) {
					unmapped_ret_regs[unmapped_ret_reg_count] = dests.regs[di];
					unmapped_ret_reg_count += 1;
				}
#endif
				
				for (int i = 0; i < unmapped_ret_reg_count; i += 1) {
					if (unmapped_ret_regs[i] != i) {
						Instr swap = {0};
						
						swap.operation = Instr_Operation_SWAP;
						swap.dest      = unmapped_ret_regs[i];
						swap.source    = i;
						swap.mode      = Addressing_Mode_REGISTER;
						
						instructions[instruction_count] = swap;
						instruction_count += 1;
					}
				}
				
				retval_count = unmapped_ret_reg_count;
			}
#endif
			
			ret.operation = Instr_Operation_RETURN;
			ret.ret_reg_count = retval_count; // TODO: Remove; it should be stored in the procedure defition, not here
			
			instructions[instruction_count] = ret;
			instruction_count += 1;
		} break;
		
		default: break;
	}
}

internal void
generate_bytecode_for_declaration(Ast_Declaration *declaration) {
	switch (declaration->entity) {
		case Ast_Declaration_Entity_PROCEDURE: {
			Instr instr = {0};
			
			instr.label      = declaration->ident;
			instr.label_kind = Label_Kind_PROCEDURE;
			instr.operation  = Instr_Operation_NULL;
			
			instructions[instruction_count] = instr;
			instruction_count += 1;
			
			generate_bytecode_for_statement(declaration->body);
			
			if (instructions[instruction_count-1].operation != Instr_Operation_RETURN) {
				fprintf(stderr, "Warning: Unreachable code after return statement.\n");
			}
		} break;
		
		default: break;
	}
}

////////////////////////////////
//~ MASM Source

typedef struct MASM_Context MASM_Context;
struct MASM_Context {
	Arena  arena;
	String current_label;
	
	String_List lines;
	int    indent_level;
	String indent_string;
};

global MASM_Context masm_context = {
	.indent_string = string_from_lit_const("\t"),
};

internal void
masm_append_line(String line) {
	Scratch scratch = scratch_begin(0, 0);
	
	int indent_cap = masm_context.indent_level * masm_context.indent_string.len;
	u8 *indent_buf = push_array(scratch.arena, u8, indent_cap);
	int indent_len = 0;
	
	if (indent_buf != NULL) {
		for (int i = 0; i < masm_context.indent_level; i += 1) {
			memcpy(indent_buf + indent_len, masm_context.indent_string.data, masm_context.indent_string.len);
			indent_len += masm_context.indent_string.len;
		}
		
		assert(indent_len == indent_cap);
		
		String temp[] = {string(indent_buf, indent_len), line};
		string_list_push(&masm_context.arena, &masm_context.lines, strings_concat(&masm_context.arena, temp, array_count(temp)));
	} else {
		string_list_push(&masm_context.arena, &masm_context.lines, string_clone(&masm_context.arena, line));
	}
	
	scratch_end(scratch);
}

internal String
masm_register_from_bytecode_register(int bytecode_reg) {
	String register_names[] = {
		string_from_lit_const("rax"),
		string_from_lit_const("rbx"),
		string_from_lit_const("rcx"),
		string_from_lit_const("rdx"),
		string_from_lit_const("rdi"),
		string_from_lit_const("rsi"),
		string_from_lit_const("rbp"),
		string_from_lit_const("rsp"),
		string_from_lit_const("r8"),
		string_from_lit_const("r9"),
		string_from_lit_const("r10"),
		string_from_lit_const("r11"),
		string_from_lit_const("r12"),
		string_from_lit_const("r13"),
		string_from_lit_const("r14"),
		string_from_lit_const("r15"),
	};
	assert(array_count(register_names) == 16);
	
	// Temporarily shift all registers from rsp to r15 up by 1, so rsp isn't used in
	// normal calculations. This doesn't seem like a good way to do it but it's fine for now.
	if (bytecode_reg >= 7) bytecode_reg += 1;
	assert(bytecode_reg < array_count(register_names));
	
	return register_names[bytecode_reg];
}

internal String
masm_generate_source(void) {
	
	masm_append_line(string_from_lit("; Generated"));
	masm_append_line(string_from_lit("includelib msvcrt.lib"));
	masm_append_line(string_from_lit(".data"));
	masm_append_line(string_from_lit(".code\n")); // Not .text, aparently
	
	Scratch scratch = scratch_begin(0, 0);
	
	for (int i = 0; i < instruction_count; i += 1) {
		arena_reset(scratch.arena);
		
		Instr *instr = &instructions[i];
		
		switch (instr->label_kind) {
			case Label_Kind_PROCEDURE: {
				String line = push_stringf(scratch.arena, "%.*s proc", string_expand(instr->label));
				masm_append_line(line);
				
				// Remember the current label so we can write the 'endp' line
				masm_context.current_label = instr->label;
				
				masm_context.indent_level += 1;
				
				{
					// Push callee-saved registers
					// TODO: Only do this if necessary
					// TODO: Figure out why the call stack disappears from the debugger when rbx and rbp are
					// pushed, and why it reappears when they are popped.
					masm_append_line(string_from_lit("; Procedure prologue"));
					masm_append_line(string_from_lit("push rbx"));
					masm_append_line(string_from_lit("push rbp"));
					masm_append_line(string_from_lit("push r12"));
					masm_append_line(string_from_lit("push r13"));
					masm_append_line(string_from_lit("push r14"));
					masm_append_line(string_from_lit("push r15"));
					masm_append_line(string_from_lit("; Procedure body"));
				}
				
			} break;
			
			default: break;
		}
		
		switch (instr->operation) {
			case Instr_Operation_NULL: {
				String line = push_stringf(scratch.arena, "; Null instruction", instr->operation);
				masm_append_line(line);
			} break;
			
			case Instr_Operation_SET:
			case Instr_Operation_ADD:
			case Instr_Operation_SUB:
			case Instr_Operation_MUL: {
				
				String source = {0};
				String dest   = {0};
				
				switch (instr->mode) {
					case Addressing_Mode_CONSTANT: {
						source = push_stringf(scratch.arena, "%d", instr->source);
						dest   = masm_register_from_bytecode_register(instr->dest);
					} break;
					
					case Addressing_Mode_REGISTER: {
						source = masm_register_from_bytecode_register(instr->source);
						dest   = masm_register_from_bytecode_register(instr->dest);
					} break;
					
					default: break;
				}
				
				String mnemonic = {0};
				switch (instr->operation) {
					case Instr_Operation_SET: { mnemonic = string_from_lit("mov"); } break;
					case Instr_Operation_ADD: { mnemonic = string_from_lit("add"); } break;
					case Instr_Operation_SUB: { mnemonic = string_from_lit("sub"); } break;
					case Instr_Operation_MUL: { mnemonic = string_from_lit("imul"); } break;
					default: break;
				}
				
				String line = push_stringf(scratch.arena, "%.*s %.*s, %.*s", string_expand(mnemonic),
										   string_expand(dest), string_expand(source));
				masm_append_line(line);
			} break;
			
			case Instr_Operation_CALL: {
				
				{
					// Push caller-saved registers
					// TODO: Only do this if necessary
					masm_append_line(string_from_lit("; Call prologue"));
					masm_append_line(string_from_lit("push rax"));
					masm_append_line(string_from_lit("push rcx"));
					masm_append_line(string_from_lit("push rdx"));
					masm_append_line(string_from_lit("push rdi"));
					masm_append_line(string_from_lit("push rsi"));
					masm_append_line(string_from_lit("push rsp"));
					masm_append_line(string_from_lit("push r8"));
					masm_append_line(string_from_lit("push r9"));
					masm_append_line(string_from_lit("push r10"));
					masm_append_line(string_from_lit("push r11"));
				}
				
				{
					// Put arguments in the correct place
					
					for (int i = 0; i < instr->arg_reg_count; i += 1) {
						String source = masm_register_from_bytecode_register(instr->arg_regs[instr->arg_reg_count]);
						
						String line = push_stringf(scratch.arena, "mov rdi, %.*s", string_expand(source));
						masm_append_line(line);
					}
				}
				
				String line = push_stringf(scratch.arena, "call %.*s", string_expand(instr->jump_dest_label));
				masm_append_line(line);
				
				{
					// Pop caller-saved registers
					// NOTE: Remember that the stack is FILO! Do this in reverse push order.
					// TODO: Only do this if necessary
					masm_append_line(string_from_lit("; Call epilogue"));
					masm_append_line(string_from_lit("pop r11"));
					masm_append_line(string_from_lit("pop r10"));
					masm_append_line(string_from_lit("pop r9"));
					masm_append_line(string_from_lit("pop r8"));
					masm_append_line(string_from_lit("pop rsp"));
					masm_append_line(string_from_lit("pop rsi"));
					masm_append_line(string_from_lit("pop rdi"));
					masm_append_line(string_from_lit("pop rdx"));
					masm_append_line(string_from_lit("pop rcx"));
					masm_append_line(string_from_lit("pop rax"));
				}
				
			} break;
			
			case Instr_Operation_SWAP: {
				assert(registers_used < 14); // For now. TODO: Use memory if no more registers
				
				String source = masm_register_from_bytecode_register(instr->source);
				String dest   = masm_register_from_bytecode_register(instr->dest);
				String temp   = masm_register_from_bytecode_register(registers_used);
				
				masm_append_line(push_stringf(&masm_context.arena, "mov %.*s, %.*s", string_expand(temp),
											  string_expand(source)));
				masm_append_line(push_stringf(&masm_context.arena, "mov %.*s, %.*s", string_expand(source),
											  string_expand(dest)));
				masm_append_line(push_stringf(&masm_context.arena, "mov %.*s, %.*s", string_expand(dest),
											  string_expand(temp)));
			} break;
			
			case Instr_Operation_RETURN: {
				
				{
					// Convert the bytecode calling convention to the platform calling convention:
					// For now, just map BYTECODE_RETURN_REGISTER_0 to rax
					
					if (instr->ret_reg_count == 1) {
						String source = masm_register_from_bytecode_register(BYTECODE_RETURN_REGISTER_0);
						String dest   = string_from_lit("rax");
						
						String line = push_stringf(scratch.arena, "mov %.*s, %.*s", string_expand(dest),
												   string_expand(source));
						masm_append_line(line);
					} else if (instr->ret_reg_count > 1) {
						String line = push_stringf(scratch.arena, "; Unimplemented returning of multiple values");
						masm_append_line(line);
					}
				}
				
				{
					// Pop callee-saved registers
					// NOTE: Remember that the stack is FILO! Do this in reverse push order.
					// TODO: Only do this if necessary
					masm_append_line(string_from_lit("; Procedure epilogue"));
					masm_append_line(string_from_lit("pop r15"));
					masm_append_line(string_from_lit("pop r14"));
					masm_append_line(string_from_lit("pop r13"));
					masm_append_line(string_from_lit("pop r12"));
					masm_append_line(string_from_lit("pop rbp"));
					masm_append_line(string_from_lit("pop rbx"));
				}
				
				masm_append_line(string_from_lit("ret"));
				masm_context.indent_level -= 1;
				
				String line = push_stringf(&masm_context.arena, "%.*s endp\n",
										   string_expand(masm_context.current_label));
				masm_append_line(line);
			} break;
			
			default: {
				String line = push_stringf(scratch.arena, "; Unimplemented instruction '%d'", instr->operation);
				masm_append_line(line);
			} break;
		}
	}
	
	masm_append_line(string_from_lit("end"));
	
	scratch_end(scratch);
	
	return string_from_list(&masm_context.arena, masm_context.lines,
							.sep = string_from_lit("\n"),
							.suf = string_from_lit("\n"));
}

int main(void) {
	test_expression_parser();
	test_statement_parser();
	test_declaration_parser();
	
	x64_test();
	
	printf("\n\n### Main program output ###\n\n");
	
	String source = string_from_lit("main :: () { return other(); }"
									"other :: () { return 2*3 + 10/(4+1); 7-0; }");
	
	Arena tree_arena = {0};
	arena_init(&tree_arena);
	Ast_Declaration *program = &nil_declaration;
	// program = parse_program_string(&tree_arena, source);
	
	build_scope(program);
	
	// TODO: Type-checking here
	
	for (Ast_Declaration *decl = program; decl != NULL; decl = decl->next) {
	for (Ast_Declaration *decl = program; decl != NULL && decl != &nil_declaration; decl = decl->next) {
		generate_bytecode_for_declaration(decl);
	}
	
	arena_init(&masm_context.arena);
	String masm_source = masm_generate_source();
	
	FILE *sf = fopen("generated/generated.asm", "wb+");
	FILE *bs = fopen("build_generated.bat", "wb+");
	if (sf && bs) {
		fwrite(masm_source.data, sizeof(char), masm_source.len, sf);
		fclose(sf);
		
		char buf[1024] = {0};
		int buf_len = snprintf(buf, array_count(buf), "@echo off\n"
							   "del *.pdb > NUL 2> NUL\n"
							   "del *.rdi > NUL 2> NUL\n"
							   "ml64 generated/generated.asm /nologo /Fegenerated/generated.exe /W4 /WX /Zi /link /incremental:no /opt:ref\n"
							   "del *.obj > NUL 2> NUL\n"
							   "del *.ilk > NUL 2> NUL\n"
							   "del mllink$* > NUL 2> NUL\n");
		
		fwrite(buf, sizeof(char), buf_len, bs);
		fclose(bs);
		system("build_generated.bat");
	}
	
	return 0;
}
