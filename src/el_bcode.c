#ifndef EL_BCODE_C
#define EL_BCODE_C

////////////////////////////////
//~ Printing

internal String bcode_operation_name(Bcode_Operation op) {
	if (op < BCODE_COUNT) return bcode_operation_names[op];
	
	panic("Bcode operation out of range");
	return string_from_lit("UNKNOWN");
}

internal void print_bcode_instr(Bcode_Instr instr) {
	printf("[%.*s", string_expand(bcode_operation_name(instr.operation)));
	if (instr.operation != BCODE_NULL && instr.operation != BCODE_NOP)
		printf(", ");
	
	switch (instr.operation) {
		case BCODE_ALLOCA: {
			printf("r%d, size %d", instr.dest_register, instr.alloca_size);
		} break;
		
		case BCODE_STORE: {
			if (instr.mode == Addressing_Mode_CONSTANT) {
#if COMPILER_MSVC
				printf("[r%d] = %lld", instr.dest_register, instr.source_imm);
#else
				printf("[r%d] = %ld", instr.dest_register, instr.source_imm);
#endif
			} else {
				printf("[r%d] = r%d", instr.dest_register, instr.source_register);
			}
		} break;
		
		case BCODE_LOAD: {
			printf("r%d = [r%d]", instr.dest_register, instr.source_register);
		} break;
		
		case BCODE_SET: {
			if (instr.mode == Addressing_Mode_CONSTANT) {
#if COMPILER_MSVC
				printf("r%d = %lld", instr.dest_register, instr.source_imm);
#else
				printf("r%d = %ld", instr.dest_register, instr.source_imm);
#endif
			} else {
				printf("r%d = r%d", instr.dest_register, instr.source_register);
			}
		} break;
		
		case BCODE_NEG: {
			printf("r%d = -r%d", instr.dest_register, instr.source_register);
		} break;
		
		case BCODE_ADD:
		case BCODE_SUB:
		case BCODE_MUL:
		case BCODE_DIV: {
			char c = '?';
			switch (instr.operation) {
				case BCODE_ADD: c = '+';
				case BCODE_SUB: c = '-';
				case BCODE_MUL: c = '*';
				case BCODE_DIV: c = '/';
			}
			
			printf("r%d %c= r%d", instr.dest_register, c, instr.source_register);
		} break;
		
		case BCODE_SWAP: {
			printf("r%d <-> r%d", instr.dest_register, instr.source_register);
		} break;
		
		case BCODE_RETURN: {
			printf("r%d", instr.source_register);
		} break;
		
		case BCODE_CALL: {
			printf("?");
		} break;
		
		default: break;
	}
	printf("]");
}

////////////////////////////////
//~ Instruction constructors

// Assigns an immediate (constant) to a register
internal Bcode_Instr make_bcode_mov_imm2reg(int dest_register, i64 imm) {
	Bcode_Instr result = {0};
	
	result.operation     = BCODE_SET;
	result.mode          = Addressing_Mode_CONSTANT;
	result.dest_register = dest_register;
	result.source_imm    = imm;
	
	return result;
}

// Loads a local variable into a register.
// 'source_register' is the register that stores the *pointer* to the variable's address/offset
internal Bcode_Instr make_bcode_load_local(int dest_register, int source_register) {
	Bcode_Instr result = {0};
	
	result.operation       = BCODE_LOAD;
	// result.mode            = Addressing_Mode_;
	result.dest_register   = dest_register;
	result.source_register = source_register;
	
	return result;
}

// Moves the stack pointer by 'size' bytes and stores a pointer to the original address into 'dest_register'.
internal Bcode_Instr make_bcode_alloca(int dest_register, int size) {
	Bcode_Instr result = {0};
	
	result.operation     = BCODE_ALLOCA;
	result.dest_register = dest_register;
	result.alloca_size   = size;
	
	return result;
}

////////////////////////////////
//~ Builder helpers

internal Bcode_Block *push_bcode_block(Bcode_Builder *builder) {
	Bcode_Proc *last_proc = &builder->procs[builder->proc_count - 1];
	
	Bcode_Block *new_block = push_type(builder->arena, Bcode_Block);
	queue_push(last_proc->first_block, last_proc->last_block, new_block);
	
	new_block->instruction_capacity = 256;
	new_block->instructions = push_array(builder->arena, Instr, new_block->instruction_capacity);
	
	return new_block;
}

internal void append_bcode_instr(Bcode_Builder *builder, Instr instr) {
	Bcode_Proc *last_proc = &builder->procs[builder->proc_count - 1];
	
	Bcode_Block *block = last_proc->last_block;
	assert(block != NULL, "Bytecode builder not initialized");
	
	if (block->instruction_count >= block->instruction_capacity) {
		block = push_bcode_block(builder);
	}
	
	block->instructions[block->instruction_count] = instr;
	block->instruction_count += 1;
}

internal void bcode_builder_init(Bcode_Builder *builder, Arena *arena, Symbol_Table *table) {
	memset(builder, 0, sizeof(*builder));
	
	builder->arena = arena;
	builder->table = table;
	
	builder->global_var_count    = 1; // Null variable, to reduce codepaths later
	builder->global_var_capacity = builder->table->global_var_count + 1;
	builder->global_vars = push_array(builder->arena, Bcode_Var, builder->global_var_capacity);
	
	builder->proc_count    = 1; // Null procedure, to reduce codepaths later
	builder->proc_capacity = builder->table->proc_count + 1;
	builder->procs = push_array(builder->arena, Bcode_Proc, builder->proc_capacity);
	
	return;
}

internal i64 push_bcode_register(Bcode_Builder *builder) {
	i64 result = builder->registers_used;
	builder->registers_used += 1;
	
	return result;
}

internal void pop_bcode_register(Bcode_Builder *builder) {
	builder->registers_used -= 1;
}

////////////////////////////////
//~ Conversion helpers

internal Instr_Operation instr_operation_from_expr_unary(Unary_Operator expr_op) {
	Instr_Operation instr_op = 0;
	
	switch (expr_op) {
		case Unary_Operator_PLUS: { instr_op = BCODE_NOP; } break;
		case Unary_Operator_MINUS: { instr_op = BCODE_NEG; } break;
		// case Unary_Operator_DEREFERENCE: { instr_op = BCODE_DEREFERENCE; } break;
		default: break;
	}
	
	return instr_op;
}

internal Instr_Operation instr_operation_from_expr_binary(Binary_Operator expr_op) {
	Instr_Operation instr_op = 0;
	
	switch (expr_op) {
		case Binary_Operator_PLUS: { instr_op = BCODE_ADD; } break;
		case Binary_Operator_MINUS: { instr_op = BCODE_SUB; } break;
		case Binary_Operator_TIMES: { instr_op = BCODE_MUL; } break;
		case Binary_Operator_DIVIDE: { instr_op = BCODE_DIV; } break;
		// case Binary_Operator_MEMBER: { instr_op = BCODE_COMMA; } break;
		case Binary_Operator_CALL: { instr_op = BCODE_CALL; } break;
		// case Binary_Operator_ARRAY_ACCESS: { instr_op = BCODE_ARRAY_ACCESS; } break;
		default: break;
	}
	
	return instr_op;
}

////////////////////////////////
//~ Main logic

internal void generate_bytecode_for_statement(Bcode_Builder *builder, Ast_Statement *statement);
internal void generate_bytecode_for_declaration(Bcode_Builder *builder, Ast_Declaration *decl);

internal Reg_Group generate_bytecode_for_expression(Bcode_Builder *builder, Ast_Expression *expr) {
	Bcode_Instr instr = {0};
	Reg_Group dests = {0};
	
	switch (expr->kind) {
		case Ast_Expression_Kind_INT_LITERAL: {
			instr = make_bcode_mov_imm2reg(push_bcode_register(builder), expr->i64_value);
			append_bcode_instr(builder, instr);
			
			dests.regs[0] = instr.dest;
			dests.reg_count = 1;
		} break;
		
		case Ast_Expression_Kind_IDENT: {
			Symbol *symbol = expr->symbol;
			assert(symbol, "Null symbol in bytecode generation");
			
			if (symbol->kind == SYMBOL_LOCAL_VAR) {
				instr = make_bcode_load_local(push_bcode_register(builder), symbol->bcode_reg);
			} else {
				// instr = make_bcode_load_global(push_bcode_register(builder), symbol->bcode_reg);
			}
			
			append_bcode_instr(builder, instr);
			
			dests.regs[0] = instr.dest;
			dests.reg_count = 1;
		} break;
		
		// -x        => imul x, -1
		// -(x, y)   => 
		// foo(x)    => mov rdi, x, call foo
		// foo(x, y) => mov rdi, x; mov rsi, y; call foo
		
		case Ast_Expression_Kind_UNARY: {
			Reg_Group sub_dests = generate_bytecode_for_expression(builder, expr->left);
			assert(sub_dests.reg_count >= 1); // Should be ==, but first decide how to treat comma expressions
			
			instr.operation = instr_operation_from_expr_unary(expr->unary);
			instr.mode      = Addressing_Mode_REGISTER;
			instr.source    = sub_dests.regs[0];
			instr.dest      = sub_dests.regs[0];
			
			append_bcode_instr(builder, instr);
			
			dests.regs[0] = instr.dest;
			dests.reg_count = 1;
		} break;
		
		case Ast_Expression_Kind_BINARY: {
			Binary_Operator binary = expr->binary;
			
			if (binary == Binary_Operator_CALL) {
				// For now only identifiers can be lhs of calls
				assert(expr->left->kind == Ast_Expression_Kind_IDENT);
				instr.jump_dest_label = expr->left->ident;
				
				Reg_Group right_dests = generate_bytecode_for_expression(builder, expr->right);
				// assert(right_dests.reg_count >= 1); // Could have no arguments
				
				instr.operation = instr_operation_from_expr_binary(expr->binary);
				instr.mode      = Addressing_Mode_REGISTER;
				
				// TODO: For now do nothing: when executing the next instruction you will be
				// inside the function call and are going to be using the saved registers.
				// When a more robust approach to calling conventions is defined (registers vs stack),
				// maybe some registers could be freed here.
				// builder->registers_used -= 1;
				
				// Remember which registers are used to store the evaluated arguments
				for (int si = 0; si < right_dests.reg_count; si += 1) {
					instr.arg_regs[si] = right_dests.regs[si];
					instr.arg_reg_count += 1;
				}
				
				// Ast_Declaration *callee = decl_list_find_ident(&proc_list, instr.jump_dest_label);
				// assert(callee != NULL); // Typechecking should've failed
				
				append_bcode_instr(builder, instr);
				
				// Pretend that every function has 1 return value and that it is put in register 0.
				// TODO: This is very bad.
				// This is why we should use the stack:
				// before inserting the call instruction, push all live registers onto the stack
				// after the call instruction, pop all registers that were pushed before
				dests.regs[0] = 0;
				dests.reg_count = 1;
			} else if (0 /*&& binary == Binary_Operator_COMMA*/) {
				Reg_Group left_dests  = generate_bytecode_for_expression(builder, expr->left);
				Reg_Group right_dests = generate_bytecode_for_expression(builder, expr->right);
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
				Reg_Group left_dests  = generate_bytecode_for_expression(builder, expr->left);
				Reg_Group right_dests = generate_bytecode_for_expression(builder, expr->right);
				assert(left_dests.reg_count >= 1); // Same as above
				assert(right_dests.reg_count >= 1);
				
				instr.operation = instr_operation_from_expr_binary(expr->binary);
				instr.mode      = Addressing_Mode_REGISTER;
				instr.source    = right_dests.regs[0];
				instr.dest      = left_dests.regs[0];
				builder->registers_used -= 1;         // This instruction puts the result in left.dest;  right.dest can be used by the next instruction
				
				append_bcode_instr(builder, instr);
				
				dests.regs[0] = instr.dest;
				dests.reg_count = 1;
			}
		} break;
		
		default: break;
	}
	
	return dests;
}

internal void generate_bytecode_for_statement(Bcode_Builder *builder, Ast_Statement *statement) {
	switch (statement->kind) {
		case Ast_Statement_Kind_EXPR: {
			generate_bytecode_for_expression(builder, statement->expr);
		} break;
		
		case Ast_Statement_Kind_BLOCK: {
			for (Ast_Statement *s = statement->block; s != NULL && s != &nil_statement; s = s->next) {
				generate_bytecode_for_statement(builder, s);
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
					Reg_Group dests = generate_bytecode_for_expression(builder, expr);
					
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
						Reg_Group dests = generate_bytecode_for_expression(builder, expr);
						stack_pop(expr);
						
						for (int di = 0; di < dests.reg_count; di += 1) {
							unmapped_ret_regs[unmapped_ret_reg_count] = dests.regs[di];
							unmapped_ret_reg_count += 1;
						}
					}
				}
#else
				Reg_Group dests = generate_bytecode_for_expression(builder, statement->expr);
				for (int di = 0; di < dests.reg_count; di += 1) {
					unmapped_ret_regs[unmapped_ret_reg_count] = dests.regs[di];
					unmapped_ret_reg_count += 1;
				}
#endif
				
				for (int i = 0; i < unmapped_ret_reg_count; i += 1) {
					if (unmapped_ret_regs[i] != i) {
						Instr swap = {0};
						
						swap.operation = BCODE_SWAP;
						swap.dest      = unmapped_ret_regs[i];
						swap.source    = i;
						swap.mode      = Addressing_Mode_REGISTER;
						
						append_bcode_instr(builder, swap);
					}
				}
				
				retval_count = unmapped_ret_reg_count;
			}
#endif
			
			ret.operation = BCODE_RETURN;
			ret.ret_reg_count = retval_count; // TODO: Remove; it should be stored in the procedure defition, not here
			
			append_bcode_instr(builder, ret);
		} break;
		
		case Ast_Statement_Kind_DECLARATION: {
			generate_bytecode_for_declaration(builder, statement->decl);
		} break;
		
		default: break;
	}
}

internal void generate_bytecode_for_declaration(Bcode_Builder *builder, Ast_Declaration *decl) {
#if 1
	assert(!check_nil_declaration(decl));
	
	for (int i = 0; i < decl->entity_count; i += 1) {
		Entity *entity = &decl->entities[i];
		Symbol *symbol = entity->symbol;
		assert(symbol != NULL, "Symbol lookup failed in bytecode generation");
		
		if (symbol->kind == SYMBOL_LOCAL_VAR) {
			
			Instr instr = {0};
			
			int original_reg = push_bcode_register(builder);
			
			instr = make_bcode_alloca(original_reg, symbol->type->size);
			append_bcode_instr(builder, instr);
			
			symbol->bcode_reg = instr.dest;
			
			// Initialize the variable to 0 by default
			// TODO: Use STORE for atomic types, call MEMSET for others
			
			memset(&instr, 0, sizeof(instr));
			
			instr.operation = BCODE_STORE;
			instr.dest = original_reg;
			instr.source = 0;
			instr.store_mode = Addressing_Mode_CONSTANT;
			
			append_bcode_instr(builder, instr);
			
			// Evaluate initializer
			
			if (entity->initter != NULL) {
				// TODO: Different initter_value_index handling
				
				if (entity->initter->kind == Initter_Kind_EXPR) {
					assert(!check_nil_expression(entity->initter->expr));
					
					Reg_Group g = generate_bytecode_for_expression(builder, entity->initter->expr);
					
					memset(&instr, 0, sizeof(instr));
					
					instr.operation = BCODE_STORE;
					instr.dest = original_reg;
					instr.source = g.regs[0];
					instr.store_mode = Addressing_Mode_REGISTER;
					
					append_bcode_instr(builder, instr);
					
					allow_break();
				} else {
					// TODO: Local var is not a "value" (but either a proc or a type). What to do?
				}
			}
		} else if (symbol->kind == SYMBOL_GLOBAL_VAR) {
			assert(builder->global_var_count < builder->global_var_capacity, "Not enough space for bcode global var");
			
			Bcode_Var *prev = &builder->global_vars[builder->global_var_count - 1];
			
			Bcode_Var var = {0};
			var.address = prev->address + prev->size;
			var.size = symbol->type->size;
			var.ident = symbol->ident;
			
			symbol->bcode_address = var.address;
			
			builder->global_vars[builder->global_var_count] = var;
			builder->global_var_count += 1;
		} else if (symbol->kind == SYMBOL_PROC) {
			assert(entity->initter_value_index == 0);
			
			assert(!check_nil_statement(decl->initters[i].body));
			assert(decl->initters[i].body->kind == Ast_Statement_Kind_BLOCK);
			
			assert(builder->proc_count < builder->proc_capacity, "Not enough space for bcode proc");
			
			Bcode_Proc proc = {0};
			proc.name = entity->ident;
			
			builder->procs[builder->proc_count] = proc;
			builder->proc_count += 1;
			
			// This appends to the last proc in the buffer. Do this AFTER putting the proc in the buffer
			// and incrementing proc_count.
			Bcode_Block *new_block = push_bcode_block(builder);
			(void) new_block;
			
			Initter *initter = entity->initter;
			generate_bytecode_for_statement(builder, initter->body);
			
			allow_break();
		} else {
			allow_break();
		}
	}
#endif
	
	return;
}

internal void generate_bcode(Bcode_Builder *builder, Ast_Declaration *prog) {
	for (Ast_Declaration *decl = prog; !check_nil_declaration(decl); decl = decl->next) {
		generate_bytecode_for_declaration(builder, decl);
	}
	
	printf("## Generated bytecode ##\n");
	for (i64 proc_index = 1; proc_index < builder->proc_count; proc_index += 1) {
		printf("proc %.*s\n", string_expand(builder->procs[proc_index].name));
		for (Bcode_Block *block = builder->procs[proc_index].first_block; block; block = block->next) {
			for (i64 instr_index = 0; instr_index < block->instruction_count; instr_index += 1) {
				print_bcode_instr(block->instructions[instr_index]);
				printf("\n");
			}
		}
	}
	printf("\n");
	
	return;
}

#endif
