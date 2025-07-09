#ifndef EL_BCODE_C
#define EL_BCODE_C

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

internal Reg_Group generate_bytecode_for_expression(Ast_Expression *expr) {
	Instr instr = {0};
	Reg_Group dests = {0};
	
	switch (expr->kind) {
		case Ast_Expression_Kind_INT_LITERAL: {
			instr.operation = BCODE_SET;
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

internal void generate_bytecode_for_statement(Ast_Statement *statement) {
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
						
						swap.operation = BCODE_SWAP;
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
			
			ret.operation = BCODE_RETURN;
			ret.ret_reg_count = retval_count; // TODO: Remove; it should be stored in the procedure defition, not here
			
			instructions[instruction_count] = ret;
			instruction_count += 1;
		} break;
		
		default: break;
	}
}

internal void generate_bytecode_for_declaration(Symbol_Table *table, Ast_Declaration *decl) {
#if 1
	assert(!check_nil_declaration(decl));
	
	int entities_done = 0;
	for (int i = 0; i < decl->initter_count; i += 1) {
		if (decl->initters[i].kind == Initter_Kind_PROCEDURE) {
			assert(!check_nil_statement(decl->initters[i].body));
			assert(decl->initters[i].body->kind == Ast_Statement_Kind_BLOCK);
			
			Entity e = decl->entities[entities_done];
			Initter *initter = &decl->initters[i];
			
			Symbol *symbol = lookup_symbol(table, e.ident);
			assert(symbol != NULL, "Symbol lookup failed in bytecode generation");
			assert(symbol->type->kind == TYPE_PROC);
			
			Instr instr = {0};
			
			instr.label      = e.ident;
			instr.label_kind = Label_Kind_PROCEDURE;
			instr.operation  = BCODE_NULL;
			
			instructions[instruction_count] = instr;
			instruction_count += 1;
			
			generate_bytecode_for_statement(initter->body);
			
			if (instructions[instruction_count-1].operation != BCODE_RETURN) {
				fprintf(stderr, "Warning: Unreachable code after return statement.\n");
			}
			
			entities_done += 1;
		} else {
			allow_break();
		}
	}
#endif
	
	return;
}

#endif
