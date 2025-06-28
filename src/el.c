#include "el_ctx_crack.h"
#include "el_base.h"

#include "el_base.c"

#include "el_ast.h"
#include "el_check.h"

#include "el_ast_parse.c"
#include "el_ast_print.c"
#include "el_check.c"

#include "el_x64.c"

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
		case Ast_Expression_Kind_INT_LITERAL: {
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
#if 0
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
#endif
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

#include "el_all_tests.c"

/*
** TODO(ema):
** [ ] Printer
** [ ] Input reading
*/

int main(void) {
	test_all();
	printf("### Main program output ###\n\n");
	
#if 0
	String source = string_from_lit("main :: proc() { return other(); }"
									"other :: proc() { return 2*3 + 10/(4+1); 7-0; }");
#else
	String source = string_from_lit("a := 0\n"
									"\n"
									"main :: proc() {\n"
									"a = 5;\n"
									"a := \"Hello\";\n"
									"a = \"world\";\n"
									"\n"
									"{\n"
									"a = \"goodbye\";\n"
									"a := \"moon\";\n"
									"}\n"
									"\n"
									"b = 69.420;\n"
									"\n"
									"c := C;\n"
									"C :: 1;\n"
									"\n"
									"d := D;"
									"}\n"
									"\n"
									"b := 314\n"
									"D :: 0\n"
									);
#endif
	
	bool all_ok = true;
	
	Arena tree_arena = {0};
	arena_init(&tree_arena);
	
	Parse_Context parser = {0};
	parser_init(&parser, &tree_arena, source);
	
	Ast_Declaration *program = &nil_declaration;
	program = parse_program(&parser);
	
	if (there_were_parse_errors(&parser)) {
		all_ok = false;
	}
	
	if (all_ok) {
		do_all_checks(program);
		
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
	}
	
	return 0;
}
