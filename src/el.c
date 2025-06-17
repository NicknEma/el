#include "el_ctx_crack.h"
#include "el_base.h"

#include "el_base.c"

#include "el_x64.c"

static void *
cmalloc(size_t size) {
	return calloc(1, size);
}

////////////////////////////////
//~ Program Tree

//- Expressions

typedef enum Expr_Kind {
	Expr_Kind_NULL,
	Expr_Kind_LITERAL,
	Expr_Kind_UNARY,
	Expr_Kind_BINARY,
	Expr_Kind_COUNT,
} Expr_Kind;

typedef enum Expr_Unary {
	Expr_Unary_NULL,
	Expr_Unary_CALL,
	Expr_Unary_COUNT,
} Expr_Unary;

typedef enum Expr_Binary {
	Expr_Binary_ADD,
	Expr_Binary_SUB,
	Expr_Binary_MUL,
	Expr_Binary_DIV,
	Expr_Binary_COUNT,
} Expr_Binary;

typedef struct Expr Expr;
struct Expr {
	Expr_Kind kind;
	Expr_Unary unary;
	Expr_Binary binary;
	String ident;
	Expr *left;
	Expr *right;
	Expr *next;
	int value;
};

//- Statements

typedef enum Statement_Kind {
	Statement_Kind_EXPR,
	Statement_Kind_RETURN,
	Statement_Kind_COUNT,
} Statement_Kind;

typedef struct Statement Statement;
struct Statement {
	Statement_Kind kind;
	Statement     *next;
	
	Expr *expr;
};

//- Declarations

typedef enum Declaration_Kind {
	Declaration_Kind_PROCEDURE,
	Declaration_Kind_COUNT,
} Declaration_Kind;

typedef struct Declaration Declaration;
struct Declaration {
	Declaration_Kind kind;
	Declaration     *next;
	String           ident;
	
	Statement *body;
};

//- Testing

static Expr *
hardcode_an_expression(void) {
	/*
** { Plus }
  ** ** { Times }
    ** ** ** { 2 }
    ** ** ** { 3 }
  ** ** { Div }
    ** ** ** { 10 }
    ** ** ** { Plus }
      ** ** ** ** { 4 }
** ** ** ** { 1 }
*/
	
	/*
** { Sub }
** ** { 7 }
** ** { 0 }
*/
	
	Expr *plus = cmalloc(sizeof(Expr));
	plus->kind = Expr_Kind_BINARY;
	plus->binary = Expr_Binary_ADD;
	plus->left = cmalloc(sizeof(Expr));
	plus->right = cmalloc(sizeof(Expr));
	plus->value = 0;
	
	{
		Expr *times = plus->left;
		times->kind = Expr_Kind_BINARY;
		times->binary = Expr_Binary_MUL;
		times->left = cmalloc(sizeof(Expr));
		times->right = cmalloc(sizeof(Expr));
		times->value = 0;
		
		{
			Expr *two = times->left;
			two->kind = Expr_Kind_LITERAL;
			two->binary = 0;
			two->left = 0;
			two->right = 0;
			two->value = 2;
		}
		
		{
			Expr *three = times->right;
			three->kind = Expr_Kind_LITERAL;
			three->binary = 0;
			three->left = 0;
			three->right = 0;
			three->value = 3;
		}
	}
	
	{
		Expr *div = plus->right;
		div->kind = Expr_Kind_BINARY;
		div->binary = Expr_Binary_DIV;
		div->left = cmalloc(sizeof(Expr));
		div->right = cmalloc(sizeof(Expr));
		div->value = 0;
		
		{
			Expr *ten = div->left;
			ten->kind = Expr_Kind_LITERAL;
			ten->binary = 0;
			ten->left = 0;
			ten->right = 0;
			ten->value = 10;
		}
		
		{
			Expr *plus2 = div->right;
			plus2->kind = Expr_Kind_BINARY;
			plus2->binary = Expr_Binary_ADD;
			plus2->left = cmalloc(sizeof(Expr));
			plus2->right = cmalloc(sizeof(Expr));
			plus2->value = 0;
			
			{
				Expr *four = plus2->left;
				four->kind = Expr_Kind_LITERAL;
				four->binary = 0;
				four->left = 0;
				four->right = 0;
				four->value = 4;
			}
			
			{
				Expr *one = plus2->right;
				one->kind = Expr_Kind_LITERAL;
				one->binary = 0;
				one->left = 0;
				one->right = 0;
				one->value = 1;
			}
		}
	}
	
#if 0
	Expr *minus = cmalloc(sizeof(Expr));
	minus->kind = Expr_Kind_BINARY;
	minus->binary = Expr_Binary_SUB;
	minus->left = cmalloc(sizeof(Expr));
	minus->right = cmalloc(sizeof(Expr));
	minus->value = 0;
	
	{
		Expr *seven = minus->left;
		seven->kind = Expr_Kind_LITERAL;
		seven->binary = 0;
		seven->left = 0;
		seven->right = 0;
		seven->value = 7;
	}
	
	{
		Expr *zero = minus->right;
		zero->kind = Expr_Kind_LITERAL;
		zero->binary = 0;
		zero->left = 0;
		zero->right = 0;
		zero->value = 0;
	}
	
	plus->next = minus;
#endif
	
	return plus;
}

static Statement *
hardcode_a_statement(void) {
	Statement *statement = malloc(sizeof(Statement));
	statement->kind = Statement_Kind_EXPR;
	statement->expr = hardcode_an_expression();
	
	statement->next = malloc(sizeof(Statement));
	statement->next->kind = Statement_Kind_RETURN;
	statement->next->expr = statement->expr;
	statement->next->next = NULL;
	
	return statement;
}

static Declaration *
hardcode_a_declaration(void) {
	Declaration *main_decl = cmalloc(sizeof(Declaration));
	main_decl->kind = Declaration_Kind_PROCEDURE;
	main_decl->next = cmalloc(sizeof(Declaration));
	main_decl->ident = string_from_lit("main");
	
	{
		main_decl->body = cmalloc(sizeof(Statement));
		main_decl->body->kind = Statement_Kind_RETURN;
		main_decl->body->expr = cmalloc(sizeof(Expr));
		main_decl->body->expr->kind = Expr_Kind_UNARY;
		main_decl->body->expr->unary = Expr_Unary_CALL;
		main_decl->body->expr->ident = string_from_lit("other");
	}
	
	{
		Declaration *next_decl = main_decl->next;
		next_decl->kind = Declaration_Kind_PROCEDURE;
		next_decl->ident = string_from_lit("other");
		
		next_decl->body = hardcode_a_statement();
	}
	
	return main_decl;
}

////////////////////////////////
//~ Bytecode

typedef enum Instr_Operation {
	Instr_Operation_NULL,
	
	Instr_Operation_SET,
	Instr_Operation_ADD,
	Instr_Operation_SUB,
	Instr_Operation_MUL,
	Instr_Operation_DIV,
	
	Instr_Operation_CALL,
	Instr_Operation_RETURN,
	
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

typedef struct Instr Instr;
struct Instr {
	String label;
	Label_Kind label_kind;
	String jump_dest_label; // For jumps and procedure calls.
	Instr_Operation operation;
	Addressing_Mode mode;
	int source; // Register
	int dest;   // Register
	
	int ret_regs[8]; // Arbitrary number for now
	int ret_reg_count;
};

static Instr instructions[256];
static int instruction_count;

static int registers_used;

static Instr_Operation
instr_operation_from_expr_unary(Expr_Unary expr_op) {
	Instr_Operation instr_op = 0;
	
	switch (expr_op) {
		case Expr_Unary_CALL: { instr_op = Instr_Operation_CALL; } break;
		default: break;
	}
	
	return instr_op;
}

static Instr_Operation
instr_operation_from_expr_binary(Expr_Binary expr_op) {
	Instr_Operation instr_op = 0;
	
	switch (expr_op) {
		case Expr_Binary_ADD: { instr_op = Instr_Operation_ADD; } break;
		case Expr_Binary_SUB: { instr_op = Instr_Operation_SUB; } break;
		case Expr_Binary_MUL: { instr_op = Instr_Operation_MUL; } break;
		case Expr_Binary_DIV: { instr_op = Instr_Operation_DIV; } break;
		default: break;
	}
	
	return instr_op;
}

static Instr *
generate_bytecode_for_expression(Expr *expr) {
	Instr *instr = NULL;
	
	switch (expr->kind) {
		case Expr_Kind_LITERAL: {
			instr = &instructions[instruction_count];
			instruction_count += 1;
			
			instr->label.len  = 0;
			instr->label.data = NULL;
			
			instr->operation = Instr_Operation_SET;
			instr->mode      = Addressing_Mode_CONSTANT;
			instr->source    = expr->value;
			instr->dest      = registers_used;
			registers_used  += 1;
		} break;
		
		// -x        => imul x, -1
		// -(x, y)   => 
		// foo(x)    => mov rdi, x, call foo
		// foo(x, y) => mov rdi, x; mov rsi, y; call foo
		
		case Expr_Kind_UNARY: {
			int source = 0, dest = 0;
			
			if (expr->left) {
				Instr *sub = generate_bytecode_for_expression(expr->left); // Only 1 argument for now
				source = sub->source;
				dest   = sub->dest;
			}
			
			instr = &instructions[instruction_count];
			instruction_count += 1;
			
			instr->label.len  = 0;
			instr->label.data = NULL;
			
			instr->operation = instr_operation_from_expr_unary(expr->unary);
			instr->mode      = Addressing_Mode_REGISTER;
			instr->source    = dest;
			instr->dest      = dest; // If it's a procedure call, this is useless
			
			instr->jump_dest_label = expr->ident;
		} break;
		
		case Expr_Kind_BINARY: {
			Instr *left  = generate_bytecode_for_expression(expr->left);
			Instr *right = generate_bytecode_for_expression(expr->right);
			
			instr = &instructions[instruction_count];
			instruction_count += 1;
			
			instr->label.len  = 0;
			instr->label.data = NULL;
			
			instr->operation = instr_operation_from_expr_binary(expr->binary);
			instr->mode      = Addressing_Mode_REGISTER;
			instr->source    = right->dest;
			instr->dest      = left->dest;
			registers_used  -= 1;         // This instruction puts the result in left.dest;  right.dest can be used by the next instruction
		} break;
		
		default: break;
	}
	
	return instr;
}

static void
generate_bytecode_for_statement(Statement *statement) {
	switch (statement->kind) {
		case Statement_Kind_EXPR: {
			for (Expr *expr = statement->expr; expr != NULL; expr = expr->next) {
				generate_bytecode_for_expression(expr);
			}
		} break;
		
		case Statement_Kind_RETURN: {
			
			Instr ret = {0};
			
			{
				// Remember the destination register of each of the returned expressions,
				// as well as how many there are.
				int i = 0;
				for (Expr *expr = statement->expr; expr != NULL; expr = expr->next) {
					Instr *retval = generate_bytecode_for_expression(expr);
					
					assert(ret.ret_reg_count < array_count(ret.ret_regs));
					ret.ret_regs[ret.ret_reg_count] = retval->dest;
					ret.ret_reg_count += 1;
					
					i += 1;
				}
			}
			
			ret.operation = Instr_Operation_RETURN;
			
			// Return from the procedure
			instructions[instruction_count] = ret;
			instruction_count += 1;
		} break;
		
		default: break;
	}
}

static void
generate_bytecode_for_declaration(Declaration *declaration) {
	switch (declaration->kind) {
		case Declaration_Kind_PROCEDURE: {
			Instr instr = {0};
			
			instr.label      = declaration->ident;
			instr.label_kind = Label_Kind_PROCEDURE;
			instr.operation  = Instr_Operation_NULL;
			
			instructions[instruction_count] = instr;
			instruction_count += 1;
			
			for (Statement *statement = declaration->body; statement != NULL; statement = statement->next) {
				generate_bytecode_for_statement(statement);
			}
			
			assert(instructions[instruction_count-1].operation == Instr_Operation_RETURN);
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

static MASM_Context masm_context = {
	.indent_string = string_from_lit_const("\t"),
};

static void
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

static String
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

static String
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
					
					String source = masm_register_from_bytecode_register(instr->source);
					
					String line = push_stringf(scratch.arena, "mov rdi, %.*s", string_expand(source));
					masm_append_line(line);
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
			
			case Instr_Operation_RETURN: {
				
				{
					// Convert the bytecode calling convention to the platform calling convention:
					// For now, just map BYTECODE_RETURN_REGISTER_0 to rax
					
					if (instr->ret_reg_count == 1) {
						String source = masm_register_from_bytecode_register(instr->ret_regs[0]);
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
	x64_test();
	
	arena_init(&masm_context.arena);
	
	Declaration *program = hardcode_a_declaration();
	for (Declaration *decl = program; decl != NULL; decl = decl->next) {
		generate_bytecode_for_declaration(decl);
	}
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
