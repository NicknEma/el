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
	Expr_Kind_LITERAL,
	Expr_Kind_UNARY,
	Expr_Kind_BINARY,
	Expr_Kind_COUNT,
} Expr_Kind;

typedef enum Expr_Operation {
	Expr_Operation_ADD,
	Expr_Operation_SUB,
	Expr_Operation_MUL,
	Expr_Operation_DIV,
	Expr_Operation_COUNT,
} Expr_Operation;

typedef struct Expr Expr;
struct Expr {
	Expr_Kind kind;
	Expr_Operation operation;
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
	plus->operation = Expr_Operation_ADD;
	plus->left = cmalloc(sizeof(Expr));
	plus->right = cmalloc(sizeof(Expr));
	plus->value = 0;
	
	{
		Expr *times = plus->left;
		times->kind = Expr_Kind_BINARY;
		times->operation = Expr_Operation_MUL;
		times->left = cmalloc(sizeof(Expr));
		times->right = cmalloc(sizeof(Expr));
		times->value = 0;
		
		{
			Expr *two = times->left;
			two->kind = Expr_Kind_LITERAL;
			two->operation = 0;
			two->left = 0;
			two->right = 0;
			two->value = 2;
		}
		
		{
			Expr *three = times->right;
			three->kind = Expr_Kind_LITERAL;
			three->operation = 0;
			three->left = 0;
			three->right = 0;
			three->value = 3;
		}
	}
	
	{
		Expr *div = plus->right;
		div->kind = Expr_Kind_BINARY;
		div->operation = Expr_Operation_DIV;
		div->left = cmalloc(sizeof(Expr));
		div->right = cmalloc(sizeof(Expr));
		div->value = 0;
		
		{
			Expr *ten = div->left;
			ten->kind = Expr_Kind_LITERAL;
			ten->operation = 0;
			ten->left = 0;
			ten->right = 0;
			ten->value = 10;
		}
		
		{
			Expr *plus2 = div->right;
			plus2->kind = Expr_Kind_BINARY;
			plus2->operation = Expr_Operation_ADD;
			plus2->left = cmalloc(sizeof(Expr));
			plus2->right = cmalloc(sizeof(Expr));
			plus2->value = 0;
			
			{
				Expr *four = plus2->left;
				four->kind = Expr_Kind_LITERAL;
				four->operation = 0;
				four->left = 0;
				four->right = 0;
				four->value = 4;
			}
			
			{
				Expr *one = plus2->right;
				one->kind = Expr_Kind_LITERAL;
				one->operation = 0;
				one->left = 0;
				one->right = 0;
				one->value = 1;
			}
		}
	}
	
#if 0
	Expr *minus = cmalloc(sizeof(Expr));
	minus->kind = Expr_Kind_BINARY;
	minus->operation = Expr_Operation_SUB;
	minus->left = cmalloc(sizeof(Expr));
	minus->right = cmalloc(sizeof(Expr));
	minus->value = 0;
	
	{
		Expr *seven = minus->left;
		seven->kind = Expr_Kind_LITERAL;
		seven->operation = 0;
		seven->left = 0;
		seven->right = 0;
		seven->value = 7;
	}
	
	{
		Expr *zero = minus->right;
		zero->kind = Expr_Kind_LITERAL;
		zero->operation = 0;
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

////////////////////////////////
//~ Bytecode

typedef enum Instr_Operation {
	Instr_Operation_SET,
	Instr_Operation_ADD,
	Instr_Operation_SUB,
	Instr_Operation_MUL,
	Instr_Operation_DIV,
	
	Instr_Operation_RETURN,
	
	Instr_Operation_COUNT,
} Instr_Operation;

typedef enum Addressing_Mode {
	Addressing_Mode_CONSTANT,
	Addressing_Mode_REGISTER,
	Addressing_Mode_COUNT,
} Addressing_Mode;

#define BYTECODE_RETURN_REGISTER_0 0

typedef struct Instr Instr;
struct Instr {
	String label;
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
instr_operation_from_expr_operation(Expr_Operation expr_op) {
	Instr_Operation instr_op = 0;
	
	switch (expr_op) {
		case Expr_Operation_ADD: { instr_op = Instr_Operation_ADD; } break;
		case Expr_Operation_SUB: { instr_op = Instr_Operation_SUB; } break;
		case Expr_Operation_MUL: { instr_op = Instr_Operation_MUL; } break;
		case Expr_Operation_DIV: { instr_op = Instr_Operation_DIV; } break;
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
			
			instr->label     = string_from_lit("");
			instr->operation = Instr_Operation_SET;
			instr->mode      = Addressing_Mode_CONSTANT;
			instr->source    = expr->value;
			instr->dest      = registers_used;
			registers_used  += 1;
		} break;
		
		case Expr_Kind_BINARY: {
			Instr *left  = generate_bytecode_for_expression(expr->left);
			Instr *right = generate_bytecode_for_expression(expr->right);
			
			instr = &instructions[instruction_count];
			instruction_count += 1;
			
			instr->label     = string_from_lit("");
			instr->operation = instr_operation_from_expr_operation(expr->operation);
			instr->mode      = Addressing_Mode_REGISTER;
			instr->source    = right->dest;
			instr->dest      = left->dest;
			registers_used  -= 1;         // This instruction puts the result in left.dest;  right.dest can be used by the next instruction
		} break;
		
		default: break;
	}
	
	return instr;
}

static int
bytecode_register_from_index(int index) {
	return index; // For now, expression indices are mapped 1:1 to register indices
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
#if 0
			int i = 0;
			for (Expr *expr = statement->expr; expr != NULL; expr = expr->next) {
				Instr *retval = generate_bytecode_for_expression(expr);
				
				// If the return value isn't already in the correct return register, move it there
				int return_register = bytecode_register_from_index(i);
				if (retval->dest != return_register) {
					Instr *set = &instructions[instruction_count];
					instruction_count += 1;
					
					set->label     = string_from_lit("");
					set->operation = Instr_Operation_SET;
					set->mode      = Addressing_Mode_REGISTER;
					set->dest      = return_register;
					set->source    = retval->dest;
					
					registers_used -= 1;
				}
				
				i += 1;
			}
#endif
			
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
	
	if (statement->next != NULL) {
		generate_bytecode_for_statement(statement->next);
	}
}

////////////////////////////////
//~ MASM Source

static Arena masm_arena;
static String_List masm_lines;
static int indent_level;
static String indent_bytes = string_from_lit_const("\t");

static void
append_masm_line(String line) {
	Scratch scratch = scratch_begin(0, 0);
	
	int indent_cap = indent_level * indent_bytes.len;
	u8 *indent_buf = push_array(scratch.arena, u8, indent_cap);
	int indent_len = 0;
	
	if (indent_buf != NULL) {
		for (int i = 0; i < indent_level; i += 1) {
			memcpy(indent_buf + indent_len, indent_bytes.data, indent_bytes.len);
			indent_len += indent_bytes.len;
		}
		
		assert(indent_len == indent_cap);
		
		String temp[] = {string(indent_buf, indent_len), line};
		string_list_push(&masm_arena, &masm_lines, strings_concat(&masm_arena, temp, array_count(temp)));
	} else {
		string_list_push(&masm_arena, &masm_lines, string_clone(&masm_arena, line));
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
generate_masm_source(void) {
	
	append_masm_line(string_from_lit("; Generated"));
	append_masm_line(string_from_lit("includelib msvcrt.lib"));
	append_masm_line(string_from_lit(".data"));
	append_masm_line(string_from_lit(".code")); // Not .text, aparently
	
	append_masm_line(string_from_lit("main proc"));
	indent_level += 1;
	
	{
		// Push callee-saved registers
		// TODO: Only do this if necessary
		append_masm_line(string_from_lit("; Procedure prologue"));
		append_masm_line(string_from_lit("push rbx"));
		append_masm_line(string_from_lit("push rbp"));
		append_masm_line(string_from_lit("push r12"));
		append_masm_line(string_from_lit("push r13"));
		append_masm_line(string_from_lit("push r14"));
		append_masm_line(string_from_lit("push r15"));
		append_masm_line(string_from_lit("; Procedure body"));
	}
	
	Scratch scratch = scratch_begin(0, 0);
	
	for (int i = 0; i < instruction_count; i += 1) {
		arena_reset(scratch.arena);
		
		Instr *instr = &instructions[i];
		
		switch (instr->operation) {
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
				append_masm_line(line);
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
						append_masm_line(line);
					} else if (instr->ret_reg_count > 1) {
						String line = push_stringf(scratch.arena, "; Unimplemented returning of multiple values");
						append_masm_line(line);
					}
				}
				
				{
					// Pop callee-saved registers
					// NOTE: Remember that the stack is FILO! Do this in reverse push order.
					// TODO: Only do this if necessary
					append_masm_line(string_from_lit("; Procedure epilogue"));
					append_masm_line(string_from_lit("pop r15"));
					append_masm_line(string_from_lit("pop r14"));
					append_masm_line(string_from_lit("pop r13"));
					append_masm_line(string_from_lit("pop r12"));
					append_masm_line(string_from_lit("pop rbp"));
					append_masm_line(string_from_lit("pop rbx"));
				}
				
				append_masm_line(string_from_lit("ret"));
			} break;
			
			default: {
				String line = push_stringf(scratch.arena, "; Unimplemented instruction '%d'", instr->operation);
				append_masm_line(line);
			} break;
		}
	}
	
	indent_level -= 1;
	append_masm_line(string_from_lit("main endp"));
	
	append_masm_line(string_from_lit("end"));
	
	scratch_end(scratch);
	
	return string_from_list(&masm_arena, masm_lines,
							.sep = string_from_lit("\n"),
							.suf = string_from_lit("\n"));
}

int main(void) {
	x64_test();
	
	arena_init(&masm_arena);
	
	Statement *program = hardcode_a_statement();
	generate_bytecode_for_statement(program);
	String masm_source = generate_masm_source();
	
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
