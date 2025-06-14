#include "el_ctx_crack.h"
#include "el_base.h"

#include "el_base.c"

// Tree

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
	int value;
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
	
	Expr *plus = malloc(sizeof(Expr));
	plus->kind = Expr_Kind_BINARY;
	plus->operation = Expr_Operation_ADD;
	plus->left = malloc(sizeof(Expr));
	plus->right = malloc(sizeof(Expr));
	plus->value = 0;
	
	{
		Expr *times = plus->left;
		times->kind = Expr_Kind_BINARY;
		times->operation = Expr_Operation_MUL;
		times->left = malloc(sizeof(Expr));
		times->right = malloc(sizeof(Expr));
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
		div->left = malloc(sizeof(Expr));
		div->right = malloc(sizeof(Expr));
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
			plus2->left = malloc(sizeof(Expr));
			plus2->right = malloc(sizeof(Expr));
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
	
	return plus;
}

// Intermediate rep

typedef enum Instr_Operation {
	Instr_Operation_SET,
	Instr_Operation_ADD,
	Instr_Operation_SUB,
	Instr_Operation_MUL,
	Instr_Operation_DIV,
	Instr_Operation_COUNT,
} Instr_Operation;

typedef enum Addressing_Mode {
	Addressing_Mode_CONSTANT,
	Addressing_Mode_REGISTER,
	Addressing_Mode_COUNT,
} Addressing_Mode;

typedef struct Instr Instr;
struct Instr {
	String label;
	Instr_Operation operation;
	Addressing_Mode mode;
	int source; // Register
	int dest;   // Register
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
generate_pseudocode_for_expression(Expr *expr) {
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
			Instr *left  = generate_pseudocode_for_expression(expr->left);
			Instr *right = generate_pseudocode_for_expression(expr->right);
			
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

// MASM source

static Arena masm_arena;
static String_List masm_lines;

static void
append_masm_line(String line) {
	string_list_push(&masm_arena, &masm_lines, string_clone(&masm_arena, line));
}

static String
generate_masm_source(void) {
	
	append_masm_line(string_from_lit("; Generated"));
	append_masm_line(string_from_lit("includelib msvcrt.lib"));
	append_masm_line(string_from_lit(".data"));
	append_masm_line(string_from_lit(".code")); // Not .text, aparently
	
	append_masm_line(string_from_lit("main proc"));
	
	for (int i = 0; i < instruction_count; i += 1) {
		Instr *instr = &instructions[i];
		switch (instr->operation) {
			case Instr_Operation_SET: {
				char buf[256] = {0};
				int buf_len = 0;
				if (instr->mode == Addressing_Mode_CONSTANT) {
					buf_len = snprintf(buf, array_count(buf), "mov r%d, %d", instr->dest+8, instr->source);
				} else {
					buf_len = snprintf(buf, array_count(buf), "mov r%d, r%d", instr->dest+8, instr->source+8);
				}
				
				append_masm_line(string(buf, buf_len));
			} break;
			
			case Instr_Operation_ADD: {
				char buf[256] = {0};
				int buf_len = 0;
				
				if (instr->mode == Addressing_Mode_CONSTANT) {
					buf_len = snprintf(buf, array_count(buf), "add r%d, %d", instr->dest+8, instr->source);
				} else {
					buf_len = snprintf(buf, array_count(buf), "add r%d, r%d", instr->dest+8, instr->source+8);
				}
				
				append_masm_line(string(buf, buf_len));
			} break;
			
			case Instr_Operation_SUB: {
				char buf[256] = {0};
				int buf_len = 0;
				
				if (instr->mode == Addressing_Mode_CONSTANT) {
					buf_len = snprintf(buf, array_count(buf), "sub r%d, %d", instr->dest+8, instr->source);
				} else {
					buf_len = snprintf(buf, array_count(buf), "sub r%d, r%d", instr->dest+8, instr->source+8);
				}
				
				append_masm_line(string(buf, buf_len));
			} break;
			
			case Instr_Operation_MUL: {
				char buf[256] = {0};
				int buf_len = 0;
				
				if (instr->mode == Addressing_Mode_CONSTANT) {
					buf_len = snprintf(buf, array_count(buf), "imul r%d, %d", instr->dest+8, instr->source);
				} else {
					buf_len = snprintf(buf, array_count(buf), "imul r%d, r%d", instr->dest+8, instr->source+8);
				}
				
				append_masm_line(string(buf, buf_len));
			} break;
			
			default: break;
		}
	}
	
	append_masm_line(string_from_lit("mov rax, r8"));
	
	append_masm_line(string_from_lit("ret"));
	append_masm_line(string_from_lit("main endp"));
	
	append_masm_line(string_from_lit("end"));
	
	return string_from_list(&masm_arena, masm_lines,
							.sep = string_from_lit("\n"),
							.suf = string_from_lit("\n"));
}

int main(void) {
	arena_init(&masm_arena);
	
	Expr *program = hardcode_an_expression();
	generate_pseudocode_for_expression(program);
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
