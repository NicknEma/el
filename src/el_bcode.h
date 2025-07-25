#ifndef EL_BCODE_H
#define EL_BCODE_H

#define BCODE_OPERATION_NAMES \
X(BCODE_NULL) \
X(BCODE_NOP) \
X(BCODE_ALLOCA) \
X(BCODE_STORE) \
X(BCODE_LOAD) \
X(BCODE_SET) \
X(BCODE_NEG) \
X(BCODE_ADD) \
X(BCODE_SUB) \
X(BCODE_MUL) \
X(BCODE_DIV) \
X(BCODE_CALL) \
X(BCODE_RETURN) \
X(BCODE_SWAP) \
X(BCODE_COUNT) \

typedef enum Bcode_Operation {
#define X(name) name,
	BCODE_OPERATION_NAMES
#undef  X
} Bcode_Operation;

typedef Bcode_Operation Instr_Operation; // TODO: Temporary

global read_only String bcode_operation_names[] = {
#define X(name) string_from_lit_const(#name),
	BCODE_OPERATION_NAMES
#undef  X
};

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

// NOTE: The only expressions that can evaluate to multiple values are function calls, so as long as
// the code for the return statement of the callee puts values in consecutive registers,
// we can expect the expression dests to be in consecutive registers.
typedef struct Bcode_Reg_Span Bcode_Reg_Span;
struct Bcode_Reg_Span {
	int first, count;
};

typedef struct Instr Instr;
struct Instr {
	String label;
	
	String jump_dest_label; // For jumps and procedure calls.
	Bcode_Operation operation;
	
	union { int source; int source_register; i64 source_imm; };
	union { int dest; int dest_register; }; // Union is temporary
	
	// int ret_regs[8]; // Arbitrary number for now
	int ret_reg_count;
	Bcode_Reg_Span ret_registers;
	
	int arg_regs[8]; // Arbitrary number for now
	int arg_reg_count;
	Bcode_Reg_Span arg_registers;
	
	int alloca_size;
	
	union {
		Addressing_Mode mode;
		Addressing_Mode store_mode;
	};
	
	String comment;
};

typedef Instr Bcode_Instr; // Temporary

typedef struct Bcode_Block Bcode_Block;
struct Bcode_Block {
	Instr *instructions;
	i64    instruction_count;
	i64    instruction_capacity;
	
	Bcode_Block *next;
};

typedef struct Bcode_Proc Bcode_Proc;
struct Bcode_Proc {
	Bcode_Block  *first_block;
	Bcode_Block  *last_block;
	
	String name;
};

// TODO: Store the (evaluated?) initializer here
typedef struct Bcode_Var Bcode_Var;
struct Bcode_Var {
	String ident;
	i64 address;
	i64 size;
};

typedef struct Bcode_Builder Bcode_Builder;
struct Bcode_Builder {
	Arena *arena;
	Symbol_Table *table;
	
	Bcode_Proc *procs;
	i64         proc_count;
	i64         proc_capacity;
	
	Bcode_Var  *global_vars;
	i64         global_var_count;
	i64         global_var_capacity;
	
	int registers_used;
};

#endif
