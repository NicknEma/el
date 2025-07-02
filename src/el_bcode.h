#ifndef EL_BCODE_H
#define EL_BCODE_H

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

#endif
