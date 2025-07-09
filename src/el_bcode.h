#ifndef EL_BCODE_H
#define EL_BCODE_H

#define BCODE_OPERATION_NAMES \
X(BCODE_NULL) \
X(BCODE_NOP) \
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
	Bcode_Operation operation;
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
