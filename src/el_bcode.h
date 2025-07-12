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
	
	int source; // Register
	union { int dest; int dest_register; }; // Union is temporary
	
	// int ret_regs[8]; // Arbitrary number for now
	int ret_reg_count;
	
	int arg_regs[8]; // Arbitrary number for now
	int arg_reg_count;
	
	int alloca_size;
	
	union {
		Addressing_Mode mode;
		Addressing_Mode store_mode;
	};
};

typedef Instr Bcode_Instr; // Temporary

// Moves the stack pointer by 'size' bytes and stores a pointer to the original address into 'dest_register'.
internal Bcode_Instr make_bcode_alloca(int dest_register, int size) {
	Bcode_Instr result = {0};
	
	result.operation     = BCODE_ALLOCA;
	result.dest_register = dest_register;
	result.alloca_size   = size;
	
	return result;
}


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

#endif
