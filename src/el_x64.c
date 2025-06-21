#ifndef EL_X64_C
#define EL_X64_C

// Structure of a X86-64 instruction
//
// [List of 1-byte prefixes (optional)] [REX prefix (if required, only in 64-bit mode)] [Opcode (mandatory)] [ModR/M byte (if required)] [SIB byte (if required)] [Displacement (optional)] [Immediate (optional)]
//
// REX is an instruction prefix used in 64-bit mode.
//
// The ModR/M byte is:
//     Mod:        u2 = Addressing Mode
//     Reg/Opcode: u3 = Destination Register OR additional Opcode information
//     R/M:        u3 = Operand Register OR is combined with the Mod field for additional Addressing Mode information (R/M stands for Register/Mode)
// The interpretation of the fields depends on the corresponding Opcode.
//
// Certain addressing modes require an additional SIB byte (Scale-Index-Base). For example:
//     add rax, qword ptr [rbp + 4*rcx + rdx]
// 

////////////////////////////////
//~ Machine model

//- Types

typedef enum X64_Register {
	X64_Register_RAX =  0,
	X64_Register_RCX =  1,
	X64_Register_RDX =  2,
	X64_Register_RBX =  3,
	X64_Register_RSP =  4,
	X64_Register_RBP =  5,
	X64_Register_RSI =  6,
	X64_Register_RDI =  7,
	X64_Register_R8  =  8,
	X64_Register_R9  =  9,
	X64_Register_R10 = 10,
	X64_Register_R11 = 11,
	X64_Register_R12 = 12,
	X64_Register_R13 = 13,
	X64_Register_R14 = 14,
	X64_Register_R15 = 15,
} X64_Register;

typedef enum X64_Scale {
	X64_Scale_X1 = 0,
	X64_Scale_X2 = 1,
	X64_Scale_X4 = 2,
	X64_Scale_X8 = 3,
} X64_Scale;

typedef enum X64_Mode {
	X64_Mode_Indirect = 0,
	X64_Mode_Indirect_With_Byte_Displacement = 1,
	X64_Mode_Indirect_With_Multi_Byte_Displacement = 2,
	X64_Mode_Direct = 3,
} X64_Mode;

//- Global variables

global u8  x64_emit_buf[4*4096];
global u8 *x64_emit_ptr = x64_emit_buf;

//- Functions

internal void
x64_emit_u8(u8 v) {
	x64_emit_ptr[0] = v;
	x64_emit_ptr   += 1;
}

internal void
x64_emit_u16(u16 v) {
	x64_emit_u8(cast(u8) ((v >>  0) & 0xFF));
	x64_emit_u8(cast(u8) ((v >>  8) & 0xFF));
}

internal void
x64_emit_u32(u32 v) {
	x64_emit_u8(cast(u8) ((v >>  0) & 0xFF));
	x64_emit_u8(cast(u8) ((v >>  8) & 0xFF));
	x64_emit_u8(cast(u8) ((v >> 16) & 0xFF));
	x64_emit_u8(cast(u8) ((v >> 24) & 0xFF));
}

// Forces 64-bit mode and stores the high bits of the 'reg' and 'rm' fields in ModRM.
internal void
x64_emit_rex(X64_Register reg, X64_Register rm) {
	assert(cast(u8) reg   < 16); // reg   field of the ModRM byte; we only care about the high bit
	assert(cast(u8) rm    < 16); // rm    field of the ModRM byte; we only care about the high bit
	
	// 0x40 is mandatory; 0x08 forces operands to be 64-bits.
	// >> 3 because we only care about the highest bit (of a 4-bit field)
	x64_emit_u8(0x48 | (cast(u8) rm >> 3) | ((cast(u8) reg >> 3) << 2));
}

internal void
x64_emit_rex_indexed(X64_Register reg, X64_Register rm, X64_Register index) {
	assert(cast(u8) reg   < 16); // reg   field of the ModRM byte; we only care about the high bit
	assert(cast(u8) rm    < 16); // rm    field of the ModRM byte; we only care about the high bit
	assert(cast(u8) index < 16); // index field of the SIB   byte; we only care about the high bit
	
	x64_emit_u8(0x48 | (cast(u8) rm >> 3) | ((cast(u8) index >> 3) << 1) | ((cast(u8) reg >> 3) << 2));
}

internal void
x64_emit_mod_rx_rm(u8 mod, u8 rx, u8 rm) {
	assert(mod <  4);
	assert(rx  < 16);
	assert(rm  < 16);
	
	// rm and rx are registers, but they can also be something else, that's why they're encoded as regular 'u8's.
	// They can be any register up to R15, which means they require 4 bits to be encoded, BUT only the lowest 3 bits
	// are emitted here. The high bit is part of the REX prefix.
	x64_emit_u8((rm & 7) | ((rx & 7) << 3) | (mod << 6));
}

// Example instruction:
//     add rax, rcx
// reg: Destination register, 'rax' in the example
// operand: Source register, 'rcx' in the example
internal void
x64_emit_direct_operand(X64_Register reg, X64_Register operand) {
	x64_emit_mod_rx_rm(cast(u8) X64_Mode_Direct, cast(u8) reg, cast(u8) operand);
}

// Example instruction:
//     add rax, [rcx]
// rx = rax
// operand = rcx
internal void
x64_emit_indirect_operand(X64_Register reg, X64_Register operand) {
	assert(cast(X64_Register)(cast(u8) operand & 7) != X64_Register_RSP); // In 32-bit addressing mode, a R/M of 100 indicates a SIB byte will follow
	assert(cast(X64_Register)(cast(u8) operand & 7) != X64_Register_RBP); // In 32-bit addressing mode, a R/M of 101 indicates a 32-bit displacement will follow
	
	x64_emit_mod_rx_rm(cast(u8) X64_Mode_Indirect, cast(u8) reg, cast(u8) operand);
}

// TODO: Check, this looks wrong
// Huh? If RBP is interpreted as "a displacement needs to follow the ModRM", why do we have a version that takes no displacement?
// Also, what is RIP?
// Example instruction:
//     add rax, [rbp]
// rx = rax
internal void
x64_emit_indirect_rip(u8 rx) {
	x64_emit_mod_rx_rm(cast(u8) X64_Mode_Indirect, rx, 0x5);
}

// TODO: Check, this looks wrong
// Emits a ModRM byte defining an instruction that uses a constant displacement as an indirect source.
// Example instruction:
//     add rax, [0x12345678]  ; dereference memory location 0x12345678
internal void
x64_emit_indirect_displacement(X64_Register reg, u32 displacement) {
#if 0
	x64_emit_mod_rx_rm(cast(u8) X64_Mode_Indirect_With_Multi_Byte_Displacement, cast(u8) reg, 0x5);
#else
	x64_emit_mod_rx_rm(cast(u8) X64_Mode_Indirect, cast(u8) reg, X64_Register_RSP);
	x64_emit_mod_rx_rm(cast(u8) X64_Scale_X1, X64_Register_RSP, X64_Register_RBP);
#endif
	x64_emit_u32(displacement);
}

// Emits a ModRM byte defining an instruction that uses a register + a constant displacement as an indirect source.
// Example instruction:
//     add rax, [rcx + 0x12]
// rx = rax
// operand = rcx
// displacement = 0x12
internal void
x64_emit_indirect_operand_with_byte_displacement(X64_Register reg, X64_Register operand, u8 displacement) {
	assert(cast(X64_Register)(cast(u8) operand & 7) != X64_Register_RSP); // In 32-bit addressing mode, a R/M of 100 indicates a SIB byte will follow
	
	x64_emit_mod_rx_rm(cast(u8) X64_Mode_Indirect_With_Byte_Displacement, cast(u8) reg, cast(u8) operand);
	x64_emit_u8(displacement);
}

// Emits a ModRM byte defining an instruction that uses a register + a constant displacement as an indirect source.
// Example instruction:
//     add rax, [rcx + 0x12345678]
// rx = rax
// operand = rcx
// displacement = 0x12345678
internal void
x64_emit_indirect_operand_with_multi_byte_displacement(X64_Register reg, X64_Register operand, u32 displacement) {
	assert(cast(X64_Register)(cast(u8) operand & 7) != X64_Register_RSP); // In 32-bit addressing mode, a R/M of 100 indicates a SIB byte will follow
	
	x64_emit_mod_rx_rm(cast(u8) X64_Mode_Indirect_With_Multi_Byte_Displacement, cast(u8) reg, cast(u8) operand);
	x64_emit_u32(displacement);
}

internal void
x64_emit_sib(X64_Register base, X64_Scale scale, X64_Register index) {
	x64_emit_mod_rx_rm(cast(u8) scale, cast(u8) index, cast(u8) base);
}

// Example instruction:
//     add rax, [rcx + 4*rdx]
// rx = rax
// operand = rcx
// index = rdx
// scale = X4
internal void
x64_emit_indexed_indirect_operand(X64_Register reg, X64_Register base, X64_Scale scale, X64_Register index) {
	x64_emit_mod_rx_rm(cast(u8) X64_Mode_Indirect, cast(u8) reg, cast(u8) X64_Register_RSP); // RSP indicates a following SIB byte
	x64_emit_sib(base, scale, index);
}

// Example instruction:
//     add rax, [rcx + 4*rdx + 0x12]
internal void
x64_emit_indexed_indirect_operand_with_byte_displacement(X64_Register reg, X64_Register base, X64_Scale scale, X64_Register index, u8 displacement) {
	x64_emit_mod_rx_rm(cast(u8) X64_Mode_Indirect_With_Byte_Displacement, cast(u8) reg, cast(u8) X64_Register_RSP); // RSP indicates a following SIB byte
	x64_emit_sib(base, scale, index);
	x64_emit_u8(displacement);
}

// Example instruction:
//     add rax, [rcx + 4*rdx + 0x12345678]
internal void
x64_emit_indexed_indirect_operand_with_multi_byte_displacement(X64_Register reg, X64_Register base, X64_Scale scale, X64_Register index, u32 displacement) {
	x64_emit_mod_rx_rm(cast(u8) X64_Mode_Indirect_With_Multi_Byte_Displacement, cast(u8) reg, cast(u8) X64_Register_RSP); // RSP indicates a following SIB byte
	x64_emit_sib(base, scale, index);
	x64_emit_u32(displacement);
}

////////////////////////////////
//~ High-level emitters

//- Register as destination

// r_r = register <- register
#define x64_emit_r_r(mnemonic, destination, source) \
x64_emit_rex(destination, source); \
x64_emit_##mnemonic##_r(); \
x64_emit_direct_operand(destination, source)

// r_d = register <- constant displacement
#define x64_emit_r_d(mnemonic, destination, source_displacement) \
x64_emit_rex(destination, 0); \
x64_emit_##mnemonic##_r(); \
x64_emit_indirect_displacement(destination, source_displacement)

// TODO: incomplete
#define x64_emit_r_ripd(mnemonic, destination, source_displacement) \
x64_emit_rex(destination, 0); \
x64_emit_##mnemonic##_r(); \
x64_emit_indirect_displaced_rip(destination, source_displacement)

// r_m = register <- memory
#define x64_emit_r_m(mnemonic, destination, source) \
x64_emit_rex(destination, source); \
x64_emit_##mnemonic##_r(); \
x64_emit_indirect_operand(destination, source) 

// r_md8 = register <- memory + u8 displacement
#define x64_emit_r_md8(mnemonic, destination, source, displacement) \
x64_emit_rex(destination, source); \
x64_emit_##mnemonic##_r(); \
x64_emit_indirect_operand_with_byte_displacement(destination, source, displacement)

// r_md32 = register <- memory + u32 displacement
#define x64_emit_r_md32(mnemonic, destination, source, displacement) \
x64_emit_rex(destination, source); \
x64_emit_##mnemonic##_r(); \
x64_emit_indirect_operand_with_multi_byte_displacement(destination, source, displacement)

// r_sib = register <- scale + index + base
#define x64_emit_r_sib(mnemonic, destination, source_base, source_scale, source_index) \
x64_emit_rex_indexed(destination, source_base, source_index); \
x64_emit_##mnemonic##_r(); \
x64_emit_indexed_indirect_operand(destination, source_base, source_scale, source_index)

// r_sibd8 = register <- scale + index + base + u8 displacement
#define x64_emit_r_sibd8(mnemonic, destination, source_base, source_scale, source_index, displacement) \
x64_emit_rex_indexed(destination, source_base, source_index); \
x64_emit_##mnemonic##_r(); \
x64_emit_indexed_indirect_operand_with_byte_displacement(destination, source_base, source_scale, source_index, displacement)

// r_sibd8 = register <- scale + index + base + u32 displacement
#define x64_emit_r_sibd32(mnemonic, destination, source_base, source_scale, source_index, displacement) \
x64_emit_rex_indexed(destination, source_base, source_index); \
x64_emit_##mnemonic##_r(); \
x64_emit_indexed_indirect_operand_with_multi_byte_displacement(destination, source_base, source_scale, source_index, displacement)

//- Memory as destination

// d_r = constant displacement <- register
#define x64_emit_d_r(mnemonic, destination_displacement, source) \
x64_emit_rex(source, 0); \
x64_emit_##mnemonic##_m(); \
x64_emit_indirect_displacement(source, destination_displacement)

// TODO: incomplete
#define x64_emit_ripd_r(mnemonic, destination_displacement, source) \
x64_emit_rex(source, 0); \
x64_emit_##mnemonic##_m(); \
x64_emit_indirect_displaced_rip(source, destination_displacement)

// m_r = memory <- register
#define x64_emit_m_r(mnemonic, destination, source) \
x64_emit_rex(source, destination); \
x64_emit_##mnemonic##_m(); \
x64_emit_indirect_operand(source, destination);

// md8_r = memory + u8 displacement <- register
#define x64_emit_md8_r(mnemonic, destination, displacement, source) \
x64_emit_rex(source, destination); \
x64_emit_##mnemonic##_m(); \
x64_emit_indirect_operand_with_byte_displacement(source, destination, displacement);

// md32_r = memory + u32 displacement <- register
#define x64_emit_md32_r(mnemonic, destination, displacement, source) \
x64_emit_rex(source, destination); \
x64_emit_##mnemonic##_m(); \
x64_emit_indirect_operand_with_multi_byte_displacement(source, destination, displacement);

// sib_r = scale + index + base <- register
#define x64_emit_sib_r(mnemonic, destination_base, destination_scale, destination_index, source) \
x64_emit_rex_indexed(source, destination_base, destination_index); \
x64_emit_##mnemonic##_m(); \
x64_emit_indexed_indirect_operand(source, destination_base, destination_scale, destination_index);

// sibd8_r = scale + index + base + u8 displacement <- register
#define x64_emit_sibu8_r(mnemonic, destination_base, destination_scale, destination_index, displacement, source) \
x64_emit_rex_indexed(source, destination_base, destination_index); \
x64_emit_##mnemonic##_m(); \
x64_emit_indexed_indirect_operand_with_byte_displacement(source, destination_base, destination_scale, destination_index, displacement);

// sibd32_r = scale + index + base + u32 displacement <- register
#define x64_emit_sibu32_r(mnemonic, destination_base, destination_scale, destination_index, displacement, source) \
x64_emit_rex_indexed(source, destination_base, destination_index); \
x64_emit_##mnemonic##_m(); \
x64_emit_indexed_indirect_operand_with_multi_byte_displacement(source, destination_base, destination_scale, destination_index, displacement);

//- Immediate as source

// r_i = register <- immediate
#define x64_emit_r_i(mnemonic, destination, immediate) \
x64_emit_rex(0, destination); \
x64_emit_##mnemonic##_i(); \
x64_emit_direct_operand(x64_extension_##mnemonic##_i, destination); \
x64_emit_u32(immediate)

// d_i = constant displacement <- immediate
#define x64_emit_d_i(mnemonic, destination_displacement, immediate) \
x64_emit_rex(0, 0); \
x64_emit_##mnemonic##_i(); \
x64_emit_indirect_displacement(x64_extension_##mnemonic##_i, destination_displacement); \
x64_emit_u32(immediate)

// TODO: incomplete
#define x64_emit_ripd_i(mnemonic, destination_displacement, immediate) \
x64_emit_rex(0, 0); \
x64_emit_##mnemonic##_i(); \
x64_emit_indirect_displaced_rip(x64_extension_##mnemonic##_i, destination_displacement); \
x64_emit_u32(immediate)

// m_i = memory <- immediate
#define x64_emit_m_i(mnemonic, destination, immediate) \
x64_emit_rex(0, destination); \
x64_emit_##mnemonic##_i(); \
x64_emit_indirect_operand(x64_extension_##mnemonic##_i, destination); \
x64_emit_u32(immediate)

// md8_i = memory + u8 displacement <- immediate
#define x64_emit_md8_i(mnemonic, destination, destination_displacement, immediate) \
x64_emit_rex(0, destination); \
x64_emit_##mnemonic##_i(); \
x64_emit_indirect_operand_with_byte_displacement(x64_extension_##mnemonic##_i, destination, destination_displacement); \
x64_emit_u32(immediate)

// md32_i = memory + u32 displacement <- immediate
#define x64_emit_md32_i(mnemonic, destination, destination_displacement, immediate) \
x64_emit_rex(0, destination); \
x64_emit_##mnemonic##_i(); \
x64_emit_indirect_operand_with_multi_byte_displacement(x64_extension_##mnemonic##_i, destination, destination_displacement); \
x64_emit_u32(immediate)

// sib_i = scale + index + base <- immediate
#define x64_emit_sib_i(mnemonic, destination_base, destination_scale, destination_index, immediate) \
x64_emit_rex_indexed(0, destination_base, destination_index); \
x64_emit_##mnemonic##_i(); \
x64_emit_indexed_indirect_operand(x64_extension_##mnemonic##_i, destination_base, destination_scale, destination_index); \
x64_emit_u32(immediate)

// sibu8_i = scale + index + base + u8 displacement <- immediate
#define x64_emit_sibu8_i(mnemonic, destination_base, destination_scale, destination_index, destination_displacement, immediate) \
x64_emit_rex_indexed(0, destination_base, destination_index); \
x64_emit_##mnemonic##_i(); \
x64_emit_indexed_indirect_operand_with_byte_displacement(x64_extension_##mnemonic##_i, destination_base, destination_scale, destination_index, destination_displacement); \
x64_emit_u32(immediate)

// sibu32_i = scale + index + base + u32 displacement <- immediate
#define x64_emit_sibu32_i(mnemonic, destination_base, destination_scale, destination_index, destination_displacement, immediate) \
x64_emit_rex_indexed(0, destination_base, destination_index); \
x64_emit_##mnemonic##_i(); \
x64_emit_indexed_indirect_operand_with_multi_byte_displacement(x64_extension_##mnemonic##_i, destination_base, destination_scale, destination_index, destination_displacement); \
x64_emit_u32(immediate)

////////////////////////////////
//~ Instruction-specific emitters

#if 1

#define X64_DEFINE_1R(mnemonic, opcode) \
internal void x64_emit_##mnemonic##_r(void) { \
x64_emit_u8(opcode); \
}

#define X64_DEFINE_1M(mnemonic, opcode) \
internal void x64_emit_##mnemonic##_m(void) { \
x64_emit_u8(opcode); \
}

#define X64_DEFINE_1I(mnemonic, opcode, extension) \
internal void x64_emit_##mnemonic##_i(void) { \
x64_emit_u8(opcode); \
} \
enum { x64_extension_##mnemonic##_i = extension };

#define X64_DEFINE_1X(mnemonic, opcode, extension) \
internal void x64_emit_##mnemonic##_x(void) { \
x64_emit_u8(opcode); \
} \
enum { x64_extension_##mnemonic##_x = extension };

X64_DEFINE_1R(add, 0x03)
X64_DEFINE_1M(add, 0x01)
X64_DEFINE_1I(add, 0x81, 0x00)

X64_DEFINE_1R(and, 0x23)
X64_DEFINE_1M(and, 0x21)
X64_DEFINE_1I(and, 0x81, 0x04)

X64_DEFINE_1X(mul, 0xf7, 0x04)

#else

// Emits an ADD instruction where the destination is a register
internal void x64_emit_add_r(void) {
	x64_emit_u8(0x03);
}

// Emits an ADD instruction where the destination is a memory location
internal void x64_emit_add_m(void) {
	x64_emit_u8(0x01);
}

// Emits an ADD instruction where the source is a constant
internal void x64_emit_add_i(void) {
	x64_emit_u8(0x81);
}

enum {
	x64_extension_add = 0,
};

#endif

////////////////////////////////
//~ Tests

internal void x64_test(void) {
	
	x64_emit_d_i(add, 0x12345678, 0xcafebabe); // Add immediate to constant absolute location
	// x64_emit_ripd_i(add, 0x12345678, 0xcafebabe); // Add immediate to constant relative location
	
	for (int i = 0; i < 16; i += 1) {
		X64_Register dest = cast(X64_Register)i;
		
		x64_emit_r_i(add, dest, 0xcafebabe); // Add immediate to register
		
		x64_emit_r_d(add, dest, 0xcafebabe); // Add constant absolute location to register
		// x64_emit_r_ripd(add, dest, 0xcafebabe); // Add constant relative location to register
		
		if (cast(X64_Register)(cast(u8) dest & 7) != X64_Register_RSP) {
			if (cast(X64_Register)(cast(u8) dest & 7) != X64_Register_RBP) {
				x64_emit_m_i(add, dest, 0xcafebabe); // Add immediate to memory
				x64_emit_sib_i(add, dest, X64_Scale_X4, X64_Register_R8, 0xcafebabe); // Add immediate to memory with SIB
			}
			
			x64_emit_md8_i(add, dest, 0x12, 0xcafebabe); // Add immediate to memory with u8 displacement
			x64_emit_md32_i(add, dest, 0x12345678, 0xcafebabe); // Add immediate to memory with u32 displacement
			x64_emit_sibu8_i(add, dest, X64_Scale_X4, X64_Register_R8, 0x12, 0xcafebabe); // Add immediate to memory with SIB and u8 displacement
			x64_emit_sibu32_i(add, dest, X64_Scale_X4, X64_Register_R8, 0x12345678, 0xcafebabe); // Add immediate to memory with SIB and u32 displacement
		}
		
		for (int j = 0; j < 16; j += 1) {
			X64_Register src = cast(X64_Register)j;
			
			
#if 0
			emit_rex(dest, src);
			emit_add();
			emit_direct_operand(dest, src);
#else
			x64_emit_r_r(add, dest, src);
#endif
			
#if 0
			if cast(X64_Register)(cast(u8) src & 7) != .RSP {
				if cast(X64_Register)(cast(u8) src & 7) != .RBP {
#if 0
					emit_rex(dest, src);
					emit_add();
					emit_indirect_operand(dest, src);
#else
					
#endif
				}
				
				emit_rex(dest, src);
				emit_add();
				emit_indirect_operand_with_byte_displacement(dest, src, 0x12);
				
				emit_rex(dest, src);
				emit_add();
				emit_indirect_operand_with_multi_byte_displacement(dest, src, 0x12345678);
			}
			
			if cast(X64_Register)(cast(u8) src & 7) != .RBP {
				emit_rex_indexed(dest, src, dest);
				emit_add();
				emit_indexed_indirect_operand(dest, src, .X4, dest);
			}
			
			emit_rex_indexed(dest, src, dest);
			emit_add();
			emit_indexed_indirect_operand_with_byte_displacement(dest, src, .X4, dest, 0x12);
			
			emit_rex_indexed(dest, src, dest);
			emit_add();
			emit_indexed_indirect_operand_with_multi_byte_displacement(dest, src, .X4, dest, 0x12345678);
#endif
		}
	}
	
	FILE *file = fopen("tests/out.bin", "wb+");
	if (file) {
		i64 emit_len = x64_emit_ptr - x64_emit_buf;
		fwrite(x64_emit_buf, sizeof(u8), emit_len, file);
		
		fclose(file);
	}
	
	return;
}

#endif
