#ifndef EL_MASM_C
#define EL_MASM_C

internal void masm_append_line(String line) {
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
		
		String temp[] = {make_string(indent_buf, indent_len), line};
		string_list_push(&masm_context.arena, &masm_context.lines, strings_concat(&masm_context.arena, temp, array_count(temp)));
	} else {
		string_list_push(&masm_context.arena, &masm_context.lines, string_clone(&masm_context.arena, line));
	}
	
	scratch_end(scratch);
}

internal String masm_register_from_bytecode_register(int bytecode_reg) {
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

internal void masm_emit_proc(Bcode_Builder *builder, Bcode_Proc *proc) {
	Scratch scratch = scratch_begin(0, 0);
	
	{
		assert(proc->name.len > 0, "Attempt to generate a procedure with no name");
		
		String line = push_stringf(scratch.arena, "%.*s proc", string_expand(proc->name));
		masm_append_line(line);
		
		// Remember the current name so we can write the 'endp' line
		masm_context.current_label = proc->name;
		
		masm_context.indent_level += 1;
	}
	
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
	
	for (Bcode_Block *bcode_block = proc->first_block; bcode_block != NULL; bcode_block = bcode_block->next) {
		for (int i = 0; i < bcode_block->instruction_count; i += 1) {
			arena_reset(scratch.arena);
			
			Instr *instr = &bcode_block->instructions[i];
			
			switch (instr->operation) {
				case BCODE_NULL: {
					String line = push_stringf(scratch.arena, "; Null instruction", instr->operation);
					masm_append_line(line);
				} break;
				
				case BCODE_SET:
				case BCODE_ADD:
				case BCODE_SUB:
				case BCODE_MUL: {
					
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
						case BCODE_SET: { mnemonic = string_from_lit("mov"); } break;
						case BCODE_ADD: { mnemonic = string_from_lit("add"); } break;
						case BCODE_SUB: { mnemonic = string_from_lit("sub"); } break;
						case BCODE_MUL: { mnemonic = string_from_lit("imul"); } break;
						default: break;
					}
					
					String line = push_stringf(scratch.arena, "%.*s %.*s, %.*s", string_expand(mnemonic),
											   string_expand(dest), string_expand(source));
					masm_append_line(line);
				} break;
				
				case BCODE_CALL: {
					
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
						
						for (int j = 0; j < instr->arg_reg_count; j += 1) { // TODO: j is unused. Why?
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
				
				case BCODE_SWAP: {
					assert(builder->registers_used < 14); // For now. TODO: Use memory if no more registers
					
					String source = masm_register_from_bytecode_register(instr->source);
					String dest   = masm_register_from_bytecode_register(instr->dest);
					String temp   = masm_register_from_bytecode_register(builder->registers_used);
					
					masm_append_line(push_stringf(&masm_context.arena, "mov %.*s, %.*s", string_expand(temp),
												  string_expand(source)));
					masm_append_line(push_stringf(&masm_context.arena, "mov %.*s, %.*s", string_expand(source),
												  string_expand(dest)));
					masm_append_line(push_stringf(&masm_context.arena, "mov %.*s, %.*s", string_expand(dest),
												  string_expand(temp)));
				} break;
				
				case BCODE_RETURN: {
					
					// Do not emit a ret instruction; instead just setup the registers so that return values
					// are in the correct places. The ret instruction is automatically added at the end.
					
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
					
					
				} break;
				
				default: {
					String line = push_stringf(scratch.arena, "; Unimplemented instruction '%d'", instr->operation);
					masm_append_line(line);
				} break;
			}
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
	
	scratch_end(scratch);
	return;
}

internal String masm_generate_source(Bcode_Builder *builder) {
	
	Scratch scratch = scratch_begin(0, 0);
	
	masm_append_line(string_from_lit("; Generated"));
	masm_append_line(string_from_lit("includelib msvcrt.lib"));
	masm_append_line(string_from_lit(".data"));
	
	masm_context.indent_level += 1;
	
	for (i64 i = 1; i < builder->global_var_count; i += 1) {
		Bcode_Var var = builder->global_vars[i];
		
		String line = push_stringf(scratch.arena, "%.*s qword 0h", string_expand(var.ident));
		masm_append_line(line);
	}
	
	masm_context.indent_level -= 1;
	
	masm_append_line(string_from_lit("\n"));
	masm_append_line(string_from_lit(".code\n")); // Not .text, aparently
	
	for (i64 proc_index = 1; proc_index < builder->proc_count; proc_index += 1)
		masm_emit_proc(builder, &builder->procs[proc_index]);
	
	masm_append_line(string_from_lit("end"));
	
	scratch_end(scratch);
	
	return string_from_list(&masm_context.arena, masm_context.lines,
							.sep = string_from_lit("\n"),
							.suf = string_from_lit("\n"));
}

#endif
