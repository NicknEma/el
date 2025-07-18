
 global char lowercase_hex_digits[] = "0123456789abcdef";
global char uppercase_hex_digits[] = "0123456789ABCDEF";

WRITER_PROC(string_builder_writer) {
	String_Builder *dest_ = cast(String_Builder *) dest;
	
	for (; dest_->len < dest_->cap && *source && count > 0; dest_->len++, count--) {
		*dest_->data++ = *source++;
	}
	
	return dest_;
}

WRITER_PROC(libc_file_writer) {
	fwrite(source, sizeof(char), count, cast(FILE *) dest);
	return dest;
}

WRITER_PROC(print_counter) {
	usize *accumulator = cast(usize *) dest;
	*accumulator += count;
	return accumulator;
}

internal void *write_char(Writer_Proc writer, void *dest, char c) {
	return writer(dest, &c, 1);
}

internal void *write_chars(Writer_Proc writer, void *dest, char *buf, usize count) {
	return writer(dest, buf, count);
}

internal void *write_u64(Writer_Proc writer, void *dest, u64 n, int base, char *digits, char prefix) {
	assert(base != 0);
	
	char temp[32];
	int i = array_count(temp) - 1;
	
	do {
		char digit = digits[n % base];
		temp[i--] = digit;
		
		n /= base;
	} while (n != 0);
	
	if (prefix)
		temp[i--] = prefix;
	
	i++;
	
	void *new_dest = writer(dest, temp + i, array_count(temp) - i);
	return new_dest;
}

internal void *write_formatted_string(Writer_Proc writer, void *dest, char *format, va_list args) {
	char *at = format;
	while (*at != '\0') {
		if (*at == '%') {
			at++;
			
			uint integer_size  = 4;
			uint floating_size = 8;
			if (at[0] == '.') {
				at++;
				
				if (at[0] == '*') {
					at++;
					
					usize size = va_arg(args, usize);
					if (size == 1) integer_size = 1;
					else if (size == 2) integer_size = 2;
					else if (size == 4) floating_size = 4;
					else if (size == 8) integer_size = 8;
				} else if (at[0] == '8') {
					at++;
					
					integer_size = 1;
				} else if (at[0] == '1' && at[1] == '6') {
					at += 2;
					
					integer_size = 2;
				} else if (at[0] == '3' && at[1] == '2') {
					at += 2;
					
					floating_size = 4;
				} else if (at[0] == '6' && at[1] == '4') {
					at += 2;
					
					integer_size = 8;
				} else { allow_break(); }
			}
			
			switch (*at) {
				case 'i':
				case 'd':
				case 's': {
					i64 val = va_arg_to_s64(integer_size, &args);
					u64 unsigned_val = 0;
					char prefix = 0;
					
					if (val < 0) {
						prefix = '-';
						unsigned_val = (cast(u64) (-(val + 1))) + 1;
					} else {
						unsigned_val = cast(u64) val;
					}
					
					dest = write_u64(writer, dest, unsigned_val, 10, lowercase_hex_digits, prefix);
				} break;
				
				case 'u': {
					u64 val = va_arg_to_u64(integer_size, &args);
					dest = write_u64(writer, dest, val, 10, lowercase_hex_digits, 0);
				} break;
				
				case 'x': {
					dest = write_chars(writer, dest, string_lit_expand("0x"));
					
					u64 val = va_arg_to_u64(integer_size, &args);
					dest = write_u64(writer, dest, val, 16, lowercase_hex_digits, 0);
				} break;
				
				case 'X': {
					dest = write_chars(writer, dest, string_lit_expand("0x"));
					
					u64 val = va_arg_to_u64(integer_size, &args);
					dest = write_u64(writer, dest, val, 16, uppercase_hex_digits, 0);
				} break;
				
				case 'b': {
					bool val = cast(bool) va_arg(args, bool);
					
					if (val) dest = write_chars(writer, dest, string_lit_expand("true"));
					else dest = write_chars(writer, dest, string_lit_expand("false"));
				} break;
				
				case 'c': {
					char val = cast(char) va_arg(args, int);
					dest = write_char(writer, dest, val);
				} break;
				
				case 'z': {
					char *val = va_arg(args, char *);
					dest = write_chars(writer, dest, val, cast(usize) -1); // @Todo: USIZE_MAX
				} break;
				
				case 'S': {
					String val = va_arg(args, String);
					dest = write_chars(writer, dest, cast(char *) val.data, val.len);
				} break;
				
				case 'p': {
					void *val = va_arg(args, void *);
					dest = write_u64(writer, dest, *cast(u64 *) val, 16, lowercase_hex_digits, 0);
				} break;
				
				case '%': {
					dest = write_char(writer, dest, '%');
				} break;
				
				default: {
					panic("Unrecognized format specifier.");
				}
			}
			
			if (*at) at++;
		} else {
			dest = write_char(writer, dest, *at++);
		}
	}
	
	return dest;
}

internal String bsprint_va_list(SliceU8 buffer, char *format, va_list args) {
	String_Builder builder;
	string_builder_init(&builder, buffer);
	
	write_formatted_string(string_builder_writer, &builder, format, args);
	
	return string_from_builder(builder);
}

internal String bsprint(SliceU8 buffer, char *format, ...) {
	va_list args;
	va_start(args, format);
	String result = bsprint_va_list(buffer, format, args);
	va_end(args);
	
	return result;
}

internal void fprint_va_list(FILE *stream, char *format, va_list args) {
	write_formatted_string(libc_file_writer, stream, format, args);
}

internal void fprint(FILE *stream, char *format, ...) {
	va_list args;
	va_start(args, format);
	fprint_va_list(stream, format, args);
	va_end(args);
}

internal void print_va_list(char *format, va_list args) {
	write_formatted_string(libc_file_writer, stdout, format, args);
}

internal void print(char *format, ...) {
	va_list args;
	va_start(args, format);
	print_va_list(format, args);
	va_end(args);
}

internal void eprint_va_list(char *format, va_list args) {
	write_formatted_string(libc_file_writer, stderr, format, args);
}

internal void eprint(char *format, ...) {
	va_list args;
	va_start(args, format);
	eprint_va_list(format, args);
	va_end(args);
}

internal void dprint_va_list(char *format, va_list args) {
	write_formatted_string(debugger_writer, 0, format, args);
}

internal void dprint(char *format, ...) {
	va_list args;
	va_start(args, format);
	dprint_va_list(format, args);
	va_end(args);
}

internal usize nprint_va_list(char *format, va_list args) {
	usize result = 0;
	result = *cast(usize *) write_formatted_string(print_counter, &result, format, args);
	return result;
}

internal usize nprint(char *format, ...) {
	va_list args;
	va_start(args, format);
	usize result = nprint_va_list(format, args);
	va_end(args);
	
	return result;
}

internal String asprint_va_list(Arena *arena, char *format, va_list args) {
	i64   len = nprint_va_list(format, args);
	SliceU8 s = push_sliceu8(arena, len);
	return bsprint_va_list(s, format, args);
}

internal String asprint(Arena *arena, char *format, ...) {
	va_list args;
	va_start(args, format);
	String result = asprint_va_list(arena, format, args);
	va_end(args);
	
	return result;
}

////////////////////////////////
//~ Compiler-specific printing utilities

internal void inc_indent(void) { indent_level += 1; }

internal void dec_indent(void) { indent_level = max(indent_level - 1, 0); }

internal void fprint_indent(FILE *stream) {
	for (int i = 0; i < indent_level; i += 1) fputs(indent_chars, stream);
}

internal void print_indent(void) { fprint_indent(stdout); }
