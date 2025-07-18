#ifndef EL_OS_C
#define EL_OS_C

#if OS_WINDOWS
#include "el_os_windows.c"
#else
#include "el_os_linux.c"
#endif

internal String_And_Bool load_file_string(Arena *arena, char *name) {
	String_And_Bool result = {0};
	
	FILE *file = fopen(name, "rb");
	if (file) {
		size_t file_size = fsize(file);
		u8    *file_data = push_nozero(arena, file_size);
		
		if (fread(file_data, sizeof(u8), file_size, file) == file_size) {
			result.str = make_string(file_data, file_size);
			result.ok  = true;
		} else {
			perror("fread");
		}
		
		fclose(file);
	} else {
		perror("fopen");
	}
	
	return result;
}

#endif
