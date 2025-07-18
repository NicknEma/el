#ifndef EL_BASE_WINDOWS_C
#define EL_BASE_WINDOWS_C

////////////////////////////////
//~ Memory

internal void *mem_reserve(u64 size) {
	void *result = NULL;
	
	// VirtualAlloc fails with ERROR_INVALID_PARAMETER if we try to reserve 0 bytes.
	// Instead, we allow an empty allocation by simply returning NULL.
	if (size > 0) {
		// No need to align the size to a page boundary, Windows will do it for us.
		result = VirtualAlloc(NULL, size, MEM_RESERVE, PAGE_NOACCESS);
		if (!result) {
			int last_error = GetLastError();
			assert(last_error != ERROR_INVALID_PARAMETER && // If any of these is returned it is our fault,
				   last_error != ERROR_INVALID_ADDRESS   && // not the caller's
				   last_error != ERROR_NOT_SUPPORTED);
		}
	}
	
	mem_failure_handler(result, size);
	return result;
}

internal void *mem_commit(void *ptr, u64 size) {
	// No need to align the size to a page boundary, Windows will do it for us.
	void *result = VirtualAlloc(ptr, size, MEM_COMMIT, PAGE_READWRITE);
	if (!result) {
		int last_error = GetLastError();
		assert(last_error != ERROR_INVALID_PARAMETER);
	}
	
	mem_failure_handler(result, size);
	return result;
}

internal void *mem_reserve_and_commit(u64 size) {
	void *result = NULL;
	
	// VirtualAlloc fails with ERROR_INVALID_PARAMETER if we try to allocate 0 bytes.
	// Instead, we allow an empty allocation by simply returning NULL.
	if (size > 0) {
		// No need to align the size to a page boundary, Windows will do it for us.
		result = VirtualAlloc(NULL, size, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);
		if (!result) {
			int last_error = GetLastError();
			assert(last_error != ERROR_INVALID_PARAMETER && // If any of these is returned it is our fault,
				   last_error != ERROR_INVALID_ADDRESS   && // not the caller's
				   last_error != ERROR_NOT_SUPPORTED);
		}
	}
	
	mem_failure_handler(result, size);
	return result;
}

internal bool mem_decommit(void *ptr, u64 size) {
	return VirtualFree(ptr, size, MEM_DECOMMIT);
}

internal bool mem_release(void *ptr, u64 size) {
	(void)size; // Not needed on Windows
	return VirtualFree(ptr, 0, MEM_RELEASE);
}

#endif
