#ifndef EL_OS_LINUX_C
#define EL_OS_LINUX_C

////////////////////////////////
//~ Memory

internal void *mem_reserve(u64 size) {
	void *result = NULL;
	
	if (size > 0) {
		result = mmap(0, size, 0, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
		if (!result) {
			allow_break(); // TODO: assert() that the error wasn't because of invalid params
		}
	}
	
	mem_failure_handler(result, size);
	return result;
}

internal void *mem_commit(void *ptr, u64 size) {
	int r = mprotect(ptr, size, PROT_READ|PROT_WRITE);
	void *result = r != -1 ? ptr : NULL;
	if (!result) {
		allow_break();
	}
	
	mem_failure_handler(result, size);
	return result;
}

internal void *mem_reserve_and_commit(u64 size) {
	return mem_commit(mem_reserve(size), size);
}

internal bool mem_decommit(void *ptr, u64 size) {
	return (mprotect(ptr, size, 0) != -1 &&
			madvise(ptr, size, MADV_FREE) != -1);
}

internal bool mem_release(void *ptr, u64 size) {
	return munmap(ptr, size) != -1;
}

#endif
