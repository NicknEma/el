#ifndef EL_BASE_H
#define EL_BASE_H

////////////////////////////////
//~ System Headers

#if OS_WINDOWS
# define WIN32_LEAN_AND_MEAN
# include <windows.h>
typedef SSIZE_T ssize_t;
#elif OS_LINUX
# include <sys/mman.h>
# include <sys/ioctl.h>
# include <sys/unistd.h>
# if HAS_INCLUDE(<libexplain/pathconf.h>)
#  include <libexplain/pathconf.h>
# endif
#else
# error Platform not supported.
#endif

////////////////////////////////
//~ Standard Headers

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdarg.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <time.h>

#ifdef min
# undef min
#endif

#ifdef max
# undef max
#endif

////////////////////////////////
//~ Core

//- Keywords

#define cast(t) (t)

#define internal static
#define global   static

#if ASAN_ENABLED && COMPILER_MSVC
# define no_asan __declspec(no_sanitize_address)
#elif ASAN_ENABLED && (COMPILER_CLANG || COMPILER_GCC)
# define no_asan __attribute__((no_sanitize("address")))
#endif
#if !defined(no_asan)
# define no_asan
#endif

#if COMPILER_MSVC
# define per_thread __declspec(thread)
#elif COMPILER_CLANG
# define per_thread __thread
#elif COMPILER_GCC
# define per_thread __thread
#endif

#if OS_WINDOWS
# pragma section(".roglob", read)
# define read_only no_asan __declspec(allocate(".roglob"))
#else
# define read_only
#endif

#define alignof(t) _Alignof(t)

//- Integer/pointer/array/type manipulations

#define array_count(a) (i64)(sizeof(a)/sizeof((a)[0]))

#define bytes(n)     (   1ULL * n)
#define kilobytes(n) (1024ULL * bytes(n))
#define megabytes(n) (1024ULL * kilobytes(n))
#define gigabytes(n) (1024ULL * megabytes(n))

//- Clamps, mins, maxes

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))
#define clamp_top(a, b) min(a, b)
#define clamp_bot(a, b) max(a, b)

#define clamp(a, x, b) clamp_bot(a, clamp_top(x, b))

//- Macro macros

#define _stringify(x) #x
#define  stringify(x) _stringify(x)

//- Assertions

#define implies(p, q) (!(p) || (q))

#define allow_break() do{int _x=0;(void)_x;}while(0)

internal void default_assert_handler(const char *base, const char *file, const char *line, const char *message);
global void (*assert_handler)(const char *base, const char *file, const char *line, const char *message) = default_assert_handler;

#ifdef NDEBUG
#define assert(e, ...)     do{}while(0)
#define panic(...)         do{}while(0)
#define unimplemented(...) do{}while(0)
#else
#define assert(e, ...)     do{if(!(e)) assert_handler("Assertion failed: " #e, __FILE__, stringify(__LINE__), #__VA_ARGS__"");}while(0)
#define panic(...)         do{         assert_handler("Runtime panic",         __FILE__, stringify(__LINE__), #__VA_ARGS__"");}while(0)
#define unimplemented(...) do{         assert_handler("Unimplemented",         __FILE__, stringify(__LINE__), #__VA_ARGS__"");}while(0)
#endif

#if OS_WINDOWS
#define force_break() __debugbreak()
#else
#define force_break() (*(volatile int *)0=0)
#endif

//- Linked list helpers

#define check_null(p) ((p)==0)
#define set_null(p) ((p)=0)

#define stack_push_n(f,n,next) (void)((n)->next=(f),(f)=(n))
#define stack_pop_nz(f,next,zchk) (void)(zchk(f)?0:((f)=(f)->next))

#define queue_push_nz(f,l,n,next,zchk,zset) (void)(zchk(f)?\
(((f)=(l)=(n)), zset((n)->next)):\
((l)->next=(n),(l)=(n),zset((n)->next)))
#define queue_push_front_nz(f,l,n,next,zchk,zset) (void)(zchk(f) ? (((f) = (l) = (n)), zset((n)->next)) :\
((n)->next = (f)), ((f) = (n)))
#define queue_pop_nz(f,l,next,zset) (void)((f)==(l)?\
(zset(f),zset(l)):\
((f)=(f)->next))

#define dll_insert_npz(f,l,p,n,next,prev,zchk,zset) \
(void)(zchk(f) ? (((f) = (l) = (n)), zset((n)->next), zset((n)->prev)) :\
zchk(p) ? (zset((n)->prev), (n)->next = (f), (zchk(f) ? (0) : ((f)->prev = (n))), (f) = (n)) :\
((zchk((p)->next) ? (0) : (((p)->next->prev) = (n))), (n)->next = (p)->next, (n)->prev = (p), (p)->next = (n),\
((p) == (l) ? (l) = (n) : (0))))
#define dll_push_back_npz(first,last,elem,next_ident,prev_ident,zero_check,zero_set) dll_insert_npz(first,last,last,elem,next_ident,prev_ident,zero_check,zero_set)
#define dll_push_front_npz(first,last,elem,next_ident,prev_ident,zero_check,zero_set) dll_push_back_npz(last,first,elem,prev_ident,next_ident,zero_check,zero_set)
#define dll_remove_npz(f,l,n,next,prev,zchk,zset) (void)(((f)==(n))?\
((f)=(f)->next, (zchk(f) ? (zset(l)) : zset((f)->prev))):\
((l)==(n))?\
((l)=(l)->prev, (zchk(l) ? (zset(f)) : zset((l)->next))):\
((zchk((n)->next) ? (0) : ((n)->next->prev=(n)->prev)),\
(zchk((n)->prev) ? (0) : ((n)->prev->next=(n)->next))))

#define stack_push(f,n)           stack_push_n(f,n,next)
#define stack_pop(f)              stack_pop_nz(f,next,check_null)

#define queue_push(f,l,n)         queue_push_nz(f,l,n,next,check_null,set_null)
#define queue_push_front(f,l,n)   queue_push_front_nz(f,l,n,next,check_null,set_null)
#define queue_pop(f,l)            queue_pop_nz(f,l,next,set_null)

#define dll_push_back(f,l,n)      dll_push_back_npz(f,l,n,next,prev,check_null,set_null)
#define dll_push_front(f,l,n)     dll_push_back_npz(l,f,n,prev,next,check_null,set_null)
#define dll_insert(f,l,p,n)       dll_insert_npz(f,l,p,n,next,prev,check_null,set_null)
#define dll_remove(f,l,n)         dll_remove_npz(f,l,n,next,prev,check_null,set_null)

//- Basic types

typedef  uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef   int8_t  i8;
typedef  int16_t i16;
typedef  int32_t i32;
typedef  int64_t i64;

typedef  size_t usize;
typedef ssize_t ssize;
typedef unsigned int uint;

typedef float  f32;
typedef double f64;

#define  U8_MAX 0xFFU
#define U16_MAX 0xFFFFU
#define U32_MAX 0xFFFFFFFFU
#define U64_MAX 0xFFFFFFFFFFFFFFFFULL

#define  I8_MAX 0x7F
#define I16_MAX 0x7FFF
#define I32_MAX 0x7FFFFFFF
#define I64_MAX 0x7FFFFFFFFFFFFFFFLL

#define  I8_MIN 0x80
#define I16_MIN 0x8000
#define I32_MIN 0x80000000
#define I64_MIN 0x8000000000000000LL

//- Composite types

typedef struct Range1DI32 Range1DI32; struct Range1DI32 { i32 start; i32 end; };

typedef struct Text_Point Text_Point; struct Text_Point { i32 line; i32 column; };
typedef struct Text_Range Text_Range; struct Text_Range { Text_Point start; Text_Point end; };

internal Range1DI32 make_range1di32(i32 start, i32 end);
internal Range1DI32 range1di32_merge(Range1DI32 a, Range1DI32 b);

//- String and slice types

typedef struct SliceU8 SliceU8;
struct SliceU8 {
	i64  len;
	u8  *data;
};

typedef struct String String;
struct String {
	i64  len;
	u8  *data;
};

//- Integer math

internal bool is_power_of_two(u64 i);
internal u64  align_forward(u64 ptr, u64 alignment);
internal u64  round_up_to_multiple_of_u64(u64 n, u64 r);
internal i64  round_up_to_multiple_of_i64(i64 n, i64 r);
internal int  i64_digit_count(i64 n);
internal int  bit_len_u64(u64 x);

////////////////////////////////
//~ Memory

//- Memory procedures

internal void *mem_reserve(u64 size);
internal void *mem_commit(void *ptr, u64 size);
internal void *mem_reserve_and_commit(u64 size);
internal bool  mem_decommit(void *ptr, u64 size);
internal bool  mem_release(void *ptr, u64 size);

internal void  mem_failure_handler(void *ptr, u64 size);

////////////////////////////////
//~ Arena

//- Arena constants

#if !defined(ARENA_COMMIT_GRANULARITY)
#define ARENA_COMMIT_GRANULARITY kilobytes(4)
#endif

#if !defined(ARENA_DECOMMIT_THRESHOLD)
#define ARENA_DECOMMIT_THRESHOLD megabytes(64)
#endif

#if !defined(DEFAULT_ARENA_RESERVE_SIZE)
#define DEFAULT_ARENA_RESERVE_SIZE gigabytes(1)
#endif

//- Arena types

typedef struct Arena Arena;
struct Arena {
	u8  *ptr;
	u64  pos;
	u64  cap;
	u64  peak;
	u64  commit_pos;
};

typedef struct Arena_Restore_Point Arena_Restore_Point;
struct Arena_Restore_Point {
	Arena *arena;
	u64    pos;
};

typedef struct Arena_Init_Params Arena_Init_Params;
struct Arena_Init_Params {
	u64 reserve_size;
};

//- Arena procedures

internal void _arena_init(Arena *arena, Arena_Init_Params params);
#define arena_init(arena, ...) _arena_init(arena, (Arena_Init_Params){ .reserve_size = DEFAULT_ARENA_RESERVE_SIZE, __VA_ARGS__ })
internal bool arena_fini(Arena *arena);
internal void arena_reset(Arena *arena);

internal u64  arena_cap(Arena arena);
internal u64  arena_pos(Arena arena);
internal u64  arena_space(Arena arena);
internal bool arena_initted(Arena arena);

internal void *push_nozero_aligned(Arena *arena, u64 size, u64 alignment);
internal void *push_zero_aligned(Arena *arena, u64 size, u64 alignment);

internal void pop_to(Arena *arena, u64 pos);
internal void pop_amount(Arena *arena, u64 amount);

internal Arena_Restore_Point arena_begin_temp_region(Arena *arena);
internal void arena_end_temp_region(Arena_Restore_Point point);

////////////////////////////////
//~ Scratch memory

//- Scratch memory constants

#if !defined(SCRATCH_ARENA_COUNT)
#define SCRATCH_ARENA_COUNT 2
#endif

#if !defined(SCRATCH_ARENA_RESERVE_SIZE)
#define SCRATCH_ARENA_RESERVE_SIZE gigabytes(8)
#endif

//- Scratch memory types

typedef Arena_Restore_Point Scratch;

//- Scratch memory variables

#if SCRATCH_ARENA_COUNT > 0
per_thread Arena scratch_arenas[SCRATCH_ARENA_COUNT];
#endif

//- Scratch memory functions

internal Scratch scratch_begin(Arena **conflicts, i64 conflict_count);
internal void    scratch_end(Scratch scratch);

////////////////////////////////
//~ Heap

//- Generic heap interface

typedef enum Heap_Mode {
	Heap_Mode_ALLOC,
	Heap_Mode_FREE,
	Heap_Mode_FREE_ALL,
	Heap_Mode_QUERY_FEATURES,
} Heap_Mode;

typedef enum Heap_Features {
	Heap_Feature_FREE_ALL = (1<<0),
} Heap_Features;

typedef void *(*Heap_Proc)(void *old_ptr, u64 old_size, u64 size, Heap_Mode mode);

typedef struct Heap Heap;
struct Heap {
	Heap_Proc proc;
	void *data;
};

internal void *heap_alloc(Heap heap, u64 size);
internal void  heap_free(Heap heap, void *ptr, u64 size);
internal void  heap_free_all(Heap heap);
internal Heap_Features heap_query_features(Heap heap);

//- Default C heap

internal void *libc_heap_proc(void *old_ptr, u64 old_size, u64 size, Heap_Mode mode);

global Heap libc_heap = {
	.proc = libc_heap_proc,
};

////////////////////////////////
//~ Strings and slices

//- Types

typedef struct Strings_Concat_Params Strings_Concat_Params;
struct Strings_Concat_Params {
	String pre;
	String sep;
	String suf;
};

//- Slice functions

internal SliceU8 make_sliceu8(u8 *data, i64 len);
internal SliceU8 push_sliceu8(Arena *arena, i64 len);
internal SliceU8 sliceu8_from_string(String s);
internal SliceU8 sliceu8_clone(Arena *arena, SliceU8 s);

//- String functions

#define string_lit_expand(s)     s, (sizeof(s)-1)
#define string_expand(s)         cast(int)(s).len, (s).data

#define string_from_lit(s)       make_string(cast(u8 *)s, sizeof(s)-1)
#define string_from_cstring(s)   make_string(cast(u8 *)s, strlen(s))
#define string_from_lit_const(s)       {sizeof(s)-1, cast(u8 *)s}

internal String make_string(u8 *data, i64 len);
internal String push_string(Arena *arena, i64 len);
internal String push_stringf(Arena *arena, char *fmt, ...);
internal String push_stringf_va_list(Arena *arena, char *fmt, va_list args);
internal String string_from_sliceu8(SliceU8 s);
internal String string_clone(Arena *arena, String s);
#define strings_concat(arena, strings, string_count, ...) \
strings_concat_(arena, strings, string_count, (Strings_Concat_Params){\
.pre = string_from_lit_const(""),\
.sep = string_from_lit_const(""),\
.suf = string_from_lit_const(""),\
__VA_ARGS__\
})
internal String strings_concat_(Arena *arena, String *strings, i64 string_count, Strings_Concat_Params params);
internal char *cstring_from_string(Arena *arena, String s);

internal bool string_starts_with(String a, String b);
internal bool string_ends_with(String a, String b);
internal bool string_equals(String a, String b);
internal bool string_equals_case_insensitive(String a, String b);

internal i64 string_find_first(String s, u8 c);
internal i64 string_count_occurrences(String s, u8 c);
internal i64 string_contains(String s, u8 c);

internal String string_skip(String s, i64 amount);
internal String string_chop(String s, i64 amount);
internal String string_stop(String s, i64 index);
internal String string_skip_chop_whitespace(String s);
internal String string_chop_past_last_slash(String s);
internal String string_slice(String s, i64 start, i64 end);

internal String push_rand_string(Arena *arena, i64 len, String filter);

////////////////////////////////
//~ String List

typedef Strings_Concat_Params String_List_Join_Params;

//- String List types

typedef struct String_Node String_Node;
struct String_Node {
	String       str;
	String_Node *next;
	String_Node *prev;
};

typedef struct String_List String_List;
struct String_List {
	i64          total_len;
	i64          node_count;
	String_Node *first;
	String_Node *last;
};

//- String List functions

internal void string_list_push_first(Arena *arena, String_List *list, String s);
internal void string_list_push_last(Arena *arena, String_List *list, String s);
internal void string_list_pushf_first(Arena *arena, String_List *list, char *fmt, ...);
internal void string_list_pushf_last(Arena *arena, String_List *list, char *fmt, ...);
internal void string_list_pushf_first_va_list(Arena *arena, String_List *list, char *fmt, va_list args);
internal void string_list_pushf_last_va_list(Arena *arena, String_List *list, char *fmt, va_list args);

#define string_list_push(arena, list, s) string_list_push_last(arena, list, s)
#define string_list_pushf(arena, list, fmt, ...) string_list_pushf_last(arena, list, fmt, __VA_ARGS__)
#define string_list_pushf_va_list(arena, list, fmt, args) string_list_pushf_last_va_list(arena, list, fmt, args)

#define string_list_join(arena, list, ...) \
string_list_join_(arena, list, (String_List_Join_Params){\
.pre = string_from_lit_const(""),\
.sep = string_from_lit_const(""),\
.suf = string_from_lit_const(""),\
__VA_ARGS__\
})
internal String string_list_join_(Arena *arena, String_List list, String_List_Join_Params params);

#define string_from_list(arena, list, ...) string_list_join(arena, list, __VA_ARGS__)

////////////////////////////////
//~ String Builder

//- String builder types

typedef struct String_Builder String_Builder;
struct String_Builder {
	u8  *data;
	i64  len;
	i64  cap;
};

//- String builder functions

internal void string_builder_init(String_Builder *builder, SliceU8 backing);
internal i64  string_builder_append(String_Builder *builder, String s);

internal String string_from_builder(String_Builder builder);

////////////////////////////////
//~ Variadic functions helpers

internal u64 va_arg_to_u64(int length, va_list *args);
internal i64 va_arg_to_i64(int length, va_list *args);
internal f64 va_arg_to_f64(int length, va_list *args);

////////////////////////////////
//~ File IO

typedef struct String_And_Bool String_And_Bool;
struct String_And_Bool { String str; bool ok; };

internal size_t fsize(FILE *fp);
internal String_And_Bool load_file_string(Arena *arena, char *name);

#endif
