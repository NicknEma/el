#ifndef EL_TYPE_H
#define EL_TYPE_H

// Examples of how types are represented:
//
// int
//  (1) [ INT,8 ]
//
// ^int
//  (2) [ POINTER,8 ; INT,8 ]
//
// [5]int
//  (2) [ ARRAY,8*5,5 ; INT,8 ]
//
// [2][2]int
//  (3) [ ARRAY,(8*2)*2,2 ; ARRAY,8*2,2 ; INT,8 ]
//
// struct { ^int; string }
//  (4) [ STRUCT,8+16,2 ; POINTER,8 ; INT,8 ; STRING,16 ]
//
// proc()
//  (1) [ PROC,0 ]
//
// proc(string) -> int
//  (3) [ PROC,8,1,1 ; STRING,16 ; INT,8 ]
//
// distinct int
//  (2) [ DISTINCT,8 ; INT,8 ]

typedef enum Type_Kind {
	TYPE_UNKNOWN = 0,
	TYPE_VOID,
	TYPE_TYPE,
	TYPE_BOOLEAN,
	TYPE_INTEGER,
	TYPE_STRING,
	TYPE_STRUCT,
	TYPE_POINTER,
	TYPE_PROC,
	TYPE_COUNT,
} Type_Kind;

typedef struct Type Type;
struct Type {
	Type_Kind kind;
	int   min_size;
	
	union {
		Type *pointed; // For pointers
		struct { Type *elem; int elem_count; }; // For arrays
		struct { Type **members; int member_count; }; // For structs
		struct { Type **params; Type **retvals; int param_count, retval_count; }; // For procedures
	};
	
	String name;
};

typedef struct Type_Array Type_Array;
struct Type_Array {
	Type **data;
	i64    count;
};

#endif
