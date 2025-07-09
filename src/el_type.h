#ifndef EL_TYPE_H
#define EL_TYPE_H

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


global read_only String type_kind_names[] = {
	string_from_lit_const("TYPE_UNKNOWN"),
	string_from_lit_const("TYPE_VOID"),
	string_from_lit_const("TYPE_TYPE"),
	string_from_lit_const("TYPE_BOOLEAN"),
	string_from_lit_const("TYPE_INTEGER"),
	string_from_lit_const("TYPE_STRING"),
	string_from_lit_const("TYPE_STRUCT"),
	string_from_lit_const("TYPE_POINTER"),
	string_from_lit_const("TYPE_PROC"),
	string_from_lit_const("TYPE_COUNT"),
};

#endif
