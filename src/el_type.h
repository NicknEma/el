#ifndef EL_TYPE_H
#define EL_TYPE_H

#define TYPE_KIND_NAMES \
X(TYPE_UNKNOWN) \
X(TYPE_VOID) \
X(TYPE_TYPE) \
X(TYPE_BOOLEAN) \
X(TYPE_INTEGER) \
X(TYPE_STRING) \
X(TYPE_STRUCT) \
X(TYPE_POINTER) \
X(TYPE_PROC) \
X(TYPE_COUNT)

typedef enum Type_Kind {
#define X(name) name,
	TYPE_KIND_NAMES
#undef  X
} Type_Kind;

global read_only String type_kind_names[] = {
#define X(name) string_from_lit_const(#name),
	TYPE_KIND_NAMES
#undef  X
};

#define SIGNEDNESS_NAMES \
X(SIGNEDNESS_NONE) \
X(SIGNEDNESS_SIGNED) \
X(SIGNEDNESS_UNSIGNED)

typedef enum Type_Signedness {
#define X(name) name,
	SIGNEDNESS_NAMES
#undef  X
} Type_Signedness;

global read_only String type_signedness_names[] = {
#define X(name) string_from_lit_const(#name),
	SIGNEDNESS_NAMES
#undef  X
};

typedef struct Type Type;
struct Type {
	Type_Kind kind;
	int   min_size;
	
	union {
		Type_Signedness signedness; // For integers
		Type *pointed; // For pointers
		struct { Type *elem; int elem_count; }; // For arrays
		struct { Type **members; int member_count; }; // For structs
		struct { Type **params; Type **retvals; int param_count, retval_count; }; // For procedures
	};
	
	int    size; // In bytes
	String name;
};

typedef struct Type_Array Type_Array;
struct Type_Array {
	Type **data;
	i64    count;
};


////////////////////////////////
//~ Type table --- Should probabily be a hash table

typedef struct Type_Table_Node Type_Table_Node;
struct Type_Table_Node {
	Type_Table_Node *next;
	Type *type;
};

typedef struct Type_Table Type_Table;
struct Type_Table {
	Type_Table_Node *first, *last;
};

#endif
