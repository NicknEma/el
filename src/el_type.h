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

#define INT_SUBTYPE_NAMES  \
X(INT_SUBTYPE_UNTYPED) \
X(INT_SUBTYPE_UINT) \
X(INT_SUBTYPE_U8)   \
X(INT_SUBTYPE_U16)  \
X(INT_SUBTYPE_U32)  \
X(INT_SUBTYPE_U64)  \
X(INT_SUBTYPE_INT)  \
X(INT_SUBTYPE_I8)   \
X(INT_SUBTYPE_I16)  \
X(INT_SUBTYPE_I32)  \
X(INT_SUBTYPE_I64)  \

typedef enum Int_Subtype {
#define X(name) name,
	INT_SUBTYPE_NAMES
#undef  X
} Int_Subtype;

global read_only String int_subtype_names[] = {
#define X(name) string_from_lit_const(#name),
	INT_SUBTYPE_NAMES
#undef  X
};

#define STRING_SUBTYPE_NAMES  \
X(STRING_SUBTYPE_UNTYPED) \
X(STRING_SUBTYPE_CSTRING) \
X(STRING_SUBTYPE_STRING)

typedef enum String_Subtype {
#define X(name) name,
	STRING_SUBTYPE_NAMES
#undef  X
} String_Subtype;

global read_only String string_subtype_names[] = {
#define X(name) string_from_lit_const(#name),
	STRING_SUBTYPE_NAMES
#undef  X
};

typedef i64 Type_Id;
enum {
	Type_Id_VOID,
	
	Type_Id_UNTYPED_INT,
	Type_Id_UINT,
	Type_Id_INT,
	
	Type_Id_UNTYPED_STRING,
	
	Type_Id_UNTYPED_BOOL,
	
	// User defined types...
};

typedef struct Type Type;
struct Type {
	Type_Kind kind;
	int   min_size;
	
	union {
		Int_Subtype int_subtype; // For integers
		String_Subtype string_subtype; // For strings
		Type_Id pointed; // For pointers
		struct { Type_Id elem; int elem_count; }; // For arrays
		struct { Type_Id *members; int member_count; }; // For structs
		struct { Type_Id *params; Type_Id *retvals; int param_count, retval_count; }; // For procedures
	};
	
	int    size; // In bytes
	String name;
};

typedef struct Type_Array Type_Array;
struct Type_Array {
	Type_Id *data;
	i64      count;
};


////////////////////////////////
//~ Type table --- Should probabily be a hash table

typedef struct Type_Table Type_Table;
struct Type_Table {
	Heap  heap;
	
	Type *types;
	i64   type_count;
	i64   type_capacity;
};

global Type_Table the_type_table;

#endif
