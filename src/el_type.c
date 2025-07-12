#ifndef EL_TYPE_C
#define EL_TYPE_C

internal bool type_is_atomic(Type type) {
	return type.kind == TYPE_VOID || type.kind == TYPE_BOOLEAN ||
		type.kind == TYPE_INTEGER || type.kind == TYPE_STRING;
	
	// TODO: Aliases or distinct
}

internal bool type_is_builtin(Type type) {
	return type_is_atomic(type); // TODO: check for u8, u16, u32 etc...
}

////////////////////////////////
//~ Type array

internal Type_Array push_type_array(Arena *arena, i64 count) {
	Type_Array a = {
		.data  = push_array(arena, Type_Id, count),
		.count = count,
	};
	
	return a;
}

////////////////////////////////
//~ Type table

internal Type *type_from_id(Type_Id id) {
	assert(id < the_type_table.type_count);
	return &the_type_table.types[id];
}

internal Type *type_from_name(String name) {
	Type *result = NULL;
	for (i64 type_index = 0; type_index < the_type_table.type_count; type_index += 1) {
		if (string_equals(name, the_type_table.types[type_index].name)) {
			result = &the_type_table.types[type_index];
			break;
		}
	}
	return result;
}

internal Type_Id type_id_from_name(String name) {
	Type_Id id = -1;
	for (i64 type_index = 0; type_index < the_type_table.type_count; type_index += 1) {
		if (string_equals(name, the_type_table.types[type_index].name)) {
			id = type_index;
			break;
		}
	}
	return id;
}

internal Type make_void_type(String name) {
	Type type = {.kind = TYPE_VOID, .name = name};
	return type;
}

internal Type make_int_type(Int_Subtype subtype, String name) {
	Type type = {0};
	
	type.kind        = TYPE_INTEGER;
	type.int_subtype = subtype;
	type.name        = name;
	
	return type;
}

internal Type make_proc_defn_type(Typechecker *checker, Ast_Declaration *first_param, Ast_Statement *body) {
	Type type = {0};
	
	type.kind = TYPE_PROC;
	
#if 0 // No params for now
	int param_count = 0;
	for (Ast_Declaration *param = first_param; !check_nil_declaration(param); param = param->next) {
		param_count += param->entity_count;
	}
	
	type->param_count = param_count;
	type->params = push_array(checker->arena, Type, param_count);
	
	int param_index = 0;
	for (Ast_Declaration *param = first_param; !check_nil_declaration(param); param = param->next) {
		param_index += 1;
	}
#endif
	
	type.name = string_from_lit("(anonymous)");
	
	return type;
}

internal Type_Id define_type(Type type) {
	if (the_type_table.heap.proc == 0)
		the_type_table.heap = libc_heap;
	
	if (the_type_table.types == NULL) {
		the_type_table.type_capacity = 16;
		the_type_table.types = heap_alloc(the_type_table.heap, the_type_table.type_capacity);
	}
	
	if (the_type_table.type_count >= the_type_table.type_capacity) {
		i64   new_type_capacity = the_type_table.type_capacity * 2 + 8;
		Type *new_types = heap_alloc(the_type_table.heap, new_type_capacity);
		
		memcpy(new_types, the_type_table.types, the_type_table.type_count * sizeof(Type));
		heap_free(the_type_table.heap, the_type_table.types, the_type_table.type_capacity);
		
		the_type_table.type_capacity = new_type_capacity;
		the_type_table.types = new_types;
	}
	
	the_type_table.types[the_type_table.type_count] = type;
	the_type_table.type_count += 1;
	
	return the_type_table.type_count - 1;
}

////////////////////////////////
//~ Printing

internal void print_type(Type *type) {
	assert(type != NULL);
	
	if (type->kind == TYPE_VOID || type->kind == TYPE_BOOLEAN ||
		type->kind == TYPE_INTEGER || type->kind == TYPE_STRING) {
		printf("%.*s (%d)", string_expand(type_kind_names[type->kind]), type->size);
	} else if (type->kind == TYPE_POINTER) {
		printf("^");
		print_type(type_from_id(type->pointed));
	} else if (type->kind == TYPE_STRUCT) {
		printf("struct {");
		for (int i = 0; i < type->member_count; i += 1) {
			print_type(type_from_id(type->members[i]));
			if (i < type->member_count - 1)  printf(", ");
		}
		printf("}");
	} else if (type->kind == TYPE_PROC) {
		printf("proc (");
		for (int i = 0; i < type->param_count;  i += 1) {
			print_type(type_from_id(type->params[i]));
			if (i < type->param_count  - 1)  printf(", ");
		}
		printf(")%s", type->retval_count > 0 ? " ->" : "");
		for (int i = 0; i < type->retval_count; i += 1) {
			print_type(type_from_id(type->retvals[i]));
			if (i < type->retval_count - 1)  printf(", ");
		}
	}
	
	return;
}

#endif
