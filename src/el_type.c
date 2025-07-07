#ifndef EL_TYPE_C
#define EL_TYPE_C

internal bool type_is_builtin(Type type) {
	if (type.atom_count == 1) {
		return type.atoms[0].kind == TYPE_VOID ||
			type.atoms[0].kind == TYPE_BOOLEAN ||
			type.atoms[0].kind == TYPE_INTEGER ||
			type.atoms[0].kind == TYPE_STRING;
	}
	return false;
}

internal bool type_is_atomic(Type type) {
	return type_is_builtin(type) /* TODO: || is an alias or a distinct of a builtin */;
}

internal bool type_is_atom_kind(Type type, Type_Kind kind) {
	if (type.atom_count == 1) return type.atoms[0].kind == kind;
	return false;
}

internal Type_Array push_type_array(Arena *arena, i64 count) {
	Type_Array result = {
		.data  = push_array(arena, Type, count),
		.count = count,
	};
	
	for (i64 i = 0; i < count; i += 1)
		result.data[i].atoms = &result.data[i].fallback_atom;
	
	return result;
}

internal Type_Array type_array_slice(Type_Array types, i64 start, i64 end) {
	Type_Array result = types;
	
	if (start < 0) start = 0;
	if (start > types.count - 1) start = types.count - 1;
	
	if (end < 0) end = 0;
	if (end > types.count - 1) end = types.count - 1;
	
	if (end < start) { i64 t = end; end = start; start = t; }
	
	result.count  = end;
	result.data  += start;
	result.count -= start;
	
	return result;
}

internal Type make_type_from_proc_defn(Typechecker *checker, Ast_Declaration *first_param, Ast_Statement *body) {
	int atom_count = 1; // At least 1 for the proc itself
	
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
	
	Type type = {0};
	type.atoms = push_array(checker->arena, Type_Atom, atom_count);
	type.atom_count = atom_count;
	type.atoms[0].kind = TYPE_PROC;
	
	return type;
}

#endif
