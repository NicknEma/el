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

internal Type *make_proc_defn_type(Typechecker *checker, Ast_Declaration *first_param, Ast_Statement *body) {
	Type *type = push_type(checker->arena, Type);
	type->kind = TYPE_PROC;
	
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
	
	type->name = string_from_lit("(anonymous)");
	
	return type;
}

////////////////////////////////
//~ Type array

internal Type_Array push_type_array(Arena *arena, i64 count) {
	Type_Array a = {
		.data  = push_array(arena, Type *, count),
		.count = count,
	};
	
	for (i64 i = 0; i < count; i += 1)
		a.data[i] = push_type(arena, Type);
	
	return a;
}
#endif
