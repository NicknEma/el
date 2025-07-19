#ifndef EL_ALL_TESTS_C
#define EL_ALL_TESTS_C

internal void test_expression_parser_single(String source, Parse_Flags flags) {
	Scratch scratch = scratch_begin(0, 0);
	
	static int expr_count = 0;
	String name = push_stringf(scratch.arena, "expr_%d", expr_count);
	
	printf("Parsing sample expression \"%.*s\":\n", string_expand(source));
	Ast_Expression *expr = expr_from_string(scratch.arena, source, name, flags);
	print_expression_tree(expr);
	
	expr_count += 1;
	scratch_end(scratch);
}

internal void test_statement_parser_single(String source) {
	Scratch scratch = scratch_begin(0, 0);
	
	static int stmt_count = 0;
	String name = push_stringf(scratch.arena, "stmt_%d", stmt_count);
	
	printf("Parsing sample statement %.*s:\n", string_expand(source));
	Ast_Statement *stmt = stmt_from_string(scratch.arena, source, name);
	print_statement_tree(stmt);
	
	stmt_count += 1;
	scratch_end(scratch);
}

#if 0
internal void test_declaration_parser_single(String source) {
	Scratch scratch = scratch_begin(0, 0);
	
	static int decl_count = 0;
	String name = push_stringf(scratch.arena, "decl_%d", decl_count);
	
	printf("Parsing sample declaration %.*s:\n", string_expand(source));
	Ast_Declaration *decl = decl_from_string(scratch.arena, source, name);
	print_declaration_tree(decl);
	
	decl_count += 1;
	scratch_end(scratch);
}
#endif

internal void test_expression_parser(void) {
	printf("### Testing expression parser ###\n\n");
	
	Parse_Flags def_flags = Parse_Flags_EXPR_DEFAULT;
	
#if 0
	// No errors
	test_expression_parser_single(string_from_lit("1+2"), def_flags);                    // 3
	test_expression_parser_single(string_from_lit("5 - 4"), def_flags);                  // 1
	test_expression_parser_single(string_from_lit("(3 * 4) - (10 / 2) + 1"), def_flags); // 8
	test_expression_parser_single(string_from_lit("7 + (-2) - (3 - 1)"), def_flags);     // 3
	test_expression_parser_single(string_from_lit("5 + (+4)"), def_flags);               // 9
	test_expression_parser_single(string_from_lit("1++2"), def_flags);                   // 3
	test_expression_parser_single(string_from_lit("foo()"), def_flags);
	
	test_expression_parser_single(string_from_lit("a?b:c"), def_flags);
	test_expression_parser_single(string_from_lit("a?b:c?d:e"), def_flags);
	test_expression_parser_single(string_from_lit("a?b?c:d:e"), def_flags);
	test_expression_parser_single(string_from_lit("a?b+c:d+e"), def_flags);
	test_expression_parser_single(string_from_lit("a+b?c:d"), def_flags);
	
	test_expression_parser_single(string_from_lit("true"), def_flags);
	test_expression_parser_single(string_from_lit("true + false"), def_flags);
#endif
	
	// Compound literals
	test_expression_parser_single(string_from_lit("int{}"), def_flags);
	test_expression_parser_single(string_from_lit("int{0}"), def_flags);
	test_expression_parser_single(string_from_lit("int{0,0}"), def_flags);
	test_expression_parser_single(string_from_lit("int{0,}"), def_flags);
	test_expression_parser_single(string_from_lit("[]int{}"), def_flags);
	
#if 1
	// With errors
	test_expression_parser_single(string_from_lit("1,"), def_flags);
	test_expression_parser_single(string_from_lit("+"), def_flags);
	test_expression_parser_single(string_from_lit("*"), def_flags);
	test_expression_parser_single(string_from_lit("(4 + 1"), def_flags);
	test_expression_parser_single(string_from_lit("foo("), def_flags);
#endif
	
#if 0
	// Assignments
	test_expression_parser_single(string_from_lit("a=0"), Parse_Expr_Flags_ALLOW_ASSIGNMENT|Parse_Expr_Flags_ALLOW_COMMA);
	test_expression_parser_single(string_from_lit("=0"), Parse_Expr_Flags_ALLOW_ASSIGNMENT|Parse_Expr_Flags_ALLOW_COMMA);
	test_expression_parser_single(string_from_lit("a="), Parse_Expr_Flags_ALLOW_ASSIGNMENT|Parse_Expr_Flags_ALLOW_COMMA);
#endif
	
#if 0
	Scratch scratch = scratch_begin(0, 0);
	
	srand(rand());
	char filter[] = { 0x7, 0x0 }; // Bell
	String expr_rand_1 = push_rand_string(scratch.arena, 16, string_from_lit(filter));
	String expr_rand_2 = push_rand_string(scratch.arena, 32, string_from_lit(filter));
	String expr_rand_3 = push_rand_string(scratch.arena, 64, string_from_lit(filter));
	
	test_expression_parser_single(expr_rand_1);
	test_expression_parser_single(expr_rand_2);
	test_expression_parser_single(expr_rand_3);
	
	scratch_end(scratch);
#endif
	
	printf("\n");
}

internal void
test_statement_parser(void) {
	printf("### Testing statement parser ###\n\n");
	
#if 0
	// Simple statements
	test_statement_parser_single(string_from_lit("1 + 2;"));
#endif
	
	test_statement_parser_single(string_from_lit("return;"));
	test_statement_parser_single(string_from_lit("return 0;"));
	test_statement_parser_single(string_from_lit("return 1 + 2;"));
	test_statement_parser_single(string_from_lit("return (1 + 2);"));
	
#if 0
	test_statement_parser_single(string_from_lit("{ return; return; }"));
	test_statement_parser_single(string_from_lit("{ ;; }"));
	
	// Simple statements with errors
	test_statement_parser_single(string_from_lit("1 + 2"));
	test_statement_parser_single(string_from_lit("{ return } "));
	test_statement_parser_single(string_from_lit("{ return; "));
#endif
	
	// Assignments and declarations
	test_statement_parser_single(string_from_lit("a = 0;"));
	test_statement_parser_single(string_from_lit("a, b = 0;"));
	test_statement_parser_single(string_from_lit("a, b = 0, 0;"));
	test_statement_parser_single(string_from_lit("a := 0;"));
	test_statement_parser_single(string_from_lit("a : int = 0;"));
	test_statement_parser_single(string_from_lit("a : ^int = 0;"));
	test_statement_parser_single(string_from_lit("a : []int = 0;"));
	test_statement_parser_single(string_from_lit("a := struct{x:int}{0};"));
	
	test_statement_parser_single(string_from_lit("a := struct{x:int};"));
	
#if 0
	test_statement_parser_single(string_from_lit("a :: proc();"));
	test_statement_parser_single(string_from_lit("a :: proc() ---;"));
	test_statement_parser_single(string_from_lit("a :: proc() {}"));
#endif
	
#if 0
	Scratch scratch = scratch_begin(0, 0);
	
	srand(rand());
	char filter[] = { 0x7, 0x0 }; // Bell
	String stat_rand_1 = push_rand_string(scratch.arena, 16, string_from_lit(filter));
	String stat_rand_2 = push_rand_string(scratch.arena, 32, string_from_lit(filter));
	String stat_rand_3 = push_rand_string(scratch.arena, 64, string_from_lit(filter));
	
	test_statement_parser_single(stat_rand_1);
	test_statement_parser_single(stat_rand_2);
	test_statement_parser_single(stat_rand_3);
	
	scratch_end(scratch);
#endif
	
	printf("\n");
}

#if 0
internal void test_declaration_parser(void) {
	printf("### Testing declaration parser ###\n\n");
	
#if 0
	// Single
	test_declaration_parser_single(string_from_lit("a :: proc() {}"));
	test_declaration_parser_single(string_from_lit("a :: proc() {;}"));
	test_declaration_parser_single(string_from_lit("a :: 4;"));
	// test_declaration_parser_single(string_from_lit("a : : 4;"));
	test_declaration_parser_single(string_from_lit("a : int;"));
	test_declaration_parser_single(string_from_lit("a : int = 4;"));
	test_declaration_parser_single(string_from_lit("a : int = proc() {};"));
#endif
	
	// Multiple lhs
	test_declaration_parser_single(string_from_lit("a, b := 4;"));
	test_declaration_parser_single(string_from_lit("a, b : int = 4;"));
	
	// Multiple rhs
	test_declaration_parser_single(string_from_lit("a := 4, 5;"));
	test_declaration_parser_single(string_from_lit("a := 4, proc();"));
	test_declaration_parser_single(string_from_lit("a := proc(), 4;"));
	
	// Procedures
	test_declaration_parser_single(string_from_lit("a := proc();"));
	test_declaration_parser_single(string_from_lit("a := proc() {};"));
	test_declaration_parser_single(string_from_lit("a := proc() ---;"));
	test_declaration_parser_single(string_from_lit("a := proc() -> int;"));
	test_declaration_parser_single(string_from_lit("a := proc() -> int {};"));
	test_declaration_parser_single(string_from_lit("a := proc() -> int ---;"));
	test_declaration_parser_single(string_from_lit("a := proc() -> int, int;"));
	test_declaration_parser_single(string_from_lit("a := proc() -> int, int {};"));
	test_declaration_parser_single(string_from_lit("a := proc() -> int, int ---;"));
	
	test_declaration_parser_single(string_from_lit("foo :: ("));
	
#if 0
	Scratch scratch = scratch_begin(0, 0);
	
	srand(rand());
	char filter[] = { 0x7, 0x0 }; // Bell
	String decl_rand_1 = push_rand_string(scratch.arena, 16, string_from_lit(filter));
	String decl_rand_2 = push_rand_string(scratch.arena, 32, string_from_lit(filter));
	String decl_rand_3 = push_rand_string(scratch.arena, 64, string_from_lit(filter));
	
	test_declaration_parser_single(decl_rand_1);
	test_declaration_parser_single(decl_rand_2);
	test_declaration_parser_single(decl_rand_3);
	
	scratch_end(scratch);
#endif
	
	printf("\n");
}
#endif

internal void test_all(void) {
	
	{
		test_expression_parser();
		test_statement_parser();
		// test_declaration_parser();
	}
	
	// x64_test();
	
	{
#if 0
		Arena idk_arena = {0};
		arena_init(&idk_arena);
		
		Ast_Statement *stat = parse_statement_string(&idk_arena, string_from_lit("{ return 0+2; }"));
		analyse_statement(&idk_arena, stat, NULL);
#endif
	}
	
	return;
}

#endif
