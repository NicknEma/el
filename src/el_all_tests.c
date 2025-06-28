#ifndef EL_ALL_TESTS_C
#define EL_ALL_TESTS_C

internal void
test_expression_parser_single(String source) {
	Scratch scratch = scratch_begin(0, 0);
	
	printf("Parsing sample expression %.*s:\n", string_expand(source));
	Ast_Expression *tree = parse_expression_string(scratch.arena, source);
	print_expression_tree(tree);
	
	scratch_end(scratch);
}

internal void
test_expression_parser(void) {
	printf("### Testing expression parser ###\n\n");
	
	String expr_1 = string_from_lit("1+2");                    // 3
	String expr_2 = string_from_lit("5 - 4");                  // 1
	String expr_3 = string_from_lit("(3 * 4) - (10 / 2) + 1"); // 8
	String expr_4 = string_from_lit("7 + (-2) - (3 - 1)");     // 3
	String expr_5 = string_from_lit("5 + (+4)");               // 9
	String expr_6 = string_from_lit("1++2");                   // 3
	String expr_7 = string_from_lit("foo()");
	
	String expr_8 = string_from_lit("+");
	String expr_9 = string_from_lit("(4 + 1");
	String expr_0 = string_from_lit("foo(");
	
	test_expression_parser_single(expr_1);
	test_expression_parser_single(expr_2);
	test_expression_parser_single(expr_3);
	test_expression_parser_single(expr_4);
	test_expression_parser_single(expr_5);
	test_expression_parser_single(expr_6);
	test_expression_parser_single(expr_7);
	test_expression_parser_single(expr_8);
	test_expression_parser_single(expr_9);
	test_expression_parser_single(expr_0);
	
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
	
	printf("\n");
}

internal void
test_statement_parser_single(String source) {
	Scratch scratch = scratch_begin(0, 0);
	
	printf("Parsing sample statement %.*s:\n", string_expand(source));
	Ast_Statement *tree = parse_statement_string(scratch.arena, source);
	print_statement_tree(tree);
	
	scratch_end(scratch);
}

internal void
test_statement_parser(void) {
	printf("### Testing statement parser ###\n\n");
	
#if 0
	{
		test_statement_parser_single((String)string_from_lit_const("1 + 2;"));
		test_statement_parser_single((String)string_from_lit_const("return;"));
		test_statement_parser_single((String)string_from_lit_const("return 0;"));
		test_statement_parser_single((String)string_from_lit_const("return 1 + 2;"));
		test_statement_parser_single((String)string_from_lit_const("return (1 + 2);"));
		test_statement_parser_single((String)string_from_lit_const("{ return; return; }"));
		test_statement_parser_single((String)string_from_lit_const("{ ;; }"));
		
		test_statement_parser_single((String)string_from_lit_const("1 + 2"));
		test_statement_parser_single((String)string_from_lit_const("{ return } "));
		test_statement_parser_single((String)string_from_lit_const("{ return; "));
	}
#endif
	
#if 1
	{
		test_statement_parser_single((String)string_from_lit_const("a = 0;"));
		test_statement_parser_single((String)string_from_lit_const("a, b = 0;"));
		test_statement_parser_single((String)string_from_lit_const("a, b = 0, 0;"));
		test_statement_parser_single((String)string_from_lit_const("a := 0;"));
		test_statement_parser_single((String)string_from_lit_const("a : int = 0;"));
		test_statement_parser_single((String)string_from_lit_const("a :: proc();"));
		test_statement_parser_single((String)string_from_lit_const("a :: proc() ---;"));
	}
#endif
	
#if 0
	{
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
	}
#endif
	
	printf("\n");
}

internal void
test_declaration_parser_single(String source) {
	Scratch scratch = scratch_begin(0, 0);
	
	printf("Parsing sample declaration %.*s:\n", string_expand(source));
	Ast_Declaration *tree = parse_declaration_string(scratch.arena, source);
	print_declaration_tree(tree);
	
	scratch_end(scratch);
}

internal void
test_declaration_parser(void) {
	printf("### Testing declaration parser ###\n\n");
	
	String decl_1 = string_from_lit_const("foo :: () { }");
	String decl_2 = string_from_lit_const("foo :: () { ; }");
	String decl_3 = string_from_lit_const("foo :: 4;");
	String decl_4 = string_from_lit_const("foo : : 4;");
	String decl_5 = string_from_lit_const("foo : int;");
#if 0
	String decl_6 = string_from_lit_const("{ return; return; }");
	String decl_7 = string_from_lit_const("{ ;; }");
#endif
	
	String decl_8 = string_from_lit_const("foo :: (");
#if 0
	String decl_9 = string_from_lit_const("{ return } ");
	String decl_0 = string_from_lit_const("{ return; ");
#endif
	
	test_declaration_parser_single(decl_1);
	test_declaration_parser_single(decl_2);
	test_declaration_parser_single(decl_3);
	test_declaration_parser_single(decl_4);
	test_declaration_parser_single(decl_5);
	test_declaration_parser_single(decl_8);
	
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
	
	printf("\n");
}

internal void
test_all(void) {
	
	{
		// max_printed_lex_errors   = 0;
		// max_printed_parse_errors = 0;
		
		// test_expression_parser();
		test_statement_parser();
		// test_declaration_parser();
		
		// max_printed_lex_errors   = I64_MAX;
		// max_printed_parse_errors = 1;
	}
	
	// x64_test();
	
	{
		Arena idk_arena = {0};
		arena_init(&idk_arena);
		
		Ast_Statement *stat = parse_statement_string(&idk_arena, string_from_lit("{ return 0+2; }"));
		analyse_statement(&idk_arena, stat, NULL);
	}
	
}

#endif
