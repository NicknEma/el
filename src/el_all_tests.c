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
	
#if 0
	// No errors
	test_expression_parser_single((String)string_from_lit_const("1+2"));                    // 3
	test_expression_parser_single((String)string_from_lit_const("5 - 4"));                  // 1
	test_expression_parser_single((String)string_from_lit_const("(3 * 4) - (10 / 2) + 1")); // 8
	test_expression_parser_single((String)string_from_lit_const("7 + (-2) - (3 - 1)"));     // 3
	test_expression_parser_single((String)string_from_lit_const("5 + (+4)"));               // 9
	test_expression_parser_single((String)string_from_lit_const("1++2"));                   // 3
	test_expression_parser_single((String)string_from_lit_const("foo()"));
	
	// With errors
	test_expression_parser_single((String)string_from_lit_const("+"));
	test_expression_parser_single((String)string_from_lit_const("(4 + 1"));
	test_expression_parser_single((String)string_from_lit_const("foo("));
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
	// Simple statements
	test_statement_parser_single((String)string_from_lit_const("1 + 2;"));
	test_statement_parser_single((String)string_from_lit_const("return;"));
	test_statement_parser_single((String)string_from_lit_const("return 0;"));
	test_statement_parser_single((String)string_from_lit_const("return 1 + 2;"));
	test_statement_parser_single((String)string_from_lit_const("return (1 + 2);"));
	test_statement_parser_single((String)string_from_lit_const("{ return; return; }"));
	test_statement_parser_single((String)string_from_lit_const("{ ;; }"));
	
	// Simple statements with errors
	test_statement_parser_single((String)string_from_lit_const("1 + 2"));
	test_statement_parser_single((String)string_from_lit_const("{ return } "));
	test_statement_parser_single((String)string_from_lit_const("{ return; "));
#endif
	
#if 0
	// Assignments and declarations
	test_statement_parser_single((String)string_from_lit_const("a = 0;"));
	test_statement_parser_single((String)string_from_lit_const("a, b = 0;"));
	test_statement_parser_single((String)string_from_lit_const("a, b = 0, 0;"));
	test_statement_parser_single((String)string_from_lit_const("a := 0;"));
	test_statement_parser_single((String)string_from_lit_const("a : int = 0;"));
	test_statement_parser_single((String)string_from_lit_const("a :: proc();"));
	test_statement_parser_single((String)string_from_lit_const("a :: proc() ---;"));
	test_statement_parser_single((String)string_from_lit_const("a :: proc() {}"));
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
	
#if 0
	// Single
	test_declaration_parser_single((String)string_from_lit_const("a :: proc() {}"));
	test_declaration_parser_single((String)string_from_lit_const("a :: proc() {;}"));
	test_declaration_parser_single((String)string_from_lit_const("a :: 4;"));
	// test_declaration_parser_single((String)string_from_lit_const("a : : 4;"));
	test_declaration_parser_single((String)string_from_lit_const("a : int;"));
	test_declaration_parser_single((String)string_from_lit_const("a : int = 4;"));
	test_declaration_parser_single((String)string_from_lit_const("a : int = proc() {};"));
#endif
	
	// Multiple lhs
	test_declaration_parser_single((String)string_from_lit_const("a, b := 4;"));
	test_declaration_parser_single((String)string_from_lit_const("a, b : int = 4;"));
	
	// Multiple rhs
	test_declaration_parser_single((String)string_from_lit_const("a := 4, 5;"));
	test_declaration_parser_single((String)string_from_lit_const("a := 4, proc();"));
	test_declaration_parser_single((String)string_from_lit_const("a := proc(), 4;"));
	
	// Procedures
	test_declaration_parser_single((String)string_from_lit_const("a := proc();"));
	test_declaration_parser_single((String)string_from_lit_const("a := proc() {};"));
	test_declaration_parser_single((String)string_from_lit_const("a := proc() ---;"));
	test_declaration_parser_single((String)string_from_lit_const("a := proc() -> int;"));
	test_declaration_parser_single((String)string_from_lit_const("a := proc() -> int {};"));
	test_declaration_parser_single((String)string_from_lit_const("a := proc() -> int ---;"));
	test_declaration_parser_single((String)string_from_lit_const("a := proc() -> int, int;"));
	test_declaration_parser_single((String)string_from_lit_const("a := proc() -> int, int {};"));
	test_declaration_parser_single((String)string_from_lit_const("a := proc() -> int, int ---;"));
	
	test_declaration_parser_single((String)string_from_lit_const("foo :: ("));
	
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

internal void
test_all(void) {
	
	{
		// max_printed_lex_errors   = 0;
		// max_printed_parse_errors = 0;
		
		// test_expression_parser();
		// test_statement_parser();
		test_declaration_parser();
		
		// max_printed_lex_errors   = I64_MAX;
		// max_printed_parse_errors = 1;
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
