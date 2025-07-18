#include "raddbg_markup.h"

#include "el_base/el_ctx_crack.h"
#include "el_base/el_base.h"
#include "el_base/el_print.h"
#include "el_os/el_os.h"

#include "el_base/el_base.c"
#include "el_base/el_print.c"
#include "el_os/el_os.c"

#define EL_CHECK_SINGLE_PASS 1

// #include "el_print.h"
#include "el_type.h"
#include "el_lex.h"
#include "el_ast.h"
#include "el_parse.h"

#if EL_CHECK_SINGLE_PASS
// #include "el_check_single_pass.h"
#else
#include "el_check.h"
#endif

// #include "el_bcode.h"
// #include "el_masm.h"

// #include "el_print.c"
#include "el_type.c"
#include "el_lex.c"
#include "el_parse.c"
#include "el_ast_print.c"

#if EL_CHECK_SINGLE_PASS
// #include "el_check_single_pass.c"
#else
#include "el_check.c"
#endif

// #include "el_bcode.c"
// #include "el_masm.c"

#include "el_x64.c"
#include "el_all_tests.c"

/*
** TODO(ema):
** [ ] Change initialization of parser/checker/builder so that they own their arena
** [ ] Make parse_* take an arena param so we can allocate nodes on scratch arenas
** [ ] Type annotations are stored by pointer
** [ ] More 'recursive' compound literal parsing ([]int vs [][]int etc.)
**
** [ ] Store pseudo-registers in the Symbol struct
**     https://godbolt.org/z/G1e4d8M3M
** [ ] Symbol_Kind (local var, global var, function, etc) so we can either store a stack offset or an address
** [ ] Assigner and Declarator types
** [ ] Refactor Expr, Stat and Decl from lists into arrays
** [ ] Printer
** [ ] Input reading
*/

int main(int argc, char **argv) {
	test_all();
	printf("### Main program output ###\n\n");
	
	bool all_ok = true;
	
	// Load source
	String source = {0};
	Arena  source_arena = {0};
	arena_init(&source_arena);
	if (argc > 1) {
		String_And_Bool load_result = load_file_string(&source_arena, argv[1]);
		source = load_result.str;
		all_ok = load_result.ok;
	} else {
		all_ok = false;
	}
	
#if 0
	// Parse
	Ast_Declaration *program = &nil_declaration;
	
	Arena  ast_arena = {0};
	Parser parser = {0};
	if (all_ok) {
		arena_init(&ast_arena, .reserve_size = estimate_ast_arena_size(source));
		parser_init(&parser, &ast_arena, .text = source);
		
		program = parse_program(&parser);
		
		if (there_were_parse_errors(&parser)) {
			all_ok = false;
		}
	}
	
	// Typecheck
	Arena symbol_arena = {0};
	Arena scope_name_arena = {0};
	Typechecker checker = {0};
	if (all_ok) {
		arena_init(&symbol_arena);
		arena_init(&scope_name_arena);
		
		typechecker_init(&checker, &symbol_arena, &scope_name_arena);
		
		do_all_checks(&checker, program);
		
		if (checker.error_count > 0) {
			all_ok = false;
		}
	}
	
	// Generate code
	if (all_ok) {
		Arena bcode_arena;
		arena_init(&bcode_arena);
		
		Bcode_Builder builder = {0};
		bcode_builder_init(&builder, &bcode_arena, &checker.symbol_table);
		
		generate_bcode(&builder, program);
		
		arena_init(&masm_context.arena);
		String masm_source = masm_generate_source(&builder);
		
		FILE *sf = fopen("generated/generated.asm", "wb+");
		FILE *bs = fopen("build_generated.bat", "wb+");
		if (sf && bs) {
			fwrite(masm_source.data, sizeof(char), masm_source.len, sf);
			fclose(sf);
			
			char buf[1024] = {0};
			int buf_len = snprintf(buf, array_count(buf), "@echo off\n"
								   "del *.pdb > NUL 2> NUL\n"
								   "del *.rdi > NUL 2> NUL\n"
								   "ml64 generated/generated.asm /nologo /Fegenerated/generated.exe /W4 /WX /Zi /link /incremental:no /opt:ref\n"
								   "del *.obj > NUL 2> NUL\n"
								   "del *.ilk > NUL 2> NUL\n"
								   "del mllink$* > NUL 2> NUL\n");
			
			fwrite(buf, sizeof(char), buf_len, bs);
			fclose(bs);
			system("build_generated.bat");
		}
	}
#endif
	
	return 0;
}
