#include "el_ctx_crack.h"
#include "el_base.h"

#include "el_base.c"

#define EL_CHECK_SINGLE_PASS 1

#include "el_type.h"
#include "el_lex.h"
#include "el_ast.h"
#include "el_parse.h"

#if EL_CHECK_SINGLE_PASS
#include "el_check_single_pass.h"
#else
#include "el_check.h"
#endif

#include "el_bcode.h"
#include "el_masm.h"

#include "el_print.c"
#include "el_type.c"
#include "el_lex.c"
#include "el_parse.c"
#include "el_ast_print.c"

#if EL_CHECK_SINGLE_PASS
#include "el_check_single_pass.c"
#else
#include "el_check.c"
#endif

#include "el_bcode.c"
#include "el_masm.c"

#include "el_x64.c"
#include "el_all_tests.c"

/*
** TODO(ema):
** [ ] Symbol_Kind (local var, global var, function, etc) so we can either store a stack offset or an address
** [ ] Assigner and Declarator types
** [ ] Refactor Expr, Stat and Decl from lists into arrays
** [ ] Printer
** [ ] Input reading
*/

int main(void) {
	test_all();
	printf("### Main program output ###\n\n");
	
#if 0
	String source = string_from_lit("main :: proc() { return other(); }"
									"other :: proc() { return 2*3 + 10/(4+1); 7-0; }");
#elif 0
	String source = string_from_lit("a := 0\n"
									"\n"
									"main :: proc() {\n"
									"a = 5;\n"
									"a := \"Hello\";\n"
									"a = \"world\";\n"
									"\n"
									"{\n"
									"a = \"goodbye\";\n"
									"a := \"moon\";\n"
									"}\n"
									"\n"
									"b = 69.420;\n"
									"\n"
									"c := C;\n"
									"C :: 1;\n"
									"\n"
									"d := D;\n"
									"}\n"
									"\n"
									"b := 314\n"
									"D :: 0\n"
									);
#elif 1
	String source = string_from_lit("a := 0\n"
									"b := \"Hi\"\n"
									"c := a\n"
									"main :: proc() {\n"
									"a;\n"
									"a := 5;\n"
									"a;\n"
									"}\n"
									);
#endif
	
	bool all_ok = true;
	
	Arena tree_arena = {0};
	arena_init(&tree_arena);
	
	Parser parser = {0};
	parser_init(&parser, &tree_arena, source);
	
	Ast_Declaration *program = &nil_declaration;
	program = parse_program(&parser);
	
	if (there_were_parse_errors(&parser)) {
		all_ok = false;
	}
	
	if (all_ok) {
		do_all_checks(program);
		
		for (Ast_Declaration *decl = program; decl != NULL && decl != &nil_declaration; decl = decl->next) {
			generate_bytecode_for_declaration(decl);
		}
		
		arena_init(&masm_context.arena);
		String masm_source = masm_generate_source();
		
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
	
	return 0;
}
