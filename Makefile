
dumptoken: lexer.cmx dumptoken.ml
	ocamlfind ocamlopt -o dumptoken -linkpkg -package sedlex \
	token.cmx loc.cmx lex_env.cmx lexer.cmx dumptoken.ml

lexer.cmx: loc.cmx token.cmx lex_env.cmx lexer.ml
	ocamlfind ocamlopt -c -package sedlex \
	loc.cmx lex_env.cmx token.cmx lexer.ml

loc.cmx: loc.ml
	ocamlc -c loc.mli;
	ocamlopt -c loc.ml

token.cmx: token.ml
	ocamlfind ocamlopt -c -package sedlex token.ml

lex_env.cmx: loc.cmx lex_env.ml
	ocamlfind ocamlopt -c -package sedlex loc.cmx lex_env.ml

ast.cmx: ast.ml
	ocamlfind ocamlopt -c -package sedlex ast.ml

parser_env.cmx: token.cmx lex_env.cmx  \
	parser_env.mli parser_env.ml
	ocamlfind ocamlc -c parser_env.mli;
	ocamlfind ocamlopt -c token.cmx lex_env.cmx \
	parser_env.ml

parser_common.cmx : ast.cmx parser_common.ml
	ocamlopt -c ast.cmx parser_common.ml

statementParser.cmx : ast.cmx token.cmx parser_common.cmx \
	statementParser.ml
	ocamlopt -c ast.cmx token.cmx parser_common.cmx \
		statementParser.ml

expressionParser.cmx : ast.cmx token.cmx parser_common.cmx \
	expressionParser.ml
	ocamlopt -c ast.cmx token.cmx parser_common.cmx \
		expressionParser.ml

parser.cmx: parser_env.cmx parser_common.cmx statementParser.cmx \
	expressionParser.cmx parser.ml
	ocamlfind ocamlopt -c -package sedlex \
	parser_env.cmx parser_common.cmx \
	statementParser.cmx expressionParser.cmx \
	parser.ml

clean:
	rm ./*.cmi;
	rm ./*.cmx;
	rm ./*.o;
	rm ./dumptoken
