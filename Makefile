
dumptoken: lexer.cmx dumptoken.ml
	ocamlfind ocamlopt -o dumptoken -linkpkg -package sedlex \
	token.cmx loc.cmx lex_env.cmx lexer.cmx dumptoken.ml

lexer.cmx: loc.cmx token.cmx lex_env.cmx lexer.ml
	ocamlfind ocamlopt -c -package sedlex \
	loc.cmx lex_env.cmx token.cmx lexer.ml

loc.cmx: loc.ml
	ocamlc -c loc.mli;
	ocamlopt -c loc.ml

token.cmx: loc.cmx token.ml
	ocamlfind ocamlopt -c -package sedlex token.ml

lex_env.cmx: loc.cmx lex_env.ml
	ocamlfind ocamlopt -c -package sedlex loc.cmx lex_env.ml

ast.cmx: ast.ml
	ocamlfind ocamlopt -c -package sedlex ast.ml

parser_env.cmx: token.cmx lex_env.cmx lexer.cmx  \
	parser_env.mli parser_env.ml
	ocamlfind ocamlc -c -package sedlex parser_env.mli;
	ocamlfind ocamlopt -c -package sedlex \
	token.cmx lex_env.cmx \
	lexer.cmx parser_env.ml

parser_common.cmx : ast.cmx parser_common.ml
	ocamlopt -c ast.cmx parser_common.ml

statementParser.cmx : ast.cmx token.cmx parser_common.cmx \
	statementParser.ml
	ocamlopt -c ast.cmx token.cmx parser_common.cmx \
		statementParser.ml

parser_utils.cmx: ast.cmx parser_utils.ml
	ocamlopt -c ast.cmx parser_utils.ml

expressionParser.cmx : ast.cmx token.cmx parser_common.cmx \
	parser_utils.cmx expressionParser.ml
	ocamlopt -c ast.cmx token.cmx parser_common.cmx \
		parser_utils.cmx expressionParser.ml

# parser.cmx: parser_env.cmx parser_common.cmx statementParser.cmx \
# 	expressionParser.cmx parser.ml
# 	ocamlfind ocamlopt -c -package sedlex \
# 	parser_env.cmx parser_common.cmx \
# 	statementParser.cmx expressionParser.cmx \
# 	parser.ml

parser: lexer.cmx lex_env.cmx token.cmx \
	parser_env.cmx parser_common.cmx statementParser.cmx \
	expressionParser.cmx parser_utils.cmx parser.ml
	ocamlfind ocamlopt -o parser -linkpkg -package sedlex \
	loc.cmx token.cmx lex_env.cmx lexer.cmx \
	parser_env.cmx parser_common.cmx \
	statementParser.cmx expressionParser.cmx \
	parser_utils.cmx parser.ml

clean:
	rm -f ./*.cmi;
	rm -f ./*.cmx;
	rm -f ./*.o;
	rm -f ./dumptoken
	rm -f ./parser
