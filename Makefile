
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

# lexer.cmx: lex_env.ml
# 	ocamlfind ocamlopt -c -linkpkg -package sedlex lex_env.ml

clean:
	rm ./*.cmi;
	rm ./*.cmx;
	rm ./*.o;
	rm ./dumptoken
