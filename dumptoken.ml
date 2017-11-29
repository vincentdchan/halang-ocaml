open Token
open Lexer

let () =
  let src = "def name(ok) do print(\"hello\") end"
  in
  let lexbuf = Sedlexing.Latin1.from_string src in
  let env = {
    Lex_env.lex_lb                = lexbuf;
    Lex_env.lex_bol               = Lex_env.init_bol;
    Lex_env.lex_in_comment_syntax = false;
    Lex_env.lex_enable_comment_syntax = true;
    Lex_env.lex_state             = Lex_env.empty_lex_state;
  } in
  let rec print_token env lexbuf =
    match token env lexbuf with
    | Token (lex_env, tok) ->
      Loc.(
        let begin_loc = Lex_env.(loc_of_lexbuf env env.lex_lb) in
        let start = begin_loc.start in
        let _end = begin_loc._end in
        Printf.printf "%s %d:%d %d:%d\n" (token_to_string tok)
          start.line start.column
          _end.line _end.column);
      if tok <> T_EOF then print_token lex_env lexbuf else ()
    | Continue lex_env -> print_token lex_env lexbuf
  in
  print_token env lexbuf
