open Token

type t = {
  lex_env : Lex_env.t;
  mutable current_tok : Token.t;
}

let create lex_env = {
  lex_env;
  current_tok = T_RPAREN;
}

let peek tt = tt.current_tok

let lexbuf env = env.lex_env.lex_lb

let rec next_token t : Token.t =
  let tmp = peek t in
  let () =
    if tmp <> Token.T_EOF then
      let rec _read_token () =
        match Lexer.token t.lex_env (lexbuf t) with
          | Lexer.Token (lex_env, tok) ->
            t.current_tok <- tok
          | Lexer.Continue lex_env ->
            _read_token ()
      in _read_token ()
    else ()
  in
  tmp

let match_token t tok =
  match (peek t), tok with
  | T_NUMBER _, T_NUMBER _
  | T_STRING _, T_STRING _
  | T_IDENTIFIER _, T_IDENTIFIER _
  (* Syntax *)
  | T_LCURLY, T_LCURLY
  | T_RCURLY, T_RCURLY
  | T_LCURLYBAR, T_LCURLYBAR
  | T_RCURLYBAR, T_RCURLYBAR
  | T_LPAREN, T_LPAREN
  | T_RPAREN, T_RPAREN
  | T_LBRACKET, T_LBRACKET
  | T_RBRACKET, T_RBRACKET
  | T_SEMICOLON, T_SEMICOLON
  | T_COMMA, T_COMMA
  | T_PERIOD, T_PERIOD
  | T_ARROW, T_ARROW
  | T_ELLIPSIS, T_ELLIPSIS
  | T_AT, T_AT
  | T_POUND, T_POUND
  (* Keywords *)
  | T_DEF, T_DEF
  | T_FUN, T_FUN
  | T_IF, T_IF
  | T_THEN, T_THEN
  | T_END, T_END -> true
  | _, _ -> false

let eat t tok =
  if match_token t tok then
    let _ = next_token t in
    true
  else false

(* TODO: finish  *)
let expect t tok =
  if match_token t tok then
  ()
  else
    failwith "unexpected token"
