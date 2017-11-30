open Token

type t = {
  lexbuf : Sedlexing.lexbuf;
  mutable lex_env : Lex_env.t;
  mutable current_tok : Token.t;
}

let create lexbuf =
  let lex_env = {
    Lex_env.lex_lb                = lexbuf;
    Lex_env.lex_bol               = Lex_env.init_bol;
    Lex_env.lex_in_comment_syntax = false;
    Lex_env.lex_enable_comment_syntax = true;
    Lex_env.lex_state             = Lex_env.empty_lex_state;
  } in {
    lexbuf;
    lex_env;
    current_tok = T_RPAREN;
  }

let peek tt = tt.current_tok

let lexbuf env = env.lexbuf

let rec next_token t : Token.t =
  let tmp = peek t in
  let () =
    if tmp <> Token.T_EOF then
      let rec read_token env lexbuf = (
        match Lexer.token env lexbuf with
        | Lexer.Token (env, tok) ->
          t.lex_env <- env;
          t.current_tok <- tok;
        | Lexer.Continue env ->
          read_token env lexbuf
        )
      in read_token t.lex_env t.lexbuf
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
  | T_ASSIGN, T_ASSIGN
  (* Keywords *)
  | T_DEF, T_DEF
  | T_FUN, T_FUN
  | T_IF, T_IF
  | T_THEN, T_THEN
  | T_END, T_END
  | T_IN, T_IN
  | T_INSTANCEOF, T_INSTANCEOF
  | T_RETURN, T_RETURN
  | T_SWITCH, T_SWITCH
  | T_THROW, T_THROW
  | T_TRY, T_TRY
  | T_WHILE, T_WHILE
  | T_WITH, T_WITH
  | T_CONST, T_CONST
  | T_LET, T_LET
  | T_NULL, T_NULL
  | T_FALSE, T_FALSE
  | T_TRUE, T_TRUE
  | T_BREAK, T_BREAK
  | T_CASE, T_CASE
  | T_CATCH, T_CATCH
  | T_CONTINUE, T_CONTINUE
  | T_DEFAULT, T_DEFAULT
  | T_DO, T_DO
  | T_FINALLY, T_FINALLY
  | T_FOR, T_FOR
  | T_CLASS, T_CLASS
  | T_EXTENDS, T_EXTENDS
  | T_ELSE, T_ELSE
  | T_NEW, T_NEW
  | T_TYPEOF, T_TYPEOF
  | T_EXPORT, T_EXPORT
  | T_IMPORT, T_IMPORT
  | T_SUPER, T_SUPER
  | T_IMPLEMENTS, T_IMPLEMENTS
  | T_INTERFACE, T_INTERFACE
  | T_PACKAGE, T_PACKAGE
  | T_PRIVATE, T_PRIVATE
  | T_PROTECTED, T_PROTECTED
  | T_PUBLIC, T_PUBLIC
  | T_YIELD, T_YIELD
  | T_DEBUGGER, T_DEBUGGER
  | T_DECLARE, T_DECLARE
  | T_TYPE, T_TYPE
  | T_OF, T_OF
  | T_ASYNC, T_ASYNC
  | T_AWAIT, T_AWAIT
  | T_CHECKS, T_CHECKS
  -> true
  | _, _ -> false

let eat t tok =
  if match_token t tok then
    let _ = next_token t in
    true
  else false


exception ParsingError of string

let throw_error t (content:string) =
  let env = t.lex_env in
  let begin_loc = Lex_env.(Lexer.loc_of_lexbuf env env.lex_lb) in
  let start = begin_loc.start in
  let messasge = Printf.sprintf "%d:%d: Error: %s"
    start.line start.column
    content
  in
  raise (ParsingError messasge)

let expect t tok =
  if match_token t tok then ()
  else
    let this_tok_name = value_of_token tok in
    let target_tok_name = value_of_token (peek t) in
    let messasge = Printf.sprintf
      "Unexpected: %s expected: %s"
      this_tok_name target_tok_name
    in
    throw_error t messasge
