
type t = {
  lex_lb                : Sedlexing.lexbuf;
  lex_bol               : bol;
  lex_in_comment_syntax : bool;
  lex_enable_comment_syntax: bool;
  lex_state             : lex_state;
}

(* bol = Beginning Of Line *)
and bol = {
  line: int;
  offset: int;
}

and lex_state = {
  lex_errors_acc: (Loc.t * string) list;
  lex_comments_acc: (Loc.t * string) list;
}

let empty_lex_state = {
  lex_errors_acc = [];
  lex_comments_acc = [];
}

let init_bol = {
  line = 1;
  offset = 0;
}

let lexbuf env = env.lex_lb
let with_lexbuf ~lexbuf env = { env with lex_lb = lexbuf }
let state env = env.lex_state
let line env = env.lex_bol.line
let bol_offset env = env.lex_bol.offset
let is_in_comment_syntax env = env.lex_in_comment_syntax
let is_comment_syntax_enabled env = env.lex_enable_comment_syntax
let in_comment_syntax is_in env =
  if is_in <> env.lex_in_comment_syntax
  then { env with lex_in_comment_syntax = is_in }
  else env