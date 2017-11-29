open Ast
open Token
open Parser_common
open StatementParser
open ExpressionParser

module rec RealParser : PARSER = struct
  include StatementParser(RealParser)
  include ExpressionParser(RealParser)

  let parse env : Program.t =
    Ast.Program.([])

end

let () =
  let lexbuf = Sedlexing.Latin1.from_string "foobar \"asdf\" A123Bfoo  ++++123Xbar/foo" in
  let lex_env = {
    Lex_env.lex_lb                = lexbuf;
    Lex_env.lex_bol               = Lex_env.init_bol;
    Lex_env.lex_in_comment_syntax = false;
    Lex_env.lex_enable_comment_syntax = true;
    Lex_env.lex_state             = Lex_env.empty_lex_state;
  }
  in

  let env = Parser_env.create lex_env
  in

  let _ = Parser_env.next_token env
  in

  ()
