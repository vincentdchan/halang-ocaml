open Ast
open Token
open Parser_common
open StatementParser
open Parser_utils
open ExpressionParser

module rec RealParser : PARSER = struct
  include StatementParser(RealParser)
  include ExpressionParser(RealParser)

  let parse env : Program.t =
    let _ = Parser_env.next_token env in

    let stats = ref [] in

    let rec parse_statements () =
      if Parser_env.peek env <> T_EOF then
        let stat = RealParser.parse_statement env in
        stats := stat::!stats;
        parse_statements()
    in

    parse_statements ();

    Ast.Program.(List.rev !stats)

end

let () =
  let lexbuf = Sedlexing.Latin1.from_string "let a = 323" in
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

  let program = RealParser.parse env in

  AstDump.dump_program program 0
