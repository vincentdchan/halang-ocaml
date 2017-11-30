open Ast
open Token
open Parser_common
open StatementParser
open Parser_utils
open ExpressionParser

module rec RealParser : PARSER = struct
  include StatementParser(RealParser)
  include ExpressionParser(RealParser)

  let token_precedence = function
  | T_PLUS | T_MINUS -> 10
  | T_LSHIFT | T_RSHIFT -> 15
  | T_MULT | T_DIV | T_MOD -> 20
  | T_EXP -> 30
  | T_EQUAL
  | T_NOT_EQUAL
  | T_LT | T_LTEQ
  | T_GT | T_GTEQ -> 5
  | _ -> 0

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
  let lexbuf = Sedlexing.Latin1.from_channel stdin in

  let env = Parser_env.create lexbuf
  in

  let program = try
    Some (RealParser.parse env)
  with Parser_env.ParsingError content ->
    Printf.printf "%s\n" content;
    None
  in

  match program with
  | Some prog ->
    AstDump.dump_program prog 0
  | None -> ()
