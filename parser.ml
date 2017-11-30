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
