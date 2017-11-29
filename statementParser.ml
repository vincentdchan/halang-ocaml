open Ast
open Token
open Parser_common

module StatementParser (Parser : PARSER) : sig
  val parse_statement : Parser_env.t -> Statement.t
  val parse_ifstatement : Parser_env.t -> Statement.If.t
  val parse_whilestatement : Parser_env.t -> Statement.While.t
  val parse_letstatement : Parser_env.t -> Statement.Let.t
end = struct

  let parse_statement env : Statement.t =
    let peek () = Parser_env.peek env in
    let current_tok = peek () in
    Statement.(
      match current_tok with
      | T_IF ->
        let stmt = Parser.parse_ifstatement env in
        (If stmt)
      | _ ->
        let expr = Parser.parse_expression env in
        (Expression expr)
    )

  let rec parse_ifstatement env : Ast.Statement.If.t =
    let expect = Parser_env.expect env in
    let peek () = Parser_env.peek env in
    let next_token () : Token.t =
      Parser_env.next_token env
    in

    Parser_env.expect env T_IF;
    let _ = Parser_env.next_token env in
    let expr = Parser.parse_expression env in

    let true_branch' = ref [] in
    let false_branch' = ref [] in

    let make_node () =
      Statement.(
        {If.
          condition = expr;
          true_branch = List.rev !true_branch';
          false_branch = List.rev !false_branch';
        }
      )
    in

    Parser_env.expect env T_THEN;
    let _ = Parser_env.next_token env in
    (* TODO: parse_statement  *)

    let following_tok = Parser_env.next_token env in

    Statement.(
      match following_tok with
      | T_END ->
        let _ = next_token () in
        make_node ()
      | T_ELSE ->
        let _ = next_token () in
        let fol = peek () in
        match fol with
        | T_IF ->
          let if_stmt = parse_ifstatement env in
          false_branch' := (If if_stmt)::!false_branch';
          make_node ()
        | _ ->
          (* TODO parse statement  *)
          expect T_END;
          let _ = next_token () in
          make_node ()
      | _ -> failwith "unreachable"
    )

    let parse_whilestatement env : Statement.While.t =
      let expect = Parser_env.expect env in
      let peek () = Parser_env.peek env in
      let next_token () : Token.t =
        Parser_env.next_token env
      in
      expect T_WHILE;
      let _ = next_token () in
      let condition = Parser.parse_expression env in

      expect T_DO;
      let _ = next_token () in

      (* parse statements  *)

      expect T_END;
      let _ = next_token () in
      Statement.{While.
        condition;
        body = [];
      }

    let parse_letstatement env : Statement.Let.t =
      let expect = Parser_env.expect env in
      let peek () = Parser_env.peek env in
      let next_token () : Token.t =
        Parser_env.next_token env
      in
      expect T_LET;
      let _ = next_token () in
      let id_tok = T_IDENTIFIER {
        loc = Loc.none;
        value = "";
        raw = "";
      } in

      expect id_tok;

      let rec parse_assign () : Expression.Assign.t list =
        if Parser_env.match_token env id_tok then
          let assign_expr = Parser.parse_assign_expr env None in
          assign_expr::parse_assign ()
        else
          []
      in

      let body = parse_assign() in

      Statement.Let.(body)

end
