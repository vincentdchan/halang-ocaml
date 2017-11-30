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
      | T_WHILE ->
        let stmt = Parser.parse_whilestatement env in
        (While stmt)
      | T_LET ->
        let stmt = Parser.parse_letstatement env in
        (Let stmt)
      | T_BREAK ->
        let _ = Parser_env.next_token env in
        Break
      | T_CONTINUE ->
        let _ = Parser_env.next_token env in
        Continue
      | T_RETURN ->
        let next = Parser_env.next_token env in (
        match next with
        | T_NUMBER _
        | T_STRING _
        | T_IDENTIFIER _
        | T_PLUS | T_MINUS
        | T_LPAREN ->
          let expr = Parser.parse_expression env in
          Return (Some expr)
        | _ -> Return None
        )
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

    let true_branch = ref [] in
    let false_branch = ref [] in

    let make_node () =
      Statement.(
        {If.
          condition = expr;
          true_branch = !true_branch;
          false_branch = !false_branch;
        }
      )
    in

    Parser_env.expect env T_THEN;
    let _ = next_token () in

    let parse_statements () =
      let stmts = ref [] in

      let rec parse_statement' () =
        let tok = peek () in
        if tok <> T_END && tok <> T_ELSE then
          let stmt = Parser.parse_statement env in
          stmts := stmt::!stmts;
          parse_statement' ()
        else ()
      in

      parse_statement' ();

      List.rev !stmts
    in

    true_branch := parse_statements ();

    let following_tok = next_token () in

    Statement.(
      match following_tok with
      | T_END ->
        make_node ()
      | T_ELSE ->
        let fol = peek () in
        (match fol with
          | T_IF ->
            let if_stmt = parse_ifstatement env in
            false_branch := (If if_stmt)::!false_branch;
            make_node ()
          | _ ->
            false_branch := parse_statements ();
            expect T_END;
            let _ = next_token () in
            make_node ()
        )
      | _ ->
        Parser_env.throw_error env "Expected THEN or ELSE";
        failwith "unreachable"
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

      let body' = ref [] in

      let rec parse_statements () =
        let tok = peek () in
        if tok <> T_END then
          let stat = Parser.parse_statement env in
          body' := stat::!body';
          parse_statements ();

      in
      parse_statements ();
      (* parse statements  *)

      expect T_END;
      let _ = next_token () in
      Statement.{While.
        condition;
        body = List.rev !body';
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
