open Ast
open Token
open Parser_common

module ExpressionParser (Parser : PARSER) : sig
  val parse_identifier : Parser_env.t -> Identifier.t
  val parse_string : Parser_env.t -> StringLiteral.t
  val parse_number : Parser_env.t -> Number.t
  val parse_expression : Parser_env.t -> Expression.t
  val parse_assign_expr : Parser_env.t -> Identifier.t option -> Expression.Assign.t
  val parse_unary_expr : Parser_env.t -> Expression.Unary.t
  (* val parse_binary_expr : Parser_env.t -> Expression.Binary.t
  val parse_call_expr : Parser_env.t -> Expression.Call.t
  val parse_member_expr : Parser_env.t -> Expression.Member.t *)
end = struct

  let parse_identifier env : Identifier.t =
    let current_tok = Parser_env.peek env in
      Expression.(
        match current_tok with
        | T_IDENTIFIER content ->
          let _ = Parser_env.next_token env in
          {Identifier.
            value = content.value;
            raw = content.raw;
          }
        | _ -> failwith ("unreachable")
      )

  let parse_number env : Number.t =
    let current_tok = Parser_env.peek env in
      Expression.(
        match current_tok with
        | T_NUMBER raw ->
          let _ = Parser_env.next_token env in
          {Number.
            value = 3;
            raw = "ok";
          }
        | _ -> failwith ("unreachable")
      )

  let parse_string env : StringLiteral.t =
    let current_tok = Parser_env.peek env in
    Expression.(
      match current_tok with
      | T_STRING (_, content, raw, _) ->
        let _ = Parser_env.next_token env in
        {StringLiteral.
          value = content;
          raw = raw;
        }
      | _ -> failwith ("unreachable")
    )

  let parse_assign_expr env opId : Expression.Assign.t =
    let id_tok = T_IDENTIFIER {
      loc = Loc.none;
      value = "";
      raw = "";
    } in
    let refId = ref None in

    let make_node_with_id id =
      Parser_env.expect env T_ASSIGN;

      let expr = Parser.parse_expression env in

      Expression. {Assign.
        left = id;
        right = expr;
      }
    in

    match opId with
    | None ->
      Parser_env.expect env id_tok;
      let id = parse_identifier env in
      make_node_with_id id
    | Some id ->
      make_node_with_id id

  let parse_unary_expr env : Expression.Unary.t =
    let current_tok = Parser_env.peek env in

    let go_with_op op : Expression.Unary.t =
      let _ = Parser_env.next_token env in
      let expr = Parser.parse_expression env in
      Expression. {Unary.
        op;
        body = expr;
      }
    in

    Expression.(
      match current_tok with
      | T_PLUS -> (go_with_op Plus)
      | T_MINUS -> (go_with_op Minus)
      | T_MULT -> (go_with_op Mult)
      | T_DIV -> (go_with_op Div)
      | T_EXP -> (go_with_op Exp)
      | _ -> failwith "unreachable"
    )

  let parse_expression env : Expression.t =
    let current_tok = Parser_env.peek env in
    Expression.(
      match current_tok with
      | T_NUMBER raw ->
        let _ = Parser_env.next_token env in
        Number (parse_number env);
      | T_STRING (_, content, raw, _) ->
        let _ = Parser_env.next_token env in
        StringLiteral (parse_string env);
      | T_IDENTIFIER content ->
        let _ = Parser_env.next_token env in
        Identifier (parse_identifier env);
      | _ -> failwith "unreachable"
    )

end
