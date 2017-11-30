open Ast
open Token
open Parser_common

module ExpressionParser (Parser : PARSER) : sig
  val parse_identifier : Parser_env.t -> Identifier.t
  val parse_string : Parser_env.t -> StringLiteral.t
  val parse_number : Parser_env.t -> Number.t
  val parse_expression : Parser_env.t -> Expression.t
  (* val parse_unary_expr : Parser_env.t -> Expression.Unary.t *)
  val parse_maybe_unary : Parser_env.t -> Expression.t
  val parse_expression_unit : Parser_env.t -> Expression.t
  val parse_assign_expr : Parser_env.t -> Identifier.t option -> Expression.Assign.t
  val parse_call_expr : Parser_env.t -> Expression.t -> Expression.Call.t
  val parse_member_expr : Parser_env.t -> Expression.t -> Expression.Member.t
  val parse_call_assign_member_expr : Parser_env.t -> Expression.t

  val parse_binary_expr : Parser_env.t -> Expression.t -> Token.t -> Expression.t
end = struct

  let id_tok = T_IDENTIFIER {
    loc = Loc.none;
    value = "";
    raw = "";
  }

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
        | _ ->
          Parser_env.throw_error env "Expect IDENTIFIER";
          failwith ("unreachable")
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
        | _ ->
          Parser_env.throw_error env "Expect NUMBER";
          failwith ("unreachable")
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
      | _ ->
        Parser_env.throw_error env "Expect STRING";
        failwith ("unreachable")
    )

  let parse_assign_expr env opId : Expression.Assign.t =

    let make_node_with_id id =
      Parser_env.expect env T_ASSIGN;

      let _ = Parser_env.next_token env in

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

  let parse_maybe_unary env : Expression.t =
    let current_tok = Parser_env.peek env in

    if (Parser.token_precedence current_tok) > 0 then
      let _ = Parser_env.next_token env in

      let op = Expression.(match current_tok with
        | T_PLUS -> O_PLUS
        | T_MINUS -> O_MINUS
        | _ ->
          Parser_env.throw_error env "Expect PLUS or MINUS";
          failwith "unreachable"
      )
      in

      Expression.(Unary {Unary.
        op;
        body = Parser.parse_expression_unit env;
      })

    else
      Parser.parse_expression_unit env

  let parse_expression_unit env : Expression.t =
    let current_tok = Parser_env.peek env in
    let next_token () = Parser_env.next_token env in
    let expect = Parser_env.expect env in
    match current_tok with
    | T_LPAREN ->
      let _ =next_token () in
      let expr = Parser.parse_expression env in
      expect T_LPAREN;
      let _ = next_token () in
      expr
    | T_IDENTIFIER _ ->
      Parser.parse_call_assign_member_expr env
    (* TODO: list expression  *)
    | T_NUMBER _ ->
      Expression.(Number (Parser.parse_number env))
    | T_STRING _ ->
      Expression.(StringLiteral (Parser.parse_string env))
    | _ ->
      Parser_env.throw_error env "Unexpected expression unit";
      failwith "unreachable"

  let parse_expression env : Expression.t =
    let left = parse_maybe_unary env in

    let tok = Parser_env.peek env in

    if (Parser.token_precedence tok) > 0 then
      let next_tok = Parser_env.next_token env in

      Parser.parse_binary_expr env left next_tok
    else
      left

  let parse_call_expr env expr : Expression.Call.t =
    let params = ref [] in
    Parser_env.expect env T_LPAREN;
    let _ = Parser_env.next_token env in

    let rec parse_params () =
      if Parser_env.peek env <> T_RPAREN then
        let param = Parser.parse_expression env in
        params := param::!params;
        if Parser_env.peek env = T_COMMA then
          let _ = Parser_env.next_token env in
          parse_params ();
        else if Parser_env.peek env <> T_RPAREN then
          Parser_env.throw_error env "Expect COMMA or RPAREN"
        else ()
    in

    parse_params ();

    Parser_env.expect env T_RPAREN;
    let _ = Parser_env.next_token env in
    Expression. {Call.
      body = expr;
      params = List.rev !params;
    }

  let parse_member_expr env expr : Expression.Member.t =
    Parser_env.expect env T_LBRACKET;
    let _ = Parser_env.next_token env in
    Parser_env.expect env T_RBRACKET;
    let _ = Parser_env.next_token env in
    let property = Parser.parse_expression env in
    Expression. {Member.
      body = expr;
      property;
    }

  let parse_call_assign_member_expr env : Expression.t =
    Parser_env.expect env id_tok;
    let id = Parser.parse_identifier env in
    let next_tok = Parser_env.peek env in
    Expression.(
      match next_tok with
      | T_ASSIGN ->
          Assign (Parser.parse_assign_expr env (Some id))
      | T_LPAREN ->
          Call (Parser.parse_call_expr env (Identifier id))
      | T_LBRACKET ->
          Member (Parser.parse_member_expr env (Identifier id))
      | _ ->
        Identifier id
    )

  let rec parse_binary_expr env left_expr left_tok: Expression.t =
    let expr = (parse_maybe_unary env) in

    let token_to_op = Expression.(function
      | T_LG_OR -> O_LG_OR
      | T_LG_AND -> O_LG_AND
      | T_EQUAL -> O_EQUAL
      | T_NOT_EQUAL -> O_NOT_EQUAL
      | T_LTEQ -> O_LTEQ
      | T_GTEQ -> O_GTEQ
      | T_LT -> O_LT
      | T_GT -> O_GT
      | T_LSHIFT -> O_LSHIFT
      | T_RSHIFT -> O_RSHIFT
      | T_PLUS -> O_PLUS
      | T_MINUS -> O_MINUS
      | T_MULT -> O_MULT
      | T_DIV -> O_DIV
      | T_EXP -> O_EXP
      | T_MOD -> O_MOD
      | _ -> failwith "unknown operator"
    )
    in

    let rec loop exp : Expression.t =

      let right_tok = Parser_env.peek env in

      let left_precedence = Parser.token_precedence left_tok in
      let right_precedence = Parser.token_precedence right_tok in

      if left_precedence < right_precedence then

        let _ = Parser_env.next_token env in
        let exp = Parser.parse_binary_expr env expr right_tok
        in
        loop exp
      else if left_precedence = right_precedence then
      (
        if left_precedence = 0 then
          exp
        else
          let exp = Expression.(Binary {Binary.
            op = token_to_op left_tok;
            left = left_expr;
            right = expr;
          })
          in
          let _ = Parser_env.next_token env in
          parse_binary_expr env exp right_tok
      )
      else
        Expression.(Binary {Binary.
          op = token_to_op left_tok;
          left = left_expr;
          right = exp;
        })

    in

    loop expr


end
