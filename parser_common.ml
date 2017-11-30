open Ast

module type PARSER = sig
  val token_precedence : Token.t -> int

  val parse : Parser_env.t -> Program.t
  val parse_identifier : Parser_env.t -> Identifier.t
  val parse_string : Parser_env.t -> StringLiteral.t
  val parse_number : Parser_env.t -> Number.t
  val parse_statement : Parser_env.t -> Statement.t
  val parse_ifstatement : Parser_env.t -> Statement.If.t
  val parse_whilestatement : Parser_env.t -> Statement.While.t
  val parse_letstatement : Parser_env.t -> Statement.Let.t
  val parse_defstatement : Parser_env.t -> Statement.Def.t

  val parse_expression : Parser_env.t -> Expression.t
  val parse_maybe_unary : Parser_env.t -> Expression.t
  (* val parse_unary_expr : Parser_env.t -> Expression.Unary.t *)
  val parse_expression_unit : Parser_env.t -> Expression.t
  val parse_assign_expr : Parser_env.t -> Identifier.t option -> Expression.Assign.t
  val parse_call_expr : Parser_env.t -> Expression.t -> Expression.Call.t
  val parse_member_expr : Parser_env.t -> Expression.t -> Expression.Member.t
  val parse_call_assign_member_expr : Parser_env.t -> Expression.t

  val parse_binary_expr : Parser_env.t -> Expression.t -> Token.t -> Expression.Binary.t
end
