open Ast

module type PARSER = sig
  val parse : Parser_env.t -> Program.t
  val parse_identifier : Parser_env.t -> Identifier.t
  val parse_string : Parser_env.t -> StringLiteral.t
  val parse_number : Parser_env.t -> Number.t
  val parse_statement : Parser_env.t -> Statement.t
  val parse_ifstatement : Parser_env.t -> Statement.If.t
  val parse_whilestatement : Parser_env.t -> Statement.While.t
  val parse_letstatement : Parser_env.t -> Statement.Let.t
  val parse_expression : Parser_env.t -> Expression.t
  val parse_assign_expr : Parser_env.t -> Identifier.t option -> Expression.Assign.t
  (* val parse_unary_expr : Parser_env.t -> Expression.Unary.t
  val parse_binary_expr : Parser_env.t -> Expression.Binary.t
  val parse_call_expr : Parser_env.t -> Expression.Call.t
  val parse_member_expr : Parser_env.t -> Expression.Member.t *)
end
