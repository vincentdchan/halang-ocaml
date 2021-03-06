open Ast

module type ASTDUMP = sig
  val dump_program : Program.t -> int -> unit

  val dump_identifier : Identifier.t -> int -> unit
  val dump_string : StringLiteral.t -> int -> unit
  val dump_number : Number.t -> int -> unit

  val dump_expression : Expression.t -> int -> unit
  val dump_assign_expr : Expression.Assign.t -> int -> unit
  val dump_unary_expr : Expression.Unary.t -> int -> unit
  val dump_binary_expr : Expression.Binary.t -> int -> unit
  val dump_call_expr : Expression.Call.t -> int -> unit
  val dump_member_expr : Expression.Member.t -> int -> unit

  val dump_statement : Statement.t -> int -> unit
  val dump_if_stat : Statement.If.t -> int -> unit
  val dump_while_stat : Statement.While.t -> int -> unit
  val dump_let_stat : Statement.Let.t -> int -> unit
  val dump_def_stat : Statement.Def.t -> int -> unit

end

module AstDumpFunctor(Dump : ASTDUMP) = struct

  let rec make_spaces number =
    match number with
    | 0 -> ""
    | _ -> "  " ^ make_spaces (number - 1)

  let dump_stats stats depth =
    List.iter (fun stat ->
      Dump.dump_statement stat depth;
    ) stats;
  ;;

  let op_to_string = Expression.(function
  | O_LG_OR -> "LG_OR"
  | O_LG_AND -> "LG_AND"
  | O_EQUAL -> "EQUAL"
  | O_NOT_EQUAL -> "NOT_EQUAL"
  | O_LTEQ -> "LTEQ"
  | O_GTEQ -> "GTEQ"
  | O_LT -> "LT"
  | O_GT -> "GT"
  | O_LSHIFT -> "LSHIFT"
  | O_RSHIFT -> "RSHIGT"
  | O_PLUS -> "PLUS"
  | O_MINUS -> "MINUS"
  | O_MULT -> "MULT"
  | O_DIV -> "DIV"
  | O_EXP -> "EXP"
  | O_MOD -> "MOD"
  )


  let dump_program prog depth =
    Program.(
      Printf.printf "%sProgram\n" (make_spaces depth);
      dump_stats prog (depth + 1)
    )

  let dump_identifier id depth =
    Identifier.(
      let dump_property depth =
        Printf.printf "%svalue: %s\n" (make_spaces depth) id.value;
        Printf.printf "%sraw: %s\n" (make_spaces depth) id.raw;
      in
      Printf.printf "%sIdentifier\n" (make_spaces depth);
      dump_property (depth + 1)
    )

  let dump_string str_lit depth =
    StringLiteral.(
      let dump_property depth =
        Printf.printf "%svalue: %s\n" (make_spaces depth) str_lit.value;
        Printf.printf "%sraw: %s\n" (make_spaces depth) str_lit.raw;
      in
      Printf.printf "%sString\n" (make_spaces depth);
      dump_property (depth + 1)
    )

  let dump_number num depth =
    Number.(
      let dump_property depth =
        Printf.printf "%svalue: %d\n" (make_spaces depth) num.value;
        Printf.printf "%sraw: %s\n" (make_spaces depth) num.raw;
      in
      Printf.printf "%sNumber\n" (make_spaces depth);
      dump_property (depth + 1)
    )

  let dump_expression expr depth =
    Expression.(match expr with
    | Assign assign -> Dump.dump_assign_expr assign depth
    | Unary unary -> Dump.dump_unary_expr unary depth
    | Binary bin -> Dump.dump_binary_expr bin depth
    | Call call -> Dump.dump_call_expr call depth
    | Member mem -> Dump.dump_member_expr mem depth
    | Identifier id -> dump_identifier id depth
    | StringLiteral str -> dump_string str depth
    | Number num -> dump_number num depth
    )

  let dump_assign_expr expr depth =
    Expression.Assign.(
      let dump_property depth =
        Printf.printf "%sleft:\n" (make_spaces depth);
        Dump.dump_identifier expr.left (depth + 1);
        Printf.printf "%sright:\n" (make_spaces depth);
        Dump.dump_expression expr.right (depth + 1);
      in
      Printf.printf "%sAssignExpression\n" (make_spaces depth);
      dump_property (depth + 1)
    )

  let dump_unary_expr expr depth =
    Expression.Unary.(
      let dump_property depth =
        Printf.printf "%sop: %s\n" (make_spaces depth) Expression.(
          op_to_string expr.op
        );
        Printf.printf "%sbody:\n" (make_spaces depth);
        Dump.dump_expression expr.body (depth + 1);
      in
      Printf.printf "%sUnaryExpression\n" (make_spaces depth);
      dump_property (depth + 1)
    )

  let dump_binary_expr expr depth =
    Expression.Binary.(
      let dump_property depth =
        Printf.printf "%sop: %s\n" (make_spaces depth) Expression.(
          op_to_string expr.op
        );
        Printf.printf "%sleft:\n" (make_spaces depth);
        Dump.dump_expression expr.left (depth + 1);
        Printf.printf "%sright:\n" (make_spaces depth);
        Dump.dump_expression expr.right (depth + 1);
      in
      Printf.printf "%sBinaryExpression\n" (make_spaces depth);
      dump_property (depth + 1)
    )

  let dump_call_expr expr depth =
    Expression.Call.(

      let dump_property depth =

        let rec dump_params params depth =
          match params with
          | [] -> ()
          | param::rest ->
            Dump.dump_expression param depth;
            dump_params rest depth
        in

        Printf.printf "%sbody:\n" (make_spaces depth);
        Dump.dump_expression expr.body (depth + 1);
        Printf.printf "%sparams:\n" (make_spaces depth);
        dump_params expr.params (depth + 1);

      in

      Printf.printf "%sCallExpression\n" (make_spaces depth);
      dump_property (depth + 1)
    )

  let dump_member_expr expr depth =
    Expression.Member.(

      let dump_property depth =
        Printf.printf "%sbody:\n" (make_spaces depth);
        Dump.dump_expression expr.body (depth + 1);
        Printf.printf "%sproperty:\n" (make_spaces depth);
        Dump.dump_expression expr.property (depth + 1);

      in

      Printf.printf "%sMemberExpression\n" (make_spaces depth);
      dump_property (depth + 1)
    )

  let dump_statement stat depth =
    Statement.(
      match stat with
      | If _if -> Dump.dump_if_stat _if depth
      | While _while -> Dump.dump_while_stat _while depth
      | Let _let -> Dump.dump_let_stat _let depth
      | Break ->
        Printf.printf "%sBreakStatement\n" (make_spaces depth);
      | Continue ->
        Printf.printf "%sContinueStatement\n" (make_spaces depth);
      | Return expr ->
        Printf.printf "%sReturnStatement\n" (make_spaces depth);
        (match expr with
        | None ->
          Printf.printf "%sNone\n" (make_spaces (depth + 1));
        | Some expr ->
          Dump.dump_expression expr (depth + 1);
        )
      | Def _def -> Dump.dump_def_stat _def (depth + 1)
      | Expression expr ->
        Printf.printf "%sExpressionStatement\n" (make_spaces depth);
        dump_expression expr (depth + 1)
      | Null ->
        Printf.printf "%sNullStatement\n" (make_spaces depth);
    )

  let dump_if_stat stat depth =
    Statement.If.(

      let dump_property depth =

        Printf.printf "%scondition:\n" (make_spaces depth);
        Dump.dump_expression stat.condition (depth + 1);
        Printf.printf "%strue_branch:\n" (make_spaces depth);
        dump_stats stat.true_branch (depth + 1);
        Printf.printf "%sfalse_branch:\n" (make_spaces depth);
        dump_stats stat.false_branch (depth + 1);

      in

      Printf.printf "%sIfStatement\n" (make_spaces depth);
      dump_property (depth + 1)
    )

  let dump_while_stat stat depth =
    Statement.While.(

      let dump_property depth =
        Printf.printf "%scondition:\n" (make_spaces depth);
        Dump.dump_expression stat.condition (depth + 1);
        Printf.printf "%sbody:\n" (make_spaces depth);
        dump_stats stat.body (depth + 1);
      in

      Printf.printf "%sWhileStatement\n" (make_spaces depth);
      dump_property (depth + 1)
    )

  let dump_let_stat stat depth =
   Statement.Let.(
      let dump_property depth =
        Printf.printf "%sbody:\n" (make_spaces depth);
        List.iter (fun assign ->
          Dump.dump_assign_expr assign (depth + 1);
        ) stat;
      in

      Printf.printf "%sLetStatement\n" (make_spaces depth);
      dump_property (depth + 1)
   )

  let dump_def_stat stat depth =
    Statement.Def.(
      let dump_property depth =
        Printf.printf "%sname:\n" (make_spaces depth);
        Dump.dump_identifier stat.name (depth + 1);

        Printf.printf "%sparams:\n" (make_spaces depth);
        List.iter (fun param ->
          Dump.dump_identifier param (depth + 1)
        ) stat.params;

        Printf.printf "%sbody:\n" (make_spaces depth);
        List.iter (fun stat ->
          Dump.dump_statement stat (depth + 1)
        ) stat.body;

      in

      Printf.printf "%sDefStatement\n" (make_spaces depth);
      dump_property (depth + 1)
    )

end

module rec AstDump : ASTDUMP = struct
  include AstDumpFunctor(AstDump)
end
