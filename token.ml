
type t =
  | T_NUMBER of { kind: number_type; raw: string }
  | T_STRING of (Loc.t * string * string * bool) (* loc, value, raw, octal *)
  | T_TEMPLATE_PART of (Loc.t * template_part * bool) (* loc, value, is_tail *)
  | T_IDENTIFIER of { loc: Loc.t; value: string; raw: string }
  (* Syntax *)
  | T_LCURLY
  | T_RCURLY
  | T_LPAREN
  | T_RPAREN
  | T_LBRACKET
  | T_RBRACKET
  | T_SEMICOLON
  | T_COMMA
  | T_PERIOD
  | T_ARROW
  | T_ELLIPSIS
  | T_AT
  | T_POUND
  (* Keywords *)
  | T_DEF
  | T_FUN
  | T_IF
  | T_THEN
  | T_END
  | T_IN
  | T_INSTANCEOF
  | T_RETURN
  | T_SWITCH
  | T_THROW
  | T_TRY
  | T_WHILE
  | T_WITH
  | T_CONST
  | T_LET
  | T_NULL
  | T_FALSE
  | T_TRUE
  | T_BREAK
  | T_CASE
  | T_CATCH
  | T_CONTINUE
  | T_DEFAULT
  | T_DO
  | T_FINALLY
  | T_FOR
  | T_CLASS
  | T_EXTENDS
  | T_ELSE
  | T_NEW
  | T_TYPEOF
  | T_EXPORT
  | T_IMPORT
  | T_SUPER
  | T_IMPLEMENTS
  | T_INTERFACE
  | T_PACKAGE
  | T_PRIVATE
  | T_PROTECTED
  | T_PUBLIC
  | T_YIELD
  | T_DEBUGGER
  | T_DECLARE
  | T_TYPE
  | T_OF
  | T_ASYNC
  | T_AWAIT
  | T_CHECKS
  (* Operators *)
  | T_ASSIGN
  | T_COLON
  | T_LG_OR
  | T_LG_AND
  | T_EQUAL
  | T_NOT_EQUAL
  | T_LTEQ
  | T_GTEQ
  | T_LT
  | T_GT
  | T_LSHIFT
  | T_RSHIFT
  | T_PLUS
  | T_MINUS
  | T_DIV
  | T_MULT
  | T_EXP
  | T_MOD
  | T_NOT
  | T_BIT_NOT
  | T_INCR
  | T_DECR
  (* Extra tokens *)
  | T_ERROR of string
  | T_EOF
  (* Type primitives *)
  | T_ANY_TYPE
  | T_MIXED_TYPE
  | T_EMPTY_TYPE
  | T_BOOLEAN_TYPE of bool_or_boolean
  | T_NUMBER_TYPE
  | T_NUMBER_SINGLETON_TYPE of { kind: number_type; value: float; raw: string }
  | T_STRING_TYPE
  | T_VOID_TYPE

(* `bool` and `boolean` are equivalent annotations, but we need to track
   which one was used for when it might be an identifier, as in
   `(bool: boolean) => void`. It's lexed as two T_BOOLEAN_TYPEs, then the
   first one is converted into an identifier. *)
and bool_or_boolean =
  | BOOL
  | BOOLEAN

and number_type =
  | BINARY
  | LEGACY_OCTAL
  | OCTAL
  | NORMAL

and template_part = {
  cooked: string; (* string after processing special chars *)
  raw: string; (* string as specified in source *)
  literal: string; (* same as raw, plus characters like ` and ${ *)
}

(*****************************************************************************)
(* Pretty printer (pretty?) *)
(*****************************************************************************)
let token_to_string = function
  | T_NUMBER _ -> "T_NUMBER"
  | T_STRING _ -> "T_STRING"
  | T_TEMPLATE_PART _ -> "T_TEMPLATE_PART"
  | T_IDENTIFIER _ -> "T_IDENTIFIER"
  | T_DEF -> "T_DEF"
  | T_FUN -> "T_FUN"
  | T_IF -> "T_IF"
  | T_THEN -> "T_THEN"
  | T_END -> "T_END"
  | T_IN -> "T_IN"
  | T_INSTANCEOF -> "T_INSTANCEOF"
  | T_RETURN -> "T_RETURN"
  | T_SWITCH -> "T_SWITCH"
  | T_THROW -> "T_THROW"
  | T_TRY -> "T_TRY"
  | T_WHILE -> "T_WHILE"
  | T_WITH -> "T_WITH"
  | T_CONST -> "T_CONST"
  | T_LET  -> "T_LET"
  | T_NULL -> "T_NULL"
  | T_FALSE -> "T_FALSE"
  | T_TRUE -> "T_TRUE"
  | T_BREAK -> "T_BREAK"
  | T_CASE -> "T_CASE"
  | T_CATCH -> "T_CATCH"
  | T_CONTINUE -> "T_CONTINUE"
  | T_DEFAULT -> "T_DEFAULT"
  | T_DO -> "T_DO"
  | T_FINALLY -> "T_FINALLY"
  | T_FOR -> "T_FOR"
  | T_CLASS -> "T_CLASS"
  | T_EXTENDS -> "T_EXTENDS"
  | T_ELSE -> "T_ELSE"
  | T_NEW -> "T_NEW"
  | T_TYPEOF -> "T_TYPEOF"
  | T_EXPORT  -> "T_EXPORT"
  | T_IMPORT -> "T_IMPORT"
  | T_SUPER  -> "T_SUPER"
  | T_IMPLEMENTS -> "T_IMPLEMENTS"
  | T_INTERFACE -> "T_INTERFACE"
  | T_PACKAGE -> "T_PACKAGE"
  | T_PRIVATE -> "T_PRIVATE"
  | T_PROTECTED -> "T_PROTECTED"
  | T_PUBLIC -> "T_PUBLIC"
  | T_YIELD -> "T_YIELD"
  | T_DEBUGGER -> "T_DEBUGGER"
  | T_DECLARE -> "T_DECLARE"
  | T_TYPE -> "T_TYPE"
  | T_OF -> "T_OF"
  | T_ASYNC -> "T_ASYNC"
  | T_AWAIT -> "T_AWAIT"
  | T_CHECKS -> "T_CHECKS"
  | T_LCURLY -> "T_LCURLY"
  | T_RCURLY -> "T_RCURLY"
  | T_LPAREN -> "T_LPAREN"
  | T_RPAREN -> "T_RPAREN"
  | T_LBRACKET -> "T_LBRACKET"
  | T_RBRACKET -> "T_RBRACKET"
  | T_SEMICOLON -> "T_SEMICOLON"
  | T_COMMA -> "T_COMMA"
  | T_PERIOD -> "T_PERIOD"
  | T_ARROW -> "T_ARROW"
  | T_ELLIPSIS -> "T_ELLIPSIS"
  | T_AT -> "T_AT"
  | T_POUND -> "T_POUND"
  | T_ASSIGN -> "T_ASSIGN"
  | T_COLON -> "T_COLON"
  | T_LG_OR -> "T_LG_OR"
  | T_LG_AND -> "T_LG_AND"
  | T_EQUAL -> "T_EQUAL"
  | T_NOT_EQUAL -> "T_NOT_EQUAL"
  | T_LTEQ -> "T_LTEQ"
  | T_GTEQ -> "T_GTEQ"
  | T_LT -> "T_LT"
  | T_GT -> "T_GT"
  | T_LSHIFT -> "T_LSHIFT"
  | T_RSHIFT -> "T_RSHIFT"
  | T_PLUS -> "T_PLUS"
  | T_MINUS -> "T_MINUS"
  | T_DIV -> "T_DIV"
  | T_MULT -> "T_MULT"
  | T_EXP -> "T_EXP"
  | T_MOD -> "T_MOD"
  | T_NOT -> "T_NOT"
  | T_BIT_NOT -> "T_BIT_NOT"
  | T_INCR -> "T_INCR"
  | T_DECR -> "T_DECR"
  (* Extra tokens *)
  | T_ERROR _ -> "T_ERROR"
  | T_EOF -> "T_EOF"
  (* Type primitives *)
  | T_ANY_TYPE -> "T_ANY_TYPE"
  | T_MIXED_TYPE -> "T_MIXED_TYPE"
  | T_EMPTY_TYPE -> "T_EMPTY_TYPE"
  | T_BOOLEAN_TYPE _ -> "T_BOOLEAN_TYPE"
  | T_NUMBER_TYPE -> "T_NUMBER_TYPE"
  | T_NUMBER_SINGLETON_TYPE _ -> "T_NUMBER_SINGLETON_TYPE"
  | T_STRING_TYPE -> "T_STRING_TYPE"
  | T_VOID_TYPE -> "T_VOID_TYPE"

let value_of_token = function
  | T_NUMBER { raw; _ } -> raw
  | T_STRING (_, _, raw, _) -> raw
  | T_TEMPLATE_PART (_, { literal; _ }, _) -> literal
  | T_IDENTIFIER { raw; _ } -> raw
  | T_LCURLY -> "{"
  | T_RCURLY -> "}"
  | T_LPAREN -> "("
  | T_RPAREN -> ")"
  | T_LBRACKET -> "["
  | T_RBRACKET -> "]"
  | T_SEMICOLON -> ";"
  | T_COMMA -> ","
  | T_PERIOD -> "."
  | T_ARROW -> "=>"
  | T_ELLIPSIS -> "..."
  | T_AT -> "@"
  | T_POUND -> "#"
  | T_DEF ->  "def"
  | T_FUN -> "fun"
  | T_IF -> "if"
  | T_THEN -> "then"
  | T_END -> "end"
  | T_IN -> "in"
  | T_INSTANCEOF -> "instanceof"
  | T_RETURN -> "return"
  | T_SWITCH -> "switch"
  | T_THROW -> "throw"
  | T_TRY -> "try"
  | T_WHILE -> "while"
  | T_WITH -> "with"
  | T_CONST -> "const"
  | T_LET -> "let"
  | T_NULL -> "null"
  | T_FALSE -> "false"
  | T_TRUE -> "true"
  | T_BREAK -> "break"
  | T_CASE -> "case"
  | T_CATCH -> "catch"
  | T_CONTINUE -> "continue"
  | T_DEFAULT -> "default"
  | T_DO -> "do"
  | T_FINALLY -> "finally"
  | T_FOR -> "for"
  | T_CLASS -> "class"
  | T_EXTENDS -> "extends"
  | T_ELSE -> "else"
  | T_NEW -> "new"
  | T_TYPEOF -> "typeof"
  | T_EXPORT -> "export"
  | T_IMPORT -> "import"
  | T_SUPER -> "super"
  | T_IMPLEMENTS -> "implements"
  | T_INTERFACE -> "interface"
  | T_PACKAGE -> "package"
  | T_PRIVATE -> "private"
  | T_PROTECTED -> "protected"
  | T_PUBLIC -> "public"
  | T_YIELD -> "yield"
  | T_DEBUGGER -> "debugger"
  | T_DECLARE -> "declare"
  | T_TYPE -> "type"
  | T_OF -> "of"
  | T_ASYNC -> "async"
  | T_AWAIT -> "await"
  | T_CHECKS -> "%checks"
  | T_ASSIGN -> "="
  | T_COLON -> ":"
  | T_LG_OR -> "|"
  | T_LG_AND -> "&"
  | T_EQUAL -> "=="
  | T_NOT_EQUAL -> "!="
  | T_LTEQ -> "<="
  | T_GTEQ -> ">="
  | T_LT -> "<"
  | T_GT -> ">"
  | T_LSHIFT -> "<<"
  | T_RSHIFT -> ">>"
  | T_PLUS -> "+"
  | T_MINUS -> "-"
  | T_DIV -> "/"
  | T_MULT -> "*"
  | T_EXP -> "**"
  | T_MOD -> "%"
  | T_NOT -> "!"
  | T_BIT_NOT -> "~"
  | T_INCR -> "++"
  | T_DECR -> "--"
  | T_ERROR raw -> raw
  | T_EOF -> ""
  | T_ANY_TYPE -> "any"
  | T_MIXED_TYPE -> "mixed"
  | T_EMPTY_TYPE -> "empty"
  | T_BOOLEAN_TYPE kind -> begin match kind with BOOL -> "bool" | BOOLEAN -> "boolean" end
  | T_NUMBER_TYPE -> "number"
  | T_NUMBER_SINGLETON_TYPE { raw; _ } -> raw
  | T_STRING_TYPE -> "string"
  | T_VOID_TYPE -> "void"
