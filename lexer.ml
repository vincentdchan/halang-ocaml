open Token

let is_keyword = function
  | "def" | "break" | "case" | "catch" | "const" | "continue"
  | "debugger" | "do" | "else" | "export" | "extends"
  | "finally" | "for" | "if" | "import" | "in" | "instanceof"
  | "new" | "return" | "super" | "throw" | "try"
  | "typeof" | "let" | "end" | "while" | "with" | "yield" -> true
  | _ -> false

let lexeme = Sedlexing.Utf8.lexeme

let letter = [%sedlex.regexp? 'a'..'z' | 'A'..'Z' | '_' | '$']
let digit = [%sedlex.regexp? '0'..'9']
let decintlit = [%sedlex.regexp? '0' | ('1'..'9', Star digit)] (* DecimalIntegerLiteral *)
let alphanumeric = [%sedlex.regexp? digit | letter]
let word = [%sedlex.regexp? letter, Star alphanumeric]

let hex_digit = [%sedlex.regexp? digit | 'a'..'f' | 'A'..'F']
let non_hex_letter = [%sedlex.regexp? 'g'..'z' | 'G'..'Z' | '_' | '$']

(* Different ways you can write a number *)
let number = [%sedlex.regexp? Plus digit]
let binnumber = [%sedlex.regexp? '0', ('B' | 'b'), Plus ('0' | '1')]
let octnumber = [%sedlex.regexp? '0', ('O' | 'o'), Plus ('0'..'7')]
let legacyoctnumber = [%sedlex.regexp? '0', Plus ('0'..'7')]
let hexnumber = [%sedlex.regexp? '0', ('X' | 'x'), Plus hex_digit]
let scinumber = [%sedlex.regexp?
  ((decintlit, Opt ('.', Star digit)) | ('.', Plus digit)),
  ('e' | 'E'), Opt ('-' | '+'), Plus digit
]
let wholenumber = [%sedlex.regexp? Plus digit, Opt '.']
let floatnumber = [%sedlex.regexp? Star digit, '.', Plus digit]

(* http://www.ecma-international.org/ecma-262/6.0/#table-32 *)
let whitespace = [%sedlex.regexp?
  0x0009 | 0x000B | 0x000C | 0x0020 | 0x00A0 | 0xfeff |
  0x1680 | 0x180e | 0x2000 .. 0x200a | 0x202f | 0x205f | 0x3000
]

(* minus sign in front of negative numbers
   (only for types! regular numbers use T_MINUS!) *)
let neg = [%sedlex.regexp? '-', Star whitespace]

let line_terminator_sequence = [%sedlex.regexp? '\n' | '\r' | "\r\n" | 0x2028 | 0x2029]

let hex_quad = [%sedlex.regexp? hex_digit, hex_digit, hex_digit, hex_digit]
let unicode_escape = [%sedlex.regexp? "\\u", hex_quad]
let codepoint_escape = [%sedlex.regexp? "\\u{", Plus hex_digit, '}']
let id_start = [%sedlex.regexp? '$' | '_' | id_start | unicode_escape | codepoint_escape]
let id_continue = [%sedlex.regexp?
  '$' | '_' | 0x200C | 0x200D | id_continue | unicode_escape | codepoint_escape
]

let loc_of_offsets env start_offset end_offset =
  { Loc.
    start = { Loc.
      line = Lex_env.line env;
      column = start_offset - Lex_env.bol_offset env;
      offset = start_offset;
    };
    _end = { Loc.
      line = Lex_env.line env;
      column = end_offset - Lex_env.bol_offset env;
      offset = end_offset;
    }
  }

let lex_error (env: Lex_env.t) loc err: Lex_env.t =
  let lex_errors_acc = (loc, err)::env.lex_state.lex_errors_acc in
  { env with lex_state = { env.lex_state with lex_errors_acc; } }

let illegal (env: Lex_env.t) (loc: Loc.t) =
  lex_error env loc "ILLEGAL"

let loc_of_lexbuf env (lexbuf: Sedlexing.lexbuf) =
  let start_offset = Sedlexing.lexeme_start lexbuf in
  let end_offset = Sedlexing.lexeme_end lexbuf in
  loc_of_offsets env start_offset end_offset

let new_line env lexbuf =
  let offset = Sedlexing.lexeme_end lexbuf in
  let lex_bol = Lex_env.{ line = Lex_env.line env + 1; offset; } in
  { env with Lex_env.lex_bol }

type result =
  | Token of Lex_env.t * Token.t
  | Continue of Lex_env.t

let string_escape env lexbuf =
  match%sedlex lexbuf with
  | eof
  | '\\' ->
    let str = lexeme lexbuf in
    let codes = Sedlexing.lexeme lexbuf in
    env, str, codes, false

  | 'x', hex_digit, hex_digit ->
    let str = lexeme lexbuf in
    let code = int_of_string ("0"^str) in (* 0xAB *)
    env, str, [|code|], false

  | '0'..'7', '0'..'7', '0'..'7' ->
    let str = lexeme lexbuf in
    let code = int_of_string ("0o"^str) in (* 0o012 *)
    (* If the 3 character octal code is larger than 256
      * then it is parsed as a 2 character octal code *)
    if code < 256 then
      env, str, [|code|], true
    else
      let remainder = code land 7 in
      let code = code lsr 3 in
      env, str, [|code; Char.code '0' + remainder|], true

  | '0'..'7', '0'..'7' ->
    let str = lexeme lexbuf in
    let code = int_of_string ("0o"^str) in (* 0o01 *)
    env, str, [|code|], true

  | '0' -> env, "0", [|0x0|], false
  | 'b' -> env, "b", [|0x8|], false
  | 'f' -> env, "f", [|0xC|], false
  | 'n' -> env, "n", [|0xA|], false
  | 'r' -> env, "r", [|0xD|], false
  | 't' -> env, "t", [|0x9|], false
  | 'v' -> env, "v", [|0xB|], false
  | '0'..'7' ->
    let str = lexeme lexbuf in
    let code = int_of_string ("0o"^str) in (* 0o1 *)
    env, str, [|code|], true

  | 'u', hex_quad ->
    let str = lexeme lexbuf in
    let hex = String.sub str 1 (String.length str - 1) in
    let code = int_of_string ("0x"^hex) in
    env, str, [|code|], false

  | "u{", Plus hex_digit, '}' ->
    let str = lexeme lexbuf in
    let hex = String.sub str 2 (String.length str - 3) in
    let code = int_of_string ("0x"^hex) in
    (* 11.8.4.1 *)
    let env = if code > 1114111
      then illegal env (loc_of_lexbuf env lexbuf)
      else env
    in
    env, str, [|code|], false

  | 'u' | 'x' | '0'..'7' ->
    let str = lexeme lexbuf in
    let codes = Sedlexing.lexeme lexbuf in
    let env = illegal env (loc_of_lexbuf env lexbuf) in
    env, str, codes, false

  | line_terminator_sequence ->
    let str = lexeme lexbuf in
    let env = new_line env lexbuf in
    env, str, [||], false

  | any ->
    let str = lexeme lexbuf in
    let codes = Sedlexing.lexeme lexbuf in
    env, str, codes, false

  | _ -> failwith "unreachable"


(* Really simple version of string lexing. Just try to find beginning and end of
 * string. We can inspect the string later to find invalid escapes, etc *)
 let rec string_quote env q buf raw octal lexbuf =
  match%sedlex lexbuf with
  | "'" | '"' ->
    let q' = lexeme lexbuf in
    Buffer.add_string raw q';
    if q = q'
    then env, loc_of_lexbuf env lexbuf, octal
    else begin
      Buffer.add_string buf q';
      string_quote env q buf raw octal lexbuf
    end

  | '\\' ->
    Buffer.add_string raw "\\";
    let env, str, codes, octal' = string_escape env lexbuf in
    let octal = octal' || octal in
    Buffer.add_string raw str;
    (* Array.iter (Wtf8.add_wtf_8 buf) codes; *)
    string_quote env q buf raw octal lexbuf

  | '\n' | eof ->
    let x = lexeme lexbuf in
    Buffer.add_string raw x;
    let env = illegal env (loc_of_lexbuf env lexbuf) in
    Buffer.add_string buf x;
    env, loc_of_lexbuf env lexbuf, octal

  | any ->
    let x = lexeme lexbuf in
    Buffer.add_string raw x;
    Buffer.add_string buf x;
    string_quote env q buf raw octal lexbuf

  | _ -> failwith "unreachable"

let token (env: Lex_env.t) lexbuf : result =
  let letter = [%sedlex.regexp? 'a'..'z'|'A'..'Z'] in
  match%sedlex lexbuf with
  | line_terminator_sequence ->
    let env = new_line env lexbuf in
    Continue env

  | number -> Token (env , T_NUMBER {
      kind = BINARY;
      raw =(Sedlexing.Latin1.lexeme lexbuf);
    })

  | id_start, Star (id_continue) ->
    let content = Sedlexing.Latin1.lexeme lexbuf in
    if is_keyword content then
      match content with
      | "def" -> Token (env, T_DEF)
      | "break" -> Token (env, T_BREAK)
      | "case" -> Token (env, T_CASE)
      | "catch" -> Token (env, T_CATCH)
      | "const" -> Token (env, T_CONST)
      | "continue" -> Token (env, T_CONTINUE)
      | "debugger" -> Token (env, T_DEBUGGER)
      | "do" -> Token (env, T_DO)
      | "else" -> Token (env, T_ELSE)
      | "export" -> Token (env, T_EXPORT)
      | "extends" -> Token (env, T_EXTENDS)
      | "finally" -> Token (env, T_FINALLY)
      | "for" -> Token (env, T_FOR)
      | "if" -> Token (env, T_IF)
      | "import" -> Token (env, T_IMPORT)
      | "in" -> Token (env, T_IN)
      | "instanceof" -> Token (env, T_INSTANCEOF)
      | "new" -> Token (env, T_NEW)
      | "return" -> Token (env, T_RETURN)
      | "super" -> Token (env, T_SUPER)
      | "throw" -> Token (env, T_THROW)
      | "try" -> Token (env, T_TRY)
      | "typeof" -> Token (env, T_TYPEOF)
      | "let" -> Token (env, T_LET)
      | "end" -> Token (env, T_END)
      | "while" -> Token (env, T_WHILE)
      | "with" -> Token (env, T_WITH)
      | "yield" -> Token (env, T_YIELD)
    else
      Token(env, T_IDENTIFIER { loc = loc_of_lexbuf env lexbuf ; value = content; raw = content})

  (* Values *)
  | "'" | '"' ->
    let quote = lexeme lexbuf in
    let start = loc_of_lexbuf env lexbuf in
    let buf = Buffer.create 127 in
    let raw = Buffer.create 127 in
    Buffer.add_string raw quote;
    let octal = false in
    let env, _end, octal = string_quote env quote buf raw octal lexbuf in
    let loc = Loc.btwn start _end in
    Token (env, T_STRING (loc, Buffer.contents buf, Buffer.contents raw, octal))

  | '(' -> Token (env, T_LPAREN)
  | ')' -> Token (env, T_RPAREN)

  | '=' -> Token (env, T_ASSIGN)
  | ':' -> Token (env, T_COLON)
  | '|' -> Token (env, T_LG_OR)
  | '&' -> Token (env, T_LG_AND)
  | "==" -> Token (env, T_EQUAL)
  | "!=" -> Token (env, T_NOT_EQUAL)
  | "<=" -> Token (env, T_LTEQ)
  | ">=" -> Token (env, T_GTEQ)
  | '>' -> Token (env, T_GT)
  | '<' -> Token (env, T_LT)
  | "<<" -> Token (env, T_LSHIFT)
  | ">>" -> Token (env, T_RSHIFT)
  | '+' -> Token (env, T_PLUS)
  | '-' -> Token (env, T_MINUS)
  | '*' -> Token (env, T_MULT)
  | '/' -> Token (env, T_DIV)
  | 128 .. 255 -> Continue env
  | eof -> Token (env, T_EOF)
  | Star whitespace -> Continue env
  | _ -> failwith "unreachable"
