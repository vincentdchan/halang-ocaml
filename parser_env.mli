
  type t
  val create : Sedlexing.lexbuf -> t
  val peek : t -> Token.t
  val next_token : t -> Token.t
  val match_token : t -> Token.t -> bool
  val eat : t -> Token.t -> bool
  val expect : t -> Token.t -> unit
  val throw_error : t -> string -> unit

  exception ParsingError of string
