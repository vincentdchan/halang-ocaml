
type position = { line : int; column : int; offset : int; }
type t = { start : position; _end : position; }
val none : t
val btwn : t -> t -> t
val btwn_exclusive : t -> t -> t
val char_before : t -> t
val first_char: t -> t
val contains : t -> t -> bool
val pos_cmp : position -> position -> int
val span_compare : t -> t -> int
val compare : t -> t -> int
val to_string : t -> string
module LocSet : Set.S with type elt = t
