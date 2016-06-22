type t =
  | Sym of bytes
  | Int of int
  | Float of float
  | Bool of bool
  | Str of bytes
  | Cons of t * t
  | Null

val to_string : t -> bytes

val to_type_string : t -> bytes
