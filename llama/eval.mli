exception Undefined_variable of bytes
exception Arity_mismatch of bytes
exception Invalid_arg_type of bytes
exception Expected_procedure of bytes

val eval : Sxp.t -> Sxp.t
