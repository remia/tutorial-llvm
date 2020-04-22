type expr =
  (* variant for numeric literals like "1.0" *)
  | Number of float

  (* variant for referencing a variable, like "a" *)
  | Variable of string

  (* variant for a binary operator *)
  | Binary of char * expr * expr

  (* variant for function calls *)
  | Call of string * expr array

(* captures the name and arguments name of the function *)
type proto = Prototype of string * string array

(* function definition *)
type func = Function of proto * expr