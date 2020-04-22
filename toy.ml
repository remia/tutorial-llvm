open Llvm

let main () =
  (* 1 is the lowest precedence *)
  Hashtbl.add Parser.binop_precedence '<' 10;
  Hashtbl.add Parser.binop_precedence '+' 20;
  Hashtbl.add Parser.binop_precedence '-' 20;
  Hashtbl.add Parser.binop_precedence '*' 40;

  (* first token *)
  print_string "ready> "; flush stdout;
  let stream = Lexer.lex (Stream.of_channel stdin) in

  Toplevel.main_loop stream;

  (* print out all generated code *)
  dump_module Codegen.the_module
;;

main()