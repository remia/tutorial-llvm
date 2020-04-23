open Llvm
open Llvm_executionengine
open Llvm_target
open Llvm_scalar_opts

let main () =
  ignore (initialize ());

  (* 1 is the lowest precedence *)
  Hashtbl.add Parser.binop_precedence '<' 10;
  Hashtbl.add Parser.binop_precedence '+' 20;
  Hashtbl.add Parser.binop_precedence '-' 20;
  Hashtbl.add Parser.binop_precedence '*' 40;

  (* first token *)
  print_string "ready> "; flush stdout;
  let stream = Lexer.lex (Stream.of_channel stdin) in

  (* create the JIT *)
  let the_execution_engine = Llvm_executionengine.create Codegen.the_module in
  let the_fpm = PassManager.create_function Codegen.the_module in

  (* setup optimizer pipeline *)
  (* DataLayout.add (ExecutionEngine.target_data the_execution_engine) the_fpm; *)

  (* simple "peephole" optimizations and bit-twiddling optmz *)
  add_instruction_combination the_fpm;

  (* reassociate expressions *)
  add_reassociation the_fpm;

  (* common sub expressions elimination *)
  add_gvn the_fpm;

  (* simplify control flow graph (eg. delete unreachable blocks) *)
  add_cfg_simplification the_fpm;

  ignore (PassManager.initialize the_fpm);

  Toplevel.main_loop the_fpm the_execution_engine stream;

  (* print out all generated code *)
  dump_module Codegen.the_module
;;

main()