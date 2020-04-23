open Llvm
open Llvm_executionengine

(* top
 *  ::= definition | external | expression | ';'
 *)
let rec main_loop the_fpm the_execution_engine stream =
  match Stream.peek stream with
    | None -> ()

    (* ignore top-level semicolons *)
    | Some (Token.Kwd ';') ->
      Stream.junk stream;
      main_loop the_fpm the_execution_engine stream

    | Some token ->
      begin
        try match token with
          | Token.Def ->
            let e = Parser.parse_definition stream in
            print_endline "parsed a function definition";
            dump_value (Codegen.codegen_func the_fpm e);
          | Token.Extern ->
            let e = Parser.parse_extern stream in
            print_endline "parsed an extern";
            dump_value (Codegen.codegen_proto e);
          | _ ->
            (* top-level expression to anonymous function *)
            let e = Parser.parse_toplevel stream in
            print_endline "parsed a top-level expr";
            let the_function = Codegen.codegen_func the_fpm e in
            dump_value the_function;

            (* JIT the function and returns a function pointer *)
            (* let result = ExecutionEngine.run_function the_function [||]
              the_execution_engine in *)
            let func_name = value_name the_function in
            let func_addr = Llvm_executionengine.get_function_address
              func_name
              (Foreign.funptr Ctypes.(void @-> returning double))
              the_execution_engine
            in
            let result = func_addr () in

            print_string "Evaluated to ";
            print_float (result);
            print_newline ();

        with Stream.Error s | Codegen.Error s ->
          (* skip token for error recovery *)
          Stream.junk stream;
          print_endline s;
      end;
      print_string "ready> "; flush stdout;
      main_loop the_fpm the_execution_engine stream