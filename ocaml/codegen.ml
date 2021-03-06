open Llvm

exception Error of string

let context = global_context ()
let the_module = create_module context "my cool jit"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context

let rec codegen_expr = function
  | Ast.Number n -> const_float double_type n
  | Ast.Variable name ->
    (try Hashtbl.find named_values name with
      | Not_found -> raise (Error "unknown variable name"))
  | Ast.Binary (op, lhs, rhs) ->
    let lhs_val = codegen_expr lhs in
    let rhs_val = codegen_expr rhs in
    begin
      match op with
        | '+' -> build_fadd lhs_val rhs_val "addtmp" builder
        | '-' -> build_fsub lhs_val rhs_val "subtmp" builder
        | '*' -> build_fmul lhs_val rhs_val "multmp" builder
        | '<' ->
          (* convert bool 0 / 1 to double 0.0 / 1.0 *)
          let i = build_fcmp Fcmp.Ult lhs_val rhs_val "cmptmp" builder in
          build_uitofp i double_type "booltmp" builder
        | _ -> raise (Error "invalid binary operator")
    end
  | Ast.Call (callee, args) ->
    (* look up the name in the module table *)
    let callee =
      match lookup_function callee the_module with
        | Some callee -> callee
        | None -> raise (Error "unknown function referenced")
      in
      let params = params callee in

      (* if argument mismatch error *)
      if Array.length params == Array.length args then () else
        raise (Error "incorrect # arguments passed");
      let args = Array.map codegen_expr args in
      build_call callee args "calltmp" builder

let codegen_proto = function
  | Ast.Prototype (name, args) ->
      (* make the function type: double(double, double) etc. *)
      let doubles = Array.make (Array.length args) double_type in
      let ft = function_type double_type doubles in
      let f =
        match lookup_function name the_module with
          | None -> declare_function name ft the_module
          (* there is already something named `name` *)
          | Some f ->
            (* already has a body *)
            if block_begin f <> At_end f then
              raise (Error "redefinition of function");

            if element_type (type_of f) <> ft then
              raise (Error "redefinition of function with different # args");
            f
      in

      (* set names for all arguments *)
      Array.iteri (fun i a ->
        let n = args.(i) in
        set_value_name n a;
        Hashtbl.add named_values n a;
      ) (params f);
      f

let codegen_func the_fpm = function
  | Ast.Function (proto, body) ->
    Hashtbl.clear named_values;
    let the_function = codegen_proto proto in

    (* create a new basic block *)
    let bb = append_block context "entry" the_function in
    position_at_end bb builder;

    try
      let ret_val = codegen_expr body in
      let _ = build_ret ret_val builder in

      (* code consistency checks on the function *)
      Llvm_analysis.assert_valid_function the_function;

      (* optimization passes on the function *)
      let _ = PassManager.run_function the_function the_fpm in

      the_function
    with e ->
      delete_function the_function;
      raise e
