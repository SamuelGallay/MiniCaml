open Types
module L = Llvm

let ast_ex = Bin (Sum, Float 12.3, Float 15.7)
let ctx = L.create_context ()
let m = L.create_module ctx "moduleID"
let b = L.builder ctx

(* Types *)
let doubleType = L.float_type ctx
let intType = L.integer_type ctx 32
let ptrType x = L.pointer_type (x ctx)
let i8PtrType = ptrType L.i8_type
let i32PtrType = ptrType L.i32_type
let voidType = L.void_type ctx
let dataType = L.pointer_type (L.i8_type ctx)
let idType = L.function_type dataType [| dataType |]
let mainType = L.function_type intType [||]

(*code generating functions*)
let rec llvmize = function
  | Float f -> L.const_float doubleType f
  | Bin (Sum, a, c) -> L.build_fadd (llvmize a) (llvmize c) "addtmp" b
  | Bin (Prod, a, c) -> L.build_mul (llvmize a) (llvmize c) "multmp" b

(* Function declarations *)
let fctPrintf = L.declare_function "printf" (L.var_arg_function_type intType [| i8PtrType |]) m

(* Function definitions *)
let fctId = L.define_function "id" idType m

let _ =
  L.position_at_end (L.entry_block fctId) b;
  L.build_ret (L.param fctId 0) b

(* Main function *)
let fctMain = L.define_function "main" mainType m
let () = L.position_at_end (L.entry_block fctMain) b

(*let hello = L.build_global_stringptr "Hello %s %s!\n" "hello" b
  let world = L.build_global_stringptr "World" "world" b
  let _ = L.build_call fctPrintf [| hello; world; world |] "code" b
  let castedResult = L.build_fptosi (llvmize ast_ex) intType "castedResult" b*)
let ptrFctId = L.build_bitcast fctId dataType "blabla" b
let dataCast x = L.build_bitcast x dataType "smth2" b

(*let fct_id = L.build_load test "fct_id" b*)

let id2_data = L.build_call fctId [| ptrFctId |] "id2_data" b
let id3 = L.build_bitcast id2_data (L.pointer_type idType) "id3" b
let int_var = L.build_alloca intType "myint" b
let _ = L.build_store (L.const_int intType 3) int_var b
let res = L.build_call id3 [| dataCast int_var |] "res" b
let res1 = L.build_bitcast res i32PtrType "" b
let res2 = L.build_load res1 "" b
let _ = L.build_ret res2 b

let main () =
  let stringModule = Llvm.string_of_llmodule m in
  Printf.printf "\x1B[31m# Output LLVM IR :\x1B[0m\n%s" stringModule;
  let file = open_out "main.ll" in
  Printf.fprintf file "%s" stringModule;
  close_out file;
  Printf.printf "\x1B[31m# Compiling using Clang :\x1B[0m\nReturn code : %i\n"
    (Sys.command "clang main.ll");
  Printf.printf "\x1B[31m# Executing the compiled program: :\x1B[0m\n";
  flush stdout;
  let return_code = Sys.command "./a.out" in
  Printf.printf "Return code : %i\n" return_code
