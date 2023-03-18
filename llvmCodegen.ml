module L = Llvm
module T = Type
module CC = ClosureConversion
module E = Expr
module VarMap = Map.Make (String)
module F = Format

exception Codegen_Error of string

(*code generating functions*)
type ctx = { m : L.llmodule; b : L.llbuilder; lCtx : L.llcontext; vCtx : L.llvalue VarMap.t }

let intType c = L.integer_type c.lCtx 64
let int_of c i = L.const_int (intType c) i
let dataType c = L.pointer_type (L.i8_type c.lCtx)
let dataCast c x = L.build_bitcast x (dataType c) "casted_data" c.b
let pairType c = L.array_type (dataType c) 2
let pairPtrCast c x = L.build_bitcast x (L.pointer_type (pairType c)) "casted_pair" c.b
let nTupType c n = L.array_type (dataType c) n
let nTupPtrCast c n x = L.build_bitcast x (L.pointer_type (nTupType c n)) "casted_ntup" c.b
let closureType c = L.function_type (dataType c) [| dataType c; dataType c |]
let closureCast c x = L.build_bitcast x (L.pointer_type (closureType c)) "casted_cls" c.b
let projType c = L.function_type (dataType c) [| dataType c |]

let is_already_captured t =
  (*F.printf "Is captured ? %s\n" (T.string_of t);*)
  match t with T.Fct (T.F.Arrow, [ _; T.Fct (T.F.Arrow, _) ]) -> false | _ -> true

let print_value llv =
  Format.printf "%s of type %s\n" (L.string_of_llvalue llv) (L.type_of llv |> L.string_of_lltype);
  flush_all ()

let rec generate_elementary c expr =
  if not (CC.is_elementary expr) then failwith "Not Elementary"
  else
    match expr with
    | Var (_t, v) -> (
        try VarMap.find v c.vCtx
        with Not_found -> raise (Codegen_Error (F.sprintf "Not finding %s" v)))
    | App (_t, e1, e2) ->
        let f = generate_elementary c e1 in
        let x = generate_elementary c e2 in
        if is_already_captured (E.get_type e1) then
          (* F.printf "Expression %s in is_captured\n" (E.string_of expr); *)
          (*f is a pair fct ptr + tuple, x is data *)
          (* print_value f; *)
          let pair = pairPtrCast c f in
          (* print_value pair; *)
          let gepf = L.build_gep pair [| int_of c 0; int_of c 0 |] "crash" c.b in
          let geptup = L.build_gep pair [| int_of c 0; int_of c 1 |] "geptup" c.b in
          let func = L.build_load gepf "func" c.b in
          let tup = L.build_load geptup "tup" c.b in
          L.build_call (closureCast c func) [| tup; x |] "" c.b
        else
          (* F.printf "Expression %s in is NOT captured\n" (E.string_of expr); *)
          (*f is a function (ptr?) x is data (ptr to a tuple);  return a pair fct ptr + tuple*)
          let tup = L.build_malloc (pairType c) "fct_tup" c.b in
          let gepf = L.build_gep tup [| int_of c 0; int_of c 0 |] "gepf" c.b in
          let _ = L.build_store f gepf c.b in
          let gepx = L.build_gep tup [| int_of c 0; int_of c 1 |] "gepx" c.b in
          let _ = L.build_store x gepx c.b in
          dataCast c tup
    | Let (x, e1, e2) ->
        let le1 = generate_elementary c e1 in
        let c = { c with vCtx = VarMap.add x le1 c.vCtx } in
        generate_elementary c e2
    | Cst i ->
        let int_var = L.build_malloc (intType c) "myint" c.b in
        let _ = L.build_store (int_of c i) int_var c.b in
        dataCast c int_var
    | Tup l ->
        (* ll dataType list*)
        let tup = L.build_malloc (L.array_type (dataType c) (List.length l)) "tup_name" c.b in
        let f i e =
          let llv = generate_elementary c e in
          let gep = L.build_gep tup [| int_of c 0; int_of c i |] "gep" c.b in
          let _ = L.build_store llv gep c.b in
          ()
        in
        List.iteri f l;
        dataCast c tup
    | Abs _ -> failwith "Nope"

let generate_main c e =
  let fctMain = L.define_function "main" (L.function_type (intType c) [||]) c.m in
  L.position_at_end (L.entry_block fctMain) c.b;
  let res = generate_elementary c e in
  let res2 = L.build_bitcast res (L.pointer_type (L.integer_type c.lCtx 64)) "back_to_int" c.b in
  let res3 = L.build_load res2 "res3" c.b in
  let _ =
    match E.get_type e with
    | T.Fct (T.F.Int, []) -> L.build_ret res3 c.b
    | _ -> L.build_ret (int_of c 42) c.b
  in
  ()

let generate_closure c x expr =
  let fctCls = L.define_function x (closureType c) c.m in
  L.position_at_end (L.entry_block fctCls) c.b;
  let tup, arg, e =
    match expr with
    | E.Abs (_, tup, E.Abs (_, arg, e)) when CC.is_elementary e -> (tup, arg, e)
    | _ -> failwith (F.sprintf "I'm not a closure %s" (E.string_of expr))
  in
  let vCtx = c.vCtx |> VarMap.add tup (L.param fctCls 0) |> VarMap.add arg (L.param fctCls 1) in
  let ret_data = generate_elementary { c with vCtx } e in
  let _ = L.build_ret ret_data c.b in
  dataCast c fctCls

let list_builtin e =
  E.list_vars e |> List.sort_uniq String.compare |> List.filter Context.is_built_in

let generate_built_in c s =
  match String.split_on_char '_' s with
  | [ ""; "p"; g; p ] ->
      let fct = L.define_function s (projType c) c.m in
      L.position_at_end (L.entry_block fct) c.b;
      let tup = nTupPtrCast c (int_of_string g) (L.param fct 0) in
      let gep = L.build_gep tup [| int_of c 0; int_of c (int_of_string p - 1) |] "gep" c.b in
      let elt = L.build_load gep "elt" c.b in
      let _ = L.build_ret elt c.b in
      dataCast c fct
  | _ -> raise (Codegen_Error (F.sprintf "Not finding build_in %s" s))

let rec generate_llvm_ir c expr =
  if not (CC.is_in_closure_form expr) then failwith "Not in Closure form"
  else
    let c =
      List.fold_left
        (fun c s -> { c with vCtx = VarMap.add s (generate_built_in c s) c.vCtx })
        c (list_builtin expr)
    in
    match expr with
    | E.Let (x, e0, e1) when CC.is_closure e0 ->
        let fctCls = generate_closure c x e0 in
        generate_llvm_ir { c with vCtx = VarMap.add x fctCls c.vCtx } e1
    | _ -> if CC.is_elementary expr then generate_main c expr else failwith "Not in closure form"

(* ******************** Main ****************************** *)

let main typed_expr =
  let lCtx = L.create_context () in
  let vCtx = VarMap.empty in
  let m = L.create_module lCtx "my_fancy_module" in
  let b = L.builder lCtx in
  L.set_target_triple "x86_64-pc-linux-gnu" m;

  generate_llvm_ir { m; b; lCtx; vCtx } typed_expr;

  let stringModule = Llvm.string_of_llmodule m in
  stringModule

(* ******************** Tests ****************************** *)
(*
let test () =
  let ctx = L.create_context () in
  let m = L.create_module ctx "moduleID" in
  let b = L.builder ctx in

  (* Types *)
  let _doubleType = L.float_type ctx in
  let intType = L.integer_type ctx 32 in
  let int64Type = L.integer_type ctx 64 in
  let ptrType x = L.pointer_type (x ctx) in
  let i8PtrType = ptrType L.i8_type in
  let i32PtrType = ptrType L.i32_type in
  let _voidType = L.void_type ctx in
  let dataType = L.pointer_type (L.i8_type ctx) in
  let idType = L.function_type dataType [| dataType |] in
  let mainType = L.function_type intType [||] in
  let arrayType = L.array_type dataType in

  (* Function declarations *)
  let _fctPrintf =
    L.declare_function "printf" (L.var_arg_function_type intType [| i8PtrType |]) m
  in

  (* Function definitions *)
  let fctId = L.define_function "id" idType m in

  let _ =
    L.position_at_end (L.entry_block fctId) b;
    L.build_ret (L.param fctId 0) b
  in

  (* Main function *)
  let fctMain = L.define_function "main" mainType m in
  let () = L.position_at_end (L.entry_block fctMain) b in

  (*let hello = L.build_global_stringptr "Hello %s %s!\n" "hello" b
    let world = L.build_global_stringptr "World" "world" b
    let _ = L.build_call fctPrintf [| hello; world; world |] "code" b
    let castedResult = L.build_fptosi (llvmize ast_ex) intType "castedResult" b*)
  let ptrFctId = L.build_bitcast fctId dataType "blabla" b in
  let dataCast x = L.build_bitcast x dataType "smth2" b in

  (*let fct_id = L.build_load test "fct_id" b*)
  let id2_data = L.build_call fctId [| ptrFctId |] "id2_data" b in
  let id3 = L.build_bitcast id2_data (L.pointer_type idType) "id3" b in
  let tup = L.build_alloca (arrayType 3) "tup_name" b in
  let _gep = L.build_gep tup [| L.const_int int64Type 0; L.const_int int64Type 2 |] "gep" b in
  let int_var = L.build_alloca intType "myint" b in
  let _ = L.build_store (L.const_int intType 3) int_var b in
  let res = L.build_call id3 [| dataCast int_var |] "res" b in
  let res1 = L.build_bitcast res i32PtrType "" b in
  let res2 = L.build_load res1 "" b in
  let _ = L.build_ret res2 b in
  ()
*)
