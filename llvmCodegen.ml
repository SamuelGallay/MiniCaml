module L = Llvm
module T = Type
module CC = ClosureConversion
module E = Expr
module VarMap = Map.Make (String)
module F = Format

exception Codegen_Error of string

type callType = Direct | AddCtx | Unpack

let string_of_callType = function Direct -> "Direct" | AddCtx -> "AddCtx" | Unpack -> "Unpack"

(*code generating functions*)
type ctx = {
  m : L.llmodule;
  b : L.llbuilder;
  lCtx : L.llcontext;
  vCtx : L.llvalue VarMap.t;
  ct : callType VarMap.t;
  log : string -> unit;
}

let intType c = L.integer_type c.lCtx 32
let int_of c i = L.const_int (intType c) i
let dataType c = L.pointer_type (L.i8_type c.lCtx)
let dataCast c ?(name = "casted_data") x = L.build_bitcast x (dataType c) name c.b
let pairType c = L.array_type (dataType c) 2
let pairPtrCast c x = L.build_bitcast x (L.pointer_type (pairType c)) "casted_pair" c.b
let nTupType c n = L.array_type (dataType c) n
let nTupPtrCast c n x = L.build_bitcast x (L.pointer_type (nTupType c n)) "casted_ntup" c.b
let closureType c = L.function_type (dataType c) [| dataType c; dataType c |]
let closureCast c x = L.build_bitcast x (L.pointer_type (closureType c)) "to_function" c.b
let directType c = L.function_type (dataType c) [| dataType c |]
let directCast c x = L.build_bitcast x (L.pointer_type (directType c)) "to_direct" c.b

let call_type c = function
  | E.Var (_, v) -> ( match VarMap.find_opt v c.ct with Some b -> b | None -> Unpack)
  | _ -> Unpack

let print_value llv =
  Format.printf "%s of type %s\n" (L.string_of_llvalue llv) (L.type_of llv |> L.string_of_lltype);
  flush_all ()

let rec generate_elementary c ?name expr =
  if not (CC.is_elementary expr) then failwith "Not Elementary"
  else
    match expr with
    | Var (_t, v) -> (
        try VarMap.find v c.vCtx
        with Not_found -> raise (Codegen_Error (F.sprintf "Not finding %s" v)))
    | App (_t, e1, e2) -> (
        let f = generate_elementary c e1 in
        let x = generate_elementary c e2 in
        c.log
        @@ F.sprintf "CallType of %s ? %s\n" (E.string_of e1) (call_type c e1 |> string_of_callType);
        match call_type c e1 with
        | Unpack ->
            (* F.printf "Expression %s in is_captured\n" (E.string_of expr); *)
            (*f is a pair fct ptr + tuple, x is data *)
            (* print_value f; *)
            let pair = pairPtrCast c f in
            (* print_value pair; *)
            let gepf = L.build_gep pair [| int_of c 0; int_of c 0 |] "get_fun_of_closure" c.b in
            let geptup = L.build_gep pair [| int_of c 0; int_of c 1 |] "get_tup_of_closure" c.b in
            let func = L.build_load gepf "fun_of_closure" c.b in
            let tup = L.build_load geptup "tup_of_closure" c.b in
            let name = Option.value ~default:"closed_call" name in
            L.build_call (closureCast c func) [| tup; x |] name c.b
        | AddCtx ->
            (* F.printf "Expression %s in is NOT captured\n" (E.string_of expr); *)
            (*f is a function (ptr?) x is data (ptr to a tuple);  return a pair fct ptr + tuple*)
            let tup = L.build_malloc (pairType c) "init_tup_of_closure" c.b in
            let gepf = L.build_gep tup [| int_of c 0; int_of c 0 |] "set_fun_of_closure" c.b in
            let _ = L.build_store f gepf c.b in
            let gepx = L.build_gep tup [| int_of c 0; int_of c 1 |] "set_tup_of_closure" c.b in
            let _ = L.build_store x gepx c.b in
            let name = Option.value ~default:"datacasted_closure" name in
            dataCast c ~name tup
        | Direct ->
            let name = Option.value ~default:"direct_call" name in
            L.build_call (directCast c f) [| x |] name c.b)
    | Let (x, e1, e2) ->
        let le1 = generate_elementary c ~name:x e1 in
        let c = { c with vCtx = VarMap.add x le1 c.vCtx } in
        generate_elementary c e2
    | Cst i ->
        let int_var = L.build_malloc (intType c) (F.sprintf "myint_%i" i) c.b in
        let _ = L.build_store (int_of c i) int_var c.b in
        let name = Option.value ~default:(F.sprintf "const_%i" i) name in
        dataCast c ~name int_var
    | Tup l ->
        (* ll dataType list*)
        let tup = L.build_malloc (L.array_type (dataType c) (List.length l)) "init_tuple" c.b in
        let f i e =
          let llv = generate_elementary c e in
          let gep = L.build_gep tup [| int_of c 0; int_of c i |] "set_elt_of_tup" c.b in
          let _ = L.build_store llv gep c.b in
          ()
        in
        List.iteri f l;
        let name = Option.value ~default:"tuple" name in
        dataCast c ~name tup
    | Abs _ -> failwith "Nope"

let generate_main c e =
  let fctMain = L.define_function "main" (L.function_type (intType c) [||]) c.m in
  L.position_at_end (L.entry_block fctMain) c.b;
  let _ = generate_elementary c e in
  let _ = L.build_ret (int_of c 0) c.b in
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
      let fct = L.define_function s (directType c) c.m in
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
    let f c s =
      let vCtx = VarMap.add s (generate_built_in c s) c.vCtx in
      let ct = VarMap.add s Direct c.ct in
      { c with vCtx; ct }
    in
    let c = List.fold_left f c (list_builtin expr) in
    let print_int =
      L.declare_function "print_int" (L.function_type (dataType c) [| dataType c |]) c.m
    in
    let vCtx = VarMap.add "print_int" (dataCast c print_int) c.vCtx in
    let ct = VarMap.add "print_int" Direct c.ct in
    let c = { c with vCtx; ct } in
    match expr with
    | E.Let (x, e0, e1) when CC.is_closure e0 ->
        let fctCls = generate_closure c x e0 in
        let vCtx = VarMap.add x fctCls c.vCtx in
        let ct = VarMap.add x AddCtx c.ct in
        generate_llvm_ir { c with vCtx; ct } e1
    | _ -> if CC.is_elementary expr then generate_main c expr else failwith "Not in closure form"

(* ******************** Main ****************************** *)

let main log typed_expr =
  let lCtx = L.create_context () in
  let vCtx = VarMap.empty in
  let ct = VarMap.empty in
  let m = L.create_module lCtx "my_fancy_module" in
  let b = L.builder lCtx in
  L.set_target_triple "x86_64-pc-linux-gnu" m;
  (*L.set_data_layout "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128" m;*)
  generate_llvm_ir { m; b; lCtx; vCtx; ct; log } typed_expr;

  let stringModule = Llvm.string_of_llmodule m in
  stringModule
