module T = Type
module VarMap = Map.Make (String)
module VarSet = Set.Make (String)
module F = Format

(* bv : Bound Type Variables *)
type t = { kt : T.t VarMap.t; bv : VarSet.t; nt : unit -> T.t }

let empty =
  let count = ref 0 in
  {
    kt = VarMap.empty;
    bv = VarSet.empty;
    nt =
      (fun () ->
        count := !count + 1;
        T.Var (Format.sprintf "@%d" !count));
  }

let apply subst ctx = { ctx with kt = VarMap.map (T.apply subst) ctx.kt }

let addbv v ctx =
  match v with
  | T.Var s -> { ctx with bv = VarSet.add s ctx.bv }
  | _ -> failwith "Trying to add a BoundVar that isn't a var..."

let add v t c = { c with kt = VarMap.add v t c.kt }
let new_type ctx = ctx.nt ()
let is_built_in var = String.length var >= 1 && var.[0] = '_'

let built_in ctx = function
  (* Projections... _p_2_1 *)
  | [ ""; "p"; a; b ] ->
      let l = List.init (int_of_string a) (fun _ -> new_type ctx) in
      T.arrow (T.tuple l) (List.nth l (int_of_string b - 1))
  | l -> failwith (F.sprintf "Unknown builtin : %s" (String.concat "_" l))

let find_and_instantiate var ctx =
  (*print_endline
    (Format.sprintf "Instantiate %s in %s" (T.string_of e)
       (string_of_list (BoundVars.to_seq bv |> List.of_seq)));*)
  if is_built_in var then built_in ctx (String.split_on_char '_' var)
  else
    let e = VarMap.find var ctx.kt in
    let s =
      VarSet.diff (VarSet.of_list (T.get_vars e)) ctx.bv
      |> VarSet.to_seq |> List.of_seq
      |> List.map (fun v -> (v, new_type ctx))
    in
    F.printf "INSTANTIATE %s ... %s\n" var (T.string_of_subst s);
    T.apply s e

(* Printing functions *)

let string_of c = VarMap.to_seq c.kt |> List.of_seq |> T.string_of_subst
