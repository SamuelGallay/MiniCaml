module T = Type
module VarMap = Map.Make (String)
module VarSet = Set.Make (String)
module F = Format

let debug = T.debug

(* VarSet.t : ForAll type variables *)
type t = { kt : (T.t * VarSet.t) VarMap.t; nt : unit -> T.t }

let empty =
  let count = ref 0 in
  {
    kt = VarMap.add "print_int" (T.arrow T.tint (T.tuple []), VarSet.empty) VarMap.empty;
    nt =
      (fun () ->
        count := !count + 1;
        T.Var (Format.sprintf "@%d" !count));
  }

let subst_poly ((x : T.var), (s : T.t)) ((t : T.t), (g : VarSet.t)) =
  if VarSet.mem x g then (t, g) else (T.subst_mono s x t, g)

(* apply a substitution right to left *)
(*let apply_poly (s : T.substitution) ((t : T.t), (g : VarSet.t)) : T.t =
  List.fold_right (fun (x, u) -> subst_poly u x) s t
*)

let union = List.fold_left VarSet.union VarSet.empty

let rec free_mono = function
  | T.Fct (_, l) -> union (List.map free_mono l)
  | T.Var s -> VarSet.singleton s

let free_type_vars c =
  c.kt |> VarMap.to_seq |> List.of_seq |> List.map snd
  |> List.map (fun (t, s) -> VarSet.diff (free_mono t) s)
  |> union

let apply_poly = List.fold_right subst_poly
let apply subst ctx = { ctx with kt = VarMap.map (apply_poly subst) ctx.kt }

let add_poly x t c =
  { c with kt = VarMap.add x (t, VarSet.diff (free_mono t) (free_type_vars c)) c.kt }

let add_mono x t c = { c with kt = VarMap.add x (t, VarSet.empty) c.kt }
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
    let e, fav = VarMap.find var ctx.kt in
    let s = fav |> VarSet.to_seq |> List.of_seq |> List.map (fun v -> (v, new_type ctx)) in
    if debug then F.printf "INSTANTIATE %s ... %s\n" var (T.string_of_subst s);
    T.apply_mono s e

(* Printing functions *)
let string_of _c = failwith "TODO..."
(*

let string_of c =
  VarMap.to_seq c.kt |> List.of_seq |> List.map (fun ((a, _), b) -> (a, b)) |> T.string_of_subst*)
