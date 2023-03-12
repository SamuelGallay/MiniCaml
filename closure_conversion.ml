module E = Expr
module VarSet = Set.Make (String)
module F = Format
module T = Type

let debug = T.debug

let rec free_vars bound_vars = function
  | E.Var (_, v) -> VarSet.diff (VarSet.singleton v) bound_vars
  | App (_, e0, e1) -> VarSet.union (free_vars bound_vars e0) (free_vars bound_vars e1)
  | Abs (_, x, e) -> VarSet.diff (free_vars bound_vars e) (VarSet.singleton x)
  | Cst _ -> VarSet.empty
  | Let (x, e0, e1) ->
      VarSet.union (free_vars bound_vars e0)
        (VarSet.diff (free_vars bound_vars e1) (VarSet.singleton x))
  | Tup l -> List.map (free_vars bound_vars) l |> List.fold_left VarSet.union VarSet.empty

let is_closed bv e =
  VarSet.filter (fun v -> not (Context.is_built_in v)) (free_vars bv e) = VarSet.empty

let rec is_elementary = function
  | E.Var _ | Cst _ -> true
  | Abs _ -> false
  | App (_, e0, e1) | Let (_, e0, e1) -> is_elementary e0 && is_elementary e1
  | Tup l -> List.for_all is_elementary l

let is_closure = function
  | E.Abs (_, _, Abs (_, _, e)) -> is_elementary e
  | Abs (_, _, e) -> is_elementary e
  | _ -> false

let rec is_in_closure_form bv e =
  is_elementary e
  ||
  match e with
  | E.Let (x, e0, e1) -> is_closure e0 && is_in_closure_form (VarSet.add x bv) e1
  | _ -> false

let new_fct_symb =
  let i = ref 0 in
  fun () ->
    incr i;
    F.sprintf "b_f%i" !i

let new_var_symb =
  let i = ref 0 in
  fun () ->
    incr i;
    F.sprintf "b_t%i" !i

let subst_of_tuple var_tuple = function
  | E.Tup l ->
      let f i var =
        let t, v = match var with E.Var (t, v) -> (t, v) | _ -> failwith "Not a Var" in
        let proj = F.sprintf "_p_%i_%i" (List.length l) (i + 1) in
        (v, E.App (t, E.Var (t, proj), E.Var (t, var_tuple)))
        (* Bullshit Types *)
      in
      List.mapi f l
  | _ -> failwith "Not a Tuple"

let apply l e = List.fold_right E.map_vars l e

let rec transform bv exp =
  match exp with
  | E.Var _ | Cst _ -> ([], exp)
  | Abs (t, x, e1) ->
      let f = new_fct_symb () in
      let l, ce1 = transform bv e1 in
      let fv =
        VarSet.diff (free_vars (VarSet.add x bv) ce1) (l |> List.map fst |> VarSet.of_list)
        |> VarSet.to_seq |> List.of_seq
      in
      if debug then Format.printf "Free vars in %s : %s\n" (E.string_of exp) (String.concat ", " fv);
      let tup = E.Tup (List.map (fun v -> E.Var (t, v)) fv) in
      (*Bullshit type*)
      let t' = T.arrow (E.get_type tup) t in
      let tupvar = new_var_symb () in
      let subst = subst_of_tuple tupvar tup in
      let new_closure = E.Abs (t', tupvar, E.Abs (t, x, ce1)) in
      (l @ [ (f, apply subst new_closure) ], App (t, Var (t', f), tup))
  | App (t, e0, e1) ->
      let l0, ce0 = transform bv e0 in
      let l1, ce1 = transform bv e1 in
      (l0 @ l1, App (t, ce0, ce1))
  | Tup l ->
      let lp = List.map (transform bv) l in
      (List.concat_map fst lp, Tup (List.map snd lp))
  | Let (x, e0, e1) ->
      let l0, ce0 = transform bv e0 in
      let l1, ce1 = transform (VarSet.add x bv) e1 in
      (l0 @ l1, Let (x, ce0, ce1))

let expr_of_transformed (l, expr) = List.fold_right (fun (v, e) exp -> E.Let (v, e, exp)) l expr
let converted e = expr_of_transformed (transform VarSet.empty e)
let is_in_closure_form e = is_in_closure_form VarSet.empty e
