module E = Expr
module T = Type
module C = Context

exception Inference_Error of string

let debug = false

let hindley_milner expression_to_type =
  let rec f : C.t -> 'a E.t -> T.t E.t * T.substitution =
   fun c expr ->
    let result_typed, result_subst =
      match expr with
      | Var (_, v) -> (
          try (E.Var (C.find_and_instantiate v c, v), [])
          with _ -> raise (Inference_Error (Format.sprintf "Symbol not found : %s" v)))
      | Abs (_, v, e) ->
          let tv = C.new_type c in
          let typed_e, s = f (C.add_mono v tv c) e in
          let final_type = T.Fct (T.F.Arrow, [ T.apply_mono s tv; E.get_type typed_e ]) in
          (Abs (final_type, v, typed_e), s)
      | Cst c -> (Cst c, [])
      | App (_, e0, e1) as expr -> (
          let typed_e0, s0 = f c e0 in
          let typed_e1, s1 = f (C.apply s0 c) e1 in
          let typed_e0 = E.apply_subst s1 typed_e0 in
          let t' = C.new_type c in
          try
            let s2 =
              T.unify_one (E.get_type typed_e0)
                (*(T.apply s1 (E.get_type typed_e0))*)
                (T.Fct (T.F.Arrow, [ E.get_type typed_e1; t' ]))
            in
            (* Probablement un erreur : s1 n'est pas appliquée à typed_e0... *)
            ( App (T.apply_mono s2 t', E.apply_subst s2 typed_e0, E.apply_subst s2 typed_e1),
              s2 @ s1 @ s0 )
          with T.Unification_Error s ->
            raise
              (Inference_Error
                 (Format.sprintf "Inference error in %s :\n    %s\n    Context : %s"
                    (E.string_of expr) s (C.string_of c))))
      | Let (x, e0, e1) ->
          let typed_e0, s0 = f c e0 in
          let typed_e1, s1 = f (c |> C.apply s0 |> C.add_poly x (E.get_type typed_e0)) e1 in
          (Let (x, typed_e0, typed_e1), s1 @ s0)
      | Tup l ->
          let g (ct, s1) e2 =
            let typed_e2, s2 = f (C.apply s1 c) e2 in
            (typed_e2 :: ct, s2 @ s1)
          in
          let tl, s = List.fold_left g ([], []) l in
          (Tup (List.rev tl |> List.map (E.apply_subst s)), s)
    in
    if debug then
      Format.printf "DEBUG : %s || %s || %s || %s \n" (E.string_of expr)
        (T.string_of (E.get_type result_typed))
        (E.basic_of_typed result_typed) (T.string_of_subst result_subst);

    (result_typed, result_subst)
  in
  let t, _ = f C.empty expression_to_type in
  t
