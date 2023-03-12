type var = string

type 'a t =
  | Var of 'a * var (* x *)
  | App of 'a * 'a t * 'a t (* f x *)
  | Abs of 'a * var * 'a t (* fun x -> e *)
  (* Let x = e1 in e2 *)
  | Let of var * 'a t * 'a t
  | Cst of int
  | Tup of 'a t list

let rec get_type = function
  | Var (t, _) | App (t, _, _) | Abs (t, _, _) -> t
  | Cst _ -> Type.Fct (Type.F.Int, [])
  | Let (_, _, e2) -> get_type e2
  | Tup l -> Type.Fct (Type.F.Tuple, List.map get_type l)

(* Replace Var s par e *)
let rec map_vars (s, e) = function
  | Var (t, v) -> if v = s then e else Var (t, v)
  | Cst c -> Cst c
  | App (t, e0, e1) -> App (t, map_vars (s, e) e0, map_vars (s, e) e1)
  | Abs (t, x, e0) -> Abs (t, x, map_vars (s, e) e0)
  | Let (v, e0, e1) -> Let (v, map_vars (s, e) e0, map_vars (s, e) e1)
  | Tup l -> Tup (List.map (map_vars (s, e)) l)

let rec map_types f = function
  | Var (t, v) -> Var (f t, v)
  | Cst c -> Cst c
  | App (t, e0, e1) -> App (f t, map_types f e0, map_types f e1)
  | Abs (t, x, e0) -> Abs (f t, x, map_types f e0)
  | Let (v, e0, e1) -> Let (v, map_types f e0, map_types f e1)
  | Tup l -> Tup (List.map (map_types f) l)

let apply_subst s e = map_types (Type.apply_mono s) e

let rec basic_string_of pt =
  let sprintf = Format.sprintf in
  function
  | Var (t, v) -> sprintf "%s%s" (pt t) v
  | App (t, f, x) -> sprintf "(%s %s %s)" (pt t) (basic_string_of pt f) (basic_string_of pt x)
  | Abs (t, x, e) -> sprintf "(%sfun %s -> %s)" (pt t) x (basic_string_of pt e)
  | Let (v, e1, e2) -> sprintf "let %s = %s in %s" v (basic_string_of pt e1) (basic_string_of pt e2)
  | Cst i -> sprintf "%d" i
  | Tup l -> l |> List.map (basic_string_of pt) |> String.concat ", " |> sprintf "(%s)"

let basic_of_typed = basic_string_of Type.string_of
(*| Cls (t, f, xi, e1, e2) ->
    sprintf "cls%s %s (%s) = %s in %s" (pt t) f (String.concat ", " xi) (string_of pt e1)
      (string_of pt e2)*)

let rec pretty_string_of pt i e =
  let sprintf = Format.sprintf in
  match e with
  | Var (_, v) -> v
  | App (_, f, x) ->
      let s1 =
        match x with
        | Abs _ | Let _ -> sprintf "(%s)" (pretty_string_of pt i f)
        | Var _ | Cst _ | Tup _ | App _ -> sprintf "%s" (pretty_string_of pt i f)
      in
      let s2 =
        match x with
        | Abs _ | Let _ | App _ -> sprintf "(%s)" (pretty_string_of pt i x)
        | Var _ | Cst _ | Tup _ -> sprintf "%s" (pretty_string_of pt i x)
      in
      sprintf "%s %s" s1 s2
  | Abs (_, x, e) -> sprintf "fun %s -> %s" x (pretty_string_of pt i e)
  | Let (v, e1, e2) ->
      sprintf "%s\n%slet %s = %s in %s" (pt e1)
        (String.make (2 * i) ' ')
        v
        (pretty_string_of pt (i + 1) e1)
        (pretty_string_of pt i e2)
  | Cst i -> sprintf "%d" i
  | Tup l -> l |> List.map (pretty_string_of pt i) |> String.concat ", " |> sprintf "(%s)"

(*let string_of_typed_expr = string_of Type.string_of
  let basic_of x = string_of (fun _ -> "") x*)
let string_of_typed e =
  pretty_string_of (fun e -> Format.sprintf "\n%s" (Type.string_of (get_type e))) 2 e

let string_of e = pretty_string_of (fun _ -> "") 2 e
