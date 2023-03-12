type var = string

module F = struct
  type t = Arrow | Int | Tuple
end

type t = Fct of F.t * t list | Var of var

(* À échange de variables... *)
let rec equal t1 t2 =
  match (t1, t2) with
  | Var _, Var _ -> true
  | Fct (tf1, l1), Fct (tf2, l2) when tf1 = tf2 && List.length l1 = List.length l2 ->
      List.for_all2 equal l1 l2
  | _, _ -> false

let rec string_of = function
  | Var v -> v
  | Fct (F.Arrow, [ t1; t2 ]) -> Format.sprintf "(%s -> %s)" (string_of t1) (string_of t2)
  | Fct (F.Int, []) -> "int"
  | Fct (F.Tuple, l) -> String.concat " * " (List.map string_of l)
  | Fct (F.Arrow, _) | Fct (F.Int, _) -> failwith "Incorrect type"

let arrow a b = Fct (F.Arrow, [ a; b ])
let tuple l = Fct (F.Tuple, l)
let tint = Fct (F.Int, [])
let is_tuple = function Fct (F.Tuple, _) -> true | _ -> false
let rec get_vars = function Var s -> [ s ] | Fct (_, l) -> List.concat_map get_vars l

(* BEGIN : Stolen Unification *)

(* invariant for substitutions: *)
(* no id on a lhs occurs in any term earlier in the list *)
type substitution = (var * t) list

(* check if a variable occurs in a term *)
let rec occurs (x : var) (t : t) : bool =
  match t with Var y -> x = y | Fct (_, s) -> List.exists (occurs x) s

(* substitute term s for all occurrences of variable x in term t *)
let rec subst (s : t) (x : var) (t : t) : t =
  match t with Var y -> if x = y then s else t | Fct (f, u) -> Fct (f, List.map (subst s x) u)

(* apply a substitution right to left *)
let apply (s : substitution) (t : t) : t = List.fold_right (fun (x, u) -> subst u x) s t

exception Unification_Error of string

(* unify one pair *)
let rec unify_one (s : t) (t : t) : substitution =
  match (s, t) with
  | Var x, Var y -> if x = y then [] else [ (x, t) ]
  | Fct (f, sc), Fct (g, tc) ->
      if f = g && List.length sc = List.length tc then unify (List.combine sc tc)
      else raise (Unification_Error "not unifiable: head symbol conflict")
  | Var x, (Fct (_, _) as t) | (Fct (_, _) as t), Var x ->
      if occurs x t then raise (Unification_Error "not unifiable: circularity") else [ (x, t) ]

(* unify a list of pairs *)
and unify (s : (t * t) list) : substitution =
  match s with
  | [] -> []
  | (x, y) :: t ->
      let t2 = unify t in
      let t1 = unify_one (apply t2 x) (apply t2 y) in
      t1 @ t2
(* END : Stolen Unification *)

let string_of_list l = Format.sprintf "[%s]" (String.concat "; " l)

let string_of_subst l =
  l |> List.map (fun (v, t) -> Format.sprintf "(%s, %s)" v (string_of t)) |> string_of_list

let rec old_unify t1 t2 =
  match (t1, t2) with
  | Fct (tf1, l1), Fct (tf2, l2) ->
      if tf1 = tf2 && List.length l1 = List.length l2 then Fct (tf1, List.map2 old_unify l1 l2)
      else
        failwith (Format.sprintf "Types '%s' and '%s' don't unify!\n" (string_of t1) (string_of t2))
  | Var _, Fct (tf, l) | Fct (tf, l), Var _ -> Fct (tf, l)
  | Var _, Var _ -> t1
