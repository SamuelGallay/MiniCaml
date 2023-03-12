(*open Types*)
open Angstrom

let is_space = function ' ' | '\t' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_alphanum = function '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false
let digits = take_while1 is_digit >>| fun d -> Expr.Cst (int_of_string d)
let spaces = skip_while is_space
let variable = spaces *> take_while1 is_alphanum <* spaces
let test s p = peek_string (String.length s) >>= fun t -> if t = s then p else fail "tkt"

let expression =
  fix (fun e ->
      let letexp =
        test "let"
        @@ lift3
             (fun v e1 e2 -> Expr.Let (v, e1, e2))
             (string "let" *> variable <* char '=')
             (e <* string "in")
             e
      in

      let abs =
        test "fun"
        @@ lift2 (fun v exp -> Expr.Abs ((), v, exp)) (string "fun" *> variable) (string "->" *> e)
      in

      let app =
        test "(" @@ lift2 (fun f x -> Expr.App ((), f, x)) (char '(' *> e) (e <* char ')')
      in

      let tup = test "[" @@ (char '[' *> sep_by (char ',') e <* char ']') >>| fun l -> Expr.Tup l in

      let digit = peek_char_fail >>= fun i -> if is_digit i then digits else fail "tkt" in

      let e_variable = variable >>| fun s -> Expr.Var ((), s) in

      spaces *> (letexp <|> abs <|> app <|> tup <|> digit <|> e_variable) <* spaces)

let parse = parse_string ~consume:Consume.All expression
