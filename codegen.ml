let spf = Format.sprintf

type returntype = [ `Int | `Float | `Char | `Ptr of returntype ]
type ctype = [ returntype | `FPtr of returntype * ctype list ]
type cfun = [ `Function of returntype * ctype list ]
type fdecl = { name : string; arg_type : ctype list; ret_type : returntype }
type fdef = string
type c_ast = { declarations : fdecl list; definitions : fdef list }

let rec string_of_returntype = function
  | `Int -> "int"
  | `Float -> "float"
  | `Char -> "char"
  | `Ptr rt -> spf "%s*" (string_of_returntype rt)

let rec string_of_ctype : ctype -> string = function
  | #returntype as r -> string_of_returntype r
  | `FPtr (rt, l) ->
      spf "%s (*)(%s)" (string_of_returntype rt)
        (l |> List.map string_of_ctype |> String.concat ", ")

let string_of_declaration decl =
  spf "%s %s(%s);"
    (string_of_returntype decl.ret_type)
    decl.name
    (decl.arg_type |> List.map string_of_ctype |> String.concat ", ")

let string_of_c ast =
  let dec = String.concat "\n" (List.map string_of_declaration ast.declarations) in
  let def = String.concat "\n" ast.definitions in
  spf "%s\n\n%s\n" dec def

let ex_ast =
  let d1 = { name = "id"; arg_type = [ `Ptr `Int ]; ret_type = `Ptr `Int } in
  let d2 = { name = "eval2"; arg_type = [ `FPtr (`Int, [ `Int ]) ]; ret_type = `Int } in
  { declarations = [ d1; d2 ]; definitions = [] }

let hello () = print_endline (string_of_c ex_ast)
