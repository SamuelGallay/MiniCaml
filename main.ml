module E = Expr
module T = Type
module HM = HindleyMilner
module CC = ClosureConversion

let sprintf = Printf.sprintf
let debug = T.debug

exception Clang_Error of int
exception Parser_Error of string

let execute s =
  let log_string = ref "" in
  let log s = log_string := !log_string ^ s in

  try
    (*Can error*)
    log @@ sprintf "Pre-parsing expression : %s\n" s;
    let expr = match Parser.parse s with Ok e -> e | Error e -> raise (Parser_Error e) in
    log @@ sprintf "Expression : %s\n" (E.string_of expr);

    (*Can Inference_Error*)
    let typed_expr = HM.hindley_milner expr in
    if debug then Format.printf "Typed expression : %s\n" (E.string_of_typed typed_expr);
    log @@ sprintf "Type : %s\n" (T.string_of (E.get_type typed_expr));
    log @@ sprintf "Closure form : %b\n" (CC.is_in_closure_form typed_expr);

    let conv = CC.convert typed_expr in
    log @@ sprintf "New Expression : %s\n" (E.string_of conv);
    if not (CC.is_in_closure_form conv) then
      log @@ sprintf "ERROR: EXPRESSION IS NOT IN CLOSURE FORM ! : \n";

    (*Can Inference_Error *)
    let typed_conv = HM.hindley_milner conv in
    log @@ sprintf "New Type : %s\n" (T.string_of (E.get_type typed_conv));

    (*Can probably fail*)
    let stringModule = LlvmCodegen.main typed_conv in
    log @@ sprintf "\x1B[31m# Output LLVM IR :\x1B[0m\n%s" stringModule;
    let file = open_out "main.ll" in
    Printf.fprintf file "%s" stringModule;
    close_out file;

    (*Can Compilation_Error*)
    log @@ sprintf "\x1B[31m# Compiling using Clang...\x1B[0m\n";
    let c = Sys.command "clang main.ll" in
    if c != 0 then raise (Clang_Error c);

    log @@ sprintf "\x1B[31m# Executing the compiled program: \x1B[0m";
    let return_code = Sys.command "./a.out" in
    log @@ sprintf "Return code : %i\n" return_code;
    log @@ sprintf "\n";
    Ok (return_code, !log_string)
  with
  | LlvmCodegen.Codegen_Error err -> Error (sprintf "Codegen_Error : \"%s\"\n\n%s" err !log_string)
  | Parser_Error err -> Error (sprintf "Parser_Error [good luck...] : \"%s\"\n\n%s" err !log_string)
  | HM.Inference_Error err -> Error (sprintf "Inference_Error : \"%s\"\n\n%s" err !log_string)
  | e -> raise e

let test_list =
  [
    ("_p_1_1", 42);
    ("2", 2);
    ("[1, 2]", 42);
    ("let f = fun x -> 3 in (f 2)", 3);
    ("let f = fun x -> x in f", 42);
    ("let f = fun x -> x in (f f)", 42);
    ("let f = fun x -> x in let f = 2 in f", 2);
    ("let f = fun x -> fun y -> x in f", 42);
    ("let eval = fun f -> fun x -> (f x) in eval", 42);
    ("let pair = fun x -> fun f -> ((f x) x) in pair", 42);
    ("let id = fun x -> x in ((id fun y -> y) (id 2))", 2);
    ("let x = [fun y -> y , 2, 3] in x", 42);
    ("let f = _p_3_2 in f", 42);
    ("fun x -> fun y -> fun z ->  x", 42);
    ("let rb_f16 = fun rb_t14 -> fun z -> [(_p_2_1 rb_t14), (_p_2_2 rb_t14), z] in rb_f16", 42);
    ("fun rb_t14  -> [(_p_1_1 rb_t14), (_p_1_1 rb_t14)]", 42);
    ("fun a -> [a, a]", 42);
    ("fun x -> fun y -> fun z -> [x, y, z]", 42);
  ]

let all_errors = false

let () =
  let g r exec =
    match exec with
    | Ok (a, log) ->
        if a = r then None
        else Some (sprintf "Test Mismatch Error : expected %i and got %i instead !\n\n%s" r a log)
    | Error e -> Some e
  in
  let executed =
    List.map execute (List.map fst test_list) |> List.map2 g (List.map snd test_list)
  in
  let nb_correct = List.fold_left (fun c r -> if r = None then c + 1 else c) 0 executed in

  if List.length test_list = nb_correct then Format.printf "Hurray ! All tests succeeded !\n"
  else if all_errors then (
    Format.printf "Sadly only %i tests over %i succeeded... printing all errors. \n\n" nb_correct
      (List.length test_list);
    let errors = List.filter_map (fun x -> x) executed in
    Format.printf "%s" (String.concat "\n" errors))
  else (
    Format.printf "Sadly only %i tests over %i succeeded... printing the first error. \n\n"
      nb_correct (List.length test_list);
    let errors = List.filter_map (fun x -> x) executed in
    Format.printf "%s" (List.hd errors))
