module E = Expr
module T = Type
module HM = HindleyMilner
module CC = ClosureConversion

let sprintf = Printf.sprintf
let debug = T.debug

exception Clang_Error of int
exception Opt_Error of int
exception Parser_Error of string
exception Execution_Error of int

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
    let stringModule = LlvmCodegen.main log typed_conv in
    log @@ sprintf "\x1B[31m# Output LLVM IR :\x1B[0m\n%s" stringModule;
    let file = open_out "_temp/main.ll" in
    Printf.fprintf file "%s" stringModule;
    close_out file;

    let c = Sys.command "opt -O3 -S _temp/main.ll > _temp/optimized.ll" in
    if c != 0 then raise (Opt_Error c);
    let file = open_in "_temp/optimized.ll" in
    let stringOptimized = really_input_string file (in_channel_length file) in
    log @@ sprintf "\x1B[31m# Output Optimized LLVM IR :\x1B[0m\n%s" stringOptimized;
    close_in file;

    (*Can Compilation_Error*)
    log @@ sprintf "\x1B[31m# Compiling using Clang...\x1B[0m\n";
    let c = Sys.command "clang -g _temp/optimized.ll _temp/runtime.o -o _temp/a.out" in
    if c != 0 then raise (Clang_Error c);

    log @@ sprintf "\x1B[31m# Executing the compiled program: \x1B[0m";
    let return_code = Sys.command "./_temp/a.out > _temp/output.txt" in
    log @@ sprintf "Return code : %i\n" return_code;
    if return_code != 0 then raise (Execution_Error return_code);

    let file = open_in "_temp/output.txt" in
    let stringOutput = really_input_string file (in_channel_length file) in
    log @@ sprintf "\x1B[31m# String Output : :\x1B[0m\n%s" stringOutput;
    close_in file;

    log @@ sprintf "\n";
    Ok (stringOutput, !log_string)
  with
  | LlvmCodegen.Codegen_Error err -> Error (sprintf "Codegen_Error : \"%s\"\n\n%s" err !log_string)
  | Parser_Error err -> Error (sprintf "Parser_Error [good luck...] : \"%s\"\n\n%s" err !log_string)
  | HM.Inference_Error err -> Error (sprintf "Inference_Error : \"%s\"\n\n%s" err !log_string)
  | Clang_Error err -> Error (sprintf "Clang_Error : \"Code %i\"\n\n%s" err !log_string)
  | Execution_Error err -> Error (sprintf "Execution_Error : \"Code %i\"\n\n%s" err !log_string)
  | e -> raise e

let test_list =
  [
    ("_p_1_1", "");
    ("2", "");
    ("(print_int 14)", "14 ");
    ("2023", "");
    ("[1, 2]", "");
    ("let f = fun x -> 3 in (print_int (f 2))", "3 ");
    ("let f = fun x -> x in f", "");
    ("let f = fun x -> x in (f f)", "");
    ("let f = fun x -> x in let f = 2 in (print_int f)", "2 ");
    ("let f = fun x -> fun y -> x in f", "");
    ("let eval = fun f -> fun x -> (f x) in eval", "");
    ("let pair = fun x -> fun f -> ((f x) x) in pair", "");
    ("let id = fun x -> x in (id id)", "");
    ("let id = fun x -> x in (print_int (id (id 2)))", "2 ");
    ("let id = fun x -> x in let idid = (id id) in (print_int (idid 2023))", "2023 ");
    ("let id = fun x -> x in (print_int ((id id) 367))", "367 ");
    ("let id = fun x -> x in (print_int ((id fun y -> y) (id 2)))", "2 ");
    ("let x = [fun y -> y , 2, 3] in x", "");
    ("let f = _p_3_2 in f", "");
    ("fun x -> fun y -> fun z ->  x", "");
    ("let rb_f16 = fun rb_t14 -> fun z -> [(_p_2_1 rb_t14), (_p_2_2 rb_t14), z] in rb_f16", "");
    ("fun rb_t14  -> [(_p_1_1 rb_t14), (_p_1_1 rb_t14)]", "");
    ("fun a -> [a, a]", "");
    ("fun x -> fun y -> fun z -> [x, y, z]", "");
  ]

let all_errors = false

let () =
  let _ = Sys.command "rm _temp/*" in
  let c = Sys.command "clang -c runtime.c -o _temp/runtime.o" in
  if c != 0 then failwith "Can't compile the runtime...";
  let g r exec =
    match exec with
    | Ok (a, log) ->
        if a = r then None
        else
          Some
            (sprintf "Test Mismatch Error : expected \"%s\" and got \"%s\" instead !\n\n%s" r a log)
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
