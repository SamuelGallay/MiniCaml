(*let () = Codegen.hello ()*)
(*let () = Llvm_codegen.main ()*)
module E = Expr
module T = Type
module HM = HindleyMilner
module CC = ClosureConversion

let debug = T.debug

let () =
  let f s =
    let expr = Result.get_ok (Parser.parse s) in
    Format.printf "Expression : %s\n" (E.string_of expr);
    try
      let typed_expr = HM.hindley_milner expr in
      if debug then Format.printf "Typed expression : %s\n" (E.string_of_typed typed_expr);
      (*if check_type typed_expr = false then Format.printf "TYPE DOESN'T CHECK\n";*)
      Format.printf "Type : %s\n" (T.string_of (E.get_type typed_expr));
      Format.printf "Closure form : %b\n" (CC.is_in_closure_form typed_expr);
      let conv = CC.converted typed_expr in
      Format.printf "New Expression : %s\n" (E.string_of conv);
      if not (CC.is_in_closure_form conv) then
        Format.printf "ERROR: EXPRESSION IS NOT IN CLOSURE FORM ! : \n";
      try
        let typed_conv = HM.hindley_milner conv in
        Format.printf "New Type : %s\n" (T.string_of (E.get_type typed_conv));
        Format.printf "\n"
      with HM.Inference_Error err -> Format.printf "Error : %s\n\n" err
    with HM.Inference_Error err -> Format.printf "Error : %s\n\n" err
  in

  let test_list =
    [
      "let f = fun x -> 3 in (f 2)";
      "let f = fun x -> x in f";
      "let f = fun x -> x in (f f)";
      "let f = fun x -> x in let f = 2 in f";
      "let f = fun x -> fun y -> x in f";
      "let eval = fun f -> fun x -> (f x) in eval";
      "let pair = fun x -> fun f -> ((f x) x) in pair";
      "let id = fun x -> x in ((id fun y -> y) (id 2))";
      "let x = [fun y -> y , 2, 3] in x";
      "let f = _p_3_2 in f";
      "fun x -> fun y -> fun z ->  x";
      (*"let rb_f16 = fun rb_t14 -> fun z -> [(_p_2_1 rb_t14), (_p_2_2 rb_t14), z] in rb_f16";*)
      "fun rb_t14  -> [(_p_1_1 rb_t14), (_p_1_1 rb_t14)]";
      "fun a -> [a, a]"
      (*"fun x -> fun y -> fun z -> [x, y, z]";
        "let b_f16 = fun b_t14 -> fun z -> [(_p_2_1 b_t14), (_p_2_2 b_t14), z] in let b_f15 = fun \
         b_t15 -> fun y -> (b_f16 [(_p_1_1 b_t15), y]) in let b_f14 = fun b_t16 -> fun x -> (b_f15 \
         [x]) in (b_f14 [])"
        "let f1 x = pair x in let f2 x = f1 (f1 x) in let f3 x = f2 (f2 x) in fun z -> f3 (fun x -> \
          x) z";*);
    ]
  in
  List.iter f test_list
