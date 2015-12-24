open OUnit

let eval_assert (expected : string) (input : string) : unit =
  assert_equal ~printer:(fun s -> s) expected (Catbird.evaluate input)

let test_functions _ =
  eval_assert "6" "(+ 1 5)";
  eval_assert "4" "(- 5 1)";
  eval_assert "3" "(/ 6 2)";
  eval_assert "8" "(* 4 2)";
  eval_assert "3" "(++ 2)";
  eval_assert "6" "(-- 7)";
  eval_assert "4" "(++ (-- 4))";
  eval_assert "false" "(& true false)";
  eval_assert "true" "(| false true)";
  eval_assert "true" "(! false)";
  eval_assert "true" "(= 'a 'a)";
  eval_assert "true" "(= _ _)";
  eval_assert "false" "(= 'a 'xq)";
  eval_assert "false" "(= 1 \"1\")";
  eval_assert "\"Hello World!\"" "(concat \"Hello \" \"World!\")";
  eval_assert "6" "(len \"!@#$%^\")";
  eval_assert "\"1923\"" "(->str 1923)";
  eval_assert "'(2 true)" "(# 2 '(true))";
  eval_assert "'(a)" "(hd# '((a) 1 2 3 false))";
  eval_assert "'(2 a)" "(#tl '(true 2 a))";
  eval_assert "<function>" "+" ;
  eval_assert "<function>" "(-)";
  eval_assert "<function>" "(/ 6)";
  eval_assert "Error: [arg type] expected integer" "(* true)";
  eval_assert "Error: [arg type] expected string" "(len 1)";
  eval_assert "Error: [apply] arg is not a function" "(len \"str\" 1)"

let test_lambdas _ =
  eval_assert "'(12 1 2 3)" "((\ (v) (# v '(1 2 3))) 12)"

let test_lists _ =
  eval_assert "'(unit unit b unit)" "'(_ _ b _)";
  eval_assert "'(unit unit b unit)" "(# _ (# _ (# 'b (# _ '()))))";
  eval_assert "'(() () ())" "'(()()())";
  eval_assert "'((a b) (1) 2 ())" "'((a b)(1)2())"

let suite = "Catbird Interp" >::: ["test_functions" >:: test_functions;
                                   "test_lambdas" >:: test_lambdas;
                                   "test_lists" >:: test_lists; ]

let _ = run_test_tt_main suite
