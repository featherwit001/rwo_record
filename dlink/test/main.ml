open OUnit2
open Double_linked


let make_test_equal n expect output =
  n >:: (fun _ -> assert_equal expect output)

let dlink1 = create ()
let tests_double_linked = "tests_double_linked" >::: [
  make_test_equal "empty" true (is_empty (create ()));
  make_test_equal "insert 1" [1] (ignore(insert_first dlink1 1); to_list dlink1);
  make_test_equal "remove 1" true (remove dlink1 1; is_empty dlink1);
  make_test_equal "insert lst" [1;2;3;4] ([1;2;3;4] |> of_list |> to_list);
]

let _ = run_test_tt_main tests_double_linked