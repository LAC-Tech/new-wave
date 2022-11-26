open OUnit2
open New_wave

let test_eval input expected =
  let top_level = Lib.Top_level.create () in

  fun _ -> assert_equal expected (Lib.Top_level.eval top_level input) 
    ~cmp:String.equal 
    ~printer:Fun.id

let suite = 
  "top level" >::: [
    "Errors" >::: [
      "stack underflow" >:: test_eval "1 +" "stack underflow"
    ];
    "Type inference" >::: [
      "two constants" >:: test_eval "42 30" "42 30 : num num\n"
    ];
    "SICP Examples" >::: [
      "eval empty input" >:: test_eval "" "";
      "primitive expression" >:: test_eval "486" "486 : num\n";
      "add ints" >:: test_eval "137 349 +" "486 : num\n";
      "subtract ints" >:: test_eval "1000 334 -" "666 : num\n"
    ]
  ]

let () = run_test_tt_main suite
