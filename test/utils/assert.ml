open OUnit2

let assert_true expected = assert_bool "Expected true value" expected

let assert_false expected = assert_bool "Expected false value" @@ not expected

let assert_close ~epsilon expected real =
  assert_equal ~cmp:(cmp_float ~epsilon) ~printer:string_of_float expected real
