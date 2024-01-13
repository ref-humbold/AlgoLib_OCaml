open OUnit2

let assert_not_equal ?(cmp = ( = )) = assert_equal ~cmp:(fun x y -> not @@ cmp x y)

module Bool = struct
  let assert_true ?(msg = "Expected true value") = assert_bool msg

  let assert_false ?(msg = "Expected false value") expected = assert_bool msg @@ not expected
end

module Float = struct
  let assert_close ~epsilon ?(printer = string_of_float) =
    assert_equal ~cmp:(cmp_float ~epsilon) ~printer
end
