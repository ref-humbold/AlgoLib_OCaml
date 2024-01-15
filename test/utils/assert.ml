open OUnit2

let assert_not_equal ?(cmp = ( = )) ?printer expected actual =
  let message =
    match printer with
    | Some p -> Printf.sprintf "expected value different than %s" @@ p actual
    | None -> "expected different values"
  in
  if cmp expected actual then assert_failure message

module Bool = struct
  let assert_true ?(msg = "Expected true value") = assert_bool msg

  let assert_false ?(msg = "Expected false value") expected = assert_bool msg @@ not expected
end

module Float = struct
  let assert_close ~epsilon ?(printer = string_of_float) =
    assert_equal ~cmp:(cmp_float ~epsilon) ~printer
end

module Option = struct
  let assert_some ?(cmp = ( = )) ~printer expected actual =
    let cmp' opt1 opt2 =
      match (opt1, opt2) with
      | Some x, Some y -> cmp x y
      | _, _ -> opt1 = opt2
    in
    assert_equal ~cmp:cmp' ~printer:(Printers.option printer) (Some expected) actual

  let assert_none ~printer actual = assert_equal ~printer:(Printers.option printer) None actual
end
