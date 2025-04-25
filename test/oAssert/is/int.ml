open Common.Types
module I = Stdlib.Int

let zero =
  Assertion
    (fun actual ->
      {is_success = actual = 0; expected_str = "zero"; actual_str = string_of_int actual} )

let positive =
  Assertion
    (fun actual ->
      {is_success = actual > 0; expected_str = "positive integer"; actual_str = string_of_int actual} )

let negative =
  Assertion
    (fun actual ->
      {is_success = actual < 0; expected_str = "negative integer"; actual_str = string_of_int actual} )

let equal_to expected =
  Assertion
    (fun actual ->
      { is_success = expected = actual;
        expected_str = Printf.sprintf "%d" expected;
        actual_str = string_of_int actual } )

let greater_than expected =
  Assertion
    (fun actual ->
      { is_success = actual > expected;
        expected_str = Printf.sprintf "greater than %d" expected;
        actual_str = string_of_int actual } )

let greater_than_or_equal_to expected =
  Assertion
    (fun actual ->
      { is_success = actual >= expected;
        expected_str = Printf.sprintf "greater than or equal to %d" expected;
        actual_str = string_of_int actual } )

let less_than expected =
  Assertion
    (fun actual ->
      { is_success = actual < expected;
        expected_str = Printf.sprintf "less than %d" expected;
        actual_str = string_of_int actual } )

let less_than_or_equal_to expected =
  Assertion
    (fun actual ->
      { is_success = actual <= expected;
        expected_str = Printf.sprintf "less than or equal to %d" expected;
        actual_str = string_of_int actual } )

let between start_inclusive end_inclusive =
  Assertion
    (fun actual ->
      { is_success = start_inclusive <= actual && actual <= end_inclusive;
        expected_str = Printf.sprintf "between %d and %d" start_inclusive end_inclusive;
        actual_str = string_of_int actual } )

let strictly_between start_exclusive end_exclusive =
  Assertion
    (fun actual ->
      { is_success = start_exclusive < actual && actual < end_exclusive;
        expected_str = Printf.sprintf "strictly between %d and %d" start_exclusive end_exclusive;
        actual_str = string_of_int actual } )
