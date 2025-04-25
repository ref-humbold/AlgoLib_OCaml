open Common.Types
module F = Stdlib.Float

let nan =
  Assertion
    (fun actual ->
      {is_success = F.is_nan actual; expected_str = "zero"; actual_str = string_of_float actual} )

let zero =
  Assertion
    (fun actual ->
      {is_success = actual = 0.0; expected_str = "zero"; actual_str = string_of_float actual} )

let positive =
  Assertion
    (fun actual ->
      { is_success = actual > 0.0;
        expected_str = "positive float";
        actual_str = string_of_float actual } )

let negative =
  Assertion
    (fun actual ->
      { is_success = actual < 0.0;
        expected_str = "negative float";
        actual_str = string_of_float actual } )

let close_to expected tolerance =
  Assertion
    (fun actual ->
      { is_success = expected -. tolerance <= actual && actual <= expected +. tolerance;
        expected_str = Printf.sprintf "close to %f +/- %f" expected tolerance;
        actual_str = string_of_float actual } )

let greater_than expected =
  Assertion
    (fun actual ->
      { is_success = actual > expected;
        expected_str = Printf.sprintf "greater than %f" expected;
        actual_str = string_of_float actual } )

let greater_than_or_equal_to expected =
  Assertion
    (fun actual ->
      { is_success = actual >= expected;
        expected_str = Printf.sprintf "greater than or equal to %f" expected;
        actual_str = string_of_float actual } )

let less_than expected =
  Assertion
    (fun actual ->
      { is_success = actual < expected;
        expected_str = Printf.sprintf "less than %f" expected;
        actual_str = string_of_float actual } )

let less_than_or_equal_to expected =
  Assertion
    (fun actual ->
      { is_success = actual <= expected;
        expected_str = Printf.sprintf "less than or equal to %f" expected;
        actual_str = string_of_float actual } )

let between start_inclusive end_inclusive =
  Assertion
    (fun actual ->
      { is_success = start_inclusive <= actual && actual <= end_inclusive;
        expected_str = Printf.sprintf "between %f and %f" start_inclusive end_inclusive;
        actual_str = string_of_float actual } )

let strictly_between start_exclusive end_exclusive =
  Assertion
    (fun actual ->
      { is_success = start_exclusive < actual && actual < end_exclusive;
        expected_str = Printf.sprintf "strictly between %f and %f" start_exclusive end_exclusive;
        actual_str = string_of_float actual } )
