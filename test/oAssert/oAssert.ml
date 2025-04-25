exception Assertion_failed of string

type assertion_result = {is_success : bool; expected_str : string; actual_str : string}

type 'a assertion = Assertion of ('a -> assertion_result)

let fail ?(msg = "") = raise @@ Assertion_failed msg

let assert_that actual (Assertion f) =
  let {is_success; expected_str; actual_str} = f actual in
  if not is_success
  then
    let msg = Printf.sprintf "Expecting %s, but was %s" expected_str actual_str in
    fail ~msg
