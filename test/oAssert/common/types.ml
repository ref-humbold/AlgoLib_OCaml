exception Assertion_failed of string

type assertion_result = {is_success : bool; expected_str : string; actual_str : string}

type 'a assertion = Assertion of ('a -> assertion_result)
