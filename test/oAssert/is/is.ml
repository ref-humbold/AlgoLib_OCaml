open Common.Types

let true_ =
  Assertion
    (fun actual ->
       {is_success = actual; expected_str = string_of_bool true; actual_str = string_of_bool actual} )

let false_ =
  Assertion
    (fun actual ->
       { is_success = not actual;
         expected_str = string_of_bool true;
         actual_str = string_of_bool actual } )
