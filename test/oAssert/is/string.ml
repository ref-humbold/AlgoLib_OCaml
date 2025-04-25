open Common.Types
module S = Stdlib.String

let empty =
  Assertion
    (fun actual ->
       { is_success = S.equal actual "";
         expected_str = "empty string";
         actual_str = Printf.sprintf "\"%s\"" (S.escaped actual) } )

let of_length length =
  Assertion
    (fun actual ->
       let actual_length = S.length actual in
       { is_success = actual_length = length;
         expected_str = Printf.sprintf "length to be %d" length;
         actual_str = string_of_int actual_length } )

let equal expected =
  Assertion
    (fun actual ->
       { is_success = S.equal expected actual;
         expected_str = Printf.sprintf "\"%s\"" (S.escaped expected);
         actual_str = Printf.sprintf "\"%s\"" (S.escaped actual) } )

let starting_with prefix =
  Assertion
    (fun actual ->
       { is_success = S.starts_with ~prefix actual;
         expected_str = Printf.sprintf "starting with \"%s\"" (S.escaped prefix);
         actual_str = Printf.sprintf "\"%s\"" (S.escaped actual) } )

let ending_with suffix =
  Assertion
    (fun actual ->
       { is_success = S.ends_with ~suffix actual;
         expected_str = Printf.sprintf "ending with \"%s\"" (S.escaped suffix);
         actual_str = Printf.sprintf "\"%s\"" (S.escaped actual) } )
