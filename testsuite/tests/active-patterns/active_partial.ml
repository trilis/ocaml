(* TEST 
  flags = "-w -8 -w -11"
*)

let (|MulThree|_|) inp = 
if inp mod 3 = 0 then Some () else None
let (|MulSeven|_|) inp = 
if inp mod 7 = 0 then Some () else None

let fizzbuzz x = 
  match x with 
  | MulThree -> "fizz"
  | MulSeven -> "buz"
  | _ -> ""

let () = 
  assert (fizzbuzz 3 = "fizz");
  assert (fizzbuzz 7 = "buz");
  assert (fizzbuzz 21 = "fizz");
  assert (fizzbuzz 0 = "fizz");
  assert (fizzbuzz 5 = "");
  assert (fizzbuzz 14 = "buz")