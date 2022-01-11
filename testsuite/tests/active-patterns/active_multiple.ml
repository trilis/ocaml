(* TEST 
  flags = "-w -8 -w -11"
*)

let (|Zero|Succ|) n = if n = 0 then Zero else Succ(n - 1)

let rec fib n = 
  match n with
  | Succ (Succ m) -> fib m + fib (m+1)
  | Succ Zero     -> 1
  | Zero          -> 0

let () = 
  assert (fib 0 = 0); 
  assert (fib 1 = 1);
  assert (fib 2 = 1);
  assert (fib 3 = 2);
  assert (fib 4 = 3);
  assert (fib 15 = 610)
        

let (|Even|Odd|) n = if n mod 2 = 0 then Even(n / 2) else Odd(n - 1)

let rec power x n =
  match n with
  | 0 -> 1
  | Even m -> let p = power x m in p * p
  | Odd  m -> x * power x m

let () = 
  assert (power 5 0 = 1);
  assert (power 0 0 = 1);
  assert (power 6 1 = 6);
  assert (power 9 2 = 81);
  assert (power 2 3 = 8);
  assert (power 0 49 = 0);
  assert (power 1 52 = 1);
  assert (power 2 27 = 134217728)