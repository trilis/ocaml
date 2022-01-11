1) [DONE] fix occ (test_multiple.ml)
  - occ is passed incorrectly, investigate
2) if we call combine_constructor, it makes forbidden (for active patterns) optimizations (test_optional.ml)
  let fizzbuzz x = 
    match x with 
    | MulThree div -> "fizz"
    | MulSeven div -> "buz"
    | _ -> ""
  
    converts into something like this:

  let res1 = MulThree x;
  if res1 then "fizz" 
  else res2 = MulSeven x;
  (* assumes that res2 is always None, because first pattern evaluation resolved to None *)
  ""
  - for now, turned off intellectual choice of the branch where to go 
  - always goes to next available
3) cachemap stuff
  - for now, always insert if !z then z := ... 
  - not sure if it works in runtime: maybe z is not correctly initialized with zeroes
4) [FIXED] segfault


let f x y = match x, y with 
  | Some _, Rect(ar, ai) -> 1
  | None, Rect(ar, ai) -> 2 


z := makeblock 
if x then 
  z := (|Rect|) y; (ar,ai) := z; 1
else
  (ar, ai) := z; 2

CM = ()

(Rect |-> z)

z := block(0, 0)
(|A|B|) -> Choice_
setfield 0 z

<C e1 e2> x -> 
C e1 e2 x

Choice_3_1, Choice_3_2, Choice_3_3

|A|B|C| => `A | `B | `C

if x then 
  -> cm1
  if z then z := (|Rect|) y; (ar,ai) := z; 1
  <- (cm1 + z)
else
  -> (cm1 + z)
  if z then z := (|Rect|) y; (ar, ai) := z; 2
  <- cm1 

let f2 x y = match x, y with 
  | Rect(ar, ai), Some _ -> 1
  | Rect(ar, ai), None -> 2

1) generate explicit if 
2) generate explicit if + remove unneeded 
3) delayed computations

(|A|_)
(|A|B|)