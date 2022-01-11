let (|A|B|C|D|) args = function (* exhaustive: true/false *)
  | X -> if e then A else B 5   (* A, B *)
  | Y 0 -> B 2                  (* B *)
  | Y 1 -> D 3 1                (* D *)
  | Z -> C                      (* C *)
  ... 
  | _ -> fail "oops"            (* none *)

let (|A|_) = function
  | X -> Some ()
  | Y -> f (* Some, None *)
  | _ -> None

(* proposition: compile specialized versions*)
(* not really functions, just list of pattern-code pairs *)

let (|A|*) args = function 
  | X -> if e then () else FAIL

let (|B|*) args = function 
  | X -> if e then FAIL else 5
  | Y 0 -> 2

let (|C|*) args = function 
  | Z -> ()

match x with 
  | D -> ... 
  | ...

match x with
  | Y 1 -> D 3 1                (* D *)
  | Z -> C                      (* C *)
  | _ -> match_failure 

let (|D|*) args = function 
  | Y 1 -> (3, 1)
  (* if exhaustive = false *) | ??? (* !X && !(Y 0) && !Z *) -> ERR (* is it important? *)

(* we skip some tests here -- is it OK semantically? (e.g. side-effects of other active patterns) *)

match x with 
  | A -> f
  | B y -> g y
(* => *)
match x with 
  | X -> let _ = if e then () else FAIL in f
  | X -> let y = if e then FAIL else 5 in g y
  | Y 0 -> let y = 2 in g y; 
(* =>???? *)
match x with 
  | X -> (let t = if e then `A else `B 5 in match t with 
    | `A -> f 
    | `B y -> g y)
  | Y 0 -> let y = 2 in g y; 






(*CASE 1 (one possible):*)
  match x with 
    | D y -> f y
  =>
  match x with 
    | Y (1) -> f 3 (* is it easy to transform? *)
    




(*CASE 2 (many possible):*)

| X -> match (if e then A else B 5) with 
  | A -> FAIL
  | B y -> y := 5
