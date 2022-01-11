let (|A|B|C|D|) args = function
  | X -> if e then A else B 5
  | Y (0) -> B
  | Y (1) -> D
  | Z -> C

let rec (|A|B|C|) args x = match x with 

let (|A|B|C|) args x = match x with 
  | x -> e

let (|F|) args = function
| X -> if e then (1, 2) else (3, 4)
| Y 0 -> (5, 6)
| Y 1 -> (7, 8)
| Z -> (9, 10)

let (|E|_) x = if x then 0 else 1 

let (|Some|None|) = function
  | X -> if e then Some 1 else None
  | Y 0 -> Some 0 (* always some, res = 0 *)
  | Y 1 -> Some (5 + 7 * 12) (* always some, res = 5 + 7 * 12 *)
  | Z -> None (* always none *)
  | R -> if e then Some 1 else Some 2 (* ???: always some, res = if e then 1 else 2 *)

(*********)

match x with 
  | E y -> f y
  | _ -> g x

=>

match x with 
  | X -> (match (if e then Some 1 else None) with 
    | Some y -> f y
    | None -> FALL:EXIT)
  | Y 0 -> y := 0 in f y
  | Y 1 -> y := 5 + 7 * 12 in f y
  | Z -> (match (None) with 
    | Some y -> f y
    | None -> FALL:EXIT)
  | _ -> ERR
  EXIT: | _ -> g x

=>

match x with 
  | X -> if e then f 1 else FALL:EXIT
  | Y 0 -> f 0
  | Y 1 -> f 1
  | Z -> FALL:EXIT
  | _ -> ERR 
  EXIT: | _ -> g x
  (* requires modifications to optimizations, but seems possible *)
  (* in branch EXIT, we can deduce that x is not Y 0 or Y 1 *)

(*********)

let (|E|_) = function
  | X -> if e then Some 1 else None
  | Y 0 -> Some 0
  | Y 1 -> Some 1
  | Z -> None

match x with 
  | E y -> f y 
  | E z -> f z 
  | _ -> g x

=>

match x with 
  | X -> if e then f 1 else FALL:EXIT1
  | Y 0 -> f 0
  | Y 1 -> f 1
  | Z -> FALL:EXIT2 (* note immediate fallthrough to EXIT2: we don't need to test for Z again *)
  | _ -> ERR
  EXIT1: | X -> if e then f 1 else FALL:EXIT2
  (* ^ should probably stay, as e and f are not guaranteed to be pure *)
  (* TODO: needs further thoughts about desired semantics *)
  (* F# *)

  (* | Y 0 -> f 0
     | Y 1 -> f 1 -- clearly redundant, as x was already tested for Y 0 and Y 1 *)

  (*| Z -> FALL:EXIT2 *)
  | _ -> ERR 
  EXIT2: | _ -> g x

(*********)

match (x, y) with 
  | E a, E b -> f a b 
  | _ -> g x y

=>

match x with 
  | X -> if e then (match y with 
    | X -> if e then f 1 1 else FALL:EXIT
    | Y 0 -> f 1 0
    | Y 1 -> f 1 1
    | Z -> FALL:EXIT) else FALL:EXIT
  | Y 0 -> match y with 
    | X -> if e then f 0 1 else FALL:EXIT
    | Y 0 -> f 0 0
    | Y 1 -> f 0 1
    | Z -> FALL:EXIT
  | Y 1 -> match y with 
    | X -> if e then f 1 1 else FALL:EXIT
    | Y 0 -> f 1 0
    | Y 1 -> f 1 1
    | Z -> FALL:EXIT
  | Z -> FALL:EXIT 
  | _ -> ERR 
  EXIT: _ -> g x y (* we can deduce that none of x, y are Y 0 or Y 1 *)