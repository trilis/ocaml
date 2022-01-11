type T = X | Y | Z

let (|A|B|C|D|) n m = function
  | X -> if ... A else B
  | Y (0) -> B
  | Y (1) -> D
  | Z -> C
and let rec helper = match x with 
  | A -> ...

.mli -> .cmi 
.cmt

let f = match x with 
  | A -> 

match e with 
  | { expr_desc = Exp1 (...) } -> 
  | { expr_desc = Exp2 (...) } -> 

Const -> { expr_desc = Pexp_constant (...) }

let (|Ident|Const|Let|) = match e with 
  | { expr_desc = Pexp_constant (...) } -> Const(...)

match e with 
  | [%exp x]