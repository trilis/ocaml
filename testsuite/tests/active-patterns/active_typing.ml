(* TEST 
  flags = "-w -8 -w -11"
*)

module ComplexNumbers_Example = struct
  (* Dummy definitions *)
  type complex = 
      { real_part: float; imaginary_part: float }
      
  let magnitude (_t: complex) = 1.0
  let phase     (_t: complex) = 1.0
  
  module Complex = struct
    (* Dummy definitions *)
    let mk_rect  (a,b) = { real_part = a; imaginary_part = b }
    let mk_polar (a,b) = { real_part = a; imaginary_part = b }
  end
  
  let (|Rect |) (x : complex) = (x.real_part , x.imaginary_part)
  let (|Polar|) (x : complex) = (magnitude x, phase x)
end


module NaturalNumbers_Example = 
struct
  let (|Zero|Succ|) n = if n = 0 then Zero else Succ(n - 1)

  let (|Even|Odd|) n = if n mod 2 = 0 then Even(n / 2) else Odd(n - 1)
end


module FunctionalQueue_Example = 
struct
  let (|Reversed|) l = List.rev l
(*
  let (|NonEmpty|Empty|) q =
    match q with
    | (h::t), r               -> NonEmpty(h,(t,r))
    | []    , Reversed (h::t) -> NonEmpty(h,(t,[]))
    | _                       -> Empty()
*)
end

(*
module UnZip_Example = struct
  let rec (|Unzipped|) = function 
    | ((x,y) :: Unzipped (xs, ys)) -> (x :: xs, y :: ys)
    | []                           -> ([], [])
end
*)


module PartialPattern_Examples = 
struct
  let (|MulThree|_|) inp = 
    if inp mod 3 = 0 then Some(inp / 3) else None
  let (|MulSeven|_|) inp = 
    if inp mod 7 = 0 then Some(inp / 7) else None
end
     

module ParameterizedPartialPattern_Examples = 
struct
  let (|Equal|_|) x y = 
    Printf.printf "x = %d!\n" x;
    if x = y then Some() else None

  let (|Lookup|_|) x tbl = Hashtbl.find_opt tbl x
end

(*
module RegExp = 
struct
  let (|IsMatch|_|) (pat:string) (inp:string) = 
    let r = Str.regexp ("^" ^ pat ^ "$") in
    if Str.string_match r inp 0 then Some(inp) else None
end
*)
