(*****************************************************************************)
(* Total single active patterns                                              *)
(*****************************************************************************)

let (|S1|)     x = x
let ( | S2 | ) x = x
let ( | S3 
      | )      x = x

let s1 (|S4|)              = 42
let s2 ( | S5 | )          = 42
let s3 (|S6|) x ( | S7 | ) = 42

let (|S8|) (|S9|) (|S10|)  = 42

let test_first_class_of_single (|S11|) =
  ignore (s1 (|S11|));
  ignore (s2 ( | S11 | ));
  ignore (s3 (|S11|) ( | S11 | ) (|S11|))

let test_match_with_single1 = 
  match 42 with
  | S1 x -> x

let test_match_with_single2 = 
  match [42] with
  | S1 x :: _ -> x
  | []        -> 0 

let test_match_with_single3 (|S12|) = 
  match 42 with
  | S12 x -> ()

(*****************************************************************************)
(* Total multi active patterns                                               *)
(*****************************************************************************)

let (|A1|B1|)       x = A1 x
let ( | A2 | B2 | ) x = if true then A2 (Some x) else B2 None
let (| A3 
     | B3
     | C3
     | D3
     | E3
     | F3
     | G3
     | H3 
     |)             x = A3 x

let m1 (|A4|B4|C4|)         = 42
let m2 ( | A5 | B5 | C5 | ) = 42
let m3 (|A6|B6|C6|) x ( | A7 | B7 | C7 | ) = 42

let (|A8|B8|C8|) (|A9|B9|C9|) (|A10|B10|C10|) = A8 42

let test_first_class_of_multi (|A11|B11|C11|) =
  ignore (m1 (|A11|B11|C11|));
  ignore (m2 (|A11|B11|C11|));
  ignore (m3 (|A11|B11|C11|) (|A11|B11|C11|) (|A11|B11|C11|))

let test_match_with_multi1 = 
  match 42 with
  | A2(Some x) -> 1
  | A2(None)   -> 2
  | B2(Some _) -> 3
  | B2(None)   -> 4

let test_match_with_single2 = 
  match [42] with
  | A2(Some x) :: _ -> x
  | A2(None)   :: _ -> 0
  | B2 _       :: _ -> 0
  | []              -> 0 

let test_match_with_multi3 (|A12|B12|) = 
  match 42 with
  | A12(Some x) -> 1
  | A12(None)   -> 2
  | B12(Some _) -> 3
  | B12(None)   -> 4

(*****************************************************************************)
(* Partial unparameterized single active patterns                            *)
(*****************************************************************************)

let (|P1|_|)       x = Some x
let ( | P2 | _ | ) x = Some x
let ( | P3 
      | _
      | )          x = Some x

let p1 (|P4|_|)       = 42
let p2 ( | P5 | _ | ) = 42
let p3 (|P6|_|) x ( | P7 | _ | ) = 42

let (|P8|_|) (|P9|_|) (|P10|_|) = Some 42

let test_first_class_of_partial (|P11|_|) =
  ignore (p1 (|P11|_|));
  ignore (p2 ( | P11 | _ | ));
  ignore (p3 (|P11|_|) ( | P11 | _ | ) (|P11|_|))

let test_match_with_partial_unparameterized1 = 
  match 42 with
  | P1 x -> x
  | _    -> 0

let test_match_with_partial_unparameterized2 = 
  match [42] with
  | P1 x :: _ -> x
  | _    :: _ -> 0
  | []        -> 0 

let test_match_with_partial_unparameterized3 (|P12|_|) = 
  match 42 with
  | P12 x -> ()
  | _     -> ()

(*****************************************************************************)
(* Partial parameterized single active patterns                              *)
(*****************************************************************************)

let (|T1|_|)       p1 p2 x = Some(p1 + p2 + x)
let ( | T2 | _ | ) p1 p2 x = Some(p1 + p2 + x)
let ( | T3 
      | _
      | )          p1 p2 x = Some(p1 + p2 + x)

let test_match_with_partial_parameterized1 = 
  match 42 with
  | <T1 1 2> x -> x
  | _          -> 0

let test_match_with_partial_parameterized2 = 
  match [42] with
  | <T1 1 2 > x :: _ -> x
  | _           :: _ -> 0
  | []               -> 0 

let test_match_with_partial_parameterized3 (|T12|_|) = 
  match 42 with
  | <T12 1 2> x -> ()
  | _           -> ()


(*****************************************************************************)
(* Common active patterns examples                                           *)
(* See                                                                       *)
(* https://github.com/dotnet/fsharp/blob/master/tests/fsharp/core/patterns/test.fsx *)
(* https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/p29-syme.pdf *)
(*****************************************************************************)


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

  let mul_via_rect c1 c2 = 
    match c1,c2 with 
    | Rect(ar,ai), Rect(br,bi) -> 
        Complex.mk_rect(ar *. br -. ai *. bi, ai *. br +. bi *. ar)

  let mul_via_polar c1 c2 = 
    match c1,c2 with 
    | Polar(r1,th1),Polar(r2,th2) -> Complex.mk_polar(r1 *. r2, th1 +. th2)

  let mul1 (Rect(ar,ai)) (Rect(br,bi)) = 
    Complex.mk_rect(ar *. br -. ai *. bi, ai *. br +. bi *. ar)

  let mul2 (Polar(r1,th1)) (Polar(r2,th2)) = 
    Complex.mk_polar(r1 *. r2, th1 +. th2)
end


module NaturalNumbers_Example = struct

  let (|Zero|Succ|) n = if n = 0 then Zero else Succ(n - 1)

  let rec fib n = 
    match n with
    | Succ (Succ m) -> fib m + fib (m+1)
    | Succ Zero     -> 1
    | Zero          -> 0

  let (|Even|Odd|) n = if n mod 2 = 0 then Even(n / 2) else Odd(n - 1)

  let rec power x n =
    match n with
    | Even m -> let p = power x m in p * p
    | Odd  m -> x * power x m
end


module FunctionalQueue_Example = struct

  let (|Reversed|) l = List.rev l
  let (|NonEmpty|Empty|) q =
    match q with
    | (h::t), r               -> NonEmpty(h,(t,r))
    | []    , Reversed (h::t) -> NonEmpty(h,(t,[]))
    | _                       -> Empty()

  let enqueue x (f,r) = (f,x::r)

  let dequeue2 q = 
    match q with
    | NonEmpty(x,NonEmpty(y,xs)) -> x, y, xs
    | NonEmpty(x,Empty)          -> failwith "singleton queue"
    | Empty                      -> failwith "empty queue"
end


module JoinList_ExampleA = struct
  type ilist = 
    | Empty 
    | Single of int 
    | Join of ilist * ilist

  let rec (|Cons|Nil|) inp =
    match inp with  
    | Single x                -> Cons(x, Empty)
    | Join (Cons (x,xs), ys)  -> Cons(x, Join (xs, ys))
    | Join (Nil, Cons (y,ys)) -> Cons(y, Join (ys, Empty))
    | _                       -> Nil()

  let head js = 
    match js with 
    | Cons (x,_) -> x
    | _          -> failwith "empty list"
end

module JoinList_Example = struct
  type ilist = 
    | Empty 
    | Single of int 
    | Join   of ilist * ilist
  
  let rec (|Cons|Nil|) = function 
    | Single x                   -> Cons(x, Empty)
    | Join (Cons (x,xs), ys)     -> Cons(x, Join (xs, ys))
    | Join (Nil (), Cons (y,ys)) -> Cons(y, Join (ys, Empty))
    | _                          -> Nil()

  let head js = 
    match js with 
    | Cons (x,_) -> x
    | _ -> failwith "empty list"

  let rec map f xs =
    match xs with
    | Cons (y,ys) -> Join (Single (f y), map f ys)
    | Nil ()      -> Empty

  let rec to_list xs =
    match xs with
    | Cons (y,ys) -> y :: to_list ys
    | Nil () -> []
end

module PolyJoinList_Example = struct
  type 'a jlist = 
    | Empty 
    | Single of 'a 
    | Join   of 'a jlist * 'a jlist

  let rec (|JCons|JNil|) = function 
    | Single x                     -> JCons(x, Empty)
    | Join (JCons (x,xs), ys)      -> JCons(x, Join (xs, ys))
    | Join (JNil (), JCons (y,ys)) -> JCons(y, Join (ys, Empty))
    | Empty 
    | Join (JNil (), JNil ()) -> JNil()

  let jhead js = 
    match js with 
    | JCons (x,_) -> x
    | JNil        -> failwith "empty list"

  let rec jmap f xs =
    match xs with
    | JCons (y,ys) -> Join (Single (f y), jmap f ys)
    | JNil ()      -> Empty

  let rec jlist_to_list xs =
    match xs with
    | JCons (y,ys) -> y :: jlist_to_list ys
    | JNil ()      -> []
end


module UnZip_Example = struct

  let rec (|Unzipped|) = function 
    | ((x,y) :: Unzipped (xs, ys)) -> (x :: xs, y :: ys)
    | []                           -> ([], [])

  let unzip (Unzipped (xs, ys)) = xs, ys
end



module PartialPattern_Examples = struct

  let (|MulThree|_|) inp = 
    if inp mod 3 = 0 then Some(inp / 3) else None
  let (|MulSeven|_|) inp = 
    if inp mod 7 = 0 then Some(inp / 7) else None
    
  let example1 inp = 
    match 21 with 
    | MulThree(residue) -> Printf.printf "residue = %d!\n" residue
    | MulSeven(residue) -> Printf.printf "residue = %d!\n" residue
    | _                 -> Printf.printf "no match!\n"

(* 
   example1 777
   example1 9
   example1 10
   example1 21
*)

end
     

module ParameterizedPartialPattern_Examples = struct
  let (|Equal|_|) x y = 
    Printf.printf "x = %d!\n" x;
    if x = y then Some() else None
    
  let example1 = 
    match 3 with 
    | <Equal 4> () -> Printf.printf "3 = 4!\n"
    | <Equal 3> () -> Printf.printf "3 = 3!\n"
    | _            -> Printf.printf "3 = ?!\n"

  let (|Lookup|_|) x tbl = Hashtbl.find_opt tbl x
    
  let example2 = 
    let tbl = Hashtbl.create 2 in
    List.iter (fun (k,v) -> Hashtbl.add tbl k v) [ "2", "Two" ; "3", "Three" ];
    match tbl with 
    | <Lookup "4"> v -> Printf.printf "4 should not be present!\n"
    | <Lookup "3"> v -> Printf.printf "map(3) = %s\n" v
    | <Lookup "2"> v -> Printf.printf "this should not be reached\n"
    | _              -> Printf.printf "3 = ?!\n"
end
     

module Combinator_Examples = struct

  type ('a,'b) query = 'a -> 'b option 
  let mapQ1 f (|P|_|) = function (P x) -> Some (f x) | _ -> None
  let app1 (|P|)   (P x) = x
  let app2 (|P|_|) (P x) = x
  let mapQ2 f (|P|) (P x) = f x

  (* Given a partial pattern P find the first element in the list that satisfies P
     This is obviously overkill but it's showing what's possible. *)
  let find (|P|_|) =
    let rec (|E|_|) ys =
      match ys with 
      | (P x :: _  ) -> Some(x)
      | (_   :: E x) -> Some(x)
      | _ -> None
    in
    (|E|_|)    
end

(* Some troubles with Str module
module RegExp = struct

  let (|IsMatch|_|) (pat:string) (inp:string) = 
    let r = Str.regexp ("^" ^ pat ^ "$") in
    if Str.string_match r inp 0 then Some(inp) else None

  let check s b1 b2 = if b1 <> b2 then failwith s
  
  check "fwhin3op1" ((|IsMatch|_|) ".*.ml" "abc.ml") (Some "abc.ml")
end
*)

