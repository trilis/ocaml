(* TEST 
  flags = "-w -8 -w -11"
*)

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

  let rec (|Cons'|Nil'|) inp = match inp with
    | Single x                   -> Cons'(x, Empty)
    | Join (Cons'(x,xs), ys)     -> Cons'(x, Join (xs, ys))
    | Join (Nil' (), Cons' (y,ys)) -> Cons'(y, Join (ys, Empty))
    | _                          -> Nil'()

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

