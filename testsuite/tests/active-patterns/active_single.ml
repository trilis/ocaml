(* TEST 
  flags = "-w -8 -w -11"
*)

type complex = 
      { real_part: float; imaginary_part: float }
      
  let magnitude (_t: complex) = 1.0
  let phase     (_t: complex) = 1.0
  
  module Complex = struct
    (* Dummy definitions *)
    let mk_rect  (a,b) = { real_part = a; imaginary_part = b }
    let mk_polar (a,b) = { real_part = a; imaginary_part = b }
  end

  let i = { real_part = 0.0; imaginary_part = 1.0};;

  
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

  let f x y = match x, y with 
    | Some _, Rect(ar, ai) -> 1
    | None, Rect(ar, ai) -> 2 

  let f2 x y = match x, y with 
    | Rect(ar, ai), Some _  -> 1
    | Rect(br, bi), None -> 2

  let f3 x y = match x, y with
    | Some _, Rect(ar, ai) when ar >= 1.0 -> 1 
    | None, Rect(ar, ai) -> 2
    | Some _, Rect(ar, ai) -> 3

  let f4 x = match x with
    | Rect(ar, ai) when ar >= 1.0 -> 1 
    | Rect(ar, ai) -> 2
    | Rect(ar, ai) -> 3

  let print_complex c = print_float c.real_part; print_string " + "; 
                        print_float c.imaginary_part; print_string "i\n";;
