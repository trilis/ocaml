open Typedtree
open Types
open Asttypes

let current_specialization = ref 0

let rec extract_branches = function
  Texp_function { cases; } -> (match cases with
    | [{c_lhs; c_rhs; c_guard (*?*)}] -> 
                  Some (Option.value ~default:[(c_lhs, c_rhs)] (extract_branches c_rhs.exp_desc))
    | caselist -> Some (List.map (fun {c_lhs; c_rhs; c_guard (*?*)} -> (c_lhs, c_rhs)) caselist)
  )
  | _ -> None

let compile_branch pat body = ()

let try_inlining rec_flag pat_expr_list body = 
  if rec_flag || (match pat_expr_list with 
    (Tpat_var _, _, Tpat_variant _) :: [] -> true
    | _ -> false)
  then None 
  else 
  let branches = extract_branches body.exp_desc in 
  Option.map 
  (fun branches -> 
    List.map (fun (p, b) -> compile_branch p b) branches  
  )
  branches

let replace_constructor (id: Uid.t) (c: constructor_description) (es: expression list): expression_desc = 
  if c.cstr_uid = id then (Typedtree.Texp_constant (Const_int (-1))) else (Typedtree.Texp_tuple es)

