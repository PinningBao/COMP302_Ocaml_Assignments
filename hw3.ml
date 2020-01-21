(* TODO: Write some tests for tabulate. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let tabulate_tests: (((int -> int) * int) * int list) list = [
  (* Remember: your test cases should have this form:
     ((f, n), output)
     The following test case asserts that:
       tabulate (fun x -> x) (-1)
     should have the output []
  *)
  (((fun x -> x), -1), []);
  (((fun x->x*0), 0), [0]);
  (((fun x-> x*x), 3), [0;1;4;9]);
]

(* TODO: Implement dist_table. *)
let dist_table (marblesTotal, marblesDrawn) x = 
  let dist_table' = fun n -> dist_black n x (marblesTotal, marblesDrawn) in
  tabulate dist_table' marblesTotal
               (* (int -> 'a ) -> int *)

(* TODO: Write some test cases for is_empty. *)
let is_empty_tests: (float list list * bool) list = [
  ([[]], true);
  ([], true);
  ([[0.0;0.0;0.0];[0.0;0.0;0.0];[0.0;0.0;0.0]], false);
  ([[0.1;0.2];[0.3;0.4]], false);
]

(* TODO: Implement is_empty. *)
let is_empty matrix =
  List.for_all (fun l -> l=[]) matrix

(* TODO: Implement dist_matrix. *)
let dist_matrix (total, drawn) resultList =
  if resultList = [] then [] 
  else
    let dist_table' = dist_table (total, drawn) in
    List.map dist_table' resultList
    
    
(* TODO: Implement combined_dist_table. *)
let rec combined_dist_table matrix =
  if is_empty matrix then []
  else 
    let x = (List.fold_left (fun a b ->  a *. b ) 1.0 (List.map List.hd matrix)) in
    x :: combined_dist_table (List.map List.tl matrix)
        

(* Once you have implemented all the above functions, you can
   use this function to compute the maximum likelihood.
   You can try it on the given example by running:
     max_likelihood (6, 3) [2; 0; 1]
*)
let max_likelihood (total, drawn) resultList =
  max_in_list
    (combined_dist_table
       (dist_matrix (total, drawn) resultList))
