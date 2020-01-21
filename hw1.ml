(* TODO: Correct these tests for the double function. *)
let double_tests = [
  (0, 0);
  (1, 2);
  (3, 6);
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let rec double n = 
  match n with
  | 0 -> 0
  | 1 -> 2 
  | _ -> 2 + (double (n - 1))(*m*)


(* TODO: Write your own tests for the fact function.
         See the provided tests for double, above, for how to write test cases.
         Remember that you should NOT test cases for n < 0.
*)
let fact_tests = [
  (* Your test cases go here.
     Remember that the outputs of fact should be *floating-point* numbers.
  *)
  (0, 1.0);
  (1, 1.0);
  (2, 2.0);
  (5, 120.0);
  
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let rec fact (n: int) = 
  match n with
  | 0 ->  1.0
  | _ ->  (float_of_int n) *. fact (n - 1);; 

(* TODO: Write a good set of tests for max_factor. *)
let max_factor_tests = [ 
  
  (*1. input = n
    2. output = k
    3. k > 0; n > 1
    4. k < n*) 
  
  (*prime numbers*)
  (2, 1); 
  (3, 1);
  (7, 1);
  
  (*composit numbers*)
  
  (*even*)
  (6, 3);
  (100, 50);
  (*odd*) 
  (15, 5);
  (21, 7);
  
  (*special case: when it's a square*)
  (25, 5);
  (49, 7);
  (121,11);
  
]

(* TODO: Implement max_factor. *)
let max_factor n = 
  
  let rec f n k = 
    if (n mod k = 0) then
      k
    else
      f n (k-1)
        
  in f n (n-1)
  
    (*raise NotImplemented*)

(* TODO: Write a good set of tests for fib_tl. *)
let fib_tl_tests = [
  
  (*n>=0*)
  
  (0, 1);
  (1, 1);
  (2, 2);
  (3, 3);
  (4, 5);
  
  
]

(* TODO: Implement a tail-recursive helper fib_aux. *)
let rec fib_aux n a b =
  if n = 0 then
    1
  else if n = 1 then
    b
  else
    fib_aux (n-1) b (a+b)
      
      
      
(* TODO: Implement fib_tl using fib_aux. *)
let fib_tl n =
  
  fib_aux n 1 1
    