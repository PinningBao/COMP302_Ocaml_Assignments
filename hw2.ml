(* TODO: Write a good set of tests for psum. *)
let psum_tests = [
  (*edge cases*)
  ([], []);
  ([1], [1]);
  
  (*decrease in value*)
  ([1; 2; 1],[1; 3; 4]);
  
  (*increase in value*)
  ([1;3;45], [1;4;49]);
  
  (*same values*)
  ([3;3;3], [3; 6; 9])
]

(* TODO: Implement psum. *)
let rec psum l = match l with 
  | [] -> [] 
  | [x] -> [x]
  | x::y::t ->  x :: psum ((x+y) :: t) 
              
              

(* TODO: Write a good set of tests for intToBin. *)
let intToBin_tests = [
  (*edge cases: 0, 1, 2*)
  (0, E);
  (1, (One E));
  (2, Zero (One E));
  
  (*even*)
  (4, Zero (Zero (One E)));
  
  (*odd*)
  (7, One (One (One E)));
  
]

(* TODO: Implement intToBin. *) 

let rec intToBin n = match n with
  | 0 -> E
  | 1 -> (One E)
  | _ -> if n mod 2 = 0 then Zero (intToBin (n/2))
      else One (intToBin ((n-1)/2))
  
        
(* TODO: Write a good set of tests for binToInt. *)
let binToInt_tests = [
  
  (*edge cases: 0, 1, 2*)
  ((Zero E), 0);
  (E, 0);
  ((One E), 1);
  (Zero (One E), 2);
  
  (*even*)
  (Zero (Zero (One E)), 4);
  
  (*odd*)
  (One (One (One E)), 7);
  
]

(* TODO: Implement binToInt. *)
let rec binToInt b = match b with
  | E -> 0
  | (Zero E) -> 0
  | (Zero x) -> 2 * binToInt x 
  | (One E) -> 1
  | (One x) -> 1 + binToInt (Zero x)

  
(* TODO: Write a good set of tests for nnf. *)
let nnf_tests = [
  
  (*uniary cases*)
  (Atom "p", Atom "p"); 
  (Atom "r", Atom "r"); 
  (Neg (Atom "p"), Neg (Atom "p")); 
  
  (*implication*) 
  (Impl (Atom "p", Atom "q"), Disj (Neg (Atom "p"), Atom "q")); 
  (Neg (Impl (Atom "p", Atom "q")), Conj (Atom "p", Neg (Atom "q")));
  (Neg (Neg (Impl (Atom "p", Atom "q"))), Disj (Neg (Atom "p"), Atom "q"));

  (*double negation*)
  (Neg (Neg (Atom "p")), Atom "p");
  
  (*DeMogan's*)
  (Neg (Conj (Atom "p", Atom "q")), Disj (Neg (Atom "p"), Neg (Atom "q")));
  (Neg (Disj (Atom "p", Atom "q")), Conj (Neg (Atom "p"), Neg (Atom "q"))); 
  
  (*3 elements, nested negation*)
  (Neg (Conj (Conj (Atom "p", Atom "q"), Atom "r")), Disj (Disj (Neg (Atom "p"), Neg (Atom "q")), Neg (Atom "r")));
  (Neg (Disj (Disj (Atom "p", Atom "q"), Atom "r")), Conj (Conj (Neg (Atom "p"), Neg (Atom "q")), Neg (Atom "r")));
  (Impl (Impl (Atom "p", Atom "q"), Atom "r"), Disj ((Conj (Atom "p", Neg (Atom "q"))), Atom "r"));
  

]

(* TODO: Implement nnf. *)
let rec nnf p = match p with
  
  (*atom cases*)
  | Atom x -> Atom x 
  | Conj (x, y) -> Conj (nnf x, nnf y)
  | Disj (x, y) -> Disj (nnf x, nnf y)
  | Impl (x, y) ->  nnf (Disj (nnf (Neg x), nnf y))    
                      
  (*double negative*) 
  | Neg (Neg x) -> nnf x 
                     
  (*negavive atom*)
  | Neg (Atom x) -> Neg (Atom x) 
                      
  (*negative implication*)
  | Neg (Impl (x, y)) -> (nnf (Conj (nnf x, nnf (Neg y))) )
                         
  (*negative conj and disj*)
  | Neg (Conj (x, y)) -> (Disj (nnf (Neg x), nnf (Neg y)))
  | Neg (Disj (x, y)) -> (Conj (nnf (Neg x), nnf (Neg y))) 

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
