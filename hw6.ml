(* TODO: Write a good set of tests for unused_vars. *)
let unused_vars_tests = [
  (* An example test case.
     Note that you are *only* required to write tests for Rec, Fn, and Apply!
  *)
  (Let ("x", I 1, I 5), ["x"]);
  
  
  (Let ("f", ex1, Apply (Var "f", [I 3; I 4])), []);
(*test for Rec*)
  (*used*)
  (Rec ("y", (Arrow ([Int; Int], Bool)), B true), ["y"]);
   (*unused*) 
  (Rec ("fido", (Arrow ([Bool; Bool], Bool)), Var "fido"), []);
  
(*test for Fn*)
  (*no inputs*)
  (Fn ([], B true), []);
  (*single input*)
  (*unused*)
  (Fn ([("x", Int)], B true), ["x"]);
  (*used*)
  (Fn ((["x", Int]), Var "z"), ["x"]);
  (*multiple inputs*)
(*unused*)
  (Fn ([("first", Int); ("second", Bool)], B true), ["first"; "second"]);
  
(*test for Apply*)
  
  (*no input*)
  (Apply (Var "f",[]), []);
  (*one input*)
  (Apply (Var "f", [I 3; I 4]), []);
  
]

(* TODO: Implement the missing cases of unused_vars. *)
let rec unused_vars =
  function
  | Var _ | I _ | B _ -> []
  | If (e, e1, e2) -> unused_vars e @ unused_vars e1 @ unused_vars e2
  | Primop (_, args) ->
      List.fold_left (fun acc exp -> acc @ unused_vars exp) [] args
  | Let (x, e1, e2) ->
      let unused = unused_vars e1 @ unused_vars e2 in
      if List.mem x (free_variables e2) then
        unused
      else
        x :: unused

  | Rec (x, _, e) -> 
      let l1 = unused_vars e in
      let l2 = free_variables e in
      if List.mem x l2 then l1
      else
        x::l1 
  | Apply (e, es) -> 
      let l1 = unused_vars e in
      let l2 = List.fold_left (fun acc exp -> acc @ unused_vars exp) [] es in
      l1 @ l2
                       

  | Fn (xs, e) -> match xs with
    | [] -> unused_vars e
    | _ -> 
        let input = List.map (fun (x, _) -> x) xs in 
        let free = free_variables e in
        let unusedE = unused_vars e in
  
        let l1 = List.filter (fun x -> List.mem x free) input in
        
        (delete l1 (input@unusedE)) 
    
        
    

(* TODO: Write a good set of tests for subst. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let subst_tests : (((exp * name) * exp) * exp) list = [
  (* An example test case. If you have trouble writing test cases of the
     proper form, you can try copying this one and modifying it.
     Note that you are *only* required to write tests for Rec, Fn, and Apply!
  *)
  (((I 1, "x"), (* [1/x] *)
    (* let y = 2 in y + x *)
    Let ("y", I 2, Primop (Plus, [Var "y"; Var "x"]))),
   (* let y = 2 in y + 1 *)
   Let ("y", I 2, Primop (Plus, [Var "y"; I 1])));
  
  (*rec*) 
  (*x is bounded in expression*)
  (((I 3, "x"), 
    Rec ("x", (Arrow ([Int; Int], Int)), Var "x")),
   Rec ("x", (Arrow ([Int; Int], Int)), Var "x"));
  
  (*x is not bounded in expression*)
  (((I 3, "x"), 
    Rec ("z", (Arrow ([Int; Int], Int)), Var "x")),
   Rec ("z", (Arrow ([Int; Int], Int)), I 3));
  
  (*replacing variable*)
  (((Var "z", "x"), 
    Rec ("y", (Arrow ([Int; Int], Int)), 
         Primop (Plus, [Var "x"; I 4]))),
   Rec ("y", (Arrow ([Int; Int], Int)), 
        Primop (Plus, [Var "z"; I 4])));
  
  (*renaming when variable is free*)
  (((Var "y", "x"), 
    Rec ("y", (Arrow ([Int; Int], Int)), 
         Primop (Plus, [Var "x"; I 4]))),
   Rec ("z", (Arrow ([Int; Int], Int)), 
        Primop (Plus, [Var "y"; I 4])));
  
  (*fn*) 
  (*normal substitution*)
  (((I 3, "x"), 
    Fn ([("z", Int)], Primop (Equals, [Var "x"; I 3]))), 
   Fn ([("z", Int)], Primop (Equals, [I 3; I 3]))
  );
  
  (*x is not bounded*) 
  (((Var "y", "x"),
    (Fn ([("x", Int); ("y", Int)], 
         Primop (Plus, [Var "x"; Var "y"])))),
   (Fn ([("x", Int); ("z", Int)], 
        Primop (Plus, [Var "x"; Var "z"])))
  );
  
  (*rename if there is a free variable*)
  (((Var "z", "x"),
    (Fn ([("z", Int); ("y", Int)], 
         Primop (Plus, [Var "x"; Var "y"])))),
   (Fn ([("k", Int); ("y", Int)], 
        Primop (Plus, [Var "z"; Var "y"])))
  );
  
  (*apply*) 
  (*in list*)
  (((I 3, "x"),
    Apply (Primop (Plus, [Var "m"; Var "n"]), [Var "x"; Var "y"])),
   Apply (Primop (Plus, [Var "m"; Var "n"]), [I 3; Var "y"]));
  
  (*in e*)
  (((I 3, "m"),
    Apply (Primop (Plus, [Var "m"; Var "n"]), [Var "x"; Var "y"])),
   Apply (Primop (Plus, [I 3; Var "n"]), [Var "x"; Var "y"]));
  
  (*multiple substitution*)
  (((I 3, "m"),
    Apply (Primop (Plus, [Var "x"; Var "y"]), [Var "m"; Var "m"])),
   Apply (Primop (Plus, [Var "x"; Var "y"]), [I 3; I 3])); 
]

(* TODO: Implement the missing cases of subst. *)
let rec subst ((e', x) as s) exp =
  match exp with
  | Var y ->
      if x = y then e'
      else Var y
  | I n -> I n
  | B b -> B b
  | Primop (po, args) -> Primop (po, List.map (subst s) args)
  | If (e, e1, e2) ->
      If (subst s e, subst s e1, subst s e2)
  | Let (y, e1, e2) ->
      let e1' = subst s e1 in
      if y = x then
        Let (y, e1', e2)
      else
        let (y, e2) =
          if List.mem y (free_variables e') then
            rename y e2
          else
            (y, e2)
        in
        Let (y, e1', subst s e2)

  | Rec (y, t, e) -> 
      if y = x then exp else
        let freeL = free_variables e' in
        let (a, b) = 
          if List.mem y freeL then rename y e
          else
            (y, e)
        in 
        Rec (a, t, subst s b)
                       
  | Fn (xs, e) -> 
      begin
        let inputNames = List.map (fun (m, _) -> m) xs in 
        let members = List.filter (fun x -> List.mem x (free_variables e')) inputNames in
        let (a, b) = rename_all members e in 
        let l0 = List.combine members a in 
        let newX =
          
          let rec subsTup l1 l2 acc = 
            match (l1, l2) with 
            |(j, []) -> (acc@j)
            |((m, n)::t1, (o, p)::t2) ->
                if m = o then subsTup t1 t2 (acc@[(p, n)])
                else subsTup t1 l2 (acc@[(m, n)]) 
                    
             (*xs is going to be at least the length of l0*)
          in subsTup xs l0 [] in
        
  
        if List.mem x inputNames then exp else 
          Fn (newX, subst s b) 
      end

  | Apply (e, es) -> Apply (subst s e, List.map (fun a -> subst s a) es)

and rename x e =
  let x' = freshVar x in
  (x', subst (Var x', x) e)

and rename_all names exp =
  List.fold_right
    (fun name (names, exp) ->
       let (name', exp') = rename name exp in
       (name' :: names, exp'))
    names
    ([], exp)

(* Applying a list of substitutions to an expression, leftmost first *)
let subst_list subs exp =
  List.fold_left (fun exp sub -> subst sub exp) exp subs


(* TODO: Write a good set of tests for eval. *)
let eval_tests = [
  (* An example test case.
     Note that you are *only* required to write tests for Rec and Apply!
  *)
  (Let ("x", I 1, Primop (Plus, [Var "x"; I 5])), I 6);
  
  
  (Rec ("x", Int, (Let ("x", I 1, Var "x"))), I 1);
  (Rec ("x", Int, I 0), I 0);
  
  
  (((Apply (Rec ("r", Arrow ([Int], Int), 
                 Fn ([("x", Int)], Var "x" )), 
            [I 3]))),I 3);
  
  
  (Apply (Fn ([], B true), []), B true);
  (Apply (Fn ([("x", Int)], Var "x" ), [I 3]), I 3);
  (Apply (Fn ([("x", Int); ("y", Int)], 
              Primop (Plus, [Var "x"; Var "y"])),[I 4; I 3]), I 7);
  
  
]

(* TODO: Implement the missing cases of eval. *)
let rec eval exp =
  match exp with
  (* Values evaluate to themselves *)
  | I _ -> exp
  | B _ -> exp
  | Fn _ -> exp

  (* This evaluator is _not_ environment-based. Variables should never
     appear during evaluation since they should be substituted away when
     eliminating binding constructs, e.g. function applications and lets.
     Therefore, if we encounter a variable, we raise an error.
*)
  | Var x -> raise (Stuck (Free_variable x))

  (* Primitive operations: +, -, *, <, = *)
  | Primop (po, args) ->
      let args = List.map eval args in
      begin
        match eval_op po args with
        | None -> raise (Stuck Bad_primop_args)
        | Some v -> v
      end

  | If (e, e1, e2) ->
      begin
        match eval e with
        | B true -> eval e1
        | B false -> eval e2
        | _ -> raise (Stuck If_non_true_false)
      end

  | Let (x, e1, e2) ->
      let e1 = eval e1 in
      eval (subst (e1, x) e2)

  | Rec (f, _, e) ->
      
      let eE = eval e in
      begin
        match eE with 
        | B x -> eval (B x)
        | I x -> eval (I x)
        | Var x -> eval (Var x)
        | Fn (li, b) -> 
            eval (subst (exp, f) eE)
        | _ -> eval eE 
      end        
  
  | Apply (e, es) -> 
  
      let applyInput = List.map (fun x -> (eval x)) es in 
      let vE = eval e in
      
      match vE with
      | Fn (li, b) ->
          let funcInput = List.map (fun (m, _) -> m) li in 
          if List.length funcInput != List.length applyInput 
          then raise (Stuck Arity_mismatch) 
          else
            eval (subst_list (List.combine applyInput funcInput) b) 
      | _ -> eval vE
          
             

(* TODO: Write a good set of tests for infer. *)
let infer_tests = [
  (* An example test case.
     Note that you are *only* required to write tests for Rec, Fn, and Apply!
  *)
  (([("x", Int)], Var "x"), Int);
  
  ((["x", Bool], Var "x"), Bool);
  
  (*fn*)
  (([("x", Int)], 
    Fn ([], Primop (Plus, [I 3; I 1]))), 
   Arrow ([], Int));
  
  (([("x", Int)], 
    Fn ([("x", Int)], Primop (Plus, [Var "x"; I 1]))), 
   Arrow ([Int], Int));
  
  (([("x", Int); ("y", Int)], 
    Fn ([("x", Int); ("y", Int)], 
        Primop (Plus, [Var "x"; Var "y"]))), 
   Arrow ([Int; Int], Int));
  
  (*rec*)
  (([("x", Int)], 
    (Rec ("a", (Arrow ([Int], Int)), 
          (Fn ([("x", Int)],
               If (Primop (LessThan, [Var "x"; I 0]), 
                   I 0,
                   Apply (Var "a", [Primop (Minus, [Var "x"; I 1])]))))))), 
   (Arrow ([Int], Int))); 
  
  (([("n", Int)], (Rec ("a", Int, I 3))), Int);
  
  (*apply*)
  (([("a", Int)], 
    Apply (
      Fn ([], Primop (Plus, [I 3; I 4])), 
      [])), Int);
  
  (([("a", Int)], 
    Apply (
      Fn ([("x", Int)], 
          Primop (Plus, [Var "x"; I 3])), 
      [Var "a"] )), Int);
  
  (([("a", Int); ("b", Int)], 
    Apply (
      Fn ([("x", Int); ("y", Int)], Primop (Plus, [Var "x"; Var "y"])), 
      [Var "a"; Var "b"] )), Int)

]

(* TODO: Implement the missing cases of infer. *)
let rec infer ctx e =
  match e with
  | Var x ->
      begin
        try lookup x ctx
        with Not_found -> raise (TypeError (Free_variable x))
      end
  | I _ -> Int
  | B _ -> Bool

  | Primop (po, exps) ->
      let (domain, range) = primopType po in
      check ctx exps domain range

  | If (e, e1, e2) ->
      begin
        match infer ctx e with
        | Bool ->
            let t1 = infer ctx e1 in
            let t2 = infer ctx e2 in
            if t1 = t2 then t1
            else type_mismatch t1 t2
        | t -> type_mismatch Bool t
      end

  | Let (x, e1, e2) ->
      let t1 = infer ctx e1 in
      infer (extend ctx (x, t1)) e2

  | Rec (f, t, e) -> 
      begin
        (*
          let freeL = free_variables (subst (e, f) e) in 
          let freeLL = List.fold_left (fun acc a -> acc@[(a, Int); (a, Bool)]) [] freeL in
        *)
        
        let tE = 
          match e with 
          | Var p -> 
              if p = f 
              then t 
              else infer (extend ctx (f, t)) (subst (e, f) e) 
          | _ -> infer (extend ctx (f, t)) (subst (e, f) e)
        in 
        
        match (tE, t) with 
        | (a, b) -> 
            if a = b 
            then a
            else 
              raise (TypeError (Type_mismatch (b, a)))
          
      end 
  | Fn (xs, e) -> 
  
      let freeLL = List.filter (fun (m,_) -> List.mem m (free_variables e)) xs in
      let tyL = List.map (fun (_, b) -> b) xs in
      let tyE = infer (extend_list ctx freeLL) e in 
      
      (Arrow (tyL, tyE))
  
  | Apply (e, es) -> 
      begin
        
        let tE = infer ctx e in
        let tES = List.map (fun x -> (infer ctx x)) es in
  
        match tE with 
        | Arrow (tL, t2) ->
            if tL = tES then t2
            else if
              List.length tL != List.length tES then 
              raise (TypeError Arity_mismatch)
            else 
              let (expt, infered) = 
                let rec findD l1 l2 =
                  match (l1, l2) with 
                  | (a::b, c::d) -> if a != c then (a, c) else findD b d
                          
                in findD tL tES 
              in
              type_mismatch expt infered
        | _ -> raise (TypeError (Apply_non_arrow tE))
        
      end

and check ctx exps tps result =
  match exps, tps with
  | [], [] -> result
  | e :: es, t :: ts ->
      let t' = infer ctx e in
      if t = t' then check ctx es ts result
      else type_mismatch t t'
  | _ -> raise (TypeError Arity_mismatch)
