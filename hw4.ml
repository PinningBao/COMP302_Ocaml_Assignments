(* TODO: Write some test cases for map_tree. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let map_tree_tests: (((int -> int) * int tree) * int tree) list = [
  (* Remember: your test cases should have this form:
     ((f, t), output)
     The following test case asserts that:
       map_tree identity t
     should have the output
       t
  *)
  
  (*empty*)
  (((fun x -> 3*x), Empty), Empty);
  
  (*single node*)
  ((add1, (Node (0, Empty, Empty, Empty))), (Node (1, Empty, Empty, Empty)));
  
  (*recursive cases*)
  ((add1, 
    Node (1, 
          Node (0, Empty, Empty, (Node (3, 
                                        Node (2, Empty, Empty, Empty), Empty, Empty))),
          Node (0, Empty, Empty, Empty),
          Node (0, Empty, Empty, Empty))), 
   Node (2, 
         Node (1, Empty, Empty, (Node (4, 
                                       Node (3, Empty, Empty, Empty), Empty, Empty))), 
         Node (1, Empty, Empty, Empty),
         Node (1, Empty, Empty, Empty)));
  
  (((fun x -> 0*x), t), (Node ( 0, 
                                Node (0, Empty, Empty, Empty),
                                Node (0, Empty, Empty, Empty),
                                Node (0, Empty, Empty, Empty))));
  
  (((fun x -> 4*x), Node (1, 
                          Node (2, 
                                (Node (5, Empty, Empty, Empty)), Empty, Empty),
                          Node (3, Empty, Empty, Empty),
                          Node (4, Empty, Empty, Empty))), Node (4, 
                                                                 Node (8, 
                                                                       Node(20, Empty, Empty, Empty), Empty, Empty),
                                                                 Node (12, Empty, Empty, Empty),
                                                                 Node (16, Empty, Empty, Empty)));
  (*identity*)
  ((identity, t), t);
]

(* TODO: Implement map_tree. *)
let rec map_tree f t =
  match t with 
  | Empty -> Empty
  | Node (h, i, j, k) -> Node (f h, 
                               map_tree f i, 
                               map_tree f j, 
                               map_tree f k)

(* TODO: Implement delete_data. *)
let delete_data t = 
  map_tree (fun (x, y) -> x) t

(* TODO: Write some test cases for fold_tree. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let fold_tree_tests:
  (((int * int * int * int -> int) * int * int tree) * int) list =
  [
  (* Remember: your test cases should have this form:
     ((f, e, t), output))
     Where:
     - f is a function of type int * int * int * int -> int
     - e has type int
     - t is a tree of type int tree
     - output has type int.
    *)
    (((fun _ -> 0), 3, t), 0);
    (((fun (w, x, y, z) -> w+x+y+z), 2, Empty), 2 );
    (((fun (w, x, y, z) -> w+x+y+z), 2, Node (3, Empty, Empty, Empty)), 9);
    (((fun (w, x, y, z) -> w+x+y+z), 2, t), 28);
  ]

(* TODO: Implement fold_tree. *)
let rec fold_tree f e t = match t with 
  | Empty -> e
  | Node (x, w, y, z) ->
      f (x , fold_tree f e w , fold_tree f e y , fold_tree f e z)

(* TODO: Write some test cases for size. *)
let size_tests: (int tree * int) list = [
  (t, 4);
  (Empty, 0);
  (Node (0, Empty, Empty, Empty), 1);
]

(* TODO: Implement size. *)
let size t = 
  fold_tree (fun (w, x, y, z) -> 1 + x + y + z) 0 t
    
(* TODO: Write some test cases for reflect. *)
let reflect_tests: (int tree * int tree) list = [
  (t, Node(
      1, 
      Node(4, Empty, Empty, Empty),
      Node(3, Empty, Empty, Empty),
      Node(2, Empty, Empty, Empty))) ;
  (Empty, Empty);
  (Node (4, Empty, Empty, Empty), Node(4, Empty, Empty, Empty))
]

(* TODO: Implement reflect. *)
let reflect t =
  fold_tree (fun (w, x, y, z) -> Node (w, z, y, x)) Empty t

(* TODO: Write some test cases for postorder. *)
let postorder_tests: (int tree * int list) list = [
  (t, [2;3;4;1]);
  (Empty, []);
  (Node (9, Empty, Empty, Empty), [9])
]

(* TODO: Implement postorder. *)
let postorder t = 
  fold_tree (fun (w, x, y, z) -> x @ y @ z @ [w] ) [] t
    
(* TODO: Implement add_head. *)
(* x is of type (float * 'a)*)
(* head is of type 'a lcell ref*)
let add_head x head = 
  let l1 = !head in
  match !l1 with 
  | Nil -> let l0 =ref( Cons (x, ref Nil, ref Nil)) in 
      head := l0
  | Cons (e, _, t) -> 
      (*create a new head node, pre->Nil, next->current l1*)
      let l0 = ref (Cons (x, ref Nil, l1)) in
      (*change memory of l1, pre->l0, next->t*)
      l1:= Cons(e, l0, t);
      (*change where the head node is pointing to->l0*)
      head := l0 
  

(* TODO: Implement remove. *) 
let remove p head = 
  let rec remove' ll = 
    let l0 = !ll in (*l0 = list stored in ll*)
    match l0 with 
    | Nil -> () (*no list*)
    | Cons ((g, _), y, z) -> (*y = pre; z = next*)
        if (p g) then  (*satisfies the predicate*) 
          match !y, !z with 
          | Nil, Nil -> head := ref Nil (*only node*)
          | Nil, Cons (_, w, _) -> (*pre = Nil, it's a head*) 
              w:=Nil;
              head:=z 
          | Cons (_, _, e), Nil -> (*next = Nil, it's a tail*)
              e:=Nil;
          | Cons (q, w, _), Cons (i, _, k) -> (*pre != Nil, next != Nil, in the middle*) 
              let l2 = Cons (q, w, z) in 
              let l3 = Cons (i, y, k) in 
              z:=l3;
              y:=l2; 
        else (*doesn't satisfies the predicate*)
          remove' z 
  in
  remove' !head

    






