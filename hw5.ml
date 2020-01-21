(* TODO: Implement the functions in this module.
   Do not change the type signature!
*)
module Float: (METRIC with type t = float) =
struct
  type t = float

  (* TODO: Update this value. *)
  let unit = 1.0

  (* TODO: Implement the following functions. *)
  let plus x y = x+.y
  let prod n x = n*.x
  let toString x =  string_of_float x
  let toFloat x = x
  let fromFloat x = x
end

(* TODO: Use the module Float to create different representations for:
   Meter, KM, Feet, and Miles.
   Uncomment the following and replace the "???" with your solution.
*)

module Meter = (Float:METRIC) 
module KM =  (Float:METRIC)
module Feet =  (Float:METRIC)
module Miles =  (Float:METRIC)


(* TODO: Implement the functor Speed. *)
module Speed (M: METRIC): (SPEED with type distance = M.t) =
struct
  type s = float
  type distance = M.t

  (* TODO: Implement the following functions. *)
  let speed (m: distance) (h: Hour.t) =
    (M.toFloat m) /. (Hour.toFloat h) 
  let average s = 
    (*inputs a list*)
    (List.fold_left (fun x y -> x+.y) 0.0 s) /. float_of_int (List.length s) 
  let toFloat s = s

  (* You should not modify this code: it is here for testing purposes. *)
  let speed_as_float m h = toFloat (speed m (Hour.fromFloat h))
  let average_as_float s = toFloat (average s)
end

(* TODO: Use the functor Speed to create modules for computing miles per hour
   and kilometers per hour. Uncomment the following and replace the "???"
   with your solution.
*)

module MilesPerHour =  Speed (Miles)
module KMPerHour =  Speed (KM)


(* Do not remove this line from your code: we need it for testing. *)
module TestModule = Speed (Float)

(* TODO: Write some tests for neighbours. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)


(* ===== assumptions =====
i)   input graphs are well-formed, nodes list = list of all nodes in the graph
     no other values will appear in the edge pairs
ii)  no duplicate nodes or edges
iii) no self-loops
*)

(* ===== special notice =====
   i)  possible for a node in the graph not to be connected to any other nodes
  ii)  edges are directed from v1 to v2 (out-neighbours)
*)


let neighbours_tests: ((string graph * string) * string list) list = [
  (*no nodes*)
  (*no nodes + no required neighbour*)
  (({nodes= [];
     edges= []}, ""), [] ); 
  (*no nodes + required neighbour*)
  (({nodes= [];
     edges= []}, "Toronto"), [] );
  
  (*one node*)
  (*one node + one edge + no required neighbour*)
  (({nodes= ["Montreal"];
     edges= [("Montreal", "Toronto")]}, ""), [] );
  (*one node + one edge + required neighbour + no results*)
  (({nodes= ["Montreal"];
     edges= [("Montreal", "Toronto")]}, "Toronto"), [] );
  (*one node + one edge + required neighbour + with results*)
  (({nodes= ["Montreal"];
     edges= [("Montreal", "Toronto")]}, "Montreal"), ["Toronto"] );
  
  (*one node + multiple edges + no required neighbour + no results*)
  (({nodes= ["Montreal"];
     edges= [("Montreal", "Toronto"); ("Toronto", "Vancouver")]}, ""), [] ); 
  
  (*one node + multiple edges + required neighbour + no results*)
  (({nodes= ["Montreal"];
     edges= [("Montreal", "Toronto"); ("Toronto", "Vancouver")]}, "Vancouver"), [] ); 
  (*one node + multiple edges + required neighbour + with results*)
  (({nodes= ["Montreal"];
     edges= [("Montreal", "Toronto"); ("Toronto", "Vancouver"); ("Montreal", "Vancouver")]}, "Montreal"), ["Toronto"; "Vancouver"] ); 
]

(* TODO: Implement neighbours. *)
let neighbours g vertex = 
  List.fold_left (fun a (x, y) -> if x=vertex then y::a else a) [] g.edges
  
  
(* TODO: Implement find_path. *)
(* only need to return one of the paths *)

let find_path g a b =
  let rec aux_node node visited = 
    
    let listN = neighbours g node in
    
    if node = b then [b] 
    else if List.mem node visited then raise Fail 
    else 
      node::(aux_list listN (node::visited))
            
  and aux_list nodes visited = 
    match nodes with
    | [] -> raise Fail
    | h::t -> try aux_node h visited with 
      | Fail -> aux_list t visited
  in
  aux_node a []
               


(* TODO: Implement find_path'. *)
let find_path' g a b =
  let rec aux_node node visited fc sc =
    
    let listN = neighbours g node in
    
    if (List.mem node visited) || (listN = []) then (fc ())
    else 
      aux_list listN (node::visited) fc (fun r -> sc (node::r))
        
  and aux_list nodes visited fc sc = 
    match nodes with 
    | [] -> fc ()
    | [h] -> if h = b then sc [b] else aux_node h visited fc sc
    | h::t ->if h = b then sc [b] else aux_node h visited (fun () -> aux_list t visited fc sc) sc
  in 
  aux_node a [] (fun () -> raise Fail) (fun r->r)
