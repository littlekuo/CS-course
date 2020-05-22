open List

(** definition of avlnode *)

type ('k, 'v) avlnode =
  | Leaf
  | Node of int * 'k * 'v * ('k, 'v) avlnode * ('k, 'v) avlnode


let height (n : ('k, 'v) avlnode) : int =
    match  n with
      | Leaf -> -1
      | Node (h, _ , _ , _, _) -> h

let create k v l r  =  Node(1 + max (height l) (height r), k, v, l, r)


(* find *)
let rec get (n : ('k, 'v) avlnode) (key : 'k) : 'v option =
  match n with
    | Leaf -> None
    | Node(_, k, v, left, right) ->
      if k = key then Some(v)
      else if key < k then (get left key)
      else (get right key)

let get_elems n = 
   match n with
   | Leaf -> failwith("empty tree")
   | Node(_, k, v, l, r) -> k, v, l, r 
   

let left_left n =
   let k, v , l, r = get_elems n in
      let lk , lv, ll, lr = get_elems l in
         create lk lv ll (create k v lr r)

let right_right n = 
    let k,v,l,r = get_elems n in 
       let rk,rv,rl,rr = get_elems r in
         create rk rv (create k v l rl) rr


(* A useful helper function (you may need to write more) *)
let balance (n : ('k, 'v) avlnode) : ('k, 'v) avlnode =
    let k,v,l,r = get_elems n in
     if (height l)-(height r) > 1 then
       let _, _, ll, lr = get_elems l in 
          left_left ( 
             if (height ll)-(height lr) < 0 then 
                create k v (right_right l) r
             else n
           )  
    else if (height l)-(height r) < -1 then
      let _, _, rl, rr = get_elems r in 
         right_right (
           if (height rl)-(height rr) > 0 then 
             create k v l (left_left r)
           else n
         )
     else n

(* Produce a new avltree that contains the given key.  If the key already
   exists, update the value to the new value *)
let rec set (n : ('k, 'v) avlnode) (key : 'k) (value : 'v) : ('k, 'v) avlnode =
    balance (
    match n with
    | Leaf -> create key value Leaf Leaf
    | Node (x, k , v, left, right) ->
      if k = key then create k value left right
      else if key < k then create k v (set left key value) right
      else create  k  v  left (set right key value)
    )


(* Return a list of tuples containing the elements of the tree *)
let rec inorder (n : ('k, 'v) avlnode) : ('k * 'v) list =
   match n with 
    | Leaf -> []
    | Node(_, k, v , left, right) ->
       append (inorder left) (cons (k,v) (inorder right)) 


(* Write the functions below (contains, height, add_all, sum) without using
any recursion.  Use the functions above, and map/filter/fold if necessary, to
build up the behavior from existing functions. *)

let contains (n : ('k, 'v) avlnode) (key : 'k) : bool = 
   match get n key with
   | None -> false
   | Some _ -> true

(* given an AVL tree and a list of key/value tuples, set all the given keys
(first pair components) to the corresponding value (second pair components) *)
let add_all (n : ('k, 'v) avlnode) (keys : ('k * 'v) list) : ('k, 'v) avlnode =
   fold_left (fun n pair -> set n (fst pair) (snd pair)) n keys 

(* Return the total value of all the integers in a tree that has int values *)
let sum (n : ('k, int) avlnode) : int =
   fold_left (fun x pair -> x + snd pair)  0 (inorder n)


