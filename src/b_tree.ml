(** A binary search tree implementation in OCaml. *)

exception TreeIsEmpty

(** A binary search tree. *)
type 'a b_tree =
  | Empty
  | Leaf of int * 'a
  | Node of int * 'a * 'a b_tree option * 'a b_tree option

(** Normalizes a tree into a Node.
    Raise TreeIsEmpty if the tree is empty. *)
let node_of_tree tree =
  match tree with
    | Empty -> raise TreeIsEmpty
    | Leaf(k, v) -> Node (k, v, None, None)
    | Node _ -> tree

(** Attempts to find a node with the given key in the tree.
    Raises Not_found if none is found. *)
let rec search tree key =
  match tree with
    | Empty -> raise Not_found
    | Leaf(k, v) -> if k = key then v else raise Not_found
    | Node(k, v, l, r) ->
        if k = key then v
        else begin
          (* If left is not none, try to search it. *)
          match l with
            | Some left -> begin
                if key < k then
                  search left key
                else match r with
                  | None -> raise Not_found
                  | Some right -> search right key
            end
            | None -> begin
                (* Left is none, try to match against right. *)
                match r with
                  | None -> raise Not_found
                  | Some right -> search right key
              end
        end

(** Inserts, overwrites, an item with the given key, and gives it value. *)
let rec insert tree key value =
  match tree with
    | Empty -> Leaf(key, value)
    | Leaf(k, v) ->
        (* Leaves become nodes, unless you are writing the same key. *)
        if key = k then Leaf(k, value)
        else if key < k then
          Node (k, v, None, (Some (Leaf (key, value))))
        else Node (k, v, (Some (Leaf (key, value))), None)
    | Node(k, v, l, r) ->
        (* The logic is identical to the leaf logic.
         * However, node children can be None.
         * So, we match against None, and create leaves if 
         * necessary. *)
        if key = k then Node (k, value, l, r)
        else if key < k then
          match l with
            | None -> Node (k, v, Some (Leaf(key, value)), r)
            | Some left -> Node (k, v, Some (insert left key value), r)
        else
          match r with
            | None -> Node (k, v, l, Some (Leaf(key, value)))
            | Some right -> Node (k, v, l, Some (insert right key value))

(** Traverses the tree, executing the given callback on all key-value pairs. *)
let rec traverse callback tree =
  match tree with
    | Empty -> ()
    | Leaf (k, v) -> callback (k, v)
    | Node (k, v, l, r) ->
        callback (k, v);
        match l with
          | None -> ()
          | Some left -> traverse callback left;
        match r with
          | None -> ()
          | Some right -> traverse callback right;
        ()

(** Deletes the item with the given key from the tree. *)
let rec delete tree key =
  None
