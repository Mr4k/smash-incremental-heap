open Core

module Inc : Incremental.S = Incremental.Make ()

open Inc

(*
  Incremental uses a "constant time heap" to order the nodes. This boils down to a fixed size array
  of linked lists (each element in the array represents a possible node height). This structure allows 
  constant time push/pop vs log n for a typical heap. However it has the flaw of having a fixed height.
  We can exploit this flaw to force incremental to crash in a fun way...

  The idea is that we have two "walking nodes". These nodes will keep switching which one is parenting the other.
  Each tim stablize is called, the pseudo-height of the current child will be set to the pseudo-height
  of the parent + 1. So swapping the parent child dynamic twice will move increase both node's pseudo-high
  by 1. This will make them "walk" down the heap, until they pass the default max height of 128 which will
  cause an error.
*)
let toggle = Var.create 1

let node_a_parent_ref = ref (Var.watch toggle)

let node_b_parent_ref = ref (Var.watch toggle)

(*
  We abuse a combination of refs and binds to allows us to dynamic switch where a node's
  parent is pointing.
*)
let node_a_parent = bind (Var.watch toggle) ~f:(fun _ -> !node_a_parent_ref)

let node_b_parent = bind (Var.watch toggle) ~f:(fun _ -> !node_b_parent_ref)

(*
  These are our walking nodes. The contents of the function inside the map is unimportant
*)
let walking_node_a = map node_a_parent ~f:(fun x -> x)

let walking_node_b = map node_b_parent ~f:(fun y -> y)

let a_observation = observe walking_node_a

let b_observation = observe walking_node_b

let () =
  while 0 < 1 do
    (*
      Note that because of the binds there are techincally 5 nodes in the graph.
      A and B both have bind nodes "above" them.
      Therefore is the max node height increases by 2 each swap of 1, for a grand total of 4 each cycle.
    *)
    printf "max pseudo-height %i\n" (Inc.State.max_height_seen Inc.State.t);
    print_endline "walking...";
    (*
      We flip the variable toggle back and forth so the nodes actually fire. Incremental nodes typically only
      fire if the input feeding into them changes. Another way to deal with this would be to change the node's
      cutoff node.
    *)
    Var.set toggle 1;
    (*
      Interestingly because of imperfections in Incremental's cycle detection
      we cannot simply swap the parent and the child and then call stablize.
      If we tried, Incremental would think there was a cycle. So instead we "de-parent" both nodes
      before we swap the order and then stablize before swapping. We need to observe both nodes while 
      we do this because otherwise they will not be updated.
    *)
    node_a_parent_ref := Var.watch toggle;
    node_b_parent_ref := Var.watch toggle;
    stabilize ();
    Var.set toggle 0;
    node_b_parent_ref := walking_node_a;
    stabilize ();
    Var.set toggle 1;
    node_a_parent_ref := Var.watch toggle;
    node_b_parent_ref := Var.watch toggle;
    stabilize ();
    Var.set toggle 0;
    node_a_parent_ref := walking_node_b;
    stabilize ()
  done
