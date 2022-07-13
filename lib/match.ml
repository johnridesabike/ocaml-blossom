(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** {0 Weighted maximum matching in general graphs}

    This code is based on a Python implementation by Joris van Rantwijk, who had
    consulted a C implementation by Ed Rothberg.

    Joris's comment from the Python version:
{v
> The algorithm is taken from "Efficient Algorithms for Finding Maximum
> Matching in Graphs" by Zvi Galil, ACM Computing Surveys, 1986. It is based
> on the "blossom" method for finding augmenting paths and the "primal-dual"
> method for finding a matching of maximum weight, both due to Jack Edmonds.
> Some ideas came from the "Implementation of algorithms for maximum matching
> in non-bipartite graphs" by H.J. Gabow, Standford Ph.D. thesis, 1973.
>
> Many terms used in the comments (sub-blossom, T-vertex) come from the paper
> by Galil; read the paper before reading this code.
v}
*)

(** {1 The types} *)

module PList = struct
  (** The parity list GADT takes two extra type parameters: one is the current
      item's parity, and the other is the parity of its neighboring items. These
      types swap with each new item, creating the even-odd pattern.

      Although our GADT enables a higher level of polymorphism than an ADT
      parity list, the type checker still cannot append two lists of arbitrary
      parities and know the parity of the resulting list.

      As long as a and b may or may not be equal parities, then this is
      unsolvable: [a + b = ?]

      As long as a and b are not equal, then [a + a = even] and [a + b = odd].*)

  type (_, _, _) t =
    | [] : ('a, [ `Even ], [ `Odd ]) t
    | ( :: ) : ('a * ('a, 'q, 'p) t) -> ('a, 'p, 'q) t

  type 'a even = ('a, [ `Even ], [ `Odd ]) t
  type 'a odd = ('a, [ `Odd ], [ `Even ]) t

  let hd_odd : 'a odd -> 'a = fun (a :: _) -> a

  (** Adding any parity to even equals the same parity. *)
  let rec rev_append_even : type p q. ('a, p, q) t -> 'a even -> ('a, p, q) t =
   fun t acc ->
    match t with
    | [] -> acc
    | [ x ] -> x :: acc
    | x :: y :: t -> rev_append_even t (y :: x :: acc)

  let rev a = rev_append_even a []

  (** Adding two of the same parities equals even. *)
  let[@tail_mod_cons] rec append_same :
      type p q. ('a, p, q) t -> ('a, p, q) t -> 'a even =
   fun a b ->
    match a with
    | [] -> b
    | [ x ] -> x :: b
    | x :: y :: a -> x :: y :: append_same a b

  (** Adding two opposite parities equals odd. *)
  let append_opp : type p q. ('a, p, q) t -> ('a, q, p) t -> 'a odd =
   fun a b -> match a with [] -> b | x :: a -> x :: append_same a b

  let rec fold_left :
      type p q. f:('a -> 'b -> 'a) -> init:'a -> ('b, p, q) t -> 'a =
   fun ~f ~init -> function
    | [] -> init
    | a :: tl -> fold_left tl ~init:(f init a) ~f

  let rec iter : type p q. f:('a -> unit) -> ('a, p, q) t -> unit =
   fun ~f -> function
    | [] -> ()
    | a :: tl ->
        f a;
        iter tl ~f

  let rec iter_even ~f = function
    | [] -> ()
    | a :: b :: tl ->
        f a b;
        iter_even tl ~f

  let pp pp ppf l =
    let sep = ref false in
    Format.fprintf ppf "@[<hov 1>[";
    iter
      ~f:(fun x ->
        if !sep then Format.fprintf ppf ";@ ";
        pp ppf x;
        sep := true)
      l;
    Format.fprintf ppf " ]@]"
end

type stage = Not_endstage | Endstage
type allowable = Not_allowed | Allowed

type 'v graph = {
  vertices : 'v vertex list;
  mutable blossoms : 'v blossom list;
  mutable next_blossom : int;
  max_weight : float;
  edges : 'v edge list;
  mutable mates : ('v * 'v endpoint) list;
  mutable debug : (Format.formatter -> 'v -> unit) option;
}

and 'v edge = {
  i : 'v vertex;  (** Not modified by the algorithm. *)
  j : 'v vertex;  (** Not modified by the algorithm. *)
  weight : float;  (** Not modified by the algorithm. *)
  mutable allowable : allowable;
      (** If [Allowed], the edge has zero slack in the optimization problem. If
          [Not_allowed], the edge's slack may or may not be zero. *)
}
(** Edges represent a weighted connection between two vertices. *)

(** An endpoint represents where an edge connects to a vertex;
    E.g.: [I edge] represents the vertex at [edge.i]. *)
and 'v endpoint = I of 'v edge | J of 'v edge

and ('v, 'value, 'fields) base_node = {
  value : 'value;
      (** For a vertex, this is the data received from the input. It can be any
          type. For a blossom, it is an [int]. *)
  mutable parent : 'v blossom option;
      (** The node's immediate parent (sub-)blossom, or [None] if the vertex is
          a top-level blossom. *)
  mutable dual_var : float;
      (** The node's variable in the dual optimization problem. *)
  mutable best_edge : 'v edge option;
      (** If the node is free (or unreached inside a T-blossom), its best edge
          is the edge to an S-vertex with least slack, or [None] if there is no
          such edge. If it is a (possibly trivial) top-level S-blossom, its best
          edge is the least-slack edge to a different S-blossom, or [None] if
          there is no such edge. This is used for efficient computation of
          delta2 and delta3. *)
  mutable label : 'v label;
      (** The label of the node is found by looking at the label of its
          top-level containing blossom. If the node is inside a T-blossom, its
          label is T if it is reachable from an S-vertex outside the blossom.
          Labels are assigned during a stage and reset after each augmentation.
          *)
  fields : 'fields;
}

and 'v vertex_fields = {
  mutable neighbors : 'v endpoint list;
      (** A list of remote endpoints of the edges attached to the vertex.
          Not modified by the algorithm. *)
  mutable in_blossom : 'v node;
      (** The top-level blossom to which the vertex belongs. If the
          vertex is a top-level blossom, then [in_blossom] will point to itself.
          Initially, all vertices are top-level blossoms, and their own
          in_blossoms. *)
}

and 'v vertex = ('v, 'v, 'v vertex_fields) base_node
(** Vertices represent nodes of the input graph. *)

and 'v blossom_fields = {
  mutable children : 'v child PList.odd;
      (** A list of the blossom's sub-blossoms, starting with the base and going
          around the blossom. *)
  mutable blossom_best_edges : 'v blossom_best_edges list;
      (** A list of least-slack edges to neighboring S-blossoms. This is used for
          efficient computation of delta3. *)
}
(** Blossoms, also called "super-vertices," are nodes that contain vertices and
    other blossoms. *)

and 'v blossom_best_edges = { w : 'v node; edge : 'v edge }
and 'v blossom = ('v, int, 'v blossom_fields) base_node

and 'v child = {
  node : 'v node;
  endpoint : 'v endpoint;
      (** The endpoint that connects the child to the next child in the list. *)
}

and 'v node = Vertex of 'v vertex | Blossom of 'v blossom

(** Top-level blossoms are either unlabeled ("free" labeled S with no endpoint,
    S with an endpoint, or T with an endpoint.

    The label endpoint for a top-level blossom is the remote endpoint of the
    edge through which the blossom obtained its label.

    If a vertex is inside a T blossom and is also labeled T, then the endpoint
    is the remote endpoint of the edge through which the vertex is reachable
    from outside the blossom. *)
and 'v label =
  | Free
  | S_single  (** Only assigned when a stage begins. *)
  | S of 'v endpoint
  | T of 'v endpoint

(** {1 Accessor and utility functions} *)

module Edge = struct
  type 'v t = 'v edge

  (** Return the slack of the given edge. Does not work inside blossoms.*)
  let slack { i; j; weight; _ } = i.dual_var +. j.dual_var -. weight

  let pp pp ppf k =
    let i = k.i.value in
    let j = k.j.value in
    let w = k.weight in
    Format.fprintf ppf "@[<hov 1>{i = %a;@ j = %a;@ weight = %g}@]" pp i pp j w
end

module Endpoint = struct
  let to_edge (J k | I k) = k
  let to_vertex = function I k -> k.i | J k -> k.j
  let rev = function I k -> J k | J k -> I k

  (** This is equivalent to, but more performant than, [rev |> to_vertex]. *)
  let rev_to_vertex = function I edge -> edge.j | J edge -> edge.i

  let pp pp ppf = function
    | I edge -> Format.fprintf ppf "(I %a)" (Edge.pp pp) edge
    | J edge -> Format.fprintf ppf "(J %a)" (Edge.pp pp) edge
end

module Vertex = struct
  type 'v t = 'v vertex

  let equal a b =
    (* Use [compare a b = 0] instead of [a = b] to work with nan floats. *)
    compare a.value b.value = 0

  let pp pp ppf v = pp ppf v.value
end

module Blossom = struct
  type 'v t = 'v blossom

  let equal a b = Int.equal a.value b.value
  let pp ppf b = Format.pp_print_int ppf b.value
end

module Node = struct
  type 'v t = 'v node

  (** A blossom's base is the vertex at the head of its list of children. *)
  let rec base = function
    | Vertex vertex -> vertex
    | Blossom { fields = { children = { node; _ } :: _; _ }; _ } -> base node

  let equal a b =
    match (a, b) with
    | Vertex a, Vertex b -> Vertex.equal a b
    | Blossom a, Blossom b -> Blossom.equal a b
    | Vertex _, Blossom _ | Blossom _, Vertex _ -> false

  let equal_blossom a b =
    match a with Blossom a -> Blossom.equal a b | Vertex _ -> false

  let label (Vertex { label; _ } | Blossom { label; _ }) = label

  let pp pp ppf = function
    | Vertex { value; _ } -> Format.fprintf ppf "Vertex %a" pp value
    | Blossom { value; _ } -> Format.fprintf ppf "Blossom %i" value

  module Leaves = struct
    (** Fold over the leaves of a node. Leaves are the vertices in a blossom's
        children, as well as the vertices in any of its sub-blossom's children.
        *)
    let rec fold ~init ~f = function
      | Vertex vertex -> f init vertex
      | Blossom { fields = { children; _ }; _ } ->
          PList.fold_left children ~init ~f:(fun init { node; _ } ->
              fold node ~init ~f)

    let to_list l = fold ~f:(Fun.flip List.cons) l

    let pp pp ppf b =
      Format.fprintf ppf "@[<hov 1>[%a ]@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
           (Vertex.pp pp))
        (to_list ~init:[] b)
  end
end

module Child = struct
  type 'v t = 'v child

  let pp pp ppf { node; endpoint } =
    Format.fprintf ppf "@[<hov 1>{node = %a;@ endpoint = %a}@]" (Node.pp pp)
      node (Endpoint.pp pp) endpoint
end

module Mates = struct
  (** Maps vertices to remote endpoints of their attached edges. *)

  let find v { mates; _ } = List.assoc v.value mates

  let add_edge edge graph =
    graph.mates <-
      (edge.i.value, J edge) :: (edge.j.value, I edge) :: graph.mates

  let add v p graph = graph.mates <- (v.value, p) :: graph.mates
  let mem v { mates; _ } = List.mem_assoc v.value mates

  let pp pp ppf { mates; _ } =
    let sep = ref false in
    Format.fprintf ppf "@[<hov 1>(";
    List.iter
      (fun (k, v) ->
        if !sep then Format.fprintf ppf ";@ ";
        Format.fprintf ppf "%a -> %a" pp k (Endpoint.pp pp) v;
        sep := true)
      mates;
    Format.fprintf ppf " )@]"
end

module Label = struct
  let pp pp ppf = function
    | Free -> Format.pp_print_string ppf "Free"
    | S_single -> Format.pp_print_string ppf "S_single"
    | S endpoint -> Format.fprintf ppf "(S %a)" (Endpoint.pp pp) endpoint
    | T endpoint -> Format.fprintf ppf "(T %a)" (Endpoint.pp pp) endpoint

  (** Label a vertex S and add its in_blossom's children to the queue. *)
  let assign_s ~v ~label ~queue ~graph =
    let b = v.fields.in_blossom in
    (match graph.debug with
    | None -> ()
    | Some pp' ->
        Format.printf
          "assign label: @[<v>Vertex = %a@ Blossom = %a@ Label = %a@]@."
          (Vertex.pp pp') v (Node.pp pp') b (pp pp') label;
        Format.printf "PUSH %a@." (Node.Leaves.pp pp') b);
    (match b with
    | Blossom b ->
        b.label <- label;
        b.best_edge <- None;
        v.best_edge <- None;
        v.label <- label
    | Vertex _ ->
        v.best_edge <- None;
        v.label <- label);
    Node.Leaves.to_list b ~init:queue

  (** Label a vertex T, label its mate S, and add its mate's in_blossom's
      children to the queue. *)
  let assign_t ~v ~p ~graph ~queue =
    let b = v.fields.in_blossom in
    let label = T p in
    (match graph.debug with
    | None -> ()
    | Some pp' ->
        Format.printf
          "assign label: @[<v>Vertex = %a@ Blossom = %a@ Label = %a@]@."
          (Vertex.pp pp') v (Node.pp pp') b (pp pp') label);
    (match b with
    | Blossom b ->
        b.label <- label;
        b.best_edge <- None;
        v.best_edge <- None;
        v.label <- label
    | Vertex _ ->
        v.best_edge <- None;
        v.label <- label);
    let matep = Mates.find (Node.base b) graph in
    let mate = Endpoint.to_vertex matep in
    assign_s ~v:mate ~label:(S (Endpoint.rev matep)) ~queue ~graph

  (** Label a vertex T without stepping through to its mate. *)
  let assign_t_single_vertex ~v ~p = v.label <- T p

  (** Label a vertex or blossom T without stepping through to its mate. *)
  let assign_t_single ~w ~p =
    match w with
    | Vertex v -> assign_t_single_vertex ~v ~p
    | Blossom b -> b.label <- T p
end

(** {1 Let's start making a graph} *)

module OrderedEdge : sig
  type 'v t = private Edge of 'v * 'v

  val make : 'v -> 'v -> 'v t option
end = struct
  type 'v t = Edge of 'v * 'v

  (** It's important that edges are always in the same order. *)
  let make i j =
    match compare i j with
    | 0 -> None
    | x when x > 0 -> Some (Edge (i, j))
    | _ -> Some (Edge (j, i))
end

module Graph = struct
  let rec make_aux ~edges ~ord_edges ~vertices ~max_weight = function
    | [] ->
        (* Once all the edges have processed, loop over the vertices again
           to set the dual_vars and then return the graph. *)
        let vertices =
          List.map
            (fun (_, v) ->
              v.dual_var <- max_weight;
              v)
            vertices
        in
        {
          vertices;
          blossoms = [];
          max_weight;
          edges;
          next_blossom = 0;
          mates = [];
          debug = None;
        }
    | (i_value, j_value, weight) :: tl -> (
        match OrderedEdge.make i_value j_value with
        | None -> make_aux tl ~edges ~ord_edges ~vertices ~max_weight
        | Some k when List.mem k ord_edges ->
            (* Ignore duplicate edges. *)
            make_aux tl ~edges ~ord_edges ~vertices ~max_weight
        | Some (Edge (i_value, j_value) as k) -> (
            let max_weight = max max_weight weight in
            let ord_edges = k :: ord_edges in
            (* See if i or j are already created.
               If they are, then update them. *)
            match
              List.(assoc_opt i_value vertices, assoc_opt j_value vertices)
            with
            | Some i, Some j ->
                let k = { i; j; weight; allowable = Not_allowed } in
                i.fields.neighbors <- J k :: i.fields.neighbors;
                j.fields.neighbors <- I k :: j.fields.neighbors;
                make_aux tl ~edges:(k :: edges) ~ord_edges ~vertices ~max_weight
            | Some i, None ->
                let rec k = { i; j; weight; allowable = Not_allowed }
                and j =
                  {
                    value = j_value;
                    parent = None;
                    dual_var = 0.;
                    best_edge = None;
                    label = Free;
                    fields = { neighbors = [ I k ]; in_blossom = Vertex j };
                  }
                in
                i.fields.neighbors <- J k :: i.fields.neighbors;
                make_aux tl ~edges:(k :: edges) ~ord_edges
                  ~vertices:((j_value, j) :: vertices) ~max_weight
            | None, Some j ->
                let rec edge = { i; j; weight; allowable = Not_allowed }
                and i =
                  {
                    value = i_value;
                    parent = None;
                    dual_var = 0.;
                    best_edge = None;
                    label = Free;
                    fields = { neighbors = [ J edge ]; in_blossom = Vertex i };
                  }
                in
                j.fields.neighbors <- I edge :: j.fields.neighbors;
                make_aux tl ~edges:(edge :: edges) ~ord_edges
                  ~vertices:((i_value, i) :: vertices) ~max_weight
            | None, None ->
                let rec k = { i; j; weight; allowable = Not_allowed }
                and i =
                  {
                    value = i_value;
                    parent = None;
                    dual_var = 0.;
                    best_edge = None;
                    label = Free;
                    fields = { neighbors = [ J k ]; in_blossom = Vertex i };
                  }
                and j =
                  {
                    value = j_value;
                    parent = None;
                    dual_var = 0.;
                    best_edge = None;
                    label = Free;
                    fields = { neighbors = [ I k ]; in_blossom = Vertex j };
                  }
                in
                make_aux tl ~edges:(k :: edges) ~ord_edges
                  ~vertices:((i_value, i) :: (j_value, j) :: vertices)
                  ~max_weight))

  (** Turn the raw input into a recursive graph.*)
  let make input_edges =
    make_aux input_edges ~edges:[] ~ord_edges:[] ~vertices:[] ~max_weight:0.

  let update_dual_vars_by_delta graph ~delta =
    List.iter
      (fun b ->
        b.dual_var <-
          (match b with
          (* top-level S-blossom: z = z + delta *)
          | { parent = None; label = S_single | S _; _ } -> b.dual_var +. delta
          (* top-level T-blossom: z = z - delta *)
          | { parent = None; label = T _; _ } -> b.dual_var -. delta
          | { parent = Some _; _ } | { label = Free; _ } -> b.dual_var))
      graph.blossoms;
    List.iter
      (fun v ->
        v.dual_var <-
          (match Node.label v.fields.in_blossom with
          (* S-vertex: u = u - delta *)
          | S_single | S _ -> v.dual_var -. delta
          (* T-vertex: u = u + delta *)
          | T _ -> v.dual_var +. delta
          | Free -> v.dual_var))
      graph.vertices
end

(** {1 Add, augment, and expand blossoms} *)

module AddBlossom = struct
  (* First, we trace the graph to see if we are able to add a blossom. *)

  type ('v, 'a) trace_result =
    | Dead_end of 'v Node.t * 'a
    | Found_child of 'v Node.t * 'a

  type 'v scan_result = Augmenting_path | New_blossom of 'v Child.t PList.odd

  (** Trace back to the next S-blossom and add the path of blossoms to the list
      of children. *)
  let trace_backward w back_children =
    match Node.label w with
    | Free | S_single -> Dead_end (w, back_children)
    | T _ -> failwith "Label should only be S."
    | S p -> (
        let w' = (Endpoint.to_vertex p).fields.in_blossom in
        match Node.label w' with
        | Free | S_single | S _ -> failwith "Label should only be T."
        | T p' ->
            let back_children =
              PList.(
                { node = w'; endpoint = Endpoint.rev p' }
                :: { node = w; endpoint = Endpoint.rev p }
                :: back_children)
            in
            let next_w = (Endpoint.to_vertex p').fields.in_blossom in
            Found_child (next_w, back_children))

  (** Trace forward to the next S-blossom and add the path of blossoms to the
      list of children. *)
  let trace_forward v front_children =
    match Node.label v with
    | Free | S_single -> Dead_end (v, front_children)
    | T _ -> failwith "Label should only be S."
    | S p -> (
        let v' = (Endpoint.to_vertex p).fields.in_blossom in
        match Node.label v' with
        | Free | S_single | S _ -> failwith "Label should only be T."
        | T p' ->
            let last_v = (Endpoint.to_vertex p').fields.in_blossom in
            let front_children =
              PList.(
                { node = last_v; endpoint = p' }
                :: { node = v'; endpoint = p }
                :: front_children)
            in
            Found_child (last_v, front_children))

  (** Scan the found children to see if there's a connecting "base" node. *)
  let find_connection last_v next_w front back =
    let rec aux acc = function
      (* If an odd node equals the next w, then return that node and the rest
         of the children. *)
      | PList.({ node; _ } :: _) as t when Node.equal node next_w -> Some t
      (* If an even node equals the last v, return the children that came
         before it. *)
      | a :: { node; _ } :: _ when Node.equal node last_v ->
          Some (PList.rev (a :: acc))
      | [ _ ] -> None
      | a :: b :: tl -> aux (b :: a :: acc) tl
    in
    aux [] (PList.append_opp front (PList.rev back))

  (** Trace back from the given edge's vertices to discover either a new blossom
      or an augmenting path. *)
  let scan_for_blossom ({ i; j; _ } as edge) ~graph =
    (match graph.debug with
    | None -> ()
    | Some pp' ->
        Format.printf "scan for blossom: @[<v>v = %a@ w = %a@]@."
          (Vertex.pp pp') i (Vertex.pp pp') j);
    let rec aux front_path back_path =
      match (front_path, back_path) with
      | Dead_end (_, _), Dead_end (_, _) -> Augmenting_path
      | (Dead_end (last_v, front) as front_path), Found_child (next_w, back)
        -> (
          match find_connection last_v next_w front back with
          (* The first front child was a Single S; the back traced around to it. *)
          | Some children -> New_blossom children
          | None -> aux front_path (trace_backward next_w back))
      | Found_child (last_v, front), (Dead_end (next_w, back) as back_path) -> (
          match find_connection last_v next_w front back with
          (* The first back child was a Single S; the front traced around to it. *)
          | Some children -> New_blossom children
          | None -> aux (trace_forward last_v front) back_path)
      | Found_child (last_v, front), Found_child (next_w, back) -> (
          match find_connection last_v next_w front back with
          | Some children -> New_blossom children
          | None -> (
              match trace_backward next_w back with
              | Found_child (next_w, back) as back_path -> (
                  match find_connection last_v next_w front back with
                  | Some children -> New_blossom children
                  | None -> aux (trace_forward last_v front) back_path)
              | Dead_end _ as back_path ->
                  aux (trace_forward last_v front) back_path))
    in
    let initial_v = i.fields.in_blossom in
    (* Manually add the i child to connect the two lists. *)
    aux
      (Found_child (initial_v, [ { node = initial_v; endpoint = I edge } ]))
      (Found_child (j.fields.in_blossom, []))

  (* Now we can create a blossom. *)

  (** If the node has an edge set but with a higher slack, then update the node
      with the new edge. If the node has not been set yet, then add it with the
      new edge. *)
  let update_best_edges ~b ~neighbor:w ~best_edges ~edge =
    match Node.label w with
    | (S_single | S _) when not (Node.equal_blossom w b) ->
        let[@tail_mod_cons] rec aux ~has_been_set = function
          | [] -> if has_been_set then [] else [ { w; edge } ]
          | best_edge :: tl ->
              if
                Node.equal w best_edge.w
                && Edge.(slack edge < slack best_edge.edge)
              then { w; edge } :: aux ~has_been_set:true tl
              else best_edge :: aux ~has_been_set tl
        in
        aux ~has_been_set:false best_edges
    | S_single | S _ | Free | T _ -> best_edges

  let compute_best_edges b =
    PList.fold_left b.fields.children ~init:[] ~f:(fun best_edges { node; _ } ->
        let best_edges =
          match node with
          | Vertex _ | Blossom { fields = { blossom_best_edges = []; _ }; _ } ->
              (* This sub-blossom does not have a list of least-slack edges; get
                 the information from the vertices. *)
              Node.Leaves.fold node ~init:best_edges
                ~f:(fun best_edges { fields = { neighbors; _ }; _ } ->
                  List.fold_left
                    (fun best_edges endpoint ->
                      let neighbor =
                        (Endpoint.to_vertex endpoint).fields.in_blossom
                      in
                      let edge = Endpoint.to_edge endpoint in
                      update_best_edges ~b ~neighbor ~best_edges ~edge)
                    best_edges neighbors)
          | Blossom { fields = { blossom_best_edges; _ }; _ } ->
              (* Walk this sub-blossom's least-slack edges. *)
              List.fold_left
                (fun best_edges { edge; _ } ->
                  let neighbor =
                    if Node.equal_blossom edge.j.fields.in_blossom b then
                      edge.i.fields.in_blossom
                    else edge.j.fields.in_blossom
                  in
                  update_best_edges ~b ~neighbor ~best_edges ~edge)
                best_edges blossom_best_edges
        in
        (* Forget about least-slack edges of this sub-blossom. *)
        (match node with
        | Vertex v -> v.best_edge <- None
        | Blossom b ->
            b.best_edge <- None;
            b.fields.blossom_best_edges <- []);
        best_edges)

  (** Construct a new blossom with a given base, containing an edge which
      connects a pair of S vertices. Label the new blossom as S; relabel its
      T-vertices to S and add them to the queue. *)
  let make graph children queue =
    let value = graph.next_blossom in
    graph.next_blossom <- succ value;
    let base_node = (PList.hd_odd children).node in
    let b =
      {
        value;
        parent = None;
        dual_var = 0.;
        best_edge = None;
        label = Node.label base_node;
        fields = { children; blossom_best_edges = [] };
      }
    in
    PList.iter children ~f:(fun { node; _ } ->
        match node with
        | Vertex v -> v.parent <- Some b
        | Blossom b' -> b'.parent <- Some b);
    (* Relabel the vertices. *)
    let blossom = Blossom b in
    let queue =
      Node.Leaves.fold blossom ~init:queue ~f:(fun queue v ->
          let old_label = Node.label v.fields.in_blossom in
          v.fields.in_blossom <- blossom;
          match old_label with
          (* This T-Vertex now turns into an S-vertex because it becomes part
             of an S-blossom; add it to the queue. *)
          | T _ -> v :: queue
          | Free | S_single | S _ -> queue)
    in
    graph.blossoms <- b :: graph.blossoms;

    (* Compute the blossom's best edges. *)
    b.fields.blossom_best_edges <- compute_best_edges b;
    List.iter
      (fun { edge; _ } ->
        b.best_edge <-
          (match b.best_edge with
          | None -> Some edge
          | Some edge' when Edge.(slack edge < slack edge') -> Some edge
          | Some _ as k -> k))
      b.fields.blossom_best_edges;
    queue
end

module ModifyBlossom = struct
  (** When augmenting or expanding a blossom, we need to separate the base
      child, the "entry" child, and the list of children between them. Whether
      the entry child was odd or even will determine whether we move forward or
      backward when processing the children. *)

  type 'v split_children =
    | No_split of { base : 'v Child.t; rest : 'v Child.t PList.even }
    | Go_forward of {
        base : 'v Child.t;
        front : 'v Child.t PList.even;
        entry : 'v Child.t;
        back : 'v Child.t PList.odd;
      }
    | Go_backward of {
        base : 'v Child.t;
        front : 'v Child.t PList.odd;
        entry : 'v Child.t;
        back : 'v Child.t PList.even;
      }

  (** Remove the base child and split the remaining children into two lists, one
      before and one and after the entry child. *)
  let split_children PList.(base :: rest) entry_child =
    (* assert (Node.equal base.node entry_child *)
    let rec aux front back =
      match back with
      | PList.[] -> No_split { base; rest }
      | child :: back when Node.equal child.node entry_child ->
          Go_forward { base; front = PList.rev front; entry = child; back }
      | child :: child' :: back when Node.equal child'.node entry_child ->
          Go_backward
            { base; front = PList.rev (child :: front); entry = child'; back }
      | child :: child' :: back -> aux (child' :: child :: front) back
    in
    aux [] rest

  let rec bubble_blossom_tree node b = function
    | None -> failwith "There should be a parent."
    | Some parent when Blossom.equal parent b -> node
    | Some parent -> bubble_blossom_tree (Blossom parent) b parent.parent

  type direction = Backward | Forward

  (** Swap matched/unmatched edges over an alternating path through a blossom
      between vertex v and the base vertex. Keep blossom bookkeeping
      consistent. *)
  let rec augment b v graph =
    (match graph.debug with
    | None -> ()
    | Some pp ->
        Format.printf
          "augment blossom: @[<v>Blossom = %a@ Vertex = %a@ Mates = %a@]@."
          Blossom.pp b (Vertex.pp pp) v (Mates.pp pp) graph);
    (* Bubble up through the blossom tree from from the vertex to an immediate
       sub-blossom of b. *)
    let t = bubble_blossom_tree (Vertex v) b v.parent in
    (* Recursively deal with the first sub-blossom. *)
    (match t with Blossom b -> augment b v graph | Vertex _ -> ());
    (* Figure out how we'll go 'round the blossom. *)
    let move_list, direction, children =
      match split_children b.fields.children t with
      | No_split _ -> (PList.[], Backward, b.fields.children)
      | Go_forward { base; front; entry; back } ->
          let move_list = PList.append_same back [ base ] in
          (* Rotate the list of sub-blossoms to put the new base at the front.*)
          let children = PList.(entry :: append_same back (base :: front)) in
          (move_list, Forward, children)
      | Go_backward { base; front; entry; back } ->
          let move_list = PList.rev (base :: front) in
          (* Rotate the list of sub-blossoms to put the new base at the front.*)
          let children = PList.(entry :: append_same back (base :: front)) in
          (move_list, Backward, children)
    in
    b.fields.children <- children;
    (* Step into the next two sub-blossoms and augment them recursively. *)
    PList.iter_even
      ~f:(fun child child' ->
        let p =
          match direction with
          | Forward -> child.endpoint
          | Backward -> Endpoint.rev child'.endpoint
        in
        (match child.node with
        | Blossom b -> augment b (Endpoint.to_vertex p) graph
        | Vertex _ -> ());
        (match child'.node with
        | Blossom b -> augment b (Endpoint.rev_to_vertex p) graph
        | Vertex _ -> ());
        (* Match the edge connecting those sub-blossoms. *)
        Mates.add_edge (Endpoint.to_edge p) graph;
        match graph.debug with
        | None -> ()
        | Some pp ->
            Format.printf "PAIR @[<v>v = %a@ w = %a@]@." (Vertex.pp pp)
              (Endpoint.to_vertex p) (Vertex.pp pp) (Endpoint.rev_to_vertex p))
      move_list

  let rec relabel_to_base next_endpoint queue graph direction = function
    | PList.[ _ ] -> (next_endpoint, queue)
    | { endpoint; _ } :: { endpoint = endpoint'; _ } :: rest ->
        (Endpoint.to_edge endpoint).allowable <- Allowed;
        (Endpoint.to_edge endpoint').allowable <- Allowed;
        let v = Endpoint.rev_to_vertex next_endpoint in
        let queue = Label.assign_t ~v ~p:next_endpoint ~graph ~queue in
        let next_endpoint =
          match direction with
          | Forward -> endpoint'
          | Backward ->
              let { endpoint; _ } = PList.hd_odd rest in
              Endpoint.rev endpoint
        in
        (Endpoint.to_edge next_endpoint).allowable <- Allowed;
        relabel_to_base next_endpoint queue graph direction rest

  let pp_endstage ppf = function
    | Endstage -> Format.pp_print_string ppf "Endstage"
    | Not_endstage -> Format.pp_print_string ppf "Not endstage"

  (** Expand the given top-level blossom. *)
  let rec expand ~graph ~b ~stage ~queue =
    (match graph.debug with
    | None -> ()
    | Some pp' ->
        Format.printf
          "expand blossom: @[<v>Blossom = %a@ Endstage = %a@ Children = %a@]@."
          Blossom.pp b pp_endstage stage
          (PList.pp (Child.pp pp'))
          b.fields.children);
    (* Convert sub-blossoms into top-level blossoms. *)
    let queue =
      PList.fold_left b.fields.children ~init:queue ~f:(fun queue child ->
          match child.node with
          | Vertex v as vertex ->
              v.fields.in_blossom <- vertex;
              v.parent <- None;
              queue
          | Blossom b as blossom -> (
              b.parent <- None;
              match (b, stage) with
              | { dual_var = 0.; _ }, Endstage ->
                  (* Recursively expand this sub-blossom. *)
                  expand ~graph ~b ~stage ~queue
              | _ ->
                  (* This sub-blossom is becoming a top-level blossom, so change
                     its children's [in_blossom] to it. *)
                  Node.Leaves.fold blossom ~init:() ~f:(fun _ v ->
                      v.fields.in_blossom <- blossom);
                  queue))
    in
    let queue =
      match (b.label, stage) with
      (* If we expand a T-blossom during a stage, its sub-blossoms must be
         relabeled. *)
      | T label_endpoint, Not_endstage ->
          (* Start at the sub-blossom through which the expanding blossom
             obtained its label, and relabel sub-blossoms until we reach the
             base. Figure out through which sub-blossom the expanding blossom
             obtained its label initially. *)
          let entry_node =
            (Endpoint.rev_to_vertex label_endpoint).fields.in_blossom
          in
          let base, p, children_to_entry_child, queue =
            match split_children b.fields.children entry_node with
            (* If the base is the entry child, don't relabel to the base but do
               process the rest of the children. *)
            | No_split { base; rest } -> (base.node, label_endpoint, rest, queue)
            | Go_forward { base; front; entry; back } ->
                let endpoint, queue =
                  relabel_to_base label_endpoint queue graph Forward
                    (entry :: PList.append_same back [ base ])
                in
                (base.node, endpoint, front, queue)
            | Go_backward { base; front; entry; back } ->
                let endpoint, queue =
                  relabel_to_base label_endpoint queue graph Backward
                    (entry :: PList.rev (base :: front))
                in
                (base.node, endpoint, back, queue)
          in
          (* Relabel the base T-sub-blossom WITHOUT stepping through to its
             mate. *)
          (match base with
          | Blossom b -> b.best_edge <- None
          | Vertex v -> v.best_edge <- None);
          Label.assign_t_single ~w:base ~p;
          Label.assign_t_single_vertex ~v:(Endpoint.rev_to_vertex p) ~p;
          (* Continue along the blossom until we get to the entry child. *)
          PList.fold_left children_to_entry_child ~init:queue
            ~f:(fun queue child ->
              (* Examine the vertices of the sub-blossom to see whether it is
                 reachable from a neighboring S-vertex outside the expanding
                 blossom. *)
              match Node.label child.node with
              (* This sub-blossom just got its label S through one of its
                 neighbors; leave it. *)
              | S_single | S _ -> queue
              | Free | T _ ->
                  (* If the sub-blossom contains a reachable vertex, assign
                     label T to the sub-blossom. *)
                  let rec label_reachable_vertex = function
                    | [] -> queue
                    | v :: rest -> (
                        match v.label with
                        | Free -> label_reachable_vertex rest
                        | T p -> Label.assign_t ~v ~p ~graph ~queue
                        | S_single | S _ ->
                            failwith "Must be labeled Free or T.")
                  in
                  Node.Leaves.to_list child.node ~init:[]
                  |> label_reachable_vertex)
      (* Labels are erased at the end of a stage; no relabeling is necessary. *)
      | T _, Endstage | (Free | S_single | S _), (Endstage | Not_endstage) ->
          queue
    in

    (* Remove the blossom. This reverses the list, but that shouldn't matter. *)
    graph.blossoms <- List.filter (Fun.negate (Blossom.equal b)) graph.blossoms;
    queue
end

(** {1 The main loop *)

module Delta = struct
  type 'v t =
    | One of float
    | Two of float * 'v Edge.t
    | Three of float * 'v Edge.t
    | Four of float * 'v Blossom.t

  let pp ppf = function
    | One delta -> Format.fprintf ppf "One %g" delta
    | Two (delta, _) -> Format.fprintf ppf "Two %g" delta
    | Three (delta, _) -> Format.fprintf ppf "Three %g" delta
    | Four (delta, _) -> Format.fprintf ppf "Four %g" delta

  let get_min_dual_var { vertices; max_weight; _ } =
    List.fold_left
      (fun dual_var' { dual_var; _ } -> min dual_var' dual_var)
      max_weight vertices

  let finalize ~graph = function
    | Some delta -> delta
    | None ->
        (* No further improvement possible; max-cardinality optimum reached.
           Do a final delta update to make the optimum verifiable.
           (Note that we aren't currently verifying the optimum.) *)
        let delta = get_min_dual_var graph in
        One (max delta 0.)

  (** Compute delta4: minimum z variable of any T-blossom. *)
  let four delta ~graph =
    List.fold_left
      (fun delta b ->
        match b with
        | { parent = None; label = T _; dual_var; _ } -> (
            match delta with
            | None -> Some (Four (dual_var, b))
            | Some
                (One delta | Two (delta, _) | Three (delta, _) | Four (delta, _))
              when dual_var < delta ->
                Some (Four (dual_var, b))
            | Some (One _ | Two _ | Three _ | Four _) as delta -> delta)
        | { label = Free | S_single | S _ | T _; _ } -> delta)
      delta graph.blossoms

  let three_helper delta = function
    | { parent = None; best_edge = Some edge; label = S_single | S _; _ } -> (
        let kslack = Edge.slack edge /. 2. in
        match delta with
        | None -> Some (Three (kslack, edge))
        | Some (One delta | Two (delta, _) | Three (delta, _) | Four (delta, _))
          when kslack < delta ->
            Some (Three (kslack, edge))
        | Some (One _ | Two _ | Three _ | Four _) as delta -> delta)
    | { label = Free | S_single | S _ | T _; _ } -> delta

  (** Compute delta3: half the minimum slack on any edge between a pair of
      S-blossoms. *)
  let three delta ~graph =
    let delta = List.fold_left three_helper delta graph.vertices in
    List.fold_left three_helper delta graph.blossoms

  (** Compute delta2: the minimum slack on any edge between an S-vertex and a
      free vertex. *)
  let two delta ~graph =
    List.fold_left
      (fun delta v ->
        match (v.best_edge, Node.label v.fields.in_blossom) with
        | Some edge, Free -> (
            let kslack = Edge.slack edge in
            match delta with
            | None -> Some (Two (kslack, edge))
            | Some
                (One delta | Two (delta, _) | Three (delta, _) | Four (delta, _))
              when kslack < delta ->
                Some (Two (kslack, edge))
            | Some (One _ | Two _ | Three _ | Four _) as delta -> delta)
        | _, (Free | S_single | S _ | T _) -> delta)
      delta graph.vertices

  (** Compute delta1: the minimum value of any vertex dual variable. *)
  let one ~graph = function
    | `Not_max -> Some (One (get_min_dual_var graph))
    | `Max -> None

  let make cardinality ~graph =
    one cardinality ~graph |> two ~graph |> three ~graph |> four ~graph
    |> finalize ~graph
end

module Substage = struct
  (** Each iteration of the loop is a "substage." A substage tries to find an
      augmenting path. If found, the path is used to improve the matching and
      the stage ends. If there is no augmenting path, the primal-dual method is
      used to pump some slack out of the dual variables. *)

  type 'v augmented = Augmented | Not_augmented of 'v Vertex.t list

  (** Match vertex s to remote endpoint p. Then trace back from s until we
      find a single vertex, swapping matched and unmatched edges as we go. *)
  let rec augment_matching_loop graph ~s ~p =
    (match s.fields.in_blossom with
    (* Augment through the S-blossom to the base. *)
    | Blossom b -> ModifyBlossom.augment b s graph
    | Vertex _ -> ());
    (* Update s's mate. *)
    Mates.add s p graph;
    (* Trace one step back. *)
    match Node.label s.fields.in_blossom with
    | Free | T _ -> failwith "Required S vertex."
    | S endpoint -> (
        let t_in_blossom = (Endpoint.to_vertex endpoint).fields.in_blossom in
        match Node.label t_in_blossom with
        | Free | S_single | S _ -> failwith "Required T vertex."
        | T p ->
            (* Trace one step back. *)
            let s = Endpoint.to_vertex p in
            let j = Endpoint.rev_to_vertex p in
            (match t_in_blossom with
            (* Augment through the T-blossom from j to base. *)
            | Blossom bt -> ModifyBlossom.augment bt j graph
            | Vertex _ -> ());
            (* Update j's mate. *)
            Mates.add j p graph;
            (* Keep the opposite endpoint. It will be assigned to s's mate in the
               next step. *)
            augment_matching_loop graph ~s ~p:(Endpoint.rev p))
    (* Reached a single vertex; stop. *)
    | S_single -> ()

  (** Swap matched/unmatched edges over an alternating path between two single
      vertices. The augmenting path runs through the edge which connects a pair
      of S vertices. *)
  let augment_matching edge graph =
    (match graph.debug with
    | None -> ()
    | Some pp ->
        Format.printf "augment matching: @[<v>v = %a@ w = %a@]@." (Vertex.pp pp)
          edge.i (Vertex.pp pp) edge.j;
        Format.printf "PAIR: @[<v>i = %a@ j = %a@]@." (Vertex.pp pp) edge.i
          (Vertex.pp pp) edge.j);
    augment_matching_loop graph ~s:edge.i ~p:(J edge);
    augment_matching_loop graph ~s:edge.j ~p:(I edge)

  let scan_neighbors ~vertex ~graph ~queue =
    let { fields = { in_blossom; neighbors; _ }; _ } = vertex in
    let rec aux ~queue = function
      | [] -> Not_augmented queue
      | endpoint :: neighbors -> (
          let neighbor = Endpoint.to_vertex endpoint in
          (* This edge is internal to a blossom; ignore it. *)
          if Node.equal in_blossom neighbor.fields.in_blossom then
            aux ~queue neighbors
          else
            let edge = Endpoint.to_edge endpoint in
            let kslack = Edge.slack edge in
            (match edge.allowable with
            (* Edge has zero slack => it is allowable. *)
            | Not_allowed when kslack <= 0. -> edge.allowable <- Allowed
            | Allowed | Not_allowed -> ());
            match edge.allowable with
            | Allowed -> (
                match Node.label neighbor.fields.in_blossom with
                | Free ->
                    (* (C1) neighbor is a free vertex; label with T and label
                       its mate with S (R12). *)
                    let queue =
                      Label.assign_t ~v:neighbor ~p:(Endpoint.rev endpoint)
                        ~graph ~queue
                    in
                    aux ~queue neighbors
                | S_single | S _ -> (
                    (* (C2) neighbor is an S-vertex (not in the same blossom;
                       follow back-links to discover either an augmenting path
                       or a new blossom. *)
                    match AddBlossom.scan_for_blossom edge ~graph with
                    | New_blossom children ->
                        (* Found a new blossom; add it to the blossom
                           bookkeeping and turn it into an S-blossom. *)
                        (match graph.debug with
                        | None -> ()
                        | Some pp ->
                            let { node; _ } = PList.hd_odd children in
                            Format.printf
                              "add blossom: @[<v>base = %a@ v = %a@ w = %a@ \
                               blossom children = %a@]@."
                              (Node.pp pp) node (Vertex.pp pp) edge.i
                              (Vertex.pp pp) edge.j
                              (PList.pp (Child.pp pp))
                              children);
                        let queue = AddBlossom.make graph children queue in
                        aux ~queue neighbors
                    | Augmenting_path ->
                        (* Found an augmenting path; augment the matching and
                           end this stage. *)
                        augment_matching edge graph;
                        Augmented)
                | T _ -> (
                    match neighbor.label with
                    | Free ->
                        (* Neighbor is inside a T-blossom, but itself has not
                           yet been reached from outside the blossom; mark it as
                           reached (we need to relabel during T-blossom
                           expansion). *)
                        Label.assign_t_single_vertex ~v:neighbor
                          ~p:(Endpoint.rev endpoint);
                        aux ~queue neighbors
                    | S_single | S _ | T _ -> aux ~queue neighbors))
            | Not_allowed -> (
                match Node.label neighbor.fields.in_blossom with
                | S_single | S _ ->
                    (* Keep track of the least-slack non-allowable edge to a
                       different S-blossom. *)
                    (match in_blossom with
                    | Blossom ({ best_edge = None; _ } as b) ->
                        b.best_edge <- Some edge
                    | Vertex ({ best_edge = None; _ } as v) ->
                        v.best_edge <- Some edge
                    | Blossom ({ best_edge = Some best_edge; _ } as b)
                      when kslack < Edge.slack best_edge ->
                        b.best_edge <- Some edge
                    | Vertex ({ best_edge = Some best_edge; _ } as v)
                      when kslack < Edge.slack best_edge ->
                        v.best_edge <- Some edge
                    | Blossom _ | Vertex _ -> ());
                    aux ~queue neighbors
                | Free | T _ -> (
                    match neighbor.label with
                    | Free ->
                        (* Neighbor is a free vertex (or an unreached vertex
                           inside a T-blossom) but we cannot reach it yet; keep
                           track of the least-slack edge that reaches it. *)
                        (match neighbor.best_edge with
                        | None -> neighbor.best_edge <- Some edge
                        | Some best_edge when kslack < Edge.slack best_edge ->
                            neighbor.best_edge <- Some edge
                        | _ -> ());
                        aux ~queue neighbors
                    | S_single | S _ | T _ -> aux ~queue neighbors)))
    in
    aux ~queue neighbors

  (** Continue labeling until all vertices which are reachable through an
      alternating path have gotten a label. *)
  let rec labeling_loop ~graph = function
    | [] -> Not_augmented []
    | vertex :: queue -> (
        (match graph.debug with
        | None -> ()
        | Some pp -> Format.printf "POP: Vertex %a@." (Vertex.pp pp) vertex);
        match scan_neighbors ~vertex ~graph ~queue with
        | Not_augmented queue -> labeling_loop ~graph queue
        | Augmented -> Augmented)

  type 'v t = Improvement_possible | Optimum_reached

  let rec make graph queue cardinality =
    (match graph.debug with None -> () | Some _ -> Format.printf "SUBSTAGE@.");
    match labeling_loop ~graph queue with
    | Not_augmented queue -> (
        (* There is no augmenting path under these constraints;
           compute delta and reduce slack in the optimization problem. *)
        let delta = Delta.make cardinality ~graph in
        (match graph.debug with
        | None -> ()
        | Some _ -> Format.printf "DELTA: %a@." Delta.pp delta);
        (* Take action at the point where the minimum delta occurred. *)
        match delta with
        | One delta ->
            (* No further improvement possible; optimum reached. *)
            Graph.update_dual_vars_by_delta graph ~delta;
            Optimum_reached
        | Two (delta, edge) ->
            (* Use the least-slack edge to continue the search. *)
            Graph.update_dual_vars_by_delta graph ~delta;
            let next_vertex =
              match Node.label edge.i.fields.in_blossom with
              | Free -> edge.j
              | S_single | S _ | T _ -> edge.i
            in
            let queue = next_vertex :: queue in
            edge.allowable <- Allowed;
            make graph queue cardinality
        | Three (delta, edge) ->
            (* Use the least-slack edge to continue the search. *)
            Graph.update_dual_vars_by_delta graph ~delta;
            let queue = edge.i :: queue in
            edge.allowable <- Allowed;
            make graph queue cardinality
        | Four (delta, b) ->
            (* Expand the least-z blossom. *)
            Graph.update_dual_vars_by_delta graph ~delta;
            let queue =
              ModifyBlossom.expand ~graph ~b ~stage:Not_endstage ~queue
            in
            make graph queue cardinality)
    | Augmented -> Improvement_possible
end

(** Remove labels, forget least-slack edges and allowable edges, and empty
    queue. *)
let reset_stage ~graph =
  let { vertices; blossoms; edges; _ } = graph in
  (* Loss of labeling means that we can not be sure that currently allowable
     edges remain allowable throughout this stage. *)
  List.iter (fun k -> k.allowable <- Not_allowed) edges;
  List.iter
    (fun b ->
      b.best_edge <- None;
      b.fields.blossom_best_edges <- [];
      b.label <- Free)
    blossoms;
  (* Label all single blossoms/vertices with S and put them in the queue. *)
  List.fold_left
    (fun queue v ->
      if Mates.mem v graph then (
        v.best_edge <- None;
        (* Forget all about least-slack edges. *)
        v.label <- Free;
        queue)
      else (
        v.best_edge <- None;
        (* Forget all about least-slack edges. *)
        Label.assign_s ~v ~label:S_single ~queue ~graph))
    [] vertices

(** {1 The public interface} *)

type cardinality = [ `Not_max | `Max ]
type 'a t = ('a * 'a) list

(** Map endpoints to vertices and remove duplicates. *)
let finalize { mates; _ } =
  List.fold_left
    (fun l (k, v) ->
      if List.mem_assoc k l then l else (k, (Endpoint.to_vertex v).value) :: l)
    [] mates

let make ?debug ?(cardinality = `Not_max) edges =
  let graph = Graph.make edges in
  graph.debug <- debug;
  (* Loop until no further improvement is possible. *)
  let rec aux = function
    | [] -> finalize graph
    | _ :: tl -> (
        (match graph.debug with
        | None -> ()
        | Some pp ->
            Format.printf "STAGE %i@."
              (List.length graph.vertices - List.length tl);
            Format.printf "Mates = %a@." (Mates.pp pp) graph);
        (* Each iteration of this loop is a "stage". A stage finds an augmenting
           path and uses that to improve the matching. *)
        let queue = reset_stage ~graph in
        match Substage.make graph queue cardinality with
        | Optimum_reached -> finalize graph
        | Improvement_possible ->
            (* End of a stage; expand all S-blossoms which have dual_var = 0. *)
            List.iter
              (function
                | { parent = None; label = S_single | S _; dual_var = 0.; _ } as
                  b ->
                    ModifyBlossom.expand ~graph ~b ~stage:Endstage ~queue:[]
                    |> ignore
                | { label = Free | S_single | S _ | T _; _ } -> ())
              graph.blossoms;
            aux tl)
  in
  aux graph.vertices
