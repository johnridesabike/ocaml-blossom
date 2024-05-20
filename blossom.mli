(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)
(** This is an OCaml implementation of the
    {{:https://en.wikipedia.org/wiki/Blossom_algorithm} blossom algorithm}. It
    finds a maximum matching of vertices on general, undirected, weighted
    graphs.

    {1 How it works}

    Matching along an undirected, weighted graph is notoriously difficult. The
    blossom algorithm does the heavy lifting for us in O(n³) time. Let's look at
    a simple example.

    Suppose you have a list of chess players: Mary, Joseph, Matthew, Mark, Luke,
    John, Peter, Andrew, James, and Philip. You want to pair them to compete in
    a tournament round.

    Your first step is to list all of your potential pairings.

{[
let graph = [
  (Mary, Joseph);
  (Mary, Matthew);
  (Joseph, Matthew);
  (Joseph, Mark);
  (Matthew, Luke);
  (Mark, Luke);
  (Mary, Andrew);
  (Luke, Peter);
  (Peter, John);
  (Andrew, Philip);
  (Mark, James);
]
]}

    (Typically your list will be much longer and include {i all} possibilities,
    but this is just an illustration.)

    Next, you will need to determine the {i weight} of each pair. This is a
    floating-point number that indicates how desirable that pair is.

{[
let graph = [
  (Mary, Joseph, 40.);
  (Mary, Matthew, 40.);
  (Joseph, Matthew, 60.);
  (Joseph, Mark, 55.);
  (Matthew, Luke, 55.);
  (Mark, Luke, 50.);
  (Mary, Andrew, 15.);
  (Luke, Peter, 30.);
  (Peter, John, 10.);
  (Andrew, Philip, 10.);
  (Mark, James, 10.))
]
]}

    In graph theory, each of the people is a "vertex," and each pair of people
    is an "edge." We can visualize it in 2D space.

{v
  Andrew ---10--- Philip                  Peter
    |                                    /      \
    15                                  30       10
    |                                  /          \
    Mary ---40--- Matthew ---55---- Luke         John
       \          /                  /
        40      60                 50
         \      /                  /
          Joseph ------ 55 ----- Mark ----30---- James
v}

    For this graph, the algorithm would return this result:

    {[
      let result = [
        (Andrew, Philip);
        (James, Mark);
        (John, Peter);
        (Joseph, Mary);
        (Luke, Matthew);
        (Mark, James);
        (Mary, Joseph);
        (Matthew, Luke);
        (Peter, John);
        (Philip, Andrew);
      ]
    ]}

    We can compare this visually to the input graph:

{v
  Andrew -------- Philip                  Peter
                                               \
                                                \
                                                 \
   Mary          Matthew --------- Luke         John
      \
       \
        \
         Joseph                 Mark ---------- James
v}

    Note that we couldn't use the edge with the highest weight because choosing
    it would leave another vertex with no connections. We also have to use the
    two edges with the lowest weights because we're committed to matching as
    many vertices as possible.

    As you can see, finding the {i maximum} weighted matching is often
    unintuitive. Imagine how much more difficult this becomes when you have
    dozens, or hundreds, of people, and we could potentially match every person
    with anyone else!


    {2 Maximum cardinality}

    The algorithm accepts an optional parameter, [cardinality], which can be the
    value [`Max]. This enables "maximum cardinality" matching, where the
    algorithm will only accept solutions that use as many edges as possible,
    even extremely undesirable ones (such as ones with negative weights).

    {[
    let graph = [
      (1, 2, 2.);
      (1, 3, (-2.));
      (2, 3, 1.);
      (2, 4, (-1.));
      (3, 4, (-6.));
    ]

    let result = Blossom.Match.make graph
    (* result: (1, 2) *)

    let result = Blossom.Match.make ~cardinality:`Max graph
    (* result: (1, 3), (2, 4) *)
    ]}
*)

(** {1 Interface} *)

type cardinality = [ `Not_max | `Max ]
(** Determines whether or not the algorithm should {i only} accept
    maximum-cardinality solutions. *)

type 'a t = ('a * 'a) list
(** A bidirectional map of each vertex to its mate vertex. *)

val make :
  ?debug:Format.formatter * (Format.formatter -> 'a -> unit) ->
  ?cardinality:cardinality ->
  ('a -> 'a -> int) ->
  ('a * 'a * float) list ->
  'a t
(** Computes a maximum-weighted matching on a general undirected weighted graph.
    This function takes time O(n³).

    Requires an ordering function for the vertex type, such as [Int.compare] or
    [String.compare].

    Accepts a list of tuples [(i, j, w)], each describing an undirected edge
    between vertex [i] and vertex [j] with weight [w]. There will be at most one
    edge between any two vertices, and no vertex has an edge to itself.
    Duplicate edges are ignored.

    @param cardinality When set to [`Max], only maximum-cardinality matchings
    are considered as solutions. [`Not_max] is the default.

    @param debug When set, print debugging information during matching.
 *)

(**
{1 Credits}

- {{: https://johnridesa.bike/} John} - author.
- {{: http://jorisvr.nl/article/maximum-matching} Joris van Rantwijk} - his
  Python code was an invaluable reference.

I can't take any credit for the algorithm itself. It exists thanks to many
people much smarter than me.

{1 License}

{v
Copyright (c) 2022 John Jackson

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.
v}
*)
