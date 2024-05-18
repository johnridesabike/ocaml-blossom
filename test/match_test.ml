(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

let check = Alcotest.(check (list (pair int int)))
let sort l = List.sort (fun (a, _) (b, _) -> compare a b) l

let trivial () =
  check "Empty input graph" (Blossom.make Int.compare [] |> sort) [];
  check "Single edge"
    [ (0, 1); (1, 0) ]
    (Blossom.make Int.compare [ (0, 1, 1.) ] |> sort);
  check "Two edges"
    [ (2, 3); (3, 2) ]
    (Blossom.make Int.compare [ (1, 2, 10.); (2, 3, 11.) ] |> sort);
  check "Three edges"
    [ (2, 3); (3, 2) ]
    (Blossom.make Int.compare [ (1, 2, 5.); (2, 3, 11.); (3, 4, 5.) ] |> sort);
  check "Three edges again, with IDs ordered differently"
    [ (2, 3); (3, 2) ]
    (Blossom.make Int.compare [ (1, 2, 5.); (2, 3, 11.); (4, 3, 5.) ] |> sort);
  check "A simple love triangle"
    [ (0, 2); (2, 0) ]
    (Blossom.make Int.compare [ (0, 1, 6.); (0, 2, 10.); (1, 2, 5.) ] |> sort);
  check "Maximum cardinality"
    [ (1, 2); (2, 1); (3, 4); (4, 3) ]
    (Blossom.make Int.compare
       [ (1, 2, 5.); (2, 3, 11.); (3, 4, 5.) ]
       ~cardinality:`Max
    |> sort);
  check "Floating point weights"
    [ (1, 4); (2, 3); (3, 2); (4, 1) ]
    (Blossom.make Int.compare
       [
         (1, 2, Float.pi);
         (2, 3, Float.exp 1.);
         (1, 3, 3.0);
         (1, 4, Float.sqrt 2.0);
       ]
    |> sort)

let negative_weights () =
  check "Negative weights"
    [ (1, 2); (2, 1) ]
    (Blossom.make Int.compare
       [ (1, 2, 2.); (1, 3, -2.); (2, 3, 1.); (2, 4, -1.); (3, 4, -6.) ]
    |> sort);
  check "Negative weights with maximum cardinality"
    [ (1, 3); (2, 4); (3, 1); (4, 2) ]
    (Blossom.make Int.compare ~cardinality:`Max
       [ (1, 2, 2.); (1, 3, -2.); (2, 3, 1.); (2, 4, -1.); (3, 4, -6.) ]
    |> sort)

let blossoms () =
  check "S-blossom A"
    [ (1, 2); (2, 1); (3, 4); (4, 3) ]
    (Blossom.make Int.compare
       [ (1, 2, 8.); (1, 3, 9.); (2, 3, 10.); (3, 4, 7.) ]
    |> sort);
  check "S-blossom B"
    [ (1, 6); (2, 3); (3, 2); (4, 5); (5, 4); (6, 1) ]
    (Blossom.make Int.compare
       [
         (1, 2, 8.); (1, 3, 9.); (2, 3, 10.); (3, 4, 7.); (1, 6, 5.); (4, 5, 6.);
       ]
    |> sort);
  check "Create nested S-blossom, use for augmentation."
    [ (1, 3); (2, 4); (3, 1); (4, 2); (5, 6); (6, 5) ]
    (Blossom.make Int.compare
       [
         (1, 2, 9.);
         (1, 3, 9.);
         (2, 3, 10.);
         (2, 4, 8.);
         (3, 5, 8.);
         (4, 5, 10.);
         (5, 6, 6.);
       ]
    |> sort);
  check "Create S-blossom, relabel as S, include in nested S-blossom."
    [ (1, 2); (2, 1); (3, 4); (4, 3); (5, 6); (6, 5); (7, 8); (8, 7) ]
    (Blossom.make Int.compare
       [
         (1, 2, 10.);
         (1, 7, 10.);
         (2, 3, 12.);
         (3, 4, 20.);
         (3, 5, 20.);
         (4, 5, 25.);
         (5, 6, 10.);
         (6, 7, 10.);
         (7, 8, 8.);
       ]
    |> sort);
  check "Create nested S-blossom, augment, expand recursively."
    [ (1, 2); (2, 1); (3, 5); (4, 6); (5, 3); (6, 4); (7, 8); (8, 7) ]
    (Blossom.make Int.compare
       [
         (1, 2, 8.);
         (1, 3, 8.);
         (2, 3, 10.);
         (2, 4, 12.);
         (3, 5, 12.);
         (4, 5, 14.);
         (4, 6, 12.);
         (5, 7, 12.);
         (6, 7, 14.);
         (7, 8, 12.);
       ]
    |> sort);
  check "Create S-blossom, relabel as T, expand."
    [ (1, 6); (2, 3); (3, 2); (4, 8); (5, 7); (6, 1); (7, 5); (8, 4) ]
    (Blossom.make Int.compare
       [
         (1, 2, 23.);
         (1, 5, 22.);
         (1, 6, 15.);
         (2, 3, 25.);
         (3, 4, 22.);
         (4, 5, 25.);
         (4, 8, 14.);
         (5, 7, 13.);
       ]
    |> sort);
  check "Create nested S-blossom, relabel as T, expand."
    [ (1, 8); (2, 3); (3, 2); (4, 7); (5, 6); (6, 5); (7, 4); (8, 1) ]
    (Blossom.make Int.compare
       [
         (1, 2, 19.);
         (1, 3, 20.);
         (1, 8, 8.);
         (2, 3, 25.);
         (2, 4, 18.);
         (3, 5, 18.);
         (4, 5, 13.);
         (4, 7, 7.);
         (5, 6, 7.);
       ]
    |> sort);
  check "Create nested S-blossom, relabel as S, expand recursively."
    [
      (1, 2);
      (2, 1);
      (3, 5);
      (4, 9);
      (5, 3);
      (6, 7);
      (7, 6);
      (8, 10);
      (9, 4);
      (10, 8);
    ]
    (Blossom.make Int.compare
       [
         (1, 2, 40.);
         (1, 3, 40.);
         (2, 3, 60.);
         (2, 4, 55.);
         (3, 5, 55.);
         (4, 5, 50.);
         (1, 8, 15.);
         (5, 7, 30.);
         (7, 6, 10.);
         (8, 10, 10.);
         (4, 9, 30.);
       ]
    |> sort);
  check "Again, but slightly different. (A)"
    [
      (1, 8);
      (2, 3);
      (3, 2);
      (4, 5);
      (5, 4);
      (6, 7);
      (7, 6);
      (8, 1);
      (10, 11);
      (11, 10);
    ]
    (Blossom.make Int.compare
       [
         (1, 2, 40.);
         (1, 3, 40.);
         (2, 3, 60.);
         (2, 4, 55.);
         (3, 5, 55.);
         (4, 5, 50.);
         (1, 8, 15.);
         (5, 7, 30.);
         (7, 6, 10.);
         (8, 10, 10.);
         (4, 9, 30.);
         (11, 10, 100.);
       ]
    |> sort);
  check "Again, but slightly different. (B)"
    [
      (1, 2);
      (2, 1);
      (3, 6);
      (4, 9);
      (5, 7);
      (6, 3);
      (7, 5);
      (8, 10);
      (9, 4);
      (10, 8);
    ]
    (Blossom.make Int.compare
       [
         (1, 2, 40.);
         (1, 3, 40.);
         (2, 3, 60.);
         (2, 4, 55.);
         (3, 5, 55.);
         (4, 5, 50.);
         (1, 8, 15.);
         (5, 7, 30.);
         (7, 6, 10.);
         (8, 10, 10.);
         (4, 9, 30.);
         (3, 6, 36.);
       ]
    |> sort)

let s_blossom_to_t () =
  check "S-blossom, relabel as T-blossom: A"
    [ (1, 6); (2, 3); (3, 2); (4, 5); (5, 4); (6, 1) ]
    (Blossom.make Int.compare
       [
         (1, 2, 9.); (1, 3, 8.); (2, 3, 10.); (1, 4, 5.); (4, 5, 4.); (1, 6, 3.);
       ]
    |> sort);
  check "S-blossom, relabel as T-blossom: B"
    [ (1, 6); (2, 3); (3, 2); (4, 5); (5, 4); (6, 1) ]
    (Blossom.make Int.compare
       [
         (1, 2, 9.); (1, 3, 8.); (2, 3, 10.); (1, 4, 5.); (4, 5, 3.); (1, 6, 4.);
       ]
    |> sort);
  check "S-blossom, relabel as T-blossom: C"
    [ (1, 2); (2, 1); (3, 6); (4, 5); (5, 4); (6, 3) ]
    (Blossom.make Int.compare
       [
         (1, 2, 9.); (1, 3, 8.); (2, 3, 10.); (1, 4, 5.); (4, 5, 3.); (3, 6, 4.);
       ]
    |> sort)

let nasty_cases () =
  check "Create blossom, relabel as T in more than one way, expand, augment."
    [
      (1, 6);
      (2, 3);
      (3, 2);
      (4, 8);
      (5, 7);
      (6, 1);
      (7, 5);
      (8, 4);
      (9, 10);
      (10, 9);
    ]
    (Blossom.make Int.compare
       [
         (1, 2, 45.);
         (1, 5, 45.);
         (2, 3, 50.);
         (3, 4, 45.);
         (4, 5, 50.);
         (1, 6, 30.);
         (3, 9, 35.);
         (4, 8, 35.);
         (5, 7, 26.);
         (9, 10, 5.);
       ]
    |> sort);
  check "Again, but slightly different."
    [
      (1, 6);
      (2, 3);
      (3, 2);
      (4, 8);
      (5, 7);
      (6, 1);
      (7, 5);
      (8, 4);
      (9, 10);
      (10, 9);
    ]
    (Blossom.make Int.compare
       [
         (1, 2, 45.);
         (1, 5, 45.);
         (2, 3, 50.);
         (3, 4, 45.);
         (4, 5, 50.);
         (1, 6, 30.);
         (3, 9, 35.);
         (4, 8, 26.);
         (5, 7, 40.);
         (9, 10, 5.);
       ]
    |> sort);
  check
    "Create blossom, relabel as T, expand such that a new least-slack \
     S-to-free edge is produced, augment."
    [
      (1, 6);
      (2, 3);
      (3, 2);
      (4, 8);
      (5, 7);
      (6, 1);
      (7, 5);
      (8, 4);
      (9, 10);
      (10, 9);
    ]
    (Blossom.make Int.compare
       [
         (1, 2, 45.);
         (1, 5, 45.);
         (2, 3, 50.);
         (3, 4, 45.);
         (4, 5, 50.);
         (1, 6, 30.);
         (3, 9, 35.);
         (4, 8, 28.);
         (5, 7, 26.);
         (9, 10, 5.);
       ]
    |> sort);
  check
    "Create nested blossom, relabel as T in more than one way, expand outer \
     blossom such that inner blossom ends up on an augmenting path."
    [
      (1, 8);
      (2, 3);
      (3, 2);
      (4, 6);
      (5, 9);
      (6, 4);
      (7, 10);
      (8, 1);
      (9, 5);
      (10, 7);
      (11, 12);
      (12, 11);
    ]
    (Blossom.make Int.compare
       [
         (1, 2, 45.);
         (1, 7, 45.);
         (2, 3, 50.);
         (3, 4, 45.);
         (4, 5, 95.);
         (4, 6, 94.);
         (5, 6, 94.);
         (6, 7, 50.);
         (1, 8, 30.);
         (3, 11, 35.);
         (5, 9, 36.);
         (7, 10, 26.);
         (11, 12, 5.);
       ]
    |> sort)

let more_nasty () =
  check "Blossom with five children (A)."
    [
      (0, 1);
      (1, 0);
      (2, 5);
      (3, 4);
      (4, 3);
      (5, 2);
      (6, 7);
      (7, 6);
      (8, 9);
      (9, 8);
    ]
    (Blossom.make Int.compare
       [
         (9, 8, 30.);
         (9, 5, 55.);
         (9, 3, 6.);
         (9, 1, 50.);
         (8, 3, 18.);
         (8, 4, 10.);
         (7, 3, 15.);
         (7, 6, 10.);
         (5, 3, 40.);
         (5, 2, 60.);
         (3, 2, 40.);
         (3, 4, 16.);
         (2, 1, 55.);
         (1, 0, 43.);
       ]
    |> sort);
  check "Blossom with five children (B)."
    [ (1, 3); (2, 4); (3, 1); (4, 2); (5, 8); (7, 9); (8, 5); (9, 7) ]
    (Blossom.make Int.compare ~debug:Format.pp_print_int
       [
         (1, 2, 77.);
         (1, 3, 60.);
         (1, 4, 61.);
         (2, 5, 87.);
         (2, 6, 10.);
         (2, 4, 89.);
         (7, 8, 15.);
         (7, 5, 28.);
         (7, 9, 10.);
         (3, 8, 5.);
         (3, 5, 60.);
         (8, 5, 58.);
         (5, 4, 55.);
         (4, 10, 30.);
       ]
    |> sort);
  check "Scan along a long label path to create a blossom."
    [
      (1, 2);
      (2, 1);
      (3, 6);
      (4, 5);
      (5, 4);
      (6, 3);
      (7, 8);
      (8, 7);
      (9, 10);
      (10, 9);
    ]
    (Blossom.make Int.compare
       [
         (10, 6, 30.);
         (10, 9, 55.);
         (10, 8, 50.);
         (6, 3, 10.);
         (4, 1, 15.);
         (4, 5, 10.);
         (9, 1, 40.);
         (9, 2, 55.);
         (9, 8, 59.);
         (1, 2, 40.);
         (2, 8, 55.);
         (8, 7, 30.);
       ]
    |> sort)

let other () =
  check
    "(Reversed) Create nested blossom, relabel as T in more than one way, \
     expand outer blossom such that inner blossom ends up on an augmenting \
     path."
    [
      (-12, -11);
      (-11, -12);
      (-10, -7);
      (-9, -5);
      (-8, -1);
      (-7, -10);
      (-6, -4);
      (-5, -9);
      (-4, -6);
      (-3, -2);
      (-2, -3);
      (-1, -8);
    ]
    (Blossom.make Int.compare
       [
         (-1, -2, 45.);
         (-1, -7, 45.);
         (-2, -3, 50.);
         (-3, -4, 45.);
         (-4, -5, 95.);
         (-4, -6, 94.);
         (-5, -6, 94.);
         (-6, -7, 50.);
         (-1, -8, 30.);
         (-3, -11, 35.);
         (-5, -9, 36.);
         (-7, -10, 26.);
         (-11, -12, 5.);
       ]
    |> sort);
  check "(Reversed) Create nested S-blossom, augment, expand recursively."
    [ (1, 2); (2, 1); (3, 5); (4, 6); (5, 3); (6, 4); (7, 8); (8, 7) ]
    (Blossom.make Int.compare
       [
         (8, 7, 8.);
         (8, 6, 8.);
         (7, 6, 10.);
         (7, 5, 12.);
         (6, 4, 12.);
         (5, 4, 14.);
         (5, 3, 12.);
         (4, 2, 12.);
         (3, 2, 14.);
         (2, 1, 12.);
       ]
    |> sort);
  check "Vertices with edges on themselves are silently ignored."
    [ (0, 1); (1, 0) ]
    (Blossom.make Int.compare [ (0, 1, 1.); (1, 1, 9001.) ] |> sort);
  Alcotest.(check (list (pair string string)))
    "String vertices"
    [
      ("Andrew", "Gabriel");
      ("Gabriel", "Andrew");
      ("James", "Paul");
      ("John", "Peter");
      ("Joseph", "Mary");
      ("Mary", "Joseph");
      ("Michael", "Raphael");
      ("Paul", "James");
      ("Peter", "John");
      ("Raphael", "Michael");
    ]
    (Blossom.make String.compare
       [
         ("Mary", "Joseph", 40.);
         ("Mary", "Michael", 40.);
         ("Joseph", "Michael", 60.);
         ("Joseph", "Gabriel", 55.);
         ("Michael", "Raphael", 55.);
         ("Gabriel", "Raphael", 50.);
         ("Mary", "Paul", 15.);
         ("Raphael", "Peter", 30.);
         ("Peter", "John", 10.);
         ("Paul", "James", 10.);
         ("Gabriel", "Andrew", 30.);
         ("Gabriel", "Gabriel", 100.);
         ("Gabriel", "Andrew", 100.);
       ]
    |> sort);
  (*********************************************************)
  (*  Andrew ---10--- Philip                  Peter        *)
  (*    |                                    /     \       *)
  (*   15                                  30       10     *)
  (*    |                                  /         \     *)
  (*   Mary ---40--- Matthew ---55---- Luke         John   *)
  (*      \          /                  /                  *)
  (*       40      60                 50                   *)
  (*        \      /                  /                    *)
  (*         Joseph ------ 55 ----- Mark ----30---- James  *)
  (*********************************************************)
  (*  Andrew -------- Philip                  Peter        *)
  (*                                               \       *)
  (*                                                \      *)
  (*                                                 \     *)
  (*   Mary          Matthew --------- Luke         John   *)
  (*      \                                                *)
  (*       \                                               *)
  (*        \                                              *)
  (*         Joseph                 Mark ---------- James  *)
  (*********************************************************)
  Alcotest.(check (list (pair string string)))
    "String vertices (nested S-blossom, relabel as S)"
    [
      ("Andrew", "Philip");
      ("James", "Mark");
      ("John", "Peter");
      ("Joseph", "Mary");
      ("Luke", "Matthew");
      ("Mark", "James");
      ("Mary", "Joseph");
      ("Matthew", "Luke");
      ("Peter", "John");
      ("Philip", "Andrew");
    ]
    (Blossom.make String.compare
       [
         ("Mary", "Joseph", 40.);
         ("Mary", "Matthew", 40.);
         ("Joseph", "Matthew", 60.);
         ("Joseph", "Mark", 55.);
         ("Matthew", "Luke", 55.);
         ("Mark", "Luke", 50.);
         ("Mary", "Andrew", 15.);
         ("Luke", "Peter", 30.);
         ("Peter", "John", 10.);
         ("Andrew", "Philip", 10.);
         ("Mark", "James", 30.);
       ]
    |> sort)

let brute_force () =
  let open Alcotest in
  for _ = 0 to 1023 do
    List.init 63 (fun _ -> (Random.int 15, Random.int 15, Random.float 100.))
    |> Blossom.make Int.compare ~cardinality:`Max
    |> ignore
  done;
  check pass
    "Generate a large number of random graphs and check that the algorithm \
     doesn't crash."
    () ()

let () =
  let open Alcotest in
  run "Match"
    [
      ( "Trivial cases",
        [
          test_case "Trivial cases" `Quick trivial;
          test_case "Negative weights" `Quick negative_weights;
        ] );
      ( "Blossoms",
        [
          test_case "create S-blossom and use it for augmentation." `Quick
            blossoms;
          test_case
            "Create S-blossom, relabel as T-blossom, use for augmentation."
            `Quick s_blossom_to_t;
        ] );
      ("Other cases", [ test_case "Other cases" `Quick other ]);
      ( "Nasty cases",
        [
          test_case "Nasty cases" `Quick nasty_cases;
          test_case "More nasty cases" `Quick more_nasty;
          test_case "Brute force any missed edge cases" `Slow brute_force;
        ] );
    ]
