(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module F = Format

let sort l = List.sort (fun (a, _) (b, _) -> compare a b) l

let pp_list pp ppf l =
  F.fprintf ppf "@[<hv 0>[@;<0 2>@[<hv 0>%a@]%t]@]"
    (F.pp_print_list ~pp_sep:(fun ppf () -> F.fprintf ppf ";@ ") pp)
    l
    (F.pp_print_custom_break ~fits:("", 0, "") ~breaks:(";", 0, ""))

let pp_pair pp_a pp_b ppf (a, b) =
  F.fprintf ppf "@[<hv>(@;<0 2>@[<hv 0>%a,@ %a@])@]" pp_a a pp_b b

let print_title title = F.printf "@[%a@]@;" F.pp_print_text title

let check pp title l =
  print_title title;
  F.printf "%a@;@;" (pp_list (pp_pair pp pp)) l

let check_int = check F.pp_print_int
let check_string = check (fun ppf s -> F.fprintf ppf "%S" s)
let section s = F.printf "# %s@;@;" s

let () =
  F.printf "@[<v>";
  section "Trivial";
  check_int "Empty input graph" (Blossom.make Int.compare [] |> sort);
  check_int "Single edge" (Blossom.make Int.compare [ (0, 1, 1.) ] |> sort);
  check_int "Two edges"
    (Blossom.make Int.compare [ (1, 2, 10.); (2, 3, 11.) ] |> sort);
  check_int "Three edges"
    (Blossom.make Int.compare [ (1, 2, 5.); (2, 3, 11.); (3, 4, 5.) ] |> sort);
  check_int "Three edges again, with IDs ordered differently"
    (Blossom.make Int.compare [ (1, 2, 5.); (2, 3, 11.); (4, 3, 5.) ] |> sort);
  check_int "A simple love triangle"
    (Blossom.make Int.compare [ (0, 1, 6.); (0, 2, 10.); (1, 2, 5.) ] |> sort);
  check_int "Maximum cardinality"
    (Blossom.make Int.compare
       [ (1, 2, 5.); (2, 3, 11.); (3, 4, 5.) ]
       ~cardinality:`Max
    |> sort);
  check_int "Floating point weights"
    (Blossom.make Int.compare
       [
         (1, 2, Float.pi);
         (2, 3, Float.exp 1.);
         (1, 3, 3.0);
         (1, 4, Float.sqrt 2.0);
       ]
    |> sort);

  section "Negative weights";
  check_int "Negative weights"
    (Blossom.make Int.compare
       [ (1, 2, 2.); (1, 3, -2.); (2, 3, 1.); (2, 4, -1.); (3, 4, -6.) ]
    |> sort);
  check_int "Negative weights with maximum cardinality"
    (Blossom.make Int.compare ~cardinality:`Max
       [ (1, 2, 2.); (1, 3, -2.); (2, 3, 1.); (2, 4, -1.); (3, 4, -6.) ]
    |> sort);

  section "Blossoms";
  check_int "S-blossom A"
    (Blossom.make Int.compare
       [ (1, 2, 8.); (1, 3, 9.); (2, 3, 10.); (3, 4, 7.) ]
    |> sort);
  check_int "S-blossom B"
    (Blossom.make Int.compare
       [
         (1, 2, 8.); (1, 3, 9.); (2, 3, 10.); (3, 4, 7.); (1, 6, 5.); (4, 5, 6.);
       ]
    |> sort);
  check_int "Create nested S-blossom, use for augmentation."
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
  check_int "Create S-blossom, relabel as S, include in nested S-blossom."
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
  check_int "Create nested S-blossom, augment, expand recursively."
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
  check_int "Create S-blossom, relabel as T, expand."
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
  check_int "Create nested S-blossom, relabel as T, expand."
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
  check_int "Create nested S-blossom, relabel as S, expand recursively."
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
  check_int "Again, but slightly different. (A)"
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
  check_int "Again, but slightly different. (B)"
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
    |> sort);

  section "S-blossom to T-blossom";
  check_int "S-blossom, relabel as T-blossom: A"
    (Blossom.make Int.compare
       [
         (1, 2, 9.); (1, 3, 8.); (2, 3, 10.); (1, 4, 5.); (4, 5, 4.); (1, 6, 3.);
       ]
    |> sort);
  check_int "S-blossom, relabel as T-blossom: B"
    (Blossom.make Int.compare
       [
         (1, 2, 9.); (1, 3, 8.); (2, 3, 10.); (1, 4, 5.); (4, 5, 3.); (1, 6, 4.);
       ]
    |> sort);
  check_int "S-blossom, relabel as T-blossom: C"
    (Blossom.make Int.compare
       [
         (1, 2, 9.); (1, 3, 8.); (2, 3, 10.); (1, 4, 5.); (4, 5, 3.); (3, 6, 4.);
       ]
    |> sort);

  section "Nasty cases";
  check_int
    "Create blossom, relabel as T in more than one way, expand, augment."
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
  check_int "Again, but slightly different."
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
  check_int
    "Create blossom, relabel as T, expand such that a new least-slack \
     S-to-free edge is produced, augment."
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
  check_int
    "Create nested blossom, relabel as T in more than one way, expand outer \
     blossom such that inner blossom ends up on an augmenting path."
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
    |> sort);

  section "More nasty cases.";
  check_int "Blossom with five children (A)."
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
  check_int "Blossom with five children (B)."
    (Blossom.make Int.compare (* ~debug:F.pp_print_int*)
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
  check_int "Scan along a long label path to create a blossom."
    (Blossom.make Int.compare (*~debug:F.pp_print_int*)
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
    |> sort);

  section "Other tests";
  check_int
    "(Reversed) Create nested blossom, relabel as T in more than one way, \
     expand outer blossom such that inner blossom ends up on an augmenting \
     path."
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
  check_int "(Reversed) Create nested S-blossom, augment, expand recursively."
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
  check_int "Vertices with edges on themselves are silently ignored."
    (Blossom.make Int.compare [ (0, 1, 1.); (1, 1, 9001.) ] |> sort);
  check_string "String vertices"
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
  check_string "String vertices (nested S-blossom, relabel as S)"
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
    |> sort);

  section "Brute force";
  for _ = 0 to 1023 do
    List.init 63 (fun _ -> (Random.int 15, Random.int 15, Random.float 100.))
    |> Blossom.make Int.compare ~cardinality:`Max
    |> ignore
  done;
  F.printf "@[%a@]@;@;" F.pp_print_text
    "Generate a large number of random graphs and check that the algorithm \
     doesn't crash.";

  section "Debugging";
  print_title
    "Print debug info. (Same as: Create nested S-blossom, relabel as T, \
     expand.)";
  ignore
    (Blossom.make Int.compare
       ~debug:(F.std_formatter, F.pp_print_int)
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
       ]);

  F.printf "@]"
