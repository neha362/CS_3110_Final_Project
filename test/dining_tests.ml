open Final_project.Dining
open OUnit2

(**[test_make_adjustable_menu] tests the [make_adjustable_menu] function in [Dining.ml], testing that the adjustable menu contains the same information as the original function]*)
let test_make_adjustable_menu =
  "test making menu adjustable" >:: fun _ ->
  let menu = "../data/appel.csv" in
  let transformed =
    Array.map Array.to_list (make_adjustable_menu menu |> to_adjustable_csv)
    |> Array.to_list
  in
  assert_equal 0
    (Csv.compare transformed (make_menu menu |> to_csv))
    ~printer:string_of_int

(**[test_save_menu] validates that calling [save_menu] saves the information
   without corruption. *)
let test_save_menu =
  "test saving" >:: fun _ ->
  let menu = "../data/appel.csv" in
  let new_menu = "../data/appel_test.csv" in
  let made_menu_csv = make_menu menu |> to_csv in
  save_menu
    (made_menu_csv |> List.tl |> from_csv)
    (made_menu_csv |> List.hd |> ref)
    new_menu;
  assert_equal 0
    (Csv.compare made_menu_csv (make_menu new_menu |> to_csv))
    ~printer:string_of_int

(**[test_add] verifies that adding a new recipe alters the CSV*)
let test_add =
  "testing add" >:: fun _ ->
  let menu = "../data/appel.csv" in
  let new_menu = make_menu menu |> to_csv in
  let recipe =
    ((new_menu |> List.hd |> List.hd) ^ "abuebgiuerbg")
    :: (new_menu |> List.hd |> List.tl)
  in
  assert_equal 0
    (Csv.compare (recipe :: new_menu)
       (to_csv (add_recipe (new_menu |> from_csv) recipe)))
    ~printer:string_of_int

(**[test_add_adjustable] tests the same as in [test_add], but with adjustable
   arrays*)
let test_add_adjustable =
  "testing add_adjustable" >:: fun _ ->
  let menu = "../data/appel.csv" in
  let new_menu = make_adjustable_menu menu |> to_adjustable_csv in
  let recipe =
    Array.append
      [| new_menu.(0).(0) ^ "abuebgiuerbg" |]
      (new_menu.(0) |> Array.to_list |> List.tl |> Array.of_list)
  in
  let altered_menu = Array.append [| recipe |] new_menu in
  assert_equal 0
    (Csv.compare
       (add_adjustable (new_menu |> from_adjustable_csv) recipe
       |> from_adjustable |> to_csv)
       (altered_menu |> from_adjustable_csv |> from_adjustable |> to_csv))
    ~printer:string_of_int

(**[test_find_recipes] verifies finding recipes*)
let test_find_recipes =
  "testing find recipes" >:: fun _ ->
  let menu = "../data/appel.csv" |> make_menu in
  let chosen =
    menu |> to_csv
    |> List.filter (fun x -> String.compare (List.hd x) "ocaml" > 0)
    |> List.map List.hd
  in
  let subset = find_recipes (chosen |> BatSet.String.of_list) menu in
  let member = ref true in
  List.iter
    (fun x ->
      member :=
        !member && List.exists (fun y -> List.hd y = x) (subset |> to_csv))
    chosen;
  assert_bool "difference" !member

(**[to_adjustable] verifies that information is preserved while switching from a
   rigid list to an adjustable array*)
let to_adjustable =
  "testing to adjustable" >:: fun _ ->
  let menu = "../data/appel.csv" in
  let adj = make_adjustable_menu menu |> to_adjustable_csv in
  let rigid = make_menu menu |> to_adjustable |> to_adjustable_csv in
  let same = ref true in
  Array.iteri
    (fun i arr ->
      Array.iteri (fun j value -> same := !same && adj.(i).(j) = value) arr)
    rigid;
  assert_bool "difference in CSVs" !same

(**[from_adjustable] verifies that information is preserved while switching from
   an adjustable array to a rigid list*)
let from_adjustable =
  "testing to adjustable" >:: fun _ ->
  let menu = "../data/appel.csv" in
  let adj = make_adjustable_menu menu |> from_adjustable |> to_csv in
  let rigid = make_menu menu |> to_csv in
  assert_equal 0 (Csv.compare adj rigid) ~printer:string_of_int

let tests =
  "test_suite"
  >::: [
         test_make_adjustable_menu;
         test_save_menu;
         test_add;
         test_add_adjustable;
         test_find_recipes;
         to_adjustable;
         from_adjustable;
         ( "filter' test" >:: fun _ ->
           assert_equal
             (filter (make_menu "../driver_data/ratings.csv") (100, ""))
             (make_menu "../data/empty.csv") );
       ]

let _ = run_test_tt_main tests
