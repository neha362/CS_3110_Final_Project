open OUnit2
open Final_project
open Fetch_data

(**[update ()] updates the recipe book with the current dining information. *)
let update () =
  Options.recipe_book := Dining.make_menu "../driver_data/recipe_book.csv"

let small_test_menu = Dining.make_menu "../data/small_dining.csv"
let example_ratings = Csv.load "../data/test_ratings.csv" ~strip:true

let ex_drink =
  Some [ "Flavor Infused Water Station"; "None"; "Y"; "Y"; "Y"; "Y"; "Y" ]

let non_veg_drink = Some [ "Milk"; "None"; "Y"; "N"; "Y"; "Y"; "Y" ]
let ex_main = Some [ "House Made Pasta"; "Italian"; "Y"; "Y"; "Y"; "Y"; "N" ]

let veg_main =
  Some
    [
      "House Made Pasta Vegetarian NOT VEGAN";
      "Italian";
      "Y";
      "N";
      "Y";
      "Y";
      "N";
    ]

let ex_protein =
  Some [ "Chicken Souvlaki"; "Mediterranean"; "N"; "N"; "Y"; "N"; "N" ]

let vegan_protein = Some [ "BBQ Tofu"; "American"; "Y"; "Y"; "Y"; "Y"; "Y" ]
let ex_side = Some [ "Greek Salad"; "Mediterranean"; "N"; "Y"; "Y"; "N"; "N" ]

let ex_dessert =
  Some
    [
      "Overnight Oats with Blueberry & Banana";
      "Dessert";
      "Y";
      "Y";
      "Y";
      "N";
      "N";
    ]

let non_vegan_dessert =
  Some [ "Chocolate Cake"; "American"; "N"; "Y"; "Y"; "Y"; "Y" ]

(**[string_of_list l] is the string representation of [l]*)
let string_of_list lst = List.fold_left (fun acc a -> acc ^ a) "" lst

(**[make_plate_name_test name n h c d m p s e exp] is the test with name [name]
   that the plate made using the parameter [n h c d m p s e] has the name [exp]*)
let make_plate_name_test name n h c d m p s e exp =
  name >:: fun _ ->
  update ();
  assert_equal exp Plates.(name (make_plate n h c d m p s e)) ~printer:Fun.id

(**[make_plate_hall_test name n h c d m p s e exp] is the test with name [name]
   that the plate made using the parameter [n h c d m p s e] has the hall [exp]*)
let make_plate_hall_test name n h c d m p s e exp =
  name >:: fun _ ->
  update ();
  assert_equal exp Plates.(hall (make_plate n h c d m p s e)) ~printer:Fun.id

(**[make_plate_comment_test name n h c d m p s e exp] is the test with name
   [name] that the plate made using the parameter [n h c d m p s e] has the
   comment [exp]*)
let make_plate_comment_test name n h c d m p s e exp =
  name >:: fun _ ->
  update ();
  assert_equal exp Plates.(comment (make_plate n h c d m p s e)) ~printer:Fun.id

(**[make_plate_vegan_test name n h c d m p s e exp] is the test with name [name]
   that the plate made using the parameter [n h c d m p s e] has the vegan
   status [exp]*)
let make_plate_vegan_test name n h c d m p s e exp =
  name >:: fun _ ->
  update ();
  assert_equal exp
    Plates.(is_vegan (make_plate n h c d m p s e))
    ~printer:string_of_bool

(**[make_plate_vegetarian_test name n h c d m p s e exp] is the test with name
   [name] that the plate made using the parameter [n h c d m p s e] has the
   vegetarian status [exp]*)
let make_plate_vegetarian_test name n h c d m p s e exp =
  name >:: fun _ ->
  update ();
  assert_equal exp
    Plates.(is_vegetarian (make_plate n h c d m p s e))
    ~printer:string_of_bool

(**[plate_drink_test n p e] is the test with name [n] that checks whether the
   drink in plate [p] is the same as [e]*)
let plate_drink_test name plate exp =
  name >:: fun _ ->
  update ();
  assert_equal exp (Plates.drink plate) ~printer:Fun.id

(**[plate_main_test n p e] is the test with name [n] that checks whether the
   main in plate [p] is the same as [e]*)
let plate_main_test name plate exp =
  name >:: fun _ ->
  update ();
  assert_equal exp (Plates.main plate) ~printer:Fun.id

(**[plate_protein_test n p e] is the test with name [n] that checks whether the
   protein in plate [p] is the same as [e]*)
let plate_protein_test name plate exp =
  name >:: fun _ ->
  update ();
  assert_equal exp (Plates.protein plate) ~printer:Fun.id

(**[plate_side_test n p e] is the test with name [n] that checks whether the
   side in plate [p] is the same as [e]*)
let plate_side_test name plate exp =
  name >:: fun _ ->
  update ();
  assert_equal exp (Plates.side plate) ~printer:Fun.id

(**[plate_dessert_test n p e] is the test with name [n] that checks whether the
   dessert in plate [p] is the same as [e]*)
let plate_dessert_test name plate exp =
  name >:: fun _ ->
  update ();
  assert_equal exp (Plates.dessert plate) ~printer:Fun.id

(**[plate_cuisines_test n p e] is the test with name [n] that checks that the
   cuisine of plate [p] is [e]*)
let plate_cuisines_test name plate exp =
  name >:: fun _ ->
  update ();
  assert_equal exp (string_of_list (Plates.cuisines plate)) ~printer:Fun.id

let full_plate =
  update ();
  Plates.make_plate "Full Plate" "Morrison" "No comments." ex_drink ex_main
    ex_protein ex_side ex_dessert

let empty_plate =
  update ();
  Plates.make_plate "Empty Plate" "No Dining Hall" "No Comment" None None None
    None None

let tests =
  "test suite"
  >::: [
         make_plate_name_test "Empty name plate test" "" "Morrison"
           "No comments..." ex_drink ex_main ex_protein ex_side ex_dessert "";
         make_plate_name_test "Non-empty name plate test" "BEST PLATE EVER"
           "Morrison" "No comments..." ex_drink ex_main ex_protein ex_side
           ex_dessert "BEST PLATE EVER";
         make_plate_hall_test "Empty hall plate test" "Name" "" "No comments..."
           ex_drink ex_main ex_protein ex_side ex_dessert "";
         make_plate_hall_test "Morrison hall plate test" "Name" "Morrison"
           "No comments..." ex_drink ex_main ex_protein ex_side ex_dessert
           "Morrison";
         make_plate_comment_test "Empty comment plate test" "Name" "Morrison" ""
           ex_drink ex_main ex_protein ex_side ex_dessert "";
         make_plate_comment_test "Non-empty hall plate test" "Name" "Morrison"
           "COMMENT" ex_drink ex_main ex_protein ex_side ex_dessert "COMMENT";
         make_plate_vegan_test "Just water plate vegan test" "Vegan Water plate"
           "Morrison" "this is just a plate of water" ex_drink None None None
           None true;
         make_plate_vegan_test "Chicken and water plate non-vegan test"
           "Chicken + Water plate" "Morrison" "water + chicken" ex_drink None
           ex_protein None None false;
         make_plate_vegan_test "Empty plate vegan test" "Empty plate" "Morrison"
           "empty plate" None None None None None true;
         make_plate_vegan_test "Full plate non-vegan test" "Full plate"
           "Morrison" "Full plate" ex_drink ex_main ex_protein ex_side
           ex_dessert false;
         make_plate_vegan_test
           "Partly full plate which is not vegan but is vegetarian"
           "Vegetarian NOT VEGAN" "Morrison" "Plate1" ex_drink veg_main None
           None ex_dessert false;
         make_plate_vegan_test
           "Non-vegan drink, no main, vegan protein, no side, non vegan \
            dessert non vegan test"
           "Non-Vegan" "Morrison" "Plate2" non_veg_drink None vegan_protein None
           non_vegan_dessert false;
         make_plate_vegetarian_test "Just water plate vegetarian test"
           "Vegetarian Water plate" "Morrison" "this is just a plate of water"
           ex_drink None None None None true;
         make_plate_vegetarian_test
           "Chicken and water plate non-vegetarian test" "Chicken + Water plate"
           "Morrison" "water + chicken" ex_drink None ex_protein None None false;
         make_plate_vegetarian_test "Empty plate vegetarian test" "Empty plate"
           "Morrison" "empty plate" None None None None None true;
         make_plate_vegetarian_test "Full plate Non-Vegetarian test"
           "Full plate" "Morrison" "Full plate" ex_drink ex_main ex_protein
           ex_side ex_dessert false;
         make_plate_vegetarian_test
           "Partly full plate which is vegetarian but not vegan"
           "Vege non Vegan plate" "Morrison" "Plate1" ex_drink veg_main None
           None ex_dessert true;
         plate_drink_test "Full plate water drink test" full_plate
           "Flavor Infused Water Station";
         plate_drink_test "Empty plate no drink test" empty_plate
           "No drink in this plate.";
         plate_main_test "Full plate pasta main test" full_plate
           "House Made Pasta";
         plate_main_test "Empty plate no main test" empty_plate
           "No main dish in this plate.";
         plate_protein_test "Full plate chicken protein test" full_plate
           "Chicken Souvlaki";
         plate_protein_test "Empty plate no protein test" empty_plate
           "No protein in this plate.";
         plate_side_test "Full plate salad side test" full_plate "Greek Salad";
         plate_side_test "Empty plate no side test" empty_plate
           "No side dish in this plate.";
         plate_dessert_test "Full plate oats dessert test" full_plate
           "Overnight Oats with Blueberry & Banana";
         plate_dessert_test "Empty plate no dessert test" empty_plate
           "No dessert in this plate.";
         plate_cuisines_test "Empty plate cuisines test" empty_plate "";
         plate_cuisines_test "Full plate cuisines test" full_plate
           "NoneItalianMediterraneanMediterraneanDessert";
       ]

let _ =
  update ();
  run_test_tt_main tests
