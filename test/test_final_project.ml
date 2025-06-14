open OUnit2
open Final_project

let small_test_menu = Dining.make_menu "../data/small_dining.csv"
let example_ratings = Csv.load "../data/test_ratings.csv" ~strip:true

(**[id x] is [x]*)
let id x = x

(**[string_of_list l] is the string representation of [l]*)
let string_of_list lst = List.fold_left (fun acc a -> acc ^ a) "" lst

(**[filter_name_test c p e] is the test that [filter c p] produces [e] when
   using the ugly printer function*)
let filter_ugly_test csv params exp =
  "filter using ugly printing ---------" ^ "--------- test" >:: fun _ ->
  let list_list = Dining.make_menu csv in
  assert_equal exp Dining.(string_of_menu (filter list_list params)) ~printer:id

(**[filter_cuisine_test c p e] is the test that filtering for the parameters in
   [p] produces the pretty-printed [exp] when using the pretty printer function*)
let filter_pretty_test csv params exp =
  "filter using pretty printing ---------" ^ "--------- test" >:: fun _ ->
  let list_list = Dining.make_menu csv in
  assert_equal exp Dining.(user_menu (filter list_list params)) ~printer:id

(**[filter_invalid_csv_failure_test c p] is the test that filtering an csv with
   a negative argument raises [Failure "negative filter"]*)
let filter_invalid_csv_failure_test csv params =
  "filter failure test" >:: fun _ ->
  let list_list = Dining.make_menu csv in
  assert_raises (Invalid_argument "negative filter") (fun () ->
      Dining.filter list_list params)

(**[make_menu_test n d f e] is the test with name [n] that checks if filtering
   menu [d] using [f] produces [e]*)
let make_menu_test test_name csv filter_item expected =
  "Test for " ^ test_name >:: fun _ ->
  let list_list = Dining.make_menu csv in
  let filtered_menu = Dining.filter list_list filter_item in
  let string = Dining.string_of_menu filtered_menu in
  assert_equal expected string ~printer:Fun.id

(**[menu_length_test n c e] is the test with name [n] that checks whether the
   length of the menu stored at [c] is the same as [e]*)
let menu_length_test name csv exp =
  name >:: fun _ ->
  assert_equal exp
    Dining.(length_of_menu (make_menu csv))
    ~printer:string_of_int

(**[user_menu_test n c e] is the test with name [n] that checks if the menu
   created from [c] has the user menu string [e]*)
let user_menu_test name csv exp =
  name >:: fun _ ->
  assert_equal exp Dining.(user_menu (make_menu csv)) ~printer:Fun.id

(**[list_of_menu_test n c e] is the test with name [n] that checks if
   [list_of_menu] of the menu stored at [c] is [e]*)
let list_of_menu_test name csv exp =
  name >:: fun _ ->
  assert_equal exp (string_of_list Dining.(list_of_menu (make_menu csv)))

let tests =
  "test suite"
  >::: [
         (*ugly printer tests*)
         filter_ugly_test "../data/small_dining.csv" (0, "Apple")
           "AppleOtherYYYYYAppleFruitYYNYYApple2YYNYYApple1YYNYY";
         filter_ugly_test "../data/small_dining.csv"
           (0, "This shouldn't have anything :)")
           "";
         filter_ugly_test "../data/small_dining.csv" (1, "Asian")
           "PhoAsianYYYYNFried RiceAsianNNYYY";
         filter_ugly_test "../data/small_dining.csv" (1, "Not a cuisine") "";
         filter_ugly_test "../data/small_dining.csv" (2, "Y")
           "AppleOtherYYYYYPhoAsianYYYYNAppleFruitYYNYYApple2YYNYYApple1YYNYY";
         filter_ugly_test "../data/small_dining.csv" (2, "N")
           "HamburgerAmericanNNNNNFried RiceAsianNNYYY";
         filter_ugly_test "../data/small_dining.csv" (2, "Not an option") "";
         filter_ugly_test "../data/small_dining.csv" (3, "Y")
           "AppleOtherYYYYYPhoAsianYYYYNAppleFruitYYNYYApple2YYNYYApple1YYNYY";
         filter_ugly_test "../data/small_dining.csv" (3, "N")
           "HamburgerAmericanNNNNNFried RiceAsianNNYYY";
         filter_ugly_test "../data/small_dining.csv" (3, "Not an option") "";
         filter_ugly_test "../data/small_dining.csv" (4, "Y")
           "AppleOtherYYYYYPhoAsianYYYYNFried RiceAsianNNYYY";
         filter_ugly_test "../data/small_dining.csv" (4, "N")
           "HamburgerAmericanNNNNNAppleFruitYYNYYApple2YYNYYApple1YYNYY";
         filter_ugly_test "../data/small_dining.csv" (4, "Not an option") "";
         filter_ugly_test "../data/small_dining.csv" (5, "Y")
           "AppleOtherYYYYYPhoAsianYYYYNAppleFruitYYNYYFried \
            RiceAsianNNYYYApple2YYNYYApple1YYNYY";
         filter_ugly_test "../data/small_dining.csv" (5, "N")
           "HamburgerAmericanNNNNN";
         filter_ugly_test "../data/small_dining.csv" (5, "Not an option") "";
         filter_ugly_test "../data/small_dining.csv" (6, "Y")
           "AppleOtherYYYYYAppleFruitYYNYYFried \
            RiceAsianNNYYYApple2YYNYYApple1YYNYY";
         filter_ugly_test "../data/small_dining.csv" (6, "N")
           "PhoAsianYYYYNHamburgerAmericanNNNNN";
         filter_ugly_test "../data/small_dining.csv" (6, "Not an option") "";
         filter_ugly_test "../data/empty.csv" (2, "Apple") "";
         (*pretty printer tests*)
         filter_pretty_test "../data/small_dining.csv" (0, "Apple")
           "Apple\nApple\nApple\nApple\n\n";
         filter_pretty_test "../data/empty.csv" (2, ":))))") "\n";
         user_menu_test "" "../data/filter_pretty_empty_lines.csv"
           "Apple\nApple\n\nFried Rice\nApple\nHamburger\n\n\nPho\nApple\n\n";
         filter_invalid_csv_failure_test "../data/invalid_format.csv"
           (-2, ":))))");
         (*make menu tests*)
         (let expected_string =
            "AppleOtherYYYYYAppleFruitYYNYYApple2YYNYYApple1YYNYY"
          in
          make_menu_test "apple test" "../data/small_dining.csv" (0, "Apple")
            expected_string);
         (*test vegetarian*)
         (let expected_string =
            "AppleOtherYYYYYPhoAsianYYYYNAppleFruitYYNYYApple2YYNYYApple1YYNYY"
          in
          make_menu_test "Vegetarian test" "../data/second_small_dining.csv"
            (2, "Y") expected_string);
         (*test vegan*)
         (let expected_string =
            "AppleOtherYYYYYPhoAsianYYYYNAppleFruitYYNYYApple2YYNYYApple1YYNYY"
          in
          make_menu_test "Vegan test" "../data/second_small_dining.csv" (3, "Y")
            expected_string);
         (*test asian*)
         (let expected_string = "PhoAsianYYYYNFried RiceAsianNNYYY" in
          make_menu_test "Asia food test" "../data/second_small_dining.csv"
            (1, "Asian") expected_string);
         (*test american*)
         (let expected_string = "HamburgerAmericanNNNNN" in
          make_menu_test "American food test" "../data/second_small_dining.csv"
            (1, "American") expected_string);
         (*test mexican*)
         (let expected_string = "Chipotle Chicken ThighMexicanNNNYN" in
          make_menu_test "Mexican food test" "../data/second_small_dining.csv"
            (1, "Mexican") expected_string);
         (*test morrison*)
         (let expected_string =
            "AppleOtherYYYYYPhoAsianYYYYNFried RiceAsianNNYYY"
          in
          make_menu_test "Morrison Dining test"
            "../data/second_small_dining.csv" (4, "Y") expected_string);
         (*test appel*)
         (let expected_string =
            "Chipotle Chicken \
             ThighMexicanNNNYNAppleOtherYYYYYPhoAsianYYYYNAppleFruitYYNYYFried \
             RiceAsianNNYYYApple2YYNYYApple1YYNYY"
          in
          make_menu_test "Morrison Dining test"
            "../data/second_small_dining.csv" (5, "Y") expected_string);
         (*future add test okenshields*)
         menu_length_test "menu length test 1" "../data/empty.csv" 0;
         menu_length_test "menu length test 1" "../data/small_dining.csv" 7;
         menu_length_test "menu length test 1" "../data/second_small_dining.csv"
           8;
         menu_length_test "menu length test 1" "../data/appel.csv" 43;
         menu_length_test "menu length test 1" "../data/morrison.csv" 41;
         list_of_menu_test "small dining list of menu test"
           "../data/small_dining.csv" "Apple1YYNYY";
         list_of_menu_test "empty menu list of menu test" "../data/empty.csv" "";
       ]

let _ = run_test_tt_main tests
