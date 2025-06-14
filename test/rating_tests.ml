open Final_project.Dining
open Final_project.Rating
open QCheck
open OUnit2
open Final_project

(**[make_rating (name, rating, hall, comments)] checks that the fields name,
   rating, hall, and comments are properly stored when converted to a [Rating.t]
   instance by [Rating.of_strings]*)
let make_rating (name, rating, hall, comments) =
  let r = of_strings name rating hall comments in
  get_name r = name
  && get_rating r = rating
  && get_hall r = hall
  && get_comments r = comments

(**[make_csv_entry name rating comments] checks that transforming the inputs
   [name] [rating] and [comments] to a readable csv format does not alter their
   inputs*)
let make_csv_entry (name, rating, hall, comments) =
  let r = to_csv_entry (of_strings name rating hall comments) in
  List.nth r 0 = name
  && List.nth r 1 = string_of_int rating
  && List.nth r 2 = hall
  && List.nth r 3 = comments

(**[get_name_test name rating comments] checks that the name is properly stored
   and returned, in string form, by [get_name]*)
let get_name_test (name, rating, hall, comments) =
  get_name (of_strings name rating hall comments) = name

(**[get_rating_test name rating comments] checks that the rating is properly
   stored and returned, in int form, by [get_rating]*)
let get_rating_test (name, rating, hall, comments) =
  get_rating (of_strings name rating hall comments) = rating

(**[get_comments_test (name, rating, hall, comments)] checks that the comments
   are properly stored and returned, in string form, by [get_comments]*)
let get_comments_test (name, rating, hall, comments) =
  get_comments (of_strings name rating hall comments) = comments

(**[get_hall_test (name, rating, hall, comments)] verifies that the dining hall
   is properly stored and returned in string form by [get_hall] *)
let get_hall_test (name, rating, hall, comments) =
  get_hall (of_strings name rating hall comments) = hall

(**[make_randomized_tuple func name] creates a randomized rating (consistng of a
   random string, an integer between 1 and 5, and another random string) and
   tests whether the test [func] is satisfied. *)
let make_randomized_tuple func name =
  QCheck.Test.make ~name ~count:1000
    (QCheck.make
       (QCheck.Gen.tup4 QCheck.Gen.string_printable (QCheck.Gen.int_range 1 5)
          QCheck.Gen.string_printable QCheck.Gen.string_printable)
       ~print:
         (QCheck.Print.tup4 QCheck.Print.string QCheck.Print.int
            QCheck.Print.string QCheck.Print.string))
    func

let example_ratings = Csv.load "../data/test_ratings.csv" ~strip:true
let name_asc = Csv.trim (Csv.load "../data/name_asc.csv" ~strip:true)
let name_desc = Csv.trim (Csv.load "../data/name_desc.csv" ~strip:true)
let hall_asc = Csv.trim (Csv.load "../data/hall_asc.csv" ~strip:true)
let hall_desc = Csv.trim (Csv.load "../data/hall_desc.csv" ~strip:true)
let rating_asc = Csv.trim (Csv.load "../data/rating_asc.csv" ~strip:true)
let rating_desc = Csv.trim (Csv.load "../data/rating_desc.csv" ~strip:true)

(**[check_equal_name] creates randomized tests verifying the sorting of the CSV
   files by name *)
let check_equal_name =
  QCheck.Test.make ~name:"Check sorting by name" ~count:1000 QCheck.bool
    (fun b ->
      print_newline ();
      Csv.compare
        (sort_name example_ratings b)
        (if b then (
           Csv.print_readable name_asc;
           name_asc)
         else (
           Csv.print_readable name_desc;
           name_desc))
      = 0)

(**[check_equal_hall] creates randomized tests verifying the sorting of the CSV
   files by hall *)
let check_equal_hall =
  QCheck.Test.make ~name:"Check sorting by hall" ~count:1000 QCheck.bool
    (fun b ->
      Csv.print_readable (sort_hall example_ratings b);
      print_newline ();
      Csv.compare
        (sort_hall example_ratings b)
        (if b then (
           Csv.print_readable hall_asc;
           hall_asc)
         else (
           Csv.print_readable hall_desc;
           hall_desc))
      = 0)

(**[check_equal_rating] creates randomized tests verifying the sorting of the
   CSV files by rating *)
let check_equal_rating =
  QCheck.Test.make ~name:"Check sorting by hall" ~count:1000 QCheck.bool
    (fun b ->
      Csv.print_readable (sort_rating example_ratings b);
      print_newline ();
      Csv.compare
        (sort_hall example_ratings b)
        (if b then (
           Csv.print_readable rating_asc;
           rating_asc)
         else (
           Csv.print_readable rating_desc;
           rating_desc))
      = 0)

let () =
  let _ =
    QCheck_runner.run_tests
      [
        make_randomized_tuple make_rating "Make a rating";
        make_randomized_tuple make_csv_entry "Make a CSV entry";
        make_randomized_tuple get_name_test "Get the name";
        make_randomized_tuple get_rating_test "Get the rating";
        make_randomized_tuple get_comments_test "Get the comments";
        make_randomized_tuple get_hall_test "Get dining hall";
        check_equal_name;
        check_equal_hall;
      ]
  in
  ()

(**[line_to_str lst] is the string representation of [lst]*)
let line_to_str lst = List.fold_left (fun a acc -> a ^ acc) "" lst

(**[list_list_to_str lst] is the string representation of [lst]*)
let list_list_to_str lst =
  List.fold_left (fun acc a -> acc ^ line_to_str a) "" lst

(**[retrieve_ratings_name_test n c f e] is the test with name [n] that checks if
   retrieving the ratings from [c] that match the name of the food item [f]
   returns [e]*)
let retrieve_ratings_name_test name csv filt exp =
  name >:: fun _ ->
  assert_equal exp
    (list_list_to_str Rating.(retrieve_ratings_name csv filt))
    ~printer:Fun.id

(**[retrieve_ratings_int_test n c f e] is the test with name [n] that checks if
   retrieving the ratings from [c] that match the rating [f] returns [e]*)
let retrieve_ratings_int_test name csv filt exp =
  name >:: fun _ ->
  assert_equal exp
    (list_list_to_str Rating.(retrieve_ratings_int csv filt))
    ~printer:Fun.id

(**[retrieve_ratings_int_range_test n c l h e] is the test with name [n] that
   checks if retrieving the ratings from [c] that match the ratings from [l] to
   [h] returns [e]*)
let retrieve_ratings_int_range_test name csv low high exp =
  name >:: fun _ ->
  assert_equal exp
    (list_list_to_str Rating.(retrieve_ratings_int_range csv low high))
    ~printer:Fun.id

(**[retrieve_ratings_hall_test n c f e] is the test with name [n] that checks if
   retrieving the ratings from [c] that match the dining hall [f] returns [e]*)
let retrieve_ratings_hall_test name csv filt exp =
  name >:: fun _ ->
  assert_equal exp
    (list_list_to_str Rating.(retrieve_ratings_hall csv filt))
    ~printer:Fun.id

(**[print_rating csv] is a helper function that allows us to test the filter
   function of on our csv files. *)
let rec print_rating m =
  match m with
  | [] -> "\n"
  | h :: t -> String.concat " " h ^ "\n" ^ print_rating t

(**[sort_rating_test n c f e] is the test with name [n] that checks if sorting
   the ratings in [c] in the order indicated by [f] returns [e]*)
let sort_rating_test name csv filt exp =
  name >:: fun _ ->
  assert_equal exp Rating.(print_rating (sort_rating csv filt)) ~printer:Fun.id

(**[sort_name_test] verifies that sorting the csv for the specified ame yields
   the correct filtered csv. *)
let sort_name_test name csv filt exp =
  name >:: fun _ ->
  assert_equal exp Rating.(print_rating (sort_name csv filt)) ~printer:Fun.id

(**[test_ratings_with_dups] verifies that the csv with duplicates is
   appropriately loaded.*)
let test_ratings_with_dups = Csv.load "../data/test_ratings_duplicates.csv"

(**[test_print_ratings] ensures that the ratings file is printed without raised
   exceptions*)
let test_print_ratings () =
  "testing printing" >:: fun _ ->
  let menu = "../data/rating_asc.csv" in
  Rating.print_ratings (Csv.load menu) menu;
  assert_bool "dummy test" true

let tests =
  "test suite"
  >::: [
         test_print_ratings ();
         retrieve_ratings_name_test "ratings name test 1" test_ratings_with_dups
           "Greek yogurt" "Greek yogurt5MorrisonYAYGreek yogurt2MorrisonOh";
         retrieve_ratings_name_test "ratings name test 2" test_ratings_with_dups
           "Raita" "Raita5Morrisonliterally my lifelineRaita3Morrisona";
         retrieve_ratings_name_test "ratings name test 3" test_ratings_with_dups
           "soaidoasiais" "";
         retrieve_ratings_name_test "ratings name test 4" test_ratings_with_dups
           "Pineapple" "Pineapple5Appelwell cut, i felt lovedPineapple4Appelb";
         retrieve_ratings_int_test "1 star ratings test" test_ratings_with_dups
           1 "Channa Masala1OakenshieldsNo. ";
         retrieve_ratings_int_test "2 star ratings test" test_ratings_with_dups
           2 "Greek yogurt2MorrisonOhBaked tofu2Morrisonaaa";
         retrieve_ratings_int_test "3 star ratings test" test_ratings_with_dups
           3
           "Baked tofu3Morrisongreat but the lady always glares at me \
            :(Raita3Morrisona";
         retrieve_ratings_int_test "4 star ratings test" test_ratings_with_dups
           4 "Pineapple4Appelb";
         retrieve_ratings_int_test "5 star ratings test" test_ratings_with_dups
           5
           "Greek yogurt5MorrisonYAYRaita5Morrisonliterally my \
            lifelinePineapple5Appelwell cut, i felt lovedChanna \
            Masala5OakenshieldsNo. ";
         retrieve_ratings_int_range_test "int range ratings test 1 to 5"
           test_ratings_with_dups 1 5
           "Greek yogurt5MorrisonYAYChanna Masala1OakenshieldsNo. \
            Raita5Morrisonliterally my lifelinePineapple5Appelwell cut, i felt \
            lovedBaked tofu3Morrisongreat but the lady always glares at me \
            :(Greek yogurt2MorrisonOhChanna Masala5OakenshieldsNo. \
            Raita3MorrisonaPineapple4AppelbBaked tofu2Morrisonaaa";
         retrieve_ratings_int_range_test "int range ratings test 2 to 5"
           test_ratings_with_dups 2 5
           "Greek yogurt5MorrisonYAYRaita5Morrisonliterally my \
            lifelinePineapple5Appelwell cut, i felt lovedBaked \
            tofu3Morrisongreat but the lady always glares at me :(Greek \
            yogurt2MorrisonOhChanna Masala5OakenshieldsNo. \
            Raita3MorrisonaPineapple4AppelbBaked tofu2Morrisonaaa";
         retrieve_ratings_int_range_test "int range ratings test 3 to 5"
           test_ratings_with_dups 3 5
           "Greek yogurt5MorrisonYAYRaita5Morrisonliterally my \
            lifelinePineapple5Appelwell cut, i felt lovedBaked \
            tofu3Morrisongreat but the lady always glares at me :(Channa \
            Masala5OakenshieldsNo. Raita3MorrisonaPineapple4Appelb";
         retrieve_ratings_int_range_test "int range ratings test 4 to 5"
           test_ratings_with_dups 4 5
           "Greek yogurt5MorrisonYAYRaita5Morrisonliterally my \
            lifelinePineapple5Appelwell cut, i felt lovedChanna \
            Masala5OakenshieldsNo. Pineapple4Appelb";
         retrieve_ratings_int_range_test "int range ratings test 5 to 5"
           test_ratings_with_dups 5 5
           "Greek yogurt5MorrisonYAYRaita5Morrisonliterally my \
            lifelinePineapple5Appelwell cut, i felt lovedChanna \
            Masala5OakenshieldsNo. ";
         retrieve_ratings_int_range_test "int range ratings test 5 to 0"
           test_ratings_with_dups 5 0 "";
         retrieve_ratings_int_range_test "int range ratings test 0 to 0"
           test_ratings_with_dups 0 0 "";
         retrieve_ratings_int_range_test "int range ratings test -3 to 0"
           test_ratings_with_dups (-3) 0 "";
         retrieve_ratings_int_range_test "int range ratings test -3 to 2"
           test_ratings_with_dups (-3) 2
           "Channa Masala1OakenshieldsNo. Greek yogurt2MorrisonOhBaked \
            tofu2Morrisonaaa";
         retrieve_ratings_hall_test "ratings by hall Morrison test"
           test_ratings_with_dups "Morrison"
           "Greek yogurt5MorrisonYAYRaita5Morrisonliterally my lifelineBaked \
            tofu3Morrisongreat but the lady always glares at me :(Greek \
            yogurt2MorrisonOhRaita3MorrisonaBaked tofu2Morrisonaaa";
         retrieve_ratings_hall_test "ratings by hall Oakenshields test"
           test_ratings_with_dups "Oakenshields"
           "Channa Masala1OakenshieldsNo. Channa Masala5OakenshieldsNo. ";
         retrieve_ratings_hall_test "ratings by hall Appel test"
           test_ratings_with_dups "Appel"
           "Pineapple5Appelwell cut, i felt lovedPineapple4Appelb";
         sort_rating_test "sort ascending test ratings test"
           test_ratings_with_dups true
           "Channa Masala 1 Oakenshields No. \n\
            Baked tofu 2 Morrison aaa\n\
            Greek yogurt 2 Morrison Oh\n\
            Baked tofu 3 Morrison great but the lady always glares at me :(\n\
            Raita 3 Morrison a\n\
            Pineapple 4 Appel b\n\
            Channa Masala 5 Oakenshields No. \n\
            Greek yogurt 5 Morrison YAY\n\
            Pineapple 5 Appel well cut, i felt loved\n\
            Raita 5 Morrison literally my lifeline\n\n";
         sort_rating_test "sort descending test ratings test"
           test_ratings_with_dups false
           "Channa Masala 5 Oakenshields No. \n\
            Greek yogurt 5 Morrison YAY\n\
            Pineapple 5 Appel well cut, i felt loved\n\
            Raita 5 Morrison literally my lifeline\n\
            Pineapple 4 Appel b\n\
            Baked tofu 3 Morrison great but the lady always glares at me :(\n\
            Raita 3 Morrison a\n\
            Baked tofu 2 Morrison aaa\n\
            Greek yogurt 2 Morrison Oh\n\
            Channa Masala 1 Oakenshields No. \n\n";
         sort_rating_test "sort ascending empty test"
           (Csv.load "../data/empty.csv")
           true "\n";
         sort_name_test "sort descending test name test" test_ratings_with_dups
           false
           "Raita 3 Morrison a\n\
            Raita 5 Morrison literally my lifeline\n\
            Pineapple 4 Appel b\n\
            Pineapple 5 Appel well cut, i felt loved\n\
            Greek yogurt 2 Morrison Oh\n\
            Greek yogurt 5 Morrison YAY\n\
            Channa Masala 1 Oakenshields No. \n\
            Channa Masala 5 Oakenshields No. \n\
            Baked tofu 2 Morrison aaa\n\
            Baked tofu 3 Morrison great but the lady always glares at me :(\n\n";
       ]

let _ = run_test_tt_main tests
