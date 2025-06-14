open Final_project
open Final_project.Dining
open Final_project.Rating
open Final_project.Options
open Final_project.Dictionary
open Final_project.Plates
open Final_project.Fetch_data
open Unix
open Fetch_data

(* EXCEPTIONS AND FILES USED -------------------------------------------------
   ------------------------------------------------------------------------------*)

exception Unknown_Correspondence
exception Invalid_input
exception Missing_input

exception Illegal_rating of int
(**[Illegal_rating rating] documents the event that the user inputs a rating
   that is deemed illegal by the specifications in [rating.mli]*)

let rating_file = Csv.load "./driver_data/ratings.csv"
let food_dict = create_dictionary "./driver_data/fooddict.csv"
let total_dict = create_dictionary "./data/dictionary.csv"
let dining_dict = create_dictionary "./driver_data/diningdict.csv"
let user_plates = Csv.load "./driver_data/userplates.csv"
let all_dishes = ref !Options.recipe_book

(**[print_menu f lst] is a function that prints out a numerical list of options
   for a user to view. [f] is an argument that allows the function to print out
   the options, in the case that they are not of type [string]. *)
let print_menu f lst =
  List.iteri
    (fun index item ->
      print_endline (string_of_int (index + 1) ^ ") " ^ f item))
    lst

(**[yield_value lst f] returns a valid option out of the options provided in
   [lst]*)
let rec yield_value lst f =
  try
    let check_valid_bounds index lst = index < List.length lst && index >= 0 in
    print_menu f lst;
    let index = read_int () - 1 in
    if not (check_valid_bounds index lst) then
      raise (Sys_error (string_of_int index))
    else (
      print_endline ("You have selected " ^ f (List.nth lst index) ^ ". ");
      List.nth lst index)
  with _ ->
    print_endline "Please enter a valid choice.\n";
    yield_value lst f

(**[formatted_day day] takes in a day of type [Unix.tm] and returns it in
   YYYY-MM-DD form. *)
let formatted_day (day : tm) =
  Printf.(
    sprintf "%04d" (day.tm_year + 1900)
    ^ "-"
    ^ sprintf "%02d" (day.tm_mon + 1)
    ^ "-" ^ sprintf "%02d" day.tm_mday)

(**[query file_name] verifies whether the file [file_name] exists. If not, the
   data representing the current week's dining menu is updated. *)
let query = Sys.file_exists

(**[exit] handles all cases for the user to exit the interface*)
let exit () =
  print_endline "Thank you for visiting!";
  exit 0

(* CURRENTLY SUPPORTED FILTERS AND OPERATIONS --------------------------------*)

let all_supported_filters = ref []
let user_filters = ref []
let user_halls = ref !Options.halls

(**[display_all_filters] takes in all the current available filters and prints
   each one.*)
let display_all_filters list =
  let all_filters = !list in
  List.iteri
    (fun index filters ->
      print_string (string_of_int (index + 1) ^ ") ");
      print_endline filters)
    all_filters

(**[remove_item] takes in some category to be removed from the total possible
   filtering options.*)
let remove_item filter =
  user_filters := List.filter (fun x -> x <> filter) !user_filters

(**[remove_item] takes in some category to be removed from the total possible
   filtering options.*)
let remove_hall filter =
  user_halls := List.filter (fun x -> x <> filter) !user_halls

(**[choose_cuisines ()] handles the case where the user wants to sort for a
   certain cuisine*)
let rec choose_cuisines () =
  try
    print_endline "Choose the specific cuisine you want to filter by: ";
    user_filters := List.filter (fun x -> x <> "Cuisine") !user_filters;
    List.iteri
      (fun index cuisine ->
        if index mod 2 = 0 then Printf.printf "%d) %s\n" index cuisine
        else Printf.printf "%d) %s\t\t" index cuisine)
      !cuisines_list;
    let input = read_line () in
    let category =
      List.nth !cuisines_list
        (try
           let input_num = input |> int_of_string in
           if input_num < List.length !cuisines_list && input_num >= 0 then
             input_num
           else raise Unknown_Correspondence
         with _ -> raise Unknown_Correspondence)
    in
    let cuisine =
      List.find_index (( = ) "Cuisine") (!recipe_book |> to_csv |> List.hd)
    in
    if Option.is_none cuisine then raise Unknown_Correspondence
    else
      let index = Option.get cuisine in
      (index, category)
  with _ ->
    print_endline "Please choose a valid cuisine.\n";
    choose_cuisines ()

(**[finding_category index] finds the specific corresponding category for the
   user's choice in the driver and acts accordingly.*)
let finding_category index =
  let category = List.nth !user_filters (index - 1) in

  if category = "Cuisine" then choose_cuisines ()
  else if List.mem category !dietary then
    let y =
      List.find_index (( = ) category) (!recipe_book |> to_csv |> List.hd)
    in
    if Option.is_none y then raise Unknown_Correspondence
    else
      let index = Option.get y in
      (index, "Y")
  else
    let y = List.find_index (( = ) category) !Options.halls in
    if Option.is_some y then (Option.get y + 2 + List.length !dietary, "Y")
    else raise Unknown_Correspondence

(**[finding_day day hall meal] takes in the specific day's menu that the user
   wants and has the rest of the program parse accordingly.*)
let rec finding_day day hall meal =
  let now = localtime (time ()) in
  let date =
    mktime
      { now with tm_mday = now.tm_mday - (now.tm_wday - int_of_string day) }
    |> snd
  in
  let file_name =
    "./dining_data/" ^ hall ^ formatted_day date ^ meal ^ ".csv"
  in
  if query file_name then file_name
  else
    let date =
      mktime
        {
          now with
          tm_mday = now.tm_mday - (now.tm_wday - int_of_string day) + 7;
        }
      |> snd
    in
    let file_name =
      "./dining_data/" ^ hall ^ formatted_day date ^ meal ^ ".csv"
    in
    if query file_name then file_name else ""

(**[reset_filters_halls filter] removes all fitlers except the one hall
   specified. Requires: [filter] is one of the filters specified in
   Options.halls*)
let reset_filters_halls filter =
  List.iter
    (fun x ->
      if x = filter then () else remove_hall x;
      remove_item x)
    !Options.halls

(**[updating_filter_choices] removes from the possible choices displayed to the
   user all the conflicting options.*)
let updating_filter_choices input =
  if List.mem input !cuisines_list then
    cuisines_list := List.filter (( <> ) input) !cuisines_list
  else remove_item input

(**[valid_choices] is an error checker that makes sure that the user input is
   one of the supports ones.*)
let valid_choices input =
  try
    if
      int_of_string input <= List.length !(Options.get_all_filters ())
      && int_of_string input >= 1
    then input
    else raise Unknown_Correspondence
  with _ -> raise Unknown_Correspondence

(* SPELLCHECKER ---------------------------------------------------------------
   ------------------------------------------------------------------------------*)

(**[check_user_spelling] takes in the user input for a food and then spellchecks
   it so it is consistent with our menus. *)
let check_user_spelling input food_dictionary total_dictionary =
  if String.length input = 0 then raise Invalid_input
  else
    let letters = Str.regexp "[a-zA-Z]+" in
    let rec process_words acc begin_pos =
      try
        let matched_pos = Str.search_forward letters input begin_pos in
        let found_word = Str.matched_string input in
        let word = String.lowercase_ascii found_word in
        let prev_words =
          if matched_pos > begin_pos then
            acc ^ String.sub input begin_pos (matched_pos - begin_pos)
          else acc
        in
        let corrected =
          if check_spelling word food_dictionary then found_word
          else
            try corrected_word word food_dictionary
            with Word_Not_Found _ -> (
              try corrected_word word total_dictionary
              with Word_Not_Found _ -> word)
        in
        let new_pos = matched_pos + String.length found_word in
        let corrected_capitalization =
          if String.length corrected > 1 then
            String.uppercase_ascii (String.sub corrected 0 1)
            ^ String.sub corrected 1 (String.length corrected - 1)
          else String.uppercase_ascii (String.sub corrected 0 1)
        in
        process_words (prev_words ^ corrected_capitalization) new_pos
      with Not_found ->
        acc ^ String.sub input begin_pos (String.length input - begin_pos)
    in
    process_words "" 0

(* RATINGS OPERATIONS --------------------------------------------------------*)

(**[make_rating] prompts the user for what food they would like to rate, what
   rating they would like to give, and which dining hall they had it at. Also
   writes to a csv with ratings for each feedback given.*)
let make_rating () =
  try
    print_endline "Which food would you like to leave a rating for?";
    let user_input_name = read_line () in
    let name = check_user_spelling user_input_name food_dict total_dict in
    print_endline
      "With 1 star being poor and 5 stars being excellent, how would you rate \
       this food? (1, 2, 3, 4, or 5)";
    let rating = read_int () in
    if rating > 5 || rating < 1 then raise (Illegal_rating rating)
    else begin
      print_endline "Which dining hall did you try this food at?";
      print_endline " ";
      let hall = yield_value !Options.halls Fun.id in
      print_endline "Please leave any additional comments.";
      print_endline " ";
      let comments = read_line () in
      let out =
        Csv.to_channel
          (Out_channel.open_gen
             [ Out_channel.Open_append ]
             0 "./driver_data/ratings.csv")
      in
      let rating_obj = to_csv_entry (of_strings name rating hall comments) in
      Csv.output_record out rating_obj;
      Csv.close_out out;
      print_endline "Thank you for your review!"
    end
  with
  | Illegal_rating rating ->
      Printf.printf
        "Sorry, %d is not a valid input. Please enter a value between 1 and 5.\n"
        rating
  | Failure read_int ->
      print_endline
        "Sorry, the value you entered cannot be represented as an integer. \
         Please enter an integer between 1 and 5, inclusive. "
  | Invalid_input -> print_endline "Sorry, please input a valid input"

(**[retrieve_rating] takes care of retrieving the specific category of rating
   the user wants.*)
let rec retrieve_rating () =
  print_endline
    "Please indicate the category of rating you would like to view: \n\
     1) View rating for specific dish \n\
     2) View foods with specific rating \n\
     3) View foods within a range of ratings \n\
     4) View ratings for a specific dining hall";
  let input = read_line () in
  try
    match input with
    (*user wants specific dish*)
    | "1" ->
        print_endline
          "Please enter in the dish you would like to view a rating for";
        let user_input_dish = read_line () in
        let dish_name =
          check_user_spelling user_input_dish food_dict total_dict
        in
        let dish_ratings = retrieve_ratings_name rating_file dish_name in
        if dish_ratings = [] then print_endline "No ratings found for dish"
        else print_ratings dish_ratings "./driver_data/ratings.csv"
    (*user wants specific rating*)
    | "2" ->
        print_endline
          "Please enter in the rating number from 1-5 you would like to view";
        let number = read_line () in
        let rating = int_of_string number in
        if rating < 1 || rating > 5 then raise (Illegal_rating rating)
        else
          let certain_rating = retrieve_ratings_int rating_file rating in
          print_ratings certain_rating "./driver_data/ratings.csv"
    (*user wants rating range*)
    | "3" ->
        print_endline
          "You have chosen to view reviews that fall between certain ratings: \n\
          \          Please indicate the lower range of reviews (1-5):";
        let rating1 = int_of_string (read_line ()) in
        print_endline " Please indicate the upper range of reviews (1-5):";
        let rating2 = int_of_string (read_line ()) in
        if rating1 > rating2 || rating1 < 0 || rating2 > 5 then
          raise (Illegal_rating 1)
        else
          let range_ratings =
            retrieve_ratings_int_range (rating_file |> List.tl) rating1 rating2
          in
          print_ratings range_ratings "./driver_data/ratings.csv"
    (*user wants specific dining hall*)
    | "4" ->
        print_endline
          "Please enter in the dining hall name you would like to view";
        let hall = yield_value !Options.halls Fun.id in
        let hall_ratings = retrieve_ratings_hall rating_file hall in
        if hall_ratings = [] then print_endline "No ratings found for this hall"
        else print_ratings hall_ratings "./driver_data/ratings.csv"
    | _ ->
        print_endline "Please choose only one of the above";
        retrieve_rating ()
  with
  | Failure _ ->
      print_endline "Please input a valid choice";
      retrieve_rating ()
  | Invalid_input -> begin
      print_endline "- Please put in a dish name - ";
      print_endline "";
      retrieve_rating ()
    end
  | Illegal_rating 1 -> begin
      print_endline " - Please make sure your range is valid - ";
      print_endline "";
      retrieve_rating ()
    end
  | Illegal_rating rating -> begin
      Printf.printf
        "- Sorry, %d is not a valid input. Please enter a value between 1 and \
         5. - \n"
        rating;
      print_endline "";
      retrieve_rating ()
    end

(**[handle_feedback] handles the different cases of giving or receiving feedback
   that the user may want. *)
let rec handle_feedback () =
  print_endline "Would you like to submit feedback or see a rating?";
  print_endline "1) Input feedback";
  print_endline "2) View rating";
  let input = read_line () in
  if input = "1" then make_rating ()
  else if input = "2" then retrieve_rating ()
  else (
    print_endline "Please choose one of the valid options.";
    handle_feedback ())

(* PLATE MAKER ---------------------------------------------------------------
   ---------------------------------------------------------------------------*)

(**[printing_options_for_plates] takes all the current filters and prints them
   out to the user to choose from. *)
let printing_options_for_plates () =
  let num = ref 1 in
  begin
    List.iter
      (fun filters ->
        print_endline
          (string_of_int !num ^ ") " ^ "Contains: " ^ filters ^ " cuisine");
        num := !num + 1)
      !Options.cuisines_list;

    List.iter
      (fun filters ->
        print_endline (string_of_int !num ^ ") " ^ "Is " ^ filters);
        num := !num + 1)
      !dietary;

    List.iter
      (fun filters ->
        print_endline (string_of_int !num ^ ") " ^ "Is from: " ^ filters);
        num := !num + 1)
      !halls
  end

(**[custom_categorization] is to categorize dishes that are not yet known in the
   menus. *)
let custom_categorization dish_name dining_hall =
  if String.length dish_name = 0 then None
  else
    let dish = ref [ "N"; "N"; "None"; dish_name ] in
    match List.find_index (( = ) dining_hall) !Options.halls with
    | None ->
        for i = 0 to List.length !Options.halls - 1 do
          dish := "N" :: !dish
        done;
        Some (List.rev !dish)
    | Some index ->
        for i = 0 to index - 1 do
          dish := "N" :: !dish
        done;
        dish := "Y" :: !dish;
        for i = 0 to List.length !Options.halls - index - 1 do
          dish := "N" :: !dish
        done;
        Some (List.rev !dish)

let array_of_plates = ref []

(**[storing_all_plates] takes all of the plates currently saved and then stores
   them into a list for easy access.*)
let storing_all_plates () =
  let csv_plates = Csv.load "./driver_data/userplates.csv" in
  List.iter
    (fun row ->
      let plate_name = ref "" in
      let hall_name = ref "" in
      let user_comments = ref "" in
      let user_drink = ref None in
      let main_dish = ref None in
      let protein = ref None in
      let side_dish = ref None in
      let dessert = ref None in

      List.iteri
        (fun index item ->
          match index with
          | 0 -> plate_name := item
          | 1 -> hall_name := item
          | 2 -> user_comments := item
          | 3 ->
              user_drink :=
                if item = "None" then None
                else if
                  List.length (list_of_menu (filter !all_dishes (0, item))) = 0
                then custom_categorization item !hall_name
                else Some (list_of_menu (filter !all_dishes (0, item)))
          | 4 ->
              main_dish :=
                if item = "None" then None
                else if
                  List.length (list_of_menu (filter !all_dishes (0, item))) = 0
                then custom_categorization item !hall_name
                else Some (list_of_menu (filter !all_dishes (0, item)))
          | 5 ->
              protein :=
                if item = "None" then None
                else if
                  List.length (list_of_menu (filter !all_dishes (0, item))) = 0
                then custom_categorization item !hall_name
                else Some (list_of_menu (filter !all_dishes (0, item)))
          | 6 ->
              side_dish :=
                if item = "None" then None
                else if
                  List.length (list_of_menu (filter !all_dishes (0, item))) = 0
                then custom_categorization item !hall_name
                else Some (list_of_menu (filter !all_dishes (0, item)))
          | 7 ->
              dessert :=
                if item = "None" then None
                else if
                  List.length (list_of_menu (filter !all_dishes (0, item))) = 0
                then custom_categorization item !hall_name
                else Some (list_of_menu (filter !all_dishes (0, item)))
          | _ -> ())
        row;
      let new_plate =
        make_plate !plate_name !hall_name !user_comments !user_drink !main_dish
          !protein !side_dish !dessert
      in
      array_of_plates := new_plate :: !array_of_plates)
    csv_plates

(**[make_plates] allows for a user to input a plate that they would like to save*)
let rec make_plates () =
  try
    print_endline "What you like your plate to be named?\n";
    let plate_name = read_line () in
    if String.length plate_name = 0 then raise Missing_input
    else
      print_endline
        "What dining hall are the contents of your plate built from?\n";
    let user_dining_hall = yield_value !halls Fun.id in
    let hall = check_user_spelling user_dining_hall dining_dict dining_dict in
    if String.length hall = 0 then raise Missing_input
    else print_endline "What was the main dish? Please input dish name or Skip.";
    let main_dish_name =
      let user_main_dish = read_line () in
      if user_main_dish = "skip" || user_main_dish = "Skip" then "None"
      else check_user_spelling user_main_dish food_dict total_dict
    in
    print_endline "What was the side dish? Please input dish name or Skip.";
    let side_dish_name =
      let user_side_dish = read_line () in
      if user_side_dish = "skip" || user_side_dish = "Skip" then "None"
      else check_user_spelling user_side_dish food_dict total_dict
    in
    print_endline
      "What drink would you like to add? Please input drink or Skip.";
    let user_drink =
      let user_drink_input = read_line () in
      if user_drink_input = "skip" || user_drink_input = "Skip" then "None"
      else user_drink_input
    in
    print_endline
      "What protein would you like to add? Please input dish name or Skip";
    let user_protein = read_line () in
    let protein = check_user_spelling user_protein food_dict total_dict in
    print_endline
      "What dessert would you like to add? Please input dish name or Skip";
    let user_dessert =
      let user_dessert_input = read_line () in
      if user_dessert_input = "skip" || user_dessert_input = "Skip" then "None"
      else user_dessert_input
    in
    print_endline "Do you have any additional comments?";
    let user_comments = read_line () in
    let out =
      Csv.to_channel
        (Out_channel.open_gen
           [ Out_channel.Open_append ]
           0 "./driver_data/userplates.csv")
    in
    let plate_obj =
      [
        plate_name;
        hall;
        user_comments;
        user_drink;
        main_dish_name;
        protein;
        side_dish_name;
        user_dessert;
      ]
    in
    Csv.output_record out plate_obj;
    Csv.close_out out;
    print_endline "Thank you for inputting your plate!"
  with
  | Invalid_input -> begin
      print_endline "Sorry, please input a valid food item"
    end
  | Missing_input -> begin
      print_endline "Sorry, you must provide an input for this."
    end

(**[plate_filtering] takes in the input for the filtering and all of the plates
   currently saved and then returns the ones that satisfies the filters. *)
let plate_filtering num all_plates =
  if num <= List.length !cuisines_list then
    List.filter
      (fun plate ->
        List.mem (List.nth !cuisines_list (num - 1)) (cuisines plate))
      all_plates
  else if num - List.length !cuisines_list <= List.length !dietary then
    let x = num - List.length !cuisines_list in
    let selected_diet = List.nth !dietary (x - 1) in
    List.filter
      (fun plate ->
        match selected_diet with
        | "Vegetarian" -> is_vegetarian plate
        | "Vegan" -> is_vegan plate
        | _ -> false)
      all_plates
  else if
    num - List.length !cuisines_list - List.length !dietary
    <= List.length !halls
  then
    let x = num - List.length !cuisines_list - List.length !dietary in
    List.filter
      (fun plate -> hall plate = List.nth !Options.halls (x - 1))
      all_plates
  else all_plates

(**[plates_printing] prints out all of the specified plates*)
let plates_printing plates =
  print_endline
    "Here are all the plates that other users have made that satisfy your \
     requirements";
  print_endline "----------------------------------------------------";
  List.iter
    (fun plate ->
      print_endline ("Plate name: " ^ name plate);
      print_endline ("Hall: " ^ hall plate);
      print_endline ("Main dish: " ^ main plate);
      print_endline ("Side dish: " ^ side plate);
      print_endline ("Protein: " ^ protein plate);
      print_endline ("Dessert: " ^ dessert plate);
      print_endline ("Drink: " ^ drink plate);
      print_endline "----------------------------------------------------")
    plates

(**[plate_access] asks the user for which filters they would like to apply to
   view certain plates with those attributes. *)
let plate_access () =
  try
    storing_all_plates ();
    let num_option =
      List.length !halls
      + List.length !Options.cuisines_list
      + List.length !dietary
    in
    print_endline
      "What type of plate would you like access? Please indicate with a number:";
    print_endline "These are the possible attributes: ";
    printing_options_for_plates ();
    let user_input = read_int () in
    if user_input < 1 || user_input > num_option then raise Invalid_input
    else
      let plates_collection = plate_filtering user_input !array_of_plates in
      if plates_collection = [] then
        print_endline "There were no plates satisfying your chosen attributes "
      else begin
        print_endline "Here are some users' saved plates!";
        plates_printing plates_collection
      end
  with
  | Failure _ ->
      print_endline "Please only input an integer for one of the above options"
  | Invalid_input ->
      print_endline "Please only input an integer for one of the above options"

(**[view_make_plate] handles the interactions between the users and the saved
   users plates.*)
let rec view_make_plate () =
  print_endline
    "Would you like to view other user's save plates or create your own?";
  print_endline "1) View\n2) Create";
  let user_input = read_line () in
  if user_input = "1" then plate_access ()
  else if user_input = "2" then make_plates ()
  else begin
    print_endline "Please choose one or two";
    view_make_plate ()
  end

(* DISPLAYS & INTERACTIONS-----------------------------------------------------
   ---------------------------------------------------------------------------*)

(**[print_by_dining_hall] prints the dining halls and the dishes they have that
   satisfies the user's needs.*)
let print_by_dining_hall filtered_list =
  let print_hall hall =
    let index =
      Option.get
        (List.find_index (( = ) hall) (!recipe_book |> to_csv |> List.hd))
    in
    let menu = filter filtered_list (index, "Y") in
    print_endline " ";
    print_endline (hall ^ " options -----------------");
    print_endline " ";
    if length_of_menu menu = 0 || not (List.mem hall !user_halls) then
      print_endline "None"
    else print_string (user_menu menu)
  in
  List.iter print_hall !user_halls

(**[prompter] continually prompts the user to add in more filter options if they
   way, or the user can choose to finish and see the foods they have chosen.*)
let rec prompter created_menu =
  try
    if List.length !user_filters = 0 then begin
      print_endline "No more filters to apply, here are the results: ";
      print_by_dining_hall !created_menu;
      exit ()
    end
    else print_endline "-------------------------------------------------";
    print_endline
      "Please choose what type of cuisine you would like to see, \n\
      what dietary restriction you would like to filter by, \n\
       or if you are done selecting: ";
    display_all_filters user_filters;
    print_endline "Done";
    print_endline " ";
    let user_input = read_line () in
    if user_input = "Done" then begin
      print_by_dining_hall !created_menu
    end
    else
      let valid_input = valid_choices user_input in
      let filter_cat = int_of_string valid_input |> finding_category in
      let corresponding =
        List.nth (!recipe_book |> to_csv |> List.hd) (fst filter_cat)
      in
      let filter_category =
        if corresponding <> "Cuisine" then corresponding else snd filter_cat
      in
      updating_filter_choices filter_category;
      created_menu := filter !created_menu filter_cat;
      prompter created_menu
  with Unknown_Correspondence ->
    begin
      print_endline "Please only choose one of the accepted categories";
      prompter created_menu
    end

let meal_exists day_chosen hall_chosen meal_chosen =
  finding_day (day_chosen |> string_of_int) hall_chosen meal_chosen <> ""

(**[menu_view] handles the user path where the user chooses to view a menu. *)
let rec menu_view () =
  try
    print_endline
      "\nPlease choose what day you would like to see the menu for: \n";
    let day_chosen = yield_value Options.days_of_week snd |> fst in
    print_endline
      "\nPlease choose which dining hall you'd like to view the menu for: \n";
    let hall_chosen = yield_value !Options.halls Fun.id in
    user_halls := [ hall_chosen ];
    user_filters :=
      List.filter (fun cat -> not (List.mem cat !Options.halls)) !user_filters;
    all_supported_filters :=
      List.filter
        (fun cat -> not (List.mem cat !Options.halls))
        !all_supported_filters;

    let available_meals =
      List.filter
        (meal_exists day_chosen hall_chosen)
        !(List.assoc hall_chosen !Options.meal_types)
    in
    if available_meals = [] then begin
      print_endline
        "No meals available for that dining hall on the selected day.";
      exit ()
    end
    else
      print_endline "\nPlease choose a meal you'd like to view the menu for: \n";
    let meal_chosen =
      yield_value
        (List.filter
           (meal_exists day_chosen hall_chosen)
           !(List.assoc hall_chosen !Options.meal_types))
        Fun.id
    in
    let csv_file =
      finding_day (string_of_int day_chosen) hall_chosen meal_chosen
    in
    let curated_menu = ref (make_menu csv_file) in
    prompter curated_menu
  with
  | Sys_error x ->
      let actual_input = int_of_string x + 1 in
      print_endline
        (string_of_int actual_input
       ^ " is out of bounds for the required selection.")
  | read_int -> print_endline "Please enter a valid integer. "

let () =
  try
    print_endline "Would you like to update the dining menu? (Y to update)";
    Fetch_data.fetch (read_line () = "Y") ();
    user_filters := !(Options.get_all_filters ());
    all_dishes := !Options.recipe_book;
    all_supported_filters := !user_filters;
    (*create an empty list list of food [food] display to user all the days they
      can choose from [food] gets loaded by the csv of the particular day.*)
    print_endline
      "Would you like to view feedback/leave a rating, filter menus, \
       view/create plates, or exit?";
    print_endline
      "1) View feedback/Leave a rating\n\
       2) View menus\n\
       3) View/Create plates\n\
       4) Exit";
    let user_input = read_line () in
    if user_input = "1" then handle_feedback ()
    else if user_input = "3" then view_make_plate ()
    else if user_input = "4" then exit ()
    else if user_input = "2" then menu_view ()
    else raise Unknown_Correspondence
  with
  | Unknown_Correspondence ->
      print_endline "please input only one of the choices"
  | Invalid_Csv "Invalid CSV format" -> print_endline "Invalid CSV format"
  | Failure _ -> print_endline "please input one of the choices"
  | Sys_error _ -> print_endline "please input a number for your selection"
