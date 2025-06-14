open Csv

type t = string list list
type v = string array array
(*AF: Type [t] represents a shared menu across all the dining halls on campus; the string list [a1;a2;...;an] element of [t] represents the food item with name = a1, vegetarian = a2, vegan = a3, and, so on, with each list index corresponding to a restriction.  Type [v] represents an adjustable version of type [t], allowing for additional columns and entries*)
(*RI: Each equal-numbered index of each element (e.g. the first element of the first list and the first element of the second list) must correspond to the same category (e.g. name). *)

exception Invalid_Csv of string

let empty = []

(**[check m f h a] is the helper for filter' to either append [h] to [acc] or
   return only [acc]*)
let check m f h acc = if m = snd f then h :: acc else acc

(**[filter' c f a] returns the csv [c] filtered to include only food items that
   match the filter parameter [f]. Requires: [c] is a valid CSV according to the
   spec. [filt] should be a value between 0 and 6. Raises [Invalid_Csv] if the
   CSV is malformatted. *)
let rec filter' (csv : t) (filt : int * string) (acc : t) =
  if not (fst filt >= 0) then raise (Invalid_argument "negative filter")
  else
    match csv with
    | [] -> acc (*reached end of csv file*)
    | h :: t ->
        if fst filt >= List.length h then acc
        else filter' t filt (check (List.nth h (fst filt)) filt h acc)

(**[filter csv prefs] returns the filtered entries of [csv] that match [prefs].
   Requires: [prefs] is a tuple of type [int * string], with the first argument
   being an integer from 0 through 6, representing the category of the csv file
   (check filter' for specifications) and the second argument being a setting.
   For instance, [prefs = (0, "Apple")] returns all the dining options where
   [name = "Apple"]. Requires: [csv] is a valid dining file with entries of the
   form [string;string;(Y/N);(Y/N);(Y/N);(Y/N);(Y/N)]*)
let filter csv prefs = filter' csv prefs []

(**[make_menu c] loads the list of dining options available for a day from file
   path [c]. Requires: [c] is a file path to a valid dining csv file. See
   [filter] documentation for specification.*)
let make_menu c = Csv.load c

(**[make_adjustable_menu c] takes the file path [c] and converts it to an
   adjustable menu. *)
let make_adjustable_menu c =
  List.map Array.of_list (Csv.load c) |> Array.of_list

(**[string_of_list m acc] is the string consisting of the elements in [m]
   appended to [acc]. Requires [m] is a string list*)
let rec string_of_list m acc = List.fold_left ( ^ ) acc m

(**[string_of_menu' m acc] is the helper function to make a string list list
   into a string using tail recursion*)
let rec string_of_menu' m acc =
  List.fold_left (fun acc lst -> acc ^ string_of_list lst "") acc m

(**[string_of_menu m] returns the menu [m] represented as a string. Requires:
   [m] is a CSV file fitting the specifications noted in [filter] documentation.
*)
let string_of_menu m = string_of_menu' m ""

let rec user_menu m =
  match m with
  | [] -> "\n"
  | h :: t -> List.hd h ^ "\n" ^ user_menu t

let length_of_menu (menu : string list list) = List.length menu

let list_of_menu (filtered_menu : t) =
  match filtered_menu with
  | [] -> []
  | h :: _ -> h

let save_menu menu headers file_name = Csv.save file_name (!headers :: menu)
let add_recipe menu recipe = recipe :: menu
let add_adjustable menu recipe = Array.append [| recipe |] menu

let find_recipes recipe_list recipe_book =
  List.filter
    (fun row -> BatSet.String.mem (List.hd row) recipe_list)
    recipe_book

let to_csv = Fun.id
let from_csv = Fun.id
let to_adjustable menu = List.map Array.of_list menu |> Array.of_list
let from_adjustable menu = Array.map Array.to_list menu |> Array.to_list
let to_adjustable_csv = Fun.id
let from_adjustable_csv = Fun.id
