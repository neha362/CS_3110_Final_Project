type t
(**type of a menu*)

type v
(**type of an adjustable menu*)

val empty : t
(**[empty] represents the empty menu with no additional foods.*)

val filter : t -> int * string -> t
(**[filter c f] is the csv located at the path [c] filtered to include only
   lines that match the desired filter parameters [f]. Raises: Exception
   Unknown_Index when the input index is not recognized and Invalid_Csv when the
   csv format is incorrect.*)

val make_menu : string -> t
(**[make_menu c] is the menu made by using the csv located at the file path [c]*)

val make_adjustable_menu : string -> v
(**[make_adjustable_menu c] returns a [string array list] that represents the
   adjustable menu contained in the file*)

val string_of_menu : t -> string
(**[string_of_menu m] is the string representation of the menu [m]*)

val user_menu : t -> string
(**[user_menu m] is the menu printed out in a user friendly output*)

val length_of_menu : t -> int
(**[lenght_of_menu menu] takes in a menu and retrieves how many entries are in
   it*)

val list_of_menu : t -> string list
(**[list_of_menu menu] takes in a menu and returns the first entries in a list
*)

val save_menu : t -> string list ref -> string -> unit
(**[save_menu menu headers file] takes in a menu, column headers, a file name,
   and saves the menu to the specified file.*)

val add_recipe : t -> string list -> t
(**[add_recipe menu recipe] extends a recipe list [menu] by adding a new recipe
   [recipe], which contains all information in the form of a [string list]*)

val find_recipes : BatSet.String.t -> t -> t
(**[find_recipes recipe_list recipe_book] returns the entries of the
   [recipe_book] including the recipe specifications (e.g. dietary restrictions
   or cuisine) of the recipes specified in [recipe_list]. Requires: all elements
   of [recipe_list] have a corresponding [recipe_book] entry, but not
   necessarily vice versa. *)

val to_csv : t -> Csv.t
(**[to_csv menu] returns a [Csv.t] representation of the dining [menu]. *)

val from_csv : Csv.t -> t
(**[from_csv menu] returns a [Dining.t] using the information contained in
   [menu]. Requires: [menu] is square and contains all the appropriate elements.
*)

val from_adjustable : v -> t
(**[from_adjustable c] takes in an adjustable CSV and returns a dining menu*)

val to_adjustable : t -> v
(**[to_adjustable c] takes in a dining menu and returns an ajustable CSV*)

exception Invalid_Csv of string
(**Raised when the csv input is not rectangular*)

val add_adjustable : v -> string array -> v
(**[add_adjustable]*)

val to_adjustable_csv : v -> string array array
(**[to_adjustable_csv] takes in an adjustable menu and returns a horizontally
   adjustable csv.*)

val from_adjustable_csv : string array array -> v
(**[from_adjustable_csv] takes in a horizontally adjustable csv and returns an
   adjustable menu. *)
