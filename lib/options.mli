val halls : string list ref
(**[halls] keeps track of the different dining halls that are supported by this
   project*)

val cuisines_file_path : string
(**[cuisines_file_path] keeps track of the file path name that stores the
   cuisines*)

val read_cuisines : unit -> unit
(**[read_cuisines ()] initializes [cuisines_list] using the CSV stored at the
   file path designated by [cuisines_file_path]*)

val write_cuisines : unit -> unit
(**[write_cuisines ()] writes the existing list of cuisines to the specified
   file [cuisines_file_path]*)

val cuisines_list : string list ref
(**[cuisines] keeps track of the different cuisines that are supported by this
   project. *)

val dietary : string list ref
(**[dietary] keeps track of the different dietary restrictions that are
   supported by this project. *)

val get_all_filters : unit -> string list ref
(**[get_all_filters ()] returns a comprehensive list of all the filters that are
   supported by this project. *)

val days_of_week : (int * string) list
(**[days_of_week] documents the conversion between [int] and [string] to convert
   days of the week, with 0 corresponding to "Sunday". This mirrors the
   implementation used by the Unix library*)

val meal_types : (string * string list ref) list ref
(**[meal_types] documents the types of meals (e.g. Lunch) that are offered at
   each dining hall. *)

val recipe_book : Dining.t ref
(**[recipe_book] keeps track of all the different dishes offered by Cornell
   Dining, in a master list of sorts. *)

val master_file_name : string
(**[master_file_name] stores the name of the file that contains the recipe_book.
*)

val recipes : string BatSet.t ref
(**[recipes] stores all possible meals made by Cornell Dining. *)

val meats : string list
(**[meats] keeps track of the different meats that are used by Cornell Dining.
   Used mainly for purposes with entering data.*)
