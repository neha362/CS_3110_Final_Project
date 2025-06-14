type t
(**[t] represents the food that the user is leaving a rating on. *)

val get_name : t -> string
(**[get_food_name rating] returns the food that the user left a review of,
   represented as a [string]. *)

val get_rating : t -> int
(**[get_rating rating] returns the "stars" that the user gave the food,
   represented as an [int]. 1 star corresponds to a poor review, 5 stars
   correspond to a fantastic review. *)

val get_comments : t -> string
(**[get_comments rating] returns the comments that the user left in their
   rating, if any. The comments are returned as represented in a [string]*)

val of_strings : string -> int -> string -> string -> t
(**[of_strings name rating dining hall comments] creates a rating given the
   name, rating, dining hall and comments provided, of types [string], [int],
   [string], and [string], respectively. Requires: [rating] is an integer
   between 1 and 5. *)

val to_csv_entry : t -> string list
(**[to_csv_entry rating] returns [rating] represented as a CSV entry*)

val get_hall : t -> string
(**[get_hall rating] returns the associated dining hall the food was rated at*)

val retrieve_ratings_name : Csv.t -> string -> Csv.t
(**[retrieve_ratings_name csv_file name] returns the ratings in [csv_file] that
   are associated with the specific food [name], if any. Requires: [csv_file] is
   a legitimate rating file with entries representable by [Rating.t]*)

val retrieve_ratings_int : Csv.t -> int -> Csv.t
(**[retrieve_ratings_name csv_file rating] returns the ratings in [csv_file]
   that are associated with exactly the specific rating [rating], if any.
   Requires: [csv_file] is a legitimate rating file with entries representable
   by [Rating.t]*)

val retrieve_ratings_int_range : Csv.t -> int -> int -> Csv.t
(**[retrieve_ratings_int_range csv_file low high] returns the ratings in
   [csv_file] that have a rating of at least [low] and at most [high]. Requires:
   [low] <= [high], and [csv_file] is a legitimate rating file with entries
   representable by [Rating.t]*)

val retrieve_ratings_hall : Csv.t -> string -> Csv.t
(**[retrieve_ratings_hall csv_file hall] returns the ratings in [csv_file] that
   correspond to the specified [hall], if any. Requires: [csv_file] is a
   legitimate rating file with entries representable by [Rating.t]*)

val sort_name : Csv.t -> bool -> Csv.t
(**[sort_name csv_file asc] returns the csv, sorted by the food names. The
   returned file is sorted in ascending order if [asc] is true, descending
   otherwise. Requires: [csv_file] is a legitimate rating file with entries
   representable by [Rating.t]*)

val sort_hall : Csv.t -> bool -> Csv.t
(**[sort_hall csv_file asc] returns the csv, sorted by the dining hall. The
   returned file is sorted in ascending order if [asc] is true, descending
   otherwise. Requires: [csv_file] is a legitimate rating file with entries
   representable by [Rating.t]*)

val sort_rating : Csv.t -> bool -> Csv.t
(**[sort_rating csv_file asc] returns the csv, sorted by the ratings. The
   returned file is sorted in ascending order if [asc] is true, descending
   otherwise. Requires: [csv_file] is a legitimate rating file with entries
   representable by [Rating.t]*)

val print_ratings : Csv.t -> string -> unit
(**[print_ratings] prints the string out of the CSV*)
