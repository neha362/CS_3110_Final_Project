type t
(**The type representing the dictionary of foods. *)

val transform_to_list : t -> string list
(**[transform_to_list d] is the dictionary [d] as a list of foods. *)

val check_spelling : string -> t -> bool
(**[check_spelling w d] checks to see if the word [w] is in the dictionary [d].
   Raises: Word_Not_Found if the word cannot be found in the dictionary.
*)

val remove_dup : t -> string list
(**[remove_dup d] is [d] without duplicate words. *)

val corrected_word : string -> t -> string
(**[corrected_word w d] is [w] with corrected spelling based on the dictionary
   [d]. *)

val create_dictionary : string -> t
(**[create_dictionary p] creates a dictionary from the file located at the path
   [p]. *)

exception Word_Not_Found of string
(** Raised when a word cannot be found in the dictionary*)
