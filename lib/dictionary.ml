type t = Csv.t

(*AF: [Dictionary] is a list of a list of strings. This is a dictionary containing
one string per line. An empty dictionary is an [[]]. *)

(*RI: There are no duplicated strings. *)

exception Word_Not_Found of string

let create_dictionary csv = Csv.load csv
let transform_to_list (dictionary : t) = List.flatten dictionary

let check_spelling word dictionary =
  let new_dictionary = transform_to_list dictionary in
  List.mem word new_dictionary

let remove_dup dictionary =
  let new_dictionary = transform_to_list dictionary in
  List.sort_uniq compare new_dictionary

let corrected_word word dictionary =
  if check_spelling word dictionary then word
  else
    let used_dictionary = remove_dup dictionary in
    let list_of_corrections =
      List.filter
        (fun check -> BatString.edit_distance check word = 1)
        used_dictionary
    in
    match list_of_corrections with
    | [] -> raise (Word_Not_Found "Word cannot be found")
    | [ item ] -> item
    | h :: t -> h
