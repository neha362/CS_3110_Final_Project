type t = {
  name : string;
  rating : int;
  hall : string;
  comments : string;
}

(*AF: type [t] represents a rating by the user. The record {name;rating;hall;comments} corresponds to the user rating with the food name = name, the rating = rating, the dining hall where the food was sampled = hall, and any additional comments (perhaps an empty string) = comments. *)
(*RI: rating is a integer value between 1 and 5. Name and hall are non-empty strings, and comments can be an empty string. *)
let get_name r = r.name
let get_hall r = r.hall
let get_rating r = r.rating
let get_comments r = r.comments
let of_strings name rating hall comments = { name; rating; hall; comments }
let to_csv_entry r = [ r.name; string_of_int r.rating; r.hall; r.comments ]

let retrieve_ratings_name csv_file name =
  List.filter (fun lst -> List.nth lst 0 = name) csv_file

let retrieve_ratings_int csv_file rating =
  List.filter (fun lst -> List.nth lst 1 = string_of_int rating) csv_file

let retrieve_ratings_int_range csv_file low high =
  List.filter
    (fun lst ->
      let v = int_of_string (List.nth lst 1) in
      v >= low && v <= high)
    csv_file

let retrieve_ratings_hall csv_file hall =
  List.filter (fun lst -> List.nth lst 2 = hall) csv_file

let sort_name csv_file asc =
  List.sort
    (fun lst1 lst2 ->
      let c = compare (List.nth lst1 0) (List.nth lst2 0) in
      if c = 0 then compare lst1 lst2 else c * if asc then 1 else -1)
    csv_file

let sort_hall csv_file asc =
  List.sort
    (fun lst1 lst2 ->
      let c = compare (List.nth lst1 2) (List.nth lst2 2) in
      if c = 0 then compare lst1 lst2 else c * if asc then 1 else -1)
    csv_file

let sort_rating csv_file asc =
  List.sort
    (fun lst1 lst2 ->
      let c = compare (List.nth lst1 1) (List.nth lst2 1) in
      if c = 0 then compare lst1 lst2 else c * if asc then 1 else -1)
    csv_file

(**[print_helper l] is the nicely formatted version of the rating [l]*)
let print_helper line =
  let string_stars n =
    if n = 1 then "☆■■■■\t"
    else if n = 2 then "☆☆■■■\t"
    else if n = 3 then "☆☆☆■■\t"
    else if n = 4 then "☆☆☆☆■\t"
    else "☆☆☆☆☆\t"
  in
  [
    List.nth line 0;
    string_stars (int_of_string (List.nth line 1));
    List.nth line 2;
    List.nth line 3;
  ]

(**[ratings_formatter c a] is the pretty format version of the ratings in [c]
   appended to [a]*)
let rec ratings_formatter csv acc =
  match csv with
  | [] -> acc
  | h :: t -> ratings_formatter t (print_helper h :: acc)

let print_ratings m f =
  print_newline ();
  Csv.print_readable (List.hd (Csv.load f) :: ratings_formatter m []);
  print_newline ()
