type p = {
  name : string;
  hall : string;
  vegetarian : bool;
  vegan : bool;
  comment : string;
  drink : string list option;
  main : string list option;
  protein : string list option;
  side : string list option;
  dessert : string list option;
}

(**AF: Type [p] is the type of a plate. A plate has a name stored in p.name and
   a dining hall at which it is found stored in p.hall. If a plate is
   vegetarian, p.vegetarian is true, and if vegan, p.vegan is true. Any comment
   the user may want to store about the plate is stored in p.comment. The plate
   stores the users food choices as Some string list in p.drink, p.main,
   p.protein, p.side, and p.dessert. If the user does not select any food item
   for any of these fields, the corresponding field instead stores None.*)

(**RI: None.*)

exception Invalid_plate

(**[check_attribute d m p s e num] is the truth statement of if all the foods
   [d, m, p, s, e] all have ["Y"] in the [num]th index. If the options match
   with [None], it is considered true.*)
let check_attribute d m p s e num =
  let d' =
    match d with
    | Some d1 -> if List.nth d1 num = "Y" then true else false
    | None -> true
  in
  let m' =
    match m with
    | Some m1 -> if List.nth m1 num = "Y" then true else false
    | None -> true
  in
  let p' =
    match p with
    | Some p1 -> if List.nth p1 num = "Y" then true else false
    | None -> true
  in
  let s' =
    match s with
    | Some s1 -> if List.nth s1 num = "Y" then true else false
    | None -> true
  in
  let e' =
    match e with
    | Some e1 -> if List.nth e1 num = "Y" then true else false
    | None -> true
  in
  d' && m' && p' && s' && e'

let make_plate n h c d m p s e =
  let veg_index =
    !Options.recipe_book |> Dining.to_csv |> List.hd
    |> List.find_index (( = ) "Vegetarian")
    |> Option.get
  in
  let vegan_index =
    !Options.recipe_book |> Dining.to_csv |> List.hd
    |> List.find_index (( = ) "Vegan")
    |> Option.get
  in
  if check_attribute d m p s e vegan_index then
    {
      name = n;
      hall = h;
      vegetarian = true;
      vegan = true;
      comment = c;
      drink = d;
      main = m;
      protein = p;
      side = s;
      dessert = e;
    }
  else if check_attribute d m p s e veg_index then
    {
      name = n;
      hall = h;
      vegetarian = true;
      vegan = false;
      comment = c;
      drink = d;
      main = m;
      protein = p;
      side = s;
      dessert = e;
    }
  else
    {
      name = n;
      hall = h;
      vegetarian = false;
      vegan = false;
      comment = c;
      drink = d;
      main = m;
      protein = p;
      side = s;
      dessert = e;
    }

let is_vegetarian p = p.vegetarian
let is_vegan p = p.vegan
let name p = p.name
let hall p = p.hall
let comment p = p.comment

let drink p =
  match p.drink with
  | Some d -> List.hd d
  | None -> "No drink in this plate."

let main p =
  match p.main with
  | Some m -> List.hd m
  | _ -> "No main dish in this plate."

let protein p =
  match p.protein with
  | Some p' -> List.hd p'
  | _ -> "No protein in this plate."

let side p =
  match p.side with
  | Some s -> List.hd s
  | _ -> "No side dish in this plate."

let dessert p =
  match p.dessert with
  | Some e -> List.hd e
  | _ -> "No dessert in this plate."

let cuisines p =
  let d =
    match p.drink with
    | Some d' -> List.nth d' 1
    | None -> ""
  in
  let m =
    match p.main with
    | Some m' -> List.nth m' 1
    | None -> ""
  in
  let pr =
    match p.protein with
    | Some p' -> List.nth p' 1
    | None -> ""
  in
  let s =
    match p.side with
    | Some s' -> List.nth s' 1
    | None -> ""
  in
  let e =
    match p.dessert with
    | Some e' -> List.nth e' 1
    | None -> ""
  in
  [ d; m; pr; s; e ]
