type p
(**[p] is the type of a plate. Plates can consist of a drink, a main dish, a
   protein, a side dish, and a dessert. Plates should be labeled with dietary
   restrictions vegan and vegetarian. They should also have a name and a dining
   hall at which all of its constituents can be found.*)

exception Invalid_plate

val make_plate :
  string ->
  string ->
  string ->
  string list option ->
  string list option ->
  string list option ->
  string list option ->
  string list option ->
  p
(**[make_plate n h c d m p s e] is the plate with name [n] made from the optional
   drink [d], optional main dish [m], optional protein [p], optional side dish
   [s], and optional dessert [e] which can be found at the dining hall [h]. [c] is any comment the user has left about the plate.
   @requires [d, m, p, s, e] are all valid representations of a dish according
   to the Dining compilation unit.*)

val is_vegetarian : p -> bool
(**[is_vegetarian p] is if the plate is vegetarian*)

val is_vegan : p -> bool
(**[is_vegan p] is if the plate is vegan*)

val name : p -> string
(**[name p] is the name of the plate [p].*)

val hall : p -> string
(**[hall p] is the dining hall at which [p] can be found.*)

val comment : p -> string
(**[comment p] is the comment associated with the plate [p].*)

val drink : p -> string
(**[drink p] is the drink in the plate [p]*)

val main : p -> string
(**[main p] is the main dish stored in the plate [p] if any.*)

val protein : p -> string
(**[protein p] is the protein stored in the plate [p] if any.*)

val side : p -> string
(**[side p] is the side dish stored in the plate [p] if any.*)

val dessert : p -> string
(**[dessert p] is the dessert stored in the plate [p] if any.*)

val cuisines : p -> string list
(**[cuisines p] is the list of cuisines of the dishes in [p].*)
