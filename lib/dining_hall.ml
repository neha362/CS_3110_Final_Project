type t = string * Csv.t
(*AF: [Dining_hall.t] is a tuple with the first value containing the name of the
  dining hall and the second value containing the CSV file representing the
  options at that dining hall. *)

(*RI: the first value of the tuple should be a non-empty [string]. The second
  value should be a [string list list] of 1-element [string list] types. Each of
  the singleton elements should be a non-empty [string]. *)

let fetch_csv dining_hall = snd dining_hall
let extract_hall name csv = (name, csv)
