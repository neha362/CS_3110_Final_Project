type t
(**[Dining_hall.t] represents one day's menu at a particular dining hall. *)

val fetch_csv : t -> Csv.t
(**[fetch_csv dining_hall] returns the foods available in this dining hall,
   representable as a [Csv.t] type.*)

val extract_hall : string -> Csv.t -> t
(**[extract_hall name csv] returns the dining hall's menu, represented by
   [Dining_hall.t], contained in the csv [csv], where the name of the dining
   hall is [name]. Requires: [csv] is a [Csv.t] type with each entry containing
   only the name of the food option, and [name] is a non-empty string.*)

