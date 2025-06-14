module Fetch_data : sig
  val fetch : bool -> unit -> unit
  (**[fetch update ()] retrieves the data for each dining hall and each of its
     corresponding meals from Cornell Dining's website. The effect is tht files
     are added/deleted from ./dining_data/, allowing for the driver program to
     pull the necessary data based on user queries. [update] states whether the
     user wants to update the dictionary *)
end
