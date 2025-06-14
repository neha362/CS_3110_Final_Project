let halls = ref []
let dietary = ref [ "Vegetarian"; "Vegan" ]
let cuisines_list = ref []
let cuisines_file_path = "driver_data/cuisines.csv"

let read_cuisines () =
  cuisines_list := Csv.load cuisines_file_path |> List.map List.hd

let write_cuisines () =
  Csv.save cuisines_file_path
    (!cuisines_list |> List.map (fun cuisine -> [ cuisine ]))

let get_all_filters () = ref (!halls @ !dietary @ [ "Cuisine" ])

let days_of_week =
  [
    (0, "Sunday");
    (1, "Monday");
    (2, "Tuesday");
    (3, "Wednesday");
    (4, "Thursday");
    (5, "Friday");
    (6, "Saturday");
  ]

let meal_types = ref []
let master_file_name = "./driver_data/recipe_book.csv"
let recipe_book = ref Dining.empty
let recipes = ref BatSet.empty

let meats =
  [
    "Beef";
    "Chicken";
    "Tuna";
    "Egg";
    "Pork";
    "Salmon";
    "Lamb";
    "Fish";
    "Bacon";
    "Ham";
    "Turkey";
    "Shrimp";
    "Duck";
    "Cod";
    "Haddock";
  ]
