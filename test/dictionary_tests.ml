open OUnit2
open Final_project

let test_dict = Dictionary.create_dictionary "../data/dictionary.csv"
let test_food_dict = Dictionary.create_dictionary "../data/fooddict.csv"

(**[id x] is x*)
let id x = x

(**[string_of_list l] is the string representation of [l]*)
let string_of_list lst = List.fold_left (fun acc a -> acc ^ a) "" lst

(**[transform_test n d e] is the test with name [n] that checks if the list
   version of the dictionary [d] matches [e]*)
let transform_test name dict exp =
  name >:: fun _ ->
  assert_equal exp
    (string_of_list Dictionary.(transform_to_list (create_dictionary dict)))
    ~printer:Fun.id

(**[check_spelling_test n d w e] is the test with name [n] that the word [w]
   being in the dictionary [d] has truth value [e]*)
let check_spelling_test name dict word exp =
  name >:: fun _ ->
  assert_equal exp Dictionary.(check_spelling word (create_dictionary dict))

(**[remove_dup_test n d e] is the test with name [n] that checks if the
   dictionary [d] without duplicates matches [e]*)
let remove_dup_test name dict exp =
  name >:: fun _ ->
  assert_equal exp
    (string_of_list Dictionary.(remove_dup (create_dictionary dict)))
    ~printer:Fun.id

(**[corrected_word_test n d w e] is the test with name [n] that checks that the
   word [w] is corrected to [e] using the dictionary [d]*)
let corrected_word_test name dict word exp =
  name >:: fun _ ->
  assert_equal exp
    Dictionary.(corrected_word word (create_dictionary dict))
    ~printer:Fun.id

let tests =
  "test suite"
  >::: [
         transform_test "food dict transform test" "../data/fooddict.csv"
           "FruitYogurtBarOatsBlueberryBananaCoconutChiaPuddingSaladVeggiesGreensVegetablesGrainsLegumesDressingQuinoaArugulaCucumberFetaCarrotsPeppersBulgurChickpeaTomatoSpinachStrawberryAlmondBBQBeefOnionSoupChipotleLimeChickenThighCilantroBlackBeansGalloRiceCauliflowerGremolataPastaCheesePizzaPepperoniVeganTikkaMasalaChanaBasmatiEggColeslawFajitaCookiesDessertIceCreamSoftServeCerealsWaffleBarBrownSugarRaisinsBroccoliTofuCornedHashFrenchToastSticksRootVegetableHomefriesBreakfastSmoothiesMuffinsBagelsRollsSalsaSaltPotatoesFetaJambalayaGarlicHerbStewBakedMacaroniBiscuitsFreshMangoChutneyRaitaTamarindCouscousStroganoffTagineNaanDalMakhaniLentilsCurryMarrakeshVegetableBlackBeanRollVeganTandooriLentilsBasmatiEggOatmealFireRoastedPorkSausagePattyScrambledHashBrownShreddedBigBreakfastSmoothiesMiniBagelsTacoTwoFriendsCubanCodOreganataCaliforniaHerbMushroomsPolentaTaglierniRosemaryWhiteStewPotatoesEscalividaSeafoodVeracruzPastaSicilianRisleyGFHummusAppleCornellUnsweetenedLambMushroomMeatloafNachoBarSteamedCaliforniaRoastedMashedNoodleBowlsBowlToppingsPhoPickledTamariJasmineBrownWaffleHotColdAssortedDessertsCornellNewEnglandClamChowderRisleyGFSaladBarSesameHerbBakedTofuFreshFruitSaladTraditionalHummusCoconutChiaSeedPuddingOvernightOatsAppleCornellUnsweetenedPlainYogurtDairyYogurtNashvilleHotChickenSmokedGoudaMacCheeseNachoBarCollardGreensRedPepperOnionsSweetPotatoesCaramelizedSteamedCaliforniaVegetablesSsamBarSsamTofuKoreanRedDragonSauceSsamSauceChiliChickenToppingsJasmineBrownRiceFreshWholeFruitWaffleBarHotColdBeveragesIceCreamAssortedDessertsChickenChipotleSoupRisleyGFSaladBarFreshFruitQuinoaTraditionalHummusSesameHerbBakedTofuOvernightOatsAppleDairyYogurtCoconutChiaSeedPuddingTacosAlPastorPorkPineappleWholeGrainRiceBeansStuffedPeppersJicamaColeslawNachoBarLongGrainMexicanGreenSimplySteamedGreenBeansRoastedGrapeTomatoesSummerSquashChickenTostadaBeanTostadaSweetChiliDippingSauceSesameVinaigretteSeaweedSaladKimchiPokeBarTunaToppingsTofuBowlBrownRiceJasmineWaffleBarWholeFruitHotColdBeveragesDessertsIceCreamChickenChipotleSoupRisleyGFSaladBarFreshFruitQuinoaTraditionalHummusSesameHerbBakedTofuOvernightOatsAppleDairyYogurtCoconutChiaSeedPuddingTacosAlPastorPorkPineappleWholeGrainRiceBeansStuffedPeppersJicamaColeslawNachoBarLongGrainMexicanGreenSimplySteamedGreenBeansRoastedGrapeTomatoesSummerSquashChickenTostadaBeanTostadaSweetChiliDippingSauceSesameVinaigretteSeaweedSaladKimchiPokeBarTunaToppingsTofuBowlBrownRiceJasmineWaffleBarWholeFruitHotColdBeveragesDessertsIceCream";
         transform_test "empty dict transform test" "../data/empty.csv" "";
         check_spelling_test "Apple true in fooddict" "../data/fooddict.csv"
           "Apple" true;
         check_spelling_test "borbyeoin false in fooddict"
           "../data/fooddict.csv" "borbyeoin" false;
         remove_dup_test "Remove duplicates from file with duplicates test"
           "../data/food_dict_with_dups.csv" "AppleBurgerCheeseSaladWater";
         remove_dup_test "Remove duplicates from file without duplicates test"
           "../data/food_dict_no_dups.csv" "AppleBurgerCheeseSaladWater";
         corrected_word_test "Incorrect spelling Applee correct to Apple test"
           "../data/fooddict.csv" "Applee" "Apple";
         (let dictionary =
            Dictionary.create_dictionary "../data/fooddict.csv"
          in
          "Matching the wrong malady" >:: fun _ ->
          assert_raises (Dictionary.Word_Not_Found "Word cannot be found")
            (fun () -> Dictionary.corrected_word "ashdasudhiaus" dictionary));
         corrected_word_test "Correct spelling Apple correct to Apple test"
           "../data/fooddict.csv" "Apple" "Apple";
         corrected_word_test "Correct spelling apple correct to Apple test"
           "../data/fooddict.csv" "apple" "Apple";
         corrected_word_test "Correct spelling Barg correct to Bar test"
           "../data/multiple_corrections_dict.csv" "Barg" "Bar";
       ]

let _ = run_test_tt_main tests
