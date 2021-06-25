

#function to calcualte consumption rate of products
calc_consume <- function(){
  consumed_N_beef <- consume_beef * N_content_beef * population
  consumed_N_butter <- consume_butter * N_content_butter * population
  consumed_N_cheese <- consume_cheese * N_content_cheese * population
  consumed_N_citrus_fruits <- consume_citrus_fruits * N_content_citrus_fruits * population
  consumed_N_cocoa <- consume_cocoa * N_content_cocoa * population
  consumed_N_condensed_milk <- consume_condensed_milk * N_content_condensed_milk * population
  consumed_N_cream <- consume_cream * N_content_cream * population
  consumed_N_dried_fruit <- consume_dried_fruit * N_content_dried_fruit * population
  consumed_N_egg <- consume_egg * N_content_egg * population
  consumed_N_fish <- consume_fish * N_content_fish * population
  consumed_N_honey <- consume_honey * N_content_honey * population
  consumed_N_legumes <- consume_legumes * N_content_legumes * population
  consumed_N_margarine <- consume_margarine * N_content_margarine * population
  consumed_N_milk <- consume_milk * N_content_milk * population
  consumed_N_nuts <- consume_nuts * N_content_nuts * population
  consumed_N_offal <- consume_offal * N_contente_offal * population
  consumed_N_other_meat <- consume_other_meat * N_content_other_meat * population
  consumed_N_pork <- consume_pork * N_content_pork * population
  consumed_N_potato <- consume_potato * N_content_potato * population
  consumed_N_potato_starch <- consume_potato_starch * N_content_potato_starch * population
  consumed_N_poultry <- consume_poultry * N_content_poultry * population
  consumed_N_rice <- consume_rice * N_content_rice * population
  consumed_N_rye <- consume_rye * N_content_rye * population
  consumed_N_sheep <- consume_sheep * N_content_sheep * population
  consumed_N_sugar <- consume_sugar * N_content_sugar * population
  consumed_N_tree_fruits <- consume_tree_fruits * N_content_tree_fruits * population
  consumed_N_vegetable_fat <- consume_vegetable_fat * N_content_vegetables * population
  consumed_N_vegetable <- consume_vegetables * N_content_vegetables * population
  consumed_N_wheat <- consume_wheat * N_content_wheat * population
}