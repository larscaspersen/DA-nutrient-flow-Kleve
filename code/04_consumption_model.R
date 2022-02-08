# library(decisionSupport)
# 
#read input table
consumption_input <- read.csv('data/input-all.csv')

#function to draw random variables from input and create global variables
make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

#create variable so I can test the function
make_variables(as.estimate(consumption_input),n=1)

#function to calcualte consumption rate of products
calc_consume <- function(population, 
                         consume_beef, N_content_beef,
                         consume_butter, N_content_butter,
                         consume_citrus_fruits, N_content_citrus_fruits,
                         consume_cocoa, N_content_cocoa,
                         consume_condensed_milk, N_content_condensed_milk,
                         consume_cream, N_content_cream,
                         consume_dried_fruit, N_content_dried_fruit,
                         consume_egg, N_content_egg,
                         consume_fish, N_content_fish,
                         consume_honey, N_content_honey,
                         consume_legumes, N_content_legumes,
                         consume_margarine, N_content_margarine,
                         consume_milk, N_content_milk,
                         consume_nuts, N_content_nuts,
                         consume_offal, N_contente_offal,
                         consume_other_meat, N_content_other_meat,
                         consume_pork, N_content_pork,
                         consume_potato, N_content_potato,
                         consume_potato_starch, N_content_potato_starch,
                         consume_poultry, N_content_poultry_meat,
                         consume_rice, N_content_rice,
                         consume_rye, N_content_rye,
                         consume_sheep, N_content_sheep_meat,
                         consume_sugar, N_content_sugar,
                         consume_tree_fruits, N_content_tree_fruits,
                         consume_vegetables, N_content_vegetables,
                         consume_wheat, N_content_wheat,
                         consume_coffee, N_content_coffee, convert_coffee,
                         consume_black_tea, N_content_black_tea, convert_black_tea,
                         consume_herb_tea, N_content_herb_tea, convert_herb_tea,
                         consume_sparkling_wine, N_content_sparkling_wine
                         ){
  
  #create food-consumption df
  
  consumption_df <- data.frame(product = c('beef','beer', 'butter','cheese','citrus_fruit',
                                           'cocoa', 'condensed_milk','cream',
                                           'dried_fruit','egg','fish','honey',
                                           'legumes','margarine','milk',
                                           'nuts','offal','other_meat',
                                           'pork','potato','potato_starch',
                                           'poultry','rice','rye','sheep',
                                           'sugar','tree_fruit',
                                           'vegetable','wheat',
                                           'coffee','black_tea','herb_tea',
                                           'sparkling_wine'),
                               consumption_per_capita = c(consume_beef, consume_beer,
                                                          consume_butter,
                                                          consume_cheese, consume_citrus_fruits,
                                                          consume_cocoa, consume_condensed_milk,
                                                          consume_cream, consume_dried_fruit,
                                                          consume_egg, consume_fish,
                                                          consume_honey, consume_legumes,
                                                          consume_margarine, consume_milk, 
                                                          consume_nuts, consume_offal,
                                                          consume_other_meat, consume_pork,
                                                          consume_potato, consume_potato_starch,
                                                          consume_poultry, consume_rice,
                                                          consume_rye, consume_sheep,
                                                          consume_sugar, consume_tree_fruits,
                                                          consume_vegetables,
                                                          consume_wheat,
                                                          consume_coffee * convert_coffee / 1000,
                                                          consume_black_tea * convert_black_tea / 1000,
                                                          consume_herb_tea * convert_herb_tea / 1000,
                                                          consume_sparkling_wine),
                               N_content = c(N_content_beef, N_content_beer,
                                             N_content_butter,
                                             N_content_cheese, N_content_citrus_fruits,
                                             N_content_cocoa, N_content_condensed_milk,
                                             N_content_cream, N_content_dried_fruit,
                                             N_content_egg, N_content_fish,
                                             N_content_honey, N_content_legumes,
                                             N_content_margarine, N_content_milk,
                                             N_content_nuts, N_content_offal,
                                             N_content_other_meat, N_content_pork,
                                             N_content_potato, N_content_potato_starch,
                                             N_content_poultry_meat, N_content_rice,
                                             N_content_rye, N_content_sheep_meat,
                                             N_content_sugar, N_content_tree_fruits,
                                             N_content_vegetables,
                                             N_content_wheat,
                                             N_content_coffee, N_content_black_tea,
                                             N_content_herb_tea, N_content_sparkling_wine),
                               P_content = c(P_content_beef, P_content_beer,
                                             P_content_butter,
                                             P_content_cheese, P_content_citrus_fruits,
                                             P_content_cocoa, P_content_condensed_milk,
                                             P_content_cream, P_content_dried_fruit,
                                             P_content_egg, P_content_fish,
                                             P_content_honey, P_content_legumes,
                                             P_content_margarine, P_content_milk,
                                             P_content_nuts, P_content_offal,
                                             P_content_other_meat, P_content_pork,
                                             P_content_potato, P_content_potato_starch,
                                             P_content_poultry_meat, P_content_rice,
                                             P_content_rye, P_content_sheep_meat,
                                             P_content_sugar, P_content_tree_fruits,
                                             P_content_vegetables,
                                             P_content_wheat,
                                             P_content_coffee, P_content_black_tea,
                                             P_content_herb_tea, P_content_sparkling_wine),
                               K_content = c(K_content_beef, K_content_beer,
                                             K_content_butter,
                                             K_content_cheese, K_content_citrus_fruits,
                                             K_content_cocoa, K_content_condensed_milk,
                                             K_content_cream, K_content_dried_fruit,
                                             K_content_egg, K_content_fish,
                                             K_content_honey, K_content_legumes,
                                             K_content_margarine, K_content_milk,
                                             K_content_nuts, K_content_offal,
                                             K_content_other_meat, K_content_pork,
                                             K_content_potato, K_content_potato_starch,
                                             K_content_poultry_meat, K_content_rice,
                                             K_content_rye, K_content_sheep_meat,
                                             K_content_sugar, K_content_tree_fruits,
                                             K_content_vegetables,
                                             K_content_wheat,
                                             K_content_coffee, K_content_black_tea,
                                             K_content_herb_tea, K_content_sparkling_wine))
  
  #get the total amount of N conusmed by inhabitants of kleve per product
  consumption_df$N_product_consumed <- consumption_df$consumption_per_capita * population * consumption_df$N_content * 10 / 1000
  consumption_df$P_product_consumed <- consumption_df$consumption_per_capita * population * consumption_df$P_content * 10 / 1000
  consumption_df$K_product_consumed <- consumption_df$consumption_per_capita * population * consumption_df$K_content * 10 / 1000
  
  #sum up the products
  total_N_consumed <- sum(consumption_df$N_product_consumed)
  total_P_consumed <- sum(consumption_df$P_product_consumed)
  total_K_consumed <- sum(consumption_df$K_product_consumed)
  
  
  
  #get sums for different product types
  
  #sum consumption meat
  meat_products <- c('beef', 'offal', 'other_meat', 'pork', 'poultry', 'sheep')
  consumed_N_meat <- sum(consumption_df$N_product_consumed[consumption_df$product %in% meat_products])
  consumed_P_meat <- sum(consumption_df$P_product_consumed[consumption_df$product %in% meat_products])
  consumed_K_meat <- sum(consumption_df$K_product_consumed[consumption_df$product %in% meat_products])
  
  #fish (must imported no matter what)
  fish_product <- c('fish')
  consumed_N_fish <- sum(consumption_df$N_product_consumed[consumption_df$product %in% fish_product])
  consumed_P_fish <- sum(consumption_df$P_product_consumed[consumption_df$product %in% fish_product])
  consumed_K_fish <- sum(consumption_df$K_product_consumed[consumption_df$product %in% fish_product])
  
  #dairy products
  dairy_products <- c('milk', 'cream', 'condensed_milk', 'cheese', 'butter')
  consumed_N_dairy <- sum(consumption_df$N_product_consumed[consumption_df$product %in% dairy_products])
  consumed_P_dairy <- sum(consumption_df$P_product_consumed[consumption_df$product %in% dairy_products])
  consumed_K_dairy <- sum(consumption_df$K_product_consumed[consumption_df$product %in% dairy_products])
  
  #egg
  egg_product <- c('egg')
  consumed_N_egg <- sum(consumption_df$N_product_consumed[consumption_df$product %in% egg_product])
  consumed_P_egg <- sum(consumption_df$P_product_consumed[consumption_df$product %in% egg_product])
  consumed_K_egg <- sum(consumption_df$K_product_consumed[consumption_df$product %in% egg_product])
  
  #vegetables (which can be grown here)
  vegetable_product <- c('wheat','rye', 'legumes','potato','potato_starch',
                         'sugar', 'honey', 'tree_fruit', 'dried_fruit',
                         'vegetable', 'margarine', 'beer', 'sparkling_wine')
  consumed_N_vegetable <- sum(consumption_df$N_product_consumed[consumption_df$product %in% vegetable_product])
  consumed_P_vegetable <- sum(consumption_df$P_product_consumed[consumption_df$product %in% vegetable_product])
  consumed_K_vegetable <- sum(consumption_df$K_product_consumed[consumption_df$product %in% vegetable_product])
  
  #'exotic vegetables (need to be imported for sure'
  foreign_vegetable_product <- c('rice','cocoa','citrus_fruit','nuts', 'coffee',
                                 'black_tea','herb_tea')
  consumed_N_foreign_vegetable <- sum(consumption_df$N_product_consumed[consumption_df$product %in% foreign_vegetable_product])
  consumed_P_foreign_vegetable <- sum(consumption_df$P_product_consumed[consumption_df$product %in% foreign_vegetable_product])
  consumed_K_foreign_vegetable <- sum(consumption_df$K_product_consumed[consumption_df$product %in% foreign_vegetable_product])
  
  #control
  #round((consumed_N_dairy + consumed_N_egg + consumed_N_fish + 
  #    consumed_N_foreign_vegetable + consumed_N_meat + consumed_N_vegetable),digits = 1) == round(total_N_consumed, digits =1 )

  
  return(list(consumed_N_dairy = consumed_N_dairy, 
              consumed_P_dairy = consumed_P_dairy,
              consumed_K_dairy = consumed_K_dairy,
              consumed_N_egg = consumed_N_egg,
              consumed_P_egg = consumed_P_egg,
              consumed_K_egg = consumed_K_egg,
              consumed_N_fish = consumed_N_fish, 
              consumed_P_fish = consumed_P_fish,
              consumed_K_fish = consumed_K_fish,
              consumed_N_foreign_vegetable = consumed_N_foreign_vegetable,
              consumed_P_foreign_vegetable = consumed_P_foreign_vegetable,
              consumed_K_foreign_vegetable = consumed_K_foreign_vegetable,
              consumed_N_meat = consumed_N_meat, 
              consumed_P_meat = consumed_P_meat, 
              consumed_K_meat = consumed_K_meat, 
              consumed_N_vegetable = consumed_N_vegetable,
              consumed_P_vegetable = consumed_P_vegetable,
              consumed_K_vegetable = consumed_K_vegetable))  
}
# 
# nitrogen_mc_simulation <- mcSimulation(estimate = as.estimate(combined_input),
#                                        model_function = calc_consume,
#                                        numberOfModelRuns = 10000,
#                                        functionSyntax = "plainNames")