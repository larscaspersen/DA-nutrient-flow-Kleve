##############
#create simple model of N-flow in district of Kleve
##############

library(decisionSupport)

#read input table
#input <- read.csv('data/input-table.csv')

#x <- input$median
#as.numeric(x[1])
#input$variable


make_median <- function(est){
  x<-est$median
  for(i in 1:length(est$variable)) assign(est$variable[i],
                               as.numeric(x[i]),envir=.GlobalEnv)
}
#make_median(input)

animal_input <- read.csv('data/input-animal.csv')
make_median(animal_input)

#detailed_input <- read.csv('data/input-table-detailed.csv')
#make_median(detailed_input)



#function for animal subsystem flows
calc_animal <- function(){
  
  #prepare slaughter df with all the inputs
  
  animal <- c('dairy_cattle', 'female_cattle', 'bull', 'oxen', 'youngstock_youngage',
              'youngstock_midage', 'pig', 'poultry', 'lamb', 'sheep', 'goat', 
              'horse')
  
  n_slaughter <- c(n_slaughter_dairy_cattle, n_slaughter_female_cattle,
                   n_slaughter_bulls, n_slaughter_oxes, n_slaughter_younstock_youngage,
                   n_slaughter_younstock_midage, n_slaughter_pig,
                   n_slaughter_poultry, n_slaughter_lamb,
                   n_slaughter_sheep, n_slaughter_goat, n_slaughter_horse)
  
  n_import <- c(n_slaughter_import_dairy_cattle, n_slaughter_import_female_cattle,
                n_slaughter_import_bulls, n_slaughter_import_oxes,
                n_slaughter_import_younstock_youngage,
                n_slaughter_import_younstock_midage,
                n_slaughter_import_pig,n_slaughter_import_poultry,
                n_slaughter_import_lamb, n_slaughter_import_sheep,
                n_slaughter_import_goat, n_slaughter_import_horse)
  
  slaughter_weight <- c(slaughter_weight_dairy_cattle, slaughter_weight_female_cattle,
                        slaughter_weight_bulls, slaughter_weight_oxes, 
                        slaughter_weight_younstock_youngage, slaughter_weight_younstock_midage,
                        slaughter_weight_pig, slaughter_weight_poultry,
                        slaughter_weight_lamb, slaughter_weight_sheep,
                        slaughter_weight_goat,slaughter_weight_horse)
  
  slaughter_weight_fraction <- c(rep(slaughter_weight_fraction_cattle, 6),
                                 slaughter_weight_fraction_pig, slaughter_weight_fraction_poultry,
                                 rep(slaughter_weight_fraction_others,4))
  
  edible_fraction <- c(rep(edible_fraction_cattle,6),
                       edible_fraction_pig, edible_fraction_poultry,
                       rep(edible_fraction_other,4))
  
  N_content <- c(rep(N_content_female_cattle,2),
                 rep(N_content_male_cattle,2),
                 rep(N_content_female_cattle,2),
                 N_content_pig, N_content_poultry,
                 rep(N_content_sheep, 3),
                 N_content_horse)
  
  slaughter_df <- data.frame(animal, n_slaughter, n_import,slaughter_weight,
                             slaughter_weight_fraction, edible_fraction,
                             N_content)
  
  #calculate total weight of animal type that goes to food processing:
  #first get weight of individuum when slaughtered: slaughter_weight / slaughter_fraction
  #second multiply by number of individuals (in NRW and imported)
  
  #total weight which goes to the slaughter house
  slaughter_df$total_weight <- (slaughter_df$slaughter_weight / slaughter_df$slaughter_weight_fraction) * (slaughter_df$n_slaughter + slaughter_df$n_import)
  
  #total amount of Nitrogen that goes to the slaughterhouse
  slaughter_df$N_to_slaughter <- slaughter_df$total_weight / 100 * slaughter_df$N_content 
  
  #total amount of N that can be consumed by customers
  slaughter_df$N_meat_consumption <- slaughter_df$total_weight * slaughter_df$edible_fraction / 100 * slaughter_df$N_content
  
  #total amount of meat comming out of the slaughter house
  slaughter_df$total_slaughter <- slaughter_df$slaughter_weight * (slaughter_df$n_slaughter + slaughter_df$n_import)
  
  #total amount of slaughter waste comming out of the slaughter house
  slaughter_df$total_slaughter_waste <- slaughter_df$total_weight - (slaughter_df$total_weight * slaughter_df$edible_fraction)
  
  #total amount of N in the slaughter waster
  slaughter_df$N_slaughter_waste <- slaughter_df$total_slaughter_waste / 100 * slaughter_df$N_content
  
  return(c(sum(slaughter_df$N_to_slaughter),sum(slaughter_df$N_meat_consumption), sum(slaughter_df$N_slaughter_waste)))
  
}




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


#function to draw random variables of e.g. input data set to make a run of the model
make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

#draw random variables from nput table, so it is easier to define the model and try it out on the fly
#make_variables(as.estimate(input),n=1)

#define the model
model_function <- function(){
  
  #bundle in and outgoing flows with same start and end
  consumption_to_outside <- waste_water_direct_charge
  consumption_to_waste <- sewage + OFMSW + OFMSW_in_residual_waste
  
  waste_to_outside <- sewage_sludge_export + gaseous_losses_WWT + fresh_compost_export
  waste_to_crop <- fresh_compost + sewage_sludge + digestate
  waste_to_consumption <- finished_compost_to_hobbygardeners
  
  crop_to_outside <- cultivation_losses + other_organic_fertilizer_export
  crop_to_animal <- feed_crops + grass_based_feed + straw
  crop_to_processing <- food_and_feed_crops + fruits_and_vegetables
  crop_to_waste <- vegetal_biogas_substrate
  
  animal_to_crop <- manure
  animal_to_outside <- manure_export + production_animal_housing
  animal_to_waste <- manure_as_biogas_substrate
  animal_to_processing <- slaughter_animals + local_animal_products_produced
  
  processing_to_animal <- processed_import_feed + feed_from_processed_crops
  processing_to_outside <- meat_export + vegetal_products_export + milk_export + slaughter_waste
  processing_to_consumption <- local_vegetal_products + local_animal_products_consumed + imported_animal_products + imported_vegetal_products
  
  outside_to_waste <- OFMSW_import
  outside_to_crop <- inorganic_fertilizers + organic_fertilizer_import
  outside_to_processing <- net_food_import + net_feed_import
  
  
  #calculate total inflow, outflow and balance of subsystem
  consumption_in <- processing_to_consumption +waste_to_consumption
  consumption_out <- consumption_to_outside + consumption_to_waste
  consumption_balance <- consumption_in - consumption_out
  
  waste_in <- consumption_to_waste + crop_to_waste + animal_to_waste + outside_to_waste
  waste_out <- waste_to_crop + waste_to_outside + waste_to_consumption
  waste_balance <- waste_in - waste_out
  
  crop_in <- waste_to_crop + animal_to_crop + outside_to_crop
  crop_out <- crop_to_waste + crop_to_processing + crop_to_animal + crop_to_outside
  crop_balance <- crop_in - crop_out
  
  animal_in <- crop_to_animal + processing_to_animal
  animal_out <- animal_to_processing + animal_to_waste + animal_to_outside + animal_to_crop
  animal_balance <- animal_in - animal_out
  
  processing_in <- crop_to_processing + animal_to_processing + outside_to_processing
  processing_out <- processing_to_animal + processing_to_consumption + processing_to_outside
  processing_balance <- processing_in - processing_out
  
  total_import <- outside_to_waste + outside_to_processing + outside_to_crop
  total_export <- consumption_to_outside + waste_to_outside + crop_to_outside + animal_to_outside + processing_to_outside
  total_balance <- total_import - total_export
  
  #decide what to return from the monte-carlo simulation
  return(list(consumption_in = consumption_in, consumption_out = consumption_out, consumption_balance = consumption_balance,
              waste_in = waste_in, waste_out = waste_out, waste_balance = waste_balance,
              crop_in = crop_in, crop_out = crop_out, crop_balance = crop_balance,
              animal_in = animal_in, animal_out = animal_out, animal_balance = animal_balance,
              processing_in = processing_in, processing_out = processing_out, processing_balance = processing_balance,
              total_import = total_import, total_export = total_export, total_balance = total_balance))

}



nitrogen_mc_simulation <- mcSimulation(estimate = as.estimate(input),
                                       model_function = model_function,
                                       numberOfModelRuns = 10000,
                                       functionSyntax = "plainNames")

plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
                   vars = c("total_import",'total_export'),
                   method = "smooth_simple_overlay",
                   old_names = c("total_import",'total_export'),
                   new_names = c("Total Nitrogen Import to Kleve","Total Nitrogen Export to Kleve"),
                   x_axis_name = 't N  / year')

plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
                   vars = c("total_balance"),
                   method = "boxplot_density",
                   old_names = c('total_balance'),
                   new_names = c("Total Nitrogen Balance of Kleve"),
                   x_axis_name = 't N  / year')

plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
                   vars = c("consumption_balance"),
                   method = "boxplot_density",
                   old_names = c('consumption_balance'),
                   new_names = c("Nitrogen Balance of Consumption"),
                   x_axis_name = 't N  / year')
plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
                   vars = c('animal_balance',"crop_balance",'processing_balance'),
                   method = "smooth_simple_overlay",
                   old_names = c('animal_balance','crop_balance','processing_balance'),
                   new_names = c("2_Nitrogen Balance of Animal Production",
                                 "1_Nitrogen Balance of Crop Production",
                                 "3_Nitrogen Balance of Food and Feed Processing"),
                   x_axis_name = 't N  / year')
#food and feed processing the widest, then comes animal, then comes crop production
#waste and consumption pretty certain



#investiagte which of the three parameters had the biggest impact on the final_profits
pls_result <- plsr.mcSimulation(object = nitrogen_mc_simulation,
                                resultName = names(nitrogen_mc_simulation$y['total_balance']), ncomp = 1)

plot_pls(pls_result, input = input, threshold = 0.8)

#here we subset the outputs from the mcSimulation function (y) by selecting the correct variables
# this should be done by the user (be sure to run the multi_EVPI only on the variables that the user wants)
#caution, this step takes forever (~30min at leas)
mcSimulation_table <- data.frame(nitrogen_mc_simulation$x, nitrogen_mc_simulation$y[c('animal_balance')]
)

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "animal_balance")
plot_evpi(evpi, decision_vars = "animal_balance")


