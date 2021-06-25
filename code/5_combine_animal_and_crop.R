#in this file I will try to combine the submodels of different scripts

library(decisionSupport)

#first test for nested function in decisionsupport

animal_input <- read.csv('data/input-animal.csv')

#function to draw random variables from input and create global variables
make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

#some tests regarding nested functions in decision support

# make_variables(as.estimate(animal_input),n=1)
# 
# nested_function_1 <- function(n_eggs){
#   #get N content of all eggs
#   N_eggs <- n_eggs * `egg-weight` / 100 *  N_content_egg / 1000
#   return(list(N_eggs = N_eggs))
# }
# 
# nested_function_2 <- function(){
#   #calculate amount of milk
#   N_milk <- n_dairy_cow * milk_per_cow * N_content_milk
#   return(list(N_milk = N_milk))
# }
# 
# test_function <- function(){
#   n_eggs <- egg_per_chicken * n_chicken
#   egg_output <- nested_function_1(n_eggs)
#   milk_output <- nested_function_2()
#   #combine both outputs
#   combined_output <- c(egg_output, milk_output)
#   return(combined_output)
# }
# 
# nitrogen_mc_simulation <- mcSimulation(estimate = as.estimate(animal_input),
#                                        model_function = test_function,
#                                        numberOfModelRuns = 10000,
#                                        functionSyntax = "plainNames")
# 
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c("N_eggs"),
#                    method = "smooth_simple_overlay",
#                    old_names = c('N_eggs'),
#                    x_axis_name = 'kg N  / year')

#so it turns out that in nested functions, you only need to define newly inside 
#the function created objects, but objects which are listed inside the input table
#don't need to be specified in the function argument




#read function etc from the scripts
source('code/2_animal_model.R')
source('code/3_crop_model.R')

#combine the input files
combined_input <- merge.data.frame(x = animal_input, y = crop_input, 
                                   by.x = c('variable','lower','median','upper',
                                            'distribution'),all.x = TRUE,all.y = TRUE)
#create function for mc-simulation
crop_animal_combined <- function(){
  
  #let the animla function run
  animal_output <- calc_animal(n_slaughter_dairy_cattle, n_slaughter_female_cattle,
                               n_slaughter_bulls, n_slaughter_oxes, n_slaughter_younstock_youngage,
                               n_slaughter_younstock_midage, n_slaughter_pig,
                               n_slaughter_poultry, n_slaughter_lamb,
                               n_slaughter_sheep, n_slaughter_goat, n_slaughter_horse,
                               n_slaughter_import_dairy_cattle, n_slaughter_import_female_cattle,
                               n_slaughter_import_bulls, n_slaughter_import_oxes,
                               n_slaughter_import_younstock_youngage,
                               n_slaughter_import_younstock_midage,
                               n_slaughter_import_pig,n_slaughter_import_poultry,
                               n_slaughter_import_lamb, n_slaughter_import_sheep,
                               n_slaughter_import_goat, n_slaughter_import_horse,
                               slaughter_weight_dairy_cattle, slaughter_weight_female_cattle,
                               slaughter_weight_bulls, slaughter_weight_oxes, 
                               slaughter_weight_younstock_youngage, slaughter_weight_younstock_midage,
                               slaughter_weight_pig, slaughter_weight_poultry,
                               slaughter_weight_lamb, slaughter_weight_sheep,
                               slaughter_weight_goat,slaughter_weight_horse,
                               slaughter_weight_fraction_cattle,
                               slaughter_weight_fraction_pig, slaughter_weight_fraction_poultry,
                               slaughter_weight_fraction_others,
                               edible_fraction_cattle, edible_fraction_pig, edible_fraction_poultry,
                               edible_fraction_other,
                               N_content_female_cattle, N_content_male_cattle, N_content_pig,
                               N_content_poultry, N_content_sheep, N_content_horse,
                               n_dairy_cow, milk_per_cow, N_content_milk,
                               share_milk_direct_sale, share_milk_other_use, 
                               share_milk_to_factory,
                               n_chicken, egg_per_chicken, `egg-weight`, N_content_egg,
                               n_bull, n_heifer, n_calf,
                               n_pig, n_other_poultry,
                               n_sheep,
                               N_excretion_dairy, N_excretion_bull,
                               N_excretion_heifer, N_exctretion_calf,
                               N_excretion_pig, N_excretion_hen,
                               N_excretion_other_poultry,
                               N_excretion_sheep,
                               cattle_on_slurry, pig_on_slurry,
                               cattle_housingloss_rate_liquid, cattle_housingloss_rate_solid,
                               pig_housinglosss_rate_liquid, pig_housinglosss_rate_solid,
                               others_housingloss_rate,
                               N_biogas_input, share_N_biogas_input_animal,
                               export_org_fertilizer)
  
  #let crop function run
  crop_output <- crop_function(share_beans, share_corn, share_fodder_peas, 
                               share_mais_silage, share_oat, 
                               share_oilseed_rape, share_potato, share_rye,
                               share_sugar_beet, share_summer_barley,
                               share_summer_wheat, share_triticale,
                               share_winter_barley, share_winter_wheat,
                               share_crop_land,
                               yield_beans, yield_corn, yield_fodder_peas,
                               yield_mais_silage, yield_oat, yield_oilseed_rape,
                               yield_potato, yield_rye, yield_sugar_beet,
                               yield_summer_barley, yield_summer_wheat,
                               yield_triticale, yield_winter_barley,
                               yield_winter_wheat,
                               N_yield_beans,N_yield_corn,N_yield_fodder_peas,
                               N_yield_mais_silage,N_yield_oat,
                               N_yield_oilseed_rape,N_yield_potato,
                               N_yield_rye,N_yield_sugar_beet,N_yield_summer_barley,
                               N_yield_summer_weat,N_yield_triticale,
                               N_yield_winter_barley,N_yield_winter_weat,
                               N_leftover_beans,N_leftover_corn,
                               N_leftover_foder_peas,N_leftover_mais_silage,
                               N_leftover_oat,N_leftover_oilseed_rape,
                               N_leftover_potato,N_leftover_rye,
                               N_leftover_sugar_beet,N_leftover_summer_barley,
                               N_leftover_summer_wheat,N_leftover_triticale,
                               N_leftover_winter_barley,N_leftover_winter_wheat,
                               straw_share,
                               beans_to_animal,corn_to_animal,fodder_peas_to_animal,
                               mais_silage_to_animal,oat_to_animal, oilseed_rape_to_animal,
                               potato_to_animal, rye_to_animal, sugar_beet_to_animal,
                               summer_barley_to_animal, summer_wheat_to_animal,
                               triticale_to_animal, winter_barley_to_animal,
                               winter_wheat_to_animal,
                               beans_to_consumption, corn_to_consumption,fodder_peas_to_consumption,
                               mais_silage_to_consumption, oat_to_consumption,
                               oilseed_rape_to_consumption, potato_to_ponsumption,
                               rye_to_consumption, sugar_beet_to_sunsumption,
                               summer_barley_to_consumption, summer_wheat_to_consumption,
                               triticale_to_consumption, winter_barley_to_consumption,
                               winter_wheat_to_consumption,
                               mais_silage_to_biogas,
                               area_grassland, share_grazing, N_yield_grazing,
                               N_yield_mowing)
  
  #calculate the amount of feed needed to be imported ----
  #= sum of products produced (egg, milk, meat (lifeweight) minus local feed (gras, fodder crops))
  
  animal_output_produced <- animal_output$N_milk_available + animal_output$N_egg_available +
                              animal_output$N_remaining_manure + animal_output$N_housing_loss +
                              animal_output$N_to_slaughter
  
  animal_local_input <- crop_output$N_straw + crop_output$N_grassland + 
                          crop_output$N_crop_animal_feeding_processed +
                          crop_output$N_crop_human_consumption_unprocessed
  
  feed_import <- animal_output_produced - animal_local_input
  
  
  #get a balance of N for animal subsystem----
  N_animal_in <- feed_import + animal_local_input
  
  N_animal_out <- animal_output_produced
  
  N_animal_balance <- N_animal_in - N_animal_out
  
  
  #combine output lists
  combined_output <- c(animal_output, crop_output, feed_import = feed_import,
                       export_org_fertilizer = export_org_fertilizer,
                       N_animal_in = N_animal_in,
                       N_animal_out = N_animal_out,
                       N_animal_balance = N_animal_balance)
  
  return(combined_output)
}

#let mc simulation run, just to test if everything works out
nitrogen_mc_simulation <- mcSimulation(estimate = as.estimate(combined_input),
                                       model_function = crop_animal_combined,
                                       numberOfModelRuns = 10000,
                                       functionSyntax = "plainNames")

#correct the crop shares in the input table of the mcsimulation object ----

#create object with crop names
crop = c('beans','corn', 'fodder_peas', 'mais_silage', 'oat',
         'oilseed_rape', 'potato', 'rye', 'sugar_beet', 
         'summer_barley', 'summer_wheat', 'triticale', 
         'winter_barley', 'winter_wheat')

#add 'share' to it
crop <- paste('share_', crop, sep = '')

#replace initial shares (in x) with corrected shares (from y)
for(crop_i in crop){
  print(crop_i)
  nitrogen_mc_simulation$x[crop_i] <- nitrogen_mc_simulation$y[crop_i]
}

#plots of flows in the model
plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
                   vars = c("N_to_slaughter",'N_meat_local_to_consumption','N_slaughter_waste'),
                   method = "smooth_simple_overlay",
                   old_names = c('N_to_slaughter','N_meat_local_to_consumption', 'N_slaughter_waste'),
                   x_axis_name = 'kg N  / year')

plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
                   vars = c('N_egg_available'),
                   method = "smooth_simple_overlay",
                   old_names = c('N_egg_available'),
                   x_axis_name = 'kg N  / year')

plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
                   vars = c('N_milk_available'),
                   method = "smooth_simple_overlay",
                   old_names = c('N_milk_available'),
                   x_axis_name = 'kg N  / year')

plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
                   vars = c('N_housing_loss','N_biogas_input_animal','export_org_fertilizer'),
                   method = "smooth_simple_overlay",
                   x_axis_name = 'kg N  / year')

plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
                   vars = c('N_manure_to_crop'),
                   method = "smooth_simple_overlay",
                   x_axis_name = 'kg N  / year')

#plots flow entering the system
plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
                   vars = c("N_straw"),
                   method = "smooth_simple_overlay",
                   x_axis_name = 'kg N  / year')

plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
                   vars = c('N_crop_animal_feeding_unprocessed','N_grassland'),
                   method = "smooth_simple_overlay",
                   x_axis_name = 'kg N  / year')

plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
                   vars = c('N_crop_animal_feeding_processed'),
                   method = "smooth_simple_overlay",
                   x_axis_name = 'kg N  / year')

plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
                   vars = c('feed_import'),
                   method = "smooth_simple_overlay",
                   x_axis_name = 'kg N  / year')

#PLS----

pls_result <- plsr.mcSimulation(object = nitrogen_mc_simulation,
                                resultName = names(nitrogen_mc_simulation$y['N_manure_to_crop']), ncomp = 1)

plot_pls(pls_result, input = combined_input, threshold = 0.8)

