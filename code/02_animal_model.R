##############
#create simple model of N-flow in district of Kleve
##############

#in this file all the outgoing flows from the animal subsystem are modelled
#ingoing flows still need to be processed

library(decisionSupport)


animal_input <- read.csv('data/input-animal.csv')

#function to draw random variables from input and create global variables
make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

#make_variables(as.estimate(animal_input),n=1)


#detailed_input <- read.csv('data/input-table-detailed.csv')
#make_median(detailed_input)



#function for animal subsystem flows
calc_animal <- function(n_slaughter_dairy_cattle, n_slaughter_female_cattle,
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
                        N_biogas_input, 
                        export_org_fertilizer ){
  
  #slaughtering -----
  
  
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
  
  
  
  #the following calculations should be acutally in the processing subsystem
  
  #total amount of N that can be consumed by customers
  slaughter_df$N_meat_consumption <- slaughter_df$total_weight * slaughter_df$edible_fraction / 100 * slaughter_df$N_content
  
  #total amount of meat comming out of the slaughter house
  slaughter_df$total_slaughter <- slaughter_df$slaughter_weight * (slaughter_df$n_slaughter + slaughter_df$n_import)
  
  #total amount of slaughter waste comming out of the slaughter house
  slaughter_df$total_slaughter_waste <- slaughter_df$total_weight - (slaughter_df$total_weight * slaughter_df$edible_fraction)
  
  #total amount of N in the slaughter waster
  slaughter_df$N_slaughter_waste <- slaughter_df$total_slaughter_waste / 100 * slaughter_df$N_content
  

  
  
  
  #milk production ----
  
  #total amount of N in milk
  N_milk_produced <- n_dairy_cow * milk_per_cow * N_content_milk / 100
  
  N_milk_available <- N_milk_produced * (share_milk_direct_sale +
                                           share_milk_other_use + share_milk_to_factory)
  
  #egg production ----
  N_egg_available <- n_chicken * egg_per_chicken * `egg-weight` / 100 * N_content_egg / 1000
  
  #manure production ----
  #create dataframe
  manure_df <- data.frame(animal = c('dairy_cow','bull','heifer','calf',
                                     'pig','hen','other_poultry',
                                     'sheep'),
                          n_animal = c(n_dairy_cow, n_bull, n_heifer, n_calf,
                                       n_pig, n_chicken, n_other_poultry,
                                       n_sheep),
                          excretion_rate = c(N_excretion_dairy, N_excretion_bull,
                                             N_excretion_heifer, N_exctretion_calf,
                                             N_excretion_pig, N_excretion_hen,
                                             N_excretion_other_poultry,
                                             N_excretion_sheep),
                          on_slurry = c( rep(cattle_on_slurry, 4), pig_on_slurry,
                                         rep(1, 3)),
                          housing_loss_rate_liquid = c( rep(cattle_housingloss_rate_liquid, 4),
                                                        pig_housinglosss_rate_liquid,
                                                        rep(others_housingloss_rate,3)),
                          housing_loss_rate_solid = c( rep(cattle_housingloss_rate_solid, 4),
                                                       pig_housinglosss_rate_solid,
                                                       rep(0, 3)))
  #for other animals the housing loss rate doesn't differentiate between liquid and
  #solid manure, so the share of liquid was set 1 and the housing loss rate for solid was set to 0
  
  #total manure coming from animals
  manure_df$N_total_manure <- manure_df$n_animal * manure_df$excretion_rate
  
  #housing losses
  manure_df$N_housing_loss <- manure_df$N_total_manure * manure_df$on_slurry * manure_df$housing_loss_rate_liquid +
                                manure_df$N_total_manure * (1- manure_df$on_slurry) * manure_df$housing_loss_rate_solid
  
  
  #remaining manure after housing losses
  manure_df$N_manure_remaining <- manure_df$N_total_manure - manure_df$N_housing_loss
  
  

  
  
  return(list(N_to_slaughter = sum(slaughter_df$N_to_slaughter),
              N_meat_local_to_consumption = sum(slaughter_df$N_meat_consumption),
              N_slaughter_waste = sum(slaughter_df$N_slaughter_waste),
              N_milk_available = N_milk_available,
              N_egg_available = N_egg_available,
              N_total_manure = sum(manure_df$N_total_manure),
              N_housing_loss = sum(manure_df$N_housing_loss),
              N_remaining_manure = sum(manure_df$N_manure_remaining)))
  
}

# nitrogen_mc_simulation <- mcSimulation(estimate = as.estimate(animal_input),
#                                        model_function = calc_animal,
#                                        numberOfModelRuns = 10000,
#                                        functionSyntax = "plainNames")
# 
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c("N_to_slaughter"),
#                    method = "smooth_simple_overlay",
#                    old_names = c('N_to_slaughter'),
#                    x_axis_name = 'kg N  / year')
# 
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c("N_meat_local_to_consumption"),
#                    method = "smooth_simple_overlay",
#                    old_names = c('N_meat_local_to_consumption'),
#                    x_axis_name = 'kg N  / year')
# #the meat still needs to be compared with local consumption rates and then is split
# #between consumption and export, same goes with milk and eggs
# 
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c("N_housing_loss"),
#                    method = "smooth_simple_overlay",
#                    old_names = c('N_housing_loss'),
#                    x_axis_name = 'kg N  / year')
# 
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c("N_biogas_input_animal"),
#                    method = "smooth_simple_overlay",
#                    old_names = c('N_biogas_input_animal'),
#                    x_axis_name = 'kg N  / year')
# 
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c("N_manure_to_crop"),
#                    method = "smooth_simple_overlay",
#                    old_names = c('N_manure_to_crop'),
#                    x_axis_name = 'kg N  / year')



