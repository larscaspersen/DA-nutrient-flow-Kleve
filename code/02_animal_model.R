##############
#create simple model of N-flow in district of Kleve
##############

#in this file all the outgoing flows from the animal subsystem are modelled
#ingoing flows still need to be processed
# 
# library(decisionSupport)
# 
# input <- read.csv('data/input-all.csv')
# 
# #function to draw random variables from input and create global variables
# make_variables<-function(est,n=1)
# { x<-random(rho=est, n=n)
# for(i in colnames(x)) assign(i,
#                              as.numeric(x[1,i]),envir=.GlobalEnv)
# }
# 
# make_variables(as.estimate(input),n=1)
# # 
# # 
# # #detailed_input <- read.csv('data/input-table-detailed.csv')
# # #make_median(detailed_input)



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
                        P_content_female_cattle, P_content_male_cattle, P_content_pig,
                        P_content_poultry, P_content_sheep, P_content_horse,
                        K_content_female_cattle, K_content_male_cattle, K_content_pig,
                        K_content_poultry, K_content_sheep, K_content_horse,
                        n_dairy_cow, milk_per_cow, 
                        N_content_milk, P_content_milk, K_content_milk,
                        share_milk_direct_sale, share_milk_other_use, 
                        share_milk_to_factory,
                        n_chicken, egg_per_chicken, `egg-weight`, 
                        N_content_egg, P_content_egg, K_content_egg,
                        n_bull, n_heifer, n_calf,
                        n_pig, n_other_poultry,
                        n_sheep,
                        N_excretion_dairy, N_excretion_bull,
                        N_excretion_heifer, N_exctretion_calf,
                        N_excretion_pig, N_excretion_hen,
                        N_excretion_other_poultry,
                        N_excretion_sheep,
                        manure_N_to_PtwoOfive_dairy,
                        manure_N_to_PtwoOfive_bull,
                        manure_N_to_PtwoOfive_heifer,
                        manure_N_to_PtwoOfive_calf,
                        manure_N_to_PtwoOfive_pig,
                        manure_N_to_PtwoOfive_hen,
                        manure_N_to_PtwoOfive_other_poultry,
                        manure_N_to_PtwoOfive_sheep,
                        manure_N_to_KOtwo,
                        cattle_on_slurry, pig_on_slurry,
                        cattle_housingloss_rate_liquid, cattle_housingloss_rate_solid,
                        pig_housinglosss_rate_liquid, pig_housinglosss_rate_solid,
                        others_housingloss_rate,
                        convert_phosphorous_pentoxide_to_p,
                        convert_potassium_oxide_to_k,
                        P_housing_losses,
                        K_housing_losses){
  
  #slaughtering -----
  
  #calculatate the rate of slaughtered animals relative to total stock
  #this might be important when we reduce the animal number not homogenously but say that the composition reacts
  #to the change in feed production
  
  slaughter_rate_dairy_cow <- n_slaughter_dairy_cattle / n_dairy_cow
  slaughter_rate_heifer <- n_slaughter_female_cattle / n_heifer
  slaughter_rate_younstock_midage <- n_slaughter_younstock_midage / n_calf
  slaughter_rate_younstock_youngage <-  n_slaughter_younstock_youngage / n_calf
  slaughter_rate_bull <- n_slaughter_bulls / n_bull
  #note: pigs are ~6months old when slaughtered, that is why share is almost 2
  slaughter_rate_pig <- n_slaughter_pig / n_pig
  #chicken usually 6 weeks old when slaughtered, so share should be ~ 8 times higher but I got only value of 4.5
  slaughter_rate_poultry <- n_slaughter_poultry / n_chicken
  slaughter_rate_sheep <- n_slaughter_sheep / n_sheep
  
  #for oxes, goats and horses we dont have information on their stock, so there we reduce the number of slaughtered animals
  #proportionally to the reduction in available feed no matter how the composition changes
  
  
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
  
  #actually P2O5 content
  P_content_slaughter <- c(rep(P_content_female_cattle,2),
                          rep(P_content_male_cattle,2),
                          rep(P_content_female_cattle,2),
                          P_content_pig, 
                          P_content_poultry,
                          rep(P_content_sheep,3),
                          P_content_horse)
  
  #actually KO2 content
  K_content_slaughter <- c(rep(K_content_female_cattle,2),
                           rep(K_content_male_cattle,2),
                           rep(K_content_female_cattle,2),
                           K_content_pig, 
                           K_content_poultry,
                           rep(K_content_sheep,3),
                           K_content_horse)
  
  slaughter_df <- data.frame(animal, n_slaughter, n_import,slaughter_weight,
                             slaughter_weight_fraction, edible_fraction,
                             N_content, P_content_slaughter, K_content_slaughter)
  
  

  #calculate total weight of animal type that goes to food processing:
  #first get weight of individuum when slaughtered: slaughter_weight / slaughter_fraction
  #second multiply by number of individuals (in NRW and imported)
  
  #######
  # meat from ANIMAL to PROCESSING
  ######
  
  #total weight which goes to the slaughter house
  #differentiate local and imported meat and give one for everything
  slaughter_df$total_weight <- (slaughter_df$slaughter_weight / slaughter_df$slaughter_weight_fraction) * (slaughter_df$n_slaughter + slaughter_df$n_import)
  slaughter_df$total_weight_local <- (slaughter_df$slaughter_weight / slaughter_df$slaughter_weight_fraction) * (slaughter_df$n_slaughter)
  slaughter_df$total_weight_import <- (slaughter_df$slaughter_weight / slaughter_df$slaughter_weight_fraction) * (slaughter_df$n_import)
  
  
  #total amount of Nitrogen that goes to the slaughterhouse
  slaughter_df$N_to_slaughter <- slaughter_df$total_weight / 100 * slaughter_df$N_content 
  slaughter_df$N_to_slaughter_local <- slaughter_df$total_weight_local / 100 * slaughter_df$N_content 
  slaughter_df$N_to_slaughter_import <- slaughter_df$total_weight_import / 100 * slaughter_df$N_content 
  
  #total amount of P in going to slaughter house
  #convert_phosphorous_pentoxide_to_p <- 0.4364
  slaughter_df$P_to_slaughter <- (slaughter_df$total_weight / 100 * slaughter_df$P_content_slaughter) * convert_phosphorous_pentoxide_to_p
  slaughter_df$P_to_slaughter_local <- (slaughter_df$total_weight_local / 100 * slaughter_df$P_content_slaughter) * convert_phosphorous_pentoxide_to_p
  slaughter_df$P_to_slaughter_import <- (slaughter_df$total_weight_import / 100 * slaughter_df$P_content_slaughter) * convert_phosphorous_pentoxide_to_p
  
  
  #total amount of K going to slaughter house
  #convert_potassium_oxide_to_k <- 0.8301
  slaughter_df$K_to_slaughter <- (slaughter_df$total_weight / 100 * slaughter_df$K_content_slaughter) * convert_potassium_oxide_to_k
  slaughter_df$K_to_slaughter_local <- (slaughter_df$total_weight_local / 100 * slaughter_df$K_content_slaughter) * convert_potassium_oxide_to_k
  slaughter_df$K_to_slaughter_import <- (slaughter_df$total_weight_import / 100 * slaughter_df$K_content_slaughter) * convert_potassium_oxide_to_k
  
  
  #######
  # meat from PROCESSING to CONSUMPTION
  #######
  
  
  #the following calculations should be acutally in the processing subsystem
  
  #total amount of N that can be consumed by customers
  #important: import does not mean imported meat but locally butchered meat from imported slaughter animals
  slaughter_df$N_meat_consumption <- slaughter_df$total_weight * slaughter_df$edible_fraction / 100 * slaughter_df$N_content
  slaughter_df$N_meat_consumption_local <- slaughter_df$total_weight_local * slaughter_df$edible_fraction / 100 * slaughter_df$N_content
  slaughter_df$N_meat_consumption_import <- slaughter_df$total_weight_import * slaughter_df$edible_fraction / 100 * slaughter_df$N_content
  
  slaughter_df$P_meat_consumption <- (slaughter_df$total_weight * slaughter_df$edible_fraction / 100 * slaughter_df$P_content_slaughter) * convert_phosphorous_pentoxide_to_p
  slaughter_df$P_meat_consumption_local <- (slaughter_df$total_weight_local * slaughter_df$edible_fraction / 100 * slaughter_df$P_content_slaughter) * convert_phosphorous_pentoxide_to_p
  slaughter_df$P_meat_consumption_import <- (slaughter_df$total_weight_import * slaughter_df$edible_fraction / 100 * slaughter_df$P_content_slaughter) * convert_phosphorous_pentoxide_to_p
  
  slaughter_df$K_meat_consumption <- (slaughter_df$total_weight * slaughter_df$edible_fraction / 100 * slaughter_df$K_content_slaughter) * convert_potassium_oxide_to_k
  slaughter_df$K_meat_consumption_local <- (slaughter_df$total_weight_local * slaughter_df$edible_fraction / 100 * slaughter_df$K_content_slaughter) * convert_potassium_oxide_to_k
  slaughter_df$K_meat_consumption_import <- (slaughter_df$total_weight_import * slaughter_df$edible_fraction / 100 * slaughter_df$K_content_slaughter) * convert_potassium_oxide_to_k
  
  
  
  
  
  
  #total amount of meat comming out of the slaughter house
  slaughter_df$total_slaughter <- slaughter_df$slaughter_weight * (slaughter_df$n_slaughter + slaughter_df$n_import)
  slaughter_df$total_slaughter_local <- slaughter_df$slaughter_weight * (slaughter_df$n_slaughter)
  slaughter_df$total_slaughter_import <- slaughter_df$slaughter_weight * (slaughter_df$n_import)
  
  #total amount (kg) of slaughter waste comming out of the slaughter house
  slaughter_df$total_slaughter_waste <- slaughter_df$total_weight - (slaughter_df$total_weight * slaughter_df$edible_fraction)
  slaughter_df$total_slaughter_waste_local <- slaughter_df$total_weight_local - (slaughter_df$total_weight_local * slaughter_df$edible_fraction)
  slaughter_df$total_slaughter_waste_import <- slaughter_df$total_weight_import - (slaughter_df$total_weight_import * slaughter_df$edible_fraction)
  
  
  
  ########
  # meat from PROCESSING to WASTE
  ########
  
  #total amount of N in the slaughter waster
  slaughter_df$N_slaughter_waste <- slaughter_df$total_slaughter_waste / 100 * slaughter_df$N_content
  slaughter_df$N_slaughter_waste_local <- slaughter_df$total_slaughter_waste_local / 100 * slaughter_df$N_content
  slaughter_df$N_slaughter_waste_import <- slaughter_df$total_slaughter_waste_import / 100 * slaughter_df$N_content
  
  slaughter_df$P_slaughter_waste <- (slaughter_df$total_slaughter_waste / 100 * slaughter_df$P_content_slaughter) * convert_phosphorous_pentoxide_to_p
  slaughter_df$P_slaughter_waste_local <- (slaughter_df$total_slaughter_waste_local / 100 * slaughter_df$P_content_slaughter) * convert_phosphorous_pentoxide_to_p
  slaughter_df$P_slaughter_waste_import <- (slaughter_df$total_slaughter_waste_import / 100 * slaughter_df$P_content_slaughter) * convert_phosphorous_pentoxide_to_p

  slaughter_df$K_slaughter_waste <- (slaughter_df$total_slaughter_waste / 100 * slaughter_df$K_content_slaughter) * convert_potassium_oxide_to_k
  slaughter_df$K_slaughter_waste_local <- (slaughter_df$total_slaughter_waste_local / 100 * slaughter_df$K_content_slaughter) * convert_potassium_oxide_to_k
  slaughter_df$K_slaughter_waste_import <- (slaughter_df$total_slaughter_waste_import / 100 * slaughter_df$K_content_slaughter) * convert_potassium_oxide_to_k
  
  
  
  ########
  # MILK
  ########
  
  #total amount of N in milk
  N_milk_produced <- n_dairy_cow * milk_per_cow * N_content_milk / 100
  #for P and K content is in mg
  P_milk_produced <- n_dairy_cow * milk_per_cow * P_content_milk / 100000
  K_milk_produced <- n_dairy_cow * milk_per_cow * K_content_milk / 100000 
  
  
  N_milk_available <- N_milk_produced * (share_milk_direct_sale +
                                           share_milk_other_use + share_milk_to_factory)
  P_milk_available <- P_milk_produced * (share_milk_direct_sale +
                                           share_milk_other_use + share_milk_to_factory)
  K_milk_available <- K_milk_produced * (share_milk_direct_sale +
                                           share_milk_other_use + share_milk_to_factory)
  
  
  
  #########
  # EGG
  #########
  
  N_egg_available <- n_chicken * egg_per_chicken * `egg-weight` / 100 * N_content_egg / 1000
  P_egg_available <- n_chicken * egg_per_chicken * `egg-weight` / 100 * N_content_egg / 1000000
  K_egg_available <- n_chicken * egg_per_chicken * `egg-weight` / 100 * N_content_egg / 1000000
  
  
  
  
  ##########
  # MANURE
  #########
  
  
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
                          N_to_PtwoO5 = c(manure_N_to_PtwoOfive_dairy,
                                          manure_N_to_PtwoOfive_bull,
                                          manure_N_to_PtwoOfive_heifer,
                                          manure_N_to_PtwoOfive_calf,
                                          manure_N_to_PtwoOfive_pig,
                                          manure_N_to_PtwoOfive_hen,
                                          manure_N_to_PtwoOfive_other_poultry,
                                          manure_N_to_PtwoOfive_sheep),
                          N_to_KOtwo = manure_N_to_KOtwo,
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
  
  
  #PROBLEM: all the manure calculations are already in kg N, so I need to convert it to
  #         kg manure with a typical N content and then convert it to P 
  
  #NÄHRSTOFFBERICHT: 
  # dairy cow excretes 99.7 - 128.8 kg N per year; 36.8 - 44.7 kg P2O5 per year
  # combine max N with min P and min N with max P to get maximum possible ranges in N:P
  # max N : min P2O5: 1 : 0.2857
  # min N : max P2O5: 1 : 0.4483

  manure_df$P_total_manure <- (manure_df$N_total_manure * manure_df$N_to_PtwoO5) * convert_phosphorous_pentoxide_to_p
  manure_df$K_total_manure <- (manure_df$N_total_manure * manure_df$N_to_KOtwo) * convert_potassium_oxide_to_k
  
  
  
  #housing losses
  manure_df$N_housing_loss <- manure_df$N_total_manure * manure_df$on_slurry * manure_df$housing_loss_rate_liquid +
                                manure_df$N_total_manure * (1- manure_df$on_slurry) * manure_df$housing_loss_rate_solid
  
  manure_df$P_housing_loss <- (manure_df$N_housing_loss * manure_df$N_to_PtwoO5) * convert_phosphorous_pentoxide_to_p * P_housing_losses 
  manure_df$K_housing_loss <- (manure_df$N_housing_loss * manure_df$N_to_KOtwo) * convert_potassium_oxide_to_k * K_housing_losses
  #housing losses only affect N?
  
  
  #remaining manure after housing losses
  manure_df$N_manure_remaining <- manure_df$N_total_manure - manure_df$N_housing_loss
  manure_df$P_manure_remaining <- manure_df$P_total_manure - manure_df$P_housing_loss
  manure_df$K_manure_remaining <- manure_df$K_total_manure - manure_df$K_housing_loss
  
  

  #add conversion factor to K and set to zero
  #set housing loss rate K and P to zero
  #better have values and variables for every nutrient, even if set to zero
  
  return(list(N_to_slaughter = sum(slaughter_df$N_to_slaughter),
              P_to_slaughter = sum(slaughter_df$P_to_slaughter),
              K_to_slaughter = sum(slaughter_df$K_to_slaughter),
              
              N_to_slaughter_import = sum(slaughter_df$N_to_slaughter_import),
              P_to_slaughter_import = sum(slaughter_df$P_to_slaughter_import),
              K_to_slaughter_import = sum(slaughter_df$K_to_slaughter_import),
              
              N_meat_local_to_consumption = sum(slaughter_df$N_meat_consumption),
              P_meat_local_to_consumption = sum(slaughter_df$P_meat_consumption),
              K_meat_local_to_consumption = sum(slaughter_df$K_meat_consumption),
              
              N_meat_local_to_consumption_import = sum(slaughter_df$N_meat_consumption_import),
              P_meat_local_to_consumption_import = sum(slaughter_df$P_meat_consumption_import),
              K_meat_local_to_consumption_import = sum(slaughter_df$K_meat_consumption_import),
              
              N_slaughter_waste = sum(slaughter_df$N_slaughter_waste),
              P_slaughter_waste = sum(slaughter_df$P_slaughter_waste),
              K_slaughter_waste = sum(slaughter_df$K_slaughter_waste),
              
              
              N_slaughter_waste_import = sum(slaughter_df$N_slaughter_waste_import),
              P_slaughter_waste_import = sum(slaughter_df$P_slaughter_waste_import),
              K_slaughter_waste_import = sum(slaughter_df$K_slaughter_waste_import),
              
              N_milk_available = N_milk_available,
              P_milk_available = P_milk_available,
              K_milk_available = K_milk_available,
              
              N_egg_available = N_egg_available,
              P_egg_available = P_egg_available,
              K_egg_available = K_egg_available,
              
              N_total_manure = sum(manure_df$N_total_manure),
              P_total_manure = sum(manure_df$P_total_manure),
              K_total_manure = sum(manure_df$K_total_manure),
              
              N_housing_loss = sum(manure_df$N_housing_loss),
              P_housing_loss = sum(manure_df$P_housing_loss),
              K_housing_loss = sum(manure_df$K_housing_loss),
              
              N_remaining_manure = sum(manure_df$N_manure_remaining),
              P_remaining_manure = sum(manure_df$P_manure_remaining),
              K_remaining_manure = sum(manure_df$K_manure_remaining),
              
              slaughter_rate_dairy_cow = slaughter_rate_dairy_cow,
              slaughter_rate_heifer = slaughter_rate_heifer,
              slaughter_rate_bull = slaughter_rate_bull,
              slaughter_rate_younstock_midage = slaughter_rate_younstock_midage,
              slaughter_rate_younstock_youngage = slaughter_rate_younstock_youngage,
              slaughter_rate_pig = slaughter_rate_pig,
              slaughter_rate_poultry = slaughter_rate_poultry,
              slaughter_rate_sheep = slaughter_rate_sheep))
  
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



