#in this file I will try to combine the submodels of different scripts

library(decisionSupport)


#function to draw random variables from input and create global variables
make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

#read function etc from the scripts
source('code/02_animal_model.R')
source('code/03_crop_model.R')
source('code/04_consumption_model.R')
source('code/05_waste_submodel.R')

input <- read.csv('data/input-all.csv')

#combine the input files
#combined_input <- rbind.data.frame(crop_input, animal_input, consumption_input, waste_input)

#find and removed duplicates from input table (same inputs listed in two different input tables)
#combined_input[duplicated(combined_input$variable),]
#combined_input <- combined_input[!duplicated(input$variable),]

make_variables(as.estimate(input))


#create function for mc-simulation
combined_function <- function(){
  
  #container for the model evaluation criteria
  model_evaluation <- list()
  
  
  #loop for the scenarios
  for(scenario in c('normal', 'local_feed', 'technology')){
    
    #this scenario is not worked out yet, so skip it in the loop
    if(scenario == 'technology'){
      next()
    }
    
    
    #at first calculate the crop production system
    
    ###############
    #CROP SUBSYSTEM
    ###############
    
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
                                 P_yield_beans,P_yield_corn,P_yield_fodder_peas,
                                 P_yield_mais_silage,P_yield_oat,
                                 P_yield_oilseed_rape,P_yield_potato,
                                 P_yield_rye,P_yield_sugar_beet,P_yield_summer_barley,
                                 P_yield_summer_weat,P_yield_triticale,
                                 P_yield_winter_barley,P_yield_winter_weat,
                                 P_leftover_beans,P_leftover_corn,
                                 P_leftover_foder_peas,P_leftover_mais_silage,
                                 P_leftover_oat,P_leftover_oilseed_rape,
                                 P_leftover_potato,P_leftover_rye,
                                 P_leftover_sugar_beet,P_leftover_summer_barley,
                                 P_leftover_summer_wheat,P_leftover_triticale,
                                 P_leftover_winter_barley,P_leftover_winter_wheat,
                                 K_yield_beans,K_yield_corn,K_yield_fodder_peas,
                                 K_yield_mais_silage,K_yield_oat,
                                 K_yield_oilseed_rape,K_yield_potato,
                                 K_yield_rye,K_yield_sugar_beet,K_yield_summer_barley,
                                 K_yield_summer_weat,K_yield_triticale,
                                 K_yield_winter_barley,K_yield_winter_weat,
                                 K_leftover_beans,K_leftover_corn,
                                 K_leftover_foder_peas,K_leftover_mais_silage,
                                 K_leftover_oat,K_leftover_oilseed_rape,
                                 K_leftover_potato,K_leftover_rye,
                                 K_leftover_sugar_beet,K_leftover_summer_barley,
                                 K_leftover_summer_wheat,K_leftover_triticale,
                                 K_leftover_winter_barley,K_leftover_winter_wheat,
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
                                 area_grassland, share_grazing,
                                 N_yield_grazing,
                                 N_yield_mowing, P_yield_mowing, K_yield_mowing,
                                 land_apple_ha, land_arugula_ha,
                                 land_asparagus_ha, land_berries_ha,
                                 land_cabbage_ha, land_carrot_ha,
                                 land_celery_ha,
                                 land_green_bean_ha, land_lambs_lettuce_ha,
                                 land_lettuce_ha, land_onion_ha,
                                 land_parsley_ha,land_pumpkin_ha,
                                 land_radishes_ha, land_rhubarb_ha,
                                 land_spinash_ha, land_stone_fruit_ha,
                                 land_strawberry_ha, land_sweet_corn_ha,
                                 land_veggie_peas_ha,
                                 yield_apple_dt_ha, yield_arugula_dt_ha,
                                 yield_asparagus_dt_ha, yield_berries_dt_ha,
                                 yield_cabbage_dt_ha, yield_carrot_dt_ha,
                                 yield_celery_dt_ha, yield_green_bean_dt_ha,
                                 yield_lambs_lettuce_dt_ha, yield_lettuce_dt_ha,
                                 yield_onion_dt_ha, yield_parsley_dt_ha,
                                 yield_pumpkin_dt_ha, yield_radishes_dt_ha,
                                 yield_rhubarb_dt_ha, yield_spinash_dt_ha,
                                 yield_stone_fruit_dt_ha, yield_strawberry_dt_ha,
                                 yield_sweet_corn_dt_ha, yield_veggie_peas_dt_ha,
                                 N_content_apple_gr_100gr,
                                 N_content_arugula_gr_100gr,
                                 N_content_asparagus_gr_100gr,
                                 N_content_berries_gr_100gr,
                                 N_content_cabbage_gr_100gr,
                                 N_content_carrot_gr_100gr,
                                 N_content_celery_gr_100gr,
                                 N_content_green_bean_gr_100gr,
                                 N_content_lambs_lettuce_gr_100gr,
                                 N_content_lettuce_gr_100gr,
                                 N_content_onion_gr_100gr,
                                 N_content_parsley_gr_100gr,
                                 N_content_pumpkin_gr_100gr,
                                 N_content_radishes_gr_100gr,
                                 N_content_rhubarb_gr_100gr,
                                 N_content_spinash_gr_100gr,
                                 N_content_stone_fruit_gr_100gr,
                                 N_content_strawberry_gr_100gr,
                                 N_content_sweet_corn_gr_100gr,
                                 N_content_veggie_peas_gr_100gr,
                                 P_content_apple_gr_100gr,
                                 P_content_arugula_gr_100gr,
                                 P_content_asparagus_gr_100gr,
                                 P_content_berries_gr_100gr,
                                 P_content_cabbage_gr_100gr,
                                 P_content_carrot_gr_100gr,
                                 P_content_celery_gr_100gr,
                                 P_content_green_bean_gr_100gr,
                                 P_content_lambs_lettuce_gr_100gr,
                                 P_content_lettuce_gr_100gr,
                                 P_content_onion_gr_100gr,
                                 P_content_parsley_gr_100gr,
                                 P_content_pumpkin_gr_100gr,
                                 P_content_radishes_gr_100gr,
                                 P_content_rhubarb_gr_100gr,
                                 P_content_spinash_gr_100gr,
                                 P_content_stone_fruit_gr_100gr,
                                 P_content_strawberry_gr_100gr,
                                 P_content_sweet_corn_gr_100gr,
                                 P_content_veggie_peas_gr_100gr,
                                 K_content_apple_gr_100gr,
                                 K_content_arugula_gr_100gr,
                                 K_content_asparagus_gr_100gr,
                                 K_content_berries_gr_100gr,
                                 K_content_cabbage_gr_100gr,
                                 K_content_carrot_gr_100gr,
                                 K_content_celery_gr_100gr,
                                 K_content_green_bean_gr_100gr,
                                 K_content_lambs_lettuce_gr_100gr,
                                 K_content_lettuce_gr_100gr,
                                 K_content_onion_gr_100gr,
                                 K_content_parsley_gr_100gr,
                                 K_content_pumpkin_gr_100gr,
                                 K_content_radishes_gr_100gr,
                                 K_content_rhubarb_gr_100gr,
                                 K_content_spinash_gr_100gr,
                                 K_content_stone_fruit_gr_100gr,
                                 K_content_strawberry_gr_100gr,
                                 K_content_sweet_corn_gr_100gr,
                                 K_content_veggie_peas_gr_100gr,
                                 imported_inorganic_N, imported_inorganic_P, imported_inorganic_K)
    
    
    
    #################
    #ANIMAL SUBSYSTEM
    #################
    
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
                                 others_housingloss_rate)
    
    ############
    #FEED IMPORT
    ############
    
    #calculate the amount of feed needed to be imported ----
    #= sum of products produced (egg, milk, meat (lifeweight) minus local feed (gras, fodder crops))
    
    N_animal_output_produced <- animal_output$N_milk_available + animal_output$N_egg_available +
      animal_output$N_remaining_manure + animal_output$N_housing_loss +
      animal_output$N_to_slaughter
    P_animal_output_produced <- animal_output$P_milk_available + animal_output$P_egg_available +
      animal_output$P_remaining_manure + animal_output$P_housing_loss +
      animal_output$P_to_slaughter
    K_animal_output_produced <- animal_output$K_milk_available + animal_output$K_egg_available +
      animal_output$K_remaining_manure + animal_output$K_housing_loss +
      animal_output$K_to_slaughter
    
    N_animal_local_input <- crop_output$N_straw + crop_output$N_grassland + 
      crop_output$N_crop_animal_feeding_processed +
      crop_output$N_crop_animal_feeding_unprocessed
    P_animal_local_input <- crop_output$P_straw + crop_output$P_grassland + 
      crop_output$P_crop_animal_feeding_processed +
      crop_output$P_crop_animal_feeding_unprocessed
    K_animal_local_input <- crop_output$K_straw + crop_output$K_grassland + 
      crop_output$K_crop_animal_feeding_processed +
      crop_output$K_crop_animal_feeding_unprocessed
    
    #in bernous file the unprocessed feed is much higher
    #is there an error in the calculations or are numbers just so different from 2016 to 2020?
    
    N_feed_import <- max(N_animal_output_produced - N_animal_local_input,0)
    P_feed_import <- max(P_animal_output_produced - P_animal_local_input,0)
    K_feed_import <- max(K_animal_output_produced - K_animal_local_input,0)
    

    #####
    #scenario locally source feed exclusively
    #####
    
    if(scenario == 'local_feed'){
      
      #calculate reduction factor if only local feed is used
      rf_local_feed_N <- N_animal_local_input / (N_animal_local_input + N_feed_import)
      rf_local_feed_P <- P_animal_local_input / (P_animal_local_input + P_feed_import)
      rf_local_feed_K <- K_animal_local_input / (K_animal_local_input + K_feed_import)
      
      #take the smallest one because this nutrient would be the limiting factor
      
      rf_local_feed <- min(rf_local_feed_N, rf_local_feed_P, rf_local_feed_K)
      
      #--> reduction by 80%
      
      #set the feed import in that case to zero
      N_feed_import <- 0
      P_feed_import <- 0
      K_feed_import <- 0
      
      #here I could change the individual animal stocks, right now I will reduce them evenly
      n_dairy_cow_reduced <- n_dairy_cow * rf_local_feed
      n_heifer_reduced <- n_heifer * rf_local_feed
      n_bull_reduced <- n_bull * rf_local_feed
      n_calf_reduced <- n_calf * rf_local_feed
      n_pig_reduced <- n_pig * rf_local_feed
      n_chicken_reduced <- n_chicken * rf_local_feed
      n_other_poultry_reduced <- n_other_poultry * rf_local_feed
      n_sheep_reduced <- n_sheep * rf_local_feed
      
      #adjust the number of slaughter animals with the help of the changed stocks and with the caluclated slaughter rate
      
      n_slaughter_dairy_cattle_reduced <- n_dairy_cow_reduced * animal_output$slaughter_rate_dairy_cow
      n_slaughter_female_cattle_reduced <- n_heifer_reduced * animal_output$slaughter_rate_heifer
      n_slaughter_bulls_reduced  <- n_bull_reduced * animal_output$slaughter_rate_bull
      n_slaughter_younstock_midage_reduced <- n_calf_reduced * animal_output$slaughter_rate_younstock_midage
      n_slaughter_younstock_youngage_reduced <- n_calf_reduced * animal_output$slaughter_rate_younstock_youngage
      n_slaughter_pig_reduced <- n_pig_reduced * animal_output$slaughter_rate_pig
      n_slaughter_poultry_reduced <- n_chicken * animal_output$slaughter_rate_poultry
      n_slaughter_sheep_reduced <- n_sheep_reduced * animal_output$slaughter_rate_sheep
      
      #reduce slaughter animals for which we dont have any stock, so there we assume reduction linear to reduction in available feed
      n_slaughter_goat_reduced <- n_slaughter_goat * rf_local_feed
      n_slaughter_horse_reduced <- n_slaughter_horse * rf_local_feed
      n_slaughter_lamb_reduced <- n_slaughter_lamb * rf_local_feed
      n_slaughter_oxes_reduced <- n_slaughter_oxes * rf_local_feed
      
      
      #is the import of slaughter animals set to zero or is it going to be the same?
      #for now I leave it the same
      
      
      #run the animal subsystem again now with the changed animal stocks
      #and with reduced local slaughter animal number
      
      #does the composition of biogas substrate change? 
      #--> assume that the composition is the same, so less manure available for arable farming
      
      
      #######
      #get manure potentially available for crops after biogas
      #according to business as usual calculations
      #######
      
      #manure going to biogas ----
      N_biogas_input_animal <- N_biogas_input * share_N_biogas_input_animal
      P_biogas_input_animal <- P_biogas_input * share_P_biogas_input_animal
      K_biogas_input_animal <- K_biogas_input * share_K_biogas_input_animal
      
      #calculate manure remaining after biogas
      N_manure_after_biogas_old <- animal_output$N_remaining_manure - N_biogas_input_animal
      P_manure_after_biogas_old <- animal_output$P_remaining_manure - P_biogas_input_animal
      K_manure_after_biogas_old <- animal_output$K_remaining_manure - K_biogas_input_animal
      
      
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
                                   others_housingloss_rate)
      
      
      #get new amount of manure after biogas
      N_manure_after_biogas_new <- animal_output$N_remaining_manure - N_biogas_input_animal
      P_manure_after_biogas_new <- animal_output$P_remaining_manure - P_biogas_input_animal
      K_manure_after_biogas_new <- animal_output$K_remaining_manure - K_biogas_input_animal
      
      #get the difference in manure available for crops, this difference needs to be imported in other ways
      #for example by inorganic fertilizer
      
      N_manure_missing <- N_manure_after_biogas_old - N_manure_after_biogas_new
      P_manure_missing <- P_manure_after_biogas_old - P_manure_after_biogas_new
      K_manure_missing <- K_manure_after_biogas_old - K_manure_after_biogas_new
      
      #assume in case of reduced manure there is no export of manure
      export_manure_N_kg <- 0
      export_manure_P_kg <- 0
      export_manure_K_kg <- 0
      
      #import the reduction in manure available to crops in form of inorganic fertilizer  
      crop_output$imported_inorganic_N <- crop_output$imported_inorganic_N + N_manure_missing
      crop_output$imported_inorganic_P <- crop_output$imported_inorganic_P + P_manure_missing
      crop_output$imported_inorganic_K <- crop_output$imported_inorganic_K + K_manure_missing
      
    }
    
    
    ########
    #BIOGAS
    ########
    
    #manure going to biogas ----
    N_biogas_input_animal <- N_biogas_input * share_N_biogas_input_animal
    P_biogas_input_animal <- P_biogas_input * share_P_biogas_input_animal
    K_biogas_input_animal <- K_biogas_input * share_K_biogas_input_animal
    
    
    #additional vegetal biomass going to biogas
    
    #N_crop_biogas contains so far only maize, but there are also other crops like sugar beet used
    #so the gap of animal based input and crop(actually maize based) input is then taken again from N_crop
    N_biogas_crop_missing <- N_biogas_input - (crop_output$N_crop_biogas + N_biogas_input_animal)
    P_biogas_crop_missing <- P_biogas_input - (crop_output$P_crop_biogas + P_biogas_input_animal)
    K_biogas_crop_missing <- K_biogas_input - (crop_output$K_crop_biogas + K_biogas_input_animal)
    
    #55% of additional biomass comes from cover crops, green rye and other crop sources
    #--> take it from the "rest" stream of the crop production, so reduced the stream by that
    crop_output$N_crop_rest <- crop_output$N_crop_rest - (0.55 * N_biogas_crop_missing)
    crop_output$P_crop_rest <- crop_output$P_crop_rest - (0.55 * P_biogas_crop_missing)
    crop_output$K_crop_rest <- crop_output$K_crop_rest - (0.55 * K_biogas_crop_missing)
    
    #20.4% comes from processed human crop food (sugar beet, cereals)
    crop_output$N_crop_human_consumption_processed <- crop_output$N_crop_human_consumption_processed - (0.204 * N_biogas_crop_missing)
    crop_output$P_crop_human_consumption_processed <- crop_output$P_crop_human_consumption_processed - (0.204 * P_biogas_crop_missing)
    crop_output$K_crop_human_consumption_processed <- crop_output$K_crop_human_consumption_processed - (0.204 * K_biogas_crop_missing)
    
    #14.2% comes from grass based sources 
    crop_output$N_grassland <- crop_output$N_grassland - (0.142 * N_biogas_crop_missing)
    crop_output$P_grassland <- crop_output$P_grassland - (0.142 * P_biogas_crop_missing)
    crop_output$K_grassland <- crop_output$K_grassland - (0.142 * K_biogas_crop_missing)
    
    #10.6% comes from processed animal food (cereal silage)
    crop_output$N_crop_animal_feeding_processed <- crop_output$N_crop_animal_feeding_processed - (0.106 * N_biogas_crop_missing)
    crop_output$P_crop_animal_feeding_processed <- crop_output$P_crop_animal_feeding_processed - (0.106 * P_biogas_crop_missing)
    crop_output$K_crop_animal_feeding_processed <- crop_output$K_crop_animal_feeding_processed - (0.106 * K_biogas_crop_missing)
    
    #update the crop biogas iput (because in the crop out it was sof ar only maize)
    crop_output$N_crop_biogas <- N_biogas_input - N_biogas_input_animal
    crop_output$P_crop_biogas <- P_biogas_input - P_biogas_input_animal
    crop_output$K_crop_biogas <- K_biogas_input - K_biogas_input_animal
    
    
    
    
    #manure going to crops after manure was sent to biogas----
    N_manure_to_crop <- animal_output$N_remaining_manure - N_biogas_input_animal - export_manure_N_kg
    P_manure_to_crop <- animal_output$P_remaining_manure - P_biogas_input_animal - export_manure_P_kg
    K_manure_to_crop <- animal_output$K_remaining_manure - K_biogas_input_animal - export_manure_K_kg
    
    
    
    #biogas output
    
    #biomass digestate (take input of animal and crop, process it for waste subsystem)
    lf_area <- arable_land + area_grassland
    
    #get total kwel by biogas (based on the current amount of agricultural land), convestion factor is percentage, thus devide by 100
    total_kwel <- lf_area * lf_to_kwel / 100
    volume_digestate <- total_kwel  * Kwel_to_digestate
    mass_digestate <- volume_digestate * digestate_density / 1000 #in tons. thus devide by 1000
    
    N_digestate <- mass_digestate * digestate_N_content #result is kg
    P_digestate <- mass_digestate * digestate_P_content #result is kg
    K_digestate <- mass_digestate * digestate_K_content #result is kg
    
    
    
    ################
    #WASTE SUBSYSTEM
    ################
    
    waste_output <- waste_function(waste_water,
                                   N_content_wastewater,
                                   P_content_wastewater,
                                   K_content_wastewater,
                                   lossrate_wastewater,
                                   share_sewage_for_agriculture,
                                   compost_to_horticulture,
                                   compost_to_export,
                                   compost_to_consumption,
                                   dm_compost_consumption,
                                   dm_compost_horticulture,
                                   N_content_compost_consumption,
                                   P_content_compost_consumption,
                                   K_content_compost_consumption,
                                   N_content_compost_horticulture,
                                   P_content_compost_horticulture,
                                   K_content_compost_horticulture,
                                   ofmsw_import,
                                   ofmsw_local,
                                   green_waste_import,
                                   green_waste_local,
                                   grey_bin_food_waste,
                                   grey_bin_garden_waste,
                                   dm_green_waste,
                                   dm_ofmsw,
                                   N_content_ofmsw_waste,
                                   P_content_ofmsw_waste,
                                   K_content_ofmsw_waste,
                                   N_content_green_waste,
                                   P_content_green_waste,
                                   K_content_green_waste)
    
    
    
    
    #################
    #FERTILIZER LOSSES
    ################
    
    #calculate fertilization losses as a difference between crop input (inorganic fertilizer, organic fertilizer,
    #digestate, sewage) minus output (vegetal products, feed, other organic fertilizer exported from field)
    
    N_fertilization_losses_blackbox <- (N_digestate + N_manure_to_crop + crop_output$imported_inorganic_N + 
                                        import_organic_N_kg + waste_output$N_sewage_to_crop + 
                                        waste_output$N_compost_crop) - 
      (crop_output$N_crop_human_consumption_processed + 
         crop_output$N_crop_human_consumption_unprocessed +
         crop_output$N_crop_animal_feeding_processed + 
         crop_output$N_crop_animal_feeding_unprocessed + 
         crop_output$N_crop_biogas + 
         crop_output$N_grassland + 
         export_other_organic_N_kg)
    
    P_fertilization_losses_blackbox <- (P_digestate + P_manure_to_crop + crop_output$imported_inorganic_P +
                                          import_organic_P_kg + waste_output$P_sewage_to_crop +
                                          waste_output$P_compost_crop) -
        (crop_output$P_crop_human_consumption_processed +
         crop_output$P_crop_human_consumption_unprocessed +
         crop_output$P_crop_animal_feeding_processed +
         crop_output$P_crop_animal_feeding_unprocessed +
         crop_output$P_crop_biogas +
         crop_output$P_grassland +
         export_other_organic_P_kg)
    
    K_fertilization_losses_blackbox <- (K_digestate + K_manure_to_crop + crop_output$imported_inorganic_K +
                                          import_organic_K_kg + waste_output$K_sewage_to_crop +
                                          waste_output$K_compost_crop) -
      (crop_output$K_crop_human_consumption_processed +
         crop_output$K_crop_human_consumption_unprocessed +
         crop_output$K_crop_animal_feeding_processed +
         crop_output$K_crop_animal_feeding_unprocessed +
         crop_output$K_crop_biogas +
         crop_output$K_grassland +
         export_other_organic_K_kg)
    
    #flag to decide which fertilization losses calculation is used
    #use input_output fertilization losses
    use_input_output_fertilization_losses <- TRUE
    
    if(use_input_output_fertilization_losses){
      crop_output$inevitable_N_losses <- N_fertilization_losses_blackbox
      crop_output$inevitable_P_losses <- P_fertilization_losses_blackbox
      crop_output$inevitable_K_losses <- K_fertilization_losses_blackbox
    }
    
    
    
    
    

    #######################
    #CONSUMPTION SUBSYSTEM
    #######################
    
    consumption_output <- calc_consume(population, 
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
                                       consume_sparkling_wine, N_content_sparkling_wine,
                                       P_content_beef, P_content_beer,
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
                                       P_content_herb_tea, P_content_sparkling_wine,
                                       K_content_beef, K_content_beer,
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
                                       K_content_herb_tea, K_content_sparkling_wine)
    

    
    
    ########
    #WASTE WATER
    ########
    
    
    #wastewater from consumption that doesnt reach the waste subsystem (remains in canal or direct discharge)
    #needs to be calculated here, because not part of the waste subsystem, but variable defined already in waste input file
    N_wastewater_direct_discharge <- (wastewater_direct_discharge + wastewater_remain_canal) * 1000 * N_content_wastewater / 1000000
    P_wastewater_direct_discharge <- (wastewater_direct_discharge + wastewater_remain_canal) * 1000 * P_content_wastewater / 1000000
    K_wastewater_direct_discharge <- (wastewater_direct_discharge + wastewater_remain_canal) * 1000 * K_content_wastewater / 1000000
    
    
    
    
    #######
    #IMPORT / EXPORT for consumption
    #######
    
    
    #calculate degree of self-sufficiency for meat, egg and milk production
    # animal_output$N_egg_available / consumption_output$consumed_N_egg
    # animal_output$N_milk_available / consumption_output$consumed_N_dairy
    # animal_output$N_meat_local_to_consumption / consumption_output$consumed_N_meat
    #production of crops still not done, cant calculate the sufficiency ratio yet
    
    #imoport is consumption - local production, prevent that lower as zero
    N_egg_import <- max(consumption_output$consumed_N_egg - animal_output$N_egg_available,0)
    P_egg_import <- max(consumption_output$consumed_P_egg - animal_output$P_egg_available,0)
    K_egg_import <- max(consumption_output$consumed_K_egg - animal_output$K_egg_available,0)
    
    N_meat_import <- max(consumption_output$consumed_N_meat - animal_output$N_meat_local_to_consumption, 0)
    P_meat_import <- max(consumption_output$consumed_P_meat - animal_output$P_meat_local_to_consumption, 0)
    K_meat_import <- max(consumption_output$consumed_K_meat - animal_output$K_meat_local_to_consumption, 0)
    
    N_dairy_import <- max(consumption_output$consumed_N_dairy - animal_output$N_milk_available, 0)
    P_dairy_import <- max(consumption_output$consumed_P_dairy - animal_output$P_milk_available, 0)
    K_dairy_import <- max(consumption_output$consumed_K_dairy - animal_output$K_milk_available, 0)
    
    N_vegetable_import <- max(consumption_output$consumed_N_vegetable - crop_output$N_crop_human_consumption_processed, 0) + consumption_output$consumed_N_foreign_vegetable
    P_vegetable_import <- max(consumption_output$consumed_P_vegetable - crop_output$P_crop_human_consumption_processed, 0) + consumption_output$consumed_P_foreign_vegetable
    K_vegetable_import <- max(consumption_output$consumed_K_vegetable - crop_output$K_crop_human_consumption_processed, 0) + consumption_output$consumed_K_foreign_vegetable
    
    N_total_food_import <-  N_egg_import + N_dairy_import + N_meat_import + N_vegetable_import
    P_total_food_import <-  P_egg_import + P_dairy_import + P_meat_import + P_vegetable_import
    K_total_food_import <-  K_egg_import + K_dairy_import + K_meat_import + K_vegetable_import
    
    
    
    #food exports (prevent negative exports if consumption exceeds the production)
    N_egg_export <- max(animal_output$N_egg_available - consumption_output$consumed_N_egg, 0)
    P_egg_export <- max(animal_output$P_egg_available - consumption_output$consumed_P_egg, 0)
    K_egg_export <- max(animal_output$K_egg_available - consumption_output$consumed_K_egg, 0)
    
    N_meat_export <- max(animal_output$N_meat_local_to_consumption - consumption_output$consumed_N_meat, 0) 
    P_meat_export <- max(animal_output$P_meat_local_to_consumption - consumption_output$consumed_P_meat, 0)
    K_meat_export <- max(animal_output$K_meat_local_to_consumption - consumption_output$consumed_K_meat, 0)
    
    N_dairy_export <- max(animal_output$N_milk_available - consumption_output$consumed_N_dairy, 0)
    P_dairy_export <- max(animal_output$P_milk_available - consumption_output$consumed_P_dairy, 0)
    K_dairy_export <- max(animal_output$K_milk_available - consumption_output$consumed_K_dairy, 0)
    
    N_vegetable_export <- max(crop_output$N_crop_human_consumption_processed - consumption_output$consumed_N_vegetable,0)
    P_vegetable_export <- max(crop_output$P_crop_human_consumption_processed - consumption_output$consumed_P_vegetable,0)
    K_vegetable_export <- max(crop_output$K_crop_human_consumption_processed - consumption_output$consumed_K_vegetable,0)
    
    #get amount of local products which also get consumed locally
    N_local_animal_products_consumed <- (animal_output$N_egg_available - N_egg_export) +
      (animal_output$N_meat_local_to_consumption - N_meat_export) + 
      (animal_output$N_milk_available - N_dairy_export)
    P_local_animal_products_consumed <- (animal_output$P_egg_available - P_egg_export) +
      (animal_output$P_meat_local_to_consumption - P_meat_export) + 
      (animal_output$K_milk_available - P_dairy_export)
    K_local_animal_products_consumed <- (animal_output$K_egg_available - K_egg_export) +
      (animal_output$K_meat_local_to_consumption - K_meat_export) + 
      (animal_output$K_milk_available - K_dairy_export)
    
    
    N_local_vegetal_products_consumed <- crop_output$N_crop_human_consumption_processed - N_vegetable_export
    P_local_vegetal_products_consumed <- crop_output$P_crop_human_consumption_processed - P_vegetable_export
    K_local_vegetal_products_consumed <- crop_output$K_crop_human_consumption_processed - K_vegetable_export
    
    
    #export of vegetal products is still missing
    
    #combine import and export to list
    import_export <- list(N_egg_import = N_egg_import, 
                          P_egg_import = P_egg_import,
                          K_egg_import = K_egg_import,
                          N_meat_import = N_meat_import,
                          P_meat_import = P_meat_import,
                          K_meat_import = K_meat_import,
                          N_dairy_import = N_dairy_import, 
                          P_dairy_import = P_dairy_import,
                          K_dairy_import = K_dairy_import,
                          N_vegetable_import = N_vegetable_import,
                          P_vegetable_import = P_vegetable_import,
                          K_vegetable_import = K_vegetable_import,
                          
                          N_egg_export = N_egg_export,
                          P_egg_export = P_egg_export,
                          K_egg_export = K_egg_export,
                          N_meat_export = N_meat_export,
                          P_meat_export = P_meat_export,
                          K_meat_export = K_meat_export,
                          N_dairy_export = N_dairy_export,
                          P_dairy_export = P_dairy_export,
                          K_dairy_export = K_dairy_export,
                          N_vegetable_export = N_vegetable_export,
                          P_vegetable_export = P_vegetable_export,
                          K_vegetable_export = K_vegetable_export)
    
    
    #get a balance of N for animal subsystem----
    N_animal_in <- N_feed_import + N_animal_local_input
    P_animal_in <- P_feed_import + P_animal_local_input
    K_animal_in <- K_feed_import + K_animal_local_input
    
    N_animal_out <- N_animal_output_produced
    P_animal_out <- P_animal_output_produced
    K_animal_out <- K_animal_output_produced
    
    N_animal_balance <- N_animal_in - N_animal_out
    P_animal_balance <- P_animal_in - P_animal_out
    K_animal_balance <- K_animal_in - K_animal_out
    
    
    
    
    
    #extract the flows shown in Bernous model and return those
    #give them same name as in the chart
    #also I need to return the correct area of crops and horticultural products, because the original input got adjusted in the submodel
    combined_output <- list(scenario = scenario,
                            sewage_N = waste_output$N_sewage_in,
                            sewage_P = waste_output$P_sewage_in,
                            sewage_K = waste_output$K_sewage_in,
                            ofmsw_residual_waste_N = waste_output$N_grey_bin_food_waste + waste_output$N_grey_bin_garden_waste,
                            ofmsw_residual_waste_P = waste_output$P_grey_bin_food_waste + waste_output$P_grey_bin_garden_waste,
                            ofmsw_residual_waste_K = waste_output$K_grey_bin_food_waste + waste_output$K_grey_bin_garden_waste,
                            ofmsw_N = waste_output$N_ofmsw_local + waste_output$N_green_waste_local,
                            ofmsw_P = waste_output$P_ofmsw_local + waste_output$P_green_waste_local,
                            ofmsw_K = waste_output$K_ofmsw_local + waste_output$K_green_waste_local,
                            wastewater_direct_discharge_N = N_wastewater_direct_discharge,
                            wastewater_direct_discharge_P = P_wastewater_direct_discharge,
                            wastewater_direct_discharge_K = K_wastewater_direct_discharge,
                            compost_to_consumption_N = waste_output$N_compost_consumption,
                            compost_to_consumption_P = waste_output$P_compost_consumption,
                            compost_to_consumption_K = waste_output$K_compost_consumption,
                            digestate_N = N_digestate,
                            digestate_P = P_digestate,
                            digestate_K = K_digestate,
                            sewage_sludge_export_N = waste_output$N_sewage_exported,
                            sewage_sludge_export_P = waste_output$P_sewage_exported,
                            sewage_sludge_export_K = waste_output$K_sewage_exported,
                            wastewater_effluent_gaseous_losses_N = waste_output$N_sewage_lost,
                            wastewater_effluent_gaseous_losses_P = waste_output$P_sewage_lost,
                            wastewater_effluent_gaseous_losses_K = waste_output$K_sewage_lost,
                            fresh_compost_export_N = waste_output$N_compost_export,
                            fresh_compost_export_P = waste_output$P_compost_export,
                            fresh_compost_export_K = waste_output$K_compost_export,
                            fresh_compost_crop_N = waste_output$N_compost_crop,
                            fresh_compost_crop_P = waste_output$P_compost_crop,
                            fresh_compost_crop_K = waste_output$K_compost_crop,
                            sewage_to_crop_N = waste_output$N_sewage_to_crop,
                            sewage_to_crop_P = waste_output$P_sewage_to_crop,
                            sewage_to_crop_K = waste_output$K_sewage_to_crop,
                            vegetal_biogas_substrate_N = crop_output$N_crop_biogas,
                            vegetal_biogas_substrate_P = crop_output$P_crop_biogas,
                            vegetal_biogas_substrate_K = crop_output$K_crop_biogas,
                            crop_cultivation_losses_N = crop_output$inevitable_N_losses,
                            crop_cultivation_losses_P = crop_output$inevitable_P_losses,
                            crop_cultivation_losses_K = crop_output$inevitable_K_losses,
                            other_organic_fertilizer_export_N = export_other_organic_N_kg,
                            other_organic_fertilizer_export_P = export_other_organic_P_kg,
                            other_organic_fertilizer_export_K = export_other_organic_K_kg,
                            straw_N = crop_output$N_straw,
                            straw_P = crop_output$P_straw,
                            straw_K = crop_output$K_straw,
                            feed_crops_N = crop_output$N_crop_animal_feeding_unprocessed,
                            feed_crops_P = crop_output$P_crop_animal_feeding_unprocessed,
                            feed_crops_K = crop_output$K_crop_animal_feeding_unprocessed,
                            grassbased_feed_N = crop_output$N_grassland,
                            grassbased_feed_P = crop_output$P_grassland,
                            grassbased_feed_K = crop_output$K_grassland,
                            fruit_and_vegetable_N = crop_output$total_N_horticulture,
                            fruit_and_vegetable_P = crop_output$total_P_horticulture,
                            fruit_and_vegetable_K = crop_output$total_K_horticulture,
                            food_and_feed_crops_N = crop_output$N_crop_animal_feeding_processed + crop_output$N_crop_human_consumption_processed,
                            food_and_feed_crops_P = crop_output$P_crop_animal_feeding_processed + crop_output$P_crop_human_consumption_processed,
                            food_and_feed_crops_K = crop_output$K_crop_animal_feeding_processed + crop_output$K_crop_human_consumption_processed,
                            manure_as_biogas_substrate_N = N_biogas_input_animal,
                            manure_as_biogas_substrate_P = P_biogas_input_animal,
                            manure_as_biogas_substrate_K = K_biogas_input_animal,
                            manure_to_crop_N = N_manure_to_crop,
                            manure_to_crop_P = P_manure_to_crop,
                            manure_to_crop_K = K_manure_to_crop,
                            manure_export_N = export_manure_N_kg,
                            manure_export_P = export_manure_P_kg,
                            manure_export_K = export_manure_K_kg,
                            animal_housing_and_storage_losses_N = animal_output$N_housing_loss,
                            animal_housing_and_storage_losses_P = animal_output$P_housing_loss,
                            animal_housing_and_storage_losses_K = animal_output$K_housing_loss,
                            slaughter_animal_N = animal_output$N_to_slaughter,
                            slaughter_animal_P = animal_output$P_to_slaughter,
                            slaughter_animal_K = animal_output$K_to_slaughter,
                            egg_and_dairy = animal_output$N_milk_available + animal_output$N_egg_available,
                            egg_and_dairy = animal_output$P_milk_available + animal_output$P_egg_available,
                            egg_and_dairy = animal_output$K_milk_available + animal_output$K_egg_available,
                            local_vegetal_products_consumed_N = N_local_vegetal_products_consumed,
                            local_vegetal_products_consumed_P = P_local_vegetal_products_consumed,
                            local_vegetal_products_consumed_K = K_local_vegetal_products_consumed,
                            imported_animal_products_N = N_meat_import + N_egg_import + N_dairy_import,
                            imported_animal_products_P = P_meat_import + P_egg_import + P_dairy_import,
                            imported_animal_products_K = K_meat_import + K_egg_import + K_dairy_import,
                            imported_vegetal_products_N = N_vegetable_import,
                            imported_vegetal_products_P = P_vegetable_import,
                            imported_vegetal_products_K = K_vegetable_import,
                            feed_from_processed_crops_N = crop_output$N_crop_animal_feeding_processed,
                            feed_from_processed_crops_P = crop_output$P_crop_animal_feeding_processed,
                            feed_from_processed_crops_K = crop_output$K_crop_animal_feeding_processed,
                            import_processed_feed_N = N_feed_import,
                            import_processed_feed_P = P_feed_import,
                            import_processed_feed_K = K_feed_import,
                            local_animal_products_consumed_N = N_local_animal_products_consumed,
                            local_animal_products_consumed_P = P_local_animal_products_consumed,
                            local_animal_products_consumed_K = K_local_animal_products_consumed,
                            export_meat_N = N_meat_export,
                            export_meat_P = P_meat_export,
                            export_meat_K = K_meat_export,
                            import_meat_N = N_meat_import,
                            import_meat_P = P_meat_import,
                            import_meat_K = K_meat_import,
                            export_egg_N = N_egg_export,
                            export_egg_P = P_egg_export,
                            export_egg_K = K_egg_export,
                            slaughter_waste_N = animal_output$N_slaughter_waste,
                            slaughter_waste_P = animal_output$P_slaughter_waste,
                            slaughter_waste_K = animal_output$K_slaughter_waste,
                            import_OFMSW_N = waste_output$N_green_waste_import + waste_output$N_ofmsw_import,
                            import_OFMSW_P = waste_output$P_green_waste_import + waste_output$P_ofmsw_import,
                            import_OFMSW_K = waste_output$K_green_waste_import + waste_output$K_ofmsw_import,
                            import_inorganic_fertilizer_N = crop_output$imported_inorganic_N,
                            import_inorganic_fertilizer_P = crop_output$imported_inorganic_P,
                            import_inorganic_fertilizer_K = crop_output$imported_inorganic_K,
                            import_organic_fertilizer_N = import_organic_N_kg,
                            import_organic_fertilizer_P = import_organic_P_kg,
                            import_organic_fertilizer_K = import_organic_K_kg,
                            net_food_import_N = N_total_food_import,
                            net_food_import_P = P_total_food_import,
                            net_food_import_K = K_total_food_import,
                            net_feed_import_N = N_feed_import,
                            net_feed_import_P = P_feed_import,
                            net_feed_import_K = K_feed_import,
                            
                            share_beans = crop_output$share_winter_wheat, 
                            share_corn = crop_output$share_corn,
                            share_fodder_peas = crop_output$share_fodder_peas, 
                            share_mais_silage = crop_output$share_mais_silage,
                            share_oat = crop_output$share_oat, 
                            share_oilseed_rape = crop_output$share_oilseed_rape,
                            share_potato = crop_output$share_potato, 
                            share_rye = crop_output$share_rye, 
                            share_sugar_beet = crop_output$share_sugar_beet, 
                            share_summer_barley = crop_output$share_summer_barley,
                            share_summer_wheat = crop_output$share_summer_wheat, 
                            share_triticale = crop_output$share_triticale,
                            share_winter_barley = crop_output$share_winter_barley, 
                            share_winter_wheat = crop_output$share_winter_wheat,
                            
                            land_apple_ha = crop_output$land_apple_ha,  
                            land_arugula_ha = crop_output$land_arugula_ha,
                            land_asparagus_ha = crop_output$land_asparagus_ha, 
                            land_berries_ha = crop_output$land_berries_ha,
                            land_cabbage_ha = crop_output$land_cabbage_ha,
                            land_carrot_ha = crop_output$land_carrot_ha,
                            land_celery_ha = crop_output$land_celery_ha,
                            land_green_bean_ha = crop_output$land_green_bean_ha,
                            land_lambs_lettuce_ha = crop_output$land_lambs_lettuce_ha,
                            land_lettuce_ha = crop_output$land_lettuce_ha,
                            land_onion_ha = crop_output$land_onion_ha,
                            land_parsley_ha = crop_output$land_parsley_ha,
                            land_pumpkin_ha = crop_output$land_pumpkin_ha,
                            land_radishes_ha = crop_output$land_radishes_ha,
                            land_rhubarb_ha = crop_output$land_rhubarb_ha,
                            land_spinash_ha = crop_output$land_spinash_ha,
                            land_stone_fruit_ha = crop_output$land_stone_fruit_ha,
                            land_strawberry_ha = crop_output$land_strawberry_ha,
                            land_sweet_corn_ha = crop_output$land_sweet_corn_ha,
                            land_veggie_peas_ha = crop_output$land_veggie_peas_ha
    )
    
    #parameters to evaluate model output:
    #
    #- total N self supplied: 
    #- total N input from external
    #- total N lost (exported)
    #- SSE: self supplied * 100 / (self supplied + import - export)
    #- total N supplied outside of Kleve (in forms of products
    
    self_supplied_N <- N_manure_to_crop + crop_output$N_crop_animal_feeding_unprocessed +
      crop_output$N_grassland + N_local_animal_products_consumed + N_digestate +
      N_local_vegetal_products_consumed + crop_output$N_crop_animal_feeding_processed +
      waste_output$N_compost_crop + waste_output$N_compost_consumption + 
      waste_output$N_sewage_to_crop + crop_output$N_straw
    
    self_supplied_P <- P_manure_to_crop + crop_output$P_crop_animal_feeding_unprocessed +
      crop_output$P_grassland + P_local_animal_products_consumed + P_digestate +
      P_local_vegetal_products_consumed + crop_output$P_crop_animal_feeding_processed +
      waste_output$P_compost_crop + waste_output$P_compost_consumption + 
      waste_output$P_sewage_to_crop + crop_output$P_straw
    
    self_supplied_K <- K_manure_to_crop + crop_output$K_crop_animal_feeding_unprocessed +
      crop_output$K_grassland + K_local_animal_products_consumed + K_digestate +
      K_local_vegetal_products_consumed + crop_output$K_crop_animal_feeding_processed +
      waste_output$K_compost_crop + waste_output$K_compost_consumption + 
      waste_output$K_sewage_to_crop + crop_output$K_straw
    
    
    
    external_input_N <- N_feed_import + crop_output$imported_inorganic_N +
      import_organic_N_kg + N_vegetable_import + waste_output$N_ofmsw_import + 
      waste_output$N_green_waste_import + N_egg_import + N_meat_import + N_dairy_import
    
    external_input_P <- P_feed_import + crop_output$imported_inorganic_P +
      import_organic_P_kg + P_vegetable_import + waste_output$P_ofmsw_import + 
      waste_output$P_green_waste_import + P_egg_import + P_meat_import + P_dairy_import
    
    external_input_K <- K_feed_import + crop_output$imported_inorganic_K +
      import_organic_K_kg + K_vegetable_import + waste_output$K_ofmsw_import + 
      waste_output$K_green_waste_import + K_egg_import + K_meat_import + K_dairy_import
    
    
    
    system_output_N  <- export_manure_N_kg + N_dairy_export + N_egg_export + N_meat_export + 
      N_vegetable_export + export_other_organic_N_kg + 
      waste_output$N_sewage_exported + waste_output$N_compost_export
    
    system_output_P  <- export_manure_P_kg + P_dairy_export + P_egg_export + P_meat_export + 
      P_vegetable_export + export_other_organic_P_kg + 
      waste_output$P_sewage_exported + waste_output$P_compost_export
    
    system_output_K  <- export_manure_K_kg + K_dairy_export + K_egg_export + K_meat_export + 
      K_vegetable_export + export_other_organic_K_kg + 
      waste_output$K_sewage_exported + waste_output$K_compost_export
    
    
    
    system_losses_N <- crop_output$inevitable_N_losses + animal_output$N_housing_loss + 
      waste_output$N_sewage_lost + animal_output$N_slaughter_waste + 
      N_wastewater_direct_discharge 
    
    system_losses_P <- crop_output$inevitable_P_losses + animal_output$P_housing_loss + 
      waste_output$P_sewage_lost + animal_output$P_slaughter_waste + 
      P_wastewater_direct_discharge 
    
    system_losses_K <- crop_output$inevitable_K_losses + animal_output$K_housing_loss + 
      waste_output$K_sewage_lost + animal_output$K_slaughter_waste + 
      K_wastewater_direct_discharge 
    
    
    
    SSE_N <- (self_supplied_N * 100) / (self_supplied_N + external_input_N - system_output_N)
    
    SSE_P <- (self_supplied_P * 100) / (self_supplied_P + external_input_P - system_output_P)
    
    SSE_K <- (self_supplied_K * 100) / (self_supplied_K + external_input_K - system_output_K)
    
    
    
    
    #this is about stuff which would have been possible to supply locally 
    supplied_by_outside_N <- N_feed_import +  crop_output$imported_inorganic_N  +
      import_organic_N_kg + N_egg_import + N_meat_import + 
      N_dairy_import + 
      (N_vegetable_import - consumption_output$consumed_N_foreign_vegetable)
    
    supplied_by_outside_P <- P_feed_import +  crop_output$imported_inorganic_P  +
      import_organic_P_kg + P_egg_import + P_meat_import + 
      P_dairy_import + 
      (P_vegetable_import - consumption_output$consumed_P_foreign_vegetable)
    
    supplied_by_outside_K <- K_feed_import +  crop_output$imported_inorganic_K  +
      import_organic_K_kg + K_egg_import + K_meat_import + 
      K_dairy_import + 
      (K_vegetable_import - consumption_output$consumed_K_foreign_vegetable)
    
    
    
    model_evaluation <- c(list(scenario = c(model_evaluation$scenario, scenario),
                               self_supplied = c(model_evaluation$self_supplied, self_supplied_N),
                               external_input = c(model_evaluation$external_input, external_input_N),
                               system_output = c(model_evaluation$system_output, system_output_N),
                               system_losses =  c(model_evaluation$system_losses, system_losses_N),
                               SSE = c(model_evaluation$SSE, SSE_N),
                               supplied_by_outside = c(model_evaluation$supplied_by_outside, supplied_by_outside_N)))
    
    

    
  }
  
  return(model_evaluation)

}

#let mc simulation run, just to test if everything works out
nitrogen_mc_simulation <- mcSimulation(estimate = as.estimate(input),
                                       model_function = combined_function,
                                       numberOfModelRuns = 100,
                                       functionSyntax = "plainNames")


#how to make this also for other nutrients without copying everything?
#loop within function for different nutrients?
#or maybe make everything a vector? each entry of the vector is one nutrient?


#for each scenario different column, can't I make so that it is combined?
scenario <- c(nitrogen_mc_simulation$y$scenario1, nitrogen_mc_simulation$y$scenario2)
self_supplied <- c(nitrogen_mc_simulation$y$self_supplied1, nitrogen_mc_simulation$y$self_supplied2)
external_input <- c(nitrogen_mc_simulation$y$external_input1, nitrogen_mc_simulation$y$external_input2)
system_output <- c(nitrogen_mc_simulation$y$system_output1, nitrogen_mc_simulation$y$system_output2)
system_losses <- c(nitrogen_mc_simulation$y$system_losses1, nitrogen_mc_simulation$y$system_losses2)
SSE <- c(nitrogen_mc_simulation$y$SSE1, nitrogen_mc_simulation$y$SSE2)
supplied_by_outside <- c(nitrogen_mc_simulation$y$supplied_by_outside1, nitrogen_mc_simulation$y$supplied_by_outside2)

#combine to new dataframe and replace the old one in nitrogen_mc_simulation
y <- data.frame(scenario, self_supplied, external_input, system_output, system_losses, SSE, supplied_by_outside)

#in that case also the 



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


#do also correction for horticulture products
crop <- c('apple','arugula','asparagus','berries','cabbage',
          'carrot','celery','green_bean','lambs_lettuce',
          'lettuce','onion', 'parsley', 'pumpkin', 'radishes',
          'rhubarb', 'spinash', 'stone_fruit', 'strawberry',
          'sweet_corn', 'veggie_peas')

crop <- paste0('land_', crop, '_ha')

for(crop_i in crop){
  print(crop_i)
  nitrogen_mc_simulation$x[crop_i] <- nitrogen_mc_simulation$y[crop_i]
}



#other visualizations attempts ----
#load needed libraries
library(tidyverse)
library(reshape2)
library(ggridges)


#assign output to new variable
mc_simulation_output <- nitrogen_mc_simulation$y

#melt df to make it ggplot-friendly
mc_simulation_output_long <- melt(mc_simulation_output)

#change output from kg to t
mc_simulation_output_long$value <- mc_simulation_output_long$value / 1000


#classify which stream belongs to which subsystem
#we always use streams leaving a system to decide to which subsystem it belongs to
#e.g. manure going to crop belongs to the animal subsystem

streams_df <- rbind.data.frame(c('sewage', 'consumption', 'waste'),
                               c('ofmsw_residual_waste', 'consumption', 'waste'),
                               c('ofmsw','consumption','waste'),
                               c('wastewater','consumption','export'),
                               c('compost_to_consumption','waste','consumption'),
                               c('digestate','waste','crop'),
                               c('sewage_sludge_export','waste','export'),
                               c('wastewater_effluent_gaseous_losses','waste','export'),
                               c('fresh_compost_export','waste','export'),
                               c('fresh_compost_crop','waste','crop'),
                               c('sewage_to_crop','waste','crop'),
                               c('vegetal_biogas_substrate','crop','waste'),
                               c('crop_cultivation_losses','crop','export'),
                               c('other_organic_fertilizer_export','crop','export'),
                               c('straw','crop','animal'),
                               c('feed_crops','crop','animal'),
                               c('grassbased_feed','crop','animal'),
                               c('fruit_and_vegetable','crop','processing'),
                               c('food_and_feed_crops','crop','processing'),
                               c('manure_as_biogas_substrate','animal','waste'),
                               c('manure_to_crop','animal','crop'),
                               c('manure_export','animal','export'),
                               c('animal_housing_and_storage_losses','animal','export'),
                               c('slaughter_animal','animal','processing'),
                               c('egg_and_dairy','animal','processing'),
                               c('local_vegetal_products_consumed','processing','consumption'),
                               c('imported_animal_products','processing','consumption'),
                               c('imported_vegetal_products','processing','consumption'),
                               c('feed_from_processed_crops','processing','animal'),
                               c('import_processed_feed','processing','animal'),
                               c('local_animal_products_consumed','processing','consumption'),
                               c('export_meat','processing','export'),
                               c('import_meat','import','processing'),
                               c('export_egg','processing','export'),
                               c('slaughter_waste','processing','export'),
                               c('import_OFMSW','import','waste'),
                               c('import_inorganic_fertilizer','import','crop'),
                               c('import_organic_fertilizer','import','crop'),
                               c('net_food_import','import','processing'),
                               c('net_feed_import','import','processing'))

#adjust column names
names(streams_df) <- c('flow', 'origin', 'destination')

#make origin and destination factor
streams_df$origin <- as.factor(streams_df$origin)
streams_df$destination <- as.factor(streams_df$destination)





mc_simulation_output_long %>%
  filter(variable %in% streams_df$flow[streams_df$origin == 'animal']) %>%
  ggplot(aes(x=value,y = variable,fill = variable)) +
  geom_density_ridges_gradient(scale=2) + 
  xlab('N [t per year]')+
  ylab('flow leaving animal subsystem (without feed import)')+
  theme_bw() +
  theme(legend.position = "none")


mc_simulation_output_long %>%
  filter(variable %in% streams_df$flow[streams_df$destination == 'animal']) %>%
  ggplot(aes(x=value,y = variable,fill = variable)) +
  geom_density_ridges_gradient(scale=2) + 
  xlab('N [t per year]')+
  ylab('flow leaving animal subsystem (without feed import)')+
  theme_bw() +
  theme(legend.position = "none")




# 
# 
# mc_simulation_output_long %>%
#   filter(variable %in% c('N_to_slaughter','N_meat_local_to_consumption','N_slaughter_waste',
#                          'N_egg_available','N_housing_loss', 'N_milk_available',
#                          'N_biogas_input_animal','export_org_fertilizer',
#                          'N_manure_to_crop')) %>%
#   
#   ggplot(aes(x=value,y = variable,fill = variable)) +
#   geom_density_ridges_gradient(scale=2) + 
#   xlab('N [t per year]')+
#   ylab('flow leaving animal subsystem (without feed import)')+
#   theme_bw() +
#   theme(legend.position = "none")
# 
# mc_simulation_output_long %>%
#   filter(variable %in% c('egg_import','dairy_import',
#                          'meat_import', 'other_food_import')) %>%
#   ggplot(aes(x=value,y = variable,fill = variable)) +
#   geom_density_ridges_gradient(scale=2) + 
#   xlab('food import (without crops) N [t per year]')+
#   ylab('')+
#   theme_bw() +
#   theme(legend.position = "none")
# 
# mc_simulation_output_long %>%
#   filter(variable %in% c('egg_export','dairy_export',
#                          'meat_export')) %>%
#   
#   ggplot(aes(x=value,y = variable,fill = variable)) +
#   geom_density_ridges_gradient(scale=2) + 
#   xlab('food export (without vegetables) N [t per year]')+
#   ylab('')+
#   theme_bw() +
#   theme(legend.position = "none")
# 
# #calculate the amount of cases more eggs produced that consumed
# sum(mc_simulation_output$egg_export > 0) / 10000
# sum(mc_simulation_output$dairy_export > 0) / 10000
# sum(mc_simulation_output$meat_export > 0) / 10000
# 
# 
# 
# #plots of flows out the animal subsystem
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c("N_to_slaughter",'N_meat_local_to_consumption','N_slaughter_waste'),
#                    method = "smooth_simple_overlay",
#                    old_names = c('N_to_slaughter','N_meat_local_to_consumption', 'N_slaughter_waste'),
#                    x_axis_name = 'kg N  / year')
# ?plot_distributions
# 
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c('N_egg_available'),
#                    method = "smooth_simple_overlay",
#                    old_names = c('N_egg_available'),
#                    x_axis_name = 'kg N  / year')
# 
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c('N_milk_available'),
#                    method = "smooth_simple_overlay",
#                    old_names = c('N_milk_available'),
#                    x_axis_name = 'kg N  / year')
# 
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c('N_housing_loss','N_biogas_input_animal','export_org_fertilizer'),
#                    method = "smooth_simple_overlay",
#                    x_axis_name = 'kg N  / year')
# 
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c('N_manure_to_crop'),
#                    method = "smooth_simple_overlay",
#                    x_axis_name = 'kg N  / year')
# 
# #plots flow entering the system
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c("N_straw"),
#                    method = "smooth_simple_overlay",
#                    x_axis_name = 'kg N  / year')
# 
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c('N_crop_animal_feeding_unprocessed','N_grassland'),
#                    method = "smooth_simple_overlay",
#                    x_axis_name = 'kg N  / year')
# 
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c('N_crop_animal_feeding_processed'),
#                    method = "smooth_simple_overlay",
#                    x_axis_name = 'kg N  / year')
# 
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c('feed_import'),
#                    method = "smooth_simple_overlay",
#                    x_axis_name = 'kg N  / year')
# 
# 
# 
# #PLS----
# 
# pls_result <- plsr.mcSimulation(object = nitrogen_mc_simulation,
#                                 resultName = names(nitrogen_mc_simulation$y['N_manure_to_crop']), ncomp = 1)
# 
# plot_pls(pls_result, input = combined_input, threshold = 0.8)
# 
