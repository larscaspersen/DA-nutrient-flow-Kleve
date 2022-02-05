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
                                 others_housingloss_rate)
    
    ############
    #FEED IMPORT
    ############
    
    #calculate the amount of feed needed to be imported ----
    #= sum of products produced (egg, milk, meat (lifeweight) minus local feed (gras, fodder crops))
    
    animal_output_produced <- animal_output$N_milk_available + animal_output$N_egg_available +
      animal_output$N_remaining_manure + animal_output$N_housing_loss +
      animal_output$N_to_slaughter
    
    animal_local_input <- crop_output$N_straw + crop_output$N_grassland + 
      crop_output$N_crop_animal_feeding_processed +
      crop_output$N_crop_human_consumption_unprocessed
    
    #in bernous file the unprocessed feed is much higher
    #is there an error in the calculations or are numbers just so different from 2016 to 2020?
    
    feed_import <- animal_output_produced - animal_local_input
    
    

    #####
    #scenario locally source feed exclusively
    #####
    
    if(scenario == 'local_feed'){
      
      #calculate reduction factor if only local feed is used
      rf_local_feed <- animal_local_input / (animal_local_input + feed_import)
      #--> reduction by 80%
      
      #set the feed import in that case to zero
      feed_import <- 0
      
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
      
      #calculate manure remaining after biogas
      manure_after_biogas_old <- animal_output$N_remaining_manure - N_biogas_input_animal
      
      
      animal_output <- calc_animal(n_slaughter_dairy_cattle_reduced, 
                                   n_slaughter_female_cattle_reduced,
                                   n_slaughter_bulls_reduced, 
                                   n_slaughter_oxes_reduced, 
                                   n_slaughter_younstock_youngage_reduced,
                                   n_slaughter_younstock_midage_reduced, 
                                   n_slaughter_pig_reduced,
                                   n_slaughter_poultry_reduced, 
                                   n_slaughter_lamb_reduced,
                                   n_slaughter_sheep_reduced, 
                                   n_slaughter_goat_reduced, 
                                   n_slaughter_horse_reduced,
                                   n_slaughter_import_dairy_cattle, 
                                   n_slaughter_import_female_cattle,
                                   n_slaughter_import_bulls, 
                                   n_slaughter_import_oxes,
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
                                   n_dairy_cow_reduced,
                                   milk_per_cow, N_content_milk,
                                   share_milk_direct_sale, share_milk_other_use, 
                                   share_milk_to_factory,
                                   n_chicken_reduced,
                                   egg_per_chicken, `egg-weight`, N_content_egg,
                                   n_bull_reduced,
                                   n_heifer_reduced, 
                                   n_calf_reduced,
                                   n_pig_reduced, 
                                   n_other_poultry_reduced,
                                   n_sheep_reduced,
                                   N_excretion_dairy, N_excretion_bull,
                                   N_excretion_heifer, N_exctretion_calf,
                                   N_excretion_pig, N_excretion_hen,
                                   N_excretion_other_poultry,
                                   N_excretion_sheep,
                                   cattle_on_slurry, pig_on_slurry,
                                   cattle_housingloss_rate_liquid, cattle_housingloss_rate_solid,
                                   pig_housinglosss_rate_liquid, pig_housinglosss_rate_solid,
                                   others_housingloss_rate)
      
      
      #get new amount of manure after biogas
      manure_after_biogas_new <- animal_output$N_remaining_manure - N_biogas_input_animal
      
      #get the difference in manure available for crops, this difference needs to be imported in other ways
      #for example by inorganic fertilizer
      
      N_manure_missing <- manure_after_biogas_old - manure_after_biogas_new
      
      #assume in case of reduced manure there is no export of manure
      export_manure_N_kg <- 0
      
      #import the reduction in manure available to crops in form of inorganic fertilizer  
      crop_output$imported_inorganic_N <- crop_output$imported_inorganic_N + N_manure_missing
      
    }
    
    
    ########
    #BIOGAS
    ########
    
    #manure going to biogas ----
    N_biogas_input_animal <- N_biogas_input * share_N_biogas_input_animal
    
    
    #additional vegetal biomass going to biogas
    
    #N_crop_biogas contains so far only maize, but there are also other crops like sugar beet used
    #so the gap of animal based input and crop(actually maize based) input is then taken again from N_crop
    N_biogas_crop_missing <- N_biogas_input - (crop_output$N_crop_biogas + N_biogas_input_animal)
    
    #55% of additional biomass comes from cover crops, green rye and other crop sources
    #--> take it from the "rest" stream of the crop production, so reduced the stream by that
    crop_output$N_crop_rest <- crop_output$N_crop_rest - (0.55 * N_biogas_crop_missing)
    
    #20.4% comes from processed human crop food (sugar beet, cereals)
    crop_output$N_crop_human_consumption_processed <- crop_output$N_crop_human_consumption_processed - (0.204 * N_biogas_crop_missing)
    
    #14.2% comes from grass based sources 
    crop_output$N_grassland <- crop_output$N_grassland - (0.142 * N_biogas_crop_missing)
    
    #10.6% comes from processed animal food (cereal silage)
    crop_output$N_crop_animal_feeding_processed <- crop_output$N_crop_animal_feeding_processed - (0.106 * N_biogas_crop_missing)
    
    #update the crop biogas iput (because in the crop out it was sof ar only maize)
    crop_output$N_crop_biogas <- N_biogas_input - N_biogas_input_animal
    
    
    
    
    #manure going to crops after manure was sent to biogas----
    N_manure_to_crop <- animal_output$N_remaining_manure - N_biogas_input_animal - export_manure_N_kg
    
    
    
    #biogas output
    
    #biomass digestate (take input of animal and crop, process it for waste subsystem)
    lf_area <- arable_land + area_grassland
    
    #get total kwel by biogas (based on the current amount of agricultural land), convestion factor is percentage, thus devide by 100
    total_kwel <- lf_area * lf_to_kwel / 100
    volume_digestate <- total_kwel  * Kwel_to_digestate
    mass_digestate <- volume_digestate * digestate_density / 1000 #in tons. thus devide by 1000
    N_digestate <- mass_digestate * digestate_N_content #result is kg
    
    
    
    
    ################
    #WASTE SUBSYSTEM
    ################
    
    waste_output <- waste_function(waste_water,
                                   N_content_wastewater,
                                   lossrate_wastewater,
                                   share_N_sewage_for_agriculture,
                                   compost_to_horticulture,
                                   compost_to_export,
                                   compost_to_consumption,
                                   dm_compost_consumption,
                                   dm_compost_horticulture,
                                   N_content_compost_consumption,
                                   N_content_compost_horticulture,
                                   ofmsw_import,
                                   ofmsw_local,
                                   green_waste_import,
                                   green_waste_local,
                                   grey_bin_food_waste,
                                   grey_bin_garden_waste,
                                   dm_green_waste,
                                   dm_ofmsw,
                                   N_content_ofmsw_waste,
                                   N_content_green_waste)
    
    
    
    
    #################
    #FERTILIER LOSSES
    ################
    
    #calculate fertilization losses as a difference between crop input (inorganic fertilizer, organic fertilizer,
    #digestate, sewage) minus output (vegetal products, feed, other organic fertilizer exported from field)
    
    fertilization_losses_blackbox <- (N_digestate + N_manure_to_crop + crop_output$imported_inorganic_N + 
                                        import_organic_N_kg + waste_output$N_sewage_to_crop + 
                                        waste_output$N_compost_crop) - 
      (crop_output$N_crop_human_consumption_processed + 
         crop_output$N_crop_human_consumption_unprocessed +
         crop_output$N_crop_animal_feeding_processed + 
         crop_output$N_crop_animal_feeding_unprocessed + 
         crop_output$N_crop_biogas + 
         crop_output$N_grassland + 
         export_other_organic_N_kg)
    
    #flag to decide which fertilization losses calculation is used
    #use input_output fertilization losses
    use_input_output_fertilization_losses <- TRUE
    
    if(use_input_output_fertilization_losses){
      crop_output$inevitable_N_losses <- fertilization_losses_blackbox
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
                                       consume_sparkling_wine, N_content_sparkling_wine)
    

    
    
    ########
    #WASTE WATER
    ########
    
    
    #wastewater from consumption that doesnt reach the waste subsystem (remains in canal or direct discharge)
    #needs to be calculated here, because not part of the waste subsystem, but variable defined already in waste input file
    N_wastewater_direct_discharge <- (wastewater_direct_discharge + wastewater_remain_canal) * 1000 * N_content_wastewater / 1000000
    
    
    
    
    #######
    #IMPORT / EXPORT for consumption
    #######
    
    
    #calculate degree of self-sufficiency for meat, egg and milk production
    # animal_output$N_egg_available / consumption_output$consumed_N_egg
    # animal_output$N_milk_available / consumption_output$consumed_N_dairy
    # animal_output$N_meat_local_to_consumption / consumption_output$consumed_N_meat
    #production of crops still not done, cant calculate the sufficiency ratio yet
    
    #imoport is consumption - local production, prevent that lower as zero
    egg_import <- max(consumption_output$consumed_N_egg - animal_output$N_egg_available,0)
    meat_import <- max(consumption_output$consumed_N_meat - animal_output$N_meat_local_to_consumption, 0)
    dairy_import <- max(consumption_output$consumed_N_dairy - animal_output$N_milk_available, 0)
    vegetable_import <- max(consumption_output$consumed_N_vegetable - crop_output$N_crop_human_consumption_processed, 0) + consumption_output$consumed_N_foreign_vegetable
    
    total_food_import <-  egg_import + dairy_import + meat_import + vegetable_import
    
    #food exports (prevent negative exports if consumption exceeds the production)
    egg_export <- max(animal_output$N_egg_available - consumption_output$consumed_N_egg, 0)
    meat_export <- max(animal_output$N_meat_local_to_consumption - consumption_output$consumed_N_meat, 0) 
    dairy_export <- max(animal_output$N_milk_available - consumption_output$consumed_N_dairy, 0)
    vegetable_export <- max(crop_output$N_crop_human_consumption_processed - consumption_output$consumed_N_vegetable,0)
    
    #get amount of local products which also get consumed locally
    local_animal_products_consumed <- (animal_output$N_egg_available - egg_export) +
      (animal_output$N_meat_local_to_consumption - meat_export) + 
      (animal_output$N_milk_available - dairy_export)
    local_vegetal_products_consumed <- crop_output$N_crop_human_consumption_processed - vegetable_export
    
    #export of vegetal products is still missing
    
    #combine import and export to list
    import_export <- list(egg_import = egg_import, 
                          meat_import = meat_import,
                          dairy_import = dairy_import, 
                          vegetable_import = vegetable_import,
                          egg_export = egg_export, 
                          meat_export = meat_export,
                          dairy_export = dairy_export,
                          vegetable_export = vegetable_export)
    
    
    #get a balance of N for animal subsystem----
    N_animal_in <- feed_import + animal_local_input
    
    N_animal_out <- animal_output_produced
    
    N_animal_balance <- N_animal_in - N_animal_out
    
    
    
    
    
    #extract the flows shown in Bernous model and return those
    #give them same name as in the chart
    #also I need to return the correct area of crops and horticultural products, because the original input got adjusted in the submodel
    combined_output <- list(scenario = scenario,
                            sewage = waste_output$N_sewage_in,
                            ofmsw_residual_waste = waste_output$N_grey_bin_food_waste + waste_output$N_grey_bin_garden_waste,
                            ofmsw = waste_output$N_ofmsw_local + waste_output$N_green_waste_local,
                            wastewater_direct_discharge = wastewater_direct_discharge,
                            compost_to_consumption = waste_output$N_compost_consumption,
                            digestate = N_digestate,
                            sewage_sludge_export = waste_output$N_sewage_exported,
                            wastewater_effluent_gaseous_losses = waste_output$N_sewage_lost,
                            fresh_compost_export = waste_output$N_compost_export,
                            fresh_compost_crop = waste_output$N_compost_crop,
                            sewage_to_crop = waste_output$N_sewage_to_crop,
                            vegetal_biogas_substrate = crop_output$N_crop_biogas,
                            crop_cultivation_losses = crop_output$inevitable_N_losses,
                            other_organic_fertilizer_export = export_other_organic_N_kg,
                            straw = crop_output$N_straw,
                            feed_crops = crop_output$N_crop_animal_feeding_unprocessed,
                            grassbased_feed = crop_output$N_grassland,
                            fruit_and_vegetable = crop_output$total_N_horticulture,
                            food_and_feed_crops = crop_output$N_crop_animal_feeding_processed + crop_output$N_crop_human_consumption_processed,
                            manure_as_biogas_substrate = N_biogas_input_animal,
                            manure_to_crop = N_manure_to_crop,
                            manure_export = export_manure_N_kg,
                            animal_housing_and_storage_losses = animal_output$N_housing_loss,
                            slaughter_animal = animal_output$N_to_slaughter,
                            egg_and_dairy = animal_output$N_milk_available + animal_output$N_egg_available,
                            local_vegetal_products_consumed = local_vegetal_products_consumed,
                            imported_animal_products = meat_import + egg_import + dairy_import,
                            imported_vegetal_products = vegetable_import,
                            feed_from_processed_crops = crop_output$N_crop_animal_feeding_processed,
                            import_processed_feed = feed_import,
                            local_animal_products_consumed = local_animal_products_consumed,
                            export_meat = meat_export,
                            import_meat = meat_import,
                            export_egg = egg_export,
                            slaughter_waste = animal_output$N_slaughter_waste,
                            import_OFMSW = waste_output$N_green_waste_import + waste_output$N_ofmsw_import,
                            import_inorganic_fertilizer = crop_output$imported_inorganic_N,
                            import_organic_fertilizer = import_organic_N_kg,
                            net_food_import = total_food_import,
                            net_feed_import = feed_import,
                            
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
    
    self_supplied <- N_manure_to_crop + crop_output$N_crop_animal_feeding_unprocessed +
      crop_output$N_grassland + local_animal_products_consumed + N_digestate +
      local_vegetal_products_consumed + crop_output$N_crop_animal_feeding_processed +
      waste_output$N_compost_crop + waste_output$N_compost_consumption + 
      waste_output$N_sewage_to_crop + crop_output$N_straw
    
    external_input <- feed_import + crop_output$imported_inorganic_N +
      import_organic_N_kg + vegetable_import + waste_output$N_ofmsw_import + 
      waste_output$N_green_waste_import + egg_import + meat_import + dairy_import
    
    system_output  <- export_manure_N_kg + dairy_export + egg_export + meat_export + 
      vegetable_export + export_other_organic_N_kg + 
      waste_output$N_sewage_exported + waste_output$N_compost_export
    
    system_losses <- crop_output$inevitable_N_losses + animal_output$N_housing_loss + 
      waste_output$N_sewage_lost + animal_output$N_slaughter_waste + 
      N_wastewater_direct_discharge 
    
    SSE <- (self_supplied * 100) / (self_supplied + external_input - system_output)
    
    #this is about stuff which would have been possible to supply locally 
    supplied_by_outside <- feed_import +  crop_output$imported_inorganic_N  +
      import_organic_N_kg + egg_import + meat_import + 
      dairy_import + 
      (vegetable_import - consumption_output$consumed_N_foreign_vegetable)
    
    
    model_evaluation <- c(list(scenario = c(model_evaluation$scenario, scenario),
                               self_supplied = c(model_evaluation$self_supplied, self_supplied),
                               external_input = c(model_evaluation$external_input, external_input),
                               system_output = c(model_evaluation$system_output, system_output),
                               system_losses =  c(model_evaluation$system_losses, system_losses),
                               SSE = c(model_evaluation$SSE, SSE),
                               supplied_by_outside = c(model_evaluation$supplied_by_outside, supplied_by_outside)))
    
    

    
  }
  
  return(model_evaluation)

}

#let mc simulation run, just to test if everything works out
nitrogen_mc_simulation <- mcSimulation(estimate = as.estimate(input),
                                       model_function = combined_function,
                                       numberOfModelRuns = 100,
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
