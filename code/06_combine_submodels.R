# in this file I will try to combine the submodels of different scripts

library(decisionSupport)


# function to draw random variables from input and create global variables
make_variables <- function(est, n = 1) {
  x <- random(rho = est, n = n)
  for (i in colnames(x)) {
    assign(i,
      as.numeric(x[1, i]),
      envir = .GlobalEnv
    )
  }
}

# read function etc from the scripts
source("code/02_animal_model.R")
source("code/03_crop_model.R")
source("code/04_consumption_model.R")
source("code/05_waste_submodel.R")

input <- read.csv("data/input-all.csv")

make_variables(as.estimate(input))


# create function for mc-simulation
combined_function <- function() {

  # container for the model evaluation criteria
  model_evaluation <- list()

  combined_output <- list()


  # loop for the scenarios
  for (scenario in c("normal", "local_feed", "technology")) {

    # factor deciding how much manure input into biogas is reduced by scenario
    # default value is 1.0, so no reduction
    rf_manure_biogas_scenario <- 1.0

    # change factor for animal composition under local feed scenario
    # set to 1 in the beginning, so it doesnt affect normal scenario
    # changed in if part of local feed to value in input table
    cf_cattle_local_feed <- 1
    cf_pig_local_feed <- 1
    cf_poultry_local_feed <- 1
    cf_others_local_feed <- 1

    # this scenario is not worked out yet, so skip it in the loop
    if (scenario == "technology") {
      next()
    }


    # at first calculate the crop production system

    #-------------------#
    # CROP SUBSYSTEM ====
    #-------------------#

    crop_output <- crop_function(
      arable_land,
      land_horticulture_ha,
      estimated_vegetable_land,
      share_beans, share_corn, share_fodder_peas,
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
      yield_share_beans, yield_share_corn,
      yield_share_fodder_peas,
      yield_share_mais_silage, yield_share_oat,
      yield_share_oilseed_rape,
      yield_share_potato, yield_share_rye,
      yield_share_sugar_beet, yield_share_summer_barley,
      yield_share_summer_wheat, yield_share_triticale,
      yield_share_winter_barley, yield_share_winter_wheat,
      dm_beans,
      dm_corn,
      dm_fodder_peas,
      dm_mais_silage,
      dm_oat,
      dm_oilseed_rape,
      dm_potato, dm_rye,
      dm_sugar_beet,
      dm_summer_barley,
      dm_summer_wheat,
      dm_triticale, dm_winter_barley,
      dm_winter_wheat,
      N_yield_beans, N_yield_corn, N_yield_fodder_peas,
      N_yield_mais_silage, N_yield_oat,
      N_yield_oilseed_rape, N_yield_potato,
      N_yield_rye, N_yield_sugar_beet, N_yield_summer_barley,
      N_yield_summer_weat, N_yield_triticale,
      N_yield_winter_barley, N_yield_winter_weat,
      N_leftover_beans, N_leftover_corn,
      N_leftover_foder_peas, N_leftover_mais_silage,
      N_leftover_oat, N_leftover_oilseed_rape,
      N_leftover_potato, N_leftover_rye,
      N_leftover_sugar_beet, N_leftover_summer_barley,
      N_leftover_summer_wheat, N_leftover_triticale,
      N_leftover_winter_barley, N_leftover_winter_wheat,
      P_yield_beans, P_yield_corn, P_yield_fodder_peas,
      P_yield_mais_silage, P_yield_oat,
      P_yield_oilseed_rape, P_yield_potato,
      P_yield_rye, P_yield_sugar_beet, P_yield_summer_barley,
      P_yield_summer_weat, P_yield_triticale,
      P_yield_winter_barley, P_yield_winter_weat,
      P_leftover_beans, P_leftover_corn,
      P_leftover_foder_peas, P_leftover_mais_silage,
      P_leftover_oat, P_leftover_oilseed_rape,
      P_leftover_potato, P_leftover_rye,
      P_leftover_sugar_beet, P_leftover_summer_barley,
      P_leftover_summer_wheat, P_leftover_triticale,
      P_leftover_winter_barley, P_leftover_winter_wheat,
      K_yield_beans, K_yield_corn, K_yield_fodder_peas,
      K_yield_mais_silage, K_yield_oat,
      K_yield_oilseed_rape, K_yield_potato,
      K_yield_rye, K_yield_sugar_beet, K_yield_summer_barley,
      K_yield_summer_weat, K_yield_triticale,
      K_yield_winter_barley, K_yield_winter_weat,
      K_leftover_beans, K_leftover_corn,
      K_leftover_foder_peas, K_leftover_mais_silage,
      K_leftover_oat, K_leftover_oilseed_rape,
      K_leftover_potato, K_leftover_rye,
      K_leftover_sugar_beet, K_leftover_summer_barley,
      K_leftover_summer_wheat, K_leftover_triticale,
      K_leftover_winter_barley, K_leftover_winter_wheat,
      straw_share,
      beans_to_animal, corn_to_animal, fodder_peas_to_animal,
      mais_silage_to_animal, oat_to_animal, oilseed_rape_to_animal,
      potato_to_animal, rye_to_animal, sugar_beet_to_animal,
      summer_barley_to_animal, summer_wheat_to_animal,
      triticale_to_animal, winter_barley_to_animal,
      winter_wheat_to_animal,
      beans_to_consumption, corn_to_consumption, fodder_peas_to_consumption,
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
      land_parsley_ha, land_pumpkin_ha,
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
      import_inorganic_N_kg_LF, import_inorganic_P2O5_kg_LF, import_inorganic_K2O_t,
      convert_phosphorous_pentoxide_to_p, convert_potassium_oxide_to_k
    )

    if(scenario == 'local_feed'){
      
      #output of crop system that stays in Kleve (so without export and without losses)
      total_vegetable_production_without_export <- combined_output$vegetal_biogas_substrate_N[1] +
        combined_output$straw_N[1] + combined_output$feed_crops_N[1] +
        combined_output$grassbased_feed_N[1] + combined_output$feed_from_processed_crops_N[1] +
        combined_output$local_vegetal_products_consumed_N[1]
    
      #allocation of local crop output to biogas, share
      current_allocation_crop_biogas <- (combined_output$vegetal_biogas_substrate_N[1]) / total_vegetable_production_without_export
      
      #allocation of local crop to animal subsystem
      current_allocation_crop_animal <- (combined_output$straw_N[1] + combined_output$feed_crops_N[1] +
                                          combined_output$grassbased_feed_N[1] + combined_output$feed_from_processed_crops_N[1]) / total_vegetable_production_without_export
      
      #allocation of local crop to local human consumption
      current_allocation_crop_human <- combined_output$local_vegetal_products_consumed_N[1] / total_vegetable_production_without_export
      
      #see if the stakeholder crop allocation adds up to 1, otherwise correct
      stakeholder_crop_allocation_total <- scenario_allocate_crop_biogas + 
        scenario_allocate_crop_feed + scenario_allocate_crop_food
      
      correction_factor <- 1 / stakeholder_crop_allocation_total
      
      scenario_allocate_crop_biogas_corrected <- scenario_allocate_crop_biogas * correction_factor
      scenario_allocate_crop_feed_corrected <- scenario_allocate_crop_feed * correction_factor
      scenario_allocate_crop_food_corrected <- scenario_allocate_crop_food * correction_factor
      
      #get change factor for allocation in the local feed scenario, so that it 
      #alligns with stakeholder-propesed allocation
      cf_crop_to_biogas <- scenario_allocate_crop_biogas_corrected / current_allocation_crop_biogas
      cf_crop_to_feed <- scenario_allocate_crop_feed_corrected / current_allocation_crop_animal
      cf_crop_to_food <- scenario_allocate_crop_food_corrected / current_allocation_crop_human
      
      #multiply streams with the change factor
      #but when?
    }
    #---------------------#
    # ANIMAL SUBSYSTEM ####
    #---------------------#



    #--------------------------------------------------#
    ## convert livestock units to amount of animals ====
    #--------------------------------------------------#

    if (scenario == "local_feed") {

      # calculate the current compostion of the animals
      LLU_total <- LLU_cattle_2020 + LLU_pig_2020 + LLU_poultry_2020 + LLU_others_2020
      current_share_cattle <- LLU_cattle_2020 / LLU_total
      current_share_pig <- LLU_pig_2020 / LLU_total
      current_share_poultry <- LLU_poultry_2020 / LLU_total
      current_share_others <- LLU_others_2020 / LLU_total


      # change the compostion of animals by percentage changes reported by farmers under local feed scenario
      # this needs to be done before local feed calculations, so that I don't have to redo them
      # for changed compostion
      # factor to increase decrease the animal LLU

      # check if the scenario_composition adds up to 1
      sum_scenario_livestock_compostion <- scenario_share_cattle + scenario_share_pig + scenario_share_poultry + scenario_share_others
      # correction factor, so that they perfectly add up to 1
      correction_factor <- 1 / sum_scenario_livestock_compostion

      scenario_share_cattle_corrected <- scenario_share_cattle * correction_factor
      scenario_share_pig_corrected <- scenario_share_pig * correction_factor
      scenario_share_poultry_corrected <- scenario_share_poultry * correction_factor
      scenario_share_others_corrected <- scenario_share_others * correction_factor

      # change factor for local feed
      cf_cattle_local_feed <- scenario_share_cattle_corrected / current_share_cattle
      cf_pig_local_feed <- scenario_share_pig_corrected / current_share_pig
      cf_poultry_local_feed <- scenario_share_poultry_corrected / current_share_poultry
      cf_others_local_feed <- scenario_share_others_corrected / current_share_others
    }

    # nährstoffreport only says LLU but 'cattle' can mean anything from dairy cow to bull
    # so we linked it with compostion number of it.nrw
    # however, for 2020 it.nrw has no new number, so we assume same compostion, scaled by LLU of nährstoffreport

    n_dairy_cow <- (LLU_cattle_2020 / LLU_cattle_2016) * n_dairy_cow_2016 * cf_cattle_local_feed
    n_bull <- (LLU_cattle_2020 / LLU_cattle_2016) * n_bull_2016 * cf_cattle_local_feed
    n_heifer <- (LLU_cattle_2020 / LLU_cattle_2016) * n_heifer_2016 * cf_cattle_local_feed
    n_calf <- (LLU_cattle_2020 / LLU_cattle_2016) * n_calf_2016 * cf_cattle_local_feed
    n_chicken <- (LLU_poultry_2020 / LLU_poultry_2016) * n_chicken_2016 * cf_poultry_local_feed
    n_other_poultry <- (LLU_poultry_2020 / LLU_poultry_2016) * n_other_poultry_2016 * cf_poultry_local_feed
    n_pig <- (LLU_pig_2020 / LLU_pig_2016) * n_pig_2016 * cf_pig_local_feed
    n_sheep <- (LLU_others_2020 / LLU_others_2016) * n_sheep_2016 * cf_others_local_feed


    # calculate animal output with changed composition, but with same LLU as
    # in normal scenario
    animal_output <- calc_animal(
      n_slaughter_dairy_cattle,
      n_slaughter_female_cattle,
      n_slaughter_bulls,
      n_slaughter_oxes,
      n_slaughter_younstock_youngage,
      n_slaughter_younstock_midage,
      n_slaughter_pig,
      n_slaughter_poultry,
      n_slaughter_lamb,
      n_slaughter_sheep,
      n_slaughter_goat,
      n_slaughter_horse,
      n_slaughter_import_dairy_cattle,
      n_slaughter_import_female_cattle,
      n_slaughter_import_bulls,
      n_slaughter_import_oxes,
      n_slaughter_import_younstock_youngage,
      n_slaughter_import_younstock_midage,
      n_slaughter_import_pig,
      n_slaughter_import_poultry,
      n_slaughter_import_lamb,
      n_slaughter_import_sheep,
      n_slaughter_import_goat,
      n_slaughter_import_horse,
      slaughter_weight_dairy_cattle,
      slaughter_weight_female_cattle,
      slaughter_weight_bulls,
      slaughter_weight_oxes,
      slaughter_weight_younstock_youngage,
      slaughter_weight_younstock_midage,
      slaughter_weight_pig,
      slaughter_weight_poultry,
      slaughter_weight_lamb,
      slaughter_weight_sheep,
      slaughter_weight_goat,
      slaughter_weight_horse,
      slaughter_weight_fraction_cattle,
      slaughter_weight_fraction_pig,
      slaughter_weight_fraction_poultry,
      slaughter_weight_fraction_others,
      edible_fraction_cattle,
      edible_fraction_pig,
      edible_fraction_poultry,
      edible_fraction_other,
      N_content_female_cattle,
      N_content_male_cattle,
      N_content_pig,
      N_content_poultry,
      N_content_sheep,
      N_content_horse,
      P_content_female_cattle,
      P_content_male_cattle,
      P_content_pig,
      P_content_poultry,
      P_content_sheep,
      P_content_horse,
      K_content_female_cattle,
      K_content_male_cattle,
      K_content_pig,
      K_content_poultry,
      K_content_sheep,
      K_content_horse,
      n_dairy_cow,
      milk_per_cow,
      N_content_milk,
      P_content_milk,
      K_content_milk,
      share_milk_direct_sale,
      share_milk_other_use,
      share_milk_to_factory,
      n_chicken,
      egg_per_chicken,
      `egg-weight`,
      N_content_egg,
      P_content_egg,
      K_content_egg,
      n_bull,
      n_heifer,
      n_calf,
      n_pig,
      n_other_poultry,
      n_sheep,
      N_excretion_dairy,
      N_excretion_bull,
      N_excretion_heifer,
      N_exctretion_calf,
      N_excretion_pig,
      N_excretion_hen,
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
      cattle_on_slurry,
      pig_on_slurry,
      cattle_housingloss_rate_liquid,
      cattle_housingloss_rate_solid,
      pig_housinglosss_rate_liquid,
      pig_housinglosss_rate_solid,
      others_housingloss_rate,
      convert_phosphorous_pentoxide_to_p,
      convert_potassium_oxide_to_k,
      P_housing_losses,
      K_housing_losses
    )

    #-----------------#
    ## Feed import ####
    #-----------------#

    # calculate the amount of feed needed to be imported
    # = sum of products produced (egg, milk, meat (lifeweight) minus local feed (gras, fodder crops))

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

    # in bernous file the unprocessed feed is much higher
    # is there an error in the calculations or are numbers just so different from 2016 to 2020?

    N_feed_import <- max(N_animal_output_produced - N_animal_local_input, 0)
    P_feed_import <- max(P_animal_output_produced - P_animal_local_input, 0)
    K_feed_import <- max(K_animal_output_produced - K_animal_local_input, 0)


    #--------------------------------#
    ## reduction factor livestock ####
    #--------------------------------#

    if (scenario == "local_feed") {

      # calculate reduction factor if only local feed is used
      rf_local_feed_N <- N_animal_local_input / (N_animal_local_input + N_feed_import)
      rf_local_feed_P <- P_animal_local_input / (P_animal_local_input + P_feed_import)
      rf_local_feed_K <- K_animal_local_input / (K_animal_local_input + K_feed_import)

      #--> Phosphorous is much more limiting than Nitrogen, is that realistic?

      # reduction by Nitrogen, because farmers wouldnt care for P or K
      rf_local_feed <- rf_local_feed_N

      # stakeholders came up with different reduction factor
      rf_stakeholder <- 1 - scenario_overall_livestock_reduction

      ### temp

      rf_local_feed <- c(rf_local_feed, rf_stakeholder)

      ### tmep

      # if reduction by P would be larger, say that the animals just excrete less P
      # because they were fed excess P anyway

      if (rf_local_feed_N > rf_local_feed_P | rf_local_feed_N > rf_local_feed_K) {

        # calculate how much less P or K the animals would excrete
        # make sure the reduction is not lower than 0
        P_reduction_manure <- max(((rf_local_feed_N - rf_local_feed_P) * P_animal_local_input), 0)
        K_reduction_manure <- max(((rf_local_feed_N - rf_local_feed_K) * K_animal_local_input), 0)
      }

      # set the feed import in case of local feed to zero
      N_feed_import <- 0
      P_feed_import <- 0
      K_feed_import <- 0

      # here I could change the individual animal stocks, right now I will reduce them evenly
      n_dairy_cow_reduced <- n_dairy_cow * rf_local_feed
      n_heifer_reduced <- n_heifer * rf_local_feed
      n_bull_reduced <- n_bull * rf_local_feed
      n_calf_reduced <- n_calf * rf_local_feed
      n_pig_reduced <- n_pig * rf_local_feed
      n_chicken_reduced <- n_chicken * rf_local_feed
      n_other_poultry_reduced <- n_other_poultry * rf_local_feed
      n_sheep_reduced <- n_sheep * rf_local_feed



      # adjust the number of slaughter animals with the help of the changed stocks and with the caluclated slaughter rate

      n_slaughter_dairy_cattle_reduced <- n_dairy_cow_reduced * animal_output$slaughter_rate_dairy_cow
      n_slaughter_female_cattle_reduced <- n_heifer_reduced * animal_output$slaughter_rate_heifer
      n_slaughter_bulls_reduced <- n_bull_reduced * animal_output$slaughter_rate_bull
      n_slaughter_younstock_midage_reduced <- n_calf_reduced * animal_output$slaughter_rate_younstock_midage
      n_slaughter_younstock_youngage_reduced <- n_calf_reduced * animal_output$slaughter_rate_younstock_youngage
      n_slaughter_pig_reduced <- n_pig_reduced * animal_output$slaughter_rate_pig
      n_slaughter_poultry_reduced <- n_chicken_reduced * animal_output$slaughter_rate_poultry
      n_slaughter_sheep_reduced <- n_sheep_reduced * animal_output$slaughter_rate_sheep

      # reduce slaughter animals for which we dont have any stock, so there we assume reduction linear to reduction in available feed
      n_slaughter_goat_reduced <- n_slaughter_goat * rf_local_feed
      n_slaughter_horse_reduced <- n_slaughter_horse * rf_local_feed
      n_slaughter_lamb_reduced <- n_slaughter_lamb * rf_local_feed
      n_slaughter_oxes_reduced <- n_slaughter_oxes * rf_local_feed


      # is the import of slaughter animals set to zero or is it going to be the same?
      # for now I leave it the same


      # run the animal subsystem again now with the changed animal stocks
      # and with reduced local slaughter animal number


      #-------------------------#
      ## Manure after biogas ====
      #-------------------------#

      # get manure potentially available for crops after biogas
      # according to business as usual calculations

      # manure going to biogas
      N_biogas_input_animal <- N_biogas_input * share_N_biogas_input_animal
      P_biogas_input_animal <- P_biogas_input * share_P_biogas_input_animal
      K_biogas_input_animal <- K_biogas_input * share_K_biogas_input_animal

      # calculate manure remaining after biogas
      N_manure_after_biogas_old <- animal_output$N_remaining_manure - N_biogas_input_animal
      P_manure_after_biogas_old <- animal_output$P_remaining_manure - P_biogas_input_animal
      K_manure_after_biogas_old <- animal_output$K_remaining_manure - K_biogas_input_animal


      # check if allocation of manure adds up to 1, otherwise correct like
      # in crop land or animal composition
      sum_scenario_manure_allocation <- scenario_allocate_manure_biogas + scenario_allocate_manure_crop + scenario_allocate_manure_export
      correction_factor <- 1 / sum_scenario_manure_allocation

      # correct manure allocation values
      scenario_allocate_manure_biogas_corrected <- scenario_allocate_manure_biogas * correction_factor
      scenario_allocate_manure_crop_corrected <- scenario_allocate_manure_crop * correction_factor
      scenario_allocate_manure_export_corrected <- scenario_allocate_manure_export * correction_factor


      # calculate share of manure to total manure which went to the biogas
      share_N_manure_allocated_biogas <- N_biogas_input_animal / animal_output$N_remaining_manure

      # problem: allocation of manure is sofar not done by allocation rules but by
      # variable which explicitly states the amount of animal-derived biogas input

      # change factor: reduction of animal manure going to biogas
      cf_manure_to_biogas <- scenario_allocate_manure_biogas_corrected / share_N_manure_allocated_biogas





      #--------------------------------------------------#
      ## animal calculation under local feed scenario ####
      #--------------------------------------------------#
      
      animal_output <- list()
      
      for(i in 1:length(rf_local_feed)){
        animal_output <- c(animal_output, list(calc_animal(
          n_slaughter_dairy_cattle = n_slaughter_dairy_cattle_reduced[i],
          n_slaughter_female_cattle = n_slaughter_female_cattle_reduced[i],
          n_slaughter_bulls = n_slaughter_bulls_reduced[i],
          n_slaughter_oxes = n_slaughter_oxes_reduced[i],
          n_slaughter_younstock_youngage = n_slaughter_younstock_youngage_reduced[i],
          n_slaughter_younstock_midage = n_slaughter_younstock_midage_reduced[i],
          n_slaughter_pig = n_slaughter_pig_reduced[i],
          n_slaughter_poultry = n_slaughter_poultry_reduced[i],
          n_slaughter_lamb = n_slaughter_lamb_reduced[i],
          n_slaughter_sheep = n_slaughter_sheep_reduced[i],
          n_slaughter_goat = n_slaughter_goat_reduced[i],
          n_slaughter_horse = n_slaughter_horse_reduced[i],
          n_slaughter_import_dairy_cattle = n_slaughter_import_dairy_cattle,
          n_slaughter_import_female_cattle = n_slaughter_import_female_cattle,
          n_slaughter_import_bulls = n_slaughter_import_bulls,
          n_slaughter_import_oxes = n_slaughter_import_oxes,
          n_slaughter_import_younstock_youngage = n_slaughter_import_younstock_youngage,
          n_slaughter_import_younstock_midage = n_slaughter_import_younstock_midage,
          n_slaughter_import_pig = n_slaughter_import_pig,
          n_slaughter_import_poultry = n_slaughter_import_poultry,
          n_slaughter_import_lamb = n_slaughter_import_lamb,
          n_slaughter_import_sheep = n_slaughter_import_sheep,
          n_slaughter_import_goat = n_slaughter_import_goat,
          n_slaughter_import_horse = n_slaughter_import_horse,
          slaughter_weight_dairy_cattle = slaughter_weight_dairy_cattle,
          slaughter_weight_female_cattle = slaughter_weight_female_cattle,
          slaughter_weight_bulls = slaughter_weight_bulls,
          slaughter_weight_oxes = slaughter_weight_oxes,
          slaughter_weight_younstock_youngage = slaughter_weight_younstock_youngage,
          slaughter_weight_younstock_midage = slaughter_weight_younstock_midage,
          slaughter_weight_pig = slaughter_weight_pig,
          slaughter_weight_poultry = slaughter_weight_poultry,
          slaughter_weight_lamb = slaughter_weight_lamb,
          slaughter_weight_sheep = slaughter_weight_sheep,
          slaughter_weight_goat = slaughter_weight_goat,
          slaughter_weight_horse = slaughter_weight_horse,
          slaughter_weight_fraction_cattle = slaughter_weight_fraction_cattle,
          slaughter_weight_fraction_pig = slaughter_weight_fraction_pig,
          slaughter_weight_fraction_poultry = slaughter_weight_fraction_poultry,
          slaughter_weight_fraction_others = slaughter_weight_fraction_others,
          edible_fraction_cattle = edible_fraction_cattle,
          edible_fraction_pig = edible_fraction_pig,
          edible_fraction_poultry = edible_fraction_poultry,
          edible_fraction_other = edible_fraction_other,
          N_content_female_cattle = N_content_female_cattle,
          N_content_male_cattle = N_content_male_cattle,
          N_content_pig = N_content_pig,
          N_content_poultry = N_content_poultry,
          N_content_sheep = N_content_sheep,
          N_content_horse = N_content_horse,
          P_content_female_cattle = P_content_female_cattle,
          P_content_male_cattle = P_content_male_cattle,
          P_content_pig = P_content_pig,
          P_content_poultry = P_content_poultry,
          P_content_sheep = P_content_sheep,
          P_content_horse = P_content_horse,
          K_content_female_cattle = K_content_female_cattle,
          K_content_male_cattle = K_content_male_cattle,
          K_content_pig = K_content_pig,
          K_content_poultry = K_content_poultry,
          K_content_sheep = K_content_sheep,
          K_content_horse = K_content_horse,
          n_dairy_cow = n_dairy_cow_reduced[i],
          milk_per_cow = milk_per_cow,
          N_content_milk = N_content_milk,
          P_content_milk = P_content_milk,
          K_content_milk = K_content_milk,
          share_milk_direct_sale = share_milk_direct_sale,
          share_milk_other_use = share_milk_other_use,
          share_milk_to_factory = share_milk_to_factory,
          n_chicken = n_chicken_reduced[i],
          egg_per_chicken = egg_per_chicken,
          `egg-weight` = `egg-weight`,
          N_content_egg = N_content_egg,
          P_content_egg = P_content_egg,
          K_content_egg = K_content_egg,
          n_bull = n_bull_reduced[i],
          n_heifer = n_heifer_reduced[i],
          n_calf = n_calf_reduced[i],
          n_pig = n_pig_reduced[i],
          n_other_poultry = n_other_poultry_reduced[i],
          n_sheep = n_sheep_reduced[i],
          N_excretion_dairy = N_excretion_dairy,
          N_excretion_bull = N_excretion_bull,
          N_excretion_heifer = N_excretion_heifer,
          N_exctretion_calf = N_exctretion_calf,
          N_excretion_pig = N_excretion_pig,
          N_excretion_hen = N_excretion_hen,
          N_excretion_other_poultry = N_excretion_other_poultry,
          N_excretion_sheep = N_excretion_sheep,
          manure_N_to_PtwoOfive_dairy = manure_N_to_PtwoOfive_dairy,
          manure_N_to_PtwoOfive_bull = manure_N_to_PtwoOfive_bull,
          manure_N_to_PtwoOfive_heifer = manure_N_to_PtwoOfive_heifer,
          manure_N_to_PtwoOfive_calf = manure_N_to_PtwoOfive_calf,
          manure_N_to_PtwoOfive_pig = manure_N_to_PtwoOfive_pig,
          manure_N_to_PtwoOfive_hen = manure_N_to_PtwoOfive_hen,
          manure_N_to_PtwoOfive_other_poultry = manure_N_to_PtwoOfive_other_poultry,
          manure_N_to_PtwoOfive_sheep = manure_N_to_PtwoOfive_sheep,
          manure_N_to_KOtwo = manure_N_to_KOtwo,
          cattle_on_slurry = cattle_on_slurry,
          pig_on_slurry = pig_on_slurry,
          cattle_housingloss_rate_liquid = cattle_housingloss_rate_liquid,
          cattle_housingloss_rate_solid = cattle_housingloss_rate_solid,
          pig_housinglosss_rate_liquid = pig_housinglosss_rate_liquid,
          pig_housinglosss_rate_solid = pig_housinglosss_rate_solid,
          others_housingloss_rate = others_housingloss_rate,
          convert_phosphorous_pentoxide_to_p = convert_phosphorous_pentoxide_to_p,
          convert_potassium_oxide_to_k = convert_potassium_oxide_to_k,
          P_housing_losses = P_housing_losses,
          K_housing_losses = K_housing_losses
        )))
      }
      
      N_animal_output_produced <- purrr::map_dbl(animal_output, 'N_milk_available')  + 
        purrr::map_dbl(animal_output, 'N_egg_available') +
        purrr::map_dbl(animal_output, 'N_remaining_manure') + 
        purrr::map_dbl(animal_output, 'N_housing_loss') +
        purrr::map_dbl(animal_output, 'N_to_slaughter')
      P_animal_output_produced <- purrr::map_dbl(animal_output, 'P_milk_available')  + 
        purrr::map_dbl(animal_output, 'P_egg_available') +
        purrr::map_dbl(animal_output, 'P_remaining_manure') + 
        purrr::map_dbl(animal_output, 'P_housing_loss') +
        purrr::map_dbl(animal_output, 'P_to_slaughter')
      K_animal_output_produced <- purrr::map_dbl(animal_output, 'K_milk_available')  + 
        purrr::map_dbl(animal_output, 'K_egg_available') +
        purrr::map_dbl(animal_output, 'K_remaining_manure') + 
        purrr::map_dbl(animal_output, 'K_housing_loss') +
        purrr::map_dbl(animal_output, 'K_to_slaughter')
      
      # get new amount of manure after biogas

      # add a factor downscaling the manure to biogas according to what farmers report
      # this needs to go into input file
      # this needs to be set to 1.0 in the beginning, and in the if of scenario it it set to e.g. 0.8
      rf_manure_biogas_scenario <- cf_manure_to_biogas

      N_manure_after_biogas_new <- animal_output$N_remaining_manure - (N_biogas_input_animal * rf_manure_biogas_scenario)
      P_manure_after_biogas_new <- animal_output$P_remaining_manure - (P_biogas_input_animal * rf_manure_biogas_scenario)
      K_manure_after_biogas_new <- animal_output$K_remaining_manure - (K_biogas_input_animal * rf_manure_biogas_scenario)

      
      # apply new allocation of manure to export by stakeholders
      export_manure_N_kg <- export_manure_N_kg * scenario_allocate_manure_export
      export_manure_P_kg <- export_manure_P_kg * scenario_allocate_manure_export
      export_manure_K_kg <- export_manure_K_kg * scenario_allocate_manure_export
      
      # get the difference in manure available for crops, this difference needs 
      # to be imported in other ways for example by inorganic fertilizer

      N_manure_missing <- N_manure_after_biogas_old - N_manure_after_biogas_new - export_manure_N_kg
      P_manure_missing <- P_manure_after_biogas_old - P_manure_after_biogas_new - export_manure_P_kg
      K_manure_missing <- K_manure_after_biogas_old - K_manure_after_biogas_new - export_manure_K_kg
      

      # import the reduction in manure available to crops in form of inorganic fertilizer
      crop_output$imported_inorganic_N <- crop_output$imported_inorganic_N + N_manure_missing
      crop_output$imported_inorganic_P <- crop_output$imported_inorganic_P + P_manure_missing
      crop_output$imported_inorganic_K <- crop_output$imported_inorganic_K + K_manure_missing

      # inorganic fertilizers have less nutrient losses when applied than organic ones
      # have a reduction factor for environmental losses
    }


    #-----------#
    # BIOGAS ====
    #-----------#

    ## manure going to biogas ----
    N_biogas_input_animal <- N_biogas_input * share_N_biogas_input_animal * rf_manure_biogas_scenario
    P_biogas_input_animal <- P_biogas_input * share_P_biogas_input_animal * rf_manure_biogas_scenario
    K_biogas_input_animal <- K_biogas_input * share_K_biogas_input_animal * rf_manure_biogas_scenario

    # avoid that reduced manure input in local feed scenario is substituted with crops
    # keeo track of how much less is put in, under local feed scenario
    N_biogas_input_animal_not_used_scen <- N_biogas_input * share_N_biogas_input_animal * (1 - rf_manure_biogas_scenario)
    P_biogas_input_animal_not_used_scen <- P_biogas_input * share_P_biogas_input_animal * (1 - rf_manure_biogas_scenario)
    K_biogas_input_animal_not_used_scen <- K_biogas_input * share_K_biogas_input_animal * (1 - rf_manure_biogas_scenario)

    # additional vegetal biomass going to biogas
    # take from crops to humans by processing

    # N_crop_biogas contains so far only maize, but there are also other crops like sugar beet used
    # so the gap of animal based input and crop(actually maize based) input is then taken again from N_crop
    N_biogas_crop_missing <- N_biogas_input - (crop_output$N_crop_biogas + N_biogas_input_animal + N_biogas_input_animal_not_used_scen)
    P_biogas_crop_missing <- P_biogas_input - (crop_output$P_crop_biogas + P_biogas_input_animal + P_biogas_input_animal_not_used_scen)
    K_biogas_crop_missing <- K_biogas_input - (crop_output$K_crop_biogas + K_biogas_input_animal + K_biogas_input_animal_not_used_scen)

    # take missing input from the crops for human consumption
    crop_output$N_crop_human_consumption_processed <- crop_output$N_crop_human_consumption_processed - N_biogas_crop_missing
    crop_output$P_crop_human_consumption_processed <- crop_output$P_crop_human_consumption_processed - P_biogas_crop_missing
    crop_output$K_crop_human_consumption_processed <- crop_output$K_crop_human_consumption_processed - K_biogas_crop_missing


    # alternatively take original share of original animal to crop input for biogas and scale it down accordingly


    # update the crop biogas iput (because in the crop out it was sofar only maize)
    crop_output$N_crop_biogas <- N_biogas_input - N_biogas_input_animal
    crop_output$P_crop_biogas <- P_biogas_input - P_biogas_input_animal
    crop_output$K_crop_biogas <- K_biogas_input - K_biogas_input_animal

    
    if(scenario == "local_feed"){
      ## manure going to crops after manure was sent to biogas and exported----
      N_manure_to_crop <- purrr::map_dbl(animal_output, 'N_remaining_manure') - N_biogas_input_animal 
      P_manure_to_crop <- purrr::map_dbl(animal_output, 'P_remaining_manure') - P_biogas_input_animal 
      K_manure_to_crop <- purrr::map_dbl(animal_output, 'K_remaining_manure') - K_biogas_input_animal 
      
    } else {
      N_manure_to_crop <- animal_output$N_remaining_manure - N_biogas_input_animal 
      P_manure_to_crop <- animal_output$P_remaining_manure - P_biogas_input_animal 
      K_manure_to_crop <- animal_output$K_remaining_manure - K_biogas_input_animal 
      
    }



    # biogas output

    # biomass digestate (take input of animal and crop, process it for waste subsystem)
    lf_area <- arable_land + area_grassland

    # get total kwel by biogas (based on the current amount of agricultural land), convestion factor is percentage, thus devide by 100
    total_kwel <- lf_area * lf_to_kwel / 100
    volume_digestate <- total_kwel * Kwel_to_digestate
    mass_digestate <- volume_digestate * digestate_density / 1000 # in tons. thus devide by 1000

    # rf_manure_biogas_scenario is used to decide how much less manure is used in biogas
    # the rest of inputs is kept constant


    N_digestate <- mass_digestate * digestate_N_content * rf_manure_biogas_scenario # result is kg
    P_digestate <- mass_digestate * digestate_P_content * rf_manure_biogas_scenario # result is kg
    K_digestate <- mass_digestate * digestate_K_content * rf_manure_biogas_scenario # result is kg



    #--------------------#
    # WASTE SUBSYSTEM ====
    #--------------------#

    waste_output <- waste_function(
      waste_water,
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
      K_content_green_waste,
      lossrate_N_wastewater,
      lossrate_P_wastewater,
      lossrate_K_wastewater,
      convert_potassium_oxide_to_k
    )




    #----------------------#
    # FERTILIZER LOSSES ####
    #----------------------#

    # calculate fertilization losses as a difference between crop input (inorganic fertilizer, organic fertilizer,
    # digestate, sewage) minus output (vegetal products, feed, other organic fertilizer exported from field)

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

    # flag to decide which fertilization losses calculation is used
    # use input_output fertilization losses
    use_input_output_fertilization_losses <- TRUE
    
    #this needs reviews
    
    if (use_input_output_fertilization_losses) {
      crop_output$inevitable_N_losses <- N_fertilization_losses_blackbox
      crop_output$inevitable_P_losses <- P_fertilization_losses_blackbox
      crop_output$inevitable_K_losses <- K_fertilization_losses_blackbox
    }






    #--------------------------#
    # CONSUMPTION SUBSYSTEM ####
    #--------------------------#

    consumption_output <- calc_consume(
      population = population,
      consume_beef = consume_beef, 
      N_content_beef = N_content_beef,
      consume_beer = consume_beer,
      N_content_beer =  N_content_beer,
      consume_butter = consume_butter,
      N_content_butter =  N_content_butter,
      consume_cheese = consume_cheese, 
      N_content_cheese =  N_content_cheese,
      consume_citrus_fruits = consume_citrus_fruits,
      N_content_citrus_fruits =  N_content_citrus_fruits,
      consume_cocoa =  consume_cocoa,
      N_content_cocoa =  N_content_cocoa,
      consume_condensed_milk =  consume_condensed_milk,
      N_content_condensed_milk =  N_content_condensed_milk,
      consume_cream =  consume_cream,
      N_content_cream =  N_content_cream,
      consume_dried_fruit =  consume_dried_fruit,
      N_content_dried_fruit =  N_content_dried_fruit,
      consume_egg =  consume_egg,
      N_content_egg =  N_content_egg,
      consume_fish =  consume_fish,
      N_content_fish =  N_content_fish,
      consume_honey =  consume_honey,
      N_content_honey =  N_content_honey,
      consume_legumes =  consume_legumes,
      N_content_legumes =  N_content_legumes,
      consume_margarine =  consume_margarine,
      N_content_margarine =  N_content_margarine,
      consume_milk =  consume_milk,
      N_content_milk =  N_content_milk,
      consume_nuts =  consume_nuts,
      N_content_nuts =  N_content_nuts,
      consume_offal =  consume_offal,
      N_content_offal =  N_content_offal,
      consume_other_meat =  consume_other_meat,
      N_content_other_meat =  N_content_other_meat,
      consume_pork =  consume_pork,
      N_content_pork =  N_content_pork,
      consume_potato = consume_potato,
      N_content_potato = N_content_potato,
      consume_potato_starch = consume_potato_starch,
      N_content_potato_starch = N_content_potato_starch,
      consume_poultry = consume_poultry, 
      N_content_poultry_meat = N_content_poultry_meat,
      consume_rice = consume_rice,
      N_content_rice =  N_content_rice,
      consume_rye = consume_rye,
      N_content_rye =  N_content_rye,
      consume_sheep = consume_sheep,
      N_content_sheep_meat =  N_content_sheep_meat,
      consume_sugar = consume_sugar,
      N_content_sugar =  N_content_sugar,
      consume_tree_fruits = consume_tree_fruits,
      N_content_tree_fruits =  N_content_tree_fruits,
      consume_vegetables = consume_vegetables,
      N_content_vegetables =  N_content_vegetables,
      consume_wheat = consume_wheat,
      N_content_wheat =  N_content_wheat,
      consume_coffee = consume_coffee,
      N_content_coffee =  N_content_coffee,
      convert_coffee =  convert_coffee,
      consume_black_tea = consume_black_tea,
      N_content_black_tea =  N_content_black_tea,
      convert_black_tea =  convert_black_tea,
      consume_herb_tea = consume_herb_tea,
      N_content_herb_tea =  N_content_herb_tea,
      convert_herb_tea =  convert_herb_tea,
      consume_sparkling_wine = consume_sparkling_wine,
      N_content_sparkling_wine =  N_content_sparkling_wine,
      P_content_beef =   P_content_beef,
      P_content_beer =  P_content_beer,
      P_content_butter =  P_content_butter,
      P_content_cheese =  P_content_cheese,
      P_content_citrus_fruits =  P_content_citrus_fruits,
      P_content_cocoa = P_content_cocoa,
      P_content_condensed_milk =  P_content_condensed_milk,
      P_content_cream =  P_content_cream,
      P_content_dried_fruit =  P_content_dried_fruit,
      P_content_egg =  P_content_egg,
      P_content_fish =  P_content_fish,
      P_content_honey =  P_content_honey,
      P_content_legumes =  P_content_legumes,
      P_content_margarine =  P_content_margarine, 
      P_content_milk = P_content_milk,
      P_content_nuts =  P_content_nuts,
      P_content_offal =  P_content_offal,
      P_content_other_meat =  P_content_other_meat,
      P_content_pork =  P_content_pork,
      P_content_potato = P_content_potato,
      P_content_potato_starch =  P_content_potato_starch,
      P_content_poultry_meat = P_content_poultry_meat, 
      P_content_rice = P_content_rice,
      P_content_rye = P_content_rye,
      P_content_sheep_meat =  P_content_sheep_meat,
      P_content_sugar = P_content_sugar,
      P_content_tree_fruits =  P_content_tree_fruits,
      P_content_vegetables = P_content_vegetables,
      P_content_wheat = P_content_wheat,
      P_content_coffee = P_content_coffee,
      P_content_black_tea =  P_content_black_tea,
      P_content_herb_tea = P_content_herb_tea,
      P_content_sparkling_wine =  P_content_sparkling_wine,
      K_content_beef =   K_content_beef, 
      K_content_beer = K_content_beer,
      K_content_butter = K_content_butter,
      K_content_cheese = K_content_cheese,
      K_content_citrus_fruits =  K_content_citrus_fruits,
      K_content_cocoa = K_content_cocoa,
      K_content_condensed_milk =  K_content_condensed_milk,
      K_content_cream = K_content_cream,
      K_content_dried_fruit =  K_content_dried_fruit,
      K_content_egg = K_content_egg,
      K_content_fish =  K_content_fish,
      K_content_honey = K_content_honey,
      K_content_legumes =  K_content_legumes,
      K_content_margarine = K_content_margarine,
      K_content_milk =  K_content_milk,
      K_content_nuts = K_content_nuts,
      K_content_offal =  K_content_offal,
      K_content_other_meat = K_content_other_meat,
      K_content_pork =  K_content_pork,
      K_content_potato = K_content_potato,
      K_content_potato_starch =  K_content_potato_starch,
      K_content_poultry_meat = K_content_poultry_meat,
      K_content_rice =  K_content_rice,
      K_content_rye = K_content_rye,
      K_content_sheep_meat =  K_content_sheep_meat,
      K_content_sugar = K_content_sugar,
      K_content_tree_fruits =  K_content_tree_fruits,
      K_content_vegetables = K_content_vegetables,
      K_content_wheat = K_content_wheat,
      K_content_coffee = K_content_coffee,
      K_content_black_tea =  K_content_black_tea,
      K_content_herb_tea = K_content_herb_tea, 
      K_content_sparkling_wine = K_content_sparkling_wine
    )




    #-----------------#
    ## waste water ####
    #-----------------#


    # wastewater from consumption that doesnt reach the waste subsystem (remains in canal or direct discharge)
    # needs to be calculated here, because not part of the waste subsystem, but variable defined already in waste input file
    N_wastewater_direct_discharge <- (wastewater_direct_discharge + wastewater_remain_canal) * 1000 * N_content_wastewater / 1000000
    P_wastewater_direct_discharge <- (wastewater_direct_discharge + wastewater_remain_canal) * 1000 * P_content_wastewater / 1000000
    K_wastewater_direct_discharge <- (wastewater_direct_discharge + wastewater_remain_canal) * 1000 * K_content_wastewater / 1000000




    #-------------------------------------#
    ## import / export for consumption ####
    #-------------------------------------#


    # calculate degree of self-sufficiency for meat, egg and milk production
    # animal_output$N_egg_available / consumption_output$consumed_N_egg
    # animal_output$N_milk_available / consumption_output$consumed_N_dairy
    # animal_output$N_meat_local_to_consumption / consumption_output$consumed_N_meat
    # production of crops still not done, cant calculate the sufficiency ratio yet
    get_scenario_import <- function(consumed, produced_list, var){
      
      #check if produced list contains further lists, 
      if(is.list(produced_list[[1]])){
        purrr::map_dbl(consumed - purrr::map_dbl(produced_list, var), function(x) max(x,0))
      } else{
        max(consumed - produced_list[[var]], 0)
      }

      
    }
    
    
    
    
    # imoport is consumption - local production, prevent that lower as zero
    
    
    N_egg_import <- get_scenario_import(consumption_output$consumed_N_egg, animal_output, 'N_egg_available')
    P_egg_import <- get_scenario_import(consumption_output$consumed_N_egg, animal_output, 'P_egg_available')
    K_egg_import <- get_scenario_import(consumption_output$consumed_N_egg, animal_output, 'K_egg_available')
    
    # N_egg_import <- max(consumption_output$consumed_N_egg - animal_output$N_egg_available, 0)
    # P_egg_import <- max(consumption_output$consumed_P_egg - animal_output$P_egg_available, 0)
    # K_egg_import <- max(consumption_output$consumed_K_egg - animal_output$K_egg_available, 0)

    N_meat_import <- get_scenario_import(consumption_output$consumed_N_meat, animal_output, 'N_meat_local_to_consumption')
    P_meat_import <- get_scenario_import(consumption_output$consumed_P_meat, animal_output, 'P_meat_local_to_consumption')
    K_meat_import <- get_scenario_import(consumption_output$consumed_K_meat, animal_output, 'K_meat_local_to_consumption')
    
    # N_meat_import <- max(consumption_output$consumed_N_meat - animal_output$N_meat_local_to_consumption, 0)
    # P_meat_import <- max(consumption_output$consumed_P_meat - animal_output$P_meat_local_to_consumption, 0)
    # K_meat_import <- max(consumption_output$consumed_K_meat - animal_output$K_meat_local_to_consumption, 0)

    N_dairy_import <- get_scenario_import(consumption_output$consumed_N_dairy, animal_output, 'N_milk_available')
    P_dairy_import <- get_scenario_import(consumption_output$consumed_P_dairy, animal_output, 'P_milk_available')
    K_dairy_import <- get_scenario_import(consumption_output$consumed_K_dairy, animal_output, 'K_milk_available')
    
    # N_dairy_import <- max(consumption_output$consumed_N_dairy - animal_output$N_milk_available, 0)
    # P_dairy_import <- max(consumption_output$consumed_P_dairy - animal_output$P_milk_available, 0)
    # K_dairy_import <- max(consumption_output$consumed_K_dairy - animal_output$K_milk_available, 0)

    
    N_vegetable_import <- max(consumption_output$consumed_N_vegetable - crop_output$N_crop_human_consumption_processed, 0) + consumption_output$consumed_N_foreign_vegetable
    P_vegetable_import <- max(consumption_output$consumed_P_vegetable - crop_output$P_crop_human_consumption_processed, 0) + consumption_output$consumed_P_foreign_vegetable
    K_vegetable_import <- max(consumption_output$consumed_K_vegetable - crop_output$K_crop_human_consumption_processed, 0) + consumption_output$consumed_K_foreign_vegetable

    N_total_food_import <- N_egg_import + N_dairy_import + N_meat_import + N_vegetable_import
    P_total_food_import <- P_egg_import + P_dairy_import + P_meat_import + P_vegetable_import
    K_total_food_import <- K_egg_import + K_dairy_import + K_meat_import + K_vegetable_import


    get_scenario_export <- function(consumed, produced_list, var){
      
      #check if produced list contains further lists, 
      if(is.list(produced_list[[1]])){
        purrr::map_dbl(purrr::map_dbl(produced_list, var) - consumed, function(x) max(x,0))
      } else{
        max(produced_list[[var]] - consumed, 0)
      }
    }

    # food exports (prevent negative exports if consumption exceeds the production)
    N_egg_export <- get_scenario_export(consumption_output$consumed_N_egg, animal_output, 'N_egg_available')
    P_egg_export <- get_scenario_export(consumption_output$consumed_N_egg, animal_output, 'P_egg_available')
    K_egg_export <- get_scenario_export(consumption_output$consumed_N_egg, animal_output, 'K_egg_available')
    
    # N_egg_export <- max(animal_output$N_egg_available - consumption_output$consumed_N_egg, 0)
    # P_egg_export <- max(animal_output$P_egg_available - consumption_output$consumed_P_egg, 0)
    # K_egg_export <- max(animal_output$K_egg_available - consumption_output$consumed_K_egg, 0)

    
    N_meat_export <- get_scenario_export(consumption_output$consumed_N_meat, animal_output, 'N_meat_local_to_consumption')
    P_meat_export <- get_scenario_export(consumption_output$consumed_P_meat, animal_output, 'P_meat_local_to_consumption')
    K_meat_export <- get_scenario_export(consumption_output$consumed_K_meat, animal_output, 'K_meat_local_to_consumption')
    
    # N_meat_export <- max(animal_output$N_meat_local_to_consumption - consumption_output$consumed_N_meat, 0)
    # P_meat_export <- max(animal_output$P_meat_local_to_consumption - consumption_output$consumed_P_meat, 0)
    # K_meat_export <- max(animal_output$K_meat_local_to_consumption - consumption_output$consumed_K_meat, 0)

    N_dairy_export <- get_scenario_export(consumption_output$consumed_N_dairy, animal_output, 'N_milk_available')
    P_dairy_export <- get_scenario_export(consumption_output$consumed_P_dairy, animal_output, 'P_milk_available')
    K_dairy_export <- get_scenario_export(consumption_output$consumed_K_dairy, animal_output, 'K_milk_available')
    
    # N_dairy_export <- max(animal_output$N_milk_available - consumption_output$consumed_N_dairy, 0)
    # P_dairy_export <- max(animal_output$P_milk_available - consumption_output$consumed_P_dairy, 0)
    # K_dairy_export <- max(animal_output$K_milk_available - consumption_output$consumed_K_dairy, 0)

    N_vegetable_export <- max(crop_output$N_crop_human_consumption_processed - consumption_output$consumed_N_vegetable, 0)
    P_vegetable_export <- max(crop_output$P_crop_human_consumption_processed - consumption_output$consumed_P_vegetable, 0)
    K_vegetable_export <- max(crop_output$K_crop_human_consumption_processed - consumption_output$consumed_K_vegetable, 0)

    
     
    
    # get amount of local products which also get consumed locally
    if(scenario == 'local_feed'){
      N_local_animal_products_consumed <- (purrr::map_dbl(animal_output, 'N_egg_available') - N_egg_export) +
        (purrr::map_dbl(animal_output, 'N_meat_local_to_consumption') - N_meat_export) +
        (purrr::map_dbl(animal_output, 'N_milk_available') - N_dairy_export)
      P_local_animal_products_consumed <- (purrr::map_dbl(animal_output, 'P_egg_available') - P_egg_export) +
        (purrr::map_dbl(animal_output, 'P_meat_local_to_consumption') - P_meat_export) +
        (purrr::map_dbl(animal_output, 'P_milk_available') - P_dairy_export)
      K_local_animal_products_consumed <- (purrr::map_dbl(animal_output, 'K_egg_available') - K_egg_export) +
        (purrr::map_dbl(animal_output, 'K_meat_local_to_consumption') - K_meat_export) +
        (purrr::map_dbl(animal_output, 'K_milk_available') - K_dairy_export)
    } else{
      N_local_animal_products_consumed <- (animal_output$N_egg_available - N_egg_export) +
        (animal_output$N_meat_local_to_consumption - N_meat_export) +
        (animal_output$N_milk_available - N_dairy_export)
      P_local_animal_products_consumed <- (animal_output$P_egg_available - P_egg_export) +
        (animal_output$P_meat_local_to_consumption - P_meat_export) +
        (animal_output$K_milk_available - P_dairy_export)
      K_local_animal_products_consumed <- (animal_output$K_egg_available - K_egg_export) +
        (animal_output$K_meat_local_to_consumption - K_meat_export) +
        (animal_output$K_milk_available - K_dairy_export)
    }


    N_local_vegetal_products_consumed <- crop_output$N_crop_human_consumption_processed - N_vegetable_export
    P_local_vegetal_products_consumed <- crop_output$P_crop_human_consumption_processed - P_vegetable_export
    K_local_vegetal_products_consumed <- crop_output$K_crop_human_consumption_processed - K_vegetable_export


    # export of vegetal products is still missing

    # combine import and export to list
    import_export <- list(
      N_egg_import = N_egg_import,
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
      K_vegetable_export = K_vegetable_export
    )


    # N BALANCE----
    N_animal_in <- N_feed_import + N_animal_local_input
    P_animal_in <- P_feed_import + P_animal_local_input
    K_animal_in <- K_feed_import + K_animal_local_input

    N_animal_out <- N_animal_output_produced
    P_animal_out <- P_animal_output_produced
    K_animal_out <- K_animal_output_produced

    N_animal_balance <- N_animal_in - N_animal_out
    P_animal_balance <- P_animal_in - P_animal_out
    K_animal_balance <- K_animal_in - K_animal_out



    # extract the flows shown in Bernous model and return those
    # give them same name as in the chart
    # also I need to return the correct area of crops and horticultural products, because the original input got adjusted in the submodel
    combined_output <- list(
      scenario = c(combined_output$scenario, scenario),
      sewage_N = c(combined_output$sewage_N, waste_output$N_sewage_in),
      sewage_P = c(combined_output$sewage_P, waste_output$P_sewage_in),
      sewage_K = c(combined_output$sewage_K, waste_output$K_sewage_in),
      ofmsw_residual_waste_N = c(combined_output$ofmsw_residual_waste_N, waste_output$N_grey_bin_food_waste + waste_output$N_grey_bin_garden_waste),
      ofmsw_residual_waste_P = c(combined_output$ofmsw_residual_waste_P, waste_output$P_grey_bin_food_waste + waste_output$P_grey_bin_garden_waste),
      ofmsw_residual_waste_K = c(combined_output$ofmsw_residual_waste_K, waste_output$K_grey_bin_food_waste + waste_output$K_grey_bin_garden_waste),
      ofmsw_N = c(combined_output$ofmsw_N, waste_output$N_ofmsw_local + waste_output$N_green_waste_local),
      ofmsw_P = c(combined_output$ofmsw_P, waste_output$P_ofmsw_local + waste_output$P_green_waste_local),
      ofmsw_K = c(combined_output$ofmsw_K, waste_output$K_ofmsw_local + waste_output$K_green_waste_local),
      wastewater_direct_discharge_N = c(combined_output$wastewater_direct_discharge_N, N_wastewater_direct_discharge),
      wastewater_direct_discharge_P = c(combined_output$wastewater_direct_discharge_P, P_wastewater_direct_discharge),
      wastewater_direct_discharge_K = c(combined_output$wastewater_direct_discharge_K, K_wastewater_direct_discharge),
      compost_to_consumption_N = c(combined_output$compost_to_consumption_N, waste_output$N_compost_consumption),
      compost_to_consumption_P = c(combined_output$compost_to_consumption_P, waste_output$P_compost_consumption),
      compost_to_consumption_K = c(combined_output$compost_to_consumption_K, waste_output$K_compost_consumption),
      digestate_N = c(combined_output$digestate_N, N_digestate),
      digestate_P = c(combined_output$digestate_P, P_digestate),
      digestate_K = c(combined_output$digestate_K, K_digestate),
      sewage_sludge_export_N = c(combined_output$sewage_sludge_export_N, waste_output$N_sewage_exported),
      sewage_sludge_export_P = c(combined_output$sewage_sludge_export_P, waste_output$P_sewage_exported),
      sewage_sludge_export_K = c(combined_output$sewage_sludge_export_K, waste_output$K_sewage_exported),
      wastewater_effluent_gaseous_losses_N = c(combined_output$wastewater_effluent_gaseous_losses_N, waste_output$N_sewage_lost),
      wastewater_effluent_gaseous_losses_P = c(combined_output$wastewater_effluent_gaseous_losses_P, waste_output$P_sewage_lost),
      wastewater_effluent_gaseous_losses_K = c(combined_output$wastewater_effluent_gaseous_losses_K, waste_output$K_sewage_lost),
      fresh_compost_export_N = c(combined_output$fresh_compost_export_N, waste_output$N_compost_export),
      fresh_compost_export_P = c(combined_output$fresh_compost_export_P, waste_output$P_compost_export),
      fresh_compost_export_K = c(combined_output$fresh_compost_export_K, waste_output$K_compost_export),
      fresh_compost_crop_N = c(combined_output$fresh_compost_crop_N, waste_output$N_compost_crop),
      fresh_compost_crop_P = c(combined_output$fresh_compost_crop_P, waste_output$P_compost_crop),
      fresh_compost_crop_K = c(combined_output$fresh_compost_crop_K, waste_output$K_compost_crop),
      sewage_to_crop_N = c(combined_output$sewage_to_crop_N, waste_output$N_sewage_to_crop),
      sewage_to_crop_P = c(combined_output$sewage_to_crop_P, waste_output$P_sewage_to_crop),
      sewage_to_crop_K = c(combined_output$sewage_to_crop_K, waste_output$K_sewage_to_crop),
      vegetal_biogas_substrate_N = c(combined_output$vegetal_biogas_substrate_N, crop_output$N_crop_biogas),
      vegetal_biogas_substrate_P = c(combined_output$vegetal_biogas_substrate_P, crop_output$P_crop_biogas),
      vegetal_biogas_substrate_K = c(combined_output$vegetal_biogas_substrate_K, crop_output$K_crop_biogas),
      crop_cultivation_losses_N = c(combined_output$crop_cultivation_losses_N, crop_output$inevitable_N_losses),
      crop_cultivation_losses_P = c(combined_output$crop_cultivation_losses_P, crop_output$inevitable_P_losses),
      crop_cultivation_losses_K = c(combined_output$crop_cultivation_losses_K, crop_output$inevitable_K_losses),
      other_organic_fertilizer_export_N = c(combined_output$other_organic_fertilizer_export_N, export_other_organic_N_kg),
      other_organic_fertilizer_export_P = c(combined_output$other_organic_fertilizer_export_P, export_other_organic_P_kg),
      other_organic_fertilizer_export_K = c(combined_output$other_organic_fertilizer_export_K, export_other_organic_K_kg),
      straw_N = c(combined_output$straw_N, crop_output$N_straw),
      straw_P = c(combined_output$straw_P, crop_output$P_straw),
      straw_K = c(combined_output$straw_K, crop_output$K_straw),
      feed_crops_N = c(combined_output$feed_crops_N, crop_output$N_crop_animal_feeding_unprocessed),
      feed_crops_P = c(combined_output$feed_crops_P, crop_output$P_crop_animal_feeding_unprocessed),
      feed_crops_K = c(combined_output$feed_crops_K, crop_output$K_crop_animal_feeding_unprocessed),
      grassbased_feed_N = c(combined_output$grassbased_feed_N, crop_output$N_grassland),
      grassbased_feed_P = c(combined_output$grassbased_feed_P, crop_output$P_grassland),
      grassbased_feed_K = c(combined_output$grassbased_feed_K, crop_output$K_grassland),
      fruit_and_vegetable_N = c(combined_output$fruit_and_vegetable_N, crop_output$total_N_horticulture),
      fruit_and_vegetable_P = c(combined_output$fruit_and_vegetable_P, crop_output$total_P_horticulture),
      fruit_and_vegetable_K = c(combined_output$fruit_and_vegetable_K, crop_output$total_K_horticulture),
      food_and_feed_crops_N = c(combined_output$food_and_feed_crops_N, crop_output$N_crop_animal_feeding_processed + crop_output$N_crop_human_consumption_processed),
      food_and_feed_crops_P = c(combined_output$food_and_feed_crops_P, crop_output$P_crop_animal_feeding_processed + crop_output$P_crop_human_consumption_processed),
      food_and_feed_crops_K = c(combined_output$food_and_feed_crops_K, crop_output$K_crop_animal_feeding_processed + crop_output$K_crop_human_consumption_processed),
      manure_as_biogas_substrate_N = c(combined_output$manure_as_biogas_substrate_N, N_biogas_input_animal),
      manure_as_biogas_substrate_P = c(combined_output$manure_as_biogas_substrate_P, P_biogas_input_animal),
      manure_as_biogas_substrate_K = c(combined_output$manure_as_biogas_substrate_K, K_biogas_input_animal),
      manure_to_crop_N = c(combined_output$manure_to_crop_N, N_manure_to_crop),
      manure_to_crop_P = c(combined_output$manure_to_crop_P, P_manure_to_crop),
      manure_to_crop_K = c(combined_output$manure_to_crop_K, K_manure_to_crop),
      manure_export_N = c(combined_output$manure_export_N, export_manure_N_kg),
      manure_export_P = c(combined_output$manure_export_P, export_manure_P_kg),
      manure_export_K = c(combined_output$manure_export_K, export_manure_K_kg),
      animal_housing_and_storage_losses_N = c(combined_output$animal_housing_and_storage_losses_N, animal_output$N_housing_loss),
      animal_housing_and_storage_losses_P = c(combined_output$animal_housing_and_storage_losses_P, animal_output$P_housing_loss),
      animal_housing_and_storage_losses_K = c(combined_output$animal_housing_and_storage_losses_K, animal_output$K_housing_loss),
      slaughter_animal_N = c(combined_output$slaughter_animal_N, animal_output$N_to_slaughter),
      slaughter_animal_P = c(combined_output$slaughter_animal_P, animal_output$P_to_slaughter),
      slaughter_animal_K = c(combined_output$slaughter_animal_K, animal_output$K_to_slaughter),
      egg_and_dairy_N = c(combined_output$egg_and_dairy_N, animal_output$N_milk_available + animal_output$N_egg_available),
      egg_and_dairy_P = c(combined_output$egg_and_dairy_P, animal_output$P_milk_available + animal_output$P_egg_available),
      egg_and_dairy_K = c(combined_output$egg_and_dairy_K, animal_output$K_milk_available + animal_output$K_egg_available),
      local_vegetal_products_consumed_N = c(combined_output$local_vegetal_products_consumed_N, N_local_vegetal_products_consumed),
      local_vegetal_products_consumed_P = c(combined_output$local_vegetal_products_consumed_P, P_local_vegetal_products_consumed),
      local_vegetal_products_consumed_K = c(combined_output$local_vegetal_products_consumed_K, K_local_vegetal_products_consumed),
      export_vegetable_N = c(combined_output$export_vegetable_N, N_vegetable_export),
      export_vegetable_P = c(combined_output$export_vegetable_P, P_vegetable_export),
      export_vegetable_K = c(combined_output$export_vegetable_K, K_vegetable_export),
      imported_animal_products_N = c(combined_output$imported_animal_products_N, N_meat_import + N_egg_import + N_dairy_import),
      imported_animal_products_P = c(combined_output$imported_animal_products_P, P_meat_import + P_egg_import + P_dairy_import),
      imported_animal_products_K = c(combined_output$imported_animal_products_K, K_meat_import + K_egg_import + K_dairy_import),
      imported_vegetal_products_N = c(combined_output$imported_vegetal_products_N, N_vegetable_import),
      imported_vegetal_products_P = c(combined_output$imported_vegetal_products_P, P_vegetable_import),
      imported_vegetal_products_K = c(combined_output$imported_vegetal_products_K, K_vegetable_import),
      feed_from_processed_crops_N = c(combined_output$feed_from_processed_crops_N, crop_output$N_crop_animal_feeding_processed),
      feed_from_processed_crops_P = c(combined_output$feed_from_processed_crops_P, crop_output$P_crop_animal_feeding_processed),
      feed_from_processed_crops_K = c(combined_output$feed_from_processed_crops_K, crop_output$K_crop_animal_feeding_processed),
      import_processed_feed_N = c(combined_output$import_processed_feed_N, N_feed_import),
      import_processed_feed_P = c(combined_output$import_processed_feed_P, P_feed_import),
      import_processed_feed_K = c(combined_output$import_processed_feed_K, K_feed_import),
      local_animal_products_consumed_N = c(combined_output$local_animal_products_consumed_N, N_local_animal_products_consumed),
      local_animal_products_consumed_P = c(combined_output$local_animal_products_consumed_P, P_local_animal_products_consumed),
      local_animal_products_consumed_K = c(combined_output$local_animal_products_consumed_K, K_local_animal_products_consumed),
      export_meat_N = c(combined_output$export_meat_N, N_meat_export),
      export_meat_P = c(combined_output$export_meat_P, P_meat_export),
      export_meat_K = c(combined_output$export_meat_K, K_meat_export),
      import_meat_N = c(combined_output$import_meat_N, N_meat_import),
      import_meat_P = c(combined_output$import_meat_P, P_meat_import),
      import_meat_K = c(combined_output$import_meat_K, K_meat_import),
      export_egg_N = c(combined_output$export_egg_N, N_egg_export),
      export_egg_P = c(combined_output$export_egg_P, P_egg_export),
      export_egg_K = c(combined_output$export_egg_K, K_egg_export),
      slaughter_waste_N = c(combined_output$slaughter_waste_N, animal_output$N_slaughter_waste),
      slaughter_waste_P = c(combined_output$slaughter_waste_P, animal_output$P_slaughter_waste),
      slaughter_waste_K = c(combined_output$slaughter_waste_K, animal_output$K_slaughter_waste),
      import_OFMSW_N = c(combined_output$import_OFMSW_N, waste_output$N_green_waste_import + waste_output$N_ofmsw_import),
      import_OFMSW_P = c(combined_output$import_OFMSW_P, waste_output$P_green_waste_import + waste_output$P_ofmsw_import),
      import_OFMSW_K = c(combined_output$import_OFMSW_K, waste_output$K_green_waste_import + waste_output$K_ofmsw_import),
      import_inorganic_fertilizer_N = c(combined_output$import_inorganic_fertilizer_N, crop_output$imported_inorganic_N),
      import_inorganic_fertilizer_P = c(combined_output$import_inorganic_fertilizer_P, crop_output$imported_inorganic_P),
      import_inorganic_fertilizer_K = c(combined_output$import_inorganic_fertilizer_K, crop_output$imported_inorganic_K),
      import_organic_fertilizer_N = c(combined_output$import_organic_fertilizer_N, import_organic_N_kg),
      import_organic_fertilizer_P = c(combined_output$import_organic_fertilizer_P, import_organic_P_kg),
      import_organic_fertilizer_K = c(combined_output$import_organic_fertilizer_K, import_organic_K_kg),
      net_food_import_N = c(combined_output$net_food_import_N, N_total_food_import),
      net_food_import_P = c(combined_output$net_food_import_P, P_total_food_import),
      net_food_import_K = c(combined_output$net_food_import_K, K_total_food_import),
      net_feed_import_N = c(combined_output$net_feed_import_N, N_feed_import),
      net_feed_import_P = c(combined_output$net_feed_import_P, P_feed_import),
      net_feed_import_K = c(combined_output$net_feed_import_K, K_feed_import),
      share_beans = c(combined_output$share_beans, crop_output$share_winter_wheat),
      share_corn = c(combined_output$share_corn, crop_output$share_corn),
      share_fodder_peas = c(combined_output$share_fodder_peas, crop_output$share_fodder_peas),
      share_mais_silage = c(combined_output$share_mais_silage, crop_output$share_mais_silage),
      share_oat = c(combined_output$share_oat, crop_output$share_oat),
      share_oilseed_rape = c(combined_output$share_oilseed_rape, crop_output$share_oilseed_rape),
      share_potato = c(combined_output$share_potato, crop_output$share_potato),
      share_rye = c(combined_output$share_rye, crop_output$share_rye),
      share_sugar_beet = c(combined_output$share_sugar_beet, crop_output$share_sugar_beet),
      share_summer_barley = c(combined_output$share_summer_barley, crop_output$share_summer_barley),
      share_summer_wheat = c(combined_output$share_summer_wheat, crop_output$share_summer_wheat),
      share_triticale = c(combined_output$share_triticale, crop_output$share_triticale),
      share_winter_barley = c(combined_output$share_winter_barley, crop_output$share_winter_barley),
      share_winter_wheat = c(combined_output$share_winter_wheat, crop_output$share_winter_wheat),
      land_apple_ha = c(combined_output$land_apple_ha, crop_output$land_apple_ha),
      land_arugula_ha = c(combined_output$land_arugula_ha, crop_output$land_arugula_ha),
      land_asparagus_ha = c(combined_output$land_asparagus_ha, crop_output$land_asparagus_ha),
      land_berries_ha = c(combined_output$land_berries_ha, crop_output$land_berries_ha),
      land_cabbage_ha = c(combined_output$land_cabbage_ha, crop_output$land_cabbage_ha),
      land_carrot_ha = c(combined_output$land_carrot_ha, crop_output$land_carrot_ha),
      land_celery_ha = c(combined_output$land_celery_ha, crop_output$land_celery_ha),
      land_green_bean_ha = c(combined_output$land_green_bean_ha, crop_output$land_green_bean_ha),
      land_lambs_lettuce_ha = c(combined_output$land_lambs_lettuce_ha, crop_output$land_lambs_lettuce_ha),
      land_lettuce_ha = c(combined_output$land_lettuce_ha, crop_output$land_lettuce_ha),
      land_onion_ha = c(combined_output$land_onion_ha, crop_output$land_onion_ha),
      land_parsley_ha = c(combined_output$land_pumpkin_ha, crop_output$land_parsley_ha),
      land_pumpkin_ha = c(combined_output$land_pumpkin_ha, crop_output$land_pumpkin_ha),
      land_radishes_ha = c(combined_output$land_radishes_ha, crop_output$land_radishes_ha),
      land_rhubarb_ha = c(combined_output$land_rhubarb_ha, crop_output$land_rhubarb_ha),
      land_spinash_ha = c(combined_output$land_spinash_ha, crop_output$land_spinash_ha),
      land_stone_fruit_ha = c(combined_output$land_stone_fruit_ha, crop_output$land_stone_fruit_ha),
      land_strawberry_ha = c(combined_output$land_strawberry_ha, crop_output$land_strawberry_ha),
      land_sweet_corn_ha = c(combined_output$land_sweet_corn_ha, crop_output$land_sweet_corn_ha),
      land_veggie_peas_ha = c(combined_output$land_veggie_peas_ha, crop_output$land_veggie_peas_ha)
    )
  } # end of the loop for the different scenarios



  # parameters to evaluate model output:
  #   one calculation for the two / three different scenarios
  #   that is why outside the loop

  # amount of nutrients imported to the system

  total_input_N <- combined_output$net_feed_import_N +
    combined_output$imported_vegetal_products_N +
    combined_output$import_inorganic_fertilizer_N +
    combined_output$import_organic_fertilizer_N +
    combined_output$import_OFMSW_N +
    combined_output$imported_animal_products_N

  total_input_P <- combined_output$net_feed_import_P +
    combined_output$imported_vegetal_products_P +
    combined_output$import_inorganic_fertilizer_P +
    combined_output$import_organic_fertilizer_P +
    combined_output$import_OFMSW_P +
    combined_output$imported_animal_products_P

  total_input_K <- combined_output$net_feed_import_K +
    combined_output$imported_vegetal_products_K +
    combined_output$import_inorganic_fertilizer_K +
    combined_output$import_organic_fertilizer_K +
    combined_output$import_OFMSW_K +
    combined_output$imported_animal_products_K


  # use efficiency
  #
  # percentage of all input (also locally sourced) into crop and animal leads
  # to products (manure not considered a product of animal production but as
  # input to crop production, vegetal biogas substrate and manure not seen as
  # product output from resp. crop and animal production, but digestate as an input into crop production)

  use_efficiency_N <- ((combined_output$feed_crops_N +
    combined_output$straw_N +
    combined_output$grassbased_feed_N +
    combined_output$food_and_feed_crops_N +
    combined_output$fruit_and_vegetable_N +
    combined_output$egg_and_dairy_N +
    combined_output$slaughter_animal_N) /
    (combined_output$manure_to_crop_N +
      combined_output$net_feed_import_N +
      combined_output$import_inorganic_fertilizer_N +
      combined_output$feed_crops_N +
      combined_output$grassbased_feed_N +
      combined_output$digestate_N +
      combined_output$import_organic_fertilizer_N +
      combined_output$feed_from_processed_crops_N +
      combined_output$fresh_compost_crop_N +
      combined_output$sewage_N +
      combined_output$straw_N)) * 100

  use_efficiency_P <- ((combined_output$feed_crops_P +
    combined_output$straw_P +
    combined_output$grassbased_feed_P +
    combined_output$food_and_feed_crops_P +
    combined_output$fruit_and_vegetable_P +
    combined_output$egg_and_dairy_P +
    combined_output$slaughter_animal_P) /
    (combined_output$manure_to_crop_P +
      combined_output$net_feed_import_P +
      combined_output$import_inorganic_fertilizer_P +
      combined_output$feed_crops_P +
      combined_output$grassbased_feed_P +
      combined_output$digestate_P +
      combined_output$import_organic_fertilizer_P +
      combined_output$feed_from_processed_crops_P +
      combined_output$fresh_compost_crop_P +
      combined_output$sewage_P +
      combined_output$straw_P)) * 100

  use_efficiency_K <- ((combined_output$feed_crops_K +
    combined_output$straw_K +
    combined_output$grassbased_feed_K +
    combined_output$food_and_feed_crops_K +
    combined_output$fruit_and_vegetable_K +
    combined_output$egg_and_dairy_K +
    combined_output$slaughter_animal_K) /
    (combined_output$manure_to_crop_K +
      combined_output$net_feed_import_K +
      combined_output$import_inorganic_fertilizer_K +
      combined_output$feed_crops_K +
      combined_output$grassbased_feed_K +
      combined_output$digestate_K +
      combined_output$import_organic_fertilizer_K +
      combined_output$feed_from_processed_crops_K +
      combined_output$fresh_compost_crop_K +
      combined_output$sewage_K +
      combined_output$straw_K)) * 100


  # share_reused_to_total_input
  #
  # share of local input into consumption, crop - and animal production to total input into consumption,
  # crop - and animal production (locally produced crops and grass is considered as reused as well as manure)

  share_reuse_to_total_input_N <- ((combined_output$manure_to_crop_N +
    combined_output$grassbased_feed_N +
    combined_output$feed_crops_N +
    combined_output$local_animal_products_consumed_N +
    combined_output$digestate_N +
    combined_output$feed_from_processed_crops_N +
    combined_output$local_vegetal_products_consumed_N +
    combined_output$fresh_compost_crop_N +
    combined_output$sewage_to_crop_N +
    combined_output$straw_N) /
    (combined_output$manure_to_crop_N +
      combined_output$grassbased_feed_N +
      combined_output$feed_crops_N +
      combined_output$local_animal_products_consumed_N +
      combined_output$digestate_N +
      combined_output$feed_from_processed_crops_N +
      combined_output$local_vegetal_products_consumed_N +
      combined_output$fresh_compost_crop_N +
      combined_output$sewage_to_crop_N +
      combined_output$straw_N +
      combined_output$net_feed_import_N +
      combined_output$import_inorganic_fertilizer_N +
      combined_output$imported_vegetal_products_N +
      combined_output$imported_animal_products_N +
      combined_output$import_organic_fertilizer_N)) * 100

  share_reuse_to_total_input_P <- ((combined_output$manure_to_crop_P +
    combined_output$grassbased_feed_P +
    combined_output$feed_crops_P +
    combined_output$local_animal_products_consumed_P +
    combined_output$digestate_P +
    combined_output$feed_from_processed_crops_P +
    combined_output$local_vegetal_products_consumed_P +
    combined_output$fresh_compost_crop_P +
    combined_output$sewage_to_crop_P +
    combined_output$straw_P) /
    (combined_output$manure_to_crop_P +
      combined_output$grassbased_feed_P +
      combined_output$feed_crops_P +
      combined_output$local_animal_products_consumed_P +
      combined_output$digestate_P +
      combined_output$feed_from_processed_crops_P +
      combined_output$local_vegetal_products_consumed_P +
      combined_output$fresh_compost_crop_P +
      combined_output$sewage_to_crop_P +
      combined_output$straw_P +
      combined_output$net_feed_import_P +
      combined_output$import_inorganic_fertilizer_P +
      combined_output$imported_vegetal_products_P +
      combined_output$imported_animal_products_P +
      combined_output$import_organic_fertilizer_P)) * 100

  share_reuse_to_total_input_K <- ((combined_output$manure_to_crop_K +
    combined_output$grassbased_feed_K +
    combined_output$feed_crops_K +
    combined_output$local_animal_products_consumed_K +
    combined_output$digestate_K +
    combined_output$feed_from_processed_crops_K +
    combined_output$local_vegetal_products_consumed_K +
    combined_output$fresh_compost_crop_K +
    combined_output$sewage_to_crop_K +
    combined_output$straw_K) /
    (combined_output$manure_to_crop_K +
      combined_output$grassbased_feed_K +
      combined_output$feed_crops_K +
      combined_output$local_animal_products_consumed_K +
      combined_output$digestate_K +
      combined_output$feed_from_processed_crops_K +
      combined_output$local_vegetal_products_consumed_K +
      combined_output$fresh_compost_crop_K +
      combined_output$sewage_to_crop_K +
      combined_output$straw_K +
      combined_output$net_feed_import_K +
      combined_output$import_inorganic_fertilizer_K +
      combined_output$imported_vegetal_products_K +
      combined_output$imported_animal_products_K +
      combined_output$import_organic_fertilizer_K)) * 100

  # recycling_rate
  #
  # how much of produced biomass (manure, compost, sewage sludge, organic fert., biogas substrate)
  # is input locally (manure, sewage sludge, compost, biogas substrate)
  # Effluent and gaseous losses during WwT are considered losses while direct discharge
  # and that what remains in canalisation are considered a potentially recyclable biomass

  recycling_rate_N <- ((combined_output$manure_to_crop_N +
    combined_output$digestate_N +
    combined_output$fresh_compost_crop_N +
    combined_output$sewage_to_crop_N) /
    (combined_output$manure_to_crop_N +
      combined_output$digestate_N +
      combined_output$fresh_compost_crop_N +
      combined_output$sewage_to_crop_N +
      combined_output$manure_export_N +
      combined_output$wastewater_direct_discharge_N +
      combined_output$slaughter_waste_N +
      combined_output$other_organic_fertilizer_export_N +
      combined_output$sewage_sludge_export_N +
      combined_output$fresh_compost_export_N)) * 100

  recycling_rate_P <- ((combined_output$manure_to_crop_P +
    combined_output$digestate_P +
    combined_output$fresh_compost_crop_P +
    combined_output$sewage_to_crop_P) /
    (combined_output$manure_to_crop_P +
      combined_output$digestate_P +
      combined_output$fresh_compost_crop_P +
      combined_output$sewage_to_crop_P +
      combined_output$manure_export_P +
      combined_output$wastewater_direct_discharge_P +
      combined_output$slaughter_waste_P +
      combined_output$other_organic_fertilizer_export_P +
      combined_output$sewage_sludge_export_P +
      combined_output$fresh_compost_export_P)) * 100

  recycling_rate_K <- ((combined_output$manure_to_crop_K +
    combined_output$digestate_K +
    combined_output$fresh_compost_crop_K +
    combined_output$sewage_to_crop_K) /
    (combined_output$manure_to_crop_K +
      combined_output$digestate_K +
      combined_output$fresh_compost_crop_K +
      combined_output$sewage_to_crop_K +
      combined_output$manure_export_K +
      combined_output$wastewater_direct_discharge_K +
      combined_output$slaughter_waste_K +
      combined_output$other_organic_fertilizer_export_K +
      combined_output$sewage_sludge_export_K +
      combined_output$fresh_compost_export_K)) * 100


  # losses
  #
  # all losses (leaching / cultivation losses, housing and storage losses, effluent / gaseous losses during SwT)
  # Effluent and gaseous losses during WwT are considered losses while direct discharge
  # and that what remains in canalisation are considered a potentially recyclable biomass

  losses_N <- combined_output$crop_cultivation_losses_N +
    combined_output$animal_housing_and_storage_losses_N +
    combined_output$wastewater_effluent_gaseous_losses_N

  losses_P <- combined_output$crop_cultivation_losses_P +
    combined_output$animal_housing_and_storage_losses_P +
    combined_output$wastewater_effluent_gaseous_losses_P

  losses_K <- combined_output$crop_cultivation_losses_K +
    combined_output$animal_housing_and_storage_losses_K +
    combined_output$wastewater_effluent_gaseous_losses_K



  # indicators, summarizing the flows
  model_evaluation <- list(
    scenario = c(model_evaluation$scenario, scenario),
    total_input_N = c(model_evaluation$total_input_N, total_input_N),
    total_input_P = c(model_evaluation$total_input_P, total_input_P),
    total_input_K = c(model_evaluation$total_input_K, total_input_K),
    use_efficiency_N = c(model_evaluation$use_efficiency_N, use_efficiency_N),
    use_efficiency_P = c(model_evaluation$use_efficiency_P, use_efficiency_P),
    use_efficiency_K = c(model_evaluation$use_efficiency_K, use_efficiency_K),
    share_reuse_to_total_input_N = c(model_evaluation$share_reuse_to_total_input_N, share_reuse_to_total_input_N),
    share_reuse_to_total_input_P = c(model_evaluation$share_reuse_to_total_input_P, share_reuse_to_total_input_P),
    share_reuse_to_total_input_K = c(model_evaluation$share_reuse_to_total_input_K, share_reuse_to_total_input_K),
    recycling_rate_N = c(model_evaluation$recycling_rate_N, recycling_rate_N),
    recycling_rate_P = c(model_evaluation$recycling_rate_P, recycling_rate_P),
    recycling_rate_K = c(model_evaluation$recycling_rate_K, recycling_rate_K),
    losses_N = c(model_evaluation$losses_N, losses_N),
    losses_P = c(model_evaluation$losses_P, losses_P),
    losses_K = c(model_evaluation$losses_K, losses_K)
  )


  if (return_flows) {
    return(combined_output)
  } else {
    return(model_evaluation)
  }
}

# decide what to return
return_flows <- FALSE

# let mc simulation run, just to test if everything works out
nitrogen_mc_simulation <- mcSimulation(
  estimate = as.estimate(input),
  model_function = combined_function,
  numberOfModelRuns = 10000,
  functionSyntax = "plainNames"
)


# somehow the resulting dataframe has one column per variable, but each variable twice:
# once for normal scenario and once for animal reduction scenario
#
#--> I extract the values and combine them to a new object
#--> this probably causes problems for pls analysis, because now it is not linked to the inputs anymore
#
# if(return_flows){
#   result_df <- data.frame(scenario = c(nitrogen_mc_simulation$y$scenario1, nitrogen_mc_simulation$y$scenario2),
#                           sewage_N = as.numeric(c(nitrogen_mc_simulation$y$sewage_N1, nitrogen_mc_simulation$y$sewage_N2)),
#                           ofmsw_residual_waste_N = as.numeric(c(nitrogen_mc_simulation$y$ofmsw_residual_waste_N1, nitrogen_mc_simulation$y$ofmsw_residual_waste_N2)),
#                           ofmsw_N = as.numeric(c(nitrogen_mc_simulation$y$ofmsw_N1, nitrogen_mc_simulation$y$ofmsw_N2)),
#                           wastewater_direct_discharge_N = as.numeric(c(nitrogen_mc_simulation$y$wastewater_direct_discharge_N1, nitrogen_mc_simulation$y$wastewater_direct_discharge_N2)),
#                           compost_to_consumption_N = as.numeric(c(nitrogen_mc_simulation$y$compost_to_consumption_N1, nitrogen_mc_simulation$y$compost_to_consumption_N2)),
#                           digestate_N = as.numeric(c(nitrogen_mc_simulation$y$digestate_N1, nitrogen_mc_simulation$y$digestate_N2)),
#                           sewage_sludge_export_N = as.numeric(c(nitrogen_mc_simulation$y$sewage_sludge_export_N1, nitrogen_mc_simulation$y$sewage_sludge_export_N2)),
#                           wastewater_effluent_gaseous_losses_N = as.numeric(c(nitrogen_mc_simulation$y$wastewater_effluent_gaseous_losses_N1, nitrogen_mc_simulation$y$wastewater_effluent_gaseous_losses_N2)),
#                           fresh_compost_export_N = as.numeric(c(nitrogen_mc_simulation$y$fresh_compost_export_N1, nitrogen_mc_simulation$y$fresh_compost_crop_N2)),
#                           fresh_compost_crop_N = as.numeric(c(nitrogen_mc_simulation$y$fresh_compost_crop_N1, nitrogen_mc_simulation$y$fresh_compost_crop_N2)),
#                           sewage_to_crop_N = as.numeric(c(nitrogen_mc_simulation$y$sewage_to_crop_N1, nitrogen_mc_simulation$y$sewage_N2)),
#                           vegetal_biogas_substrate_N = as.numeric(c(nitrogen_mc_simulation$y$vegetal_biogas_substrate_N1, nitrogen_mc_simulation$y$vegetal_biogas_substrate_N2)),
#                           crop_cultivation_losses_N = as.numeric(c(nitrogen_mc_simulation$y$crop_cultivation_losses_N1, nitrogen_mc_simulation$y$crop_cultivation_losses_N2)),
#                           other_organic_fertilizer_export_N = as.numeric(c(nitrogen_mc_simulation$y$other_organic_fertilizer_export_N1, nitrogen_mc_simulation$y$other_organic_fertilizer_export_N2)),
#                           straw_N = as.numeric(c(nitrogen_mc_simulation$y$straw_N1, nitrogen_mc_simulation$y$straw_N2)),
#                           feed_crops_N = as.numeric(c(nitrogen_mc_simulation$y$feed_crops_N1, nitrogen_mc_simulation$y$feed_crops_N2)),
#                           grassbased_feed_N = as.numeric(c(nitrogen_mc_simulation$y$grassbased_feed_N1, nitrogen_mc_simulation$y$grassbased_feed_N2)),
#                           fruit_and_vegetable_N = as.numeric(c(nitrogen_mc_simulation$y$fruit_and_vegetable_N1, nitrogen_mc_simulation$y$fruit_and_vegetable_N2)),
#                           manure_as_biogas_substrate_N = as.numeric(c(nitrogen_mc_simulation$y$manure_as_biogas_substrate_N1, nitrogen_mc_simulation$y$manure_as_biogas_substrate_N2)),
#                           manure_to_crop_N = as.numeric(c(nitrogen_mc_simulation$y$manure_to_crop_N1, nitrogen_mc_simulation$y$manure_to_crop_N2)),
#                           manure_export_N = as.numeric(c(nitrogen_mc_simulation$y$manure_export_N1, nitrogen_mc_simulation$y$manure_export_N2)),
#                           animal_housing_and_storage_losses_N = as.numeric(c(nitrogen_mc_simulation$y$animal_housing_and_storage_losses_N1, nitrogen_mc_simulation$y$animal_housing_and_storage_losses_N2)),
#                           slaughter_animal_N = as.numeric(c(nitrogen_mc_simulation$y$slaughter_animal_N1, nitrogen_mc_simulation$y$slaughter_animal_N2)),
#                           egg_and_dairy_N = as.numeric(c(nitrogen_mc_simulation$y$egg_and_dairy_N1, nitrogen_mc_simulation$y$egg_and_dairy_N2)),
#                           local_vegetal_products_consumed_N = as.numeric(c(nitrogen_mc_simulation$y$local_vegetal_products_consumed_N1, nitrogen_mc_simulation$y$local_vegetal_products_consumed_N2)),
#                           imported_animal_products_N = as.numeric(c(nitrogen_mc_simulation$y$imported_animal_products_N1, nitrogen_mc_simulation$y$imported_animal_products_N2)),
#                           imported_vegetal_products_N = as.numeric(c(nitrogen_mc_simulation$y$imported_vegetal_products_N1, nitrogen_mc_simulation$y$imported_vegetal_products_N2)),
#                           feed_from_processed_crops_N = as.numeric(c(nitrogen_mc_simulation$y$feed_from_processed_crops_N1, nitrogen_mc_simulation$y$feed_from_processed_crops_N2)),
#                           import_processed_feed_N = as.numeric(c(nitrogen_mc_simulation$y$import_processed_feed_N1, nitrogen_mc_simulation$y$import_processed_feed_N2)),
#                           local_animal_products_consumed_N = as.numeric(c(nitrogen_mc_simulation$y$local_animal_products_consumed_N1, nitrogen_mc_simulation$y$local_animal_products_consumed_N2)),
#                           export_meat_N = as.numeric(c(nitrogen_mc_simulation$y$export_meat_N1, nitrogen_mc_simulation$y$export_meat_N2)),
#                           import_meat_N = as.numeric(c(nitrogen_mc_simulation$y$import_meat_N1, nitrogen_mc_simulation$y$import_meat_N2)),
#                           export_egg_N = as.numeric(c(nitrogen_mc_simulation$y$export_egg_N1, nitrogen_mc_simulation$y$export_egg_N2)),
#                           slaughter_waste_N = as.numeric(c(nitrogen_mc_simulation$y$slaughter_waste_N1, nitrogen_mc_simulation$y$slaughter_waste_N2)),
#                           import_OFMSW_N = as.numeric(c(nitrogen_mc_simulation$y$import_OFMSW_N1, nitrogen_mc_simulation$y$import_OFMSW_N2)),
#                           import_inorganic_fertilizer_N = as.numeric(c(nitrogen_mc_simulation$y$import_inorganic_fertilizer_N1, nitrogen_mc_simulation$y$import_inorganic_fertilizer_N2)),
#                           import_organic_fertilizer_N = as.numeric(c(nitrogen_mc_simulation$y$import_organic_fertilizer_N1, nitrogen_mc_simulation$y$import_organic_fertilizer_N2)),
#                           net_food_import_N = as.numeric(c(nitrogen_mc_simulation$y$net_food_import_N1, nitrogen_mc_simulation$y$net_food_import_N2)),
#                           net_feed_import_N = as.numeric(c(nitrogen_mc_simulation$y$net_feed_import_N1, nitrogen_mc_simulation$y$net_feed_import_N2))
#   )
# } else {
#   result_df <- data.frame(scenario = c(nitrogen_mc_simulation$y$scenario1, nitrogen_mc_simulation$y$scenario2),
#                            self_supplied = c(nitrogen_mc_simulation$y$self_supplied1, nitrogen_mc_simulation$y$self_supplied2),
#                            external_input = c(nitrogen_mc_simulation$y$external_input1, nitrogen_mc_simulation$y$external_input2),
#                            system_output = c(nitrogen_mc_simulation$y$system_output1, nitrogen_mc_simulation$y$system_output2),
#                            system_losses =  c(nitrogen_mc_simulation$y$system_losses1, nitrogen_mc_simulation$y$system_losses2),
#                            SSE = c(nitrogen_mc_simulation$y$SSE1, nitrogen_mc_simulation$y$SSE2),
#                            supplied_by_outside = c(nitrogen_mc_simulation$y$supplied_by_outside1, nitrogen_mc_simulation$y$supplied_by_outside2),
#                            N_manure_to_crop = c(nitrogen_mc_simulation$y$N_manure_to_crop1, nitrogen_mc_simulation$y$N_manure_to_crop2))
# }

# nitrogen_mc_simulation$y <- result_df

# nitrogen_mc_simulation$x <- rbind(nitrogen_mc_simulation$x, nitrogen_mc_simulation$x)

# add scenario as input variable
# nitrogen_mc_simulation$x$scenario <- as.factor(nitrogen_mc_simulation$y$scenario)
#--> looks like PLS cant handle factors, so split it?


# should I use the PLS to analyse the outcomes of the scenario
# or should I use it to analyse the CHANGE in the variable

# somehow number were characters, so change back to numeric
nitrogen_mc_simulation$y[, 2:31] <- sapply(nitrogen_mc_simulation$y[, 2:31], as.numeric)


nitrogen_mc_simulation$y$total_input_N_change <- nitrogen_mc_simulation$y$total_input_N1 - nitrogen_mc_simulation$y$total_input_N2
nitrogen_mc_simulation$y$total_input_P_change <- nitrogen_mc_simulation$y$total_input_P1 - nitrogen_mc_simulation$y$total_input_P2
nitrogen_mc_simulation$y$total_input_K_change <- nitrogen_mc_simulation$y$total_input_K1 - nitrogen_mc_simulation$y$total_input_K2

nitrogen_mc_simulation$y$use_efficiency_N_change <- nitrogen_mc_simulation$y$use_efficiency_N1 - nitrogen_mc_simulation$y$use_efficiency_N2
nitrogen_mc_simulation$y$use_efficiency_P_change <- nitrogen_mc_simulation$y$use_efficiency_P1 - nitrogen_mc_simulation$y$use_efficiency_P2
nitrogen_mc_simulation$y$use_efficiency_K_change <- nitrogen_mc_simulation$y$use_efficiency_K1 - nitrogen_mc_simulation$y$use_efficiency_K2

nitrogen_mc_simulation$y$share_reuse_to_total_input_N_change <- nitrogen_mc_simulation$y$share_reuse_to_total_input_N1 - nitrogen_mc_simulation$y$share_reuse_to_total_input_N2
nitrogen_mc_simulation$y$share_reuse_to_total_input_P_change <- nitrogen_mc_simulation$y$share_reuse_to_total_input_P1 - nitrogen_mc_simulation$y$share_reuse_to_total_input_P2
nitrogen_mc_simulation$y$share_reuse_to_total_input_K_change <- nitrogen_mc_simulation$y$share_reuse_to_total_input_K1 - nitrogen_mc_simulation$y$share_reuse_to_total_input_K2

nitrogen_mc_simulation$y$recycling_rate_N_change <- nitrogen_mc_simulation$y$recycling_rate_N1 - nitrogen_mc_simulation$y$recycling_rate_N2
nitrogen_mc_simulation$y$recycling_rate_P_change <- nitrogen_mc_simulation$y$recycling_rate_P1 - nitrogen_mc_simulation$y$recycling_rate_P2
nitrogen_mc_simulation$y$recycling_rate_K_change <- nitrogen_mc_simulation$y$recycling_rate_K1 - nitrogen_mc_simulation$y$recycling_rate_K2

nitrogen_mc_simulation$y$losses_N_change <- nitrogen_mc_simulation$y$losses_N1 - nitrogen_mc_simulation$y$losses_N2
nitrogen_mc_simulation$y$losses_P_change <- nitrogen_mc_simulation$y$losses_P1 - nitrogen_mc_simulation$y$losses_P2
nitrogen_mc_simulation$y$losses_K_change <- nitrogen_mc_simulation$y$losses_K1 - nitrogen_mc_simulation$y$losses_K2


# PLS----
# what variable should be used for it?

pls_result <- plsr.mcSimulation(
  object = nitrogen_mc_simulation,
  resultName = names(nitrogen_mc_simulation$y["total_input_N_change"]), ncomp = 1
)
VIP <- function(object) {
  if (object$method != "oscorespls") {
    stop("Only implemented for orthogonal scores algorithm.  Refit with 'method = \"oscorespls\"'")
  }
  if (nrow(object$Yloadings) > 1) {
    stop("Only implemented for single-response models")
  }
  SS <- c(object$Yloadings)^2 * colSums(object$scores^2)
  Wnorm2 <- colSums(object$loading.weights^2)
  SSW <- sweep(
    object$loading.weights^2, 2, SS / Wnorm2,
    "*"
  )
  sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS))
}

library(tidyverse)

# function to return ordered and filtered vip scores

get_clean_VIP <- function(object, vip_threshold = 0.8) {
  # calculate VIP
  scores <- VIP(object)

  # filter scores by threshold
  scores <- scores[scores >= vip_threshold]

  # order by decreasing vip score
  scores <- scores[order(scores, decreasing = T)]

  # create dataframe which will be returned
  scores_df <- data.frame(variable = names(scores), vip = scores)

  # reset rownames()
  rownames(scores_df) <- NULL


  return(scores_df)
}

# loop over all variables I want to make a pls analysis with
pls_names <- c(
  "total_input_N_change", "total_input_P_change", "total_input_K_change",
  "use_efficiency_N_change", "use_efficiency_P_change", "use_efficiency_K_change",
  "share_reuse_to_total_input_N_change", "share_reuse_to_total_input_P_change", "share_reuse_to_total_input_K_change",
  "recycling_rate_N_change", "recycling_rate_P_change", "recycling_rate_K_change",
  "losses_N_change", "losses_P_change", "losses_K_change"
)

pls_list <- list()
for (var in pls_names) {
  # make pls analysis
  pls_result <- plsr.mcSimulation(
    object = nitrogen_mc_simulation,
    resultName = names(nitrogen_mc_simulation$y[var]), ncomp = 1
  )
  # get clean vip score, filtered and ordered
  vip <- get_clean_VIP(pls_result)

  # add from which flow it is calculated
  vip$indicator <- var

  vip$vip <- round(vip$vip, digits = 2)

  # save to list
  pls_list[[var]] <- vip
}

library(openxlsx)


# change some names because they are too long
names(pls_list)[7:9] <- c(
  "share_reuse_N_change", "share_reuse_P_change",
  "share_reuse_K_change"
)

# export each data frame to separate sheets in same Excel file
openxlsx::write.xlsx(pls_list, file = "data/vip/vip_per_indicator_ver2.xlsx")







# EVPI----
# all the flows are positive and that is why the evpi doesnt yield a sensible result
# I need the difference of the two to find something meaningful
boxplot(as.numeric(nitrogen_mc_simulation$y$total_input_N1) - as.numeric(nitrogen_mc_simulation$y$total_input_N2))


# here we subset the outputs from the mcSimulation function (y) by selecting the correct variables
# this should be done by the user (be sure to run the multi_EVPI only on the variables that the user wants)
mcSimulation_table <- data.frame(nitrogen_mc_simulation$x, nitrogen_mc_simulation$y[2])

# its always positive
#--> its always a good "decision"
mcSimulation_table$total_input_N1 <- as.numeric(mcSimulation_table$total_input_N1)



mcSimulation_table

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "total_input_N1")



plot_evpi(evpi, decision_vars = "NPV_decision_do")






# summarise the flows for bernou
library(tidyverse)

sum_result <- psych::describeBy(result_df, result_df$scenario, mat = TRUE)

write.csv(sum_result, file = "summary_model_flow.csv")

#
# #summarise the flows by min, 10% percentile, median, mean, sd, 90% percentile, max
# #for both scenarios
#
# names(nitrogen_mc_simulation$y)
# levels(as.factor(nitrogen_mc_simulation$y))
#
#
# #make new object which has two options combined to one column
#
# result_df <- data.frame(scenario = c(nitrogen_mc_simulation$y$scenario1, nitrogen_mc_simulation$y$scenario2),
#            self_supplied = as.numeric(c(nitrogen_mc_simulation$y$self_supplied1, nitrogen_mc_simulation$y$self_supplied2)),
#            external_input = as.numeric(c(nitrogen_mc_simulation$y$external_input1, nitrogen_mc_simulation$y$external_input2)),
#            system_output = as.numeric(c(nitrogen_mc_simulation$y$system_output1, nitrogen_mc_simulation$y$system_output2)),
#            system_losses = as.numeric(c(nitrogen_mc_simulation$y$system_losses1, nitrogen_mc_simulation$y$system_losses2)),
#            SSE = as.numeric(c(nitrogen_mc_simulation$y$SSE1, nitrogen_mc_simulation$y$SSE2)),
#            supplied_by_outside = as.numeric(c(nitrogen_mc_simulation$y$supplied_by_outside1, nitrogen_mc_simulation$y$supplied_by_outside2)),
#            N_manure_to_crops = as.numeric(c(nitrogen_mc_simulation$y$N_manure_to_crop1, nitrogen_mc_simulation$y$N_manure_to_crop2)))
#
# summary(result_df)
#
#
# library(tidyverse)
# library(ggridges)
#
# ggplot(result_df, aes(x = N_manure_to_crops, y = scenario, fill = scenario)) +
#   geom_density_ridges_gradient() +
#   xlab('Manure to crops N [t per year]')+
#   theme_bw() +
#   theme(legend.position = "none")
#
# ggplot(result_df, aes(x = self_supplied, y = scenario, fill = scenario)) +
#   geom_density_ridges_gradient() +
#   xlab('Self supplied N [t per year]')+
#   theme_bw() +
#   theme(legend.position = "none")
#
# ggplot(result_df, aes(x = external_input, y = scenario, fill = scenario)) +
#   geom_density_ridges_gradient() +
#   xlab('External Input N [t per year]')+
#   theme_bw() +
#   theme(legend.position = "none")
#
# ggplot(result_df, aes(x = system_output, y = scenario, fill = scenario)) +
#   geom_density_ridges_gradient() +
#   xlab('System Output N [t per year]')+
#   theme_bw() +
#   theme(legend.position = "none")
#
# #why are system losses almost the same?????
# ggplot(result_df, aes(x = system_losses, y = scenario, fill = scenario)) +
#   geom_density_ridges_gradient() +
#   xlab('System Losses N [t per year]')+
#   theme_bw() +
#   theme(legend.position = "none")
#
# ggplot(result_df, aes(x = SSE, y = scenario, fill = scenario)) +
#   geom_density_ridges_gradient() +
#   xlab('SSE N [t per year]')+
#   theme_bw() +
#   theme(legend.position = "none")
#
# ggplot(result_df, aes(x = supplied_by_outside, y = scenario, fill = scenario)) +
#   geom_density_ridges_gradient() +
#   xlab('supplied by outside N [t per year]')+
#   theme_bw() +
#   theme(legend.position = "none")
#
#
#
#
#
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c("N_manure_to_crop1",'N_manure_to_crop2'),
#                    old_names = c('1','2'),
#                    method = "smooth_simple_overlay",
#                    x_axis_name = 'kg N  / year')
#
#
# #how to make this also for other nutrients without copying everything?
# #loop within function for different nutrients?
# #or maybe make everything a vector? each entry of the vector is one nutrient?
#
#
# #for each scenario different column, can't I make so that it is combined?
# scenario <- c(nitrogen_mc_simulation$y$scenario1, nitrogen_mc_simulation$y$scenario2)
# self_supplied <- c(nitrogen_mc_simulation$y$self_supplied1, nitrogen_mc_simulation$y$self_supplied2)
# external_input <- c(nitrogen_mc_simulation$y$external_input1, nitrogen_mc_simulation$y$external_input2)
# system_output <- c(nitrogen_mc_simulation$y$system_output1, nitrogen_mc_simulation$y$system_output2)
# system_losses <- c(nitrogen_mc_simulation$y$system_losses1, nitrogen_mc_simulation$y$system_losses2)
# SSE <- c(nitrogen_mc_simulation$y$SSE1, nitrogen_mc_simulation$y$SSE2)
# supplied_by_outside <- c(nitrogen_mc_simulation$y$supplied_by_outside1, nitrogen_mc_simulation$y$supplied_by_outside2)
#
# #combine to new dataframe and replace the old one in nitrogen_mc_simulation
# y <- data.frame(scenario, self_supplied, external_input, system_output, system_losses, SSE, supplied_by_outside)
#
# #in that case also the
#
#
#
# #correct the crop shares in the input table of the mcsimulation object ----
#
# #create object with crop names
# crop = c('beans','corn', 'fodder_peas', 'mais_silage', 'oat',
#          'oilseed_rape', 'potato', 'rye', 'sugar_beet',
#          'summer_barley', 'summer_wheat', 'triticale',
#          'winter_barley', 'winter_wheat')
#
# #add 'share' to it
# crop <- paste('share_', crop, sep = '')
#
# #replace initial shares (in x) with corrected shares (from y)
# for(crop_i in crop){
#   print(crop_i)
#   nitrogen_mc_simulation$x[crop_i] <- nitrogen_mc_simulation$y[crop_i]
# }
#
#
# #do also correction for horticulture products
# crop <- c('apple','arugula','asparagus','berries','cabbage',
#           'carrot','celery','green_bean','lambs_lettuce',
#           'lettuce','onion', 'parsley', 'pumpkin', 'radishes',
#           'rhubarb', 'spinash', 'stone_fruit', 'strawberry',
#           'sweet_corn', 'veggie_peas')
#
# crop <- paste0('land_', crop, '_ha')
#
# for(crop_i in crop){
#   print(crop_i)
#   nitrogen_mc_simulation$x[crop_i] <- nitrogen_mc_simulation$y[crop_i]
# }
#
#
#
# #other visualizations attempts ----
# #load needed libraries
# library(tidyverse)
# library(reshape2)
#
#
#
# #assign output to new variable
# mc_simulation_output <- nitrogen_mc_simulation$y
#
# #melt df to make it ggplot-friendly
# mc_simulation_output_long <- melt(mc_simulation_output)
#
# #change output from kg to t
# mc_simulation_output_long$value <- mc_simulation_output_long$value / 1000
#
#
# #classify which stream belongs to which subsystem
# #we always use streams leaving a system to decide to which subsystem it belongs to
# #e.g. manure going to crop belongs to the animal subsystem
#
# streams_df <- rbind.data.frame(c('sewage', 'consumption', 'waste'),
#                                c('ofmsw_residual_waste', 'consumption', 'waste'),
#                                c('ofmsw','consumption','waste'),
#                                c('wastewater','consumption','export'),
#                                c('compost_to_consumption','waste','consumption'),
#                                c('digestate','waste','crop'),
#                                c('sewage_sludge_export','waste','export'),
#                                c('wastewater_effluent_gaseous_losses','waste','export'),
#                                c('fresh_compost_export','waste','export'),
#                                c('fresh_compost_crop','waste','crop'),
#                                c('sewage_to_crop','waste','crop'),
#                                c('vegetal_biogas_substrate','crop','waste'),
#                                c('crop_cultivation_losses','crop','export'),
#                                c('other_organic_fertilizer_export','crop','export'),
#                                c('straw','crop','animal'),
#                                c('feed_crops','crop','animal'),
#                                c('grassbased_feed','crop','animal'),
#                                c('fruit_and_vegetable','crop','processing'),
#                                c('food_and_feed_crops','crop','processing'),
#                                c('manure_as_biogas_substrate','animal','waste'),
#                                c('manure_to_crop','animal','crop'),
#                                c('manure_export','animal','export'),
#                                c('animal_housing_and_storage_losses','animal','export'),
#                                c('slaughter_animal','animal','processing'),
#                                c('egg_and_dairy','animal','processing'),
#                                c('local_vegetal_products_consumed','processing','consumption'),
#                                c('imported_animal_products','processing','consumption'),
#                                c('imported_vegetal_products','processing','consumption'),
#                                c('feed_from_processed_crops','processing','animal'),
#                                c('import_processed_feed','processing','animal'),
#                                c('local_animal_products_consumed','processing','consumption'),
#                                c('export_meat','processing','export'),
#                                c('import_meat','import','processing'),
#                                c('export_egg','processing','export'),
#                                c('slaughter_waste','processing','export'),
#                                c('import_OFMSW','import','waste'),
#                                c('import_inorganic_fertilizer','import','crop'),
#                                c('import_organic_fertilizer','import','crop'),
#                                c('net_food_import','import','processing'),
#                                c('net_feed_import','import','processing'))
#
# #adjust column names
# names(streams_df) <- c('flow', 'origin', 'destination')
#
# #make origin and destination factor
# streams_df$origin <- as.factor(streams_df$origin)
# streams_df$destination <- as.factor(streams_df$destination)
#
#
#
#
#
# mc_simulation_output_long %>%
#   filter(variable %in% streams_df$flow[streams_df$origin == 'animal']) %>%
#   ggplot(aes(x=value,y = variable,fill = variable)) +
#   geom_density_ridges_gradient(scale=2) +
#   xlab('N [t per year]')+
#   ylab('flow leaving animal subsystem (without feed import)')+
#   theme_bw() +
#   theme(legend.position = "none")
#
#
# mc_simulation_output_long %>%
#   filter(variable %in% streams_df$flow[streams_df$destination == 'animal']) %>%
#   ggplot(aes(x=value,y = variable,fill = variable)) +
#   geom_density_ridges_gradient(scale=2) +
#   xlab('N [t per year]')+
#   ylab('flow leaving animal subsystem (without feed import)')+
#   theme_bw() +
#   theme(legend.position = "none")
#
#
#
#
# #
# #
# # mc_simulation_output_long %>%
# #   filter(variable %in% c('N_to_slaughter','N_meat_local_to_consumption','N_slaughter_waste',
# #                          'N_egg_available','N_housing_loss', 'N_milk_available',
# #                          'N_biogas_input_animal','export_org_fertilizer',
# #                          'N_manure_to_crop')) %>%
# #
# #   ggplot(aes(x=value,y = variable,fill = variable)) +
# #   geom_density_ridges_gradient(scale=2) +
# #   xlab('N [t per year]')+
# #   ylab('flow leaving animal subsystem (without feed import)')+
# #   theme_bw() +
# #   theme(legend.position = "none")
# #
# # mc_simulation_output_long %>%
# #   filter(variable %in% c('egg_import','dairy_import',
# #                          'meat_import', 'other_food_import')) %>%
# #   ggplot(aes(x=value,y = variable,fill = variable)) +
# #   geom_density_ridges_gradient(scale=2) +
# #   xlab('food import (without crops) N [t per year]')+
# #   ylab('')+
# #   theme_bw() +
# #   theme(legend.position = "none")
# #
# # mc_simulation_output_long %>%
# #   filter(variable %in% c('egg_export','dairy_export',
# #                          'meat_export')) %>%
# #
# #   ggplot(aes(x=value,y = variable,fill = variable)) +
# #   geom_density_ridges_gradient(scale=2) +
# #   xlab('food export (without vegetables) N [t per year]')+
# #   ylab('')+
# #   theme_bw() +
# #   theme(legend.position = "none")
# #
# # #calculate the amount of cases more eggs produced that consumed
# # sum(mc_simulation_output$egg_export > 0) / 10000
# # sum(mc_simulation_output$dairy_export > 0) / 10000
# # sum(mc_simulation_output$meat_export > 0) / 10000
# #
# #
# #
# # #plots of flows out the animal subsystem
# # plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
# #                    vars = c("N_to_slaughter",'N_meat_local_to_consumption','N_slaughter_waste'),
# #                    method = "smooth_simple_overlay",
# #                    old_names = c('N_to_slaughter','N_meat_local_to_consumption', 'N_slaughter_waste'),
# #                    x_axis_name = 'kg N  / year')
# # ?plot_distributions
# #
# # plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
# #                    vars = c('N_egg_available'),
# #                    method = "smooth_simple_overlay",
# #                    old_names = c('N_egg_available'),
# #                    x_axis_name = 'kg N  / year')
# #
# # plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
# #                    vars = c('N_milk_available'),
# #                    method = "smooth_simple_overlay",
# #                    old_names = c('N_milk_available'),
# #                    x_axis_name = 'kg N  / year')
# #
# # plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
# #                    vars = c('N_housing_loss','N_biogas_input_animal','export_org_fertilizer'),
# #                    method = "smooth_simple_overlay",
# #                    x_axis_name = 'kg N  / year')
# #
# # plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
# #                    vars = c('N_manure_to_crop'),
# #                    method = "smooth_simple_overlay",
# #                    x_axis_name = 'kg N  / year')
# #
# # #plots flow entering the system
# # plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
# #                    vars = c("N_straw"),
# #                    method = "smooth_simple_overlay",
# #                    x_axis_name = 'kg N  / year')
# #
# # plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
# #                    vars = c('N_crop_animal_feeding_unprocessed','N_grassland'),
# #                    method = "smooth_simple_overlay",
# #                    x_axis_name = 'kg N  / year')
# #
# # plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
# #                    vars = c('N_crop_animal_feeding_processed'),
# #                    method = "smooth_simple_overlay",
# #                    x_axis_name = 'kg N  / year')
# #
# # plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
# #                    vars = c('feed_import'),
# #                    method = "smooth_simple_overlay",
# #                    x_axis_name = 'kg N  / year')
# #
# #
# #
# # #PLS----
# #
# # pls_result <- plsr.mcSimulation(object = nitrogen_mc_simulation,
# #                                 resultName = names(nitrogen_mc_simulation$y['N_manure_to_crop']), ncomp = 1)
# #
# # plot_pls(pls_result, input = combined_input, threshold = 0.8)
# #
