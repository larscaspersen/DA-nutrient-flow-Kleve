#try out how to solve the problem of several percantages in input table 
#which need to add up to 1
# # 
# library(decisionSupport)
# 
# crop_input <- read.csv('data/input-all.csv')
# 
# make_variables<-function(est,n=1)
# { x<-random(rho=est, n=n)
# for(i in colnames(x)) assign(i,
#                              as.numeric(x[1,i]),envir=.GlobalEnv)
# }
# 
# crop_input$lower <- as.numeric(crop_input$lower)
# crop_input$upper <- as.numeric(crop_input$upper)
# 
# make_variables(as.estimate(crop_input),n=1)

crop_function <- function(arable_land,
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
                          yield_share_beans,yield_share_corn,
                          yield_share_fodder_peas,
                          yield_share_mais_silage,yield_share_oat,
                          yield_share_oilseed_rape,
                          yield_share_potato,yield_share_rye,
                          yield_share_sugar_beet,yield_share_summer_barley,
                          yield_share_summer_wheat,yield_share_triticale,
                          yield_share_winter_barley,yield_share_winter_wheat,
                          dm_beans, 
                          dm_corn, 
                          dm_fodder_peas,
                          dm_mais_silage,
                          dm_oat, 
                          dm_oilseed_rape,
                          dm_potato,dm_rye,
                          dm_sugar_beet,
                          dm_summer_barley,
                          dm_summer_wheat,
                          dm_triticale,dm_winter_barley,
                          dm_winter_wheat,
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
                          import_inorganic_N_kg_LF, import_inorganic_P2O5_kg_LF, import_inorganic_K2O_t,
                          convert_phosphorous_pentoxide_to_p, convert_potassium_oxide_to_k){
  
  #correct land shares ----
  #the land shares of the crops are randomly drawn, but they need to add up to 100%
  #so add up everything and determine by how much each share needs to be corrected to reach 100%
  #--> correction factor
  
  #sum of all shares
  estimated_crop_land <- share_beans + share_corn + share_fodder_peas + 
    share_mais_silage + share_oat + 
    share_oilseed_rape + share_potato +share_rye + share_sugar_beet + 
    share_summer_barley + share_summer_wheat + 
    share_triticale + share_winter_barley + share_winter_wheat
  
  #calc correction factor
  correction_factor <- share_crop_land / estimated_crop_land
  
  #apply correction factor to randomly drawn shares
  new_share_beans <- share_beans * correction_factor
  new_share_corn <- correction_factor * share_corn
  new_share_fodder_peas <- correction_factor * share_fodder_peas
  new_share_mais_silage <- correction_factor * share_mais_silage
  new_share_oat <- correction_factor * share_oat
  new_share_oilseed_rape <- correction_factor * share_oilseed_rape
  new_share_potato <- correction_factor * share_potato
  new_share_rye <- correction_factor * share_rye
  new_share_sugar_beet <- correction_factor * share_sugar_beet
  new_share_summer_barley <- correction_factor * share_summer_barley
  new_share_summer_wheat <- correction_factor * share_summer_wheat
  new_share_triticale <- correction_factor * share_triticale
  new_share_winter_barley <- correction_factor *  share_winter_barley
  new_share_winter_wheat <- correction_factor * share_winter_wheat
  
  #####
  #I wonder if this correctio pushes values outside their upper and lower limits? 
  #####
  
  #bind all variables to data frame
  crop_df <- data.frame(crop = c('bean','corn', 'fodder_peas', 'mais_silage', 'oat',
                                  'oilseed_rape', 'potato', 'rye', 'sugar_beet', 
                                  'summer_barley', 'summer_wheat', 'triticale', 
                                  'winter_barley', 'winter_wheat'),
                        land_share = c(new_share_beans,new_share_corn, new_share_fodder_peas,
                          new_share_mais_silage, new_share_oat, new_share_oilseed_rape,
                          new_share_potato, new_share_rye, new_share_sugar_beet,
                          new_share_summer_barley, new_share_summer_wheat,
                          new_share_triticale, new_share_winter_barley, new_share_winter_wheat),
                        yield = c(yield_beans, yield_corn, yield_fodder_peas,
                                  yield_mais_silage, yield_oat, yield_oilseed_rape,
                                  yield_potato, yield_rye, yield_sugar_beet,
                                  yield_summer_barley, yield_summer_wheat,
                                  yield_triticale, yield_winter_barley,
                                  yield_winter_wheat),
                        dm = c(dm_beans, dm_corn, dm_fodder_peas,dm_mais_silage,
                               dm_oat, dm_oilseed_rape,dm_potato,dm_rye,
                               dm_sugar_beet,dm_summer_barley,dm_summer_wheat,
                               dm_triticale,dm_winter_barley,dm_winter_wheat),
                        yield_share = c(yield_share_beans,yield_share_corn,
                                        yield_share_fodder_peas,
                                        yield_share_mais_silage,yield_share_oat,
                                        yield_share_oilseed_rape,
                                        yield_share_potato,yield_share_rye,
                                        yield_share_sugar_beet,yield_share_summer_barley,
                                        yield_share_summer_wheat,yield_share_triticale,
                                        yield_share_winter_barley,yield_share_winter_wheat),
                        N_yield = c(N_yield_beans,N_yield_corn,N_yield_fodder_peas,
                                    N_yield_mais_silage,N_yield_oat,
                                    N_yield_oilseed_rape,N_yield_potato,
                                    N_yield_rye,N_yield_sugar_beet,N_yield_summer_barley,
                                    N_yield_summer_weat,N_yield_triticale,
                                    N_yield_winter_barley,N_yield_winter_weat),
                        N_leftover = c(N_leftover_beans,N_leftover_corn,
                                       N_leftover_foder_peas,N_leftover_mais_silage,
                                       N_leftover_oat,N_leftover_oilseed_rape,
                                       N_leftover_potato,N_leftover_rye,
                                       N_leftover_sugar_beet,N_leftover_summer_barley,
                                       N_leftover_summer_wheat,N_leftover_triticale,
                                       N_leftover_winter_barley,N_leftover_winter_wheat),
                        P_yield = c(P_yield_beans,
                                    P_yield_corn,
                                    P_yield_fodder_peas,
                                    P_yield_mais_silage,
                                    P_yield_oat,
                                    P_yield_oilseed_rape,
                                    P_yield_potato,
                                    P_yield_rye,
                                    P_yield_sugar_beet,
                                    P_yield_summer_barley,
                                    P_yield_summer_weat,
                                    P_yield_triticale,
                                    P_yield_winter_barley,
                                    P_yield_winter_weat),
                        P_leftover = c(P_leftover_beans,
                                       P_leftover_corn,
                                       P_leftover_foder_peas,
                                       P_leftover_mais_silage,
                                       P_leftover_oat,
                                       P_leftover_oilseed_rape,
                                       P_leftover_potato,
                                       P_leftover_rye,
                                       P_leftover_sugar_beet,
                                       P_leftover_summer_barley,
                                       P_leftover_summer_wheat,
                                       P_leftover_triticale,
                                       P_leftover_winter_barley,
                                       P_leftover_winter_wheat),
                        K_yield = c(K_yield_beans,
                                    K_yield_corn,
                                    K_yield_fodder_peas,
                                    K_yield_mais_silage,
                                    K_yield_oat,
                                    K_yield_oilseed_rape,
                                    K_yield_potato,
                                    K_yield_rye,
                                    K_yield_sugar_beet,
                                    K_yield_summer_barley,
                                    K_yield_summer_weat,
                                    K_yield_triticale,
                                    K_yield_winter_barley,
                                    K_yield_winter_weat),
                        K_leftover = c(K_leftover_beans,
                                       K_leftover_corn,
                                       K_leftover_foder_peas,
                                       K_leftover_mais_silage,
                                       K_leftover_oat,
                                       K_leftover_oilseed_rape,
                                       K_leftover_potato,
                                       K_leftover_rye,
                                       K_leftover_sugar_beet,
                                       K_leftover_summer_barley,
                                       K_leftover_summer_wheat,
                                       K_leftover_triticale,
                                       K_leftover_winter_barley,
                                       K_leftover_winter_wheat),
                        share_leftover_straw = c(rep(0,4),straw_share,0,0,straw_share, 0, rep(straw_share,5)),
                        share_to_animal = c(beans_to_animal,corn_to_animal,fodder_peas_to_animal,
                                            mais_silage_to_animal,oat_to_animal, oilseed_rape_to_animal,
                                            potato_to_animal, rye_to_animal, sugar_beet_to_animal,
                                            summer_barley_to_animal, summer_wheat_to_animal,
                                            triticale_to_animal, winter_barley_to_animal,
                                            winter_wheat_to_animal),
                        share_to_consumption = c(beans_to_consumption, corn_to_consumption,fodder_peas_to_consumption,
                                                 mais_silage_to_consumption, oat_to_consumption,
                                                 oilseed_rape_to_consumption, potato_to_ponsumption,
                                                 rye_to_consumption, sugar_beet_to_sunsumption,
                                                 summer_barley_to_consumption, summer_wheat_to_consumption,
                                                 triticale_to_consumption, winter_barley_to_consumption,
                                                 winter_wheat_to_consumption),
                        share_to_biogas = c( rep(0,3), mais_silage_to_biogas, rep(0,10) ),
                        through_processing = c( rep(1, 3), 0, rep(1, 2), 0.5, rep(1, 7) ) )
  
  #calculate absolute amount of ha per crop
  crop_df$land_absolute <- crop_df$land_share*arable_land
  
  #get absolute yield of crops
  crop_df$yield_total <- crop_df$yield * crop_df$land_absolute #in dt
  
  #get N of consumable part
  crop_df$N_main <- crop_df$yield_total * crop_df$yield_share * crop_df$N_yield
  crop_df$P_main <- crop_df$yield_total * crop_df$yield_share * crop_df$P_yield
  #K is expressed as share of DM total yield; yield total is in dt, so multiply by 100 in the end
  crop_df$K_main <- crop_df$yield_total * crop_df$yield_share * crop_df$dm * crop_df$K_yield * 100
  
  
  #distributute main N by animal and human consumption and by with or without processing ----
  
  #human consumption with processing
  crop_df$N_crop_human_consumption_processed <- crop_df$N_main * crop_df$share_to_consumption * crop_df$through_processing
  crop_df$P_crop_human_consumption_processed <- crop_df$P_main * crop_df$share_to_consumption * crop_df$through_processing
  crop_df$K_crop_human_consumption_processed <- crop_df$K_main * crop_df$share_to_consumption * crop_df$through_processing
  
  #human consumption without processing
  crop_df$N_crop_human_consumption_unprocessed <- crop_df$N_main * crop_df$share_to_consumption * (1 - crop_df$through_processing)
  crop_df$P_crop_human_consumption_unprocessed <- crop_df$P_main * crop_df$share_to_consumption * (1 - crop_df$through_processing)
  crop_df$K_crop_human_consumption_unprocessed <- crop_df$K_main * crop_df$share_to_consumption * (1 - crop_df$through_processing)
  
  #to animal consumption with processing
  crop_df$N_crop_animal_feeding_processed <- crop_df$N_main * crop_df$share_to_animal * crop_df$through_processing
  crop_df$P_crop_animal_feeding_processed <- crop_df$P_main * crop_df$share_to_animal * crop_df$through_processing
  crop_df$K_crop_animal_feeding_processed <- crop_df$K_main * crop_df$share_to_animal * crop_df$through_processing
  
  #to animal consumption without processing
  crop_df$N_crop_animal_feeding_unprocessed <- crop_df$N_main * crop_df$share_to_animal * ( 1 - crop_df$through_processing )
  crop_df$P_crop_animal_feeding_unprocessed <- crop_df$P_main * crop_df$share_to_animal * ( 1 - crop_df$through_processing )
  crop_df$K_crop_animal_feeding_unprocessed <- crop_df$K_main * crop_df$share_to_animal * ( 1 - crop_df$through_processing )
  
  
  
  
  ######
  # BIOGAS
  ######
  
  #(actually stream from crop to waste)
  #this mostly includes maize, but rest is filled up with human food
  crop_df$N_crop_biogas <- crop_df$N_main * crop_df$share_to_biogas
  crop_df$P_crop_biogas <- crop_df$P_main * crop_df$share_to_biogas
  crop_df$K_crop_biogas <- crop_df$K_main * crop_df$share_to_biogas
  
  
  
  
  #get N of not consumable part
  crop_df$N_rest <- crop_df$yield_total * (1-crop_df$yield_share) * crop_df$N_leftover
  crop_df$P_rest <- crop_df$yield_total * (1-crop_df$yield_share) * crop_df$P_leftover
  crop_df$K_rest <- crop_df$yield_total * (1-crop_df$yield_share) * crop_df$K_leftover
  
  #calculate N in straw which is used for animal bedding
  crop_df$N_straw <- crop_df$N_rest * crop_df$share_leftover_straw
  crop_df$P_straw <- crop_df$P_rest * crop_df$share_leftover_straw
  crop_df$K_straw <- crop_df$K_rest * crop_df$share_leftover_straw
  

  
  #sum up the columns of interest ----
  N_crop_main <- sum(crop_df$N_main)
  P_crop_main <- sum(crop_df$P_main)
  K_crop_main <- sum(crop_df$K_main)
  
  N_straw <- sum(crop_df$N_straw)
  P_straw <- sum(crop_df$P_straw)
  K_straw <- sum(crop_df$K_straw)
  
  #part which neither straw nor consumable yield
  N_crop_rest <- sum(crop_df$N_rest) - N_straw
  P_crop_rest <- sum(crop_df$P_rest) - P_straw
  K_crop_rest <- sum(crop_df$K_rest) - K_straw
  
  
  N_crop_human_consumption_processed <- sum(crop_df$N_crop_human_consumption_processed)
  P_crop_human_consumption_processed <- sum(crop_df$P_crop_human_consumption_processed)
  K_crop_human_consumption_processed <- sum(crop_df$K_crop_human_consumption_processed)
  
  N_crop_human_consumption_unprocessed <- sum(crop_df$N_crop_human_consumption_unprocessed)
  P_crop_human_consumption_unprocessed <- sum(crop_df$P_crop_human_consumption_unprocessed)
  K_crop_human_consumption_unprocessed <- sum(crop_df$K_crop_human_consumption_unprocessed)
  
  N_crop_animal_feeding_processed <- sum(crop_df$N_crop_animal_feeding_processed)
  P_crop_animal_feeding_processed <- sum(crop_df$P_crop_animal_feeding_processed)
  K_crop_animal_feeding_processed <- sum(crop_df$K_crop_animal_feeding_processed)
  
  N_crop_animal_feeding_unprocessed <- sum(crop_df$N_crop_animal_feeding_unprocessed)
  P_crop_animal_feeding_unprocessed <- sum(crop_df$P_crop_animal_feeding_unprocessed)
  K_crop_animal_feeding_unprocessed <- sum(crop_df$K_crop_animal_feeding_unprocessed)
  
  #------------#
  ## BIOGAS ####
  #------------#
  
  #maize going to biogas
  N_crop_biogas <- sum(crop_df$N_crop_biogas)
  P_crop_biogas <- sum(crop_df$P_crop_biogas)
  K_crop_biogas <- sum(crop_df$K_crop_biogas)
  
  #non-maize going to biogas is total biogas - manure - maize
  nonmaize_to_biogas_N <- N_biogas_input - N_crop_biogas - (N_biogas_input * share_N_biogas_input_animal)
  nonmaize_to_biogas_P <- P_biogas_input - P_crop_biogas - (P_biogas_input * share_P_biogas_input_animal)
  nonmaize_to_biogas_K <- K_biogas_input - K_crop_biogas - (K_biogas_input * share_K_biogas_input_animal)
  
  #add nonmaize to N_crop_biogas, subtract it from processed food
  N_crop_biogas <- N_crop_biogas + nonmaize_to_biogas_N
  P_crop_biogas <- P_crop_biogas + nonmaize_to_biogas_P
  K_crop_biogas <- K_crop_biogas + nonmaize_to_biogas_K
  
  N_crop_human_consumption_processed <- N_crop_human_consumption_processed - nonmaize_to_biogas_N
  P_crop_human_consumption_processed <- P_crop_human_consumption_processed - nonmaize_to_biogas_P
  K_crop_human_consumption_processed <- K_crop_human_consumption_processed - nonmaize_to_biogas_K
  
  
  ################
  # HORTICULTURE
  ################
  
  #NOTE: used N content of peas for green bean because no value in the excel file
  #NOTE II: ha of vegetables add to only 60% of non-crop land
  
  #correct the vegetable land, so that it doesn't exceed the total
  
  horti_df <- data.frame(crop = c('apple','arugula','asparagus','berries','cabbage',
                        'carrot','celery','green_bean','lambs_lettuce',
                        'lettuce','onion', 'parley', 'pumpkin', 'radishes',
                        'rhubarb', 'spinash', 'stone_fruit', 'strawberry',
                        'sweet_corn', 'veggie_peas'),
                         area_ha = c(land_apple_ha, land_arugula_ha,
                         land_asparagus_ha, land_berries_ha,
                         land_cabbage_ha, land_carrot_ha,
                         land_celery_ha,
                         land_green_bean_ha, land_lambs_lettuce_ha,
                         land_lettuce_ha, land_onion_ha,
                         land_parsley_ha,land_pumpkin_ha,
                         land_radishes_ha, land_rhubarb_ha,
                         land_spinash_ha, land_stone_fruit_ha,
                         land_strawberry_ha, land_sweet_corn_ha,
                         land_veggie_peas_ha),
                         yield_dt_ha = c(yield_apple_dt_ha, yield_arugula_dt_ha,
                                         yield_asparagus_dt_ha, yield_berries_dt_ha,
                                         yield_cabbage_dt_ha, yield_carrot_dt_ha,
                                         yield_celery_dt_ha, yield_green_bean_dt_ha,
                                         yield_lambs_lettuce_dt_ha, yield_lettuce_dt_ha,
                                         yield_onion_dt_ha, yield_parsley_dt_ha,
                                         yield_pumpkin_dt_ha, yield_radishes_dt_ha,
                                         yield_rhubarb_dt_ha, yield_spinash_dt_ha,
                                         yield_stone_fruit_dt_ha, yield_strawberry_dt_ha,
                                         yield_sweet_corn_dt_ha, yield_veggie_peas_dt_ha),
                        N_content_gr_100gr = c(N_content_apple_gr_100gr,
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
                                               N_content_veggie_peas_gr_100gr),
                        P_content_mg_100gr = c(P_content_apple_gr_100gr,
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
                                               P_content_veggie_peas_gr_100gr),
                        K_content_mg_100gr = c(K_content_apple_gr_100gr,
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
                                               K_content_veggie_peas_gr_100gr)
                        )
  
  #calculate modelled horticultural land to actual land and calculate correction_factor
  estimated_vegetable_land <- sum(horti_df$area_ha)
  correction_factor <- land_horticulture_ha / estimated_vegetable_land
  
  
  #correct the ha of the vegetables so that they add to total horticultural land
  horti_df$corrected_area_ha <- horti_df$area_ha * correction_factor
  
  #calculate total yield of crop
  horti_df$total_yield_kg <- horti_df$corrected_area_ha * horti_df$yield_dt_ha * 100
  
  #calculate total N (kg)
  horti_df$total_N_kg <- horti_df$total_yield_kg * horti_df$N_content_gr_100gr * 10 / 1000
  horti_df$total_P_kg <- horti_df$total_yield_kg * horti_df$P_content_mg_100gr * 10 / 1000000
  horti_df$total_K_kg <- horti_df$total_yield_kg * horti_df$K_content_mg_100gr * 10 / 1000000
  
  #get total N from horticultural production
  horti_N_kg <- sum(horti_df$total_N_kg)
  horti_P_kg <- sum(horti_df$total_P_kg)
  horti_K_kg <- sum(horti_df$total_K_kg)
  
  
  
  
  
  ###########
  # IMPORT INORGANIC FERTILIZER
  ###########
  
  #this is only coupled to the LF, but it should be affected by the amount of available animal N otherwise
  imported_inorganic_N <- import_inorganic_N_kg_LF * (area_grassland + arable_land)
  imported_inorganic_P <- import_inorganic_P2O5_kg_LF * (area_grassland + arable_land) * convert_phosphorous_pentoxide_to_p
  imported_inorganic_K <- import_inorganic_K2O_t * convert_potassium_oxide_to_k * 1000

    
  
  
  # 
  # ########
  # # N-LOSSES after AG Düngung
  # ########
  # 
  # if(precipitation_sum < 600){
  #   
  #   if(ackerzahl <45){
  #     
  #     arable_N_leached <- 30 * arable_land
  #     
  #   } else if(ackerzahl >= 45 & ackerzahl <= 65){
  #     
  #     arable_N_leached <- 25 * arable_land
  #     
  #   } else if(ackerzahl >65 & ackerzahl <= 85){
  #     
  #     arable_N_leached <- 15 * arable_land
  #     
  #   } else if(ackerzahl > 85){
  #     
  #     arable_N_leached <- 5 * arable_land
  #     
  #   }
  #   
  # } else if(precipitation_sum >= 600 & precipitation_sum <= 750 ){
  #   
  #   if(ackerzahl <45){
  #     
  #     arable_N_leached <- 35 * arable_land
  #     
  #   } else if(ackerzahl >= 45 & ackerzahl <= 65){
  #     
  #     arable_N_leached <- 30 * arable_land
  #     
  #   } else if(ackerzahl >65 & ackerzahl <= 85){
  #     
  #     arable_N_leached <- 20 * arable_land
  #     
  #   } else if(ackerzahl > 85){
  #     
  #     arable_N_leached <- 10 * arable_land
  #     
  #   }
  #   
  # } else if(precipitation_sum > 750){
  #   
  #   if(ackerzahl <45){
  #     
  #     arable_N_leached <- 40 * arable_land
  #     
  #   } else if(ackerzahl >= 45 & ackerzahl <= 65){
  #     
  #     arable_N_leached <- 35 * arable_land
  #     
  #   } else if(ackerzahl >65 & ackerzahl <= 85){
  #     
  #     arable_N_leached <- 25 * arable_land
  #     
  #   } else if(ackerzahl > 85){
  #     
  #     arable_N_leached <- 15 * arable_land
  #     
  #   }
  # }
  # 
  # 
  # #modifiy arable N losses by certain criteria:
  # #fertilization intensity of organic fertilizer
  # #fertilization intensity of inorganic fertilizer
  # #share of vulnerable crops
  # 
  # if(livestock_density < 0.5){
  #   arable_N_leached <- arable_N_leached
  # } else if(livestock_density >= 0.5 & livestock_density <= 1){
  #   arable_N_leached <- arable_N_leached * 1.1
  # }  else if(livestock_density > 1 & livestock_density <= 1.5){
  #   arable_N_leached <- arable_N_leached * 1.2
  # }  else if(livestock_density > 1.5 & livestock_density <= 2.0){
  #   arable_N_leached <- arable_N_leached * 1.3
  # }  else if(livestock_density > 2 & livestock_density <= 2.5){
  #   arable_N_leached <- arable_N_leached * 1.45
  # }  else if(livestock_density  > 2.5){
  #   arable_N_leached <- arable_N_leached * 1.6
  # } 
  # 
  # #additional losses depending inorganic fertilization regime
  # if(fertilization_rate < 50){
  #   arable_N_leached <- arable_N_leached
  # }  else if(fertilization_rate >= 50 & fertilization_rate <= 100){
  #   arable_N_leached <- arable_N_leached * 1.2
  # } else if(fertilization_rate > 100 & fertilization_rate <= 150){
  #   arable_N_leached <- arable_N_leached * 1.3
  # } else if(fertilization_rate > 150 & fertilization_rate <= 200){
  #   arable_N_leached <- arable_N_leached * 1.4
  # } else if(fertilization_rate > 200){
  #   arable_N_leached <- arable_N_leached * 1.6
  # }
  # 
  # #additional losses if high share of loss vulnerable crops
  # share_vulnerable_area <- share_beans + share_fodder_peas + share_oilseed_rape + (land_horticulture_ha / (land_horticulture_ha+arable_land))
  # 
  # if(share_vulnerable_area < 0.20){
  #   arable_N_leached <- arable_N_leached
  # } else if(share_vulnerable_area >= 0.20 & share_vulnerable_area <= 0.45){
  #   arable_N_leached <- arable_N_leached * 1.3
  # } else if(share_vulnerable_area > 0.45 & share_vulnerable_area <= 0.70){
  #   arable_N_leached <- arable_N_leached * 1.5
  # } else if(share_vulnerable_area > 0.7){
  #   arable_N_leached <- arable_N_leached * 1.8
  # }
  # 
  # #arable land amminioa losses
  # 
  # #roughly 2% of applied inorganic fertilizer
  # arable_ammonia_losses <-  import_organic_N_kg * ammonia_loss_rate_fertilizer
  # 
  # 
  # #in the manual for unvermeidbare düngungsverluste it sais there is a constant loss of 4 kg N which is added ontop after modifying
  # #mostly for losses from plant material, especially if mulch is used intensively
  # 
  # arable_ammonia_losses <- arable_ammonia_losses + (arable_land * flatrate_ammonia_losses) 
  # 
  # #further losses if ammonia rich fertilizer is applied, ignored for now
  # 
  # 
  # #further N losses from grassland (leached)
  # grassland_N_losses <- (area_grassland * share_gorundwater_influenced_grassland * 30) + 
  #   (area_grassland * (1-share_gorundwater_influenced_grassland) * 20)
  # 
  # 
  # #these are the losses calculated after the approach of bundesarbeitskreis düngung, 
  # #alternatively it is also possible to calculate losses as difference between input and output
  # #of nitrogen
  # inevitable_N_losses <- grassland_N_losses + arable_ammonia_losses + arable_N_leached
  #   
  # 
  # #add rapeseed, peas, beans and horticultural area together
  # #if larger than 20 then add extra share of losses to loss calculation
  # #(quite unlikely, because by current numbers ~15%)
  
  
  
  ##########
  # GRASSLAND
  ###########

  N_mowing <- area_grassland * (1 - share_grazing) * N_yield_mowing
  P_mowing <- area_grassland * (1 - share_grazing) * P_yield_mowing * convert_phosphorous_pentoxide_to_p
  K_mowing <- area_grassland * (1 - share_grazing) * K_yield_mowing * convert_potassium_oxide_to_k
  
  
  #P2O5 and K2O yield of grazing not known, so assumed to be in the same N:P:K as mowing
  P_yield_grazing <- P_yield_mowing / N_yield_mowing * N_yield_grazing
  K_yield_grazing <- K_yield_mowing / N_yield_mowing * N_yield_grazing
  
  N_grazing <- area_grassland * share_grazing * N_yield_grazing
  P_grazing <- area_grassland * share_grazing * P_yield_grazing * convert_phosphorous_pentoxide_to_p
  K_grazing <- area_grassland * share_grazing * K_yield_grazing * convert_potassium_oxide_to_k
  
  N_grassland <- N_grazing + N_mowing
  P_grassland <- P_grazing + P_mowing
  K_grassland <- K_grazing + K_mowing
  
  

  

  
  
  
  
  return(list(N_crop_main = N_crop_main, 
              P_crop_main = P_crop_main,
              K_crop_main = K_crop_main,
              N_crop_rest = N_crop_rest,
              P_crop_rest = P_crop_rest,
              K_crop_rest = K_crop_rest,
              N_straw = N_straw,
              P_straw = P_straw,
              K_straw = K_straw,
              N_crop_human_consumption_processed = N_crop_human_consumption_processed,
              P_crop_human_consumption_processed = P_crop_human_consumption_processed,
              K_crop_human_consumption_processed = K_crop_human_consumption_processed,
              N_crop_human_consumption_unprocessed = N_crop_human_consumption_unprocessed,
              P_crop_human_consumption_unprocessed = P_crop_human_consumption_unprocessed,
              K_crop_human_consumption_unprocessed = K_crop_human_consumption_unprocessed,
              N_crop_animal_feeding_processed = N_crop_animal_feeding_processed,
              P_crop_animal_feeding_processed = P_crop_animal_feeding_processed,
              K_crop_animal_feeding_processed = K_crop_animal_feeding_processed,
              N_crop_animal_feeding_unprocessed = N_crop_animal_feeding_unprocessed,
              P_crop_animal_feeding_unprocessed = P_crop_animal_feeding_unprocessed,
              K_crop_animal_feeding_unprocessed = K_crop_animal_feeding_unprocessed,
              N_crop_biogas = N_crop_biogas,
              P_crop_biogas = P_crop_biogas,
              K_crop_biogas = K_crop_biogas,
              N_grassland = N_grassland,
              P_grassland = P_grassland,
              K_grassland = K_grassland,
              
              total_N_horticulture = horti_N_kg,
              total_P_horticulture = horti_P_kg,
              total_K_horticulture = horti_K_kg,
              imported_inorganic_N = imported_inorganic_N,
              imported_inorganic_P = imported_inorganic_P,
              imported_inorganic_K = imported_inorganic_K
              ))
}

# nitrogen_mc_simulation <- mcSimulation(estimate = as.estimate(crop_input),
#                                        model_function = crop_function,
#                                        numberOfModelRuns = 100,
#                                        functionSyntax = "plainNames")
# 
# nitrogen_mc_simulation$x$share_beans

# #exchange the share values of the inputs of the mc_simulation object,
# #because they were altered in the function to yield together 1
# 
# 
# nitrogen_mc_simulation$x$share_winter_wheat = nitrogen_mc_simulation$y$share_winter_wheat
# nitrogen_mc_simulation$x$share_summer_wheat = nitrogen_mc_simulation$y$share_summer_wheat
# nitrogen_mc_simulation$x$share_rye = nitrogen_mc_simulation$y$share_rye
# nitrogen_mc_simulation$x$share_winter_barley = nitrogen_mc_simulation$y$share_winter_barley
# nitrogen_mc_simulation$x$share_summer_barley = nitrogen_mc_simulation$y$share_summer_barley
# nitrogen_mc_simulation$x$share_oat = nitrogen_mc_simulation$y$share_oat
# nitrogen_mc_simulation$x$share_triticale = nitrogen_mc_simulation$y$share_triticale
# nitrogen_mc_simulation$x$share_corn = nitrogen_mc_simulation$y$share_corn
# nitrogen_mc_simulation$x$share_fodder_peas = nitrogen_mc_simulation$y$share_fodder_peas
# nitrogen_mc_simulation$x$share_beans = nitrogen_mc_simulation$y$share_beans
# nitrogen_mc_simulation$x$share_oilseed_rape = nitrogen_mc_simulation$y$share_oilseed_rape
# nitrogen_mc_simulation$x$share_potato = nitrogen_mc_simulation$y$share_potato
# nitrogen_mc_simulation$x$share_sugar_beet = nitrogen_mc_simulation$y$share_sugar_beet
# nitrogen_mc_simulation$x$share_mais_silage = nitrogen_mc_simulation$y$share_mais_silage
# 
# 
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c("N_crop_main",'N_crop_rest'),
#                    method = "smooth_simple_overlay",
#                    old_names = c("N_crop_main",'N_crop_rest'),
#                    x_axis_name = 't N  / year')
# 
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c('N_straw'),
#                    method = "smooth_simple_overlay",
#                    old_names = c('N_straw'),
#                    x_axis_name = 't N  / year')
# 
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c('N_crop_human_consumption_unprocessed'),
#                    method = "smooth_simple_overlay",
#                    old_names = c('N_crop_human_consumption_unprocessed'),
#                    x_axis_name = 't N  / year')
# 
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c('N_crop_human_consumption_processed'),
#                    method = "smooth_simple_overlay",
#                    old_names = c('N_crop_human_consumption_processed'),
#                    x_axis_name = 't N  / year')
# 
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c('N_crop_animal_feeding_processed','N_crop_animal_feeding_unprocessed'),
#                    method = "smooth_simple_overlay",
#                    old_names = c('N_crop_animal_feeding_processed','N_crop_animal_feeding_unprocessed'),
#                    x_axis_name = 't N  / year')
# 
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c('N_crop_biogas'),
#                    method = "smooth_simple_overlay",
#                    old_names = c('N_crop_biogas'),
#                    x_axis_name = 't N  / year')
# 
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c('N_grassland'),
#                    method = "smooth_simple_overlay",
#                    old_names = c('N_grassland'),
#                    x_axis_name = 't N  / year')
# 
# 
# pls_result <- plsr.mcSimulation(object = nitrogen_mc_simulation,
#                                 resultName = names(nitrogen_mc_simulation$y['N_crop_main']), ncomp = 1)
# 
# plot_pls(pls_result, input = crop_input, threshold = 0.8)
# mcSimulation_table <- data.frame(nitrogen_mc_simulation$x, nitrogen_mc_simulation$y[c('N_crop_main')]
# )
# 
# #evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "N_crop_main")
# #plot_evpi(evpi, decision_vars = "N_crop_main")


