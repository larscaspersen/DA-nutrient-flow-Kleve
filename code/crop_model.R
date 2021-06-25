#try out how to solve the problem of several percantages in input table 
#which need to add up to 1

library(decisionSupport)

crop_input <- read.csv('data/crop_input.csv')

make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

crop_input$lower <- as.numeric(crop_input$lower)
crop_input$upper <- as.numeric(crop_input$upper)

make_variables(as.estimate(crop_input),n=1)

crop_function <- function(){

  estimated_crop_land <- share_beans + share_corn + share_fodder_peas + 
    share_mais_silage + share_oat + 
    share_oilseed_rape + share_potato +share_rye + share_sugar_beat + 
    share_summer_barley + share_summer_wheat + 
    share_triticale + share_winter_barley + share_winter_wheat
  
  correction_factor <- share_crop_land / estimated_crop_land
  
  share_beans <- share_beans * correction_factor
  share_corn <- correction_factor * share_corn
  share_fodder_peas <- correction_factor * share_fodder_peas
  share_mais_silage <- correction_factor * share_mais_silage
  share_oat <- correction_factor * share_oat
  share_oilseed_rape <- correction_factor * share_oilseed_rape
  share_potato <- correction_factor * share_potato
  share_rye <- correction_factor * share_rye
  share_sugar_beat <- correction_factor * share_sugar_beat
  share_summer_barley <- correction_factor * share_summer_barley
  share_summer_wheat <- correction_factor * share_summer_wheat
  share_triticale <- correction_factor * share_triticale
  share_winter_barley <- correction_factor *  share_winter_barley
  share_winter_wheat <- correction_factor * share_winter_wheat
  
  #####
  #I wonder if this correctio pushes values outside their upper and lower limits? 
  #####
  
  round(sum(share_beans, share_corn, share_fodder_peas, share_mais_silage, 
            share_oat, share_oilseed_rape, share_potato, share_rye, 
            share_sugar_beat, share_summer_barley, share_summer_wheat, 
            share_triticale, share_winter_barley, share_winter_wheat),3) == share_crop_land
  
  #bind to data frame
  crop_df <- data.frame(crop = c('bean','corn', 'fodder_peas', 'mais_silage', 'oat',
                                  'oilseed_rape', 'potato', 'rye', 'sugar_beet', 
                                  'summer_barley', 'summer_wheat', 'triticale', 
                                  'winter_barley', 'winter_wheat'),
                        land_share = c(share_beans,share_corn, share_fodder_peas,
                          share_mais_silage, share_oat, share_oilseed_rape,
                          share_potato, share_rye, share_sugar_beat,
                          share_summer_barley, share_summer_wheat,
                          share_triticale, share_winter_barley, share_winter_wheat),
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
  crop_df$yield_total <- crop_df$yield * crop_df$land_absolute 
  
  #get N of consumable part
  crop_df$N_main <- crop_df$yield_total * crop_df$yield_share * crop_df$N_yield
  
  #distributute main N by animal and human consumption and by with or without processing ----
  
  #human consumption with processing
  crop_df$N_crop_human_consumption_processed <- crop_df$N_main * crop_df$share_to_consumption * crop_df$through_processing
  
  #human consumption without processing
  crop_df$N_crop_human_consumption_unprocessed <- crop_df$N_main * crop_df$share_to_consumption * (1 - crop_df$through_processing)
  
  #to animal consumption without processing
  crop_df$N_crop_animal_feeding_processed <- crop_df$N_main * crop_df$share_to_animal * crop_df$through_processing
  
  #to animal consumption without processing
  crop_df$N_crop_animal_feeding_unprocessed <- crop_df$N_main * crop_df$share_to_animal * ( 1 - crop_df$through_processing )
  
  #to biogas
  crop_df$N_crop_biogas <- crop_df$N_main * crop_df$share_to_biogas 
  
  #get N of not consumable part
  crop_df$N_rest <- crop_df$yield_total * (1-crop_df$yield_share) * crop_df$N_leftover
  
  #calculate N in straw which is used for animal bedding
  crop_df$N_straw <- crop_df$N_rest * crop_df$share_leftover_straw
  
  #calculate N of crop which goes to animal feeding
  
  #sum up the columns of interest ----
  N_crop_main <- sum(crop_df$N_main)
  
  N_crop_rest <- sum(crop_df$N_rest)
  
  N_straw <- sum(crop_df$N_straw)
  
  N_crop_human_consumption_processed <- sum(crop_df$N_crop_human_consumption_processed)
  
  N_crop_human_consumption_unprocessed <- sum(crop_df$N_crop_human_consumption_unprocessed)
  
  N_crop_animal_feeding_processed <- sum(crop_df$N_crop_animal_feeding_processed)
  
  N_crop_animal_feeding_unprocessed <- sum(crop_df$N_crop_animal_feeding_unprocessed)
  
  N_crop_biogas <- sum(crop_df$N_crop_biogas)
  
  
  # grassland ----
  N_grazing <- area_grassland * share_grazing * N_yield_grazing
  
  N_mowing <- area_grassland * (1 - share_grazing) * N_yield_mowing
  
  N_grassland <- N_grazing + N_mowing
  
  
  
  
  
  return(list(N_crop_main = N_crop_main, N_crop_rest = N_crop_rest,
              N_straw = N_straw, N_crop_human_consumption_processed = N_crop_human_consumption_processed,
              N_crop_human_consumption_unprocessed = N_crop_human_consumption_unprocessed,
              N_crop_animal_feeding_processed = N_crop_animal_feeding_processed,
              N_crop_animal_feeding_unprocessed = N_crop_animal_feeding_unprocessed,
              N_crop_biogas = N_crop_biogas,
              N_grassland = N_grassland,
              share_beans = share_beans, share_corn = share_corn,
              share_fodder_peas = share_fodder_peas, share_mais_silage = share_mais_silage,
              share_oat = share_oat, share_oilseed_rape = share_oilseed_rape,
              share_potato = share_potato, share_rye = share_rye, 
              share_sugar_beat = share_sugar_beat, share_summer_barley = share_summer_barley,
              share_summer_wheat = share_summer_wheat, share_triticale = share_triticale,
              share_winter_barley = share_winter_barley, share_winter_wheat = share_winter_wheat))
}
# 
# nitrogen_mc_simulation <- mcSimulation(estimate = as.estimate(crop_input),
#                                        model_function = crop_function,
#                                        numberOfModelRuns = 10000,
#                                        functionSyntax = "plainNames")
# 
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
# nitrogen_mc_simulation$x$share_sugar_beat = nitrogen_mc_simulation$y$share_sugar_beat
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
