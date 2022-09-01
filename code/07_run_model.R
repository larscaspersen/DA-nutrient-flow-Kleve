#run nutrient flow simulations
source('code/06_combine_submodels.R')

# decide what to return
return_flows <- TRUE

# let mc simulation run, just to test if everything works out
nitrogen_mc_simulation <- mcSimulation(
  estimate = as.estimate(input),
  model_function = combined_function,
  numberOfModelRuns = 100,
  functionSyntax = "plainNames"
)

#everything with same digit at the end of the name belongs together
#eg scenario1, sewageN1, ....
#the different numbers come from the scenarios, the stakeholders answers and the strict reductions


#for (n in 1:100) {
#  make_variables(as.estimate(input))
#  combined_function()
#}

#problem_levers <- read.csv("problem_levers.csv")


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

nitrogen_mc_simulation$y

# somehow number were characters, so change back to numeric
n_y <- length(nitrogen_mc_simulation$y)
nitrogen_mc_simulation$y[, 2:n_y] <- sapply(nitrogen_mc_simulation$y[, 2:n_y], as.numeric)


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
