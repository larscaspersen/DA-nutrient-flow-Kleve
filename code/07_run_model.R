library(decisionSupport)

#run nutrient flow simulations
source('code/06_combine_submodels.R')

# decide what to return
#return_flows <- TRUE

n_runs <- 10000


start <- Sys.time()
# let mc simulation run, just to test if everything works out
nitrogen_mc_simulation <- mcSimulation(
  estimate = as.estimate(input),
  model_function = combined_function,
  numberOfModelRuns = n_runs,
  functionSyntax = "plainNames"
)
end <- Sys.time()
end - start


#bring stakeholder_allocation to the inputs, not the outputs of the model
nitrogen_mc_simulation$x$scenario_allocate_crop_feed_a <- as.numeric(nitrogen_mc_simulation$y[,'scenario_allocate_crop_feed_a1'])
nitrogen_mc_simulation$x$scenario_allocate_crop_food_a <- as.numeric(nitrogen_mc_simulation$y[,'scenario_allocate_crop_food_a1'])
nitrogen_mc_simulation$x$scenario_allocate_manure_biogas_a <- as.numeric(nitrogen_mc_simulation$y[,'scenario_allocate_manure_biogas_a1'])
nitrogen_mc_simulation$x$scenario_allocate_manure_export_a <- as.numeric(nitrogen_mc_simulation$y[,'scenario_allocate_manure_export_a1'])
nitrogen_mc_simulation$x$scenario_allocate_manure_crop_a <- as.numeric(nitrogen_mc_simulation$y[,'scenario_allocate_manure_crop_a1'])


#find which position columns have
drop_col <-  which(colnames(nitrogen_mc_simulation$y) %in% c('scenario_allocate_crop_feed_a1', 'scenario_allocate_crop_food_a1',
                                          'scenario_allocate_manure_biogas_a1', 'scenario_allocate_manure_export_a1',
                                          'scenario_allocate_manure_crop_a1',
                                          'scenario_allocate_crop_feed_a2', 'scenario_allocate_crop_food_a2',
                                          'scenario_allocate_manure_biogas_a2', 'scenario_allocate_manure_export_a2',
                                          'scenario_allocate_manure_crop_a2',
                                          'scenario_allocate_crop_feed_a3', 'scenario_allocate_crop_food_a3',
                                          'scenario_allocate_manure_biogas_a3', 'scenario_allocate_manure_export_a3',
                                          'scenario_allocate_manure_crop_a3',
                                          'scenario_allocate_crop_feed_a4', 'scenario_allocate_crop_food_a4',
                                          'scenario_allocate_manure_biogas_a4', 'scenario_allocate_manure_export_a4',
                                          'scenario_allocate_manure_crop_a4',
                                          'scenario_allocate_crop_feed_a5', 'scenario_allocate_crop_food_a5',
                                          'scenario_allocate_manure_biogas_a5', 'scenario_allocate_manure_export_a5',
                                          'scenario_allocate_manure_crop_a5'))

#remove the stuff from the y part
nitrogen_mc_simulation$y <- subset(nitrogen_mc_simulation$y, select = -drop_col)



#saveRDS(nitrogen_mc_simulation, file = 'data/model_output_flows.rds')

saveRDS(nitrogen_mc_simulation, file = 'data/new_run_model_output_flows.rds')

#nitrogen_mc_simulation <- readRDS('data/model_output_flows.rds')


#everything with same digit at the end of the name belongs together
#eg scenario1, sewageN1, ....
#the different numbers come from the scenarios, the stakeholders answers and the strict reductions

#get number of outcomes
n_outcomes <- sum(grepl('scenario', colnames(nitrogen_mc_simulation$y)))

#bind items with same characters but different numbers
flow_names <- unique(gsub('[0-9]+', '', colnames(nitrogen_mc_simulation$y)))


#create empty data.frame with the right dimensions
combined_results <- data.frame(matrix(nrow = n_runs * n_outcomes, ncol = length(flow_names)))

for(i in 1:length(flow_names)){
  #take variable names
  target_names <- colnames(nitrogen_mc_simulation$y)[flow_names[i] == gsub('[0-9]+', '', colnames(nitrogen_mc_simulation$y))]
  #append to data frame
  combined_results[,i] <- unlist(nitrogen_mc_simulation$y[target_names],use.names = FALSE)
}

#adjust the column names
colnames(combined_results) <- flow_names


#change the name of the scenarios, drop sh2
#combined_results <- combined_results[!(combined_results$scenario %in% c('all_adjustments_sh_2_stakeholder_reduction', 
#                                                                        'all_adjustments_sh_2_strict_reduction',
#                                                                        'buffer_no_herdsize_sh_2')),]

combined_results$scenario <- as.factor(combined_results$scenario)

levels(combined_results$scenario)

levels(combined_results$scenario) <- list(reference_year  = "normal", 
                                          interventions = "all_adjustments_sh_1",
                                          interventions_animal_adjusted = 'animal_buffered_sh_1',
                                          interventions_crop_adjusted = 'buffer_no_herdsize_sh_1',
                                          traditional_agriculture = "back_to_roots") 
#split the results the different scenarios? In the end the scenarios are not a result but an input
result_list <- split(x = combined_results, f = combined_results$scenario)

#change columns to numeric
result_list$reference_year[,-1] <- lapply(result_list$reference_year[,-1], as.numeric)
result_list$interventions[,-1] <- lapply(result_list$interventions[,-1], as.numeric)
result_list$interventions_animal_adjusted[,-1] <- lapply(result_list$interventions_animal_adjusted[,-1], as.numeric)
result_list$interventions_crop_adjusted[,-1] <- lapply(result_list$interventions_crop_adjusted[,-1], as.numeric)
result_list$traditional_agriculture[,-1] <- lapply(result_list$traditional_agriculture[,-1], as.numeric)

#save result list
#saveRDS(result_list, file = 'data/model_result_flows.rds')
saveRDS(result_list, file = 'data/new_run_model_result_flows.rds')
