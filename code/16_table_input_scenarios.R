#have a table with median values for how the scenario played out

#number of animals: total Livestock units
#                   change in livestock units for the scenarios

#number of cows: total livestock units
#               change in livestock units (%)
#same for chicken, pig and others

#allocation of crop to biogas
#allocation of crop to animal
#allocation of crop to local consumption

#allocation of manure to biogas
#allocation of manure to crop
#allocation of manure to export

library(decisionSupport)
library(tidyverse)

#run nutrient flow simulations
source('code/06_combine_submodels.R')

# decide what to return
#return_flows <- TRUE

n_runs <- 10000

return_alt_output <- TRUE

# start <- Sys.time()
# # let mc simulation run, just to test if everything works out
# nitrogen_mc_simulation <- mcSimulation(
#   estimate = as.estimate(input),
#   model_function = combined_function,
#   numberOfModelRuns = n_runs,
#   functionSyntax = "plainNames"
# )
# end <- Sys.time()
# end - start
# 
# saveRDS(nitrogen_mc_simulation, file = 'data/alternative_output.rds')

nitrogen_mc_simulation <- readRDS('data/alternative_output.rds')

n_outcomes <- sum(grepl('current_share_cattle', colnames(nitrogen_mc_simulation$y)))

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

combined_results$scenario <- rep(c('Ref', 'PS', 'LBS', 'CBS', 'Trad'), each = n_runs)
combined_results$run <- rep(1:n_runs, n_outcomes)

#change the name of the scenarios, drop sh2
#combined_results <- combined_results[!(combined_results$scenario %in% c('all_adjustments_sh_2_stakeholder_reduction', 
#                                                                        'all_adjustments_sh_2_strict_reduction',
#                                                                        'buffer_no_herdsize_sh_2')),]


combined_results %>% 
  reshape2::melt(id.vars = c('scenario', 'run')) %>% 
  reshape2::dcast(run + variable ~ scenario, value.var = 'value') %>% 
  filter(variable == 'crop_unprocessed_feed')

#bring to long format and summarize
table_sum <- combined_results %>% 
  filter(scenario != 'Trad') %>% 
  reshape2::melt(id.vars = c('scenario', 'run')) %>% 
  group_by(scenario, variable) %>% 
  summarise(median = median(value),
            iqr = IQR(value),
            q_16 = quantile(value, probs = 0.16),
            q_86 = quantile(value, probs = 0.84)) 

test <- table_sum %>%  
  reshape2::melt(id.vars = c('scenario', 'variable'), variable.name = 'measurement') %>%
  reshape2::dcast(variable + measurement ~ scenario, value.var = 'value') %>% 
  filter(measurement == 'median') %>% 
  mutate(CBS_percent_change = round(((CBS - Ref) / Ref) * 100, 1),
         LBS_percent_change = round(((LBS - Ref) / Ref) * 100, 1),
         PS_percent_change = round(((PS - Ref) / Ref) * 100))

write.csv(test, 'data/table_scenario_inputs.csv', row.names = FALSE)




combined_results %>% 
  reshape2::melt(id.vars = c('scenario', 'run')) %>% 
  reshape2::dcast(run + variable ~ scenario, value.var = 'value') %>% 
  filter(variable == 'crop_unprocessed_feed') %>% 
  group_by(variable) %>% 
  summarise(same = sum((CBS == LBS & CBS == PS & CBS == Ref )) / n(),
            lower = sum((CBS == LBS & CBS == PS & CBS < Ref )) / n())

#in 51% of the cases the values are the same
#in rest of the cases the values are lower (because we reduced the animal numbers so much that we had to reduce the unprocessed feed)