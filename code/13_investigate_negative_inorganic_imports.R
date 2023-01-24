#investigate mysterious negative imports of inorganic fertilizer

#read raw model output
nitrogen_mc_simulation <- readRDS('data/model_output_flows.rds')
#read processed model output
combined_results <- readRDS('data/model_result_flows.rds')

#extract y
x <- nitrogen_mc_simulation$x
n_runs <- nrow(nitrogen_mc_simulation$y)

#remove not needed object
rm(nitrogen_mc_simulation)

library(tidyverse)
combined_results <- bind_rows(combined_results, .id = 'scenario')

#add information on the run
combined_results$run <- rep(1:n_runs, 5)

#melt data.frame
combined_results_long <- reshape2::melt(combined_results, id.vars = c('scenario', 'run'))

#split character for var into flow and nutrient
combined_results_long <- tidyr::separate(data = combined_results_long, col = variable, sep = -1, convert = TRUE, into = c('flow', 'nutrient'))

#remove trailing _ in flow
combined_results_long$flow <- gsub('.{1}$', '', combined_results_long$flow)

#use dcast on the long results
combined_results <- reshape2::dcast(combined_results_long, formula = scenario + run + nutrient ~ flow)



########
#mysterious negative import of inorganic fertilizer

#K
run <- combined_results_long %>% 
  filter(nutrient == 'P', flow == 'import_inorganic_fertilizer', scenario == 'interventions_crop_adjusted') %>% 
  filter(value <= 0) %>% 
  select(run) %>% 
  unlist() %>% 
  unname()

#levels(as.factor(combined_results$scenario))

#scenario_allocate_crop_biogas_a
#scenario_overall_livestock_reduction_a
#scenario_allocate_manure_biogas_a
#scenario_allocate_manure_crop_a

sub_run <- x[run, c('scenario_allocate_crop_biogas_a', 'scenario_overall_livestock_reduction_a',
          'scenario_allocate_manure_biogas_a', 'scenario_allocate_manure_crop_a')] %>% 
  reshape2::melt()


p1 <- x %>% 
  reshape2::melt() %>% 
  filter(variable %in% c('scenario_allocate_crop_biogas_a', 'scenario_overall_livestock_reduction_a',
                         'scenario_allocate_manure_biogas_a', 'scenario_allocate_manure_crop_a')) %>% 
  ggplot(aes(x = variable, y = value)) +
  geom_violin()

p1 +
  geom_jitter(data = sub_run, aes(x = variable, y = value), alpha = 0.1, width = 0.1)


target <- combined_results_long %>% 
  filter(nutrient == 'P', flow == 'import_inorganic_fertilizer', scenario == 'interventions_crop_adjusted') %>% 
  select(value) %>% 
  unlist() %>% 
  unname()

cor_treshold <- 0.7

#maybe use correlation?
cor_out <- cor(x = x, y = target) 

can <- which(abs(cor_out) > cor_treshold)

colnames(x)[can]
#--> there is only a clear (linear) relationship with livestock reduction





levels(as.factor(combined_results$scenario))

#find the cases in which we supply more organic nutrient in the scenario than in the reference year
test <- combined_results %>% 
  mutate(to_crops = manure_to_crop + digestate)


prob_runs <- test$run[(test$to_crops - test$to_crops[test$scenario == 'reference_year']) > test$import_inorganic_fertilizer[test$scenario == 'reference_year']]
  
neg_import_runs <-test$run[test$import_inorganic_fertilizer < 0]

#all the runs with negative inorganic fertilizer runs also had more 
#nutrients allocated to crops than there are imports of inorganic fertilizer 

#--> should I try to fix that?


