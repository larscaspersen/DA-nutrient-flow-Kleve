library(decisionSupport)
library(tidyverse)

result_flows <- readRDS('data/model_result_flows.rds')
#result_indicators <- readRDS('data/model_result_indicators.rds')
result_flows <- do.call(rbind, result_flows)

#change names of scenarios
result_flows$scenario <- factor(result_flows$scenario, levels = c("reference_year","interventions","interventions_animal_adjusted", "interventions_crop_adjusted"),
                                labels = c('Ref', 'PS', 'LBS' ,'CBS'))

result_flows <- na.omit(result_flows)

result_flows$run <- rep(1:10000, 4)


#bring results in long format, bring differences in long format
result_flows_long <- reshape2::melt(result_flows, id.var = c('scenario', 'run'))

result_flows_long <- na.omit(result_flows_long)

result_flows_long <- result_flows_long %>% 
  filter(variable %in% c('manure_to_crop_N',
                         'manure_as_biogas_substrate_N',
                         'manure_export_N',
                         'vegetal_biogas_substrate_N',
                         'local_vegetal_products_consumed_N',
                         'feed_from_processed_crops_N',
                         'feed_crops_N',
                         'export_vegetable_N',
                         'current_share_cattle',
                         'current_share_pig',
                         'current_share_poultry',
                         'current_share_others',
                         'reduction_LLU',
                         'stakeholder_allocate_crop_biogas_corrected',
                         'stakeholder_allocate_crop_feed_corrected',
                         'stakeholder_allocate_crop_food_corrected',
                         'stakeholder_allocate_manure_biogas_corrected',
                         'stakeholder_allocate_manure_crop_corrected',
                         'stakeholder_allocate_manure_export_corrected',
                         'LLU_total_adjusted'))


# combined_results <- result_flows_long %>% 
#   reshape2::melt(id.vars = c('scenario', 'run')) %>% 
#   reshape2::dcast(run + variable ~ scenario, value.var = 'value') 

#bring to long format and summarize
table_sum <- result_flows_long %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(scenario, variable) %>% 
  summarise(median = median(value, na.rm = TRUE),
            iqr = IQR(value, na.rm = TRUE),
            q_16 = quantile(value, probs = 0.16, na.rm = TRUE),
            q_86 = quantile(value, probs = 0.84, na.rm = TRUE)) 

test <- table_sum %>%  
  reshape2::melt(id.vars = c('scenario', 'variable'), variable.name = 'measurement') %>%
  reshape2::dcast(variable + measurement ~ scenario, value.var = 'value') %>% 
  filter(measurement == 'median') %>% 
  mutate(CBS_percent_change = round(((CBS - Ref) / Ref) * 100, 1),
         LBS_percent_change = round(((LBS - Ref) / Ref) * 100, 1),
         PS_percent_change = round(((PS - Ref) / Ref) * 100))

write.csv(test, 'data/table_scenario_inputs.csv', row.names = FALSE)




result_flows_long %>% 
  reshape2::dcast(run + variable ~ scenario, value.var = 'value') %>% 
  filter(variable == 'feed_crops_N') %>% 
  group_by(variable) %>% 
  summarise(same = sum((CBS == LBS & CBS == PS & CBS == Ref )) / n(),
            lower = sum((CBS == LBS & CBS == PS & CBS < Ref )) / n())

#in 51% of the cases the values are the same
#in rest of the cases the values are lower (because we reduced the animal numbers so much that we had to reduce the unprocessed feed)