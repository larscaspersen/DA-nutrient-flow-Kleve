#extract information like median, 75% and 25% 
#also extract information like % positive

library(tidyverse)
library(ggridges)
library(scales) #to have non-scientific numbers for the x and y axis

#read saved runs for flows and for indicators

result_flows <- readRDS('data/model_result_flows.rds')


#combine meat and dairy stuff
result_flows <- purrr::map(result_flows, function(x){
  #x$import_animal_products_N <- x$import_dairy_egg_N + x$import_meat_N
  x$export_animal_products_N <- x$export_dairy_egg_N + x$export_meat_N
  
  #x$import_animal_products_P <- x$import_dairy_egg_P + x$import_meat_P
  x$export_animal_products_P <- x$export_dairy_egg_P + x$export_meat_P
  
  #x$import_animal_products_K <- x$import_dairy_egg_K + x$import_meat_K
  x$export_animal_products_K <- x$export_dairy_egg_K + x$export_meat_K
  
  
  
  
  
  #adjust the use efficiency calculations
  x$use_efficiency_N <- ((x$feed_crops_N +
                            x$straw_N +
                            x$grassbased_feed_N +
                            x$food_and_feed_crops_N +
                            x$fruit_and_vegetable_N +
                            x$egg_and_dairy_N +
                            x$vegetal_biogas_substrate_N +
                            x$slaughter_animal_N) /
                           (x$manure_to_crop_N +
                              x$net_feed_import_N +
                              x$import_inorganic_fertilizer_N +
                              x$feed_crops_N +
                              x$grassbased_feed_N +
                              x$digestate_N +
                              x$import_organic_fertilizer_N +
                              x$feed_from_processed_crops_N +
                              x$fresh_compost_crop_N +
                              x$sewage_N +
                              x$straw_N)) * 100
  
  x$use_efficiency_P <- ((x$feed_crops_P +
                            x$straw_P +
                            x$grassbased_feed_P +
                            x$food_and_feed_crops_P +
                            x$fruit_and_vegetable_P +
                            x$egg_and_dairy_P +
                            x$vegetal_biogas_substrate_P +
                            x$slaughter_animal_P) /
                           (x$manure_to_crop_P +
                              x$net_feed_import_P +
                              x$import_inorganic_fertilizer_P +
                              x$feed_crops_P +
                              x$grassbased_feed_P +
                              x$digestate_P +
                              x$import_organic_fertilizer_P +
                              x$feed_from_processed_crops_P +
                              x$fresh_compost_crop_P +
                              x$sewage_P +
                              x$straw_P)) * 100
  
  x$use_efficiency_K <- ((x$feed_crops_K +
                            x$straw_K +
                            x$grassbased_feed_K +
                            x$food_and_feed_crops_K +
                            x$fruit_and_vegetable_K +
                            x$egg_and_dairy_K +
                            x$vegetal_biogas_substrate_K +
                            x$slaughter_animal_K) /
                           (x$manure_to_crop_K +
                              x$net_feed_import_K +
                              x$import_inorganic_fertilizer_K +
                              x$feed_crops_K +
                              x$grassbased_feed_K +
                              x$digestate_K +
                              x$import_organic_fertilizer_K +
                              x$feed_from_processed_crops_K +
                              x$fresh_compost_crop_K +
                              x$sewage_K +
                              x$straw_K)) * 100
  
  
  
  
  
  
  
  
  
  
  
  
  return(x)
})

diff_flows_df <- rbind.data.frame(result_flows$interventions[-1] - result_flows$reference_year[-1],
                                  result_flows$interventions_animal_adjusted[-1] - result_flows$reference_year[-1],
                                  result_flows$interventions_crop_adjusted[-1] - result_flows$reference_year[-1])


diff_flows_df$scenario <- c(result_flows$interventions$scenario, 
                            result_flows$interventions_animal_adjusted$scenario, 
                            result_flows$interventions_crop_adjusted$scenario)

diff_flows_df <- dplyr::relocate(diff_flows_df, scenario)


result_flows <- do.call(rbind, result_flows)

#change names of scenarios
result_flows$scenario <- factor(result_flows$scenario, levels = c("reference_year","interventions","interventions_animal_adjusted", "interventions_crop_adjusted"),
                                labels = c('Ref', 'PS', 'LBS' ,'CBS'))

diff_flows_df$scenario <- factor(diff_flows_df$scenario, levels = c("reference_year","interventions","interventions_animal_adjusted", "interventions_crop_adjusted"),
                                 labels = c('Ref', 'PS', 'LBS' ,'CBS'))



#bring results in long format, bring differences in long format
result_flows_long <- reshape2::melt(result_flows, id.var = 'scenario')
diff_flows_long <- reshape::melt(diff_flows_df, id.var = 'scenario')


result_flows_long <- na.omit(result_flows_long)
diff_flows_long <- na.omit(diff_flows_long)

#split variable name into variable and nutrient
diff_flows_long <- tidyr::separate(data = diff_flows_long, col = variable, sep = -1, convert = TRUE, into = c('variable', 'nutrient'))
result_flows_long <- tidyr::separate(data = result_flows_long, col = variable, sep = -1, convert = TRUE, into = c('variable', 'nutrient'))

#remove the trailing _ in variable
diff_flows_long <- diff_flows_long %>% 
  mutate(variable = substring(variable, 1, nchar(variable)-1))
result_flows_long <- result_flows_long %>% 
  mutate(variable = substring(variable, 1, nchar(variable)-1))

results_indicators_long <- result_flows_long %>% 
  filter(variable %in% c("total_input", 'losses', "recycling_rate", "share_reuse_to_total_input", "use_efficiency"))


result_flows_long <- result_flows_long %>% 
  filter(!variable %in% c("total_input", 'losses', "recycling_rate", "share_reuse_to_total_input", "use_efficiency"))

results_indicators_long$variable <- factor(results_indicators_long$variable, 
                                           levels = c("total_input", 'losses', "recycling_rate", "share_reuse_to_total_input", "use_efficiency"),
                                           labels = c('Total Input', 'Losses', 'Recycling Rate', 'Reuse to Total Input', 'Use Efficiency'))





flow_sum <-  result_flows_long %>% 
  mutate(value = round(value  / 1000, digits = 3)) %>% 
  group_by(scenario, variable, nutrient) %>% 
  summarise(quan_25 = quantile(value, probs = 0.25),
           median = median(value),
           quan_75 = quantile(value, probs = 0.75),
           share_neg = sum(value < 0) / n(),
           share_zero = sum(value == 0) / n())

indicator_sum <- results_indicators_long %>% 
  mutate(value = round (value / 1000, digits = 3)) %>% 
  group_by(scenario, variable, nutrient) %>% 
  summarise(quan_25 = quantile(value, probs = 0.25),
            median = median(value),
            quan_75 = quantile(value, probs = 0.75),
            share_neg = sum(value < 0) / n(),
            share_zero = sum(value == 0) / n())

write.csv(indicator_sum, file = 'data/result_indicator_summarized.csv', row.names = F)
write.csv(flow_sum, file = 'data/result_flow_summarized.csv', row.names = F)
