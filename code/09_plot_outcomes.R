#setwd('../DA-job/DA-nutrient-flow-Kleve/')
library(tidyverse)
library(ggridges)

plot_flows <- TRUE

#read saved runs for flows and for indicators
result_flows <- readRDS('data/model_result_flows.rds')
result_indicators <- readRDS('data/model_result_indicators.rds')

diff_flows_df <- rbind.data.frame(result_flows$interventions[-1] - result_flows$reference_year[-1],
                                  result_flows$interventions_animal_adjusted[-1] - result_flows$reference_year[-1],
                                  result_flows$interventions_crop_adjusted[-1] - result_flows$reference_year[-1],
                                  result_flows$traditional_agriculture[-1] - result_flows$reference_year[-1])
diff_indicators_df <- rbind.data.frame(result_indicators$interventions[-1] - result_indicators$reference_year[-1],
                                       result_indicators$interventions_animal_adjusted[-1] - result_indicators$reference_year[-1],
                                       result_indicators$interventions_crop_adjusted[-1] - result_indicators$reference_year[-1],
                                       result_indicators$traditional_agriculture[-1] - result_indicators$reference_year[-1])


diff_flows_df$scenario <- c(result_flows$interventions$scenario, 
                            result_flows$interventions_animal_adjusted$scenario, 
                            result_flows$interventions_crop_adjusted$scenario,
                            result_flows$traditional_agriculture$scenario)

diff_flows_df <- dplyr::relocate(diff_flows_df, scenario)
diff_indicators_df$scenario <- c(result_indicators$interventions$scenario, 
                                 result_indicators$interventions_animal_adjusted$scenario, 
                                 result_indicators$interventions_crop_adjusted$scenario,
                                 result_indicators$traditional_agriculture$scenario)
diff_indicators_df <- dplyr::relocate(diff_indicators_df, scenario)



#--> list contains model results split by the scenarios

#check the distributions

result_flows <- do.call(rbind, result_flows)
result_indicators <- do.call(rbind, result_indicators)

#change names of scenarios
result_flows$scenario <- factor(result_flows$scenario, levels = c("reference_year","interventions","interventions_animal_adjusted", "interventions_crop_adjusted", "traditional_agriculture"),
       labels = c('Ref', 'PS', 'LBS' ,'CBS', "TA"))
result_indicators$scenario <- factor(result_indicators$scenario, levels = c("reference_year","interventions","interventions_animal_adjusted", "interventions_crop_adjusted", "traditional_agriculture"),
                                labels = c('Ref', 'PS', 'LBS' ,'CBS', "TA"))

diff_flows_df$scenario <- factor(diff_flows_df$scenario, levels = c("reference_year","interventions","interventions_animal_adjusted", "interventions_crop_adjusted", "traditional_agriculture"),
       labels = c('Ref', 'PS', 'LBS' ,'CBS', "TA"))
diff_indicators_df$scenario <- factor(diff_indicators_df$scenario, levels = c("reference_year","interventions","interventions_animal_adjusted", "interventions_crop_adjusted", "traditional_agriculture"),
       labels = c('Ref', 'PS', 'LBS' ,'CBS', "TA"))


#bring results in long format, bring differences in long format
result_flows_long <- reshape2::melt(result_flows, id.var = 'scenario')
diff_flows_long <- reshape::melt(diff_flows_df, id.var = 'scenario')

#melt combined outputs
results_indicators_long <- reshape2::melt(result_indicators, id.var = 'scenario')
diff_indicators_long <- reshape::melt(diff_indicators_df, id.var = 'scenario')






if(plot_flows){
  #visualisation for individual flows
  
  #melt combined outputs

  
  boring_streams <- c('animal_housing_and_storage_losses_K','animal_housing_and_storage_losses_P',
                      'compost_to_consumption_N','compost_to_consumption_P','compost_to_consumption_K',
                      'fresh_compost_crop_N', 'fresh_compost_crop_P','fresh_compost_crop_K',
                      'fresh_compost_export_N', 'fresh_compost_export_P', 'fresh_compost_export_K',
                      'fruit_and_vegetable_N','fruit_and_vegetable_P','fruit_and_vegetable_K',
                      'grassbased_feed_N','grassbased_feed_P','grassbased_feed_K',
                      'import_OFMSW_N','import_OFMSW_P','import_OFMSW_K',
                      'ofmsw_N','ofmsw_P','ofmsw_K',
                      'ofmsw_residual_waste_N','ofmsw_residual_waste_P','ofmsw_residual_waste_K',
                      'other_organic_fertilizer_export_N','other_organic_fertilizer_export_P','other_organic_fertilizer_export_K',
                      'sewage_N', 'sewage_P', 'sewage_K',
                      'sewage_sludge_export_N','sewage_sludge_export_P', 'sewage_sludge_export_K',
                      'sewage_to_crop_N','sewage_to_crop_P', 'sewage_to_crop_K',
                      'straw_N', 'straw_P', 'straw_K',
                      'wastewater_direct_discharge_N', 'wastewater_direct_discharge_P', 'wastewater_direct_discharge_K',
                      'wastewater_effluent_gaseous_losses_N', 'wastewater_effluent_gaseous_losses_P', 'wastewater_effluent_gaseous_losses_K')
  for(flow in unique(result_flows_long$variable)){
    
    #density for all flows
    p1 <- result_flows_long %>%
      filter(variable == flow) %>%
      ggplot(aes(x=as.numeric(value) ,y = scenario, fill = scenario)) +
      geom_density_ridges_gradient(scale=2) +
      xlab(paste0(flow, ' [t per year]'))+
      ylab('')+
      theme_bw() +
      theme(legend.position = "none")
    
    if(flow %in% boring_streams){
      pic_path <- 'figures/flows/distributions/boring_streams/'
    } else {
      pic_path <- 'figures/flows/distributions'
    }
    
    fname <- paste0(flow,'.jpg')
    ggsave(p1,  filename = fname, path = pic_path,  device = 'jpeg', width = 10, height = 7, units = 'cm')
    
    
    #density of difference to baseline
    p2 <- diff_flows_long %>%
      filter(variable == flow) %>%
      ggplot(aes(x=as.numeric(value) ,y = scenario, fill = scenario)) +
      geom_density_ridges_gradient(scale=2) +
      xlab(paste0(flow, ' [t per year]'))+
      ylab('')+
      theme_bw() +
      theme(legend.position = "none")
    
    if(flow %in% boring_streams){
      pic_path <- 'figures/flows/distribution_difference_baseline/boring_streams/'
    } else {
      pic_path <- 'figures/flows/distribution_difference_baseline/'
    }
    
    fname <- paste0(flow,'.jpg')
    ggsave(p2,  filename = fname, path = pic_path,  device = 'jpeg', width = 10, height = 7, units = 'cm')
    
    
    
    
    # #pls analysis
    # for(scenario in levels(combined_results$scenario)){
    #   
    #   if(flow %in% boring_streams){
    #     break()
    #   }
    #   
    #   #put only the corresponding scenario values to the simulation output, drop the scenario column
    #   nitrogen_mc_simulation$y <- combined_results[combined_results$scenario == scenario, -1]
    #   
    #   #do pls
    #   pls_result <- plsr.mcSimulation(
    #     object = nitrogen_mc_simulation,
    #     resultName = names(nitrogen_mc_simulation$y[flow]), ncomp = 1
    #   )
    #   
    #   if(all(is.na(pls_result$coefficients)) == TRUE){
    #     next()
    #   }
    #   
    #   pls_plot <- plot_pls(pls_result, input_table = input, threshold = 0.9)
    #   
    #   if(flow %in% boring_streams){
    #   
    #     pic_path <- 'figures/flows/pls/boring_streams/'
    #   } else {
    #     pic_path <- 'figures/flows/pls/'
    #   }
    #   
    #   fname <- paste0(flow,'_', scenario, '_pls.jpg')
    #   ggsave(pls_plot,  filename = fname, path = pic_path,  device = 'jpeg', width = 12, height = 10, units = 'cm')
    # }
    
    
    # for(scenario in unique(diff_df$scenario)){
    #   
    #   if(flow %in% boring_streams){
    #     break()
    #   }
    #   
    #   if(sum(diff_df[diff_df$scenario == scenario, flow]) == 0){
    #    next() 
    #   }
    #   
    #   #put only the corresponding scenario values to the simulation output, drop the scenario column
    #   nitrogen_mc_simulation$y <- diff_df[diff_df$scenario == scenario, -1]
    #   
    #   #do pls
    #   pls_result <- plsr.mcSimulation(
    #     object = nitrogen_mc_simulation,
    #     resultName = names(nitrogen_mc_simulation$y[flow]), ncomp = 1
    #   )
    #   
    #   pls_plot <- plot_pls(pls_result, input_table = input, threshold = 0.9)
    #   
    #   if(flow %in% boring_streams){
    #     
    #     pic_path <- 'figures/flows/pls_difference_baseline/boring_streams/'
    #   } else {
    #     pic_path <- 'figures/flows/pls_difference_baseline/'
    #   }
    #   
    #   fname <- paste0(flow,'_', scenario, '_difference_baseline_pls.jpg')
    #   ggsave(pls_plot,  filename = fname, path = pic_path,  device = 'jpeg', width = 10, height = 10, units = 'cm')
    # }
    
  } #end for loop for individual streams  
  
  
  
  #visualization for circularity metrics
  
  for(flow in unique(results_indicators_long$variable)){
    
    #density plot off all scenarios
    
    p1 <- results_indicators_long %>%
      filter(variable == flow) %>%
      ggplot(aes(x=as.numeric(value) ,y = scenario, fill = scenario)) +
      geom_density_ridges_gradient(scale=2) +
      xlab(paste0(flow, ' [t per year]'))+
      ylab('')+
      theme_bw() +
      theme(legend.position = "none")
    
    pic_path <- 'figures/circularity_metrics/raw/'
    
    fname <- paste0(flow,'.jpg')
    ggsave(p1,  filename = fname, path = pic_path,  device = 'jpeg', width = 10, height = 7, units = 'cm')
    
    
    # #pls analysis
    # 
    # for(scenario in levels(result_indicators$scenario)){
    #   
    #   #put only the corresponding scenario values to the simulation output, drop the scenario column
    #   nitrogen_mc_simulation$y <- result_indicators[result_indicators$scenario == scenario, -1]
    #   
    #   #do pls
    #   pls_result <- plsr.mcSimulation(
    #     object = nitrogen_mc_simulation,
    #     resultName = names(nitrogen_mc_simulation$y[flow]), ncomp = 1
    #   )
    #   
    #   pls_plot <- plot_pls(pls_result, input_table = input, threshold = 0.9)
    #   
    #   pic_path <- 'figures/circularity_metrics/pls_raw/'
    #   
    #   fname <- paste0(flow,'_', scenario, '_pls.jpg')
    #   ggsave(pls_plot,  filename = fname, path = pic_path,  device = 'jpeg', width = 12, height = 10, units = 'cm')
    # }
    
    
    
    
    
    
    #desnity plots of difference to baseline
    p2 <-  diff_indicators_long %>%
      filter(variable == flow) %>%
      ggplot(aes(x=as.numeric(value) ,y = scenario, fill = scenario)) +
      geom_density_ridges_gradient(scale=2) +
      geom_vline(xintercept = 0, linetype = 'dashed')+
      xlab(paste0(flow, ' [t per year]'))+
      ylab('')+
      theme_bw() +
      theme(legend.position = "none")
    
    pic_path <- 'figures/circularity_metrics/difference_to_baseline/'
    
    fname <- paste0(flow,'diff_to_baseline.jpg')
    ggsave(p2,  filename = fname, path = pic_path,  device = 'jpeg', width = 12, height = 10, units = 'cm')
    
    
    # #pls analysis
    # 
    # for(scenario in unique(diff_df$scenario)){
    #   
    #   #put only the corresponding scenario values to the simulation output, drop the scenario column
    #   nitrogen_mc_simulation$y <- diff_df[diff_df$scenario == scenario, -1]
    #   
    #   #do pls
    #   pls_result <- plsr.mcSimulation(
    #     object = nitrogen_mc_simulation,
    #     resultName = names(nitrogen_mc_simulation$y[flow]), ncomp = 1
    #   )
    #   
    #   pls_plot <- plot_pls(pls_result, input_table = input, threshold = 0.9)
    #   
    #   pic_path <- 'figures/circularity_metrics/pls_difference_baseline/'
    #   
    #   fname <- paste0(flow,'_', scenario, '_difference_baseline_pls.jpg')
    #   try(ggsave(pls_plot,  filename = fname, path = pic_path,  device = 'jpeg', width = 10, height = 10, units = 'cm'))
    # }
  } #end for loop for indicators
  
  
  
} #end plotting individual flows / indicator distributions

#split variable name into variable and nutrient
diff_flows_long <- tidyr::separate(data = diff_flows_long, col = variable, sep = -1, convert = TRUE, into = c('variable', 'nutrient'))
diff_indicators_long <- tidyr::separate(data = diff_indicators_long, col = variable, sep = -1, convert = TRUE, into = c('variable', 'nutrient'))
result_flows_long <- tidyr::separate(data = result_flows_long, col = variable, sep = -1, convert = TRUE, into = c('variable', 'nutrient'))
results_indicators_long <- tidyr::separate(data = results_indicators_long, col = variable, sep = -1, convert = TRUE, into = c('variable', 'nutrient'))

#remove the trailing _ in variable
diff_flows_long <- diff_flows_long %>% 
  mutate(variable = substring(variable, 1, nchar(variable)-1))
diff_indicators_long <- diff_indicators_long %>% 
  mutate(variable = substring(variable, 1, nchar(variable)-1))
result_flows_long <- result_flows_long %>% 
  mutate(variable = substring(variable, 1, nchar(variable)-1))
results_indicators_long <- results_indicators_long %>% 
  mutate(variable = substring(variable, 1, nchar(variable)-1))

results_indicators_long$variable <- factor(results_indicators_long$variable, 
                                           levels = c("total_input", 'losses', "recycling_rate", "share_reuse_to_total_input", "use_efficiency"),
                                           labels = c('Total Input', 'Losses', 'Recycling Rate', 'Reuse : Total Input', 'Use Efficiency'))




#make boxplots of indicators
p1 <- results_indicators_long %>% 
  filter(nutrient == 'N') %>%
  ggplot(aes(x = scenario, y = value, fill = scenario)) +
  geom_boxplot() +
  ylab('Indicator Value') +
  xlab('Model Scenario')+
  facet_wrap(~variable, scales = 'free_y') + 
  theme_bw(base_size = 15) +
  theme(
    legend.position="none",
    #legend.position = c(0.8, 0.1), # c(0,0) bottom left, c(1,1) top-right.
    #legend.background = element_rect(fill = "white", colour = NA),
    #axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

p2 <- results_indicators_long %>% 
  filter(nutrient == 'P') %>%
  ggplot(aes(x = scenario, y = value, fill = scenario)) +
  geom_boxplot() +
  ylab('Indicator Value') +
  xlab('Model Scenario')+
  facet_wrap(~variable, scales = 'free_y') + 
  theme_bw(base_size = 15) +
  theme(
    legend.position="none",
    #legend.position = c(0.8, 0.1), # c(0,0) bottom left, c(1,1) top-right.
    #legend.background = element_rect(fill = "white", colour = NA),
    #axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

p3 <- results_indicators_long %>% 
  filter(nutrient == 'K') %>%
  ggplot(aes(x = scenario, y = value, fill = scenario)) +
  geom_boxplot() +
  ylab('Indicator Value') +
  xlab('Model Scenario')+
  facet_wrap(~variable, scales = 'free_y') + 
  theme_bw(base_size = 15) +
  theme(
    legend.position="none",
    #legend.position = c(0.8, 0.1), # c(0,0) bottom left, c(1,1) top-right.
    #legend.background = element_rect(fill = "white", colour = NA),
    #axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )


ggsave(p1, filename = 'figures/boxplot_indicators_N.jpeg', device = 'jpeg', width = 20, height = 15, units = 'cm')
ggsave(p2, filename = 'figures/boxplot_indicators_P.jpeg', device = 'jpeg', width = 20, height = 15, units = 'cm')
ggsave(p3, filename = 'figures/boxplot_indicators_K.jpeg', device = 'jpeg', width = 20, height = 15, units = 'cm')



#clear workspace
rm(diff_flows_df, diff_flows_long, diff_indicators_df, diff_indicators_long, p1, p2, p3, result_flows, result_flows_long, result_indicators, results_indicators_long)

#read model outputs again
result_flows <- readRDS('data/model_result_flows.rds')
result_indicators <- readRDS('data/model_result_indicators.rds')


#combine meat and dairy stuff
result_flows <- purrr::map(result_flows, function(x){
  x$import_animal_products_N <- x$import_dairy_egg_N + x$import_meat_N
  x$export_animal_products_N <- x$export_dairy_egg_N + x$export_meat_N
  
  x$import_animal_products_P <- x$import_dairy_egg_P + x$import_meat_P
  x$export_animal_products_P <- x$export_dairy_egg_P + x$export_meat_P
  
  x$import_animal_products_K <- x$import_dairy_egg_K + x$import_meat_K
  x$export_animal_products_K <- x$export_dairy_egg_K + x$export_meat_K
  return(x)
})



#calculate change relative to the reference year
rel_change_flows <- rbind.data.frame(((result_flows$interventions[-1] - result_flows$reference_year[-1]) / result_flows$reference_year[-1]) * 100,
                                  ((result_flows$interventions_animal_adjusted[-1] - result_flows$reference_year[-1]) / result_flows$reference_year[-1])*100,
                                  ((result_flows$interventions_crop_adjusted[-1] - result_flows$reference_year[-1]) / result_flows$reference_year[-1]) * 100,
                                  ((result_flows$traditional_agriculture[-1] - result_flows$reference_year[-1]) / result_flows$reference_year[-1]) * 100)
rel_change_flows$scenario <- c(result_flows$interventions$scenario, 
                               result_flows$interventions_animal_adjusted$scenario, 
                               result_flows$interventions_crop_adjusted$scenario,
                               result_flows$traditional_agriculture$scenario)
rel_change_flows <- dplyr::relocate(rel_change_flows, scenario)

rel_change_indicators <- rbind.data.frame(((result_indicators$interventions[-1] - result_indicators$reference_year[-1]) / result_indicators$reference_year[-1]) * 100,
                                     ((result_indicators$interventions_animal_adjusted[-1] - result_indicators$reference_year[-1]) / result_indicators$reference_year[-1]) * 100,
                                     ((result_indicators$interventions_crop_adjusted[-1] - result_indicators$reference_year[-1]) / result_indicators$reference_year[-1]) * 100,
                                     ((result_indicators$traditional_agriculture[-1] - result_indicators$reference_year[-1]) / result_indicators$reference_year[-1]) * 100)
rel_change_indicators$scenario <- c(result_indicators$interventions$scenario, 
                                    result_indicators$interventions_animal_adjusted$scenario, 
                                    result_indicators$interventions_crop_adjusted$scenario,
                                    result_indicators$traditional_agriculture$scenario)
rel_change_indicators <- dplyr::relocate(rel_change_indicators, scenario)


rel_change_indicators$scenario <- factor(rel_change_indicators$scenario, levels = c("reference_year","interventions","interventions_animal_adjusted", "interventions_crop_adjusted", "traditional_agriculture"),
                                 labels = c('Ref', 'PS', 'LBS' ,'CBS', 'TA'))
rel_change_flows$scenario <- factor(rel_change_flows$scenario, levels = c("reference_year","interventions","interventions_animal_adjusted", "interventions_crop_adjusted", "traditional_agriculture"),
                                      labels = c('Ref', 'PS', 'LBS' ,'CBS', 'TA'))



#summarise results similar to eduardos banana paper
#--> calculate median and iqr

rel_change_flows_long <- reshape2::melt(rel_change_flows, id.vars = 'scenario')
rel_change_indicators_long <- reshape2::melt(rel_change_indicators, id.vars = 'scenario')


#zero devided by zero gives nan, so make it 0 again
rel_change_flows_long$value <- ifelse(is.nan(rel_change_flows_long$value), yes = 0, no = rel_change_flows_long$value)
rel_change_indicators_long$value <- ifelse(is.nan(rel_change_indicators_long$value), yes = 0, no = rel_change_indicators_long$value)


summarised_flows <- rel_change_flows_long %>%
  group_by(scenario, variable) %>%
  summarise(median = median(value, na.rm = T),
            iqr = IQR(value, na.rm = T))

summarised_indicators <- rel_change_indicators_long %>%
  group_by(scenario, variable) %>%
  summarise(median = median(value, na.rm = T),
            iqr = IQR(value, na.rm = T))




#split by nutrients; 
summarised_flows <- tidyr::separate(data = summarised_flows, col = variable, sep = -1, convert = TRUE, into = c('variable', 'nutrient'))
summarised_indicators <- tidyr::separate(data = summarised_indicators, col = variable, sep = -1, convert = TRUE, into = c('variable', 'nutrient'))

#remove trailing _
summarised_flows <- summarised_flows %>% 
  mutate(variable = substring(variable, 1, nchar(variable)-1))
summarised_indicators <- summarised_indicators %>% 
  mutate(variable = substring(variable, 1, nchar(variable)-1))

unique(summarised_flows$variable)

#only take flows Bernou wants to see
summarised_flows <- summarised_flows %>%
  filter(variable %in% c('manure_to_crop', 'manure_export', 'manure_as_biogas_substrate',
                         'import_inorganic_fertilizer', 'vegetal_biogas_substrate',
                         'feed_from_processed_crops', 
                         'net_food_import', 'crop_cultivation_losses', 'import_animal_products', 
                         'export_animal_products',
                         'animal_housing_and_storage_losses', 'animal_balance'))


summarised_flows$variable <- factor(summarised_flows$variable, 
       levels = c('manure_to_crop',
                  'manure_export',
                  'manure_as_biogas_substrate',
                  'import_inorganic_fertilizer',
                  'vegetal_biogas_substrate',
                  'feed_from_processed_crops',
                  'import_animal_products',
                  'export_animal_products',
                  'net_food_import',
                  'crop_cultivation_losses',
                  'animal_housing_and_storage_losses',
                  'animal_balance'), 
       
       labels = c('Manure to crops',
                  'Manure export',
                  'Manure biogas substrate',
                  'Import inorganic fertilizers',
                  'Vegetal biogas substrate',
                  'Feed from processed crops',
                  'Animal products import',
                  'Animal products export',
                  'Net food import',
                  'Cultivation losses',
                  'Animal housing and storage losses',
                  'Stock balance animal subsystem'
                  ))

factor(summarised_flows$variable, levels = c('Manure to crops',
                                             'Manure export',
                                             'Manure biogas substrate',
                                             'Import inorganic fertilizers',
                                             'Vegetal biogas substrate',
                                             'Feed from processed crops',
                                             'Animal products import',
                                             'Animal products export',
                                             'Net food import',
                                             'Cultivation losses',
                                             'Animal housing and storage losses',
                                             'Stock balance animal subsystem'
))



#split value into two columns, one for increase, one for decrease?
summarised_flows$increase <- ifelse(summarised_flows$median >= 100,yes = summarised_flows$median, no = NA)
summarised_flows$decrease <- ifelse(summarised_flows$median < 100,yes = summarised_flows$median, no = NA)

#iqr for some parameters complete out of range, set to 2
summarised_flows$iqr_adusted <- ifelse(summarised_flows$iqr > 200, yes = 200, no = summarised_flows$iqr)
#also have 2 as a limit for median, otherwise the scale is completely off
summarised_flows$median_adjusted <- ifelse(summarised_flows$median > 200, yes = 200, no = summarised_flows$median)
summarised_flows$median_adjusted <- ifelse(summarised_flows$median_adjusted < -100, yes = -100, no = summarised_flows$median_adjusted)




#install.packages("ggnewscale")
library(ggnewscale)
RColorBrewer::brewer.pal(7, 'PuOr')

p1 <- summarised_flows %>%
  filter(nutrient == 'N', median_adjusted >= 0) %>%
  ggplot(aes(x = scenario, y = variable)) +
  geom_tile(aes(fill = median_adjusted), data = summarised_flows[summarised_flows$nutrient == 'N' & summarised_flows$median_adjusted < 0,],
            colour="white", size=2) +
  scale_fill_gradient2("Decrease (%)", limits = c(-100, -0), 
                       low = "#542788", mid = "grey95") +
  new_scale("fill") +
  geom_tile(aes(fill = median_adjusted), colour="white", size=2) +
  scale_fill_gradient2("Increase (%)", limits = c(0, 200), 
                       mid = "grey95", high = "#B35806") +
  geom_point(aes(size = iqr_adusted), data = summarised_flows[summarised_flows$nutrient == 'N',], col = 'grey50') + 
  scale_size(range = c(.1, 7), name="IQR (%)") +
  theme_bw() + ylab('Nitrogen flow') + xlab('Scenario')+
  scale_y_discrete(limits=rev(c('Manure to crops',
                            'Manure export',
                            'Manure biogas substrate',
                            'Import inorganic fertilizers',
                            'Vegetal biogas substrate',
                            'Feed from processed crops',
                            'Animal products import',
                            'Animal products export',
                            'Net food import',
                            'Cultivation losses',
                            'Animal housing and storage losses',
                            'Stock balance animal subsystem')))

ggsave(p1, filename = 'flow_changes_N.jpg', path = 'figures/', device = 'jpeg', height = 20, width = 15, units = 'cm')


p2 <- summarised_flows %>%
  filter(nutrient == 'P', median_adjusted >= 0) %>%
  ggplot(aes(x = scenario, y = variable)) +
  geom_tile(aes(fill = median_adjusted), data = summarised_flows[summarised_flows$nutrient == 'P' & summarised_flows$median_adjusted < 0,],
            colour="white", size=2) +
  scale_fill_gradient2("Decrease (%)", limits = c(-100, -0), 
                       low = "#542788", mid = "grey95") +
  new_scale("fill") +
  geom_tile(aes(fill = median_adjusted), colour="white", size=2) +
  scale_fill_gradient2("Increase (%)", limits = c(0, 200), 
                       mid = "grey95", high = "#B35806") +
  geom_point(aes(size = iqr_adusted), data = summarised_flows[summarised_flows$nutrient == 'P',], col = 'grey50') + 
  scale_size(range = c(.1, 7), name="IQR (%)") +
  theme_bw() +
  ylab('Phosporous flow') + xlab('Scenario')+
  scale_y_discrete(limits=rev(c('Manure to crops',
                                'Manure export',
                                'Manure biogas substrate',
                                'Import inorganic fertilizers',
                                'Vegetal biogas substrate',
                                'Feed from processed crops',
                                'Animal products import',
                                'Animal products export',
                                'Net food import',
                                'Cultivation losses',
                                'Animal housing and storage losses',
                                'Stock balance animal subsystem')))

ggsave(p2, filename = 'flow_changes_P.jpg', path = 'figures/', device = 'jpeg', height = 20, width = 15, units = 'cm')


p3 <- summarised_flows %>%
  filter(nutrient == 'K', median_adjusted >= 0) %>%
  ggplot(aes(x = scenario, y = variable)) +
  geom_tile(aes(fill = median_adjusted), data = summarised_flows[summarised_flows$nutrient == 'K' & summarised_flows$median_adjusted < 0,],
            colour="white", size=2) +
  scale_fill_gradient2("Decrease (%)", limits = c(-100, -0), 
                       low = "#542788", mid = "grey95") +
  new_scale("fill") +
  geom_tile(aes(fill = median_adjusted), colour="white", size=2) +
  scale_fill_gradient2("Increase (%)", limits = c(0, 200), 
                       mid = "grey95", high = "#B35806") +
  geom_point(aes(size = iqr_adusted), data = summarised_flows[summarised_flows$nutrient == 'K',], col = 'grey50') + 
  scale_size(range = c(.1, 7), name="IQR (%)") +
  theme_bw() +  ylab('Potassium flow') + xlab('Scenario')+
  scale_y_discrete(limits=rev(c('Manure to crops',
                                'Manure export',
                                'Manure biogas substrate',
                                'Import inorganic fertilizers',
                                'Vegetal biogas substrate',
                                'Feed from processed crops',
                                'Animal products import',
                                'Animal products export',
                                'Net food import',
                                'Cultivation losses',
                                'Animal housing and storage losses',
                                'Stock balance animal subsystem')))

ggsave(p3, filename = 'flow_changes_K.jpg', path = 'figures/', device = 'jpeg', height = 20, width = 15, units = 'cm')




#indicators


#split value into two columns, one for increase, one for decrease?
summarised_indicators$increase <- ifelse(summarised_indicators$median >= 100,yes = summarised_indicators$median, no = NA)
summarised_indicators$decrease <- ifelse(summarised_indicators$median < 100,yes = summarised_indicators$median, no = NA)

#iqr for some parameters complete out of range, set to 2
summarised_indicators$iqr_adusted <- ifelse(summarised_indicators$iqr > 200, yes = 200, no = summarised_indicators$iqr)
#also have 2 as a limit for median, otherwise the scale is completely off
summarised_indicators$median_adjusted <- ifelse(summarised_indicators$median > 200, yes = 200, no = summarised_indicators$median)
summarised_indicators$median_adjusted <- ifelse(summarised_indicators$median_adjusted < -100, yes = -100, no = summarised_indicators$median_adjusted)


#install.packages("ggnewscale")
library(ggnewscale)
RColorBrewer::brewer.pal(7, 'PuOr')

summarised_indicators$variable <- factor(summarised_indicators$variable, 
                                           levels = c("total_input", 'losses', "recycling_rate", "share_reuse_to_total_input", "use_efficiency"),
                                           labels = c('Total Input', 'Losses', 'Recycling Rate', 'Reuse : Total Input', 'Use Efficiency'))



p4 <- summarised_indicators %>%
  filter(nutrient == 'N', median_adjusted >= 0) %>%
  ggplot(aes(x = scenario, y = variable)) +
  geom_tile(aes(fill = median_adjusted), data = summarised_indicators[summarised_indicators$nutrient == 'N' & summarised_indicators$median_adjusted < 0,],
            colour="white", size=2) +
  scale_fill_gradient2("Decrease (%)", limits = c(-100, -0), 
                       low = "#542788", mid = "grey95") +
  new_scale("fill") +
  geom_tile(aes(fill = median_adjusted), colour="white", size=2) +
  scale_fill_gradient2("Increase (%)", limits = c(0, 100), 
                       mid = "grey95", high = "#B35806") +
  geom_point(aes(size = iqr_adusted), data = summarised_indicators[summarised_indicators$nutrient == 'N',], col = 'grey50') + 
  scale_size(range = c(.1, 7), name="IQR (%)") +
  theme_bw() +
  ylab('Circularity Indicators') + xlab('Scenario')+
  scale_y_discrete(limits=rev(c('Total Input',
                                'Losses',
                                'Use Efficiency',
                                'Recycling Rate',
                                'Reuse : Total Input')))

ggsave(p4, filename = 'indicators_changes_N.jpg', path = 'figures/', device = 'jpeg', height = 20, width = 15, units = 'cm')


p5 <- summarised_indicators %>%
  filter(nutrient == 'P', median_adjusted >= 0) %>%
  ggplot(aes(x = scenario, y = variable)) +
  geom_tile(aes(fill = median_adjusted), data = summarised_indicators[summarised_indicators$nutrient == 'N' & summarised_indicators$median_adjusted < 0,],
            colour="white", size=2) +
  scale_fill_gradient2("Decrease (%)", limits = c(-100, -0), 
                       low = "#542788", mid = "grey95") +
  new_scale("fill") +
  geom_tile(aes(fill = median_adjusted), colour="white", size=2) +
  scale_fill_gradient2("Increase (%)", limits = c(0, 100), 
                       mid = "grey95", high = "#B35806") +
  geom_point(aes(size = iqr_adusted), data = summarised_indicators[summarised_indicators$nutrient == 'P',], col = 'grey50') + 
  scale_size(range = c(.1, 7), name="IQR (%)") +
  theme_bw() +
  ylab('Circularity Indicators') + xlab('Scenario')+
  scale_y_discrete(limits=rev(c('Total Input',
                                'Losses',
                                'Use Efficiency',
                                'Recycling Rate',
                                'Reuse : Total Input')))

ggsave(p5, filename = 'indicators_changes_P.jpg', path = 'figures/', device = 'jpeg', height = 20, width = 15, units = 'cm')


p6 <- summarised_indicators %>%
  filter(nutrient == 'K', median_adjusted >= 0) %>%
  ggplot(aes(x = scenario, y = variable)) +
  geom_tile(aes(fill = median_adjusted), data = summarised_indicators[summarised_indicators$nutrient == 'N' & summarised_indicators$median_adjusted < 0,],
            colour="white", size=2) +
  scale_fill_gradient2("Decrease (%)", limits = c(-100, -0), 
                       low = "#542788", mid = "grey95") +
  new_scale("fill") +
  geom_tile(aes(fill = median_adjusted), colour="white", size=2) +
  scale_fill_gradient2("Increase (%)", limits = c(0, 100), 
                       mid = "grey95", high = "#B35806") +
  geom_point(aes(size = iqr_adusted), data = summarised_indicators[summarised_indicators$nutrient == 'K',], col = 'grey50') + 
  scale_size(range = c(.1, 7), name="IQR (%)") +
  theme_bw() +
  ylab('Circularity Indicators') + xlab('Scenario')+
  scale_y_discrete(limits=rev(c('Total Input',
                                'Losses',
                                'Use Efficiency',
                                'Recycling Rate',
                                'Reuse : Total Input'))) 

ggsave(p6, filename = 'indicators_changes_K.jpg', path = 'figures/', device = 'jpeg', height = 20, width = 15, units = 'cm')
