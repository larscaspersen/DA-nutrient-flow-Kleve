
readRDS('')









diff_df <- rbind.data.frame(result_list$interventions[-1] - result_list$reference_year[-1],
                            result_list$interventions_animal_adjusted[-1] - result_list$reference_year[-1],
                            result_list$interventions_crop_adjusted[-1] - result_list$reference_year[-1])
diff_df$scenario <- c(result_list$interventions$scenario, result_list$interventions_animal_adjusted$scenario, result_list$interventions_crop_adjusted$scenario)
diff_df <- dplyr::relocate(diff_df, scenario)


#--> list contains model results spliut by the scenarios

#check the distributions

library(tidyverse)
library(ggridges)

if(return_flows){
  #visualisation for individual flows
  
  #melt combined outputs
  combined_results_long <- reshape2::melt(combined_results, id.var = 'scenario')
  
  diff_df_long <- reshape::melt(diff_df, id.var = 'scenario')
  
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
  
  for(flow in unique(combined_results_long$variable)){
    
    #density for all flows
    p1 <- combined_results_long %>%
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
    p2 <- diff_df_long %>%
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
    
    
  }
  
  
  
  
  
} else {
  #visualization for circularity metrics
  
  #melt combined outputs
  combined_results_long <- reshape2::melt(combined_results, id.var = 'scenario')
  
  diff_df_long <- reshape::melt(diff_df, id.var = 'scenario')
  
  for(flow in unique(combined_results_long$variable)){
    
    #density plot off all scenarios
    
    p1 <- combined_results_long %>%
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
    
    
    #pls analysis
    
    for(scenario in levels(combined_results$scenario)){
      
      #put only the corresponding scenario values to the simulation output, drop the scenario column
      nitrogen_mc_simulation$y <- combined_results[combined_results$scenario == scenario, -1]
      
      #do pls
      pls_result <- plsr.mcSimulation(
        object = nitrogen_mc_simulation,
        resultName = names(nitrogen_mc_simulation$y[flow]), ncomp = 1
      )
      
      pls_plot <- plot_pls(pls_result, input_table = input, threshold = 0.9)
      
      pic_path <- 'figures/circularity_metrics/pls_raw/'
      
      fname <- paste0(flow,'_', scenario, '_pls.jpg')
      ggsave(pls_plot,  filename = fname, path = pic_path,  device = 'jpeg', width = 12, height = 10, units = 'cm')
    }
    
    
    
    
    
    
    #desnity plots of difference to baseline
    p2 <-  diff_df_long %>%
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
    
    
    #pls analysis
    
    for(scenario in unique(diff_df$scenario)){
      
      #put only the corresponding scenario values to the simulation output, drop the scenario column
      nitrogen_mc_simulation$y <- diff_df[diff_df$scenario == scenario, -1]
      
      #do pls
      pls_result <- plsr.mcSimulation(
        object = nitrogen_mc_simulation,
        resultName = names(nitrogen_mc_simulation$y[flow]), ncomp = 1
      )
      
      pls_plot <- plot_pls(pls_result, input_table = input, threshold = 0.9)
      
      pic_path <- 'figures/circularity_metrics/pls_difference_baseline/'
      
      fname <- paste0(flow,'_', scenario, '_difference_baseline_pls.jpg')
      try(ggsave(pls_plot,  filename = fname, path = pic_path,  device = 'jpeg', width = 10, height = 10, units = 'cm'))
    }
  }
  
  
  
}




#calculate change relative to the reference year
rel_change_df <- rbind.data.frame((result_list$interventions[-1] - result_list$reference_year[-1]) / result_list$reference_year[-1],
                                  (result_list$interventions_animal_adjusted[-1] - result_list$reference_year[-1]) / result_list$reference_year[-1],
                                  (result_list$interventions_crop_adjusted[-1] - result_list$reference_year[-1]) / result_list$reference_year[-1])
rel_change_df$scenario <- c(result_list$interventions$scenario, result_list$interventions_animal_adjusted$scenario, result_list$interventions_crop_adjusted$scenario)
rel_change_df <- dplyr::relocate(rel_change_df, scenario)



#summarise results similar to eduardos banana paper
#--> calculate median and iqr

rel_change_df_long <- reshape2::melt(rel_change_df, id.vars = 'scenario')

#zero devided by zero gives nan, so make it 0 again
rel_change_df_long$value <- ifelse(is.nan(rel_change_df_long$value), yes = 0, no = rel_change_df_long$value)


summarised_df <- rel_change_df_long %>%
  group_by(scenario, variable) %>%
  summarise(median = median(value, na.rm = T),
            iqr = IQR(value, na.rm = T))


#split by nutrients; 
summarised_df <- tidyr::separate(data = summarised_df, col = variable, sep = -1, convert = TRUE, into = c('variable', 'nutrient'))

#remove last character
for(i in 1:nrow(summarised_df)){
  summarised_df$variable[i] <- substring(summarised_df$variable[i],1, nchar(summarised_df$variable[i])-1) 
}

#remove streams with boring outcomes
summarised_df <- summarised_df %>%
  filter(variable %in% c('animal_housing_and_storage_losses',
                         'manure_to_crop',
                         'import_inorganic_fertilizer',
                         'crop_cultivation_losses',
                         'manure_export',
                         'manure_as_biogas_substrate',
                         'vegetal_biogas_substrate',
                         'net_feed_import',
                         'import_meat',
                         'imported_animal_products',
                         'feed_from_processed_crops',
                         'feed_crops',
                         'export_meat',
                         'export_egg',
                         'dairy_export',
                         'animal_balance'))



#split value into two columns, one for increase, one for decrease?
summarised_df$increase <- ifelse(summarised_df$median >= 1,yes = summarised_df$median, no = NA)
summarised_df$decrease <- ifelse(summarised_df$median < 1,yes = summarised_df$median, no = NA)

#iqr for some parameters complete out of range, set to 2
summarised_df$iqr_adusted <- ifelse(summarised_df$iqr > 2, yes = 2, no = summarised_df$iqr)
#also have 2 as a limit for median, otherwise the scale is completely off
summarised_df$median_adjusted <- ifelse(summarised_df$median > 2, yes = 2.0, no = summarised_df$median)
summarised_df$median_adjusted <- ifelse(summarised_df$median_adjusted < -1, yes = -1, no = summarised_df$median_adjusted)


#install.packages("ggnewscale")
library(ggnewscale)
RColorBrewer::brewer.pal(7, 'PuOr')

p1 <- summarised_df %>%
  filter(nutrient == 'N', median_adjusted >= 0) %>%
  ggplot(aes(x = scenario, y = variable)) +
  geom_tile(aes(fill = median_adjusted), data = summarised_df[summarised_df$nutrient == 'N' & summarised_df$median_adjusted < 0,],
            colour="white", size=2) +
  scale_fill_gradient2("Decrease", limits = c(-1, -0), 
                       low = "#542788", mid = "grey95") +
  new_scale("fill") +
  geom_tile(aes(fill = median_adjusted), colour="white", size=2) +
  scale_fill_gradient2("Increase", limits = c(0, 2), 
                       mid = "grey95", high = "#B35806") +
  geom_point(aes(size = iqr_adusted), data = summarised_df, col = 'grey50') + 
  scale_size(range = c(.1, 7), name="IQR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(p1, filename = 'flow_changes.jpg', path = 'figures/', device = 'jpeg', height = 20, width = 15, units = 'cm')


p2 <- summarised_df %>%
  filter(nutrient == 'P', median_adjusted >= 0) %>%
  ggplot(aes(x = scenario, y = variable)) +
  geom_tile(aes(fill = median_adjusted), colour="white", size=2) +
  scale_fill_gradient2("Increase", limits = c(0, 2), 
                       mid = "grey95", high = "#0571b0") +
  
  new_scale("fill") +
  geom_tile(aes(fill = median_adjusted), data = summarised_df[summarised_df$nutrient == 'N' & summarised_df$median_adjusted < 0,],
            colour="white", size=2) +
  scale_fill_gradient2("Decrease", limits = c(-1, 0), 
                       low = "#ca0020", mid = "grey95") +
  geom_point(aes(size = iqr_adusted), data = summarised_df, col = 'grey50') + 
  scale_size(range = c(.1, 7), name="IQR") +
  theme_bw()

ggsave(p2, filename = 'flow_changes_P.jpg', path = 'figures/', device = 'jpeg', height = 20, width = 15, units = 'cm')


p3 <- summarised_df %>%
  filter(nutrient == 'K', median_adjusted >= 0) %>%
  ggplot(aes(x = scenario, y = variable)) +
  geom_tile(aes(fill = median_adjusted), colour="white", size=2) +
  scale_fill_gradient2("Increase", limits = c(0, 2), 
                       mid = "grey95", high = "#0571b0") +
  
  new_scale("fill") +
  geom_tile(aes(fill = median_adjusted), data = summarised_df[summarised_df$nutrient == 'N' & summarised_df$median_adjusted < 0,],
            colour="white", size=2) +
  scale_fill_gradient2("Decrease", limits = c(-1, 0), 
                       low = "#ca0020", mid = "grey95") +
  geom_point(aes(size = iqr_adusted), data = summarised_df, col = 'grey50') + 
  scale_size(range = c(.1, 7), name="IQR") +
  theme_bw()

ggsave(p3, filename = 'flow_changes_K.jpg', path = 'figures/', device = 'jpeg', height = 20, width = 15, units = 'cm')


