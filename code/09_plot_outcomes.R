#setwd('../DA-job/DA-nutrient-flow-Kleve/')
library(tidyverse)
library(ggridges)
library(scales) #to have non-scientific numbers for the x and y axis

plot_flows <- TRUE

#read saved runs for flows and for indicators
result_flows <- readRDS('data/model_result_flows.rds')
#result_indicators <- readRDS('data/model_result_indicators.rds')

diff_flows_df <- rbind.data.frame(result_flows$interventions[-1] - result_flows$reference_year[-1],
                                  result_flows$interventions_animal_adjusted[-1] - result_flows$reference_year[-1],
                                  result_flows$interventions_crop_adjusted[-1] - result_flows$reference_year[-1])


diff_flows_df$scenario <- c(result_flows$interventions$scenario, 
                            result_flows$interventions_animal_adjusted$scenario, 
                            result_flows$interventions_crop_adjusted$scenario)

diff_flows_df <- dplyr::relocate(diff_flows_df, scenario)


#--> list contains model results split by the scenarios

#check the distributions

result_flows <- do.call(rbind, result_flows)

#change names of scenarios
result_flows$scenario <- factor(result_flows$scenario, levels = c("reference_year","interventions","interventions_animal_adjusted", "interventions_crop_adjusted"),
       labels = c('Ref', 'PS', 'LBS' ,'CBS'))

result_flows <- na.omit(result_flows)

result_flows$run <- rep(rep(1:10000), length(unique(result_flows$scenario)))

diff_flows_df$scenario <- factor(diff_flows_df$scenario, levels = c("reference_year","interventions","interventions_animal_adjusted", "interventions_crop_adjusted"),
       labels = c('Ref', 'PS', 'LBS' ,'CBS'))



#bring results in long format, bring differences in long format
result_flows_long <- reshape2::melt(result_flows, id.var = c('scenario', 'run'))
diff_flows_long <- reshape::melt(diff_flows_df, id.var = 'scenario')


result_flows_long <- na.omit(result_flows_long)
diff_flows_long <- na.omit(diff_flows_long)



result_flows_long <- result_flows_long %>% 
  mutate(variable = recode(variable, 
                           import_organic_N_nonanimal = 'import_organic_nonanimal_N',
                           import_organic_P_nonanimal = 'import_organic_nonanimal_P',
                           import_organic_K_nonanimal = 'import_organic_nonanimal_K'))
#--> have to bring them into the right format



write.csv(result_flows, 'data/result_flows_new.csv', row.names = FALSE)




if(plot_flows){
  #visualisation for individual flows
  
  #melt combined outputs
  
  #remove old files
  # unlink('figures/flows/distributions/boring_streams/')
  f1 <- list.files('figures/flows/distributions/', full.names = TRUE)
  f2 <- list.files('figures/flows/distributions/boring_streams/', full.names = TRUE)
  f3 <- list.files('figures/flows/distribution_difference_baseline/', full.names = TRUE)
  f4 <- list.files('figures/flows/distribution_difference_baseline/boring_streams/', full.names = TRUE)
  unlink(c(f1,f2,f3,f4)) 

  
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
  
  indicators <- c('total_input_N', 'total_input_P', 'total_input_K', 
                  'losses_N', 'losses_P', 'losses_K',
                  'use_efficiency_N', 'use_efficiency_P', 'use_efficiency_K',
                  'share_reuse_to_total_input_N', 'share_reuse_to_total_input_P', 'share_reuse_to_total_input_K',
                  'recycling_rate_N', 'recycling_rate_P', 'recycling_rate_K')
  
  for(flow in unique(result_flows_long$variable)){
    
    #density for all flows
    p1 <- result_flows_long %>%
      filter(variable == flow) %>%
      ggplot(aes(x=as.numeric(value) ,y = scenario, fill = scenario)) +
      geom_density_ridges_gradient(scale=2) +
      xlab(paste0(flow, ' [t per year]'))+
      ylab('')+
      theme_bw() +
      scale_x_continuous(labels = label_comma())+
      theme(legend.position = "none")
    
    if(flow %in% boring_streams){
      pic_path <- 'figures/flows/distributions/boring_streams/'
    } else if(flow %in% indicators){
      pic_path <- 'figures/circularity_metrics/raw/'
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
      scale_x_continuous(labels = label_comma())+
      theme(legend.position = "none")
    
    if(flow %in% boring_streams){
      pic_path <- 'figures/flows/distribution_difference_baseline/boring_streams/'
    } else if(flow %in% indicators){
      pic_path <- 'figures/circularity_metrics/difference_to_baseline/'
    }else {
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
  
} #end plotting individual flows / indicator distributions

#split variable name into variable and nutrient
diff_flows_long <- tidyr::separate(data = diff_flows_long, col = variable, sep = -1, convert = TRUE, into = c('variable', 'nutrient'))
result_flows_long <- tidyr::separate(data = result_flows_long, col = variable, sep = -1, convert = TRUE, into = c('variable', 'nutrient'))

#remove the trailing _ in variable
diff_flows_long <- diff_flows_long %>% 
  mutate(variable = substring(variable, 1, nchar(variable)-1))
result_flows_long <- result_flows_long %>% 
  mutate(variable = substring(variable, 1, nchar(variable)-1))




result_flow_reshaped <- reshape2::dcast(result_flows_long, variable + run ~ scenario + nutrient, value.var = 'value')
result_flow_reshaped_list <- split(result_flow_reshaped, result_flow_reshaped$variable)

for(i in 1:length(result_flow_reshaped_list)){
  fname <- paste0('data/individual_flows/', names(result_flow_reshaped_list)[i],'.csv')
  write.csv(result_flow_reshaped_list[[i]], file = fname, row.names = FALSE)
}







results_indicators_long <- result_flows_long %>% 
  filter(variable %in% c("total_input", 'losses', "recycling_rate", "share_reuse_to_total_input", "use_efficiency"))

results_indicators_long$variable <- factor(results_indicators_long$variable, 
                                           levels = c("total_input", 'losses', "recycling_rate", "share_reuse_to_total_input", "use_efficiency"),
                                           labels = c('Total Input', 'Losses', 'Recycling Rate', 'Reuse to Total Input', 'Use Efficiency'))

library(patchwork)
design <- "
112
333
"   


#color palette for color blind people
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

base_size <- 17
width <- 24
height <- 17

make_indicator_plots <- TRUE

if(make_indicator_plots){
  #indicators: make two seperatre plots with SHARED y axis:
  p1.1 <- results_indicators_long %>% 
    filter(nutrient == 'N', variable %in% c('Recycling Rate', 'Reuse to Total Input', 'Use Efficiency')) %>%
    filter(value >= 0) %>% 
    na.omit() %>% 
    ggplot(aes(x = scenario, y = value, fill = scenario)) +
    geom_boxplot(outlier.alpha = 0.1) +
    scale_fill_manual(name = "Modelled Scenario", 
                      labels = c("Reference Year 2020", "Participatory Scenario", 
                                 "Crop Buffered Scenario", "Livestock Buffered Scenario"),
                      values=cbp1) +
    ylab('Circularity indicator (%)') +
    xlab('')+
    ylim(0,100)+
    facet_wrap(~variable) + 
    theme_bw(base_size = base_size) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="none")
  
  p1.2 <- results_indicators_long %>% 
    filter(nutrient == 'N', variable %in% c('Total Input', 'Losses')) %>%
    filter(value >= 0) %>% 
    mutate(value = floor(value / 1000)) %>% 
    na.omit() %>% 
    ggplot(aes(x = scenario, y = value, fill = scenario)) +
    geom_boxplot(outlier.alpha = 0.1) +
    ylab(bquote('Circularity indicator (t N'~year^-1*')')) +
    xlab('')+
    scale_fill_manual(name = "Modelled Scenario", 
                      labels = c("Reference Year 2020", "Participatory Scenario", 
                                 "Crop Buffered Scenario", "Livestock Buffered Scenario"),
                      values=cbp1) +
    scale_y_continuous(labels = scales::comma)+
    expand_limits(y=0)+
    facet_wrap(~variable) + 
    theme_bw(base_size = base_size) +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
  
  
  p1 <- p1.2 + guide_area() + p1.1  + plot_layout(design=design, guides = "collect") 
  
  #p1 <- (((p1.2  | plot_spacer()) + plot_layout(widths = c(2,1))) / p1.1) + plot_layout(guides = 'collect')
  
  
  
  
  p2.1 <- results_indicators_long %>% 
    filter(nutrient == 'P', variable %in% c('Recycling Rate', 'Reuse to Total Input', 'Use Efficiency')) %>%
    filter(value >= 0) %>% 
    na.omit() %>% 
    ggplot(aes(x = scenario, y = value, fill = scenario)) +
    geom_boxplot(outlier.alpha = 0.1) +
    scale_fill_manual(name = "Modelled Scenario", 
                      labels = c("Reference Year 2020", "Participatory Scenario", 
                                 "Crop Buffered Scenario", "Livestock Buffered Scenario"),
                      values=cbp1) +
    ylab('Circularity indicator (%)') +
    xlab('')+
    ylim(0,100)+
    facet_wrap(~variable) + 
    theme_bw(base_size = base_size) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="none")
  
  
  p2.2 <- results_indicators_long %>% 
    filter(nutrient == 'P', variable %in% c('Total Input', 'Losses')) %>%
    filter(value >= 0) %>% 
    na.omit() %>% 
    mutate(value = value / 1000) %>% 
    ggplot(aes(x = scenario, y = value, fill = scenario)) +
    geom_boxplot(outlier.alpha = 0.1) +
    ylab(bquote('Circularity indicator (t P'~year^-1*')')) +
    xlab('')+
    expand_limits(y=0)+
    scale_fill_manual(name = "Modelled Scenario", 
                      labels = c("Reference Year 2020", "Participatory Scenario", 
                                 "Crop Buffered Scenario", "Livestock Buffered Scenario"),
                      values=cbp1) +
    scale_y_continuous(labels = scales::comma)+
    facet_wrap(~variable) + 
    theme_bw(base_size = base_size) +
    theme(    axis.text.x=element_blank(),
              axis.ticks.x=element_blank()
    )
  p2 <- p2.2 + guide_area() + p2.1  + plot_layout(design=design, guides = "collect") 
  
  p3.1 <- results_indicators_long %>% 
    filter(nutrient == 'K', variable %in% c('Recycling Rate', 'Reuse to Total Input', 'Use Efficiency')) %>%
    filter(value >= 0) %>% 
    na.omit() %>% 
    ggplot(aes(x = scenario, y = value, fill = scenario)) +
    geom_boxplot(outlier.alpha = 0.1) +
    scale_fill_manual(name = "Modelled Scenario", 
                      labels = c("Reference Year 2020", "Participatory Scenario", 
                                 "Crop Buffered Scenario", "Livestock Buffered Scenario"),
                      values=cbp1) +
    ylab('Circularity indicator (%)') +
    xlab('')+
    ylim(0,100)+
    facet_wrap(~variable) + 
    theme_bw(base_size = base_size) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="none")
  
  
  p3.2 <- results_indicators_long %>% 
    filter(nutrient == 'K', variable %in% c('Total Input', 'Losses')) %>%
    filter(value >= 0) %>% 
    mutate(value = value / 1000) %>% 
    na.omit() %>% 
    ggplot(aes(x = scenario, y = value, fill = scenario)) +
    geom_boxplot(outlier.alpha = 0.1) +
    ylab(bquote('Circularity indicator (t K'~year^-1*')')) +
    scale_fill_manual(name = "Modelled Scenario", 
                      labels = c("Reference Year 2020", "Participatory Scenario", 
                                 "Crop Buffered Scenario", "Livestock Buffered Scenario"),
                      values=cbp1) +
    xlab('')+
    expand_limits(y=0)+
    scale_y_continuous(labels = scales::comma)+
    facet_wrap(~variable) + 
    theme_bw(base_size = base_size) +
    theme(    axis.text.x=element_blank(),
              axis.ticks.x=element_blank()
    )
  p3 <- p3.2 + guide_area() + p3.1  + plot_layout(design=design, guides = "collect") 
  
  
  ggsave(p1, filename = 'figures/boxplot_indicators_N.jpeg', device = 'jpeg', width = width, height = height, units = 'cm')
  ggsave(p2, filename = 'figures/boxplot_indicators_P.jpeg', device = 'jpeg', width = width, height = height, units = 'cm')
  ggsave(p3, filename = 'figures/boxplot_indicators_K.jpeg', device = 'jpeg', width = width, height = height, units = 'cm')
  
  
  
  #same boxplot with patterns
  library(ggpattern)
  
  pattern_choice <- c('gray100', 'left30',
                      'rightshingle', 'gray50')
  
  p1.1 <- results_indicators_long %>% 
    filter(nutrient == 'N', variable %in% c('Recycling Rate', 'Reuse to Total Input', 'Use Efficiency')) %>%
    filter(value >= 0) %>% 
    na.omit() %>% 
    ggplot(aes(x = scenario, y = value, fill = scenario)) +
    geom_boxplot_pattern(aes(pattern_type = scenario),
                         pattern              = 'magick',
                         pattern_fill         = 'black',
                         pattern_aspect_ratio = 1.75,
                         fill                 = 'white',
                         colour               = 'black',
                         outlier.alpha = 0.1) +
    ylab('Circularity indicator (%)') +
    xlab('')+
    ylim(0,100)+
    facet_wrap(~variable) + 
    scale_pattern_type_discrete(choices = pattern_choice) +
    theme_bw(base_size = base_size) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="none")
  
  p1.2 <- results_indicators_long %>% 
    filter(nutrient == 'N', variable %in% c('Total Input', 'Losses')) %>%
    filter(value >= 0) %>% 
    mutate(value = floor(value / 1000)) %>% 
    na.omit() %>% 
    ggplot(aes(x = scenario, y = value, fill = scenario)) +
    geom_boxplot_pattern(aes(pattern_type = scenario),
                         pattern              = 'magick',
                         pattern_fill         = 'black',
                         pattern_aspect_ratio = 1.75,
                         fill                 = 'white',
                         colour               = 'black',
                         outlier.alpha = 0.1) +
    ylab(bquote('Circularity indicator (t N'~year^-1*')')) +
    xlab('')+
    scale_pattern_type_discrete(choices = pattern_choice,
                                name = "Modelled Scenario", 
                                labels = c("Reference Year 2020", "Participatory Scenario", 
                                           "Crop Buffered Scenario", "Livestock Buffered Scenario")) +
    scale_y_continuous(labels = scales::comma)+
    expand_limits(y=0)+
    facet_wrap(~variable) + 
    theme_bw(base_size = base_size) +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
  
  
  p1 <- p1.2 + guide_area() + p1.1  + plot_layout(design=design, guides = "collect") 
  
  #p1 <- (((p1.2  | plot_spacer()) + plot_layout(widths = c(2,1))) / p1.1) + plot_layout(guides = 'collect')
  
  
  
  
  p2.1 <- results_indicators_long %>% 
    filter(nutrient == 'P', variable %in% c('Recycling Rate', 'Reuse to Total Input', 'Use Efficiency')) %>%
    filter(value >= 0) %>% 
    na.omit() %>% 
    ggplot(aes(x = scenario, y = value, fill = scenario)) +
    geom_boxplot_pattern(aes(pattern_type = scenario),
                         pattern              = 'magick',
                         pattern_fill         = 'black',
                         pattern_aspect_ratio = 1.75,
                         fill                 = 'white',
                         colour               = 'black',
                         outlier.alpha = 0.1) +
    scale_pattern_type_discrete(choices = pattern_choice,
                                name = "Modelled Scenario", 
                                labels = c("Reference Year 2020", "Participatory Scenario", 
                                           "Crop Buffered Scenario", "Livestock Buffered Scenario")) +
    ylab('Circularity indicator (%)') +
    xlab('')+
    ylim(0,100)+
    facet_wrap(~variable) + 
    theme_bw(base_size = base_size) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="none")
  
  
  p2.2 <- results_indicators_long %>% 
    filter(nutrient == 'P', variable %in% c('Total Input', 'Losses')) %>%
    filter(value >= 0) %>% 
    na.omit() %>% 
    mutate(value = value / 1000) %>% 
    ggplot(aes(x = scenario, y = value, fill = scenario)) +
    geom_boxplot_pattern(aes(pattern_type = scenario),
                         pattern              = 'magick',
                         pattern_fill         = 'black',
                         pattern_aspect_ratio = 1.75,
                         fill                 = 'white',
                         colour               = 'black',
                         outlier.alpha = 0.1) +
    ylab(bquote('Circularity indicator (t P'~year^-1*')')) +
    xlab('')+
    expand_limits(y=0)+
    scale_pattern_type_discrete(choices = pattern_choice,
                                name = "Modelled Scenario", 
                                labels = c("Reference Year 2020", "Participatory Scenario", 
                                           "Crop Buffered Scenario", "Livestock Buffered Scenario")) +
    scale_y_continuous(labels = scales::comma)+
    facet_wrap(~variable) + 
    theme_bw(base_size = base_size) +
    theme(    axis.text.x=element_blank(),
              axis.ticks.x=element_blank()
    )
  p2 <- p2.2 + guide_area() + p2.1  + plot_layout(design=design, guides = "collect") 
  
  p3.1 <- results_indicators_long %>% 
    filter(nutrient == 'K', variable %in% c('Recycling Rate', 'Reuse to Total Input', 'Use Efficiency')) %>%
    filter(value >= 0) %>% 
    na.omit() %>% 
    ggplot(aes(x = scenario, y = value, fill = scenario)) +
    geom_boxplot_pattern(aes(pattern_type = scenario),
                         pattern              = 'magick',
                         pattern_fill         = 'black',
                         pattern_aspect_ratio = 1.75,
                         fill                 = 'white',
                         colour               = 'black',
                         outlier.alpha = 0.1) +
    scale_pattern_type_discrete(choices = pattern_choice,
                                name = "Modelled Scenario", 
                                labels = c("Reference Year 2020", "Participatory Scenario", 
                                           "Crop Buffered Scenario", "Livestock Buffered Scenario")) +
    ylab('Circularity indicator (%)') +
    xlab('')+
    ylim(0,100)+
    facet_wrap(~variable) + 
    theme_bw(base_size = base_size) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="none")
  
  
  p3.2 <- results_indicators_long %>% 
    filter(nutrient == 'K', variable %in% c('Total Input', 'Losses')) %>%
    filter(value >= 0) %>% 
    mutate(value = value / 1000) %>% 
    na.omit() %>% 
    ggplot(aes(x = scenario, y = value, fill = scenario)) +
    geom_boxplot_pattern(aes(pattern_type = scenario),
                         pattern              = 'magick',
                         pattern_fill         = 'black',
                         pattern_aspect_ratio = 1.75,
                         fill                 = 'white',
                         colour               = 'black',
                         outlier.alpha = 0.1) +
    ylab(bquote('Circularity indicator (t K'~year^-1*')')) +
    scale_pattern_type_discrete(choices = pattern_choice,
                                name = "Modelled Scenario", 
                                labels = c("Reference Year 2020", "Participatory Scenario", 
                                           "Crop Buffered Scenario", "Livestock Buffered Scenario")) +
    xlab('')+
    expand_limits(y=0)+
    scale_y_continuous(labels = scales::comma)+
    facet_wrap(~variable) + 
    theme_bw(base_size = base_size) +
    theme(    axis.text.x=element_blank(),
              axis.ticks.x=element_blank()
    )
  p3 <- p3.2 + guide_area() + p3.1  + plot_layout(design=design, guides = "collect") 
  
  
  ggsave(p1, filename = 'figures/boxplot_indicators_N_pattern.jpeg', device = 'jpeg', width = width, height = height, units = 'cm')
  ggsave(p2, filename = 'figures/boxplot_indicators_P_pattern.jpeg', device = 'jpeg', width = width, height = height, units = 'cm')
  ggsave(p3, filename = 'figures/boxplot_indicators_K_pattern.jpeg', device = 'jpeg', width = width, height = height, units = 'cm')
  
  
  
  
  
  
  #now make barplots
  p1.1 <- results_indicators_long %>% 
    filter(nutrient == 'N', variable %in% c('Recycling Rate', 'Reuse to Total Input', 'Use Efficiency')) %>%
    filter(value >= 0) %>% 
    group_by(scenario, variable) %>% 
    summarise(median = median(value),
              q_16 = quantile(value, probs = 0.16),
              q_84 = quantile(value, probs = 0.84)) %>% 
    na.omit() %>% 
    ggplot(aes(x = scenario, y = median, fill = scenario)) +
    geom_bar_pattern(aes(pattern_type = scenario),
                     pattern              = 'magick',
                     pattern_fill         = 'black',
                     pattern_aspect_ratio = 1.75,
                     fill                 = 'white',
                     colour               = 'black',
                     stat = 'identity') +
    geom_errorbar(aes(ymin = q_16, ymax = q_84),
                  width = 0.5)+
    ylab('Circularity indicator (%)') +
    xlab('')+
    ylim(0,100)+
    facet_wrap(~variable) + 
    scale_pattern_type_discrete(choices = pattern_choice) +
    theme_bw(base_size = base_size) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="none")
  
  p1.2 <- results_indicators_long %>% 
    filter(nutrient == 'N', variable %in% c('Total Input', 'Losses')) %>%
    filter(value >= 0) %>% 
    mutate(value = floor(value / 1000)) %>% 
    group_by(scenario, variable) %>% 
    summarise(median = median(value),
              q_16 = quantile(value, probs = 0.16),
              q_84 = quantile(value, probs = 0.84)) %>% 
    na.omit() %>% 
    ggplot(aes(x = scenario, y = median, fill = scenario)) +
    geom_bar_pattern(aes(pattern_type = scenario),
                     pattern              = 'magick',
                     pattern_fill         = 'black',
                     pattern_aspect_ratio = 1.75,
                     fill                 = 'white',
                     colour               = 'black',
                     stat = 'identity') +
    geom_errorbar(aes(ymin = q_16, ymax = q_84),
                  width = 0.5)+
    ylab(bquote('Circularity indicator (t N'~year^-1*')')) +
    xlab('')+
    scale_pattern_type_discrete(choices = pattern_choice,
                                name = "Modelled Scenario", 
                                labels = c("Reference Year 2020", "Participatory Scenario", 
                                           "Crop Buffered Scenario", "Livestock Buffered Scenario")) +
    scale_y_continuous(labels = scales::comma)+
    expand_limits(y=0)+
    facet_wrap(~variable) + 
    theme_bw(base_size = base_size) +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
  
  
  p1 <- p1.2 + guide_area() + p1.1  + plot_layout(design=design, guides = "collect")
  
  
  p2.1 <- results_indicators_long %>% 
    filter(nutrient == 'P', variable %in% c('Recycling Rate', 'Reuse to Total Input', 'Use Efficiency')) %>%
    filter(value >= 0) %>% 
    group_by(scenario, variable) %>% 
    summarise(median = median(value),
              q_16 = quantile(value, probs = 0.16),
              q_84 = quantile(value, probs = 0.84)) %>% 
    na.omit() %>% 
    ggplot(aes(x = scenario, y = median, fill = scenario)) +
    geom_bar_pattern(aes(pattern_type = scenario),
                     pattern              = 'magick',
                     pattern_fill         = 'black',
                     pattern_aspect_ratio = 1.75,
                     fill                 = 'white',
                     colour               = 'black',
                     stat = 'identity') +
    geom_errorbar(aes(ymin = q_16, ymax = q_84),
                  width = 0.5)+
    ylab('Circularity indicator (%)') +
    xlab('')+
    ylim(0,100)+
    facet_wrap(~variable) + 
    scale_pattern_type_discrete(choices = pattern_choice) +
    theme_bw(base_size = base_size) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="none")
  
  p2.2 <- results_indicators_long %>% 
    filter(nutrient == 'P', variable %in% c('Total Input', 'Losses')) %>%
    filter(value >= 0) %>% 
    mutate(value = floor(value / 1000)) %>% 
    group_by(scenario, variable) %>% 
    summarise(median = median(value),
              q_16 = quantile(value, probs = 0.16),
              q_84 = quantile(value, probs = 0.84)) %>% 
    na.omit() %>% 
    ggplot(aes(x = scenario, y = median, fill = scenario)) +
    geom_bar_pattern(aes(pattern_type = scenario),
                     pattern              = 'magick',
                     pattern_fill         = 'black',
                     pattern_aspect_ratio = 1.75,
                     fill                 = 'white',
                     colour               = 'black',
                     stat = 'identity') +
    geom_errorbar(aes(ymin = q_16, ymax = q_84),
                  width = 0.5)+
    ylab(bquote('Circularity indicator (t P'~year^-1*')')) +
    xlab('')+
    scale_pattern_type_discrete(choices = pattern_choice,
                                name = "Modelled Scenario", 
                                labels = c("Reference Year 2020", "Participatory Scenario", 
                                           "Crop Buffered Scenario", "Livestock Buffered Scenario")) +
    scale_y_continuous(labels = scales::comma)+
    expand_limits(y=0)+
    facet_wrap(~variable) + 
    theme_bw(base_size = base_size) +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
  
  
  p2 <- p2.2 + guide_area() + p2.1  + plot_layout(design=design, guides = "collect")
  
  
  p3.1 <- results_indicators_long %>% 
    filter(nutrient == 'K', variable %in% c('Recycling Rate', 'Reuse to Total Input', 'Use Efficiency')) %>%
    filter(value >= 0) %>% 
    group_by(scenario, variable) %>% 
    summarise(median = median(value),
              q_16 = quantile(value, probs = 0.16),
              q_84 = quantile(value, probs = 0.84)) %>% 
    na.omit() %>% 
    ggplot(aes(x = scenario, y = median, fill = scenario)) +
    geom_bar_pattern(aes(pattern_type = scenario),
                     pattern              = 'magick',
                     pattern_fill         = 'black',
                     pattern_aspect_ratio = 1.75,
                     fill                 = 'white',
                     colour               = 'black',
                     stat = 'identity') +
    geom_errorbar(aes(ymin = q_16, ymax = q_84),
                  width = 0.5)+
    ylab('Circularity indicator (%)') +
    xlab('')+
    ylim(0,100)+
    facet_wrap(~variable) + 
    scale_pattern_type_discrete(choices = pattern_choice) +
    theme_bw(base_size = base_size) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="none")
  
  p3.2 <- results_indicators_long %>% 
    filter(nutrient == 'K', variable %in% c('Total Input', 'Losses')) %>%
    filter(value >= 0) %>% 
    mutate(value = floor(value / 1000)) %>% 
    group_by(scenario, variable) %>% 
    summarise(median = median(value),
              q_16 = quantile(value, probs = 0.16),
              q_84 = quantile(value, probs = 0.84)) %>% 
    na.omit() %>% 
    ggplot(aes(x = scenario, y = median, fill = scenario)) +
    geom_bar_pattern(aes(pattern_type = scenario),
                     pattern              = 'magick',
                     pattern_fill         = 'black',
                     pattern_aspect_ratio = 1.75,
                     fill                 = 'white',
                     colour               = 'black',
                     stat = 'identity') +
    geom_errorbar(aes(ymin = q_16, ymax = q_84),
                  width = 0.5)+
    ylab(bquote('Circularity indicator (t K'~year^-1*')')) +
    xlab('')+
    scale_pattern_type_discrete(choices = pattern_choice,
                                name = "Modelled Scenario", 
                                labels = c("Reference Year 2020", "Participatory Scenario", 
                                           "Crop Buffered Scenario", "Livestock Buffered Scenario")) +
    scale_y_continuous(labels = scales::comma)+
    expand_limits(y=0)+
    facet_wrap(~variable) + 
    theme_bw(base_size = base_size) +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
  
  
  p3 <- p3.2 + guide_area() + p3.1  + plot_layout(design=design, guides = "collect")
  
  ggsave(p1, filename = 'figures/barplot_indicators_N_pattern.jpeg', device = 'jpeg', width = width, height = height, units = 'cm')
  ggsave(p2, filename = 'figures/barplot_indicators_P_pattern.jpeg', device = 'jpeg', width = width, height = height, units = 'cm')
  ggsave(p3, filename = 'figures/barplot_indicators_K_pattern.jpeg', device = 'jpeg', width = width, height = height, units = 'cm')
  
  
  
  
  p1.1 <- results_indicators_long %>% 
    filter(nutrient == 'N', variable %in% c('Recycling Rate', 'Reuse to Total Input', 'Use Efficiency')) %>%
    filter(value >= 0) %>% 
    group_by(scenario, variable) %>% 
    summarise(median = median(value),
              q_16 = quantile(value, probs = 0.16),
              q_84 = quantile(value, probs = 0.84)) %>% 
    na.omit() %>% 
    ggplot(aes(x = scenario, y = median, fill = scenario)) +
    geom_bar(aes(fill = scenario),
             stat = 'identity', col = 'black') +
    geom_errorbar(aes(ymin = q_16, ymax = q_84),
                  width = 0.5)+
    ylab('Circularity indicator (%)') +
    xlab('')+
    ylim(0,100)+
    facet_wrap(~variable) + 
    scale_fill_manual(name = "Modelled Scenario", 
                      labels = c("Reference Year 2020", "Participatory Scenario", 
                                 "Crop Buffered Scenario", "Livestock Buffered Scenario"),
                      values=cbp1) +
    theme_bw(base_size = base_size) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="none")
  
  p1.2 <- results_indicators_long %>% 
    filter(nutrient == 'N', variable %in% c('Total Input', 'Losses')) %>%
    filter(value >= 0) %>% 
    mutate(value = floor(value / 1000)) %>% 
    group_by(scenario, variable) %>% 
    summarise(median = median(value),
              q_16 = quantile(value, probs = 0.16),
              q_84 = quantile(value, probs = 0.84)) %>% 
    na.omit() %>% 
    ggplot(aes(x = scenario, y = median, fill = scenario)) +
    geom_bar(aes(fill = scenario),
             stat = 'identity', col = 'black') +
    geom_errorbar(aes(ymin = q_16, ymax = q_84),
                  width = 0.5)+
    ylab(bquote('Circularity indicator (t N'~year^-1*')')) +
    xlab('')+
    scale_fill_manual(name = "Modelled Scenario", 
                      labels = c("Reference Year 2020", "Participatory Scenario", 
                                 "Crop Buffered Scenario", "Livestock Buffered Scenario"),
                      values=cbp1) +
    scale_y_continuous(labels = scales::comma)+
    expand_limits(y=0)+
    facet_wrap(~variable) + 
    theme_bw(base_size = base_size) +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
  
  
  p1 <- p1.2 + guide_area() + p1.1  + plot_layout(design=design, guides = "collect")
  
  
  p2.1 <- results_indicators_long %>% 
    filter(nutrient == 'P', variable %in% c('Recycling Rate', 'Reuse to Total Input', 'Use Efficiency')) %>%
    filter(value >= 0) %>% 
    group_by(scenario, variable) %>% 
    summarise(median = median(value),
              q_16 = quantile(value, probs = 0.16),
              q_84 = quantile(value, probs = 0.84)) %>% 
    na.omit() %>% 
    ggplot(aes(x = scenario, y = median, fill = scenario)) +
    geom_bar(aes(fill = scenario),
             stat = 'identity', col = 'black') +
    geom_errorbar(aes(ymin = q_16, ymax = q_84),
                  width = 0.5)+
    ylab('Circularity indicator (%)') +
    xlab('')+
    ylim(0,100)+
    facet_wrap(~variable) + 
    scale_fill_manual(name = "Modelled Scenario", 
                      labels = c("Reference Year 2020", "Participatory Scenario", 
                                 "Crop Buffered Scenario", "Livestock Buffered Scenario"),
                      values=cbp1) +
    theme_bw(base_size = base_size) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="none")
  
  p2.2 <- results_indicators_long %>% 
    filter(nutrient == 'P', variable %in% c('Total Input', 'Losses')) %>%
    filter(value >= 0) %>% 
    mutate(value = floor(value / 1000)) %>% 
    group_by(scenario, variable) %>% 
    summarise(median = median(value),
              q_16 = quantile(value, probs = 0.16),
              q_84 = quantile(value, probs = 0.84)) %>% 
    na.omit() %>% 
    ggplot(aes(x = scenario, y = median, fill = scenario)) +
    geom_bar(aes(fill = scenario),
             stat = 'identity', col = 'black') +
    geom_errorbar(aes(ymin = q_16, ymax = q_84),
                  width = 0.5)+
    ylab(bquote('Circularity indicator (t P'~year^-1*')')) +
    xlab('')+
    scale_fill_manual(name = "Modelled Scenario", 
                      labels = c("Reference Year 2020", "Participatory Scenario", 
                                 "Crop Buffered Scenario", "Livestock Buffered Scenario"),
                      values=cbp1) +
    scale_y_continuous(labels = scales::comma)+
    expand_limits(y=0)+
    facet_wrap(~variable) + 
    theme_bw(base_size = base_size) +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
  
  
  p2 <- p2.2 + guide_area() + p2.1  + plot_layout(design=design, guides = "collect")
  
  
  p3.1 <- results_indicators_long %>% 
    filter(nutrient == 'K', variable %in% c('Recycling Rate', 'Reuse to Total Input', 'Use Efficiency')) %>%
    filter(value >= 0) %>% 
    group_by(scenario, variable) %>% 
    summarise(median = median(value),
              q_16 = quantile(value, probs = 0.16),
              q_84 = quantile(value, probs = 0.84)) %>% 
    na.omit() %>% 
    ggplot(aes(x = scenario, y = median, fill = scenario)) +
    geom_bar(aes(fill = scenario),
             stat = 'identity', col = 'black') +
    geom_errorbar(aes(ymin = q_16, ymax = q_84),
                  width = 0.5)+
    ylab('Circularity indicator (%)') +
    xlab('')+
    ylim(0,100)+
    facet_wrap(~variable) + 
    scale_fill_manual(name = "Modelled Scenario", 
                      labels = c("Reference Year 2020", "Participatory Scenario", 
                                 "Crop Buffered Scenario", "Livestock Buffered Scenario"),
                      values=cbp1) +
    theme_bw(base_size = base_size) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="none")
  
  p3.2 <- results_indicators_long %>% 
    filter(nutrient == 'K', variable %in% c('Total Input', 'Losses')) %>%
    filter(value >= 0) %>% 
    mutate(value = floor(value / 1000)) %>% 
    group_by(scenario, variable) %>% 
    summarise(median = median(value),
              q_16 = quantile(value, probs = 0.16),
              q_84 = quantile(value, probs = 0.84)) %>% 
    na.omit() %>% 
    ggplot(aes(x = scenario, y = median, fill = scenario)) +
    geom_bar(aes(fill = scenario),
             stat = 'identity', col = 'black') +
    geom_errorbar(aes(ymin = q_16, ymax = q_84),
                  width = 0.5)+
    ylab(bquote('Circularity indicator (t K'~year^-1*')')) +
    xlab('')+
    scale_fill_manual(name = "Modelled Scenario", 
                      labels = c("Reference Year 2020", "Participatory Scenario", 
                                 "Crop Buffered Scenario", "Livestock Buffered Scenario"),
                      values=cbp1) +
    scale_y_continuous(labels = scales::comma)+
    expand_limits(y=0)+
    facet_wrap(~variable) + 
    theme_bw(base_size = base_size) +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
  
  
  p3 <- p3.2 + guide_area() + p3.1  + plot_layout(design=design, guides = "collect")
  
  ggsave(p1, filename = 'figures/barplot_indicators_N.jpeg', device = 'jpeg', width = width, height = height, units = 'cm')
  ggsave(p2, filename = 'figures/barplot_indicators_P.jpeg', device = 'jpeg', width = width, height = height, units = 'cm')
  ggsave(p3, filename = 'figures/barplot_indicators_K.jpeg', device = 'jpeg', width = width, height = height, units = 'cm')
  
}






#make boxplots of indicators
# p1 <- results_indicators_long %>% 
#   filter(nutrient == 'N') %>%
#   ggplot(aes(x = scenario, y = value, fill = scenario)) +
#   geom_boxplot() +
#   ylab('Indicator Value') +
#   xlab('Model Scenario')+
#   facet_wrap(~variable, scales = 'free_y') + 
#   theme_bw(base_size = 15) +
#   theme(
#     legend.position="none",
#     #legend.position = c(0.8, 0.1), # c(0,0) bottom left, c(1,1) top-right.
#     #legend.background = element_rect(fill = "white", colour = NA),
#     #axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
#   )
# 
# p2 <- results_indicators_long %>% 
#   filter(nutrient == 'P') %>%
#   ggplot(aes(x = scenario, y = value, fill = scenario)) +
#   geom_boxplot() +
#   ylab('Indicator Value') +
#   xlab('Model Scenario')+
#   facet_wrap(~variable, scales = 'free_y') + 
#   theme_bw(base_size = 15) +
#   theme(
#     legend.position="none",
#     #legend.position = c(0.8, 0.1), # c(0,0) bottom left, c(1,1) top-right.
#     #legend.background = element_rect(fill = "white", colour = NA),
#     #axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
#   )
# 
# p3 <- results_indicators_long %>% 
#   filter(nutrient == 'K') %>%
#   ggplot(aes(x = scenario, y = value, fill = scenario)) +
#   geom_boxplot() +
#   ylab('Indicator Value') +
#   xlab('Model Scenario')+
#   facet_wrap(~variable, scales = 'free_y') + 
#   theme_bw(base_size = 15) +
#   theme(
#     legend.position="none",
#     #legend.position = c(0.8, 0.1), # c(0,0) bottom left, c(1,1) top-right.
#     #legend.background = element_rect(fill = "white", colour = NA),
#     #axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
#   )





#clear workspace
rm(diff_flows_df, diff_flows_long, diff_indicators_df, diff_indicators_long, p1, p2, p3, result_flows, result_flows_long, result_indicators, results_indicators_long)

#read model outputs again
result_flows <- readRDS('data/model_result_flows.rds')


#combine meat and dairy stuff
result_flows <- purrr::map(result_flows, function(x){
  #x$import_animal_products_N <- x$import_dairy_egg_N + x$import_meat_N
  x$export_animal_products_N <- x$export_dairy_egg_N + x$export_meat_N
  
  #x$import_animal_products_P <- x$import_dairy_egg_P + x$import_meat_P
  x$export_animal_products_P <- x$export_dairy_egg_P + x$export_meat_P
  
  #x$import_animal_products_K <- x$import_dairy_egg_K + x$import_meat_K
  x$export_animal_products_K <- x$export_dairy_egg_K + x$export_meat_K
  return(x)
})



#calculate change relative to the reference year
rel_change_flows <- rbind.data.frame(((result_flows$interventions[-1] - result_flows$reference_year[-1]) / result_flows$reference_year[-1]) * 100,
                                  ((result_flows$interventions_animal_adjusted[-1] - result_flows$reference_year[-1]) / result_flows$reference_year[-1])*100,
                                  ((result_flows$interventions_crop_adjusted[-1] - result_flows$reference_year[-1]) / result_flows$reference_year[-1]) * 100)
rel_change_flows$scenario <- c(result_flows$interventions$scenario, 
                               result_flows$interventions_animal_adjusted$scenario, 
                               result_flows$interventions_crop_adjusted$scenario)
rel_change_flows <- dplyr::relocate(rel_change_flows, scenario)
rel_change_flows

abs_change_flows <- rbind.data.frame((result_flows$interventions[-1] - result_flows$reference_year[-1]),
                                     (result_flows$interventions_animal_adjusted[-1] - result_flows$reference_year[-1]),
                                     (result_flows$interventions_crop_adjusted[-1] - result_flows$reference_year[-1]))

abs_change_flows$scenario <- c(result_flows$interventions$scenario, 
                               result_flows$interventions_animal_adjusted$scenario, 
                               result_flows$interventions_crop_adjusted$scenario)
abs_change_flows <- dplyr::relocate(abs_change_flows, scenario)
abs_change_flows$run <- rel_change_flows$run <- rep(1:10000, 3)



rel_change_flows$scenario <- factor(rel_change_flows$scenario, levels = c("reference_year","interventions","interventions_animal_adjusted", "interventions_crop_adjusted"),
                                      labels = c('Ref', 'PS', 'LBS' ,'CBS'))
abs_change_flows$scenario <- factor(abs_change_flows$scenario, levels = c("reference_year","interventions","interventions_animal_adjusted", "interventions_crop_adjusted"),
                                    labels = c('Ref', 'PS', 'LBS' ,'CBS'))


#summarise results similar to eduardos banana paper
#--> calculate median and iqr

rel_change_flows_long <- reshape2::melt(rel_change_flows, id.vars = c('scenario', 'run'))
abs_change_flows_long <- reshape2::melt(abs_change_flows, id.vars = c('scenario', 'run'), value.name = 'abs_change')

#zero devided by zero gives nan, so make it 0 again
rel_change_flows_long$value <- ifelse(is.nan(rel_change_flows_long$value), yes = 0, no = rel_change_flows_long$value)

summarised_flows <- rel_change_flows_long %>%
  merge.data.frame(abs_change_flows_long, by = c("scenario",   "variable", "run"), all.x = TRUE) %>% 
  group_by(scenario, variable) %>%
  summarise(median = median(value, na.rm = T),
            iqr = IQR(value, na.rm = T),
            median_abs = median(abs_change),
            iqr_abs = IQR(abs_change))

#percent increase with zero as starting point does not make sense
#also value very close to zero, so changes are not meaningfull
summarised_flows$median <- ifelse(summarised_flows$scenario %in% c('CBS', 'LBS') &
                                    summarised_flows$variable %in% c('animal_balance_N', 'animal_balance_K', 'animal_balance_P'),
                                  yes = 0, no = summarised_flows$median)

#changes in animal balance K and P are close to zero, so calculating % change is misleaing
summarised_flows$median <- ifelse(summarised_flows$scenario %in% c('PS') &
                                    summarised_flows$variable %in% c('animal_balance_K', 'animal_balance_P'),
                                  yes = 0, no = summarised_flows$median)



#split by nutrients; 
rel_summarised_flows <- tidyr::separate(data = summarised_flows, col = variable, sep = -1, convert = TRUE, into = c('variable', 'nutrient'))

#remove trailing _
rel_summarised_flows <- rel_summarised_flows %>% 
  mutate(variable = substring(variable, 1, nchar(variable)-1))

rel_summarised_indicators <- rel_summarised_flows %>% 
  filter(variable %in% c("total_input", 'losses', "recycling_rate", "share_reuse_to_total_input", "use_efficiency"))

#only take flows Bernou wants to see
rel_summarised_flows <- rel_summarised_flows %>%
  filter(variable %in% c('manure_to_crop', 'manure_export', 'manure_as_biogas_substrate',
                         'import_inorganic_fertilizer', 'vegetal_biogas_substrate',
                         'feed_from_processed_crops', 
                         'net_food_import', 'crop_cultivation_losses', 'import_animal_products', 
                         'export_animal_products',
                         'animal_housing_and_storage_losses', 'animal_balance'))


rel_summarised_flows$variable <- factor(rel_summarised_flows$variable, 
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

factor(rel_summarised_flows$variable, levels = c('Manure to crops',
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
rel_summarised_flows$increase <- ifelse(rel_summarised_flows$median >= 100,yes = rel_summarised_flows$median, no = NA)
rel_summarised_flows$decrease <- ifelse(rel_summarised_flows$median < 100,yes = rel_summarised_flows$median, no = NA)

#iqr for some parameters complete out of range, set to 2
rel_summarised_flows$iqr_adusted <- ifelse(rel_summarised_flows$iqr > 200, yes = 200, no = rel_summarised_flows$iqr)
#also have 2 as a limit for median, otherwise the scale is completely off
rel_summarised_flows$median_adjusted <- ifelse(rel_summarised_flows$median > 200, yes = 200, no = rel_summarised_flows$median)
rel_summarised_flows$median_adjusted <- ifelse(rel_summarised_flows$median_adjusted < -100, yes = -100, no = rel_summarised_flows$median_adjusted)

#if the iqr is zero, then the reference value is zero and the change is certain???
rel_summarised_flows$iqr_adusted <- ifelse(is.na(rel_summarised_flows$iqr_adusted), 0, rel_summarised_flows$iqr_adusted)
rel_summarised_flows$iqr_adusted <- ifelse(rel_summarised_flows$variable == 'Stock balance animal subsystem', yes = 0, no = rel_summarised_flows$iqr_adusted)

#animal stock: iqr is 


#install.packages("ggnewscale")
library(ggnewscale)
#RColorBrewer::brewer.pal(7, 'PuOr')

rel_summarised_flows <- rel_summarised_flows %>%
  mutate(scenario = factor(scenario, levels = c('Ref', 'PS', 'CBS', 'LBS')))

#animal balance values are missleading, because the reference is zero
rel_summarised_flows %>% 
  filter(variable == 'Stock balance animal subsystem')


#########
#add reference year flows
##########

#add another column with the reference year
result_flows$reference_year$run <- 1:10000

test_long <- reshape2::melt(result_flows$reference_year, id.vars = c('scenario', 'run'))

summarised_flows_reference <- test_long %>%
  group_by(scenario, variable) %>%
  summarise(median = median(value, na.rm = T),
            iqr = IQR(value, na.rm = T)) %>% 
  mutate(iqr = (iqr / median) * 100,
         median_adjusted = 0,
         median_abs = median)

#if relative iqr is na, change it to zero instead
#(is caused by having reference value (median) equals zero)
#summarised_flows_reference$iqr_adusted <- ifelse(is.na(summarised_flows_reference$iqr), yes = 0, no = summarised_flows_reference$iqr)
summarised_flows_reference$iqr_adusted <- NaN


#split nutrient from variable name
summarised_flows_reference <- tidyr::separate(data = summarised_flows_reference, col = variable, sep = -1, convert = TRUE, into = c('variable', 'nutrient'))

#remove trailing _
summarised_flows_reference <- summarised_flows_reference %>% 
  mutate(variable = substring(variable, 1, nchar(variable)-1))

#only take flows Bernou wants to see
summarised_flows_reference <- summarised_flows_reference %>%
  filter(variable %in% c('manure_to_crop', 'manure_export', 'manure_as_biogas_substrate',
                         'import_inorganic_fertilizer', 'vegetal_biogas_substrate',
                         'feed_from_processed_crops', 
                         'net_food_import', 'crop_cultivation_losses', 'import_animal_products', 
                         'export_animal_products',
                         'animal_housing_and_storage_losses', 'animal_balance'))


summarised_flows_reference$variable <- factor(summarised_flows_reference$variable, 
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

factor(summarised_flows_reference$variable, levels = c('Manure to crops',
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







#add 
summarised_flows_reference$iqr_abs <- summarised_flows_reference$increase <- summarised_flows_reference$decrease <- NA
colnames(rel_summarised_flows)
colnames(summarised_flows_reference)

summarised_flows_reference$scenario <- 'Ref'
summarised_flows_reference$scenario <- factor(summarised_flows_reference$scenario, 
                                              levels = c('Ref', 'PS', 'CBS', 'LBS'))



p1 <- rel_summarised_flows %>%
  rbind(summarised_flows_reference) %>% 
  filter(nutrient == 'N', median_adjusted >= 0) %>%
  ggplot(aes(x = scenario, y = variable)) +
  geom_tile(aes(fill = median_adjusted, x = factor(scenario, levels = c('Ref', 'PS', 'CBS', 'LBS'))),
            data = rel_summarised_flows[rel_summarised_flows$nutrient == 'N' & rel_summarised_flows$median_adjusted < 0,],
            colour="white", size=2) +
  scale_fill_gradient2("Median reduction (%)", limits = c(-100, -0), 
                       low = "#a6611a", mid = "grey95") +
  new_scale("fill") +
  geom_tile(aes(fill = median_adjusted), colour="white", size=2) +
  scale_fill_gradient2("Increase (%)", limits = c(0, 200), 
                       mid = "grey95", high = "#018571") +
  geom_point(aes(size = iqr_adusted, x = factor(scenario, levels = c('Ref', 'PS', 'CBS', 'LBS'))), 
             data = rel_summarised_flows[rel_summarised_flows$nutrient == 'N',], 
             col = 'grey50') + 
  geom_point(aes(size = iqr_adusted), 
             data = summarised_flows_reference[summarised_flows_reference$nutrient == 'N',], col = 'grey50') + 
  geom_text(data = rel_summarised_flows[rel_summarised_flows$nutrient == 'N' & rel_summarised_flows$median_adjusted >= 0,],
            aes(label = paste0('+', floor(abs(median_abs)/1000))), nudge_y = -0.3)+
  geom_text(data = rel_summarised_flows[rel_summarised_flows$nutrient == 'N' & rel_summarised_flows$median_adjusted < 0,],
            aes(label = paste0('-', floor(abs(median_abs)/1000))), nudge_y = -0.3)+
  geom_text(data = summarised_flows_reference[summarised_flows_reference$nutrient == 'N',],
            aes(label =  floor(abs(median_abs)/1000)), nudge_y = -0.3)+
  scale_size(range = c(.1, 7), name="IQR (%)") +
  theme_bw(base_size =  15) + ylab('Nitrogen flow') + xlab('Scenario')+
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

label <- paste0('Numbers indicate median\nnutrient flow (N t ',
                as.character(expression("year"^-{1})),
                ")\nfor reference year and\nchanges in median\nfor the scenarios")

#label <- as.character('Numbers in panels\nindicate median nutrient\nflow (N t / year) for\nreference scenario (Ref)\nand changes in median\nfor the other scenarios')
label <- as.character('Numbers in panels indicate median\nnutrient flow (N t / year) for      \nreference scenario and median\nchanges for other scenarios         ')


p1 <- rel_summarised_flows %>%
  rbind(summarised_flows_reference) %>% 
  filter(nutrient == 'N') %>%
  ggplot(aes(x = scenario, y = variable)) +
  geom_tile(fill = 'grey95', color = 'white', size=2)+
  geom_tile(aes(fill = median_adjusted, x = factor(scenario, levels = c('Ref', 'PS', 'CBS', 'LBS'))),
            data = rel_summarised_flows[rel_summarised_flows$nutrient == 'N' & rel_summarised_flows$median_adjusted < 0,],
            colour="white", size=2) +
  scale_fill_gradient2("Median reduction (%)\ncompared to\nreference scenario", limits = c(-100, -0), 
                       low = "#a6611a", mid = "grey95") +
  new_scale("fill") +
  geom_tile(data = rel_summarised_flows[rel_summarised_flows$nutrient == 'N' & rel_summarised_flows$median_adjusted >= 0,],
            aes(fill = median_adjusted), colour="white", size=2) +
  scale_fill_gradient2("Median increase (%)\ncompared to\nreference scenario", limits = c(0, 200), 
                       mid = "grey95", high = "#018571") +
  geom_point(aes(size = iqr_adusted, x = factor(scenario, levels = c('Ref', 'PS', 'CBS', 'LBS'))), 
             data = rel_summarised_flows[rel_summarised_flows$nutrient == 'N',], 
             col = 'grey50') + 
  geom_point(aes(size = iqr_adusted), 
             data = summarised_flows_reference[summarised_flows_reference$nutrient == 'N',], col = 'grey50') + 
  geom_text(data = rel_summarised_flows[rel_summarised_flows$nutrient == 'N' & rel_summarised_flows$median_adjusted >= 0,],
            aes(label = paste0('+', floor(abs(median_abs)/1000))), nudge_y = -0.3)+
  geom_text(data = rel_summarised_flows[rel_summarised_flows$nutrient == 'N' & rel_summarised_flows$median_adjusted < 0,],
            aes(label = paste0('-', floor(abs(median_abs)/1000))), nudge_y = -0.3)+
  geom_text(data = summarised_flows_reference[summarised_flows_reference$nutrient == 'N',],
            aes(label =  floor(abs(median_abs)/1000)), nudge_y = -0.3)+
  scale_size(range = c(.1, 7), name="Interquartile range (%)") +
  theme_bw(base_size = 15) + ylab('Nitrogen flow') + xlab('Scenario')+
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

p1_annotated <- p1 +
  geom_text(x = Inf, y = 0.5,
           #label = bquote('Numbers median nutrient flow [N t year'~(year^-1)~'] for 
            #              reference year and median absolute changes for the scenarios'),
           label = label,
           hjust = -0.05, size = 4, 
           #parse=TRUE) 
  )

library(grid)
library(gtable)

# Turn off clipping to the plot panel
g = ggplotGrob(p1_annotated)
g$layout$clip[g$layout$name == "panel"] = "off"
grid.draw(g)

ggsave(g, filename = 'flow_changes_N_test.jpg', path = 'figures/', device = 'jpeg',
       height = 22, width = 22, units = 'cm')







label <- as.character('Numbers in panels indicate median\nnutrient flow (P t / year) for      \nreference scenario and median\nchanges for other scenarios         ')

nutrient <- 'P'

p2 <- rel_summarised_flows %>%
  rbind(summarised_flows_reference) %>% 
  filter(nutrient == nutrient) %>%
  ggplot(aes(x = scenario, y = variable)) +
  geom_tile(fill = 'grey95', color = 'white', size=2)+
  geom_tile(aes(fill = median_adjusted, x = factor(scenario, levels = c('Ref', 'PS', 'CBS', 'LBS'))),
            data = rel_summarised_flows[rel_summarised_flows$nutrient == nutrient & rel_summarised_flows$median_adjusted < 0,],
            colour="white", size=2) +
  scale_fill_gradient2("Median reduction (%)\ncompared to\nreference scenario", limits = c(-100, -0), 
                       low = "#a6611a", mid = "grey95") +
  new_scale("fill") +
  geom_tile(data = rel_summarised_flows[rel_summarised_flows$nutrient == nutrient & rel_summarised_flows$median_adjusted >= 0,],
            aes(fill = median_adjusted), colour="white", size=2) +
  scale_fill_gradient2("Median increase (%)\ncompared to\nreference scenario", limits = c(0, 200), 
                       mid = "grey95", high = "#018571") +
  geom_point(aes(size = iqr_adusted, x = factor(scenario, levels = c('Ref', 'PS', 'CBS', 'LBS'))), 
             data = rel_summarised_flows[rel_summarised_flows$nutrient == nutrient,], 
             col = 'grey50') + 
  geom_point(aes(size = iqr_adusted), 
             data = summarised_flows_reference[summarised_flows_reference$nutrient == nutrient,], col = 'grey50') + 
  geom_text(data = rel_summarised_flows[rel_summarised_flows$nutrient == nutrient & rel_summarised_flows$median_adjusted >= 0,],
            aes(label = paste0('+', floor(abs(median_abs)/1000))), nudge_y = -0.3)+
  geom_text(data = rel_summarised_flows[rel_summarised_flows$nutrient == nutrient & rel_summarised_flows$median_adjusted < 0,],
            aes(label = paste0('-', floor(abs(median_abs)/1000))), nudge_y = -0.3)+
  geom_text(data = summarised_flows_reference[summarised_flows_reference$nutrient == nutrient,],
            aes(label =  floor(abs(median_abs)/1000)), nudge_y = -0.3)+
  scale_size(range = c(.1, 7), name="Interquartile range (%)") +
  theme_bw(base_size = 15) + ylab('Phosphorous flow') + xlab('Scenario')+
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

p2_annotated <- p2 +
  geom_text(x = Inf, y = 0.5,
            #label = bquote('Numbers median nutrient flow [N t year'~(year^-1)~'] for 
            #              reference year and median absolute changes for the scenarios'),
            label = label,
            hjust = -0.05, size = 4, 
            #parse=TRUE) 
  )

# Turn off clipping to the plot panel
g = ggplotGrob(p2_annotated)
g$layout$clip[g$layout$name == "panel"] = "off"
grid.draw(g)

ggsave(g, filename = 'flow_changes_P_test.jpg', path = 'figures/', device = 'jpeg',
       height = 22, width = 22, units = 'cm')



# p2 <- summarised_flows %>%
#   filter(nutrient == 'P', median_adjusted >= 0) %>%
#   ggplot(aes(x = scenario, y = variable)) +
#   geom_tile(aes(fill = median_adjusted), data = summarised_flows[summarised_flows$nutrient == 'P' & summarised_flows$median_adjusted < 0,],
#             colour="white", size=2) +
#   scale_fill_gradient2("Decrease (%)", limits = c(-100, -0), 
#                        low = "#542788", mid = "grey95") +
#   new_scale("fill") +
#   geom_tile(aes(fill = median_adjusted), colour="white", size=2) +
#   scale_fill_gradient2("Increase (%)", limits = c(0, 200), 
#                        mid = "grey95", high = "#B35806") +
#   geom_point(aes(size = iqr_adusted), data = summarised_flows[summarised_flows$nutrient == 'P',], col = 'grey50') + 
#   scale_size(range = c(.1, 7), name="IQR (%)") +
#   theme_bw() +
#   ylab('Phosporous flow') + xlab('Scenario')+
#   scale_y_discrete(limits=rev(c('Manure to crops',
#                                 'Manure export',
#                                 'Manure biogas substrate',
#                                 'Import inorganic fertilizers',
#                                 'Vegetal biogas substrate',
#                                 'Feed from processed crops',
#                                 'Animal products import',
#                                 'Animal products export',
#                                 'Net food import',
#                                 'Cultivation losses',
#                                 'Animal housing and storage losses',
#                                 'Stock balance animal subsystem')))
# 
# ggsave(p2, filename = 'flow_changes_P.jpg', path = 'figures/', device = 'jpeg', height = 20, width = 15, units = 'cm')



label <- as.character('Numbers in panels indicate median\nnutrient flow (K t / year) for      \nreference scenario and median\nchanges for other scenarios         ')

nutrient <- 'K'

p3 <- rel_summarised_flows %>%
  rbind(summarised_flows_reference) %>% 
  filter(nutrient == nutrient) %>%
  ggplot(aes(x = scenario, y = variable)) +
  geom_tile(fill = 'grey95', color = 'white', size=2)+
  geom_tile(aes(fill = median_adjusted, x = factor(scenario, levels = c('Ref', 'PS', 'CBS', 'LBS'))),
            data = rel_summarised_flows[rel_summarised_flows$nutrient == nutrient & rel_summarised_flows$median_adjusted < 0,],
            colour="white", size=2) +
  scale_fill_gradient2("Median reduction (%)\ncompared to\nreference scenario", limits = c(-100, -0), 
                       low = "#a6611a", mid = "grey95") +
  new_scale("fill") +
  geom_tile(data = rel_summarised_flows[rel_summarised_flows$nutrient == nutrient & rel_summarised_flows$median_adjusted >= 0,],
            aes(fill = median_adjusted), colour="white", size=2) +
  scale_fill_gradient2("Median increase (%)\ncompared to\nreference scenario", limits = c(0, 200), 
                       mid = "grey95", high = "#018571") +
  geom_point(aes(size = iqr_adusted, x = factor(scenario, levels = c('Ref', 'PS', 'CBS', 'LBS'))), 
             data = rel_summarised_flows[rel_summarised_flows$nutrient == nutrient,], 
             col = 'grey50') + 
  geom_point(aes(size = iqr_adusted), 
             data = summarised_flows_reference[summarised_flows_reference$nutrient == nutrient,], col = 'grey50') + 
  geom_text(data = rel_summarised_flows[rel_summarised_flows$nutrient == nutrient & rel_summarised_flows$median_adjusted >= 0,],
            aes(label = paste0('+', floor(abs(median_abs)/1000))), nudge_y = -0.3)+
  geom_text(data = rel_summarised_flows[rel_summarised_flows$nutrient == nutrient & rel_summarised_flows$median_adjusted < 0,],
            aes(label = paste0('-', floor(abs(median_abs)/1000))), nudge_y = -0.3)+
  geom_text(data = summarised_flows_reference[summarised_flows_reference$nutrient == nutrient,],
            aes(label =  floor(abs(median_abs)/1000)), nudge_y = -0.3)+
  scale_size(range = c(.1, 7), name="Interquartile range (%)") +
  theme_bw(base_size = 15) + ylab('Potassium flow') + xlab('Scenario')+
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

p3_annotated <- p3 +
  geom_text(x = Inf, y = 0.5,
            #label = bquote('Numbers median nutrient flow [N t year'~(year^-1)~'] for 
            #              reference year and median absolute changes for the scenarios'),
            label = label,
            hjust = -0.05, size = 4, 
            #parse=TRUE) 
  )

# Turn off clipping to the plot panel
g = ggplotGrob(p3_annotated)
g$layout$clip[g$layout$name == "panel"] = "off"
grid.draw(g)

ggsave(g, filename = 'flow_changes_K_test.jpg', path = 'figures/', device = 'jpeg',
       height = 22, width = 22, units = 'cm')

# p3 <- summarised_flows %>%
#   filter(nutrient == 'K', median_adjusted >= 0) %>%
#   ggplot(aes(x = scenario, y = variable)) +
#   geom_tile(aes(fill = median_adjusted), data = summarised_flows[summarised_flows$nutrient == 'K' & summarised_flows$median_adjusted < 0,],
#             colour="white", size=2) +
#   scale_fill_gradient2("Decrease (%)", limits = c(-100, -0), 
#                        low = "#542788", mid = "grey95") +
#   new_scale("fill") +
#   geom_tile(aes(fill = median_adjusted), colour="white", size=2) +
#   scale_fill_gradient2("Increase (%)", limits = c(0, 200), 
#                        mid = "grey95", high = "#B35806") +
#   geom_point(aes(size = iqr_adusted), data = summarised_flows[summarised_flows$nutrient == 'K',], col = 'grey50') + 
#   scale_size(range = c(.1, 7), name="IQR (%)") +
#   theme_bw() +  ylab('Potassium flow') + xlab('Scenario')+
#   scale_y_discrete(limits=rev(c('Manure to crops',
#                                 'Manure export',
#                                 'Manure biogas substrate',
#                                 'Import inorganic fertilizers',
#                                 'Vegetal biogas substrate',
#                                 'Feed from processed crops',
#                                 'Animal products import',
#                                 'Animal products export',
#                                 'Net food import',
#                                 'Cultivation losses',
#                                 'Animal housing and storage losses',
#                                 'Stock balance animal subsystem')))
# 
# ggsave(p3, filename = 'flow_changes_K.jpg', path = 'figures/', device = 'jpeg', height = 20, width = 15, units = 'cm')






######bar chart of indicators
#add another column with the reference year

result_flows <- readRDS('data/model_result_flows.rds')

result_flows <- do.call(rbind, result_flows)

#change names of scenarios
result_flows$scenario <- factor(result_flows$scenario, levels = c("reference_year","interventions","interventions_animal_adjusted", "interventions_crop_adjusted"),
                                labels = c('Ref', 'PS', 'LBS' ,'CBS'))

result_flows <- na.omit(result_flows)

result_flows$run <- rep(rep(1:10000), length(unique(result_flows$scenario)))

test_long <- reshape2::melt(result_flows, id.vars = c('scenario', 'run'))

summarised_flows <- test_long %>%
  group_by(scenario, variable) %>%
  summarise(median = median(value, na.rm = T),
            iqr = IQR(value, na.rm = T),
            q_16 = quantile(value, probs = 0.16),
            q_84 = quantile(value, probs = 0.84)) %>% 
  mutate(scenario = factor(scenario, levels = c('Ref', 'PS', 'CBS', 'LBS')))

#if relative iqr is na, change it to zero instead
#(is caused by having reference value (median) equals zero)
#summarised_flows_reference$iqr_adusted <- ifelse(is.na(summarised_flows_reference$iqr), yes = 0, no = summarised_flows_reference$iqr)

#split nutrient from variable name
summarised_flows <- tidyr::separate(data = summarised_flows, col = variable, sep = -1, convert = TRUE, into = c('variable', 'nutrient'))

#remove trailing _
summarised_flows <- summarised_flows %>% 
  mutate(variable = substring(variable, 1, nchar(variable)-1))

unique(summarised_flows$variable)

#only take flows Bernou wants to see
summarised_flows <- summarised_flows %>%
  filter(variable %in% c('total_input', 'use_efficiency', 'share_reuse_to_total_input',
                         'recycling_rate', 'losses'))


summarised_flows_reference$variable <- factor(summarised_flows_reference$variable, 
                                              levels = c('total_input', 
                                                         'use_efficiency', 
                                                         'share_reuse_to_total_input',
                                                         'recycling_rate', 
                                                         'losses'), 
                                              
                                              labels = c('Total Input',
                                                         'Nutrient Use Efficiency',
                                                         'Share of Reuse to Total Input',
                                                         'Recycling Rate',
                                                         'Nutrient Losses'
                                              ))

# factor(summarised_flows_reference$variable, levels = c('Manure to crops',
#                                                        'Manure export',
#                                                        'Manure biogas substrate',
#                                                        'Import inorganic fertilizers',
#                                                        'Vegetal biogas substrate',
#                                                        'Feed from processed crops',
#                                                        'Animal products import',
#                                                        'Animal products export',
#                                                        'Net food import',
#                                                        'Cultivation losses',
#                                                        'Animal housing and storage losses',
#                                                        'Stock balance animal subsystem'
# ))
# 
# summarised_flows %>% 
#   filter(nutrient == 'N') %>% 
#   ggplot(aes(x = variable, y = median, fill = scenario)) +
#   geom_bar(stat="identity", position="dodge")
# 
# 
# 
# 
# 
# 
# 
# 
# p4 <- summarised_indicators %>%
#   filter(nutrient == 'N', median_adjusted >= 0) %>%
#   ggplot(aes(x = scenario, y = variable)) +
#   geom_tile(aes(fill = median_adjusted), data = summarised_indicators[summarised_indicators$nutrient == 'N' & summarised_indicators$median_adjusted < 0,],
#             colour="white", size=2) +
#   scale_fill_gradient2("Decrease (%)", limits = c(-100, -0), 
#                        low = "#542788", mid = "grey95") +
#   new_scale("fill") +
#   geom_tile(aes(fill = median_adjusted), colour="white", size=2) +
#   scale_fill_gradient2("Increase (%)", limits = c(0, 100), 
#                        mid = "grey95", high = "#B35806") +
#   geom_point(aes(size = iqr_adusted), data = summarised_indicators[summarised_indicators$nutrient == 'N',], col = 'grey50') + 
#   scale_size(range = c(.1, 7), name="IQR (%)") +
#   theme_bw(base_size = 15) +
#   ylab('Circularity Indicators') + xlab('Scenario')+
#   scale_y_discrete(limits=rev(c('Total Input',
#                                 'Losses',
#                                 'Use Efficiency',
#                                 'Recycling Rate',
#                                 'Reuse : Total Input')))
# 
# ggsave(p4, filename = 'indicators_changes_N.jpg', path = 'figures/', device = 'jpeg', height = 20, width = 15, units = 'cm')
# 
# 
# p5 <- summarised_indicators %>%
#   filter(nutrient == 'P', median_adjusted >= 0) %>%
#   ggplot(aes(x = scenario, y = variable)) +
#   geom_tile(aes(fill = median_adjusted), data = summarised_indicators[summarised_indicators$nutrient == 'N' & summarised_indicators$median_adjusted < 0,],
#             colour="white", size=2) +
#   scale_fill_gradient2("Decrease (%)", limits = c(-100, -0), 
#                        low = "#542788", mid = "grey95") +
#   new_scale("fill") +
#   geom_tile(aes(fill = median_adjusted), colour="white", size=2) +
#   scale_fill_gradient2("Increase (%)", limits = c(0, 100), 
#                        mid = "grey95", high = "#B35806") +
#   geom_point(aes(size = iqr_adusted), data = summarised_indicators[summarised_indicators$nutrient == 'P',], col = 'grey50') + 
#   scale_size(range = c(.1, 7), name="IQR (%)") +
#   theme_bw(base_size = 15) +
#   ylab('Circularity Indicators') + xlab('Scenario')+
#   scale_y_discrete(limits=rev(c('Total Input',
#                                 'Losses',
#                                 'Use Efficiency',
#                                 'Recycling Rate',
#                                 'Reuse : Total Input')))
# 
# ggsave(p5, filename = 'indicators_changes_P.jpg', path = 'figures/', device = 'jpeg', height = 20, width = 15, units = 'cm')
# 
# 
# p6 <- summarised_indicators %>%
#   filter(nutrient == 'K', median_adjusted >= 0) %>%
#   ggplot(aes(x = scenario, y = variable)) +
#   geom_tile(aes(fill = median_adjusted), data = summarised_indicators[summarised_indicators$nutrient == 'N' & summarised_indicators$median_adjusted < 0,],
#             colour="white", size=2) +
#   scale_fill_gradient2("Decrease (%)", limits = c(-100, -0), 
#                        low = "#542788", mid = "grey95") +
#   new_scale("fill") +
#   geom_tile(aes(fill = median_adjusted), colour="white", size=2) +
#   scale_fill_gradient2("Increase (%)", limits = c(0, 100), 
#                        mid = "grey95", high = "#B35806") +
#   geom_point(aes(size = iqr_adusted), data = summarised_indicators[summarised_indicators$nutrient == 'K',], col = 'grey50') + 
#   scale_size(range = c(.1, 7), name="IQR (%)") +
#   theme_bw(base_size = 15) +
#   ylab('Circularity Indicators') + xlab('Scenario')+
#   scale_y_discrete(limits=rev(c('Total Input',
#                                 'Losses',
#                                 'Use Efficiency',
#                                 'Recycling Rate',
#                                 'Reuse : Total Input'))) 
# 
# ggsave(p6, filename = 'indicators_changes_K.jpg', path = 'figures/', device = 'jpeg', height = 20, width = 15, units = 'cm')
