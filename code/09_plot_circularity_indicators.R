#make boxplot for circularity indicators
library(tidyverse)
library(scales) #to have non-scientific numbers for the x and y axis

#read scenario data
result_flows <- readRDS('data/model_result_flows.rds')


result_flows <- result_flows %>% 
  do.call(rbind, .) %>% 
  mutate(use_efficiency_N = (((feed_crops_N +
                               straw_N +
                               grassbased_feed_N +
                               food_and_feed_crops_N +
                               fruit_and_vegetable_N +
                               egg_and_dairy_N +
                               vegetal_biogas_substrate_N +
                               slaughter_animal_N) /
                               (manure_to_crop_N +
                                  net_feed_import_N +
                                  import_inorganic_fertilizer_N +
                                  feed_crops_N +
                                  grassbased_feed_N +
                                  digestate_N +
                                  import_organic_fertilizer_N +
                                  feed_from_processed_crops_N +
                                  fresh_compost_crop_N +
                                  sewage_N +
                                  straw_N)) * 100),
         use_efficiency_K = (((feed_crops_K +
                                 straw_K +
                                 grassbased_feed_K +
                                 food_and_feed_crops_K +
                                 fruit_and_vegetable_K +
                                 egg_and_dairy_K +
                                 vegetal_biogas_substrate_K +
                                 slaughter_animal_K) /
                                (manure_to_crop_K +
                                   net_feed_import_K +
                                   import_inorganic_fertilizer_K +
                                   feed_crops_K +
                                   grassbased_feed_K +
                                   digestate_K +
                                   import_organic_fertilizer_K +
                                   feed_from_processed_crops_K +
                                   fresh_compost_crop_K +
                                   sewage_K +
                                   straw_K)) * 100),
         use_efficiency_P = (((feed_crops_P +
                                 straw_P +
                                 grassbased_feed_P +
                                 food_and_feed_crops_P +
                                 fruit_and_vegetable_P +
                                 egg_and_dairy_P +
                                 vegetal_biogas_substrate_P +
                                 slaughter_animal_P) /
                                (manure_to_crop_P +
                                   net_feed_import_P +
                                   import_inorganic_fertilizer_P +
                                   feed_crops_P +
                                   grassbased_feed_P +
                                   digestate_P +
                                   import_organic_fertilizer_P +
                                   feed_from_processed_crops_P +
                                   fresh_compost_crop_P +
                                   sewage_P +
                                   straw_P)) * 100))


result_flows <- result_flows %>% 
  mutate(scenario = factor(scenario, 
                           levels = c("reference_year","interventions", "interventions_crop_adjusted","interventions_animal_adjusted"),
                           labels = c('Ref', 'PS', 'CBS','LBS'))) %>% 
  na.omit()  %>% 
  mutate(run = rep(1:10000, length(unique(scenario)))) %>% 
  reshape2::melt(id.vars = c('run', 'scenario')) %>% 
  tidyr::separate(col = variable, sep = -1, convert = TRUE, into = c('variable', 'nutrient')) %>% 
  mutate(variable = substring(variable, 1, nchar(variable)-1)) %>% 
  filter(variable %in% c("total_input", 'losses', "recycling_rate", "share_reuse_to_total_input", "use_efficiency")) %>% 
  mutate(variable = factor(variable, 
                           levels = c("total_input", 'losses', "recycling_rate", "share_reuse_to_total_input", "use_efficiency"),
                           labels = c('Total Input', 'Losses', 'Recycling Rate', 'Reuse to Total Input', 'Use Efficiency')))

results_indicators_long <- result_flows


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
  
  #express the circularity indicators per ha agricultural area
  ha_ag_land <- 73014
  
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
    ylab('N Circularity Indicator (%)') +
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
    mutate(value = value / ha_ag_land) %>% 
    ggplot(aes(x = scenario, y = value, fill = scenario)) +
    geom_boxplot(outlier.alpha = 0.1) +
    ylab(bquote('N Circularity Indicator (t N'~ha^-1~year^-1*')')) +
    xlab('')+
    scale_fill_manual(name = "Modelled Scenarios", 
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
  
  
  ggsave(p1, filename = 'figures/boxplot_indicators_N.jpeg', device = 'jpeg', width = width, height = height +4, units = 'cm')
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
