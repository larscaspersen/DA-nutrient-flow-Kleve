#do pls
library(tidyverse)
library(decisionSupport)


#read saved runs for flows and for indicators
result_indicators <- readRDS('data/model_result_indicators.rds')
nitrogen_mc_simulation <- readRDS('data/model_output_indicators.rds')

#read input file
input <- read.csv("data/input_all_uncertainty_classes.csv")
#remove median values
input$median <- NA

diff_indicators_df <- rbind.data.frame(result_indicators$interventions[-1] - result_indicators$reference_year[-1],
                                       result_indicators$interventions_animal_adjusted[-1] - result_indicators$reference_year[-1],
                                       result_indicators$interventions_crop_adjusted[-1] - result_indicators$reference_year[-1],
                                       result_indicators$traditional_agriculture[-1] - result_indicators$reference_year[-1])


diff_indicators_df$scenario <- c(result_indicators$interventions$scenario, 
                                 result_indicators$interventions_animal_adjusted$scenario, 
                                 result_indicators$interventions_crop_adjusted$scenario,
                                 result_indicators$traditional_agriculture$scenario)
diff_indicators_df <- dplyr::relocate(diff_indicators_df, scenario)



#--> list contains model results split by the scenarios

#check the distributions
result_indicators <- do.call(rbind, result_indicators)

#change names of scenarios
result_indicators$scenario <- factor(result_indicators$scenario, levels = c("reference_year","interventions","interventions_animal_adjusted", "interventions_crop_adjusted", "traditional_agriculture"),
                                     labels = c('Ref', 'PS', 'LBS' ,'CBS', "TA"))

diff_indicators_df$scenario <- factor(diff_indicators_df$scenario, levels = c("reference_year","interventions","interventions_animal_adjusted", "interventions_crop_adjusted", "traditional_agriculture"),
                                      labels = c('Ref', 'PS', 'LBS' ,'CBS', "TA"))

#melt combined outputs
results_indicators_long <- reshape2::melt(result_indicators, id.var = 'scenario')
diff_indicators_long <- reshape::melt(diff_indicators_df, id.var = 'scenario')


#object to save vip scores
vip_df <- data.frame()

#save PLS results to large table
#function taken from plot_pls
VIP <- function(object) {
  if (object$method != "oscorespls") 
    stop("Only implemented for orthogonal scores algorithm.  Refit with 'method = \"oscorespls\"'")
  if (nrow(object$Yloadings) > 1) 
    stop("Only implemented for single-response models")
  SS <- c(object$Yloadings)^2 * colSums(object$scores^2)
  Wnorm2 <- colSums(object$loading.weights^2)
  SSW <- sweep(object$loading.weights^2, 2, SS/Wnorm2, 
               "*")
  sqrt(nrow(SSW) * apply(SSW, 1, cumsum)/cumsum(SS))
}

#make plots of PLS / append PLS results to data.frame
for(indicator in unique(results_indicators_long$variable)){
  
  #indicator <- unique(results_indicators_long$variable)[1]
  #pls analysis

  for(scenario in unique(diff_indicators_df$scenario)){
    
    #scenario <- unique(diff_indicators_df$scenario)[1]

    #put only the corresponding scenario values to the simulation output, drop the scenario column
    nitrogen_mc_simulation$y <- diff_indicators_df[diff_indicators_df$scenario == scenario, -1]

    #do pls
    pls_result <- plsr.mcSimulation(
      object = nitrogen_mc_simulation,
      resultName = names(nitrogen_mc_simulation$y[indicator]), ncomp = 1
    )

    pls_plot <- plot_pls(pls_result, input_table = input, threshold = 0.9)

    pic_path <- 'figures/circularity_metrics/pls_difference_baseline/'

    fname <- paste0(indicator,'_', scenario, '_difference_baseline_pls.jpg')
    try(ggsave(pls_plot,  filename = fname, path = pic_path,  device = 'jpeg', width = 10, height = 10, units = 'cm'))
    
    #extract the VIP scores
    vip_res <- VIP(pls_result)
    
    #create data.frame
    vip_df <- rbind(vip_df, data.frame(name = names(vip_res), vip = vip_res, indicator = indicator,
                                       scenario = scenario, row.names = NULL))
  }
}

#drop entries with VIP < 1
vip_df <- filter(vip_df, vip > 1)

#save results
write.csv(vip_df, 'data/vip_results.csv', row.names = F)
write.csv(unique(vip_df$name), 'data/pls_flows_unique.csv', row.names = F)
#hand eddited the group names of all vasriables


vip_df <- read.csv('data/vip_results.csv')
flow_groups <- read.csv('data/pls_flows_unique.csv')

vip_df <- merge.data.frame(vip_df, flow_groups, by.x = 'name', by.y = 'stream')

vip_df$g <- as.factor(as.numeric(as.factor(vip_df$group)))

#split flows into two: one for nutrient and one for flow

vip_df <- vip_df %>% 
  separate(col = indicator, into = c('indicator', 'nutrient'), 
         sep = -1) %>% 
  mutate(indicator = gsub('.{1}$', '', indicator)) %>% 
  mutate(group = paste0(g, ': ', group))


vip_df$group <- factor(vip_df$group, 
                       levels = c('1: animal_number',
                                '2: biogas',
                                '3: consumption',
                                '4: crop_allocation',
                                '5: inorganic_fertilizer',
                                '6: local_feed',
                                '7: manure_allocation',
                                '8: manure_excretion',
                                '9: manure_export',
                                '10: manure_housinglosses',
                                '11: manure_import',
                                '12: slaughtering',
                                '13: wastewater'))

vip_df$nutrient <- factor(vip_df$nutrient, levels = c('N', 'P', 'K'))

vip_df$scenario <- factor(vip_df$scenario, levels = c('PS', 'CBS', 'LBS', 'TA'))

vip_df$g <- as.factor(vip_df$g)

#have the same color code and ticks on the y axis

library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)
brewer.pal(n = 12, name = "Paired")

p1 <- vip_df %>% 
  filter(indicator == 'total_input') %>% 
  ggplot(aes(x=g, y=vip, fill=group)) +
  geom_boxplot() +
  theme_bw() +
    facet_grid(nutrient~scenario) +
  labs(fill='Variable Group') +
  ylab('Variable Importance in Projection (VIP)') +
  scale_fill_manual(breaks = c('1: animal_number',
                               '2: biogas',
                               '3: consumption',
                               '4: crop_allocation',
                               '5: inorganic_fertilizer',
                               '6: local_feed',
                               '7: manure_allocation',
                               '8: manure_excretion',
                               '9: manure_export',
                               '10: manure_housinglosses',
                               '11: manure_import',
                               '12: slaughtering',
                               '13: wastewater'),
                    values = c(brewer.pal(n = 12, name = "Paired"), 'pink'))+
  labs(title = 'Total Input')+
  xlab('')

p2 <- vip_df %>% 
  filter(indicator == 'use_efficiency') %>% 
  ggplot(aes(x=g, y=vip, fill=group)) +
  geom_boxplot() +
  theme_bw() +
  facet_grid(nutrient~scenario) +
  labs(fill='Variable Group') +
  ylab('Variable Importance in Projection (VIP)') +
  scale_fill_manual(breaks = c('1: animal_number',
                               '2: biogas',
                               '3: consumption',
                               '4: crop_allocation',
                               '5: inorganic_fertilizer',
                               '6: local_feed',
                               '7: manure_allocation',
                               '8: manure_excretion',
                               '9: manure_export',
                               '10: manure_housinglosses',
                               '11: manure_import',
                               '12: slaughtering',
                               '13: wastewater'),
                    values = c(brewer.pal(n = 12, name = "Paired"), 'pink'))+
  labs(title = 'Use Efficiency') +
  xlab('')

p3 <- vip_df %>% 
  filter(indicator == 'recycling_rate') %>% 
  ggplot(aes(x=g, y=vip, fill=group)) +
  geom_boxplot() +
  theme_bw() +
  facet_grid(nutrient~scenario) +
  labs(fill='Variable Group') +
  ylab('Variable Importance in Projection (VIP)') +
  scale_fill_manual(breaks = c('1: animal_number',
                               '2: biogas',
                               '3: consumption',
                               '4: crop_allocation',
                               '5: inorganic_fertilizer',
                               '6: local_feed',
                               '7: manure_allocation',
                               '8: manure_excretion',
                               '9: manure_export',
                               '10: manure_housinglosses',
                               '11: manure_import',
                               '12: slaughtering',
                               '13: wastewater'),
                    values = c(brewer.pal(n = 12, name = "Paired"), 'pink'))+
  scale_x_discrete(breaks = as.factor(1:13))+
  labs(title = 'Recycling Rate') +
  xlab('')

p4 <- vip_df %>% 
  filter(indicator == 'share_reuse_to_total_input') %>% 
  ggplot(aes(x=g, y=vip, fill=group)) +
  geom_boxplot() +
  theme_bw() +
  facet_grid(nutrient~scenario) +
  labs(fill='Variable Group') +
  ylab('Variable Importance in Projection (VIP)') +
  scale_fill_manual(breaks = c('1: animal_number',
                               '2: biogas',
                               '3: consumption',
                               '4: crop_allocation',
                               '5: inorganic_fertilizer',
                               '6: local_feed',
                               '7: manure_allocation',
                               '8: manure_excretion',
                               '9: manure_export',
                               '10: manure_housinglosses',
                               '11: manure_import',
                               '12: slaughtering',
                               '13: wastewater'),
                    values = c(brewer.pal(n = 12, name = "Paired"), 'pink'))+
  labs(title = 'Share of reuse to total input') +
  xlab('')



p5 <- vip_df %>% 
  filter(indicator == 'losses') %>% 
  ggplot(aes(x=g, y=vip, fill=group)) +
  geom_boxplot() +
  theme_bw() +
  facet_grid(nutrient~scenario) +
  labs(fill='Variable Group') +
  ylab('Variable Importance in Projection (VIP)') +
  scale_fill_manual(breaks = c('1: animal_number',
                               '2: biogas',
                               '3: consumption',
                               '4: crop_allocation',
                               '5: inorganic_fertilizer',
                               '6: local_feed',
                               '7: manure_allocation',
                               '8: manure_excretion',
                               '9: manure_export',
                               '10: manure_housinglosses',
                               '11: manure_import',
                               '12: slaughtering',
                               '13: wastewater'),
                    values = c(brewer.pal(n = 12, name = "Paired"), 'pink'))+
  labs(title = 'Nutrient losses') +
  xlab('')


ggsave('figures/pls_plot_total_input.jpeg', plot = p1, device = 'jpeg', width = 25, height = 20, units = 'cm')
ggsave('figures/pls_plot_use_efficency.jpeg', plot = p2, device = 'jpeg', width = 25, height = 20, units = 'cm')
ggsave('figures/pls_plot_recycling_rate.jpeg', plot = p3, device = 'jpeg', width = 25, height = 20, units = 'cm')
ggsave('figures/pls_plot_share_reuse.jpeg', plot = p4, device = 'jpeg', width = 25, height = 20, units = 'cm')
ggsave('figures/pls_plot_losses.jpeg', plot = p5, device = 'jpeg', width = 25, height = 20, units = 'cm')

levels(as.factor(vip_df$group))


#append data.frame



