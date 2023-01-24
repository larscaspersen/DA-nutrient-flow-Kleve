#do pls
library(tidyverse)
library(decisionSupport)


#read saved runs for flows and for indicators
result_indicators <- readRDS('data/model_result_flows.rds')
nitrogen_mc_simulation <- readRDS('data/model_output_flows.rds')

#read input file
input <- read.csv("data/input_all_uncertainty_classes.csv")
#remove median values
input$median <- NA

diff_indicators_df <- rbind.data.frame(result_indicators$interventions[-1] - result_indicators$reference_year[-1],
                                       result_indicators$interventions_animal_adjusted[-1] - result_indicators$reference_year[-1],
                                       result_indicators$interventions_crop_adjusted[-1] - result_indicators$reference_year[-1])


diff_indicators_df$scenario <- c(result_indicators$interventions$scenario, 
                                 result_indicators$interventions_animal_adjusted$scenario, 
                                 result_indicators$interventions_crop_adjusted$scenario)
diff_indicators_df <- dplyr::relocate(diff_indicators_df, scenario)



#--> list contains model results split by the scenarios

#check the distributions
result_indicators <- do.call(rbind, result_indicators)

#change names of scenarios
result_indicators$scenario <- factor(result_indicators$scenario, levels = c("reference_year","interventions","interventions_animal_adjusted", "interventions_crop_adjusted"),
                                     labels = c('Ref', 'PS', 'LBS' ,'CBS'))

diff_indicators_df$scenario <- factor(diff_indicators_df$scenario, levels = c("reference_year","interventions","interventions_animal_adjusted", "interventions_crop_adjusted"),
                                      labels = c('Ref', 'PS', 'LBS' ,'CBS'))

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

indicators <- c('total_input', 'use_efficiency', 'share_reuse_to_total_input', 'recycling_rate', 'losses')


results_indicators_long <- results_indicators_long %>% 
  filter(variable %in% c(paste(indicators,'N', sep = '_'),
                         paste(indicators, 'P', sep = '_'),
                         paste(indicators, 'K', sep = '_')))

results_indicators_long$variable <- factor(results_indicators_long$variable, 
                                           levels = c(paste(indicators,'N', sep = '_'),
                                                      paste(indicators, 'P', sep = '_'),
                                                      paste(indicators, 'K', sep = '_')))

diff_indicators_long <- diff_indicators_long %>% 
  filter(variable %in% c(paste(indicators,'N', sep = '_'),
                                     paste(indicators, 'P', sep = '_'),
                                     paste(indicators, 'K', sep = '_')))
diff_indicators_long$variable <- factor(diff_indicators_long$variable, 
                                           levels = c(paste(indicators,'N', sep = '_'),
                                                      paste(indicators, 'P', sep = '_'),
                                                      paste(indicators, 'K', sep = '_')))

#add column saying run number
diff_indicators_long$run <- rep(1:10000, 45)

diff_indicators_df <- reshape2::dcast(data = diff_indicators_long, formula = scenario + run ~ variable, value.var = 'value')
diff_indicators_df <- diff_indicators_df[,-2]

vip_df <- data.frame()
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
vip_df <- filter(vip_df, vip > 2)

#save results
write.csv(vip_df, 'data/vip_results.csv', row.names = F)
write.csv(unique(vip_df$name), 'data/pls_flows_unique.csv', row.names = F)
#hand eddited the group names of all vasriables

vip_summarised <- vip_df %>% 
  group_by(name) %>% 
  summarise(median_vip = median(vip),
            n = n())


vip_df <- read.csv('data/vip_results.csv')
flow_groups <- read.csv('data/flow_groups.csv')

unique(flow_groups$group)


#remove the stuff belonging to ta
vip_df <- filter(vip_df, scenario != 'TA')

#filter out flows from flow groups


vip_df <- merge.data.frame(vip_df, flow_groups, by.x = 'name', by.y = 'flow', all.x = TRUE)

vip_df$group <- factor(vip_df$group, 
                       levels = c('lever_1', 'lever_2', 'lever_3', 'lever_4',
                                  'livestock_population',
                                  'manure_production', 'animal_housinglosses', 'import_organic_matter',
                                  'export_organic_matter', 'grass_maize_production',
                                  'inorganic_fertilizer', 'biogas',
                                  'slaughtering'),
                       labels = c('Lever 1', 'Lever 2', 'Lever 3', 'Lever 4', 
                                  'Initial Livestock Population',
                                  'Manure Production', 'Manure Storage Losses', 'Import organic matter',
                                  'Export Organic Matter', 'Local Feed Production', 'Import Inorganic Fertilizer',
                                  'Biogas', 'Slaughtering'))







vip_df$g <- as.factor(as.numeric(as.factor(vip_df$group)))

#split flows into two: one for nutrient and one for flow

vip_df <- vip_df %>% 
  separate(col = indicator, into = c('indicator', 'nutrient'), 
         sep = -1) %>% 
  mutate(indicator = gsub('.{1}$', '', indicator)) %>% 
  mutate(group = paste0(g, ': ', group))


vip_df$nutrient <- factor(vip_df$nutrient, levels = c('N', 'P', 'K'))

vip_df$scenario <- factor(vip_df$scenario, levels = c('PS', 'CBS', 'LBS'))

vip_df$g <- as.factor(vip_df$g)
vip_df$group <- as.factor(vip_df$group)

n_group <- length(levels(vip_df$group))


#maybe add empty labels for the data.frame to force each tick to be shown
test <-  data.frame(name = NA, 
           vip = NA, 
           indicator = rep(unique(vip_df$indicator), each = n_group * 3 * 3), 
           nutrient = rep(c('N', 'P', 'K'),each = 5 * n_group * 3),
           scenario = rep(unique(vip_df$scenario), each = 5 * n_group * 3) ,
           group = rep(unique(vip_df$group), 5 * 3 * 3),
           g = rep(unique(vip_df$g), 5*3 * 3))
vip_df <- rbind(vip_df, test)

#have the same color code and ticks on the y axis

library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)
#brewer.pal(n = 12, name = "Paired")

groups <- c('1: Lever 1', 
            '2: Lever 2', 
            '3: Lever 3', 
            '4: Lever 4',
            '5: Initial Livestock Population',
            '6: Manure Production', 
            '7: Manure Storage Losses', 
            '8: Import organic matter',
            '9: Export Organic Matter', 
            '10: Local Feed Production', 
            '11: Import Inorganic Fertilizer',
            '12: Biogas', 
            '13: Slaughtering')

p1 <- vip_df %>% 
  filter(indicator == 'total_input') %>% 
  ggplot(aes(x=g, y=vip, fill=group)) +
  geom_boxplot() +
  theme_bw() +
    facet_grid(nutrient~scenario) +
  labs(fill='Variable Group') +
  ylab('Variable Importance in Projection (VIP)') +
  scale_fill_manual(breaks = groups,
                    values = c(brewer.pal(n = 12, name = "Paired"), 'pink', 'grey'), drop = FALSE)+
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
  scale_fill_manual(breaks =groups,
                    values = c(brewer.pal(n = 12, name = "Paired"), 'pink', 'grey'), drop = FALSE)+
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
  scale_fill_manual(breaks =groups,
                    values = c(brewer.pal(n = 12, name = "Paired"), 'pink', 'grey'), drop = FALSE)+
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
  scale_fill_manual(breaks =groups,
                    values = c(brewer.pal(n = 12, name = "Paired"), 'pink', 'grey'), drop = FALSE)+
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
  scale_fill_manual(breaks =groups,
                    values = c(brewer.pal(n = 12, name = "Paired"), 'pink', 'grey'), drop = FALSE)+
  labs(title = 'Nutrient losses') +
  xlab('')


ggsave('figures/pls_plot_total_input.jpeg', plot = p1, device = 'jpeg', width = 25, height = 20, units = 'cm')
ggsave('figures/pls_plot_use_efficency.jpeg', plot = p2, device = 'jpeg', width = 25, height = 20, units = 'cm')
ggsave('figures/pls_plot_recycling_rate.jpeg', plot = p3, device = 'jpeg', width = 25, height = 20, units = 'cm')
ggsave('figures/pls_plot_share_reuse.jpeg', plot = p4, device = 'jpeg', width = 25, height = 20, units = 'cm')
ggsave('figures/pls_plot_losses.jpeg', plot = p5, device = 'jpeg', width = 25, height = 20, units = 'cm')

levels(as.factor(vip_df$group))


#append data.frame



