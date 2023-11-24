#setwd('../DA-job/DA-nutrient-flow-Kleve/')
library(tidyverse)
library(ggridges)
library(scales) #to have non-scientific numbers for the x and y axis


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



#calculate change relative to the reference year
rel_change_flows <- rbind.data.frame(((result_flows$interventions[-1] - result_flows$reference_year[-1]) / result_flows$reference_year[-1]) * 100,
                                  ((result_flows$interventions_animal_adjusted[-1] - result_flows$reference_year[-1]) / result_flows$reference_year[-1])*100,
                                  ((result_flows$interventions_crop_adjusted[-1] - result_flows$reference_year[-1]) / result_flows$reference_year[-1]) * 100)
rel_change_flows$scenario <- c(result_flows$interventions$scenario, 
                               result_flows$interventions_animal_adjusted$scenario, 
                               result_flows$interventions_crop_adjusted$scenario)
rel_change_flows <- dplyr::relocate(rel_change_flows, scenario)

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
                         'animal_housing_and_storage_losses', 'animal_balance',
                         
                         'import_organic_fertilizer',
                         'feed_crops',
                         'grassbased_feed',
                         'digestate'
                         ))


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
                  'animal_balance',
                  
                  'import_organic_fertilizer',
                  'feed_crops',
                  'grassbased_feed',
                  'digestate'), 
       
       labels = c('Manure to crops',
                  'Manure export',
                  'Manure biogas substrate',
                  'Inorganic fertilizer import',
                  'Vegetal biogas substrate',
                  'Feed from processed crops',
                  'Animal products import',
                  'Animal products export',
                  'Net food import',
                  'Cultivation losses',
                  'Animal housing and storage losses',
                  'Stock balance animal subsystem',
                  
                  'Organic fertilizer import',
                  'Feed crops',
                  'Grass-based feed',
                  'Digestate'
                  ))

factor(rel_summarised_flows$variable, levels = c('Manure to crops',
                                             'Manure export',
                                             'Manure biogas substrate',
                                             'Inorganic fertilizer import',
                                             'Organic fertilizer import',
                                             'Vegetal biogas substrate',
                                             'Digestate',
                                             'Feed from processed crops',
                                             'Feed crops',
                                             'Grass-based feed',
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
                         'animal_housing_and_storage_losses', 'animal_balance',
                         
                         'import_organic_fertilizer',
                         'feed_crops',
                         'grassbased_feed',
                         'digestate'))


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
                                                   'animal_balance',
                                                   
                                                   'import_organic_fertilizer',
                                                   'feed_crops',
                                                   'grassbased_feed',
                                                   'digestate'), 
                                        
                                        labels = c('Manure to crops',
                                                   'Manure export',
                                                   'Manure biogas substrate',
                                                   'Inorganic fertilizer import',
                                                   'Vegetal biogas substrate',
                                                   'Feed from processed crops',
                                                   'Animal products import',
                                                   'Animal products export',
                                                   'Net food import',
                                                   'Cultivation losses',
                                                   'Animal housing and storage losses',
                                                   'Stock balance animal subsystem',
                                                   
                                                   'Organic fertilizer import',
                                                   'Feed crops',
                                                   'Grass-based feed',
                                                   'Digestate'
                                        ))

factor(summarised_flows_reference$variable, levels = c('Manure to crops',
                                                       'Manure export',
                                                       'Manure biogas substrate',
                                                       'Inorganic fertilizer import',
                                                       'Organic fertilizer import',
                                                       'Vegetal biogas substrate',
                                                       'Digestate',
                                                       'Feed from processed crops',
                                                       'Feed crops',
                                                       'Grass-based feed',
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
                            'Inorganic fertilizer import',
                            'Organic fertilizer import',
                            'Vegetal biogas substrate',
                            'Digestate',
                            'Feed from processed crops',
                            'Feed crops',
                            'Grass-based feed',
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
                                'Inorganic fertilizer import',
                                'Organic fertilizer import',
                                'Vegetal biogas substrate',
                                'Digestate',
                                'Feed from processed crops',
                                'Feed crops',
                                'Grass-based feed',
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










#composition of feed 














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
