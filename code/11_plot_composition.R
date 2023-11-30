#make boxplot for circularity indicators
library(tidyverse)
library(scales) #to have non-scientific numbers for the x and y axis
library(ggtext)

ha_ag_land <- 73014

#read scenario data
result_flows <- readRDS('data/model_result_flows.rds') %>% 
  do.call(rbind, .) %>% 
  mutate(run = rep(1:10000, 5),
         scenario = factor(scenario, levels = c('reference_year', 
                                                'interventions',
                                                'interventions_crop_adjusted',
                                                'interventions_animal_adjusted'),
                           labels =  c('Ref', 'PS', 'CBS' ,'LBS'))) %>% 
  na.omit() %>% 
  pivot_longer(cols = -c(run, scenario)) %>% 
  tidyr::separate(col = name, sep = -1, convert = TRUE, into = c('variable', 'nutrient')) %>% 
  mutate(variable = substring(variable, 1, nchar(variable)-1)) %>% 
  filter(nutrient == 'N') %>% 
  filter(variable %in% c('grassbased_feed', 
                         'feed_crops',
                         'feed_from_processed_crops',
                         'net_feed_import')) %>% 
  pivot_wider(id_cols = c('run', 'scenario', 'nutrient'), names_from = 'variable') %>% 
  mutate(total_feed = feed_crops + grassbased_feed + feed_from_processed_crops + net_feed_import) %>% 
  pivot_longer(cols = -c(run, scenario, nutrient)) %>% 
  mutate(name = factor(name, 
                           levels = c('feed_from_processed_crops', 'feed_crops', 'grassbased_feed', 'net_feed_import', 'total_feed'),
                           labels = c('Feed from\nprocessed crops', 'Feed crops', 'Grass-based feed', 'Imported feed', 'Total')) )

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


cbp1_extended <- c(cbp1,
                   '#85a244','#9652a5', '#ae523a')

base_size <- 17
width <- 24
height <- 17
ha_ag_land <- 73014


result_flows %>% 
  mutate(value = value / ha_ag_land) %>% 
  filter(name != 'Total') %>% 
  ggplot(aes(x = name, y = value, fill = scenario)) +
  geom_boxplot(outlier.alpha = 0.1) +
  scale_fill_manual(name = "Modelled Scenario", 
                    labels = c("Reference Year 2020", "Participatory Scenario", 
                               "Crop Buffered Scenario", "Livestock Buffered Scenario"),
                    values=cbp1) +
  ylab(bquote('Feed (t N'~ha^-1~year^-1*')')) +
  xlab('')+
  scale_fill_manual(name = "Modelled scenario", 
                    labels = c("Reference Year 2020", "Participatory Scenario", 
                               "Crop Buffered Scenario", "Livestock Buffered Scenario"),
                    values=cbp1) +
  scale_y_continuous(labels = scales::comma)+
  expand_limits(y=0)+
  theme_bw(base_size = base_size) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        axis.text = element_textbox_simple())
ggsave(filename = 'figures/boxplot_feedcomposition_absolute.jpeg', device = 'jpeg', width = width, height = height, units = 'cm')


result_flows %>% 
  pivot_wider(id_cols = c('run', 'scenario', 'nutrient')) %>% 
  pivot_longer(cols = -c(run, scenario, nutrient, Total)) %>% 
  mutate(value = (value / Total)* 100) %>% 
  filter(name != 'Total') %>% 
  ggplot(aes(x = name, y = value, fill = scenario)) +
  geom_boxplot(outlier.alpha = 0.1) +
  scale_fill_manual(name = "Modelled Scenario", 
                    labels = c("Reference Year 2020", "Participatory Scenario", 
                               "Crop Buffered Scenario", "Livestock Buffered Scenario"),
                    values=cbp1) +
  ylab(bquote('Composition of N feed sources (%)')) +
  xlab('')+
  scale_fill_manual(name = "Modelled scenario", 
                    labels = c("Reference Year 2020", "Participatory Scenario", 
                               "Crop Buffered Scenario", "Livestock Buffered Scenario"),
                    values=cbp1) +
  scale_y_continuous(labels = scales::comma)+
  coord_cartesian(ylim = c(0, 100)) +
  theme_bw(base_size = base_size) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        axis.text = element_textbox_simple())
ggsave(filename = 'figures/boxplot_feedcomposition_percent.jpeg', device = 'jpeg', width = width, height = height, units = 'cm')







#read scenario data
result_flows <- readRDS('data/model_result_flows.rds') %>% 
  do.call(rbind, .) %>% 
  mutate(run = rep(1:10000, 5),
         scenario = factor(scenario, levels = c('reference_year', 
                                                'interventions',
                                                'interventions_crop_adjusted',
                                                'interventions_animal_adjusted'),
                           labels =  c('Ref', 'PS', 'CBS' ,'LBS'))) %>% 
  na.omit() %>% 
  pivot_longer(cols = -c(run, scenario)) %>% 
  tidyr::separate(col = name, sep = -1, convert = TRUE, into = c('variable', 'nutrient')) %>% 
  mutate(variable = substring(variable, 1, nchar(variable)-1)) %>% 
  filter(nutrient == 'N') %>% 
  filter(grepl(pattern = 'stakeholder', variable)) 


#select the columns I need







#composition of the indicators
result_flows <- readRDS('data/model_result_flows.rds') %>% 
  do.call(rbind, .) %>% 
  mutate(run = rep(1:10000, 5),
         scenario = factor(scenario, levels = c('reference_year', 
                                                'interventions',
                                                'interventions_crop_adjusted',
                                                'interventions_animal_adjusted'),
                           labels =  c('Ref', 'PS', 'CBS' ,'LBS'))) %>% 
  na.omit() %>% 
  pivot_longer(cols = -c(run, scenario)) %>% 
  tidyr::separate(col = name, sep = -1, convert = TRUE, into = c('variable', 'nutrient')) %>% 
  mutate(variable = substring(variable, 1, nchar(variable)-1)) %>% 
  filter(nutrient == 'N') %>% 
  filter(variable %in% c('grassbased_feed', 
                         'feed_crops',
                         'feed_from_processed_crops',
                         'net_feed_import',
                         
                         'import_OFMSW',
                         'import_dairy_egg',
                         'imported_vegetal_products',
                         'import_meat',
                         'import_animal_products',
                         'import_inorganic_fertilizer',
                         'import_organic_fertilizer',
                         
                         'crop_cultivation_losses',
                         'animal_housing_and_storage_losses',
                         'wastewater_effluent_gaseous_losses')) %>% 
  pivot_wider(id_cols = c('run', 'scenario', 'nutrient'), names_from = 'variable') %>% 
  mutate(total_feed = feed_crops + grassbased_feed + feed_from_processed_crops + net_feed_import) %>% 
  pivot_longer(cols = -c(run, scenario, nutrient)) 


result_flows %>% 
  filter(name == 'import_animal_products') %>% 
  ggplot(aes(x = scenario, y = value)) +
  geom_boxplot()

result_flows %>% 
  filter(name == 'import_meat') %>% 
  ggplot(aes(x = scenario, y = value)) +
  geom_boxplot()

result_flows %>% 
  filter(name == 'import_meat') %>% 
  group_by(scenario) %>% 
  summarise(med = median(value),
            n_zero = (sum(value == 0) / n())* 100)

result_flows %>% 
  filter(name == 'import_dairy_egg') %>% 
  group_by(scenario) %>% 
  summarise(med = median(value),
            n_zero = (sum(value == 0) / n())* 100)

#cases import animal products changed
test <- result_flows %>% 
  filter(name == 'import_animal_products') %>% 
  pivot_wider(names_from = scenario) %>% 
  mutate(eq_all = (LBS == Ref & LBS == PS & LBS == CBS),
         eq_scen = (LBS == PS & LBS == CBS)) 

sum(test$eq_all) / nrow(test)
sum(test$eq_scen) / nrow(test)


result_flows_median <- result_flows %>% 
  group_by(scenario, name) %>% 
  summarise(med_value = median(value) / ha_ag_land) 

feed_streams <- result_flows_median %>% 
  filter(name %in%c ('grassbased_feed', 
                     'feed_crops',
                     'feed_from_processed_crops',
                     'net_feed_import')) %>% 
  arrange(desc(name)) %>% 
  mutate(indicator = 'feed',
         label_ypos = cumsum(med_value))

loss_streams <- result_flows_median %>% 
  filter(name %in%c ('crop_cultivation_losses',
                     'animal_housing_and_storage_losses',
                     'wastewater_effluent_gaseous_losses')) %>% 
  arrange(desc(name)) %>% 
  mutate(indicator = 'losses',
         label_ypos = cumsum(med_value))


#import animal is always zero, so drop it

import_streams <- result_flows_median %>% 
  filter(name %in%c ('import_OFMSW',
                     'import_dairy_egg',
                     'imported_vegetal_products',
                     'import_meat',
                     'import_animal_products',
                     'import_inorganic_fertilizer',
                     'import_organic_fertilizer',
                     'net_feed_import')) %>% 
  filter(!(name %in% c('import_dairy_egg', 'import_meat'))) %>% 
  arrange(desc(name)) %>% 
  mutate(indicator = 'import',
         label_ypos = cumsum(med_value))


# loss_streams %>% 
#   rbind(feed_streams) %>% 
#   rbind(import_streams) %>% 
#   mutate(rounded_value = round(med_value, digits = 1)) %>% 
#   ggplot(aes(x=scenario, y=med_value, fill=name)) +
#   geom_bar(stat="identity")+
#   facet_grid(~indicator)+
#   geom_label(aes(y=label_ypos, label=rounded_value), vjust=1.6, 
#             color="white", size=3.5, position=position_dodge(width=1))+
#   theme_bw()


library(ggrepel)




# loss_streams %>% 
#   rbind(feed_streams) %>% 
#   rbind(import_streams) %>% 
#   mutate(rounded_value = round(med_value, digits = 1),
#          label_name = recode(name, 
#                              feed_crops = 'Feed crops',
#                              grassbased_feed = 'Grass-based feed',
#                              feed_from_processed_crops = 'Feed from processed crops',
#                              net_feed_import = 'Imported feed',
#                              import_OFMSW = 'OFMSW import',
#                              import_dairy_egg = 'Dairy and egg import',
#                              imported_vegetal_products = 'Vegetal products import',
#                              import_meat = 'Meat import',
#                              import_inorganic_fertilizer = 'Inorganic fertilizer import',
#                              import_organic_fertilizer = 'Organic fertilizer import',
#                              crop_cultivation_losses = 'Losses during cultivation',
#                              animal_housing_and_storage_losses = 'Animal housing and storage losses',
#                              wastewater_effluent_gaseous_losses = 'Effluent gaseous losses in wastewater')) %>% 
#   filter(!(name %in% c('import_dairy_egg', 'import_meat'))) %>% 
#   ggplot(aes(x=scenario, y=med_value, fill=label_name)) +
#   geom_bar(stat="identity")+
#   # geom_text_repel(aes(label = str_wrap(paste0(label_name, ': ', rounded_value), 15),
#   #                     y = label_ypos))+
#   # geom_text_repel(aes(label = rounded_value,
#   #                     y = label_ypos))+
#   geom_text(aes(label = rounded_value,
#                       y = label_ypos), nudge_y = -10)+
#   scale_fill_manual(values = cbp1_extended) +
#   facet_grid(~indicator)+
#   theme_bw()+
#   theme(legend.position = "none")


stream_names <- c('Inorganic fertilizer\nimport', 
                  'Organic fertilizer\nimport',
                  'Vegetal product\nimport',  
                  'Animal product\nimport',
                  'OFMSW import\n',
                  'Effluent/gaseous\nlosses during WwT', 
                  'Losses during\ncultivation', 
                  'Animal housing and\nstorage losses',
                  'Grass-based feed\n',
                  'Feed crops\n', 
                  'Feed from\nprocessed crops',
                  'Feed import\n'
                  )



cbp1_extended <- c("#999999", "#E69F00", "#56B4E9", "#c48e42", "#009E73",
                   "#0072B2", "#D55E00", "#CC79A7",
                   '#9652a5', '#85a244', '#ae523a', "#F0E442")

base_size <- 17
width <- 24
height <- 17

input_df <- loss_streams %>% 
  rbind(feed_streams) %>% 
  rbind(import_streams) %>% 
  mutate(rounded_value = round(med_value, digits = 0),
         label_name = recode(name, 
                             feed_crops = 'Feed crops\n',
                             grassbased_feed = 'Grass-based feed\n',
                             feed_from_processed_crops = 'Feed from\nprocessed crops',
                             net_feed_import = 'Feed import\n',
                             import_OFMSW = 'OFMSW import\n',
                             import_animal_products = 'Animal product\nimport',
                             import_dairy_egg = 'Imported dairy and eggs',
                             imported_vegetal_products = 'Vegetal product\nimport',
                             import_meat = 'Imported meat',
                             import_inorganic_fertilizer = 'Inorganic fertilizer\nimport',
                             import_organic_fertilizer = 'Organic fertilizer\nimport',
                             crop_cultivation_losses = 'Losses during\ncultivation',
                             animal_housing_and_storage_losses = 'Animal housing and\nstorage losses',
                             wastewater_effluent_gaseous_losses = 'Effluent/gaseous\nlosses during WwT')) %>% 
  filter(!(name %in% c('import_dairy_egg', 'import_meat'))) %>% 
  mutate(label_name = factor(label_name, levels = rev(stream_names)),
         indicator = factor(indicator, 
                                   levels = c('import', 'losses', 'feed'),
                                   labels = c('Total input', 'Losses', 'Feed')),
         ord = as.numeric(label_name)) %>% 
  arrange(desc(ord)) %>% 
  group_by(scenario, indicator) %>% 
  mutate(upper = cumsum(med_value),
         lower = lag(upper, default = 0),
         label_ypos =  (upper  + lower)/2,
         label_ypos = ifelse(med_value == 0, yes = NA, no = label_ypos)) %>% 
  ungroup()

#change some position values by hand
input_df$label_ypos[input_df$name == 'import_OFMSW'] <- input_df$label_ypos[input_df$name == 'import_OFMSW'] + 6
input_df$label_ypos[input_df$name == 'imported_vegetal_products' & input_df$scenario %in% c('Ref', 'PS', 'LBS')] <- input_df$label_ypos[input_df$name == 'imported_vegetal_products'  & input_df$scenario %in% c('Ref', 'PS', 'LBS')] - 6
input_df$label_ypos[input_df$name == 'imported_vegetal_products' & input_df$scenario %in% c('CBS')] <- input_df$label_ypos[input_df$name == 'imported_vegetal_products'  & input_df$scenario %in% c('CBS')] - 2
input_df$label_ypos[input_df$name == 'feed_from_processed_crops' & input_df$scenario %in% c('PS', 'LBS')] <- input_df$label_ypos[input_df$name == 'feed_from_processed_crops'  & input_df$scenario %in% c('PS', 'LBS')] + 6



p1 <- input_df %>% 
  mutate(scenario = recode(scenario,
                           Ref = "Reference Year 2020",
                           PS = "Participatory Scenario",
                           CBS = "Crop Buffered\nScenario",
                           LBS = "Livestock Buffered\nScenario")) %>% 
  ggplot(aes(x=scenario, y=med_value, fill= label_name)) +
  geom_bar(stat="identity")+
  # geom_text_repel(aes(label = rounded_value,
  #                     y = label_ypos), direction = 'y')+
  geom_text(aes(label = rounded_value,
                y = label_ypos))+
  scale_fill_manual(values = cbp1_extended, 
                    name = 'Nutrient flow', breaks = stream_names) +
  xlab("") +
  ylab(bquote('Modelled median nutrient flow (kg N'~ha^-1~year^-1*')'))+
  facet_grid(~indicator)+
  theme_bw(base_size = base_size) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(p1, filename = 'figures/composition_indicator_and_feed_with_legend.jpeg', device = 'jpeg', width = width, height = height, units = 'cm')

p1 +
  theme(legend.position = 'None')

ggsave(filename = 'figures/composition_indicator_and_feed_no_legend.jpeg', device = 'jpeg', width = width, height = height, units = 'cm')


sum_df <- input_df %>% 
  group_by(indicator, scenario) %>% 
  summarise(sum = sum(med_value) / 1000) 

write.csv(sum_df, file = 'data/indicator_composition_median.csv', row.names = FALSE)




test <- loss_streams %>% 
  rbind(feed_streams) %>% 
  rbind(import_streams) %>% 
  mutate(rounded_value = round(med_value, digits = 1),
         label_name = recode(name, 
                             feed_crops = 'Feed crops',
                             grassbased_feed = 'Grass-based feed',
                             feed_from_processed_crops = 'Feed from\nprocessed crops',
                             net_feed_import = 'Imported feed',
                             import_OFMSW = 'Imported OFMSW',
                             import_dairy_egg = 'Imported dairy and eggs',
                             imported_vegetal_products = 'Imported vegetal\nproducts',
                             import_meat = 'Imported meat',
                             import_inorganic_fertilizer = 'Imported inorganic\nfertilizer',
                             import_organic_fertilizer = 'Imported organic\nfertilizer',
                             crop_cultivation_losses = 'Losses during\ncultivation',
                             animal_housing_and_storage_losses = 'Animal housing and\nstorage losses',
                             wastewater_effluent_gaseous_losses = 'Effluent gaseous\nlosses in wastewater')) %>% 
  filter(!(name %in% c('import_dairy_egg', 'import_meat'))) %>% 
  mutate(label_name = factor(label_name, levels = rev(stream_names)),
         indicator = factor(indicator, 
                            levels = c('import', 'losses', 'feed'),
                            labels = c('Total input', 'Losses', 'Feed')),
         ord = as.numeric(label_name)) %>% 
  arrange(desc(ord)) %>% 
  group_by(scenario, indicator) %>% 
  mutate(upper = cumsum(med_value),
         lower = lag(upper, default = 0),
         label_ypos = (upper  + lower)/2)



test <- loss_streams %>% 
  rbind(feed_streams) %>% 
  rbind(import_streams) %>% 
  mutate(rounded_value = round(med_value, digits = 1),
         label_name = recode(name, 
                             feed_crops = 'Feed crops',
                             grassbased_feed = 'Grass-based feed',
                             feed_from_processed_crops = 'Feed from\nprocessed crops',
                             net_feed_import = 'Imported feed',
                             import_OFMSW = 'Imported OFMSW',
                             import_dairy_egg = 'Imported dairy and eggs',
                             imported_vegetal_products = 'Imported vegetal\nproducts',
                             import_meat = 'Imported meat',
                             import_inorganic_fertilizer = 'Imported inorganic\nfertilizer',
                             import_organic_fertilizer = 'Imported organic\nfertilizer',
                             crop_cultivation_losses = 'Losses during\ncultivation',
                             animal_housing_and_storage_losses = 'Animal housing and\nstorage losses',
                             wastewater_effluent_gaseous_losses = 'Effluent gaseous\nlosses in wastewater')) %>% 
  filter(!(name %in% c('import_dairy_egg', 'import_meat'))) %>% 
  mutate(label_name = factor(label_name, levels = rev(stream_names)),
         indicator = factor(indicator, 
                            levels = c('import', 'losses', 'feed'),
                            labels = c('Total input', 'Losses', 'Feed')),
         ord = as.numeric(label_name))
