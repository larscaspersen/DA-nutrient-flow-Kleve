library(tidyverse)
library(ggridges)
library(scales) #to have non-scientific numbers for the x and y axis

#read saved runs for flows and for indicators
result_flows <- readRDS('data/model_result_flows.rds')

#calculate crop stock, consumption stock, food processing stock, waste stock
result_flows <- do.call(rbind, result_flows)

result_flows$scenario <- factor(result_flows$scenario, levels = c("reference_year","interventions","interventions_animal_adjusted", "interventions_crop_adjusted"),
                                labels = c('Ref', 'PS', 'LBS' ,'CBS'))

#run is needed to make the dcast later on work
result_flows$run <- rep(1:10000, 5)

#melt dataframe
result_flows_long <- reshape2::melt(result_flows, id.var = c('scenario', 'run'))

#remove scenarios which were not listed when creating the factor
result_flows_long <- na.omit(result_flows_long)

#split variable into flow and nutrient
result_flows_long <- tidyr::separate(data = result_flows_long, col = variable, sep = -1, convert = TRUE, into = c('variable', 'nutrient'))

#remove trailing _
result_flows_long <- result_flows_long %>% 
  mutate(variable = substring(variable, 1, nchar(variable)-1))

#bring back to "normal" format
result_flows <- reshape2::dcast(data = result_flows_long, scenario + nutrient + run ~ variable, value.var = 'value')

#calculate stock of crop, waste, processing, consumption

result_flows_long <- result_flows %>% 
  mutate(crop_in = import_inorganic_fertilizer + import_organic_fertilizer +
           fresh_compost_crop + sewage_to_crop + digestate + manure_to_crop,
         crop_out = vegetal_biogas_substrate + crop_cultivation_losses + 
           other_organic_fertilizer_export + straw + feed_crops + grassbased_feed +
           fruit_and_vegetable + food_and_feed_crops,
         waste_in = vegetal_biogas_substrate + manure_as_biogas_substrate +
           sewage + ofmsw + ofmsw_residual_waste + import_OFMSW,
         waste_out = digestate + fresh_compost_export + fresh_compost_crop +
           sewage_to_crop + sewage_sludge_export + wastewater_effluent_gaseous_losses +
           compost_to_consumption,
         consumption_in = compost_to_consumption + local_animal_products_consumed +
           local_vegetal_products_consumed + import_dairy_egg + import_meat + imported_vegetal_products,
         consumption_out = sewage + ofmsw + ofmsw_residual_waste + wastewater_direct_discharge,
         processing_in = net_feed_import + net_food_import + food_and_feed_crops + 
           fruit_and_vegetable + slaughter_animal + egg_and_dairy,
         processing_out = local_animal_products_consumed + local_animal_products_consumed + 
           imported_vegetal_products + import_meat + import_dairy_egg + feed_from_processed_crops + 
           import_processed_feed + export_meat + export_vegetable + export_dairy_egg + 
           slaughter_waste) %>% 
  mutate(crop_stock = crop_in - crop_out,
         processing_stock = processing_in - processing_out,
         consumption_stock = consumption_in - consumption_out,
         waste_stock = waste_in - waste_out) %>% 
         reshape2::melt(id.vars = c('run', 'nutrient', 'scenario'))

result_flows_long %>% 
  filter(variable == 'crop_stock') %>% 
  ggplot(aes(x=as.numeric(value) ,y = scenario, fill = scenario)) +
  geom_density_ridges_gradient(scale=2) +
  xlab(paste0('Crop balance [t per year]'))+
  ylab('')+
  theme_bw() +
  scale_x_continuous(labels = label_comma())+
  theme(legend.position = "none") + 
  facet_wrap(~nutrient, scales = 'free_x')

result_flows_long %>% 
  filter(variable == 'processing_stock') %>% 
  ggplot(aes(x=as.numeric(value) ,y = scenario, fill = scenario)) +
  geom_density_ridges_gradient(scale=2) +
  xlab(paste0('Crop balance [t per year]'))+
  ylab('')+
  theme_bw() +
  scale_x_continuous(labels = label_comma())+
  theme(legend.position = "none") + 
  facet_wrap(~nutrient, scales = 'free_x')

#crop and processing look weird. crop has some pretty extreme outliers (probably because I prevented negative crop cultivation losses)
#but processing is supposed to be balanced but it isn't

result_flows_long %>% 
  filter(variable == 'waste_stock') %>% 
  ggplot(aes(x=as.numeric(value) ,y = scenario, fill = scenario)) +
  geom_density_ridges_gradient(scale=2) +
  xlab(paste0('Crop balance [t per year]'))+
  ylab('')+
  theme_bw() +
  scale_x_continuous(labels = label_comma())+
  theme(legend.position = "none") + 
  facet_wrap(~nutrient, scales = 'free_x')

result_flows_long %>% 
  filter(variable == 'consumption_stock') %>% 
  ggplot(aes(x=as.numeric(value) ,y = scenario, fill = scenario)) +
  geom_density_ridges_gradient(scale=2) +
  xlab(paste0('Crop balance [t per year]'))+
  ylab('')+
  theme_bw() +
  scale_x_continuous(labels = label_comma())+
  theme(legend.position = "none") + 
  facet_wrap(~nutrient, scales = 'free_x')

result_flows_long %>% 
  filter(variable %in% c('net_food_import', 
                         'food_and_feed_crops', 
                         'fruit_and_vegetable',  
                         'slaughter_animal', 
                         'egg_and_dairy')) %>% 
  filter(scenario == 'Ref') %>% 
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot() + 
  scale_y_continuous(labels = label_comma())+
  coord_flip()+
  facet_wrap(~nutrient, scales = 'free_x')

#feed import looks okay
#'net_feed_import', 
#'
#'
#'


result_flows_long %>% 
  filter(variable %in% c('local_animal_products_consumed',
                         'local_animal_products_consumed', 
                         'imported_vegetal_products',
                         'import_meat',
                         'import_dairy_egg',
                         'feed_from_processed_crops',
                         'export_meat',
                         'export_vegetable',
                         'export_dairy_egg',
                         'slaughter_waste')) %>% 
  filter(scenario == 'Ref') %>% 
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot() + 
  scale_y_continuous(labels = label_comma())+
  coord_flip()+
  facet_wrap(~nutrient, scales = 'free_x')

result_flows %>% 
  filter(scenario == 'Ref') %>% 
  ggplot(aes(x = scenario, y = local_vegetal_products_consumed + imported_vegetal_products)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = label_comma())+
  coord_flip()+
  facet_grid(~nutrient) 
