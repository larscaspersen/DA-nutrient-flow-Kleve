#extract information like median, 75% and 25% 
#also extract information like % positive

library(tidyverse)
library(ggridges)
library(scales) #to have non-scientific numbers for the x and y axis

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

#remove the import egg and dairy and import meat
flow_sum <- flow_sum %>% 
  filter(!variable %in% c('import_dairy_egg', 'import_meat', 'animal_balance'))


#write.csv(unique(flow_sum$variable), file = 'flow_names.csv')
#modify

senky_df <- read.csv('flow_names.csv')

senky_df <- merge(senky_df, flow_sum, by = 'variable', all.x = TRUE)

library(networkD3)


links_df_1 <- senky_df %>% 
  mutate(value = median) %>% 
  filter(scenario == 'Ref', nutrient == 'N')


# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links_df_1$source), 
         as.character(links_df_1$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links_df_1$IDsource <- match(links_df_1$source, nodes$name)-1 
links_df_1$IDtarget <- match(links_df_1$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links_df_1, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)
p
