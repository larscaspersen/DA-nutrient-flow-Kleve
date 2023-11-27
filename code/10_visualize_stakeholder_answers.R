answers <- read.csv('data/stakeholder_answers.csv')

library(ggplot2)
library(tidyverse)


#--------------------#
#Dotplot stakeholder answers
#---------------------#


#data of reference year
old_df <- answers[1:11,]
old_df$stakeholder <- c(rep('Reference Year',3), 'Model Output', rep('Reference Year', 7))
old_df$middle <- c(24, 67, 9, 45, 63, 28, 4, 4, 53, 10, 36)

#set uncertainty level of estimation
uncertainty_level <- 0.2

#summarise stakeholder inpu median
answers_summarised <- answers %>% 
  group_by(var, category) %>% 
  summarise(middle = median(middle)) %>% 
  mutate(upper = middle * (1 + uncertainty_level),
         lower = middle * (1 - uncertainty_level))

answers_summarised$xmin <- c(1,1,1,2,3,1,2,3,2,3,4) - 0.45
answers_summarised$xmax <- c(1,1,1,2,3,1,2,3,2,3,4) + 0.45



ggplot(answers, aes(x = var, y = middle)) + 
  facet_wrap(~category, scales = 'free') +
  geom_point(size = 0.001, col = 'white')+
  geom_rect(data = answers_summarised, aes(xmin = xmin, xmax = xmax, ymin = middle - (1-uncertainty_level), 
                                           ymax = middle + (1+uncertainty_level), group = var, 
                                           fill = 'Uncertainty level median')) +
  geom_point(position=position_dodge(width=0.5), size = 1.5, aes(col = stakeholder_group)) +
  geom_errorbar(aes(ymin=lower, ymax=upper, col = stakeholder_group), width=.2,
                position=position_dodge(0.5))+
  geom_segment(data = old_df, aes(
    x = c(1:3,1 , 1,3,2,4, 3,1,2) - .45,
    xend = c(1:3,1, 1,3,2,4, 3,1,2) + .45,
    y = middle,
    yend = middle,
    linetype = stakeholder), col = 'black')+
  geom_segment(data = answers_summarised, aes(
    x = xmin,
    xend = xmax,
    y = middle,
    yend = middle,
    linetype = 'Median of answers'))+
  scale_linetype_manual("", values = c(1,3,2)) +
  scale_fill_manual("", values = c('lightblue')) +
  ylab('Stakeholder answers (%)') +
  xlab('') +
  theme_bw() +
  labs(colour='Stakeholder Group')+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


#ggsave(filename = 'figures/stakeholder_answers.jpeg', device = 'jpeg', width = 20, height = 15, units = 'cm')


answers <- read.csv('data/stakeholder_answers.csv')


answers$stakeholder_group <- factor(answers$stakeholder_group,
                                    levels = c('farming',
                                               'food processing',
                                               'nature', 
                                               'science', 'waste'),
                                    labels = c('Farming',
                                               'Food processing',
                                      'Nature conservation',
                                               'Research',
                                      'Waste management'
                                               ))

answers$var <- factor(answers$var, 
                      levels = c('crop_biogas',
                                  'crop_feed',
                                  'crop_food',
                                  'animal_reduction',
                                  'cattle',
                                  'pig',
                                  'poultry',
                                  'other',
                                  'manure_biogas',
                                  'manure_crop',
                                 'manure_export'),
                      labels = c('Biogas\nsubstrate',
                                 'Feed',
                                 'Food',
                                 ' ',
                                 'Cattle',
                                 'Pigs',
                                 'Poultry',
                                 'Others',
                                 'Biogas\nsubstrate',
                                 'Local\napplication',
                                 'Export'))

answers$category <- factor(answers$category,
                           levels = c('crop',
                                      'livestock_compostion',
                                      'livestock_reduction',
                                      'manure'),
                           labels = c('Crop allocation',
                                      'Livestock composition',
                                      'Livestock reduction',
                                      'Manure allocation'))

unique(answers$category)

unique(answers$var)

library(ggtext)

base_size <- 17
width <- 24
height <- 17

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#CC79A7")

#make an invisible point at 000000 for all answers

ggplot(answers, aes(x = var, y = middle)) + 
  geom_point(size = 0.001, col = 'grey94', aes(y = 0)) +
  geom_point(size = 0.001, col = 'grey94', aes(y = 100)) +
  facet_wrap(~category, scales = 'free') +
  geom_point(size = 0.001, col = 'grey94')+
  geom_point(position=position_dodge(width=0.5), size = 2, aes(col = stakeholder_group)) +
  geom_errorbar(aes(ymin=lower, ymax=upper, col = stakeholder_group), width=.2,
                position=position_dodge(0.5))+
  ylab('Stakeholder estimates for the aspects, given in %') +
  xlab('') +
  scale_color_manual(values=cbp1) +
  labs(colour='Stakeholder groups')+
  theme_bw(base_size = base_size) +
  # theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
  #       axis.text = element_textbox_simple())
  theme(axis.text = element_textbox_simple())
ggsave(filename = 'figures/stakeholder_answers.jpg', device = 'jpeg', width = width, height = height, units = 'cm')
