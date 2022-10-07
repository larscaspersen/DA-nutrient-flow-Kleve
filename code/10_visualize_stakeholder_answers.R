answers <- read.csv('data/stakeholder_answers.csv')

library(ggplot2)

#data of reference year
old_df <- answers[1:11,]
old_df$stakeholder <- 'reference_year'
old_df$median <- c(24, 67, 9, 45, 63, 28, 4, 4, 53, 10, 36)


ggplot(answers, aes(x = var, y = median, col = stakeholder)) + 
  geom_point(position=position_dodge(width=0.5), size = 1.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(0.5))+
  geom_segment(data = old_df, aes(
    x = c(1:3,1 , 1,3,2,4, 3,1,2) - .45,
    xend = c(1:3,1, 1,3,2,4, 3,1,2) + .45,
    y = median,
    yend = median,
    linetype = 'Reference Year'), col = 'black')+
  facet_wrap(~category, scales = 'free') +
  scale_linetype_manual("", values = 2) +
  ylab('Stakeholder answers (%)') +
  xlab('') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


ggsave(filename = 'figures/stakeholder_answers.jpeg', device = 'jpeg', width = 20, height = 15, units = 'cm')
