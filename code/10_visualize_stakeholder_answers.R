answers <- read.csv('data/stakeholder_answers.csv')

library(ggplot2)
library(tidyverse)

#data of reference year
old_df <- answers[1:11,]
old_df$stakeholder <- c(rep('Reference Year',3), 'Model Output', rep('Reference Year', 7))
old_df$median <- c(24, 67, 9, 45, 63, 28, 4, 4, 53, 10, 36)

#set uncertainty level of estimation
uncertainty_level <- 0.2

#summarise stakeholder inpu median
answers_summarised <- answers %>% 
  group_by(var, category) %>% 
  summarise(med_answer = median(median)) %>% 
  mutate(upper = med_answer * (1 + uncertainty_level),
         lower = med_answer * (1 - uncertainty_level))

answers_summarised$xmin <- c(1,1,1,2,3,1,2,3,2,3,4) - 0.45
answers_summarised$xmax <- c(1,1,1,2,3,1,2,3,2,3,4) + 0.45

answers$stakeholder_group <- answers$group
answers$middle <- answers$median
answers_summarised$middle <- answers_summarised$med_answer


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
    y = median,
    yend = median,
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




#summarise stakeholder inpu median
answers_summarised <- answers %>% 
  group_by(var, category) %>% 
  summarise(med_answer = median(median),
            sd_answer = sd(median)) %>% 
  mutate(upper = med_answer + sd_answer,
         lower = med_answer - sd_answer)

answers_summarised$xmin <- c(1,1,1,2,3,1,2,3,2,3,4) - 0.45
answers_summarised$xmax <- c(1,1,1,2,3,1,2,3,2,3,4) + 0.45

answers$stakeholder_group <- answers$group
answers$middle <- answers$median
answers_summarised$middle <- answers_summarised$med_answer


ggplot(answers, aes(x = var, y = middle)) + 
  facet_wrap(~category, scales = 'free') +
  geom_point(size = 0.001, col = 'white')+
  geom_rect(data = answers_summarised, aes(xmin = xmin, xmax = xmax, ymin = middle - sd_answer, 
                                           ymax = middle + sd_answer, group = var, 
                                           fill = 'Uncertainty level median')) +
  geom_point(position=position_dodge(width=0.5), size = 1.5, aes(col = stakeholder_group)) +
  geom_errorbar(aes(ymin=lower, ymax=upper, col = stakeholder_group), width=.2,
                position=position_dodge(0.5))+
  geom_segment(data = old_df, aes(
    x = c(1:3,1 , 1,3,2,4, 3,1,2) - .45,
    xend = c(1:3,1, 1,3,2,4, 3,1,2) + .45,
    y = median,
    yend = median,
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


#ggsave(filename = 'figures/stakeholder_answers_V2.jpeg', device = 'jpeg', width = 20, height = 15, units = 'cm')

nsim <- 10000



answers %>% 
  filter(var == 'crop_feed') %>%
  summarise(lower = quantile(median, probs = 0.05),
            upper = quantile(median, probs = 0.95))

current_way <-   decisionSupport::rmvnorm90ci_exact(n = nsim, lower = 0.236, upper = 0.79,correlationMatrix = 1)

answers_summarised

#using median and plus minus sd
sd_way <- decisionSupport::rmvnorm90ci_exact(n = nsim, lower = 0.53, upper = 1.0 ,correlationMatrix = 1)


#chance event, ranges are max and min of the two groups, weight is the share to total answers
answers %>% 
  filter(var == 'crop_feed')

g1 <- decisionSupport::rmvnorm90ci_exact(n = nsim, lower = 0.775, upper = 0.79 ,correlationMatrix = 1)
g2 <- decisionSupport::rmvnorm90ci_exact(n = nsim, lower = 0.14, upper = 0.575 ,correlationMatrix = 1)
chance_way <- c()

for(i in 1:nsim){
  chance_way[i] <- decisionSupport::chance_event(5/9, value_if = g1[i], value_if_not = g2[i])
}

#skewed distribution

library(sn)
model <- function(x, parms) {
  omega = x[1]
  alpha = x[2]
  m = parms[1]
  p05 = parms[2]
  p95 = parms[3]
  xi = m - omega*alpha*sqrt(2/(pi*(1+alpha^2)))
  
  return ((p05 - qsn(0.05, xi, omega, alpha))^2 +
          (p95 - qsn(0.95, xi, omega, alpha))^2)
  
  
}

quantile(answers$median[answers$var == 'crop_feed'], c(0.05, 0.5, 0.95))
mean(answers$median[answers$var == 'crop_feed'])


res <- optim(par = c(12, 1), fn = model, parms = c(60.6, 23.6, 79.0), method = "Nelder-Mead", 
      lower = c(0.01, -1000), upper = c(1000,1000))

xi <- (60.6 - res$par[1]*res$par[2]*sqrt(2/(pi*(1+res$par[2]^2))))

x <- seq(30, 120, by=0.2)
plot(sn::dsn(x, omega = res$par[1], alpha = res$par[2], xi = xi), x = x)

skewed_way <- sn::rsn(n = nsim, xi = xi, omega = res$par[1], alpha = res$par[2])



#bind everything to one dataframe
dist_df <-  rbind(data.frame(value = chance_way, type = 'chance_event'),
data.frame(value = skewed_way / 100, type = 'skewed_distribution'),
data.frame(value = current_way, type = 'answers_quantiles'),
data.frame(value = sd_way, type = 'median_answer_and_sd'))

library(ggplot2)

ggplot(dist_df, aes(x = value)) + 
  geom_histogram(bins = 150) + 
  geom_vline(xintercept = answers$median[answers$var == 'crop_feed'] / 100, col = 'red')+
  facet_wrap(~type, scales = 'free_y', ncol = 1, nrow = 4)

ggsave(filename = 'figures/translate_stakeholder_answers_crop_feed.jpeg', device = 'jpeg', 
       width = 20, height = 15, units = 'cm')


#indicate answers with vertical lines


hist(answers$median[answers$var == 'crop_feed'], breaks = 9)
med <- median(answers$median[answers$var == 'crop_feed'])

sd(answers$median[answers$var == 'crop_feed'])





quantiles_org=c(38.8,77.5,79.6)
log(quantiles)

percentiles<-c(0.05,0.5, 0.95)
quantiles= c(3.015535, 3.113515, 4.114147)
plot(sn::dsn(seq(-3, 3, by=0.1), alpha=3, xi = 2), x = seq(-3, 3, by=0.1))



hist()

hist(r<-rdistq_fit(distribution="lnorm", n=10000,percentiles = percentiles, quantiles=quantiles),breaks=100)
print(quantile(x=r, probs=percentiles))