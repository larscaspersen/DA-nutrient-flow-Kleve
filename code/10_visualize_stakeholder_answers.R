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


ggsave(filename = 'figures/stakeholder_answers.jpeg', device = 'jpeg', width = 20, height = 15, units = 'cm')




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


ggsave(filename = 'figures/stakeholder_answers_V2.jpeg', device = 'jpeg', width = 20, height = 15, units = 'cm')




















#play wiht skewed distributions


library(rootSolve)
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




res <- optim(par = c(12, 1), fn = model, parms = c(61, 38.8, 79.6), method = "Nelder-Mead", 
      lower = c(0.01, -1000), upper = c(1000,1000))

xi <- (77.5 - res$par[1]*res$par[2]*sqrt(2/(pi*(1+res$par[2]^2))))

x <- seq(30, 120, by=0.2)
plot(sn::dsn(x, omega = res$par[1], alpha = res$par[2], xi = xi), x = x)


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