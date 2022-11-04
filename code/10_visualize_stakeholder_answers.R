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




#summarise stakeholder inpu median
answers_summarised <- answers %>% 
  group_by(var, category) %>% 
  summarise(sd_answer = sd(middle),
            middle = median(middle)) %>% 
  mutate(upper = middle + sd_answer,
         lower = middle - sd_answer)

answers_summarised$xmin <- c(1,1,1,2,3,1,2,3,2,3,4) - 0.45
answers_summarised$xmax <- c(1,1,1,2,3,1,2,3,2,3,4) + 0.45


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


#ggsave(filename = 'figures/stakeholder_answers_V2.jpeg', device = 'jpeg', width = 20, height = 15, units = 'cm')


#----------------------------#
#fit skewed distributions
#----------------------------#

library(sn) #package to handle skewed distributions within R

#this approach was adopted from a post on stack exchange, but I can't find the post anymore
#the idea is to find a skewed distribution, which describes the given quantiles and mean the best
#the function returns the residual sum of squares for the observed quantiles and calculated quantiles for
#the fitted distribution
#the goal is to find the parameters xi, omega and alpha, which describe a skewed normal distribution
model <- function(x, parms) {
  omega = x[1]
  alpha = x[2]
  m = parms[1]
  p05 = parms[2]
  p95 = parms[3]
  xi = m - omega*alpha*sqrt(2/(pi*(1+alpha^2)))
  
  return ((p05 - qsn(0.05, xi, omega, alpha, solver = "RFB"))^2 +
            (p95 - qsn(0.95, xi, omega, alpha, solver = "RFB"))^2)
  
  
}

#we take the quantiles and mean from the answer we are interested in
q05 <- quantile(answers$middle[answers$var == 'crop_feed'], 0.05)
q95 <- quantile(answers$middle[answers$var == 'crop_feed'], 0.95)
m <- mean(answers$middle[answers$var == 'crop_feed'])

#we use an aoptimzation function to find the set of parameters which give the lowest
#error. We also need to provide lower and upper bounds (which I chose more or less randomly)
#and we need to define a search method (I also chose something random, to be honest)
res <- optim(par = c(12, 1), fn = model, parms = c(m, q05, q95), method = "Nelder-Mead", 
             lower = c(0.01, -1000), upper = c(1000,1000))

#we calculate the parameter xi for the results
xi <- (m - res$par[1]*res$par[2]*sqrt(2/(pi*(1+res$par[2]^2))))


#here is a plot of the fitted distribution
x <- seq(0, 120, by=0.2)

#plot distribution
#plot(sn::dsn(x, omega = res$par[1], alpha = res$par[2], xi = xi), x = x)


#function to get parameters of skewed distribution based on mean, 5% quantile and 95% quantile
get_skewed_dist_parameter <- function(x){
  
  #caluclate quantiles and mean
  q05 <- quantile(x, 0.05, na.rm = T)
  q95 <- quantile(x, 0.95, na.rm = T)
  m <- mean(x, na.rm = T)
  
  #fit model
  res <- optim(par = c(12, 1), fn = model, parms = c(m, q05, q95), method = "L-BFGS-B",
               lower = c(0.01, -100), upper = c(100,100))
  
  
  #we calculate the parameter xi for the results
  xi <- (m - res$par[1]*res$par[2]*sqrt(2/(pi*(1+res$par[2]^2))))
  
  #return the parameters omega, alpha, xi
  return(c(res$par[1], res$par[2], xi))
  
}

#check if the function works
get_skewed_dist_parameter(answers$middle[answers$var == 'crop_feed'])



#---------------------------#
#stakeholder answers in distributions
#---------------------------#

#number of random draws, usually is 10,000
nsim <- 10000
#question <- 'crop_feed'

for(question in unique(answers$var)){
  

  #determine upper and lower boundaries
  lower <- ifelse(quantile(answers$middle[answers$var == question], 0.05) / 100 <= 0.01, 
                  yes = 0.02, 
                  no = quantile(answers$middle[answers$var == question], 0.05) / 100)
  upper <- ifelse(quantile(answers$middle[answers$var == question], 0.95) / 100 >= 0.99,
                  yes = 0.98, 
                  no = quantile(answers$middle[answers$var == question], 0.95) / 100)
  
  #current way to implement
  raw_quantiles <-  decisionSupport::rtnorm_0_1_90ci(n = nsim, 
                                                     lower = lower,
                                                     upper = upper,
                                                     correlationMatrix = 1)
  
  
  #calculate median, standard deviation
  med <- median(answers$middle[answers$var == question]) / 100
  sd <- sd(answers$middle[answers$var == question] / 100) 
  upper <- ifelse(med + sd > 0.98, yes = 0.98, no = med + sd)
  lower <- ifelse(med - sd < 0.01, yes = 0.01, no = med - sd)
  
  #using median and plus minus sd
  sd_way <- decisionSupport::rtnorm_0_1_90ci(n = nsim, lower = lower, upper = upper ,correlationMatrix = 1)
  
  
  #get parameters for skewed distribution
  pars <- get_skewed_dist_parameter(answers$middle[answers$var == question])
  #x <- seq(0, 100, by = 0.1)
  #plot(sn::dsn(x, omega = pars[1], alpha = pars[2], xi = pars[3]), x = x)
  
  
  #create distribution for skewed distribution
  skewed_way <- sn::rsn(n = nsim, xi = pars[3], omega = pars[1], alpha = pars[2])
  
  #in case of crop_feed and crop_food it makes sense to have two groups, so use the chance event thing too
  if(question %in% c('crop_food', 'crop_feed')){
    
    #split groups and link with chance event
    
    #caluclate upper and lower boundary of the two groups
    upper_g1 <- max(answers$middle[answers$var == question & answers$subgroup == 'a']) / 100
    lower_g1 <- min(answers$middle[answers$var == question & answers$subgroup == 'a']) / 100
    upper_g2 <- max(answers$middle[answers$var == question & answers$subgroup == 'b']) / 100
    lower_g2 <- min(answers$middle[answers$var == question & answers$subgroup == 'b']) / 100
    
    #draw numbers for both groups independently
    g1 <- decisionSupport::rtnorm_0_1_90ci(n = nsim, lower = lower_g1, upper = upper_g1 ,correlationMatrix = 1)
    g2 <- decisionSupport::rtnorm_0_1_90ci(n = nsim, lower = lower_g2, upper = upper_g2 ,correlationMatrix = 1)
    
    #weight of group1
    weight_g1 <- length(answers$middle[answers$var == question & answers$subgroup == 'a']) / length(answers$middle[answers$var == question])
    
    #object to store the final result
    chance_way <- c()
    
    for(i in 1:nsim){
      chance_way[i] <- decisionSupport::chance_event(weight_g1, value_if = g1[i], value_if_not = g2[i])
    }
    
    #bind everything to one data.frame
    dist_df <-  rbind(data.frame(value = chance_way, type = 'chance_event'),
                      data.frame(value = skewed_way / 100, type = 'skewed_distribution'),
                      data.frame(value = raw_quantiles, type = 'raw_answers_quantiles'),
                      data.frame(value = sd_way, type = 'median_answer_and_sd'))
  } else {
    #in case question is not part of crop feed or crop food, don't add chance event to the data.frame
    #bind everything to one data.frame
    dist_df <-  rbind(data.frame(value = skewed_way / 100, type = 'skewed_distribution'),
                      data.frame(value = raw_quantiles, type = 'raw_answers_quantiles'),
                      data.frame(value = sd_way, type = 'median_answer_and_sd'))
  }
  
  #set the limits of the plot depending on the limits of the dataframe
  xmin <- ifelse(min(dist_df$value) - 0.05 <= 0, yes = 0, no = min(dist_df$value) - 0.05 )
  xmax <- ifelse(max(dist_df$value) + 0.05 >= 1, yes = 1, no = max(dist_df$value) + 0.05 )
  
  #visualize the distribution
  p1 <- ggplot(dist_df, aes(x = value)) + 
    geom_histogram(bins = 150) + 
    geom_vline(xintercept = answers$middle[answers$var == question] / 100, col = 'red')+
    facet_wrap(~type, scales = 'free_y', ncol = 1, nrow = 4) + 
    xlim(xmin, xmax) + 
    theme_bw()
  
  #file name
  fname <- paste0('figures/stakeholder_input/distributions_', question,'.jpeg')
  
  ggsave(plot = p1,filename = fname, device = 'jpeg', width = 20, height = 15, units = 'cm')
  
  
  
  #(sum(dist_df$value[dist_df$type == 'skewed_distribution'] >= 0.67) / 10000) * 100
  
  
}


#retrieve the fitted values for crop_feed, crop_food, manure_export, manure_biogas, manure_crop

get_skewed_dist_parameter(answers$middle[answers$var == 'crop_feed'])
get_skewed_dist_parameter(answers$middle[answers$var == 'crop_food'])
get_skewed_dist_parameter(answers$middle[answers$var == 'manure_biogas'])
get_skewed_dist_parameter(answers$middle[answers$var == 'manure_crop'])
get_skewed_dist_parameter(answers$middle[answers$var == 'manure_export'])


















