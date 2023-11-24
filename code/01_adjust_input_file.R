#modify the input file of bernou

input <- read.csv('data/input-all_BW.csv')

#level 1 --> 1%
#level 2 --> 5%
#level 3 --> 10%
#level 4 --> 20%
#level 5 --> 25%

input$lower <- ifelse(input$ranges_from_excel == 'yes' , yes = as.numeric(input$lower), 
                      no = ifelse(input$distribution == 'const', yes = as.numeric(input$median),
                        no = ifelse(input$uncertainty.level == "1", yes =  as.numeric(input$median) * (1-0.01), 
                            no = ifelse(input$uncertainty.level == "2", yes =  as.numeric(input$median) * (1-0.05),
                               no = ifelse(input$uncertainty.level == "3", yes =  as.numeric(input$median) * (1-0.1),
                                           no = ifelse(input$uncertainty.level == "4", yes =  as.numeric(input$median) * (1-0.2),
                                                       no = ifelse(input$uncertainty.level == "5", yes =  as.numeric(input$median) * (1-0.25), no =NA)))))))

input$upper <- ifelse(input$ranges_from_excel == 'yes', yes = as.numeric(input$upper), 
                      no = ifelse(input$distribution == 'const', yes = as.numeric(input$median),
                         no = ifelse(input$uncertainty.level == "1", yes =  as.numeric(input$median) * (1+0.01), 
                                     no = ifelse(input$uncertainty.level == "2", yes =  as.numeric(input$median) * (1+0.05),
                                                 no = ifelse(input$uncertainty.level == "3", yes =  as.numeric(input$median) * (1+0.1),
                                                             no = ifelse(input$uncertainty.level == "4", yes =  as.numeric(input$median) * (1+0.2),
                                                                         no = ifelse(input$uncertainty.level == "5", yes =  as.numeric(input$median) * (1+0.25), no =NA)))))))

library(tidyverse)

#check for cases below 0
input %>% 
  filter(distribution == 'tnorm_0_1') %>%
  filter(lower <= 0)

#check for cases above 1
input %>% 
  filter(distribution == 'tnorm_0_1') %>%
  filter(upper >= 1)

#--> make sure that in this case the upper value is set to 1 instead
input$upper <- ifelse(input$distribution == 'tnorm_0_1' & input$upper >= 1, yes = 0.99, no = input$upper)
input$lower <- ifelse(input$distribution == 'tnorm_0_1' & input$lower <= 0, yes = 0.01, no = input$lower)

write.csv(input, file = 'data/input_all_uncertainty_classes.csv', row.names = F)
