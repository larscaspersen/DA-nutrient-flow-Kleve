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

write.csv(input, file = 'data/input_all_uncertainty_classes.csv', row.names = F)
