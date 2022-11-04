#posthoc analysis

library(decisionSupport)

model_out <- readRDS(file = 'data/model_output_indicators.rds')

# #make indicators difference to baseline (subtract scenario from baseline, because fewer losses are better)
# model_out$y$total_input_N_PS <- as.numeric(model_out$y$total_input_N1) - as.numeric(model_out$y$total_input_N2)
# 
# mcSimulation_table <- data.frame(model_out$x, model_out$y[c('total_input_N_PS')])
# 
# library(decisionSupport)
# evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "total_input_N_PS")





model_out$y$losses_N_PS <- as.numeric(model_out$y$losses_N1) - as.numeric(model_out$y$losses_N2)


mcSimulation_table <- data.frame(model_out$x, model_out$y[c('losses_N_PS')])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "losses_N_PS")
plot_evpi(evpi, decision_vars = c("losses_N_PS"))

#this is bullshit, the function sais there is no reason to look at it because direction is clear

model_out$y$losses_K_PS <- as.numeric(model_out$y$losses_K1) - as.numeric(model_out$y$losses_K2)
mcSimulation_table <- data.frame(model_out$x, model_out$y[c('losses_K_PS')])
evpi <- multi_EVPI(mc = mcSimulation_table[1:1000,], first_out_var = "losses_K_PS")
plot_evpi(evpi, decision_vars = c("losses_K_PS"))

model_out$y$losses_P_PS <- as.numeric(model_out$y$losses_P1) - as.numeric(model_out$y$losses_P2)
mcSimulation_table <- data.frame(model_out$x, model_out$y[c('losses_P_PS')])
evpi <- multi_EVPI(mc = mcSimulation_table[1:1000,], first_out_var = "losses_P_PS")
plot_evpi(evpi, decision_vars = c("losses_P_PS"))
