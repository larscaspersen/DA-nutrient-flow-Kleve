##############
#create simple model of N-flow in district of Kleve
##############

library(decisionSupport)

#read input table
#input <- read.csv('data/input-table.csv')

#x <- input$median
#as.numeric(x[1])
#input$variable




#function to draw random variables of e.g. input data set to make a run of the model
make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

#draw random variables from input table, so it is easier to define the model and try it out on the fly
#make_variables(as.estimate(input),n=1)

#define the model
model_function <- function(){
  
  #bundle in and outgoing flows with same start and end
  consumption_to_outside <- waste_water_direct_charge
  consumption_to_waste <- sewage + OFMSW + OFMSW_in_residual_waste
  
  waste_to_outside <- sewage_sludge_export + gaseous_losses_WWT + fresh_compost_export
  waste_to_crop <- fresh_compost + sewage_sludge + digestate
  waste_to_consumption <- finished_compost_to_hobbygardeners
  
  crop_to_outside <- cultivation_losses + other_organic_fertilizer_export
  crop_to_animal <- feed_crops + grass_based_feed + straw
  crop_to_processing <- food_and_feed_crops + fruits_and_vegetables
  crop_to_waste <- vegetal_biogas_substrate
  
  animal_to_crop <- manure
  animal_to_outside <- manure_export + production_animal_housing
  animal_to_waste <- manure_as_biogas_substrate
  animal_to_processing <- slaughter_animals + local_animal_products_produced
  
  processing_to_animal <- processed_import_feed + feed_from_processed_crops
  processing_to_outside <- meat_export + vegetal_products_export + milk_export + slaughter_waste
  processing_to_consumption <- local_vegetal_products + local_animal_products_consumed + imported_animal_products + imported_vegetal_products
  
  outside_to_waste <- OFMSW_import
  outside_to_crop <- inorganic_fertilizers + organic_fertilizer_import
  outside_to_processing <- net_food_import + net_feed_import
  
  
  #calculate total inflow, outflow and balance of subsystem
  consumption_in <- processing_to_consumption +waste_to_consumption
  consumption_out <- consumption_to_outside + consumption_to_waste
  consumption_balance <- consumption_in - consumption_out
  
  waste_in <- consumption_to_waste + crop_to_waste + animal_to_waste + outside_to_waste
  waste_out <- waste_to_crop + waste_to_outside + waste_to_consumption
  waste_balance <- waste_in - waste_out
  
  crop_in <- waste_to_crop + animal_to_crop + outside_to_crop
  crop_out <- crop_to_waste + crop_to_processing + crop_to_animal + crop_to_outside
  crop_balance <- crop_in - crop_out
  
  animal_in <- crop_to_animal + processing_to_animal
  animal_out <- animal_to_processing + animal_to_waste + animal_to_outside + animal_to_crop
  animal_balance <- animal_in - animal_out
  
  processing_in <- crop_to_processing + animal_to_processing + outside_to_processing
  processing_out <- processing_to_animal + processing_to_consumption + processing_to_outside
  processing_balance <- processing_in - processing_out
  
  total_import <- outside_to_waste + outside_to_processing + outside_to_crop
  total_export <- consumption_to_outside + waste_to_outside + crop_to_outside + animal_to_outside + processing_to_outside
  total_balance <- total_import - total_export
  
  #decide what to return from the monte-carlo simulation
  return(list(consumption_in = consumption_in, 
              consumption_out = consumption_out, 
              consumption_balance = consumption_balance,
              waste_in = waste_in, 
              waste_out = waste_out, 
              waste_balance = waste_balance,
              crop_in = crop_in, 
              crop_out = crop_out, 
              crop_balance = crop_balance,
              animal_in = animal_in, 
              animal_out = animal_out, 
              animal_balance = animal_balance,
              processing_in = processing_in, 
              processing_out = processing_out, 
              processing_balance = processing_balance,
              total_import = total_import, 
              total_export = total_export, 
              total_balance = total_balance))

}



nitrogen_mc_simulation <- mcSimulation(estimate = as.estimate(input),
                                       model_function = model_function,
                                       numberOfModelRuns = 10000,
                                       functionSyntax = "plainNames")

plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
                   vars = c("total_import",'total_export'),
                   method = "smooth_simple_overlay",
                   old_names = c("total_import",'total_export'),
                   new_names = c("Total Nitrogen Import to Kleve","Total Nitrogen Export to Kleve"),
                   x_axis_name = 't N  / year')

plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
                   vars = c("total_balance"),
                   method = "boxplot_density",
                   old_names = c('total_balance'),
                   new_names = c("Total Nitrogen Balance of Kleve"),
                   x_axis_name = 't N  / year')

plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
                   vars = c("consumption_balance"),
                   method = "boxplot_density",
                   old_names = c('consumption_balance'),
                   new_names = c("Nitrogen Balance of Consumption"),
                   x_axis_name = 't N  / year')
plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
                   vars = c('animal_balance',"crop_balance",'processing_balance'),
                   method = "smooth_simple_overlay",
                   old_names = c('animal_balance','crop_balance','processing_balance'),
                   new_names = c("2_Nitrogen Balance of Animal Production",
                                 "1_Nitrogen Balance of Crop Production",
                                 "3_Nitrogen Balance of Food and Feed Processing"),
                   x_axis_name = 't N  / year')
#food and feed processing the widest, then comes animal, then comes crop production
#waste and consumption pretty certain



#investiagte which of the three parameters had the biggest impact on the final_profits
pls_result <- plsr.mcSimulation(object = nitrogen_mc_simulation,
                                resultName = names(nitrogen_mc_simulation$y['total_balance']), ncomp = 1)

plot_pls(pls_result, input = input, threshold = 0.8)

#here we subset the outputs from the mcSimulation function (y) by selecting the correct variables
# this should be done by the user (be sure to run the multi_EVPI only on the variables that the user wants)
#caution, this step takes forever (~30min at leas)
mcSimulation_table <- data.frame(nitrogen_mc_simulation$x, nitrogen_mc_simulation$y[c('animal_balance')]
)

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "animal_balance")
plot_evpi(evpi, decision_vars = "animal_balance")


