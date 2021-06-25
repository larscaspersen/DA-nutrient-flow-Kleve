#in this file I will try to combine the submodels of different scripts

library(decisionSupport)

#first test for nested function in decisionsupport

animal_input <- read.csv('data/input-animal.csv')

#function to draw random variables from input and create global variables
make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

#some tests regarding nested functions in decision support

# make_variables(as.estimate(animal_input),n=1)
# 
# nested_function_1 <- function(n_eggs){
#   #get N content of all eggs
#   N_eggs <- n_eggs * `egg-weight` / 100 *  N_content_egg / 1000
#   return(list(N_eggs = N_eggs))
# }
# 
# nested_function_2 <- function(){
#   #calculate amount of milk
#   N_milk <- n_dairy_cow * milk_per_cow * N_content_milk
#   return(list(N_milk = N_milk))
# }
# 
# test_function <- function(){
#   n_eggs <- egg_per_chicken * n_chicken
#   egg_output <- nested_function_1(n_eggs)
#   milk_output <- nested_function_2()
#   #combine both outputs
#   combined_output <- c(egg_output, milk_output)
#   return(combined_output)
# }
# 
# nitrogen_mc_simulation <- mcSimulation(estimate = as.estimate(animal_input),
#                                        model_function = test_function,
#                                        numberOfModelRuns = 10000,
#                                        functionSyntax = "plainNames")
# 
# plot_distributions(mcSimulation_object = nitrogen_mc_simulation,
#                    vars = c("N_eggs"),
#                    method = "smooth_simple_overlay",
#                    old_names = c('N_eggs'),
#                    x_axis_name = 'kg N  / year')

#so it turns out that in nested functions, you only need to define newly inside 
#the function created objects, but objects which are listed inside the input table
#don't need to be specified in the function argument




#read function etc from the scripts
source('animal_model.R')
source('crop_model.R')

#combine the input files
combined_input <- merge.data.frame(x = animal_input, y = crop_input, 
                                   by.x = c('variable','lower','median','upper',
                                            'distribution'),all.x = TRUE,all.y = TRUE)
#create function for mc-simulation
crop_animal_combined <- function(){
  
  #let the animla function run
  animal_output <- calc_animal()
  
  #let crop function run
  crop_output <- crop_function()
  
  #calculate the amount of feed needed to be imported ----
  #= sum of products produced (egg, milk, meat (lifeweight) minus local feed (gras, fodder crops))
  
  animal_output_produced <- animal_output$N_milk_available + animal_output$N_egg_available +
                              animal_output$N_remaining_manure + animal_output$N_housing_loss +
                              animal_output$N_to_slaughter
  
  animal_local_input <- crop_output$N_straw + crop_output$N_grassland + 
                          crop_output$N_crop_animal_feeding_processed +
                          crop_output$N_crop_human_consumption_unprocessed
  
  feed_import <- animal_output_produced - animal_local_input
  
  
  #combine output lists
  combined_output <- c(animal_output,crop_output, feed_import = feed_import,
                       export_org_fertilizer = export_org_fertilizer)
  
  return(combined_output)
}

#let mc simulation run, just to test if everything works out
nitrogen_mc_simulation <- mcSimulation(estimate = as.estimate(combined_input),
                                       model_function = crop_animal_combined,
                                       numberOfModelRuns = 100,
                                       functionSyntax = "plainNames")

#correct the crop shares in the input table of the mcsimulation object

