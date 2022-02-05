# #waste subsystem
# 
# library(decisionSupport)
# 
# waste_input <- read.csv('data/input-waste.csv') 
# 
# make_variables<-function(est,n=1)
# { x<-random(rho=est, n=n)
# for(i in colnames(x)) assign(i,
#                              as.numeric(x[1,i]),envir=.GlobalEnv)
# }
# 
# waste_input$lower <- as.numeric(waste_input$lower)
# waste_input$upper <- as.numeric(waste_input$upper)
# 
# #create variable so I can test the function
# #make_variables(as.estimate(waste_input),n=1)

####wastewater

waste_function <- function( waste_water,
                            N_content_wastewater,
                            lossrate_wastewater,
                            share_N_sewage_for_agriculture,
                            compost_to_horticulture,
                            compost_to_export,
                            compost_to_consumption,
                            dm_compost_consumption,
                            dm_compost_horticulture,
                            N_content_compost_consumption,
                            N_content_compost_horticulture,
                            ofmsw_import,
                            ofmsw_local,
                            green_waste_import,
                            green_waste_local,
                            grey_bin_food_waste,
                            grey_bin_garden_waste,
                            dm_green_waste,
                            dm_ofmsw,
                            N_content_ofmsw_waste,
                            N_content_green_waste
  
){
  
  
  #transform wastewater from m3 to l
  waste_water <- waste_water * 1000
  
  #get N of waste water (= stream from consumption to waste in form of sewage)
  #N content is in mg per L but I need kg as output, so devide by 1000 * 1000
  N_sewage_in <- waste_water * N_content_wastewater / 1000000 
  
  #get amount of recovered and lost N by wastewater treatment
  N_sewage_lost <- N_sewage_in * lossrate_wastewater
  N_sewage_kept <- N_sewage_in - N_sewage_lost
  
  #get share of sewage N which is recycle back to field
  N_sewage_to_crop <- N_sewage_kept * share_N_sewage_for_agriculture
  N_sewage_exported <- N_sewage_kept - N_sewage_to_crop
  
  
  
  ####compost
  N_compost_crop <- compost_to_horticulture * dm_compost_horticulture * N_content_compost_horticulture
  N_compost_export <- compost_to_export * dm_compost_horticulture * N_content_compost_horticulture
  N_compost_consumption <- compost_to_consumption * dm_compost_consumption * N_content_compost_consumption
  
  
  ####ofmsw waste
  N_ofmsw_local <- ofmsw_local * dm_ofmsw * N_content_ofmsw_waste
  N_ofmsw_import <- ofmsw_import * dm_ofmsw * N_content_ofmsw_waste
  N_green_waste_local <- green_waste_local * dm_green_waste * N_content_green_waste
  N_green_waste_import <- green_waste_import * dm_green_waste * N_content_green_waste
  N_grey_bin_food_waste <- grey_bin_food_waste * dm_ofmsw * N_content_ofmsw_waste
  N_grey_bin_garden_waste <- grey_bin_garden_waste * dm_green_waste * N_content_green_waste
  
  return(list(N_sewage_in = N_sewage_in,
              N_sewage_lost = N_sewage_lost,
              N_sewage_to_crop = N_sewage_to_crop,
              N_sewage_exported = N_sewage_exported,
              N_compost_crop = N_compost_crop,
              N_compost_consumption = N_compost_consumption,
              N_compost_export = N_compost_export,
              N_ofmsw_local = N_ofmsw_local,
              N_ofmsw_import = N_ofmsw_import,
              N_green_waste_import = N_green_waste_import,
              N_green_waste_local = N_green_waste_local,
              N_grey_bin_garden_waste = N_grey_bin_garden_waste,
              N_grey_bin_food_waste = N_grey_bin_food_waste))
  
  
}




#in: 


# sewage from consumption
# manure as biogas substrate from animal   --- ALREADY IN ANIMAL
# vegetal biogas substrate from crop       --- ALREADY IN CROP

#out:


# digestate to crop
#calculated in biogas output neu
#total kewl * digestate (m3) per kwel --> total digestate (m3)
#transfer digetsate m3 to kg
#total digestate * N content digestate --> N of digestate




