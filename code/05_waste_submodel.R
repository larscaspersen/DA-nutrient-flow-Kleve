# #waste subsystem
# 
# library(decisionSupport)
# 
# waste_input <- read.csv('data/input-all.csv')
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
# make_variables(as.estimate(waste_input),n=1)

####wastewater

waste_function <- function( waste_water,
                            N_content_wastewater,
                            P_content_wastewater,
                            K_content_wastewater,
                            lossrate_wastewater,
                            share_sewage_for_agriculture,
                            compost_to_horticulture,
                            compost_to_export,
                            compost_to_consumption,
                            dm_compost_consumption,
                            dm_compost_horticulture,
                            N_content_compost_consumption,
                            P_content_compost_consumption,
                            K_content_compost_consumption,
                            N_content_compost_horticulture,
                            P_content_compost_horticulture,
                            K_content_compost_horticulture,
                            ofmsw_import,
                            ofmsw_local,
                            green_waste_import,
                            green_waste_local,
                            grey_bin_food_waste,
                            grey_bin_garden_waste,
                            dm_green_waste,
                            dm_ofmsw,
                            N_content_ofmsw_waste,
                            P_content_ofmsw_waste,
                            K_content_ofmsw_waste,
                            N_content_green_waste,
                            P_content_green_waste,
                            K_content_green_waste,
                            lossrate_N_wastewater,
                            lossrate_P_wastewater,
                            lossrate_K_wastewater,
                            convert_potassium_oxide_to_k,
                            convert_phosphorous_pentoxide_to_p
  
){
  
  ########
  # SEWAGE
  ########
  
  
  #transform wastewater from m3 to l
  waste_water <- waste_water * 1000
  
  #get N of waste water (= stream from consumption to waste in form of sewage)
  #N content is in mg per L but I need kg as output, so devide by 1000 * 1000
  N_sewage_in <- waste_water * N_content_wastewater / 1000000 
  P_sewage_in <- waste_water * P_content_wastewater / 1000000 
  K_sewage_in <- waste_water * K_content_wastewater / 1000000 
  
  #get amount of recovered and lost N by wastewater treatment
  N_sewage_lost <- N_sewage_in * lossrate_N_wastewater
  P_sewage_lost <- P_sewage_in * lossrate_P_wastewater
  K_sewage_lost <- K_sewage_in * lossrate_K_wastewater
  
  N_sewage_kept <- N_sewage_in - N_sewage_lost
  P_sewage_kept <- P_sewage_in - P_sewage_lost
  K_sewage_kept <- K_sewage_in - K_sewage_lost
  
  #the N / P / K sewage_kept represents the sludge which was extracted
  #but when deciding how much N from sewage goes to agriculture, it is rather a X% of the sewage goes to agriculture
  #and this amount X of sewage contains Y amount of N
  #so I should use a N concentration to get total amount of sewage
  #and then use a variable 
  
  
  #get share of sewage N which is recycle back to field
  N_sewage_to_crop <- N_sewage_kept * share_sewage_for_agriculture
  P_sewage_to_crop <- P_sewage_kept * share_sewage_for_agriculture
  K_sewage_to_crop <- K_sewage_kept * share_sewage_for_agriculture
  
  
  N_sewage_exported <- N_sewage_kept - N_sewage_to_crop
  P_sewage_exported <- P_sewage_kept - P_sewage_to_crop
  K_sewage_exported <- K_sewage_kept - K_sewage_to_crop
  
  
  
  
  ######
  # COMPOST
  ######
  
  N_compost_crop <- compost_to_horticulture * dm_compost_horticulture * N_content_compost_horticulture
  P_compost_crop <- compost_to_horticulture * dm_compost_horticulture * P_content_compost_horticulture * convert_phosphorous_pentoxide_to_p
  K_compost_crop <- compost_to_horticulture * dm_compost_horticulture * K_content_compost_horticulture * convert_potassium_oxide_to_k
  
  N_compost_export <- compost_to_export * dm_compost_horticulture * N_content_compost_horticulture
  P_compost_export <- compost_to_export * dm_compost_horticulture * P_content_compost_horticulture * convert_phosphorous_pentoxide_to_p
  K_compost_export <- compost_to_export * dm_compost_horticulture * K_content_compost_horticulture * convert_potassium_oxide_to_k
  
  N_compost_consumption <- compost_to_consumption * dm_compost_consumption * N_content_compost_consumption
  P_compost_consumption <- compost_to_consumption * dm_compost_consumption * P_content_compost_consumption * convert_phosphorous_pentoxide_to_p
  K_compost_consumption <- compost_to_consumption * dm_compost_consumption * K_content_compost_consumption * convert_potassium_oxide_to_k
  
  
  
  
  
  ######
  # SOLID WASTE
  ######
  
  
  ####ofmsw waste
  N_ofmsw_local <- ofmsw_local * dm_ofmsw * N_content_ofmsw_waste
  P_ofmsw_local <- ofmsw_local * dm_ofmsw * P_content_ofmsw_waste
  K_ofmsw_local <- ofmsw_local * dm_ofmsw * K_content_ofmsw_waste
  
  N_ofmsw_import <- ofmsw_import * dm_ofmsw * N_content_ofmsw_waste
  P_ofmsw_import <- ofmsw_import * dm_ofmsw * P_content_ofmsw_waste
  K_ofmsw_import <- ofmsw_import * dm_ofmsw * K_content_ofmsw_waste
  
  N_green_waste_local <- green_waste_local * dm_green_waste * N_content_green_waste
  P_green_waste_local <- green_waste_local * dm_green_waste * P_content_green_waste
  K_green_waste_local <- green_waste_local * dm_green_waste * K_content_green_waste
  
  N_green_waste_import <- green_waste_import * dm_green_waste * N_content_green_waste
  P_green_waste_import <- green_waste_import * dm_green_waste * P_content_green_waste
  K_green_waste_import <- green_waste_import * dm_green_waste * K_content_green_waste
  
  N_grey_bin_food_waste <- grey_bin_food_waste * dm_ofmsw * N_content_ofmsw_waste
  P_grey_bin_food_waste <- grey_bin_food_waste * dm_ofmsw * P_content_ofmsw_waste
  K_grey_bin_food_waste <- grey_bin_food_waste * dm_ofmsw * K_content_ofmsw_waste
  
  N_grey_bin_garden_waste <- grey_bin_garden_waste * dm_green_waste * N_content_green_waste
  P_grey_bin_garden_waste <- grey_bin_garden_waste * dm_green_waste * P_content_green_waste
  K_grey_bin_garden_waste <- grey_bin_garden_waste * dm_green_waste * K_content_green_waste
  

  return(list(N_sewage_in = N_sewage_in,
              P_sewage_in = P_sewage_in,
              K_sewage_in = K_sewage_in,
              
              N_sewage_lost = N_sewage_lost,
              P_sewage_lost = P_sewage_lost,
              K_sewage_lost = K_sewage_lost,
              
              N_sewage_to_crop = N_sewage_to_crop,
              P_sewage_to_crop = P_sewage_to_crop,
              K_sewage_to_crop = K_sewage_to_crop,
              
              N_sewage_exported = N_sewage_exported,
              P_sewage_exported = P_sewage_exported,
              K_sewage_exported = K_sewage_exported,
              
              #output should be in kg and not in tons
              N_compost_crop = N_compost_crop * 1000,
              P_compost_crop = P_compost_crop * 1000,
              K_compost_crop = K_compost_crop * 1000,
              
              N_compost_consumption = N_compost_consumption * 1000,
              P_compost_consumption = P_compost_consumption * 1000,
              K_compost_consumption = K_compost_consumption * 1000,
              
              N_compost_export = N_compost_export * 1000,
              P_compost_export = P_compost_export * 1000,
              K_compost_export = K_compost_export * 1000,
              
              N_ofmsw_local = N_ofmsw_local * 1000,
              P_ofmsw_local = P_ofmsw_local * 1000,
              K_ofmsw_local = K_ofmsw_local * 1000,
              
              N_ofmsw_import = N_ofmsw_import * 1000,
              P_ofmsw_import = P_ofmsw_import * 1000,
              K_ofmsw_import = K_ofmsw_import * 1000,
              
              N_green_waste_import = N_green_waste_import * 1000,
              P_green_waste_import = P_green_waste_import * 1000,
              K_green_waste_import = K_green_waste_import * 1000,
              
              N_green_waste_local = N_green_waste_local * 1000,
              P_green_waste_local = P_green_waste_local * 1000,
              K_green_waste_local = K_green_waste_local * 1000,
              
              N_grey_bin_garden_waste = N_grey_bin_garden_waste * 1000,
              P_grey_bin_garden_waste = P_grey_bin_garden_waste * 1000,
              K_grey_bin_garden_waste = K_grey_bin_garden_waste * 1000,
              
              N_grey_bin_food_waste = N_grey_bin_food_waste * 1000,
              P_grey_bin_food_waste = P_grey_bin_food_waste * 1000,
              K_grey_bin_food_waste = K_grey_bin_food_waste * 1000))
  
  
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




