#' Overview on Meta data

META_Overview <- function(stats){
  META_Info <- list()
  vent_info <- list()
  
  META_Info$all_data$studies <- paste0('There are ', length(unique(stats$study)), ' studies read in.')
  META_Info$all_data$homes <- paste0('There are ', length(unique(stats$ID_home)), ' homes read in.') 
  META_Info$all_data$rooms <- paste0('There are ', length(unique(stats$ID_room)), ' rooms read in.') 
  META_Info$all_data$ambientT <- paste0('Ambient temperatures are available for the following studies: ', paste(unique(stats$study[which(stats$room == 'AMB' & stats$variable == 'T')]), collapse = ', '))
  META_Info$all_data$variables <- paste0('The available variables are: ', paste(unique(stats$variable), collapse = ", "))
  
  #Measurement
  
  for(var in unique(stats$variable)){
    META_Info$all_data$variables_info[[var]]$studies <- paste0(var, ' can be found in: ', paste(unique(stats[stats$variable == var, ]$study), collapse = ', '))
    META_Info$all_data$variables_info[[var]]$N_datapoints <- paste0(var, ' has ', sum(stats[stats$variable == var, ]$N), ' measured datapoints.')
    META_Info$all_data$variables_info[[var]]$time_period <- paste0(var, ' was measured in the time period from ', min(stats[stats$variable == var, ]$quality_start), ' to ', max(stats[stats$variable == var, ]$quality_end), '.')
    META_Info$all_data$variables_info[[var]]$timeframe <- paste0('The mean measurement duration for ', var, ' is ', as.integer(mean(stats[stats$variable == var, ]$timeframe, na.rm = TRUE)), ' days with a maximum of ', as.integer(max(stats[stats$variable == var, ]$timeframe, na.rm = TRUE)), ' days.')
    META_Info$all_data$variables_info[[var]]$available_months <- paste0(var, ' was measured in the following months: ', paste(mixedsort(unique(stats[stats$variable == var, ]$month)), collapse = ', '), '.')
  }
  
  # wie viele Messpunkte pro Variable/Messdauer/Messmonate
  
  for (vent in unique(stats$vent_type)){
    META_Info$all_data$ventilation_info[[vent]]$N_datapoints <- paste0(vent, ' has ', sum(stats[stats$vent_type == vent, ]$N), ' measured datapoints.')
    META_Info$all_data$ventilation_info[[vent]]$homes_rooms <- paste0(vent, ' is available in ', length(unique(stats[stats$vent_type == vent, ]$ID_home)), ' homes and ', length(unique(stats[stats$vent_type == vent, ]$ID_room)), ' rooms.')
    META_Info$all_data$ventilation_info[[vent]]$time_period <- paste0(vent, ' was measured in the time period from ', min(stats[stats$vent_type == vent, ]$quality_start), ' to ', max(stats[stats$vent_type == vent, ]$quality_end), '.')
    META_Info$all_data$ventilation_info[[vent]]$timeframe <- paste0('The mean measurement duration for ', vent, ' is ', as.integer(mean(stats[stats$vent_type == vent, ]$timeframe, na.rm = TRUE)), ' days with a maximum of ', as.integer(max(stats[stats$vent_type == vent, ]$timeframe, na.rm = TRUE)), ' days.')
    META_Info$all_data$ventilation_info[[vent]]$available_months <- paste0(vent, ' was measured in the following months: ', paste(mixedsort(unique(stats[stats$vent_type == vent, ]$month)), collapse = ', '), '.')
  }
  
  META_Info$matricies$M_datapoints <- acast(stats, vent_type ~ variable, value.var = "N", fun.aggregate = sum)
  META_Info$matricies$M_home <- acast(stats, vent_type ~ variable, value.var = "ID_home", fun.aggregate = function(x) length(unique(x)))
  META_Info$matricies$M_room <- acast(stats, vent_type ~ variable, value.var = "ID_room", fun.aggregate = function(x) length(unique(x)))
  #if(!unique(grepl('BED', stats$room)) == FALSE)
  #META_Info$matricies$M_bedroom <- acast(stats[grepl('BED', stats$room), ], vent_type ~ variable, value.var = "ID_room", fun.aggregate = function(x) length(unique(x)))
  #if(!unique(grepl('LIV', stats$room)) == FALSE)
  #META_Info$matricies$M_livingroom <- acast(stats[grepl('LIV', stats$room), ], vent_type ~ variable, value.var = "ID_room", fun.aggregate = function(x) length(unique(x)))
  # ventilation types (wie viele pro variable? homes/rooms, Messdauer)
  # beides als Matrix (wie viele Messpunkte/homes/rooms für welche Variable bei welchen ventilation type)
  # Matrix noch für bedroom und livingroom, vllt für ambient T?
  
  
  for (stud in unique(stats$study)){
    ## filter for study
    meta_h_stud <- stats %>% filter(str_detect(stats$ID_home, stud))
    meta_r_stud <- stats %>% filter(str_detect(stats$ID_room, stud))
    stats_stud <- stats %>% filter(str_detect(stats$study, stud))
    ## prepare time period (just take min and max date from month == 'all' data, ok or more thorough required?)
    stats_stud_time <- stats_stud %>% filter(str_detect(stats_stud$month, 'all'))
    time_period <- paste(c(min(as.Date(stats_stud_time$quality_start)), max(as.Date(stats_stud_time$quality_end))), collapse = " to ")
    avail_T <- 
      
      META_Info$individual_studies[[stud]]$variables <- paste0('For study ', stud, ' the available variables are: ', paste(unique(stats[stats$study == stud, ]$variable), collapse = ', '))
    META_Info$individual_studies[[stud]]$homes <- paste0('For study ', stud, ' there are ', nrow(meta_h_stud), ' homes read in.')
    META_Info$individual_studies[[stud]]$rooms <- paste0('For study ', stud, ' there are ', nrow(meta_r_stud), ' rooms read in.') 
    META_Info$individual_studies[[stud]]$time_period <- paste0('For study ', stud, ' there is data in the period of ', time_period )
    META_Info$individual_studies[[stud]]$timeframe <- paste0('For study ', stud, ' the mean measurement duration is ', as.integer(mean(stats[stats$study == stud, ]$timeframe, na.rm = TRUE)), ' days with a maximum of ', as.integer(max(stats[stats$study == stud, ]$timeframe, na.rm = TRUE)), ' days.')
    META_Info$individual_studies[[stud]]$ambient_T <- paste0('For study ', stud, ' there are outside temperatures available: ', any(stats[stats$study == stud, ]$room %in% 'AMB' & stats[stats$study == stud, ]$variable == 'T'))
    META_Info$individual_studies[[stud]]$vent_rate_home <- paste0('For study ', stud, ' the ventilation rates (homes) are: ', paste(mixedsort(unique(stats[stats$study == stud, ]$vent_rate_home)), collapse = ', '))
    META_Info$individual_studies[[stud]]$vent_rate_room <- paste0('For study ', stud, ' the ventilation rates (rooms) are: ', paste(mixedsort(unique(stats[stats$study == stud, ]$vent_rate_room)), collapse = ', '))
    META_Info$individual_studies[[stud]]$energy_standard <- paste0('For study ', stud, ' the energy standard is: ', paste(unique(stats[stats$study == stud, ]$energy_standard), collapse = ', '))
  }
  
  return(META_Info)
}
