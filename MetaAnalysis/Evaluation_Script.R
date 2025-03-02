#library(remotes)
#install.packages("mvtnorm")
#install_cran("gamlss2", repos = "https://gamlss-dev.r-universe.dev")#, force = TRUE)

library(readxl)
library(dplyr)
library(gtools)
library(formatR)
library(stringr)
library(ggplot2)
library(lubridate)
library(plotly)
library(reshape2)
library(wordcloud)
library(tm)
library(tidyr)
library(readr)
library(openxlsx)
library(gamlss2)
library(knitr)
library(purrr)
library(gtsummary)
library(flextable)
library(gt)

## functions------------------
# percentiles to ecdf conversion
per2cdf <-  function(per,per_bin,cdf_bin){
  # if (is.vector(per)) {per <- matrix(per, nrow=1)}
  # else {per <- as.matrix(per)}
  approx_cdf <- function(x) {approx(x,per_bin,cdf_bin,ties="ordered",rule=2)$y}
  cdf <- t(apply(X=per, MARGIN=1, FUN=approx_cdf))
  return(data.frame(cdf))
}
# ecdf to percentile conversion
cdf2per <-  function(cdf,cdf_bin,per_bin){
  # if (is.vector(cdf)) {cdf <- matrix(cdf, nrow=1)}
  # else {cdf <- as.matrix(cdf)}
  approx_per <- function(x) {approx(x,cdf_bin,per_bin,ties="ordered",rule=2)$y}
  per <- t(apply(X=cdf, MARGIN=1, FUN=approx_per))
  return(data.frame(per))
}
# calculating mixture distrubtions
mix_cdf_CO2 <- function(df,wei,cdf = FALSE) {
  if(cdf == FALSE){
    if(missing(wei)) {
      #unweighted
      cdf_mix <- colMeans(per2cdf(df,Perc_bins,CO2_bins), na.rm=TRUE)
    } else {
      #weighted
      cdf_mix <- apply(per2cdf(df,Perc_bins,CO2_bins),MARGIN=2,FUN=weighted.mean,w=wei)
    }
    return(data.frame(t(cdf_mix)))
  }
  if(cdf == TRUE){
    if(missing(wei)) {
      #unweighted
      cdf_mix <- colMeans(df, na.rm=TRUE)
    } else {
      #weighted
      cdf_mix <- apply(df ,MARGIN=2,FUN=weighted.mean,w=wei)
    }
    return(data.frame(t(cdf_mix)))
  }
}
# check if all entries are same
first_and_check <- function(x) {
  if (length(unique(x))>1) {
    print("Warning: more than one entry in variables that should have been filtered!")
    return(NA)}
  else {
    return(first(x))}
}
# check if same and calculate mean (for sample interval)
mean_and_check <- function(x) {
  if (length(unique(x))>1) {
    print("Warning: different sample intervals have been aggregated!")}
  return(mean(x))
}
# calculate data abundance (N_rel = fraction of month with data) for weighting
data_abu <- function(N,NAs,year,month,tod,int) {
  nr_days <- lubridate::days_in_month(as.Date(paste(year, month,"01", sep = "-"), format = "%Y-%m-%d"))
  #nr_hours <- switch(tod, '23-07'=8, '07-23'=16, 'all'=24)
  nr_hours <- as.numeric(ifelse(tod=='23-07', 8, ifelse(tod=='07-23', 16, ifelse(tod=='all', 24, 0))))
  nr_datapts_per_hour <- 3600/int 
  return((N-NAs)/(nr_days*nr_hours*nr_datapts_per_hour))
}

META_Overview_GPT <- function(stats, meta_h, meta_r) {
  
  META_Info <- list(
    all_data = list(),
    matricies = list(),
    individual_studies = list()
  )
  
  # General dataset summary
  META_Info$all_data$studies <- sprintf("There are %d studies read in.", n_distinct(stats$study))
  META_Info$all_data$homes <- sprintf("There are %d homes read in.", n_distinct(stats$ID_h))
  META_Info$all_data$rooms <- sprintf("There are %d rooms read in.", n_distinct(stats$ID_r))
  META_Info$all_data$ambientT <- sprintf("Ambient temperatures are available for the following studies: %s", 
                                         paste(unique(stats$study[stats$room == 'AMB' & stats$variable == 'T']), collapse = ', '))
  META_Info$all_data$variables <- sprintf("The available variables are: %s", paste(unique(stats$variable), collapse = ", "))
  
  META_Info$all_data$variables_info <- setNames(lapply(unique(stats$variable), summarize_variable), unique(stats$variable))
  
  META_Info$all_data$ventilation_info <- setNames(lapply(unique(stats$vent_type), summarize_ventilation), unique(stats$vent_type))
  
  # Creating matrices
  META_Info$matricies$M_datapoints <- acast(stats, vent_type ~ variable, value.var = "N", fun.aggregate = sum)
  META_Info$matricies$M_home <- acast(stats, vent_type ~ variable, value.var = "ID_h", fun.aggregate = function(x) n_distinct(x))
  META_Info$matricies$M_room <- acast(stats, vent_type ~ variable, value.var = "ID_r", fun.aggregate = function(x) n_distinct(x))
  
  # Process individual studies
  for (stud in unique(stats$study)) {
    meta_h_stud <- meta_h %>% filter(str_detect(ID, stud))
    meta_r_stud <- meta_r %>% filter(str_detect(ID, stud))
    stats_stud <- stats %>% filter(study == stud)
    #stats_stud_time <- stats_stud %>% filter(month == 'all')
    meta_r_stud$home_id <- sub("-BED.*", "", meta_r_stud$ID)
    monthly_counts <- stats_stud %>%
      filter(str_detect(ID_r, "BED(1|2|3)?$")) %>%
      mutate(date = format(date, "%Y-%m")) %>%
      group_by(ID_h) %>%
      summarise(num_months = n_distinct(date))
    
    
    monthly_minmax <- stats_stud %>%
      filter(str_detect(ID_r, "BED(1|2|3)?$")) %>%
      mutate(year = format(date, "%Y"),   
             month = as.integer(format(date, "%m"))) %>%
      filter(!is.na(year)) %>%
      summarise(min_month = min(month), max_month = max(month), .groups = "drop")
    
    
    time_period <- ifelse(nrow(stats_stud) > 0,
                          sprintf("%s to %s", 
                                  min(as.Date(stats_stud$quality_start), na.rm = TRUE),
                                  max(as.Date(stats_stud$quality_end), na.rm = TRUE)),
                          "No time period available")
    
    
    print(stud)
    META_Info$individual_studies[[stud]] <- list(
      variables = sprintf("For study %s the available variables are: %s", stud, 
                          paste(unique(stats_stud$variable), collapse = ", ")),
      homes = sprintf("For study %s there are %d homes read in.", stud, length(unique(meta_h_stud$ID))),
      vent_rate_home = sprintf("For study %s the ventilation rates (homes) are: %s", stud, 
                               paste(mixedsort(unique(stats_stud$vent_rate_home)), collapse = ", ")),
      time_period = sprintf("For study %s there is data in the period of %s", stud, time_period),
      timeframe = sprintf("For study %s the mean measurement duration is %.0f days per month with a maximum of %.0f days per month.",
                          stud, mean(stats_stud$timeframe, na.rm = TRUE), 
                          max(stats_stud$timeframe, na.rm = TRUE)),
      ambient_T = sprintf("For study %s there are outside temperatures available: %s", stud, 
                          any(stats_stud$room == 'AMB' & stats_stud$variable == 'T')),
      rooms = sprintf("For study %s there are %d rooms read in.", stud, length(unique(meta_r_stud$ID))),
      bedrooms = sprintf("For study %s there are %d bedrooms read in.", stud, length(meta_r_stud$ID[grep("BED*", meta_r_stud$ID)])),
      vent_rate_room = sprintf("For study %s the ventilation rates (rooms) for bedrooms are: %s", stud, 
                               paste(mixedsort(unique(stats_stud$vent_rate_room[grep("BED*", stats_stud$room)])), collapse = ", ")),
      vent_rate_room_nr = sprintf("For study %s the number of ventilation rates (rooms) for bedrooms is: %.0f", stud, 
                                  length(meta_r_stud$ID[grepl("BED*", meta_r_stud$ID) & !is.na(as.numeric(meta_r_stud$`Ventilation rate (room; [l/s])`))])),
      occupancy_rooms = sprintf('For study %s the bedroom occupancy number is available: %s', stud,
                                any(!is.na(unique((na.omit(sapply(meta_r_stud$`Occupancy: Number`, convert_to_numeric))))))),
      energy_standard = sprintf("For study %s the energy standard is: %s", stud, 
                                paste(unique(stats_stud$energy_standard), collapse = ", ")),
      geometric_sizes = sprintf("For study %s geometry measurements are available for: home_size (%s), room_area_bedrooms (%s), room_volume_bedrooms (%s)", stud,
                      any(!is.na(as.numeric(unique(stats_stud$home_size)))), any(!is.na(as.numeric(unique(stats_stud$room_area[grepl("BED*$", stats_stud$room)])))), any(!is.na(as.numeric(unique(stats_stud$room_vol[grepl("BED*$", stats_stud$room)]))))),
      months_per_home = sprintf('For study %s the average number of months per bedroom are:  %.1f', stud,
                                mean(monthly_counts$num_months)),
      minmax_months_per_study = sprintf("For study %s the min and max of the measurement period per year for the bedrooms are: %.0f, %.0f", stud, 
                                        monthly_minmax$min_month, monthly_minmax$max_month),
      ventilation_type_mech = sprintf("For study %s vertilation type mechanical is available: %s", stud, 
                                      any(grepl("Mechanical ventilation", unique(stats_stud$vent_type)))),
      ventilation_type_hyb = sprintf("For study %s vertilation type hybrid is available: %s", stud, 
                                      any(grepl("Hybrid/mixed mode ventilation", unique(stats_stud$vent_type)))),
      ventilation_type_nat = sprintf("For study %s vertilation type natural is available: %s", stud, 
                                      any(grepl("Natural ventilation \\(designed\\)", unique(stats_stud$vent_type)))),
      ventilation_type_wind = sprintf("For study %s vertilation type window is available: %s", stud, 
                                      any(grepl("Window airing \\(not designed\\)", unique(stats_stud$vent_type))))
      #avg_p05_CO2_bed = sprintf("For study %s the average p05 of CO2 in bedrooms is: %.0f", stud,
      #                      stats_stud %>% filter(grepl("BED", room) & (variable == "CO2" & tod == "all") & !month == 'all') %>% summarise(mean_CO2_p05 = mean(p05, na.rm = TRUE))),
      #avg_p50_CO2_bed = sprintf("For study %s the average p50 of CO2 in bedrooms is: %.0f", stud,
      #                      stats_stud %>% filter(grepl("BED", room) & (variable == "CO2" & tod == "all") & !month == 'all') %>% summarise(mean_CO2_p50 = mean(p50, na.rm = TRUE))),
      #avg_p95_CO2_bed = sprintf("For study %s the average p95 of CO2 in bedrooms is: %.0f", stud,
      #                      stats_stud %>% filter(grepl("BED", room) & (variable == "CO2" & tod == "all") & !month == 'all') %>% summarise(mean_CO2_p95 = mean(p95, na.rm = TRUE)))
      
    )
  }
  
  return(META_Info)
}

# Helper function for variable summaries in META_Overview
summarize_variable <- function(var) {
  subset_stats <- stats %>% filter(variable == var)
  list(
    studies = setNames(sprintf("%s can be found in: %s", var, paste(unique(subset_stats$study), collapse = ", ")), var),
    N_datapoints = setNames(sprintf("%s has %d measured datapoints.", var, sum(subset_stats$N, na.rm = TRUE)), var),
    time_period = setNames(sprintf("%s was measured from %s to %s.", var, 
                                   min(subset_stats$quality_start, na.rm = TRUE), 
                                   max(subset_stats$quality_end, na.rm = TRUE)), var),
    timeframe = setNames(sprintf("The mean measurement duration for %s is %.0f days per month with a maximum of %.0f days per month.", var, 
                                 mean(subset_stats$timeframe, na.rm = TRUE), 
                                 max(subset_stats$timeframe, na.rm = TRUE)), var),
    available_months = setNames(sprintf("%s was measured in the following months: %s.", var, 
                                        paste(mixedsort(unique(subset_stats$month)), collapse = ", ")), var)
  )
}

# Helper function for ventilation summaries in META_Overview
summarize_ventilation <- function(vent) {
  subset_stats <- stats %>% filter(vent_type == vent)
  list(
    N_datapoints = setNames(sprintf("%s has %d measured datapoints.", vent, sum(subset_stats$N, na.rm = TRUE)), vent),
    homes_rooms = setNames(sprintf("%s is available in %d homes and %d rooms.", vent, 
                                   n_distinct(subset_stats$ID_h), n_distinct(subset_stats$ID_r)), vent),
    time_period = setNames(sprintf("%s was measured from %s to %s.", vent, 
                                   min(subset_stats$quality_start, na.rm = TRUE), 
                                   max(subset_stats$quality_end, na.rm = TRUE)), vent),
    timeframe = setNames(sprintf("The mean measurement duration for %s is %.0f days with a maximum of %.0f days.", vent, 
                                 mean(subset_stats$timeframe, na.rm = TRUE), 
                                 max(subset_stats$timeframe, na.rm = TRUE)), vent),
    available_months = setNames(sprintf("%s was measured in the following months: %s.", vent, 
                                        paste(mixedsort(unique(subset_stats$month)), collapse = ", ")), vent)
  )
}


# function to extract the volume information from the comment column in meta_r
extract_volume <- function(text) {
  
  # extract all numbers from the comment strings
  vol_string <- str_extract_all(text, "\\d{1,3}(?:[.,]\\d+)*")[[1]]
  vol <- tail(vol_string, 1)
  
  # check if vol is empty string
  if (length(vol)){
    # check if vol is NA
    if (is.na(vol))
      return(NA)
    
    # check if third entry of values is empty or only has spaces
    # for case "2.42 , , ,  - Height (m) , Floor Area (m2) , Surface Area (m2) , Volume (m3)"
    # third entry as proxy for Volume, as Surface Area is also missing when Volume is missing
    if (grepl("^\\s*$", strsplit(text, c(','))[[1]][3]))
      return(NA)
    
    # check if first read volume is 2 and return NA as proxy for case
    # for case "Room (also) used as Home-Office; [...] Room area (m2): [...]
    # for first entry value unlikely to be exactly 2?
    if (vol_string[1] == 2)
      return(NA)
    
    # check if last entry in vol_string is '3' for case: "Area: 16.5 m2, Volume: 40 m3"
    # and remove last entry from vol_string to get the volume value
    if(vol == '3'){
      vol_string <- vol_string[-length(vol_string)]
      vol <- tail(unlist(str_extract_all(vol_string, "\\d+(?:\\.\\d+)?")), 1)
    }
    
    # if last value '2', extract all values with decimal point and get last one for volume
    # for case: "1.96 , 4.31 , 16.44 , 8.43 , Height (m) , Floor Area (m2) , Surface Area (m2) , Volume (m3)"
    if(vol == '2'){
      vol <- tail(unlist(str_extract_all(vol_string, "\\d+\\.\\d+")), 1)
    }
  }
  
  # Replace commas with dots for decimal consistency
  vol <- str_replace(vol, ",", ".")
  
  return(vol)
}

convert_to_numeric <- function(x) {
  parts <- as.numeric(unlist(strsplit(x, "-")))  # Split and convert to numeric
  mean(parts)  # Compute mean (handles both single and range cases)
}
convert_to_numeric_home_size <- function(x) {
  parts <- as.numeric(unlist(strsplit(x, "-| ")))  # Split and convert to numeric
  mean(na.omit(parts))  # Compute mean (handles both single and range cases)
}

# function to extract the area information from the comment column in meta_r
extract_area <- function(text) {
  
  area <- NA
  a_string <- list()
  
  if (length(area)){
    if (grepl('Area:', text)){
      # 
      a_string <- str_extract_all(text, "\\d{1,3}(?:[.,]\\d+)*")[[1]]
      area <- a_string[1]
    }
    
    if (grepl('Floor Area |Room area', text)){
      #
      a_string <- str_extract_all(text, "\\d{1,3}(?:[.,]\\d+)*")[[1]]
      area <- a_string[2]
      if (grepl('-', str_split(text, ',')[[1]][1])){
        area <- a_string[3]
      }
      if (grepl('Home-Office', text))
        area <- a_string[2]
    }
    
    if (length(na.omit(area=='2'))){
      if (area =='2')
        area <- NA
      else if (area == '1')
        area <- NA
    }
  }
  
  # Replace commas with dots for decimal consistency
  area <- str_replace(area, ",", ".")
  
  return(area)
}

# function to remove data points / where vent_type is undefined
DelNAVentType <- function(sta){
  idxNoVent <- !sta$vent_type %in% c("Window airing (not designed)", "Natural ventilation (designed)", "Hybrid/mixed mode ventilation", "Mechanical ventilation")
  print(paste0("There were ", sum(idxNoVent, na.rm=TRUE), " data points (rooms) where ventilation type was NOT defined. -> Now deleted"))
  if (any(grepl('ID_r', names(sta))))
    print(paste0("See: ", unique(sta$ID_r[idxNoVent])))
  if (any(grepl('ID_h', names(sta))))
    print(paste0("See: ", unique(sta$ID_h[idxNoVent])))
  return(sta[!idxNoVent,])
}


## settings-------------------
who <- 'T'
switch(who,
       'T'={dir_src <- 'C:/Users/Timm/Desktop/Atmospheric Sciences/Arbeit'
       dir_dat <- './Annex86_AP3_out/'},
       'G'={dir_src <- 'C:/Daten/A86/AP3_AP4/src'
       dir_dat <- '../out/'},
       'G2'={dir_src <- 'C:/Users/Gabriel/Daten/A86/AP3_AP4/src'
       dir_dat <- '../out/'},
       'C'={dir_src <- ''
       dir_dat <- ''},
       'Sim'={dir_src <- 'D:/_USER/Gabriel/annex-86'
       dir_dat <- './out/'}
)
setwd(dir_src)

#sink(paste0("../tmp/", "Log_Evaluation_Script_10-12-24.txt"),type="output",split=TRUE)
#sink()

# define bin size for ecdfs
#Perc_bins0 <- as.numeric(unlist(str_split_i(colnames(sta_fil1_che1[,24:128]),pattern="p",i=2)))
Perc_bins <- c(seq(0,100),0.5, 2.5,97.5,99.5)
Perc_bins <- Perc_bins[order(Perc_bins)]
CO2_bins <- seq(300, 5000, length.out = 150)
CO2_midbins <- CO2_bins[-length(CO2_bins)] + diff(CO2_bins)/2

# define quality limits which entries to exclude in %
set_qual_lim <- c(1,1) # for lower and upper bound

#Choose countries to load datasets from
countries <- c('AUT', 'CHE', 'DNK', 'ESP', 'FRA', 'GBR', 'IRL', 'MEX', 'NLD', 'NOR', 'USA')
#countries <- c('AUT')
#countries <- c('AUT', 'BEL')

## Overview ---------------------------------
# create list with existing studies and files
studies <- list()
ffn_all <- c()
country_match <- tibble(country = character(), folder = character(), study = character())
for (c in countries) {
  # find subfolders in country folder
  t_names <- list.dirs(path = paste0(dir_dat, c), full.names= FALSE, recursive = FALSE)
  t_paths <- list.dirs(path = paste0(dir_dat, c), full.names= TRUE, recursive = FALSE)
  for (i in 1:length(t_names)) {
    studies[[c]][[t_names[i]]]$paths <- t_paths[i]
    studies[[c]][[t_names[i]]]$ffn <- list.files(path = t_paths[i], full.names = TRUE, "*.xlsx")
    #reads in the excel sheet an additional to match the country to the study (nessecary?)
    country_match <- add_row(country_match, country = c, study = unique(read_excel(studies[[c]][[t_names[i]]]$ffn[1], sheet = 'STAT')$study),  folder = t_names[i])
    ffn_all <- c(ffn_all, studies[[c]][[t_names[i]]]$ffn)
  }
}
country_match <- na.omit(country_match)
# tbd: add possibility to select studies (subfolders) maybe through external config (xlsfile)
# a code at the beginning could generate this file

## Step 1: Read data ----------------------------------------------------------------
stats <- data.frame()
meta_h <- data.frame()
meta_r <- data.frame()
#meta_v <- data.frame()
for (i_c in 1:length(studies)) {
  print(paste0("Country: ", names(studies)[i_c]))
  for (i_n in 1:length(studies[[i_c]])) {
    print(paste0("Loading data from study: ", names(studies[[i_c]])[i_n]))
    for (i_f in 1:length(studies[[i_c]][[i_n]]$ffn)) {
      stats <- rbind(stats, read_excel(studies[[i_c]][[i_n]]$ffn[i_f], sheet = 'STAT'))
      meta_h <- rbind(meta_h, read_excel(studies[[i_c]][[i_n]]$ffn[i_f], sheet = 'META-Home')[1:17])
      meta_r <- rbind(meta_r, read_excel(studies[[i_c]][[i_n]]$ffn[i_f], sheet = 'META-Room'))
      #meta_v <- rbind(meta_b, read_excel(studies[[i_c]][[i_n]]$ffn[i_f], sheet = 'META-Var'))
      print(paste0("Statistics and meta data of ", studies[[i_c]][[i_n]]$ffn[i_f], " has been read"))
    }
  }
}

stats0 <- stats #for development only: save duplicate for to avoid have to read again

## Step 1a: Basic data cleaning -----------------------------------

# drop all rows where study is NA to drop empty rows from dataset
print(paste0('Will remove ', nrow(subset(stats, is.na(study))), ' rows from dataframe with no study as proxy for empty rows'))
stats <- stats %>% drop_na(study)

# remove all rows with N < 30
print(paste0('Removed ', nrow(subset(stats, N < 30)), ' rows from dataframe with 30 or less measured datapoints'))
stats <- subset(stats, N >= 30)

# make sure entries in Column mean are numeric
# Note GR: why is this needed? all entries in mean should be numeric, no?
# response: they should be, however when I don't do it, the mean calculations fail without casting as they return NAs
stats$Mean <- as.numeric(stats$Mean)
print(paste0("There are ", sum(is.na(stats$Mean)), " datapoints, with no MEAN VALUE. Will be removed"))
print(paste0("See: ", unique(paste(stats$study[is.na(stats$Mean)],stats$home[is.na(stats$Mean)],stats$room[is.na(stats$Mean)] , sep="-" ))))
stats <- subset(stats, !is.na(stats$Mean))

## Step 1b: add req'd meta info to stats dataframe-----------------
# add room volume and room area to meta_r dataframe
meta_r$room_vol <- sapply(meta_r$`Additionial room information (e.g., ceiling height, with atrium)`, extract_volume)
meta_r$room_area <- sapply(meta_r$`Additionial room information (e.g., ceiling height, with atrium)`, extract_area)
meta_h$study <- str_split_i(meta_h$ID, '-', i=2)
meta_h$city_out_T <- meta_h$`Location: City`
meta_h$city_out_T[meta_h$study == "Lueftung3"] <- 'Wien'
meta_h$`Size of home / TFA [m^2]` <- sapply(meta_h$`Size of home / TFA [m^2]`, convert_to_numeric_home_size)
# add ID columns
stats$ID_h <- paste0(stats$user,"-",stats$study,"-",stats$home)
stats$ID_r <- paste0(stats$user,"-",stats$study,"-",stats$home,"-",stats$room)
# add date column: ?needed? and add 'all' for NA?
stats$date <- as.Date(paste(stats$year, stats$month, "01", sep = "-"), format = "%Y-%m-%d")
# add vent type/rate columns (not the most efficient, maybe change)
stats$vent_type <- meta_h$`Ventilation type`[match(stats$ID_h, meta_h$ID)]
stats$vent_type[is.na(stats$vent_type)] <- "No vent type given"
stats$vent_rate_home <- as.numeric(meta_h$`Ventilation rate (entire home; [l/s])`)[match(stats$ID_h, meta_h$ID)]
stats$vent_rate_room <- as.numeric(meta_r$`Ventilation rate (room; [l/s])`)[match(stats$ID_r, meta_r$ID)]
# add timeframe column in days from measurements with either month or year == 'all'
stats$timeframe <- NA
valid_rows <- grepl("^[0-9]+$", stats$month) & !is.na(stats$quality_start) & !is.na(stats$quality_end)
stats$timeframe[valid_rows] <- as.numeric(difftime(stats$quality_end[valid_rows], stats$quality_start[valid_rows], units = "days"))
# add energy standard column if available
stats$energy_standard <- meta_h$`Energy standard`[match(stats$ID_h, meta_h$ID)]
stats$building_type <- meta_h$`Type of building`[match(stats$ID_h, meta_h$ID)]
stats$home_size <- meta_h$`Size of home / TFA [m^2]`[match(stats$ID_h, meta_h$ID)]
stats$room_vol <- as.numeric(meta_r$room_vol)[match(stats$ID_r, meta_r$ID)]
stats$room_area <- as.numeric(meta_r$room_area)[match(stats$ID_r, meta_r$ID)]
stats$home_occupants <- meta_h$`Type of Occupants`[match(stats$ID_h, meta_h$ID)]
stats$room_occupants<- meta_r$`Occupancy: Number`[match(stats$ID_r, meta_r$ID)]
stats$vent_type_comment <- meta_h$`Comment Vent. Type`[match(stats$ID_h, meta_h$ID)]
stats$city <- meta_h$`Location: City`[match(stats$ID_h, meta_h$ID)]
# add countries to the corresponding studies
stats <- stats %>%
  left_join(country_match, by = "study")
# create col N_rel
stats <- stats %>% mutate(N_rel=data_abu(N,NAs,year,month,tod,interval_Median), .keep="all")

## Create META Overview table  --------

# attempt to filter the dataset and later join the p values from each study into the
# studies_df_tbl dataframe, did not work properly yet
stats_CO2 <- stats[stats$variable == 'CO2', ]
stats_CO2_che <- checkQual(stats_CO2,set_qual_lim)
stats_CO2_che <- stats_CO2_che %>%
  filter(!is.na(N_rel)) %>%
  filter(!N_rel == 0)
agg_stats_CO2_home <- aggHome(stats_CO2_che) # not read in at first start
agg_stats_CO2_room <- aggRoom(stats_CO2_che) # not read in at first start

agg_stats_CO2_home_ana <- ungroup(agg_stats_CO2_home) %>%
  group_by(study) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            nr_rooms=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(cdfs), cdf = TRUE)/100,
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

agg_stats_CO2_room_ana <- ungroup(agg_stats_CO2_room) %>%
  group_by(study) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            nr_rooms=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(cdfs), cdf = TRUE)/100,
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))


META_all <- META_Overview_GPT(stats, meta_h, meta_r)
write.xlsx(META_all,'../tmp/META_all_10-12-24.xlsx')

# Assuming META_Info is the output from META_Overview()
# Convert the individual studies list to a data frame:
studies_df <- map_dfr(META_all$individual_studies, ~ as.data.frame(.x, stringsAsFactors = FALSE), .id = "Study")

# extract META Info from META_all
studies_df_tbl <- studies_df %>%
  summarize(
    Study = Study,
    nr_homes = as.numeric(str_extract(sapply(str_split(homes, 'there'), function(x) x[2]), "\\d+")),
    vent_rate_home_available = map_chr(vent_rate_home, ~ str_split(.x, ": ")[[1]][2]),
    home_size_available  = as.logical(map_chr(str_extract_all(geometric_sizes, "(TRUE|FALSE)"), ~ .x[1])),
    nr_rooms = as.numeric(str_extract(sapply(str_split(rooms, 'there'), function(x) x[2]), "\\d+")),
    nr_bedrooms = as.numeric(str_extract(sapply(str_split(bedrooms, 'there'), function(x) x[2]), "\\d+")),
    vent_rate_room_available = map_chr(vent_rate_room, ~ str_split(.x, ": ")[[1]][2]),
    vent_rate_room_nr_bed = as.numeric(str_extract(vent_rate_room_nr, "\\d+$")),
    room_area_available  = as.logical(map_chr(str_extract_all(geometric_sizes, "(TRUE|FALSE)"), ~ .x[2])),
    room_vol_available   = as.logical(map_chr(str_extract_all(geometric_sizes, "(TRUE|FALSE)"), ~ .x[3])),
    occupancy_room_available = as.logical(map_chr(occupancy_rooms, ~ str_split(.x, ": ")[[1]][2])),
    vent_type_mechanical_available = as.logical(map_chr(ventilation_type_mech, ~ str_split(.x, ": ")[[1]][2])),
    vent_type_hybrid_available = as.logical(map_chr(ventilation_type_hyb, ~ str_split(.x, ": ")[[1]][2])),
    vent_type_natural_available = as.logical(map_chr(ventilation_type_nat, ~ str_split(.x, ": ")[[1]][2])),
    vent_type_window_available = as.logical(map_chr(ventilation_type_wind, ~ str_split(.x, ": ")[[1]][2])), 
    minmax_months_bedrooms = sapply(str_extract_all(minmax_months_per_study, "\\d+"), function(x) {
      if (length(x) > 2) {
        x <- tail(x, 2)  # Keep only the last two numbers
      }
      paste(x, collapse = " ")  # Convert back to a string
    }),
  )

#studies_df_tbl <- studies_df_tbl %>%
#  left_join(agg_stats_CO2_home_ana$percs[], by = c("Study" = "study"))
#studies_df_tbl[studies_df_tbl$Study == agg_stats_CO2_home_ana$study]

has_numeric <- function(x) {
  any(!is.na(as.numeric(unlist(str_split(x, ",\\s*")))), na.rm = TRUE)
}
studies_df_tbl$vent_rate_home_available <- sapply(studies_df_tbl$vent_rate_home_available, has_numeric)
studies_df_tbl$vent_rate_room_available <- sapply(studies_df_tbl$vent_rate_room_available, has_numeric)
studies_df_tbl$minmax_months_bedrooms[studies_df_tbl$minmax_months_bedrooms == ''] <- NA
#gt_table <- gt(studies_df_tbl)
#gt_table



# Now create a summary table.
# (Adjust the variables and labels to match the German instructions and your needs)
summary_tbl <- tbl_summary(studies_df_tbl, include = everything(),
                           by= 'Study', statistic = list(all_continuous() ~ "{sum}", all_categorical() ~ "{n}"), #all_continuous() ~ function(x) sprintf("{sum}, ({%.2f})", mean(x) / sum(x))))
                           type = list(where(is.numeric) ~ "continuous")) 


summary_tbl <- summary_tbl |>
              add_overall(statistic = list(
                all_continuous() ~ "{sum}",       # For continuous variables
                all_categorical() ~ "{n} ({p}%)", last = FALSE)) |>
              modify_header(all_stat_cols() ~ "**{level}**") |>
              modify_header(stat_0 ~ "**{level}**, N = {n}") |>
              modify_spanning_header(all_stat_cols() ~ "META variable availability overview") |>
              modify_footnote(c(all_stat_cols()) ~ NA)

summary_tbl
gt <- summary_tbl |> 
      as_gt() |> 
      gt::gtsave(filename = "Overview_table_v4.html")

## Step 1c: provide info about missing data / meta info and transform if needed-----------
idxNoVent <- !stats$vent_type %in% c("Window airing (not designed)", "Natural ventilation (designed)", "Hybrid/mixed mode ventilation", "Mechanical ventilation")
print(paste0("There are ", sum(idxNoVent, na.rm=TRUE), " data points where ventilation type is NOT defined."))
print(paste0("See: ", unique(paste(stats$study[idxNoVent],stats$home[idxNoVent],stats$room[idxNoVent] , sep="-" ))))
stats$vent_type[idxNoVent] <- NA

## Step 2: Filter --------------------------------------------

# Filter1: CO2 in BED at night in winter (Months 11-3)
# NA warnings due to conversion to numeric with some "all" -> can be ignored
fil1 <- stats$variable=="CO2" & grepl("BED", stats$room) & stats$month!="all" &
  (as.numeric(stats$month)>=11 | as.numeric(stats$month)<=3) & stats$tod=="23-07"
print(paste0(sum(fil1), " datapoints for this filter"))
sta_fil1 <- stats[fil1,]
#META_all_fil1 <- META_Overview(sta_fil1, meta_h, meta_r)

# Filter2: CO2 in LIV during the day in winter (Months 11-3)
fil2 <- stats$variable=="CO2" & grepl("LIV", stats$room) & stats$month!="all" &
  (as.numeric(stats$month)>=11 | as.numeric(stats$month)<=3) & stats$tod=="07-23"
print(paste0(sum(fil2), " datapoints for this filter"))
sta_fil2 <- stats[fil2,]

# Filter3: CO2 in BED at night at any month of year
fil3 <- stats$variable=="CO2" & grepl("BED", stats$room) & stats$month!="all" & stats$tod=="23-07"
print(paste0(sum(fil3), " datapoints for this filter"))
sta_fil3 <- stats[fil3,]

# FilterA: CO2 in any room for full day at any month of year
filA <- stats$variable=="CO2" & stats$month!="all" & stats$tod=="all"
print(paste0(sum(filA), " datapoints for this filter"))
sta_filA <- stats[filA,]

## Step 3: Check plausibility (within quality bounds) and clean if outside ------
checkQual <- function(sta,lim){
  idx <- (sta$quality_lower > lim[1] | sta$quality_upper > lim[2])
  print(paste0(sum(idx), " datapoints are below/above the quality limit and are removed."))
  print(paste0("See ", unique(sta$ID_r[idx])))
  return(sta[!idx,])
}


sta_fil1_che1 <- checkQual(sta_fil1,set_qual_lim)
sta_fil2_che2 <- checkQual(sta_fil2,set_qual_lim)
sta_fil3_che3 <- checkQual(sta_fil3,set_qual_lim)
sta_filA_cheA <- checkQual(sta_filA,set_qual_lim)

## Step 3a: Check data availability, Histograms --------------------------
# histogram functions------------------------
# histogram of measured time vs data
histoMeasTime_date <- function(sta,date_start){
  if (missing(date_start)) {
    sta_hist <- sta %>%
      group_by(date) %>%
      summarize(N = sum(N_rel, na.rm=TRUE))
  } else {
    sta_hist <- sta %>%
      filter(date>date_start) %>%
      group_by(date) %>%
      summarize(N = sum(N_rel, na.rm=TRUE))
  }
  g <- ggplot(sta_hist, aes(x = date, y = N)) +
    geom_histogram(stat = "identity") +
    labs( x = "Date",
          y = "Total measurement time [No of months]") +
    theme_minimal()
  ggplotly(g)
}

# histogram of measured time vs date and rooms
histoMeasTime_date_room <- function(sta,date_start){
  if (missing(date_start)) {  # if no date limitation is provided
    sta_hist <- sta %>%
      group_by(date,room) %>%
      summarize(N = sum(N_rel, na.rm=TRUE))
  } else {
    sta_hist <- sta %>%
      filter(date>date_start) %>%
      group_by(date,room) %>%
      summarize(N = sum(N_rel, na.rm=TRUE))
  }
  sta_hist$room <- substr(sta_hist$room,1,3) # remove room numbering
  g <- ggplot(sta_hist, aes(x = date, y = N, fill = room)) +
    geom_histogram(stat = "identity") +
    labs( x = "Date",
          y = "Total measurement time [No of months]",
          fill = "Measured Room") +
    theme_minimal() + 
    theme(legend.position = "top",
          legend.key.size = unit(0.3,"cm"),
          legend.title = element_text(size = 10, face = "bold"),
          legend.title.align=0,
          legend.box.margin = margin(t = 5)) + 
    guides(fill = guide_legend(nrow = 1))  # Ensure legend entries are in a single row
  return(g)
}

# histogram of measured time vs date and studies
histoMeasTime_date_study <- function(sta,date_start){
  if (missing(date_start)) {
    sta_hist <- sta %>%
      group_by(date,study) %>%
      summarize(N = sum(N_rel, na.rm=TRUE))
  } else {
    sta_hist <- sta %>%
      filter(date>date_start) %>%
      group_by(date,study) %>%
      summarize(N = sum(N_rel, na.rm=TRUE))
  }
  g <- ggplot(sta_hist, aes(x = date, y = N, fill = study)) +
    geom_histogram(stat = "identity") +
    labs( x = "Date",
          y = "Total measurement time [No of months]") +
    theme(legend.position = "top",
          legend.key.size = unit(0.3,"cm"),
          legend.title = element_text(size = 10, face = "bold"),
          legend.title.align=0,
          legend.box.margin = margin(t = 5)) +
    guides(fill = guide_legend(nrow = 2))  # Ensure legend entries are in two rows
  return(g)
}

#colors = c( "blue", "green", "brown", 'black')
colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728") # Bright Colors
#colors <- c("#0072B2", "#D55E00", "#F0E442", "#009E73") # Colorblind-Friendly
#colors <- c( "#3288BD", "#276419", "#F46D43", "#8E0152")  # Diverging Bright
#colors <- c("#117733", "#88CCEE", "#CC6677", "#DDCC77")  # Colorblind-Safe Vibrant
#colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")  # Bright from RColorBrewer

# histogram of measured time vs country and vent type
histoMeasTime_country_vent <- function(sta){
  sta_hist <-sta %>%
    group_by(country, vent_type) %>%
    summarize(N = sum(N_rel,na.rm=TRUE), N_vent = sum(length(vent_type),na.rm=TRUE))
  
  g <- ggplot(sta_hist, aes(x = country, y = N, fill = vent_type)) +
    geom_histogram(stat = "identity") +
    labs(x = "Country",
         y = "Total measurement time [No of months]") +
    scale_fill_manual(values = c("Mechanical ventilation" = colors[1], "Hybrid/mixed mode ventilation" = colors[2],
                                 "Natural ventilation (designed)" = colors[3], "Window airing (not designed)" = colors[4] ))+
    theme_minimal()
  return(g)
}

# plotting histograms-----------------
histoMeasTime_date(sta_filA_cheA,'2008-01-01')
histoMeasTime_date_room(sta_filA_cheA,'2008-01-01')
histoMeasTime_date_study(sta_filA_cheA,'2008-01-01')

histoMeasTime_country_vent((sta_filA_cheA))

histoMeasTime_country_vent((sta_fil1_che1))

# plot to compare studies
# boxplot functions---------------
boxpl_compare_studies <- function(sta,myvar){
  par(mar=c(7,4.1,1,1)) #mymar <- par("mar") # 5.1 4.1 4.1 2.1
  # define study names and rename some studies for better overview
  sta$study[sta$study=="Lodenareal" | sta$study=="Kufstein"] <- "Loden_Kuf"
  sta$study[sta$study=="HomeOffice_COVID19"] <- "HO_COVID19"
  study_labels <- unique(sta$study)  # Extract unique study names
  
  # group be ventilation
  fil_MV <- sta$vent_type=="Mechanical ventilation"
  myform <- as.formula(paste(myvar, "~ fil_MV:study"))
  
  # Generate the boxplot
  boxplot(myform, 
          data = sta, 
          col = c("grey", "white"), 
          las = 2, 
          xlab = "", 
          ylab = paste0(myvar," of CO2 concentration [ppm]"), 
          #main = "Boxplot by Study and Ventilation Type", 
          xaxt = "n",  # Suppress default x-axis labels
          xlim = c(1.5, length(study_labels)*2-0.5))
  #ylim = c(400,3000))
  
  # Add custom x-axis labels
  axis(1, 
       at = seq(1.5, by = 2, length.out = length(study_labels)),  # Midpoint for each study
       labels = sort(study_labels), 
       las = 2)  # Rotate labels for readability
  
  # Add vertical lines to separate studies
  abline(v = seq(2.5, by = 2, length.out = length(study_labels) - 1), col = "darkgray", lty = 2)
  
  # Add a legend
  legend("topright", 
         legend = c("Non-mechanical", "Mechanical"), 
         fill = c("grey", "white"), 
         title = "Ventilation Type", 
         cex = 0.8)
  par(mar=c(5.1,4.1,4.1,2.1)) 
}
# plotting boxplots------------
boxpl_compare_studies(sta_fil1_che1,"Mean")
boxpl_compare_studies(sta_fil1_che1,"p95")

# old simple boxplot
# boxplot(Mean ~ fil1_MV:study, data=sta_tmp, col=c("grey","white"), las=2)

# # some plot tests: ggplot nicer but how to not have the fill variable overlap?
# g <- ggplot(sta_fil1_che1, aes(x=study, y=Mean)) +
#   geom_boxplot(aes(fill=vent_type=="Mechanical ventilation"),position= position_nudge(x=-.5))+
#   guides(fill="none")
#   # scale_fill_discrete(labels=c("Mech","No-Mech","NA"))
# ggplotly(g)
# 
# 
# g <- ggplot(sta_fil3_che3, aes(x=study, y=Mean)) +
#   geom_boxplot(aes(fill=vent_type)) #+ facet_wrap( ~ vent_type, scales="free")
# ggplotly(g)

## Step 4: Aggregate to have one row per home-room -------------
# aggregate function--------------------------
# over all ID_r to get one entry per room for the entire measurement period instead of monthly 
aggRoom <- function(sta){
  sta_ag <- sta %>%
    group_by(ID_r) %>%
    summarise(across(year:month, ~min(.x)),
              #across(tod:variable, ~first_and_check(.x)),
              across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
              across(quality_start, ~min(.x)),
              across(quality_end, ~max(.x)),
              across(interval_Median, ~mean_and_check(.x)),
              across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
              across(Mean, ~weighted.mean(.x, N_rel, na.rm=TRUE)),
              across(study),
              across(vent_type),
              across(home_size, ~as.numeric(.x)),
              across(room_vol),
              across(room_area),
              across(country),
              across(vent_type_comment),
              N_count = 1,
              across(vent_rate_home:vent_rate_room),
              cdfs=mix_cdf_CO2(pick(p00:p100),N_rel,cdf=FALSE),
              percs=cdf2per(cdfs,CO2_bins,Perc_bins)) %>%
    distinct(.keep_all = TRUE)
  return(sta_ag)
}

aggHome <- function(sta){
  sta_ag <- sta %>%
    group_by(ID_h) %>%
    summarise(across(year:month, ~min(.x)),
              #across(tod:variable, ~first_and_check(.x)),
              across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
              across(quality_start, ~min(.x)),
              across(quality_end, ~max(.x)),
              across(interval_Median, ~mean_and_check(.x)),
              across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
              across(Mean, ~weighted.mean(.x, N_rel, na.rm=TRUE)),
              across(study),
              across(vent_type),
              across(home_size, ~as.numeric(.x)),
              across(room_vol),
              across(room_area),
              across(country),
              across(vent_type_comment),
              N_count = 1,
              across(vent_rate_home:vent_rate_room),
              cdfs=mix_cdf_CO2(pick(p00:p100),N_rel,cdf = FALSE),
              percs=cdf2per(cdfs,CO2_bins,Perc_bins)) %>%
    distinct(.keep_all = TRUE)
}

# histogram of number of rooms vs country and vent type
histoRooms_country_vent <- function (sta, fil_type){
  g <- ggplot(sta, aes(x = country, y = N_count, fill = vent_type)) +
    geom_histogram(stat = "identity") +
    labs(#title = paste0("Ventilation type availability per country ", fil_type),
         x = "Country",
         y = "Number of rooms") +
    scale_fill_manual(values = c("Mechanical ventilation" = colors[1], "Hybrid/mixed mode ventilation" = colors[2],
                                 "Natural ventilation (designed)" = colors[3], "Window airing (not designed)" = colors[4] ))+
    theme_minimal()
  return(g)
}

# histogram of number of homes vs country and vent type
histoHomes_country_vent <- function (sta, fil_type){
  g <- ggplot(sta, aes(x = country, y = N_count, fill = vent_type)) +
    geom_histogram(stat = "identity") +
    labs(#title = paste0("Ventilation type availability per country ", fil_type),
      x = "Country",
      y = "Number of homes") +
    scale_fill_manual(values = c("Mechanical ventilation" = colors[1], "Hybrid/mixed mode ventilation" = colors[2],
                                 "Natural ventilation (designed)" = colors[3], "Window airing (not designed)" = colors[4] ))+
    theme_minimal()
  return(g)
}

# room aggregate and plot ---------------
sta_fil1_che1_room_ag <- aggRoom(sta_fil1_che1)
sta_fil1_che1_room_ag <- DelNAVentType(sta_fil1_che1_room_ag)
histoRooms_country_vent(sta_fil1_che1_room_ag, "(Bedroom, Night, Winter)")

boxpl_compare_studies(sta_fil1_che1_room_ag,"Mean")

sta_fil2_che2_room_ag <- aggRoom(sta_fil2_che2)
sta_fil2_che2_room_ag <- DelNAVentType(sta_fil2_che2_room_ag)
histoRooms_country_vent(sta_fil2_che2_room_ag, "(Livingroom, Day, Winter)")

# home aggregate and plot --------------

sta_fil1_che1_home_ag <- aggHome(sta_fil1_che1)
sta_fil1_che1_home_ag <- DelNAVentType(sta_fil1_che1_home_ag)
histoHomes_country_vent(sta_fil1_che1_home_ag, "(Bedroom, Night, Winter)")

boxpl_compare_studies(sta_fil1_che1_home_ag,"Mean")

sta_fil2_che2_home_ag <- aggHome(sta_fil2_che2)
sta_fil2_che2_home_ag <- DelNAVentType(sta_fil2_che2_home_ag)
histoHomes_country_vent(sta_fil2_che2_home_ag, "(Livingroom, Day, Winter)")

## Step 5: filtering / grouping / aggregation for final analysis ----------------
sta_fil1_che1_room_ag_ana <- ungroup(sta_fil1_che1_room_ag) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            nr_rooms=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(cdfs), cdf = TRUE)/100,
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

sta_fil2_che2_room_ag_ana <- ungroup(sta_fil2_che2_room_ag) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            nr_rooms=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(cdfs), cdf = TRUE)/100,
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

#mean from pdf: sum(x*f(x))
mu1 <- sum(CO2_midbins*sta_fil1_che1_room_ag_ana$pdfs)
mu2 <- sum(CO2_midbins*sta_fil2_che2_room_ag_ana$pdfs)

#sd from pdf: sqrt(sum((x-mu)^2*f(x)))
sd1 <- sqrt(sum((CO2_midbins-mu1)**2*sta_fil1_che1_room_ag_ana$pdfs))
sd2 <- sqrt(sum((CO2_midbins-mu2)**2*sta_fil2_che2_room_ag_ana$pdfs))

## Step 6: visualise results ---------------------------

# plot functions ecdfs-------------
plot_ecdf_CO2 <- function(cdf_df){
  g <- ggplot(cdf_df, aes(x = CO2, y = cdfs, color = group)) +
    geom_line(size= 1) +
    labs(
      x = expression('CO'[2]*' concentration [ppm]'),
      y = 'Cumulative probability [-]',
      color = "Legend" ) +
    xlim(300, 3000) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.key.size = unit(0.3,"cm"),
          legend.title = element_text(size = 10, face = "bold"),
          legend.title.align=0,
          legend.box.margin = margin(t = 5)) + 
    scale_color_manual(values = c('bedroom, night, winter' = "#984EA3", 'living room, day, winter' = "#DDCC77"),
                       labels = c(paste0('bedroom, night, winter (N=', sta_fil1_che1_room_ag_ana$nr_rooms, ')'),
                                  paste0('living room, day, winter (N=', sta_fil2_che2_room_ag_ana$nr_rooms, ')')))
  return(g)
}

# plot ecdfs---------------------
df_plot <- data.frame(
  CO2 = rep(CO2_bins,2),
  cdfs = c(as.numeric(sta_fil1_che1_room_ag_ana$cdfs), as.numeric(sta_fil2_che2_room_ag_ana$cdfs)),
  group = rep(c("bedroom, night, winter", "living room, day, winter"), each = length(CO2_bins))
)

#ggplot style
plot_ecdf_CO2(df_plot)
#base plot style
par(mar=c(4,4,2,2)) 
plot(CO2_bins,sta_fil1_che1_room_ag_ana$cdfs, type="l", col = 'red', 
     ylab = 'Cumulative probability [-]',
     xlab = expression('CO'[2]*' concentration [ppm]'),
     xlim = c(400,3000),
     panel.first=grid())
lines(CO2_bins,sta_fil2_che2_room_ag_ana$cdfs, type="l", col ='blue')
legend("bottomright", legend = c(paste0('bedroom, night, winter (N=', sta_fil1_che1_room_ag_ana$nr_rooms, ')'),
                                 paste0('living room, day, winter (N=', sta_fil2_che2_room_ag_ana$nr_rooms, ')')),
       col = c('red', 'blue'), lwd = 1)
#par(mar=c(5.1,4.1,4.1,2.1)) 

# plot functions epdfs (Does not work properly yet)-------------
# plot_epdf_CO2 <- function(pdf_df){
#   g <- ggplot(pdf_df, aes(x = CO2, y = pdfs, color = group)) +
#     geom_line() +
#     labs(
#       x = expression('CO'[2]*' concentration [ppm]'),
#       y = 'Probability distribution [-]',
#       color = "Legend" ) +
#     #xlim(300, 3000) +
#     theme_minimal() +
#     theme(legend.position = "top",
#           legend.key.size = unit(0.3,"cm"),
#           legend.title = element_text(size = 10, face = "bold"),
#           legend.title.align=0,
#           legend.box.margin = margin(t = 5)) + 
#     scale_color_manual(values = c('bedroom, night, winter' = '#E41A1C', 'living room, day, winter' = '#1f77b4'),
#                        labels = c(paste0('bedroom, night, winter (N=', sta_fil1_che1_room_ag_ana$nr_rooms, ')'),
#                                   paste0('living room, day, winter (N=', sta_fil2_che2_room_ag_ana$nr_rooms, ')')))
#   return(g)
# }
# 
# #ggplot style
# plot_epdf_CO2(df_plot)

# base plot style
max_y = list()
max_y[1] = max(sta_fil1_che1_room_ag_ana$pdfs)
max_y[2] = max(sta_fil2_che2_room_ag_ana$pdfs)
plot(CO2_midbins,sta_fil1_che1_room_ag_ana$pdfs, type="l", col = 'red', ylim = c(0, max(unlist(max_y))), main = expression('Aggregated pdfs of CO'[2]*' concentration '), ylab = 'Probability density', xlab = expression('CO'[2]*' concentration [ppm]'))
lines(CO2_midbins,sta_fil2_che2_room_ag_ana$pdfs, type="l", col = 'blue')
legend("topright", legend = c('Bedroom, Night, Winter', 'Livingroom, Day, Winter'), col = c('red', 'blue'), lwd = 1)


#plot smoothed epdfs
max_y = list()
max_y[1] = max(smooth.spline(CO2_midbins, as.numeric(unlist(sta_fil1_che1_room_ag_ana$pdfs)), spar = 0.4)$y)
max_y[2] = max(smooth.spline(CO2_midbins, as.numeric(unlist(sta_fil2_che2_room_ag_ana$pdfs)), spar = 0.4)$y)
plot(smooth.spline(CO2_midbins, as.numeric(unlist(sta_fil1_che1_room_ag_ana$pdfs)), spar = 0.4), type="l", col = 'red', ylim = c(0, max(unlist(max_y))) ,main = expression('Aggregated smoothed pdfs of CO'[2]*' concentration '), ylab = 'Probability density', xlab = expression('CO'[2]*' concentration [ppm]'))
lines(smooth.spline(CO2_midbins, as.numeric(unlist(sta_fil2_che2_room_ag_ana$pdfs)), spar = 0.4), type="l", col = 'blue')
legend("topright", legend = c('Bedroom, Night, Winter', 'Livingroom, Day, Winter'), col = c('red', 'blue'), lwd = 1)


# provide overview of data source , homes, rooms
META_filter1 <- META_Overview(sta_fil1_che1, meta_h, meta_r)
META_filter1$all_data$homes
META_filter1$all_data$rooms

META_filter2 <- META_Overview(sta_fil2_che2, meta_h, meta_r)
META_filter2$all_data$homes
META_filter2$all_data$rooms


## Step 5.2: group by ventilation type ----------------------
# Step 5.2.1: mechanical ventilation


# sta_mech_vent_ag1 <- sta_fil1_che1_home_ag %>%
#   filter("Mechanical ventilation" %in% vent_type)
# sta_mech_vent_ag2 <- sta_fil2_che2_home_ag %>%
#   filter("Mechanical ventilation" %in% vent_type)

sta_mech_vent_ag1 <- sta_fil1_che1_room_ag %>%
  filter("Mechanical ventilation" %in% vent_type)
sta_mech_vent_ag2 <- sta_fil2_che2_room_ag %>%
  filter("Mechanical ventilation" %in% vent_type)

# group for MVHR and no MVHR
sta_mech_vent_ag1_mvhr <- sta_mech_vent_ag1 %>%
  filter(grepl('mvhr|Mechanical ventilation heat recovery|Duckless|Exhaust and supply air system|ZEHNDER', vent_type_comment, ignore.case = TRUE)) %>%
  mutate(mvhr = 'MVHR')
sta_mech_vent_ag1_nomvhr <- sta_mech_vent_ag1 %>%
  filter(!grepl('mvhr|Mechanical ventilation heat recovery|Duckless|Exhaust and supply air system|ZEHNDER', vent_type_comment, ignore.case = TRUE)) %>%
  mutate(mvhr = 'No MVHR')

sta_mech_vent_ag2_mvhr <- sta_mech_vent_ag2 %>%
  filter(grepl('mvhr|Mechanical ventilation heat recovery|Duckless|Exhaust and supply air system|ZEHNDER', vent_type_comment, ignore.case = TRUE)) %>%
  mutate(mvhr = 'MVHR')
sta_mech_vent_ag2_nomvhr <- sta_mech_vent_ag2 %>%
  filter(!grepl('mvhr|Mechanical ventilation heat recovery|Duckless|Exhaust and supply air system|ZEHNDER', vent_type_comment, ignore.case = TRUE)) %>%
  mutate(mvhr = 'No MVHR')

# All
sta_mech_vent_ag1_ana <- ungroup(sta_mech_vent_ag1) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            nr_homes=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(cdfs), cdf = TRUE)/100,
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

# MVHR
sta_mech_vent_ag1_mvhr_ana <- ungroup(sta_mech_vent_ag1_mvhr) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            mvhr = mvhr,
            nr_homes=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(cdfs), cdf = TRUE)/100,
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins)) %>%
  distinct(.keep_all = TRUE)

# No MVHR
sta_mech_vent_ag1_nomvhr_ana <- ungroup(sta_mech_vent_ag1_nomvhr) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            mvhr = mvhr,
            nr_homes=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(cdfs), cdf = TRUE)/100,
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins)) %>%
  distinct(.keep_all = TRUE)

# All
sta_mech_vent_ag2_ana <- ungroup(sta_mech_vent_ag2) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            nr_homes=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(cdfs), cdf = TRUE)/100,
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

# MVHR
sta_mech_vent_ag2_mvhr_ana <- ungroup(sta_mech_vent_ag2_mvhr) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            mvhr = mvhr,
            nr_homes=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(cdfs), cdf = TRUE)/100,
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins)) %>%
  distinct(.keep_all = TRUE)

#No MVHR
sta_mech_vent_ag2_nomvhr_ana <- ungroup(sta_mech_vent_ag2_nomvhr) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            mvhr = mvhr,
            nr_homes=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(cdfs), cdf = TRUE)/100,
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins)) %>%
  distinct(.keep_all = TRUE)

sta_mech_vent_ag1_ana$vent_type <- "Mechanical ventilation"
sta_mech_vent_ag2_ana$vent_type <- "Mechanical ventilation"
sta_mech_vent_ag1_mvhr_ana$vent_type <- "Mechanical ventilation"
sta_mech_vent_ag2_mvhr_ana$vent_type <- "Mechanical ventilation"
sta_mech_vent_ag1_nomvhr_ana$vent_type <- "Mechanical ventilation"
sta_mech_vent_ag2_nomvhr_ana$vent_type <- "Mechanical ventilation"

# Step 5.2.2: natural ventilation

sta_nat_vent_ag1 <- sta_fil1_che1_room_ag %>%
  filter("Natural ventilation (designed)" %in% vent_type)

sta_nat_vent_ag2 <- sta_fil2_che2_room_ag %>%
  filter("Natural ventilation (designed)" %in% vent_type)

sta_nat_vent_ag1_ana <- ungroup(sta_nat_vent_ag1) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            nr_homes=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(cdfs), cdf = TRUE)/100,
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

sta_nat_vent_ag2_ana <- ungroup(sta_nat_vent_ag2) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            nr_homes=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(cdfs), cdf = TRUE)/100,
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

sta_nat_vent_ag1_ana$vent_type <- "Natural ventilation (designed)"
sta_nat_vent_ag2_ana$vent_type <- "Natural ventilation (designed)"

# Step 5.2.3: hybrid ventilation

sta_hyb_vent_ag1 <- sta_fil1_che1_room_ag %>%
  filter("Hybrid/mixed mode ventilation" %in% vent_type)

sta_hyb_vent_ag2 <- sta_fil2_che2_room_ag %>%
  filter("Hybrid/mixed mode ventilation" %in% vent_type)

sta_hyb_vent_ag1_ana <- ungroup(sta_hyb_vent_ag1) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            nr_homes=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(cdfs), cdf = TRUE)/100,
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

sta_hyb_vent_ag2_ana <- ungroup(sta_hyb_vent_ag2) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            nr_homes=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(cdfs), cdf = TRUE)/100,
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

sta_hyb_vent_ag1_ana$vent_type <- "Hybrid/mixed mode ventilation"
sta_hyb_vent_ag2_ana$vent_type <- "Hybrid/mixed mode ventilation"

# Step 5.2.4: window ventilation 

sta_wind_vent_ag1 <- sta_fil1_che1_room_ag %>%
  filter("Window airing (not designed)" %in% vent_type)

sta_wind_vent_ag2 <- sta_fil2_che2_room_ag %>%
  filter("Window airing (not designed)" %in% vent_type)

sta_wind_vent_ag1_ana <- ungroup(sta_wind_vent_ag1) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            nr_homes=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(cdfs), cdf = TRUE)/100,
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

sta_wind_vent_ag2_ana <- ungroup(sta_wind_vent_ag2) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            nr_homes=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(cdfs), cdf = TRUE)/100,
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

sta_wind_vent_ag1_ana$vent_type <- "Window airing (not designed)"
sta_wind_vent_ag2_ana$vent_type <- "Window airing (not designed)"

sta_vent1 <- sta_mech_vent_ag1_ana
sta_vent1[2,] <- sta_nat_vent_ag1_ana
sta_vent1[3,] <- sta_hyb_vent_ag1_ana
sta_vent1[4,] <- sta_wind_vent_ag1_ana

sta_vent1_mvhr <- sta_mech_vent_ag1_mvhr_ana
sta_vent1_mvhr[2,] <- sta_mech_vent_ag1_nomvhr_ana

sta_vent2 <- sta_mech_vent_ag2_ana
sta_vent2[2,] <- sta_nat_vent_ag2_ana
sta_vent2[3,] <- sta_hyb_vent_ag2_ana
sta_vent2[4,] <- sta_wind_vent_ag2_ana

sta_vent2_mvhr <- sta_mech_vent_ag2_mvhr_ana
sta_vent2_mvhr[2,] <- sta_mech_vent_ag2_nomvhr_ana


## Step 6.2: visualise results ----------------
#colors = c( "blue", "green", "brown", 'black')
lstyle = c(2,3,4,1)

# vent_type ------------

## ggplot style
# Combine data into a single data frame for ggplot
plot_data_vent1 <- data.frame(
  CO2 = rep(CO2_bins, times = nrow(sta_vent1)),    # Repeat CO2 bins for each row in sta_vent1
  cdfs = as.vector(t(sta_vent1$cdfs)),            # Flatten cdfs into a vector
  vent_type = rep(sta_vent1$vent_type, each = length(CO2_bins)), # Repeat vent types for each bin
  nr_homes = rep(sta_vent1$nr_homes, each = length(CO2_bins))    # Repeat number of homes for each bin
)

plot_data_vent2 <- data.frame(
  CO2 = rep(CO2_bins, times = nrow(sta_vent2)),    # Repeat CO2 bins for each row in sta_vent2
  cdfs = as.vector(t(sta_vent2$cdfs)),            # Flatten cdfs into a vector
  vent_type = rep(sta_vent2$vent_type, each = length(CO2_bins)), # Repeat vent types for each bin
  nr_homes = rep(sta_vent2$nr_homes, each = length(CO2_bins))    # Repeat number of homes for each bin
)
# Define custom colors and line styles
colors_ecdf <- c(
  "Mechanical ventilation" = colors[1],
  "Natural ventilation (designed)" = colors[3],
  "Hybrid/mixed mode ventilation" = colors[2],
  "Window airing (not designed)" = colors[4]
)

lstyles <- c(
  "Mechanical ventilation" = "solid",
  "Natural ventilation (designed)" = "dashed",
  "Hybrid/mixed mode ventilation" = "twodash",
  "Window airing (not designed)" = 'dotted'
)

plot_ecdf_CO2_vent_type <- function(sta, colors, lstyle) {
  # Explicitly set factor levels for consistent mapping
  sta$vent_type <- factor(sta$vent_type, levels = names(colors))
  
  # Create dynamic labels based on vent_type and nr_homes
  dynamic_labels <- paste0(
    levels(sta$vent_type),
    ', Nr. homes: ',
    sapply(levels(sta$vent_type), function(vt) {
      unique(sta$nr_homes[sta$vent_type == vt])
    })
  )
  names(dynamic_labels) <- levels(sta$vent_type)  # Ensure labels align with factor levels
  
  g <- ggplot(sta, aes(x = CO2, y = cdfs, color = vent_type, linetype = vent_type)) +
    geom_line(size = 1) +  # Adjust line thickness
    scale_color_manual(
      values = colors,  # Custom color palette
      labels = dynamic_labels  # Dynamic labels
    ) +
    scale_linetype_manual(
      values = lstyles,  # Custom line types
      labels = dynamic_labels  # Dynamic labels
    ) +
    labs(
      x = expression('CO'[2]*' concentration [ppm]'),
      y = 'Cumulative probability [-]',
      color = "Ventilation Type",
      linetype = "Ventilation Type"
    ) +
    theme_minimal() +
    xlim(300, 3500) +  # Set x-axis limits
    theme(
      legend.position = "bottom",   # Position legend below the plot
      legend.box = "horizontal",    # Arrange legend items horizontally
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      legend.spacing.x = unit(0.3, 'cm')  # Adjust spacing between legend items
    ) +
    guides(
      color = guide_legend(nrow = 2),  # Split the color legend into 2 rows
      linetype = guide_legend(nrow = 2)  # Split the linetype legend into 2 rows
    )
  
  return(g)
}

# Create plots
plot_ecdf_CO2_vent_type(plot_data_vent1, colors_ecdf, lstyle)
plot_ecdf_CO2_vent_type(plot_data_vent2, colors_ecdf, lstyle)


## basic plot style
# plot ecdfs
for (i in 1:nrow(sta_vent1)){
  plot(CO2_bins,sta_vent1[i, ]$cdfs, type="l", col = colors[i],
       main = paste0('Ecdf for ', sta_vent1[i, ]$vent_type, ' (', sta_vent1[i, ]$nr_homes, ' homes, bed/night/winter)' ),
       ylab = 'Cumulative probability [-]',
       xlab = expression('CO'[2]*' concentration [ppm]'))
}

for (i in 1:nrow(sta_vent2)){
  plot(CO2_bins,sta_vent2[i, ]$cdfs, type="l", col = colors[i], main = paste0('Ecdf for ', sta_vent2[i, ]$vent_type, ' (', sta_vent2[i, ]$nr_homes, ' homes, liv/day/winter)' ), ylab = 'Cumulative probability [-]', xlab = expression('CO'[2]*' concentration [ppm]'))
}

#plot(CO2_bins, sta_vent1[1, ]$cdfs, type="l", main = paste0('Ecdfs for all ventilation types (', sum(sta_vent1$nr_homes), ' homes overall, bed/night/winter)'), col = colors[1], lwd = 2, xlab = expression('CO'[2]*' concentration [ppm]'), ylab ='Cumulative probability [-]')
plot(CO2_bins, sta_vent1[1, ]$cdfs, type="l", col = colors[1], lwd = 2, xlab = expression('CO'[2]*' concentration [ppm]'), ylab ='Cumulative probability [-]',xlim=c(400,3000))
lines(CO2_bins, sta_vent1[2, ]$cdfs, col = colors[2], lty=lstyle[2], lwd = 2)
lines(CO2_bins, sta_vent1[3, ]$cdfs, col = colors[3], lty=lstyle[3], lwd = 2)
lines(CO2_bins, sta_vent1[4, ]$cdfs, col = colors[4], lty=lstyle[4], lwd = 2)
legend("bottomright", legend = paste0(sta_vent1$vent_type, ', Nr. homes: ', sta_vent1$nr_homes), col = colors, lty=lstyle, lwd = 2)
grid()

#plot(CO2_bins, sta_vent2[1, ]$cdfs, type="l", main = paste0('Ecdfs for all ventilation types (', sum(sta_vent2$nr_homes), ' homes overall, liv/day/winter)'), col = colors[1], lwd = 2, xlab = expression('CO'[2]*' concentration [ppm]'), ylab ='Cumulative probability [-]')
plot(CO2_bins, sta_vent2[1, ]$cdfs, type="l", col = colors[1], lwd = 2, xlab = expression('CO'[2]*' concentration [ppm]'), ylab ='Cumulative probability [-]')
lines(CO2_bins, sta_vent2[2, ]$cdfs, col = colors[2], lwd = 2)
lines(CO2_bins, sta_vent2[3, ]$cdfs, col = colors[3], lwd = 2)
lines(CO2_bins, sta_vent2[4, ]$cdfs,, col = colors[4], lwd = 2)
legend("bottomright", legend = paste0(sta_vent2$vent_type, ', Nr. homes: ', sta_vent2$nr_homes), col = colors, lwd = 2)
grid()

# MVHR ------------

#ggplot style 
plot_data_mvhr1 <- data.frame(
  CO2 = rep(CO2_bins, times = nrow(sta_vent1_mvhr)),
  cdfs = as.vector(t(sta_vent1_mvhr$cdfs)),
  mvhr = rep(sta_vent1_mvhr$mvhr, each = length(CO2_bins)),
  nr_homes = rep(sta_vent1_mvhr$nr_homes, each = length(CO2_bins))
)

plot_data_mvhr2 <- data.frame(
  CO2 = rep(CO2_bins, times = nrow(sta_vent2_mvhr)),
  cdfs = as.vector(t(sta_vent2_mvhr$cdfs)),
  mvhr = rep(sta_vent2_mvhr$mvhr, each = length(CO2_bins)),
  nr_homes = rep(sta_vent2_mvhr$nr_homes, each = length(CO2_bins))
)


plot_ecdf_CO2_mvhr <- function(sta){
  g <- ggplot(sta, aes(x = CO2, y = cdfs, color = mvhr, linetype = mvhr, group = mvhr)) + 
    geom_line(size = 1) +  # Adjust line thickness
    scale_color_manual(
      values = c("darkblue", "lightblue"),  # Use the provided colors
      #labels = paste0(unique(sta$mvhr), ', Nr. homes: ', unique(sta$nr_homes))  # Custom legend labels
      labels = paste0(c("Balanced", "Extracting"), ', Nr. homes: ', unique(sta$nr_homes))  # Custom legend labels
    ) +
    scale_linetype_manual(
      values = c(2,1),  # Use custom line types
      labels = paste0(c("Balanced", "Extracting"), ', Nr. homes: ', unique(sta$nr_homes))  # Custom legend labels
    ) +
    labs(
      #title = paste0('Ecdfs for mechanical ventilation with and without MVHR (', sum(sta_vent1_mvhr$nr_homes), ' homes overall, bed/night/winter)'),
      x = expression('CO'[2]*' concentration [ppm]'),
      y = 'Cumulative probability [-]',
      color = "Mechanical ventilation:",  # Combined legend title for color
      linetype = "Mechanical ventilation:"  # Combined legend title for linetype
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",   # Position legend below the plot
      legend.box = "horizontal",    # Arrange legend items horizontally
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      legend.spacing.x = unit(0.3, 'cm')  # Adjust spacing between legend items
    ) +
    guides(
      color = guide_legend(title = "Mechanical ventilation:"),
      linetype = guide_legend(title = "Mechanical ventilation:")
    )
  
  return(g)
}

plot_ecdf_CO2_mvhr(plot_data_mvhr1)
plot_ecdf_CO2_mvhr(plot_data_mvhr2)


# base plot style
plot(CO2_bins, sta_vent1_mvhr[1, ]$cdfs, type="l", main = paste0('Ecdfs for mechanical ventilation with and without MVHR (', sum(sta_vent1_mvhr$nr_homes), ' homes overall, bed/night/winter)'), col = colors[1], lwd = 2, xlab = expression('CO'[2]*' concentration [ppm]'), ylab ='Cumulative probability [-]')
lines(CO2_bins, sta_vent1_mvhr[2, ]$cdfs, col = colors[2], lwd = 2)
legend("bottomright", legend = paste0(sta_vent1_mvhr$mvhr, ', Nr. homes: ', sta_vent1_mvhr$nr_homes), col = c("red", "blue"), lwd = 2)
grid()

plot(CO2_bins, sta_vent2_mvhr[1, ]$cdfs, type="l", main = paste0('Ecdfs for mechanical ventilation with and without MVHR (', sum(sta_vent2_mvhr$nr_homes), ' homes overall, liv/day/winter)'), col = colors[1], lwd = 2, xlab = expression('CO'[2]*' concentration [ppm]'), ylab ='Cumulative probability [-]')
lines(CO2_bins, sta_vent2_mvhr[2, ]$cdfs, col = colors[2], lwd = 2)
legend("bottomright", legend = paste0(sta_vent2_mvhr$mvhr, ', Nr. homes: ', sta_vent2_mvhr$nr_homes), col = colors, lwd = 2)
grid()

# epdfs ------------------

# plot epdfs
for (i in 1:nrow(sta_vent1)){
  plot(CO2_midbins,sta_vent1[i, ]$pdfs, type="l", main = paste0('Epdf for ', sta_vent1[i, ]$vent_type,' (', sta_vent1[i, ]$nr_homes, ' homes, bed/night/winter)' ), ylab = 'Probability density', xlab = expression('CO'[2]*' concentration [ppm]'))
  lines(smooth.spline(CO2_midbins, as.numeric(unlist(sta_vent1[i, ]$pdfs)), spar = 0.4), col = colors[i], lwd = 2)
  legend("topright", legend = c('calculated epdf', 'smoothed epdf'), col = c('black', colors[i]), lwd = c(1, 2))
}

for (i in 1:nrow(sta_vent2)){
  plot(CO2_midbins,sta_vent2[i, ]$pdfs, type="l", main = paste0('Epdf for ', sta_vent2[i, ]$vent_type,' (', sta_vent2[i, ]$nr_homes, ' homes, liv/day/winter)' ), ylab = 'Probability density', xlab = expression('CO'[2]*' concentration [ppm]'))
  lines(smooth.spline(CO2_midbins, as.numeric(unlist(sta_vent2[i, ]$pdfs)), spar = 0.4), col = colors[i], lwd = 2)
  legend("topright", legend = c('calculated epdf', 'smoothed epdf'), col = c('black', colors[i]), lwd = c(1, 2))
}

# max calc for ylim
max_y = list()
for (i in 1:nrow(sta_vent1)){
  max_y[i] = max(smooth.spline(CO2_midbins, as.numeric(unlist(sta_vent1[i, ]$pdfs)), spar = 0.4)$y)
}

plot(smooth.spline(CO2_midbins, as.numeric(unlist(sta_vent1[1, ]$pdfs)), spar = 0.4), type="l", main = paste0('Smoothed epdfs for all ventilation types (', sum(sta_vent1$nr_homes), ' homes overall, bed/night/winter)'), col = colors[1], ylim = c(0, max(unlist(max_y))), lwd = 2, xlab = expression('CO'[2]*' concentration [ppm]'), ylab = 'Probability density')
lines(smooth.spline(CO2_midbins, as.numeric(unlist(sta_vent1[2, ]$pdfs)), spar = 0.4), col = colors[2], lwd = 2)
lines(smooth.spline(CO2_midbins, as.numeric(unlist(sta_vent1[3, ]$pdfs)), spar = 0.4), col = colors[3], lwd = 2)
lines(smooth.spline(CO2_midbins, as.numeric(unlist(sta_vent1[4, ]$pdfs)), spar = 0.4), col = colors[4], lwd = 2)
legend("topright", legend = sta_vent1$vent_type, col = colors, lwd = 2)
grid()

# max calc for ylim
for (i in 1:nrow(sta_vent2)){
  max_y[i] = max(smooth.spline(CO2_midbins, as.numeric(unlist(sta_vent2[i, ]$pdfs)), spar = 0.4)$y)
}

plot(smooth.spline(CO2_midbins, as.numeric(unlist(sta_vent2[1, ]$pdfs)), spar = 0.4), type="l", main = paste0('Smoothed epdfs for all ventilation types (', sum(sta_vent2$nr_homes), ' homes overall, liv/day/winter)'), col = colors[1], ylim = c(0, max(unlist(max_y))), lwd = 2, xlab = expression('CO'[2]*' concentration [ppm]'), ylab = 'Probability density')
lines(smooth.spline(CO2_midbins, as.numeric(unlist(sta_vent2[2, ]$pdfs)), spar = 0.4), col = colors[2], lwd = 2)
lines(smooth.spline(CO2_midbins, as.numeric(unlist(sta_vent2[3, ]$pdfs)), spar = 0.4), col = colors[3], lwd = 2)
lines(smooth.spline(CO2_midbins, as.numeric(unlist(sta_vent2[4, ]$pdfs)), spar = 0.4), col = colors[4], lwd = 2)
legend("topright", legend = sta_vent2$vent_type, col = colors, lwd = 2)
grid()


# plot MVHR epdfs --------------------

max_y = list()
for (i in 1:nrow(sta_vent1_mvhr)){
  max_y[i] = max(smooth.spline(CO2_midbins, as.numeric(unlist(sta_vent1_mvhr[i, ]$pdfs)), spar = 0.4)$y)
}

plot(smooth.spline(CO2_midbins, as.numeric(unlist(sta_vent1_mvhr[1, ]$pdfs)), spar = 0.4), type="l", main = paste0('Smoothed epdfs for mechanical ventilation with and without MVHR (', sum(sta_vent1_mvhr$nr_homes), ' homes overall, bed/night/winter)'), col = colors[1], ylim = c(0, max(unlist(max_y))), lwd = 2, xlab = expression('CO'[2]*' concentration [ppm]'), ylab = 'Probability density')
lines(smooth.spline(CO2_midbins, as.numeric(unlist(sta_vent1_mvhr[2, ]$pdfs)), spar = 0.4), col = colors[2], lwd = 2)
legend("topright", legend = paste0(sta_vent1_mvhr$mvhr, ', Nr. homes: ', sta_vent1_mvhr$nr_homes), col = c('red', 'blue'), lwd = 2)
grid()

for (i in 1:nrow(sta_vent2_mvhr)){
  max_y[i] = max(smooth.spline(CO2_midbins, as.numeric(unlist(sta_vent2_mvhr[i, ]$pdfs)), spar = 0.4)$y)
}

plot(smooth.spline(CO2_midbins, as.numeric(unlist(sta_vent2_mvhr[1, ]$pdfs)), spar = 0.4), type="l", main = paste0('Smoothed epdfs for mechanical ventilation with and without MVHR (', sum(sta_vent2_mvhr$nr_homes), ' homes overall, liv/day/winter)'), col = colors[1], ylim = c(0, max(unlist(max_y))), lwd = 2, xlab = expression('CO'[2]*' concentration [ppm]'), ylab = 'Probability density')
lines(smooth.spline(CO2_midbins, as.numeric(unlist(sta_vent2_mvhr[2, ]$pdfs)), spar = 0.4), col = colors[2], lwd = 2)
legend("topright", legend = paste0(sta_vent2_mvhr$mvhr, ', Nr. homes: ', sta_vent2_mvhr$nr_homes), col = c('red', 'blue'), lwd = 2)
grid()

# small overview -----------


# provide overview of data source , homes, rooms
print(paste0('Number of homes with mechanical ventilation (bed/night/winter): ', sta_mech_vent_ag1_ana$nr_homes))
print(paste0('Number of homes with natural ventlation (bed/night/winter): ', sta_nat_vent_ag1_ana$nr_homes))
print(paste0('Number of homes with hybrid ventilation (bed/night/winter): ', sta_hyb_vent_ag1_ana$nr_homes))
print(paste0('Number of homes with window ventilation (bed/night/winter): ', sta_wind_vent_ag1_ana$nr_homes))

print(paste0('Number of homes with mechanical ventilation (liv/day/winter): ', sta_mech_vent_ag2_ana$nr_homes))
print(paste0('Number of homes with natural ventlation (liv/day/winter): ', sta_nat_vent_ag2_ana$nr_homes))
print(paste0('Number of homes with hybrid ventilation (liv/day/winter): ', sta_hyb_vent_ag2_ana$nr_homes))
print(paste0('Number of homes with window ventilation (liv/day/winter): ', sta_wind_vent_ag2_ana$nr_homes))


# calculate mean
sta_vent1$Mean <- c(sum(CO2_midbins*sta_vent1[1, ]$pdfs), sum(CO2_midbins*sta_vent1[2, ]$pdfs), sum(CO2_midbins*sta_vent1[3, ]$pdfs), sum(CO2_midbins*sta_vent1[4, ]$pdfs))
sta_vent1$Mean

sta_vent1_mvhr$Mean <- c(sum(CO2_midbins*sta_vent1_mvhr[1, ]$pdfs), sum(CO2_midbins*sta_vent1_mvhr[2, ]$pdfs))
sta_vent1_mvhr$Mean

sta_vent2$Mean <- c(sum(CO2_midbins*sta_vent2[1, ]$pdfs), sum(CO2_midbins*sta_vent2[2, ]$pdfs), sum(CO2_midbins*sta_vent2[3, ]$pdfs), sum(CO2_midbins*sta_vent2[4, ]$pdfs))
sta_vent2$Mean

sta_vent2_mvhr$Mean <- c(sum(CO2_midbins*sta_vent2_mvhr[1, ]$pdfs), sum(CO2_midbins*sta_vent2_mvhr[2, ]$pdfs))
sta_vent2_mvhr$Mean


# calculate sd
sta_vent1$Sd <- c(sqrt(sum((CO2_midbins-sta_vent1[1, ]$Mean)**2*sta_vent1[1, ]$pdfs)), sqrt(sum((CO2_midbins-sta_vent1[2, ]$Mean)**2*sta_vent1[2, ]$pdfs)), sqrt(sum((CO2_midbins-sta_vent1[3, ]$Mean)**2*sta_vent1[3, ]$pdfs)), sqrt(sum((CO2_midbins-sta_vent1[4, ]$Mean)**2*sta_vent1[4, ]$pdfs)))
sta_vent1$Sd

sta_vent1_mvhr$Sd <- c(sqrt(sum((CO2_midbins-sta_vent1_mvhr[1, ]$Mean)**2*sta_vent1_mvhr[1, ]$pdfs)), sqrt(sum((CO2_midbins-sta_vent1[2, ]$Mean)**2*sta_vent1[2, ]$pdfs)))
sta_vent1_mvhr$Sd

sta_vent2$Sd <- c(sqrt(sum((CO2_midbins-sta_vent2[1, ]$Mean)**2*sta_vent2[1, ]$pdfs)), sqrt(sum((CO2_midbins-sta_vent2[2, ]$Mean)**2*sta_vent2[2, ]$pdfs)), sqrt(sum((CO2_midbins-sta_vent2[3, ]$Mean)**2*sta_vent2[3, ]$pdfs)), sqrt(sum((CO2_midbins-sta_vent2[4, ]$Mean)**2*sta_vent2[4, ]$pdfs)))
sta_vent2$Sd

sta_vent2_mvhr$Sd <- c(sqrt(sum((CO2_midbins-sta_vent2_mvhr[1, ]$Mean)**2*sta_vent2_mvhr[1, ]$pdfs)), sqrt(sum((CO2_midbins-sta_vent2[2, ]$Mean)**2*sta_vent2[2, ]$pdfs)))
sta_vent2_mvhr$Sd


## Outside temperature -------------------------

# filter ambient T from annex excel sheets
T_out <- stats %>%
  filter(variable == 'T',  room == 'AMB') %>%
  group_by(date, ID_h) %>% 
  summarise(avg_T = mean(Mean), study = unique(study))

# omit all outside temps for date == NA (aggregated)
T_out <- na.omit(T_out)

T_out <- T_out %>%
  ungroup()

# load in external ambient T from wonderground
T_out_extern <- read.csv("Annex86_AP3_data/T_out.csv")
T_out_extern <- T_out_extern %>%
  filter(Study != 'Lueftung3')

T_out_ClimateReady <- read.csv("Annex86_AP3_data/T_Out_wunderground_ClimateReady.csv")
T_out_ClimateReady$Date <- as.Date(paste0(T_out_ClimateReady$Date, "-01"), format = '%Y-%m-%d')

#half an hour data from wunderground Pamplona airport
T_out_Pamplona_hourly <- read.csv("Annex86_AP3_data/T_Out_wunderground_Pamplona_half_hour.csv")
T_out_Pamplona_hourly$Date <- as.POSIXct(T_out_Pamplona_hourly$Date, format = "%Y-%m-%d %H:%M:%S")

T_out_Pamplona_hourly <- T_out_Pamplona_hourly %>%
  mutate(year_month = format(Date, "%Y-%m"))

T_out_Pamplona <- T_out_Pamplona_hourly %>%
  group_by(year_month) %>%
  summarise(AvgT = mean(T))

T_out_Pamplona$Date <- as.Date(paste(T_out_Pamplona$year_month, "01", sep = "-"), format = "%Y-%m-%d")


## Historic temperature data from airports to compare 

T_out_comp <- read.csv("Annex86_AP3_data/T_Out_wunderground_comparison.csv")
T_out_comp$Date <- as.Date(paste0(T_out_comp$Date, "-01"), format = '%Y-%m-%d')

# load in external ambient T from ZAMG for Lueftung3
T_out_Lueftung3 <- read.csv('Annex86_AP3_data/Lueftung3_TAMB_201010_201312.csv')
# Convert 'time' to datetime and extract year-month
T_out_Lueftung3 <- T_out_Lueftung3 %>%
  mutate(
    time = gsub("\\+00:00", "", time),
    time = gsub("T", " ", time),
    time = ymd_hm(time),
    Date = format(time, "%Y-%m")
  )

# read in station city mapping
station_ID_Synop_Lueftung3 <- read.csv('Annex86_AP3_data/Lueftung3_Stationen_ZAMG.txt', sep = ':')
station_city_mapping <- station_ID_Synop_Lueftung3 %>%
  group_by(station) %>%
  summarize(City = paste(City, collapse = "; "))

# Group by station and Date, then calculate the mean temperature
monthly_avg_lueftung<- T_out_Lueftung3 %>%
  group_by(station, Date) %>%
  summarize(AvgT = mean(T, na.rm = TRUE), .groups = "drop",
            Study = 'Lueftung3',
            Country = 'at') %>%
  left_join(station_city_mapping, by = "station")

monthly_avg_lueftung_expanded <- monthly_avg_lueftung %>%
  separate_rows(City, sep = "; ") %>%
  # Remove any leading/trailing whitespace that might have been created
  mutate(City = trimws(City))

T_out_extern <- bind_rows(T_out_extern, monthly_avg_lueftung_expanded[,-1])

## read in supplied external temperature data
# CISC
T_out_CISC <- read_excel("Annex86_AP3_data/ESP/CISC/raw/T_outdoor/datos retirto completos temperaturas.xlsx", col_names = FALSE)
colnames(T_out_CISC) <- T_out_CISC[2, ]
T_out_CISC <- T_out_CISC[-c(1, 2), ]
T_out_CISC$date <- as.Date(T_out_CISC$date, format = "%d/%m/%Y")
T_out_CISC <- T_out_CISC %>%
  mutate(across(starts_with("t_"), ~ as.numeric(gsub(",", ".", .))))
T_out_CISC <- T_out_CISC %>%
  mutate(Month = format(date, "%Y-%m")) %>% # Extract year-month
  group_by(Month) %>% # Group by month
  summarise(across(starts_with("t_"), mean, na.rm = TRUE)) # Calculate mean
# NL_ML
T_out_NLML <- read_excel("Annex86_AP3_data/NLD/NL_ML/raw/T_outdoor/Outdoor data NL.xlsx")
T_out_NLML <- T_out_NLML[-c(1, 2, 3, 4), ]
T_out_NLML$Data <- as.Date(T_out_NLML$Data, format = "%Y%m%d")
T_out_NLML <- T_out_NLML %>%
  mutate(Month = format(Data, "%Y-%m")) %>% # Extract year-month
  group_by(Month) %>% # Group by month
  summarise(across(`Daily mean temperature in (degrees Celsius)`, mean, na.rm = TRUE)) # Calculate mean
colnames(T_out_NLML)[2] <- "monthly T mean"
# ALLO
T_out_ALLO <- read.csv("Annex86_AP3_data/FRA/raw/T_outdoor/ALLO_Monthly_Temp_2020.csv")
T_out_ALLO <- rbind(T_out_ALLO, read.csv("Annex86_AP3_data/FRA/raw/T_outdoor/ALLO_Monthly_Temp_2021.csv"))
T_out_ALLO <- rbind(T_out_ALLO, read.csv("Annex86_AP3_data/FRA/raw/T_outdoor/ALLO_Monthly_Temp_2022.csv"))
T_out_ALLO$TIMESTAMP <- as.Date(paste0(T_out_ALLO$TIMESTAMP, "/01"), format = '%m/%Y/%d')

dates_CISC <- as.Date(paste0(T_out_extern$Date[T_out_extern$Study == 'CISC'], "-01"), format = '%Y-%m-%d')
values_CISC <- T_out_extern$AvgT[T_out_extern$Study == 'CISC']
gaps <- c(FALSE, diff(dates_CISC) > 31)
dates_CISC[gaps] <- NA
values_CISC[gaps] <- NA



# compare supplied external data with wonderground
plot(dates_CISC,values_CISC, type = "o", xlab = "Date", ylab = "T", main = 'Ambient temperature comparison CISC')
lines(as.Date(paste0(T_out_CISC$Month, "-01"), format = '%Y-%m-%d'), T_out_CISC$t_median, col = 'red')
legend("topright", 
       legend = c("wonderground", "externally supplied"), 
       col = c("black", "red"), 
       lty = 1, 
       lwd = 2)

plot(as.Date(paste0(T_out_extern$Date[T_out_extern$Study == 'NL_ML'], "-01"), format = '%Y-%m-%d'),T_out_extern$AvgT[T_out_extern$Study == 'NL_ML'], type = "o", xlab = "Date", ylab = "T", main = 'Ambient temperature comparison NL_ML')
lines(as.Date(paste0(T_out_NLML$Month, "-01"), format = '%Y-%m-%d'), T_out_NLML$`monthly T mean`, col = 'red')
legend("bottomright", 
       legend = c("wonderground", "externally supplied"), 
       col = c("black", "red"), 
       lty = 1, 
       lwd = 2)

plot(as.Date(T_out_comp$Date[T_out_comp$Study == 'MX1'], format = '%Y-%m-%d'),T_out_comp$AvgT[T_out_comp$Study == 'MX1'], type = "o", xlab = "Date", ylab = "T", main = 'Ambient temperature comparison MX1')
lines(as.Date(T_out$date[T_out$study == 'MX1'], format = '%Y-%m-%d'), T_out$avg_T[T_out$study == 'MX1'], col = 'red')
legend("bottomleft", 
       legend = c("wonderground", "from Annex"), 
       col = c("black", "red"), 
       lty = 1, 
       lwd = 2)

plot(as.Date(T_out_comp$Date[T_out_comp$Study == 'Lodenareal'], format = '%Y-%m-%d'),T_out_comp$AvgT[T_out_comp$Study == 'Lodenareal'], type = "o", xlab = "Date", ylab = "T", main = 'Ambient temperature comparison Lodenareal')
lines(as.Date(T_out$date[T_out$study == 'Lodenareal'], format = '%Y-%m-%d'), T_out$avg_T[T_out$study == 'Lodenareal'], col = 'red')
legend("bottomleft", 
       legend = c("wonderground", "from Annex"), 
       col = c("black", "red"), 
       lty = 1, 
       lwd = 2)

ggplot(T_out[T_out$study == 'VentStdStudy' & grepl("H_01|H_02|H_03|H_04|H_10|H_11|H_12|H_13|H_14|H_16", T_out$ID_h), ], aes(x = date, y = avg_T, color = ID_h, group = ID_h)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Temperature trends for each home in Glasgow",
    x = "Date",
    y = "Temperature (°C)"
  ) +
  theme_minimal() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  geom_line(data = T_out_comp[T_out_comp$City == 'Glasgow' & T_out_comp$Study == 'VentStdStudy' & T_out_comp$Date %in% unique(T_out$date[T_out$study == 'VentStdStudy']), ],
            aes(x = Date, y = AvgT),
            inherit.aes = FALSE,  # Prevents the layer from inheriting global aesthetics
            color = "black",      # Ensures the line has a specific color
            linetype = "dashed")  # Optionally use a dashed line to differentiate

ggplot(T_out[T_out$study == 'VentStdStudy' & grepl("H_05|H_06|H_07|H_08|H_09", T_out$ID_h), ], aes(x = date, y = avg_T, color = ID_h, group = ID_h)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Temperature trends for each home in Edinburgh",
    x = "Date",
    y = "Temperature (°C)"
  ) +
  theme_minimal() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  geom_line(data = T_out_comp[T_out_comp$City == 'Edinburgh' & T_out_comp$Study == 'VentStdStudy' & T_out_comp$Date %in% unique(T_out$date[T_out$study == 'VentStdStudy']), ],
            aes(x = Date, y = AvgT),
            inherit.aes = FALSE,  # Prevents the layer from inheriting global aesthetics
            color = "black",      # Ensures the line has a specific color
            linetype = "dashed")  # Optionally use a dashed line to differentiate



ggplot(T_out[T_out$study == 'ClimateReady', ], aes(x = date, y = avg_T, color = ID_h, group = ID_h)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Temperature Trends for each home in Pamplona",
    x = "Date",
    y = "Temperature (°C)"
  ) +
  theme_minimal() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  #geom_line(data = T_out_ClimateReady[T_out_ClimateReady$City == 'Pamplona' & T_out_ClimateReady$Date %in% unique(T_out$date[T_out$study == 'ClimateReady']), ],
  #          aes(x = Date, y = AvgT),
  #          inherit.aes = FALSE,  # Prevents the layer from inheriting global aesthetics
  #          color = "black",      # Ensures the line has a specific color
  #          linetype = "dashed") +  # Optionally use a dashed line to differentiate
  geom_line(data = T_out_Pamplona[T_out_Pamplona$Date %in% unique(T_out$date[T_out$study == 'ClimateReady']), ],
            aes(x = Date, y = AvgT),
            inherit.aes = FALSE,  # Prevents the layer from inheriting global aesthetics
            color = "black",      # Ensures the line has a specific color
            linetype = "dotted")  # Optionally use a dashed line to differentiate




# replace supplied external data with wonderground data
T_out_extern <- T_out_extern %>%
  filter(Study != 'CISC')
T_out_CISC <- T_out_CISC %>%
  mutate(City = "Madrid", Study = "CISC", Country = "es")
T_out_extern <- bind_rows(
  T_out_extern,
  T_out_CISC %>% select(Date = Month, AvgT = t_median, City, Study, Country)
)

T_out_extern <- T_out_extern %>%
  filter(Study != 'NL_ML')
T_out_NLML <- T_out_NLML %>%
  mutate(City = "Roosendaal", Study = "NL_ML", Country = "nl")
T_out_extern <- bind_rows(
  T_out_extern,
  T_out_NLML %>% select(Date = Month, AvgT = `monthly T mean`, City, Study, Country)
)
T_out_ALLO$TIMESTAMP <- format(T_out_ALLO$TIMESTAMP, "%Y-%m")
T_out_extern <- T_out_extern %>%
  filter(Study != 'ALLO')
T_out_ALLO <- T_out_ALLO %>%
  mutate(City = "La Rochelle", Study = "ALLO", Country = "fr")
T_out_extern <- bind_rows(
  T_out_extern,
  T_out_ALLO %>% select(Date = TIMESTAMP, AvgT = Monthly_Temp, City, Study, Country)
)

T_out_Pamplona$Date <- format(T_out_Pamplona$Date, "%Y-%m")
T_out_Pamplona <- T_out_Pamplona %>%
  mutate(City = "Pamplona", Study = "ClimateReady", Country = "es")
T_out_Pamplona <- T_out_Pamplona %>%
  select(-year_month)
T_out_extern <- bind_rows(
  T_out_extern,
  T_out_Pamplona %>% select(Date = Date, AvgT = AvgT, City, Study, Country)
)

T_out_ClimateReady$Date <- format(T_out_ClimateReady$Date, "%Y-%m")
T_out_ClimateReady <- T_out_ClimateReady %>%
  select(-AirportID)
T_out_extern <- bind_rows(
  T_out_extern,
  T_out_ClimateReady[T_out_ClimateReady$City == 'Sevilla', ] %>% select(Date = Date, AvgT = AvgT, City, Study, Country)
)

T_out_extern <- T_out_extern %>%
  filter(!(Date == '2021-05' & Study == 'ClimateReady'))


## mapping external data to original T_out dataframe 
# First create the mapping between city/study and ID_h
id_mapping <- meta_h %>%
  select(ID = ID, City = `Location: City`, study) %>%
  filter(!is.na(ID))

## TODO: Put result into a function
# Create all combinations at once
result <- T_out_extern %>%
  # Convert date once for all rows
  mutate(date = as.Date(paste(Date, "-01", sep = ""), format = "%Y-%m-%d")) %>%
  # Join with the ID mapping to get all relevant IDs for each city/study combo
  left_join(id_mapping, 
            by = c("City" = "City", "Study" = "study"),
            relationship = "many-to-many") %>%
  # Select only the columns we need for the final output
  select(date, ID_h = ID, avg_T = AvgT, study = Study) %>%
  # Remove any rows where no ID was found
  filter(!is.na(ID_h))

# Combine with existing T_out data
T_out <- bind_rows(T_out, result)

missing_matches <- T_out_extern %>%
  anti_join(id_mapping, by = c("City" = "City", "Study" = "study")) %>%
  rownames()

print(missing_matches)



## calculating mean temperature during the night
# Filter data for nighttime hours (23:00 to 07:00)
monthly_avg_lueftung_nighttime <- T_out_Lueftung3 %>%
  # Extract the hour from the Date-Time column (assuming it is in POSIXct or POSIXlt format)
  mutate(Hour = format(time, "%h")) %>%
  # Keep only rows where Hour is between 23 and 7
  filter(Hour >= 23 | Hour <= 7) %>%
  group_by(station, Date) %>%
  summarize(
    AvgT = mean(T, na.rm = TRUE),
    .groups = "drop",
    Study = 'Lueftung3',
    Country = 'at'
  ) %>%
  left_join(station_city_mapping, by = "station")

# Expand cities if multiple cities are listed per station
monthly_avg_lueftung_nighttime_expanded <- monthly_avg_lueftung_nighttime %>%
  separate_rows(City, sep = "; ") %>%
  mutate(City = trimws(City))  # Remove any extra whitespace

# Create all combinations at once
result <- monthly_avg_lueftung_nighttime_expanded %>%
  # Convert date once for all rows
  mutate(date = as.Date(paste(Date, "-01", sep = ""), format = "%Y-%m-%d")) %>%
  # Join with the ID mapping to get all relevant IDs for each city/study combo
  left_join(id_mapping, 
            by = c("City" = "City", "Study" = "study"),
            relationship = "many-to-many") %>%
  # Select only the columns we need for the final output
  select(date, ID_h = ID, avg_T = AvgT, study = Study) %>%
  # Remove any rows where no ID was found
  filter(!is.na(ID_h))

##Temperature dataframe with averaged values for Lueftung3 during the night
T_out_nighttime <- data.frame(result)


## create T_out_minT
T_out_minT <- read.csv("Annex86_AP3_data/T_out_MinT.csv")

# Create all combinations at once
result <- T_out_minT %>%
  # Convert date once for all rows
  mutate(date = as.Date(paste(Date, "-01", sep = ""), format = "%Y-%m-%d")) %>%
  # Join with the ID mapping to get all relevant IDs for each city/study combo
  left_join(id_mapping, 
            by = c("City" = "City", "Study" = "study"),
            relationship = "many-to-many") %>%
  # Select only the columns we need for the final output
  select(date, ID_h = ID, min_T_max = MinTMax, min_T_avg = MinTAvg, min_T_min = MinTMin, study = Study) %>%
  # Remove any rows where no ID was found
  filter(!is.na(ID_h))

# Temperature dataframe from wonderground with minimum temperatures (Max, Avg, Min)
T_out_minT <- data.frame(result)
T_out_minT <- T_out_minT %>%
  select(-min_T_max, -min_T_min) %>%
  filter(study != 'VentStdStudy')
colnames(T_out_minT)[3] <- 'avg_T'
# filter ambient T from annex excel sheets and take p05
tmp <- stats %>%
  filter(variable == 'T',  room == 'AMB') %>%
  group_by(date, ID_h) %>% 
  summarise(avg_T = mean(p05), study = unique(study))
tmp <- na.omit(tmp)
T_out_minT <- rbind(T_out_minT, tmp)

final_T_out_minT <- merge(sta_fil3_ag_AMBT, T_out_minT, by = c('date', 'ID_h'))
final_T_out_minT <- final_T_out_minT %>%
  select(-study.y) %>%
  rename(study = study.x)



#which(is.na(sta_fil3$interval_Max))
# drop faulty data, which give error in cdf2per (don't know why or where it comes from)
#sta_fil3 <- sta_fil3 %>% drop_na(interval_Max)
# over all ID_h and date to get one entry per home per date 
sta_fil3_ag_AMBT <- sta_fil3_che3 %>%
  group_by(ID_h, date) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~weighted.mean(.x, N_rel, na.rm=TRUE)),
            across(study),
            across(vent_type),
            across(home_size, ~as.numeric(.x)),
            across(room_vol),
            across(room_area),
            across(country),
            N_count = 1,
            across(vent_rate_home:vent_rate_room),
            cdfs=mix_cdf_CO2(pick(p00:p100),N_rel,cdf = FALSE),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins)) %>%
  distinct(.keep_all = TRUE)

print(paste0('There are ', nrow(subset(sta_fil3_ag_AMBT, is.na(vent_type))), ' homes with missing ventilation type'))

# match the amb t to the ID_h's, however a bit messy, as the amb t in Lodenareal gets the ID_h 'General'
T_out_Loden <- T_out[grepl('Lodenareal', T_out$ID_h), ]
Loden_df <- merge(sta_fil3_ag_AMBT, T_out_Loden, by = c('date', 'study'))
merged_df <- merge(sta_fil3_ag_AMBT, T_out, by = c('date', 'ID_h'))

cols_to_add <- setdiff(names(merged_df), names(Loden_df))
Loden_df[cols_to_add] <- NA

cols_to_add <- setdiff(names(Loden_df), names(merged_df))
merged_df[cols_to_add] <- NA

Loden_df <- Loden_df[, names(merged_df)]
final_T_out <- rbind(Loden_df, merged_df)

# final_T_out cosmetics (add ID_h and study columns together)
# Combine the ID_h and study columns, removing NA values
final_T_out$ID_h_combined <- ifelse(is.na(final_T_out$ID_h), final_T_out$ID_h.x, 
                                 ifelse(is.na(final_T_out$ID_h.x), final_T_out$ID_h, 
                                        paste0(final_T_out$ID_h, final_T_out$ID_h.x)))

final_T_out$study_combined <- ifelse(is.na(final_T_out$study), final_T_out$study.x, 
                                  ifelse(is.na(final_T_out$study.x), final_T_out$study, 
                                         paste0(final_T_out$study, final_T_out$study.x)))
# Remove the redundant columns
final_T_out <- final_T_out %>%
  select(-ID_h, -ID_h.x, -ID_h.y, -study, -study.x, -study.y)

# Rename the combined columns to original names if needed
final_T_out <- final_T_out %>%
  rename(ID_h = ID_h_combined, study = study_combined)


## mechanically vented
final_T_out_mech <- final_T_out %>%
  filter("Mechanical ventilation" == vent_type)

## window vented
final_T_out_wind <- final_T_out %>%
  filter("Window airing (not designed)" == vent_type)

## hybrid vented
final_T_out_hyb <- final_T_out %>%
  filter("Hybrid/mixed mode ventilation" == vent_type)

## naturally vented
final_T_out_nat <- final_T_out %>%
  filter("Natural ventilation (designed)" == vent_type)

## non-mechanically vented
final_T_out_non_mech <- final_T_out %>%
  filter("Window airing (not designed)" == vent_type | 
          "Natural ventilation (designed)" == vent_type |
          "Hybrid/mixed mode ventilation" == vent_type )

## mechanically vented
final_T_out_minT_mech <- final_T_out_minT %>%
  filter("Mechanical ventilation" == vent_type)

## window vented
final_T_out_minT_wind <- final_T_out_minT %>%
  filter("Window airing (not designed)" == vent_type)

## hybrid vented
final_T_out_minT_hyb <- final_T_out_minT %>%
  filter("Hybrid/mixed mode ventilation" == vent_type)

## naturally vented
final_T_out_minT_nat <- final_T_out_minT %>%
  filter("Natural ventilation (designed)" == vent_type)

## non-mechanically vented
final_T_out_minT_non_mech <- final_T_out_minT %>%
  filter("Window airing (not designed)" == vent_type | 
           "Natural ventilation (designed)" == vent_type |
           "Hybrid/mixed mode ventilation" == vent_type )


#trying out different fits
x <- subset(final_T_out, select = c(Mean, avg_T))
tmp <- data.frame(avg_T = seq(min(x$avg_T), max(x$avg_T), length.out = 201))

lm <- lm(Mean ~ avg_T, data = final_T_out)
fit_lm <- predict(lm, newdata = tmp)

g1 <- gamlss2(Mean ~ s(avg_T, k = 20), data = x)
fit_g1 <- predict(g1, newdata = tmp)$mu

g2 <- gamlss2(Mean ~ s(avg_T, k = 20) | s(avg_T, k = 20), data = x)
fit_g2 <- predict(g2, newdata = tmp)$mu

fit_l1 <- predict(loess(Mean ~ avg_T, span = 0.75, data = x), newdata = tmp)
fit_l2 <- predict(loess(Mean ~ avg_T, span = 1.00, data = x), newdata = tmp)
fit_l3 <- predict(loess(Mean ~ avg_T, span = 5.00, data = x), newdata = tmp)

y <- rbind(data.frame(model = "lm", avg_T = tmp, mu = fit_lm),
           data.frame(model = "het gamlss2", avg_T = tmp, mu = fit_g1),
           data.frame(model = "hom gamlss2", avg_T = tmp, mu = fit_g2),
           data.frame(model = "loess 0.75", avg_T = tmp, mu = fit_l1),
           data.frame(model = "loess 1.00", avg_T = tmp, mu = fit_l2),
           data.frame(model = "loess 5.00", avg_T = tmp, mu = fit_l2))

gg1 <- ggplot() +
  geom_line(aes(x = avg_T, y = mu, group = model, color = model), data = y, linewidth = 0.75) +
  theme_minimal()
gg1

gg2 <- ggplot() +
  geom_point(aes(x = avg_T, y = Mean), data = x) +
  geom_line(aes(x = avg_T, y = mu, group = model, color = model), data = y, linewidth = 0.75) +
  theme_minimal()

gg2


print(paste0('There are ', sum(final_T_out$N_count), ' different combinations of dates and homes with outside temperatures'))

# plot scatterplot function

plot_T_scatterplot <- function(data, y, subset, method = NULL, studies = FALSE, linreg = TRUE){
  if (y == 'mean'){
    ggplot(data = data, aes(x = avg_T, y = Mean, color = factor(study))) +
      geom_point(size = 1) +          
      geom_smooth(aes(group = 1),method = method, se = TRUE) +
      {if(linreg)geom_smooth(aes(group = 1),method = "lm", se = TRUE, color = 'red')} +
      labs(#title = bquote(.(sprintf("Ambient T versus CO[2] per home (%s)", subset))),
           x = "Outside Temperature [°C]",
           y = expression("CO"[2]*" concentration [ppm]")) +
      {if(studies)facet_wrap(~ study)}+
      ggtitle(bquote("Ambient T versus CO"[2]*" per home (" ~ .(subset) ~ ") (mean)"))+
      theme_minimal()
  }else if(y == 'p95'){
    ggplot(data = data, aes(x = avg_T, y = percs[[98]], color = factor(study))) +
      geom_point(size = 1) +          
      geom_smooth(aes(group = 1),method = method, se = TRUE) +
      {if(linreg)geom_smooth(aes(group = 1),method = "lm", se = TRUE, color = 'red')} +
      labs(#title = bquote(.(sprintf("Ambient T versus CO[2] per home (%s)", subset))),
           x = "Outside Temperature [°C]",
           y = expression("CO"[2]*" concentration [ppm]")) + 
      {if(studies)facet_wrap(~ study)}+
      ggtitle(bquote("Ambient T versus CO"[2]*" per home ("~.(subset)~") (p95)"))+
      theme_minimal()
  }else{
    print('Not a valid y value, try again')
  }
}

# all
plot_T_scatterplot(final_T_out,'mean','all','loess')
plot_T_scatterplot(final_T_out,'p95','all','loess')
plot_T_scatterplot(final_T_out_minT,'mean','all','loess')
plot_T_scatterplot(final_T_out_minT,'p95','all','loess')

# mechanically vented
plot_T_scatterplot(final_T_out_mech,'mean','mech vent','loess')
plot_T_scatterplot(final_T_out_mech,'p95','mech vent','loess')
plot_T_scatterplot(final_T_out_minT_mech,'mean','mech vent','loess')
plot_T_scatterplot(final_T_out_minT_mech,'p95','mech vent','loess')

# naturally vented
#plot_T_scatterplot(final_T_out_nat,'mean','mech vent','loess')
#plot_T_scatterplot(final_T_out_nat,'p95','mech vent','loess')
#plot_T_scatterplot(final_T_out_minT_nat,'mean','mech vent','loess')
#plot_T_scatterplot(final_T_out_minT_nat,'p95','mech vent','loess')

# hybrid vented
#plot_T_scatterplot(final_T_out_hyb,'mean','mech vent','loess')
#plot_T_scatterplot(final_T_out_hyb,'p95','mech vent','loess')
#plot_T_scatterplot(final_T_out_minT_hyb,'mean','mech vent','loess')
#plot_T_scatterplot(final_T_out_minT_hyb,'p95','mech vent','loess')

# window vented
#plot_T_scatterplot(final_T_out_wind,'mean','mech vent','loess')
#plot_T_scatterplot(final_T_out_wind,'p95','mech vent','loess')
#plot_T_scatterplot(final_T_out_minT_wind,'mean','mech vent','loess')
#plot_T_scatterplot(final_T_out_minT_wind,'p95','mech vent','loess')

# non-mechanically vented
plot_T_scatterplot(final_T_out_non_mech,'mean','nat vent','loess')
plot_T_scatterplot(final_T_out_non_mech,'p95','nat vent','loess')
plot_T_scatterplot(final_T_out_minT_non_mech,'mean','nat vent','loess')
plot_T_scatterplot(final_T_out_minT_non_mech,'p95','nat vent','loess')

#studies
plot_T_scatterplot(final_T_out, 'mean', 'all', 'loess', studies = TRUE, linreg = FALSE)
plot_T_scatterplot(final_T_out, 'p95', 'all', 'loess', studies = TRUE, linreg = FALSE)

plot_T_scatterplot(final_T_out_mech, 'mean', 'mech vent', 'loess', studies = TRUE, linreg = FALSE)
plot_T_scatterplot(final_T_out_mech, 'p95', 'mech vent', 'loess', studies = TRUE, linreg = FALSE)

plot_T_scatterplot(final_T_out_non_mech, 'mean', 'non-mech vent', 'gam', studies = TRUE, linreg = FALSE)
plot_T_scatterplot(final_T_out_non_mech, 'p95', 'non-mech vent', 'gam', studies = TRUE, linreg = FALSE)


# plot scatterplot (mechanically vented)
ggplot(data = final_T_out_mech, aes(x = avg_T, y = percs[[98]], color = factor(study))) +
  geom_point(size = 1) +          
  geom_smooth(aes(group = 1),method = "loess", se = TRUE) +
  labs(title = expression("Ambient T versus vs mechanically vented CO"[2]*" per home"),
       x = "Outside Temperature [°C]",
       y = expression("CO"[2]*" concentration [ppm]")) +     
  theme_minimal()

print(paste0('There are ', sum(final_T_out_mech$N_count), ' different combinations of dates and homes with outside temperatures for mechanically ventilated homes'))


# plot scatterplot (window vented)
ggplot(data = final_T_out_wind, aes(x = avg_T, y = Mean, color = factor(study))) +
  geom_point(size = 1) +          
  geom_smooth(aes(group = 1),method = "loess", se = TRUE) +
  labs(title = expression("Ambient T versus vs window vented CO"[2]*" per home"),
       x = "Outside Temperature [°C]",
       y = expression("CO"[2]*" concentration [ppm]")) +     
  theme_minimal()

print(paste0('There are ', sum(final_T_out_wind$N_count), ' different combinations of dates and homes with outside temperatures for window ventilated homes'))


# plot scatterplot (non-mechanically vented)
ggplot(data = final_T_out_non_mech, aes(x = avg_T, y = Mean, color = factor(study))) +
  geom_point(size = 1) +          
  geom_smooth(aes(group = 1),method = "loess", se = TRUE) +
  labs(title = expression("Ambient T versus vs non-mechanically vented CO"[2]*" per home"),
       x = "Outside Temperature [°C]",
       y = expression("CO"[2]*" concentration [ppm]")) +     
  theme_minimal()

print(paste0('There are ', sum(final_T_out_mech$N_count), ' different combinations of dates and homes with outside temperatures for non-mechanically ventilated homes'))












# ambient T categories

final_T_out <- final_T_out %>%
  mutate(temp_category = case_when(
    avg_T < 0 ~ "< 0",
    avg_T < 5 ~ "0-5",
    avg_T < 10 ~ "5-10",
    avg_T < 15 ~ "10-15",
    avg_T < 20 ~ "15-20",
    avg_T < 25 ~ "20-25",
    avg_T < 30 ~ "25-30",
    avg_T > 30 ~ "> 30"
  ))

# create pdfs for each temp category
final_T_out_cat <- final_T_out %>%
  group_by(temp_category) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(cdfs), cdf = TRUE)/100,
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

correct_order <- c("< 0", "0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "> 30")

# Use mutate() to convert temp_cat to a factor with the specified order
final_T_out_cat_ordered <- final_T_out_cat %>%
  mutate(temp_category = factor(temp_category, levels = correct_order, ordered = TRUE)) %>%
  arrange(temp_category)

max_y <- list()
for (i in 1:nrow(final_T_out_cat_ordered)){
  max_y[i] = max(smooth.spline(CO2_midbins, as.numeric(unlist(final_T_out_cat_ordered[i, ]$pdfs)), spar = 0.4)$y)
}

# TODO: add way to account for different number of categories

# plot ecdfs

colors = c( "red", "blue", "green", 'yellow', 'orange', 'purple', 'cyan', 'magenta' )

plot(CO2_bins, final_T_out_cat_ordered[1, ]$cdfs, type="l", main = paste0('ecdfs for temp [°C] categories'), col = colors[1], xlab = expression('CO'[2]*' concentration [ppm]'), ylab = 'Cumulative probability [-]')
lines(CO2_bins, final_T_out_cat_ordered[2, ]$cdfs, col = colors[2])
lines(CO2_bins, final_T_out_cat_ordered[3, ]$cdfs, col = colors[3])
lines(CO2_bins, final_T_out_cat_ordered[4, ]$cdfs, col = colors[4])
lines(CO2_bins, final_T_out_cat_ordered[5, ]$cdfs, col = colors[5])
lines(CO2_bins, final_T_out_cat_ordered[6, ]$cdfs, col = colors[6])
lines(CO2_bins, final_T_out_cat_ordered[7, ]$cdfs, col = colors[7])
lines(CO2_bins, final_T_out_cat_ordered[8, ]$cdfs, col = colors[8])
legend("bottomright", legend = correct_order, col = colors, lwd = 2)
grid()

# plot epdfs
plot(smooth.spline(CO2_midbins, as.numeric(unlist(final_T_out_cat_ordered[1, ]$pdfs)), spar = 0.4), type="l", ylim = c(0, max(unlist(max_y))), main = paste0('epdfs for temp [°C] categories'), col = colors[1], xlab = expression('CO'[2]*' concentration [ppm]'), ylab = 'Probability density')
lines(smooth.spline(CO2_midbins, as.numeric(unlist(final_T_out_cat_ordered[2, ]$pdfs)), spar = 0.4), col = colors[2])
lines(smooth.spline(CO2_midbins, as.numeric(unlist(final_T_out_cat_ordered[3, ]$pdfs)), spar = 0.4), col = colors[3])
lines(smooth.spline(CO2_midbins, as.numeric(unlist(final_T_out_cat_ordered[4, ]$pdfs)), spar = 0.4), col = colors[4])
lines(smooth.spline(CO2_midbins, as.numeric(unlist(final_T_out_cat_ordered[5, ]$pdfs)), spar = 0.4), col = colors[5])
lines(smooth.spline(CO2_midbins, as.numeric(unlist(final_T_out_cat_ordered[6, ]$pdfs)), spar = 0.4), col = colors[6])
lines(smooth.spline(CO2_midbins, as.numeric(unlist(final_T_out_cat_ordered[7, ]$pdfs)), spar = 0.4), col = colors[7])
lines(smooth.spline(CO2_midbins, as.numeric(unlist(final_T_out_cat_ordered[8, ]$pdfs)), spar = 0.4), col = colors[8])
legend("topright", legend = correct_order, col = colors, lwd = 2)
grid()



## home size --------------------

# filter for home size
sta_home_size1 <- sta_fil1_che1_home_ag %>%
  filter(!is.na(home_size)) %>%
  group_by(ID_h, home_size) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            across(study),
            nr_homes=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(percs)),
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

print(paste0('There are ', sum(sta_home_size1$nr_homes), ' homes available with a home size (bed/night)'))

sta_home_size2 <- sta_fil2_che2_home_ag %>%
  filter(!is.na(home_size)) %>%
  group_by(ID_h, home_size) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            across(study),
            nr_homes=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(percs)),
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

print(paste0('There are ', sum(sta_home_size2$nr_homes), ' homes available with a home size (liv/day)'))


ggplot(data = sta_home_size1, aes(x = home_size, y = Mean, color = factor(study))) +
  geom_point(size = 2.5) +          
  geom_smooth(aes(group = 1), method = "loess", se = TRUE) +
  labs(title = bquote("Home size vs CO"[2] * " concentration per home (bed/night/winter," ~ .(sum(sta_home_size1$nr_homes)) ~ "homes)"),
       x = "Home size [m²]",
       y = expression("CO"[2]*" concentration [ppm]")) +     
  theme_minimal()

ggplot(data = sta_home_size2, aes(x = home_size, y = Mean, color = factor(study))) +
  geom_point(size = 2.5) +          
  geom_smooth(aes(group = 1), method = "loess", se = TRUE) +
  labs(title = bquote("Home size vs CO"[2] * " concentration per home (liv/winter," ~ .(sum(sta_home_size2$nr_homes)) ~ "homes)"),
       x = "Home size [m²]",
       y = expression("CO"[2]*" concentration [ppm]")) +     
  theme_minimal()

# make home size categories
sta_home_size1 <- sta_home_size1 %>%
  mutate(size_category = case_when(
    home_size < 40 ~ "0-40",
    home_size < 60 ~ "40-60",
    home_size < 80 ~ "60-80",
    home_size < 100 ~ "80-100",
    home_size < 150 ~ "100-150",
    home_size < 200 ~ "150-200",
    home_size > 200 ~ "> 200"
  ))
sta_home_size2 <- sta_home_size2 %>%
  mutate(size_category = case_when(
    home_size < 40 ~ "0-40",
    home_size < 60 ~ "40-60",
    home_size < 80 ~ "60-80",
    home_size < 100 ~ "80-100",
    home_size < 150 ~ "100-150",
    home_size < 200 ~ "150-200",
    home_size > 200 ~ "> 200"
  ))

# create cdfs/pdfs for each size category
sta_home_size_cat1 <- sta_home_size1 %>%
  group_by(size_category) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(cdfs), cdf = TRUE)/100,
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

sta_home_size_cat2 <- sta_home_size2 %>%
  group_by(size_category) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(cdfs), cdf = TRUE)/100,
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

correct_order1 <- c("40-60", "60-80", '80-100', "100-150", "150-200", '> 200')
correct_order2 <- c("0-40","40-60", "60-80", '80-100', "100-150", "150-200", '> 200')

# Use mutate() to convert size_cat to a factor with the specified order
sta_home_size_cat1_ordered <- sta_home_size_cat1 %>%
  mutate(size_category = factor(size_category, levels = correct_order1, ordered = TRUE)) %>%
  arrange(size_category)
sta_home_size_cat2_ordered <- sta_home_size_cat2 %>%
  mutate(size_category = factor(size_category, levels = correct_order2, ordered = TRUE)) %>%
  arrange(size_category)

## ggplot style

# Assuming `sta_home_size_cat1_ordered` is a data frame with columns `CO2_bins`, `cdfs`, and `category`
data_long1 <- data.frame(
  CO2_bins = rep(CO2_bins, times = nrow(sta_home_size_cat1_ordered)),
  cdfs = as.vector(t(sta_home_size_cat1_ordered$cdfs)),
  category = factor(rep(correct_order1, each = length(CO2_bins)), levels = c("> 200", "150-200", "100-150", "80-100", "60-80", "40-60"))
)

data_long2 <- data.frame(
  CO2_bins = rep(CO2_bins, times = nrow(sta_home_size_cat2_ordered)),
  cdfs = as.vector(t(sta_home_size_cat2_ordered$cdfs)),
  category = factor(rep(correct_order2, each = length(CO2_bins)), levels = c("> 200", "150-200", "100-150", "80-100", "60-80", "40-60", "0-40"))
)

# Add m² to each legend label
legend_labels1 <- c(">200 m²", "150-200 m²", "100-150 m²", "80-100 m²", "60-80 m²", "40-60 m²")
legend_labels2 <- c(">200 m²", "150-200 m²", "100-150 m²", "80-100 m²", "60-80 m²", "40-60 m²", "0-40 m²")

# Define colors and line styles consistently with the category levels
colors1 <- c("red", "blue", "green", "purple", "orange", "brown")
linestyles1 <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
colors2 <- c("red", "blue", "green", "purple", "orange", "brown", 'darkgreen')
linestyles2 <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "4C88C488")

# Plot using ggplot2

plot_home_size_cat<- function(sta, colors, linestyles, legend_labels) {
  g <- ggplot(sta, aes(x = CO2_bins, y = cdfs, color = category, linetype = category)) +
          geom_line(size = 1) +
          scale_color_manual(values = colors, breaks = levels(sta$category), labels = legend_labels) +
          scale_linetype_manual(values = linestyles, breaks = levels(sta$category), labels = legend_labels) +
          labs(
            #title = "ecdfs for home size [m²] categories (bed/night/winter)",
            x = expression('CO'[2]*' concentration [ppm]'),
            y = "Cumulative probability [-]",
            color = "Category",
            linetype = "Category"http://127.0.0.1:20791/graphics/plot_zoom_png?width=796&height=514
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",
            legend.box = "horizontal"
          ) +
          guides(
            color = guide_legend(nrow = 1),
            linetype = guide_legend(nrow = 1)
          )
  return(g)
}
plot_home_size_cat(data_long1, colors1, linestyles1, legend_labels1)
plot_home_size_cat(data_long2, colors2, linestyles2, legend_labels2)


# basic plot style
max_y <- list()
for (i in 1:nrow(sta_home_size_cat1)){
  max_y[i] = max(smooth.spline(CO2_midbins, as.numeric(unlist(sta_home_size_cat1_ordered[i, ]$pdfs)), spar = 0.4)$y)
}

plot(CO2_bins, sta_home_size_cat1_ordered[1, ]$cdfs, type="l", main = paste0('ecdfs for home size [m²] categories (bed/night/winter)'), col = colors[1], xlab = expression('CO'[2]*' concentration [ppm]'), ylab = 'Cumulative probability [-]')
lines(CO2_bins, sta_home_size_cat1_ordered[2, ]$cdfs, col = colors[2])
lines(CO2_bins, sta_home_size_cat1_ordered[3, ]$cdfs, col = colors[3])
lines(CO2_bins, sta_home_size_cat1_ordered[4, ]$cdfs, col = colors[4])
lines(CO2_bins, sta_home_size_cat1_ordered[5, ]$cdfs, col = colors[5])
lines(CO2_bins, sta_home_size_cat1_ordered[6, ]$cdfs, col = colors[6])
legend("bottomright", legend = correct_order, col = colors, lwd = 2)
grid()

plot(smooth.spline(CO2_midbins, as.numeric(unlist(sta_home_size_cat1_ordered[1, ]$pdfs)), spar = 0.4), type="l", ylim = c(0, max(unlist(max_y))), main = paste0('smoothed epdfs for home size [m²] categories (bed/night/winter)'), col = colors[1], xlab = expression('CO'[2]*' concentration [ppm]'), ylab = 'Probability density')
lines(smooth.spline(CO2_midbins, as.numeric(unlist(sta_home_size_cat1_ordered[2, ]$pdfs)), spar = 0.4), col = colors[2])
lines(smooth.spline(CO2_midbins, as.numeric(unlist(sta_home_size_cat1_ordered[3, ]$pdfs)), spar = 0.4), col = colors[3])
lines(smooth.spline(CO2_midbins, as.numeric(unlist(sta_home_size_cat1_ordered[4, ]$pdfs)), spar = 0.4), col = colors[4])
lines(smooth.spline(CO2_midbins, as.numeric(unlist(sta_home_size_cat1_ordered[5, ]$pdfs)), spar = 0.4), col = colors[5])
lines(smooth.spline(CO2_midbins, as.numeric(unlist(sta_home_size_cat1_ordered[6, ]$pdfs)), spar = 0.4), col = colors[6])
legend("topright", legend = correct_order1, col = colors, lwd = 2)
grid()


max_y <- list()
for (i in 1:nrow(sta_home_size_cat2)){
  max_y[i] = max(smooth.spline(CO2_midbins, as.numeric(unlist(sta_home_size_cat2_ordered[i, ]$pdfs)), spar = 0.4)$y)
}

plot(CO2_bins, sta_home_size_cat2_ordered[1, ]$cdfs, type="l", main = paste0('ecdfs for home size [m²] categories (liv/day/winter)'), col = colors2[1], xlab = expression('CO'[2]*' concentration [ppm]'), ylab = 'Cumulative probability [-]')
lines(CO2_bins, sta_home_size_cat2_ordered[2, ]$cdfs, col = colors2[2])
lines(CO2_bins, sta_home_size_cat2_ordered[3, ]$cdfs, col = colors2[3])
lines(CO2_bins, sta_home_size_cat2_ordered[4, ]$cdfs, col = colors2[4])
lines(CO2_bins, sta_home_size_cat2_ordered[5, ]$cdfs, col = colors2[5])
lines(CO2_bins, sta_home_size_cat2_ordered[6, ]$cdfs, col = colors2[6])
lines(CO2_bins, sta_home_size_cat2_ordered[7, ]$cdfs, col = colors2[7])
legend("bottomright", legend = correct_order2, col = colors2, lwd = 2)
grid()

plot(smooth.spline(CO2_midbins, as.numeric(unlist(sta_home_size_cat2_ordered[1, ]$pdfs)), spar = 0.4), type="l", ylim = c(0, max(unlist(max_y))), main = paste0('smoothed epdfs for home size [m²] categories (liv/day/winter)'), col = colors[1], xlab = expression('CO'[2]*' concentration [ppm]'), ylab = 'Probability density')
lines(smooth.spline(CO2_midbins, as.numeric(unlist(sta_home_size_cat2_ordered[2, ]$pdfs)), spar = 0.4), col = colors[2])
lines(smooth.spline(CO2_midbins, as.numeric(unlist(sta_home_size_cat2_ordered[3, ]$pdfs)), spar = 0.4), col = colors[3])
lines(smooth.spline(CO2_midbins, as.numeric(unlist(sta_home_size_cat2_ordered[4, ]$pdfs)), spar = 0.4), col = colors[4])
lines(smooth.spline(CO2_midbins, as.numeric(unlist(sta_home_size_cat2_ordered[5, ]$pdfs)), spar = 0.4), col = colors[5])
lines(smooth.spline(CO2_midbins, as.numeric(unlist(sta_home_size_cat2_ordered[6, ]$pdfs)), spar = 0.4), col = colors[6])
legend("topright", legend = correct_order, col = colors, lwd = 2)
grid()

## room volume ---------------------------

sta_room_vol1 <- sta_fil1_che1_room_ag %>%
  filter(!is.na(room_vol)) %>%
  group_by(ID_r, room_vol) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            across(study),
            nr_rooms=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(percs)),
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

print(paste0('There are ', sum(sta_room_vol1$nr_rooms), ' rooms available with a room volume (bed/night)'))


sta_room_vol2 <- sta_fil2_che2_room_ag %>%
  filter(!is.na(room_vol)) %>%
  group_by(ID_r, room_vol) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            across(study),
            nr_rooms=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(percs)),
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

print(paste0('There are ', sum(sta_room_vol2$nr_rooms), ' rooms available with a room volume (liv/day)'))


ggplot(data = sta_room_vol1, aes(x = room_vol, y = Mean, color = factor(study))) +
  geom_point(size = 2.5) +          
  geom_smooth(aes(group = 1), method = "loess", se = TRUE) +
  labs(#title = expression("Room volume versus vs CO"[2]*" concentration per home (bed/night/winter)"),
       title = bquote("Room volume vs CO"[2] * " concentration per home (bed/night/winter," ~ .(sum(sta_room_vol1$nr_rooms)) ~ "homes)"),
       x = "Room volume [m?]",
       y = expression("CO"[2]*" concentration [ppm]")) +     
  theme_minimal()

ggplot(data = sta_room_vol2, aes(x = room_vol, y = Mean, color = factor(study))) +
  geom_point(size = 2.5) +          
  geom_smooth(aes(group = 1), method = "loess", se = TRUE) +
  labs(#title = expression("Room volume versus vs CO"[2]*" concentration per home (liv/day/winter)"),
       title = bquote("Room volume vs CO"[2] * " concentration per home (liv/day/winter," ~ .(sum(sta_room_vol2$nr_rooms)) ~ "homes)"),
       x = "Room volume [m?]",
       y = expression("CO"[2]*" concentration [ppm]")) +     
  theme_minimal()
## room area ------------------------------------

sta_room_area1 <- sta_fil1_che1_room_ag %>%
  filter(!is.na(room_area)) %>%
  group_by(ID_r, room_area) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            across(study),
            nr_rooms=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(percs)),
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

print(paste0('There are ', sum(sta_room_area1$nr_rooms), ' rooms available with a room area (bed/night)'))


sta_room_area2 <- sta_fil2_che2_room_ag %>%
  filter(!is.na(room_area)) %>%
  group_by(ID_r, room_area) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            across(study),
            nr_rooms=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(percs)),
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

print(paste0('There are ', sum(sta_room_area2$nr_rooms), ' rooms available with a room area (liv/day)'))


ggplot(data = sta_room_area1, aes(x = room_area, y = Mean, color = factor(study))) +
  geom_point(size = 2.5) +          
  geom_smooth(aes(group = 1), method = "loess", se = TRUE) +
  labs(title = expression("Room area versus vs CO"[2]*" concentration per home (bed/night/winter)"),
       x = "Room area [m²]",
       y = expression("CO"[2]*" concentration [ppm]")) +     
  theme_minimal()

ggplot(data = sta_room_area2, aes(x = room_area, y = Mean, color = factor(study))) +
  geom_point(size = 2.5) +          
  geom_smooth(aes(group = 1), method = "loess", se = TRUE) +
  labs(title = expression("Room area versus vs CO"[2]*" concentration per home (liv/day/winter)"),
       x = "Room area [m²]",
       y = expression("CO"[2]*" concentration [ppm]")) +     
  theme_minimal()

## ventilation rate (home) -----------------------

sta_vent_rate_home1 <- sta_fil1_che1_home_ag %>%
  filter(!is.na(vent_rate_home)) %>%
  group_by(ID_h, vent_rate_home) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            across(study),
            country = country,
            nr_homes=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(percs)),
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

print(paste0('There are ', sum(sta_vent_rate_home1$nr_homes), ' homes available with a ventilation rate (bed/night)'))

sta_vent_rate_home2 <- sta_fil2_che2_home_ag %>%
  filter(!is.na(vent_rate_home)) %>%
  group_by(ID_h, vent_rate_home) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            across(study),
            country = country,
            nr_homes=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(percs)),
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

print(paste0('There are ', sum(sta_vent_rate_home2$nr_homes), ' homes available with a ventilation rate (liv/day)'))

# filter for Belgium and no Belgium
sta_vent_rate_home1_bel <- sta_vent_rate_home1 %>%
  ungroup() %>%
  filter(grepl('BEL', sta_vent_rate_home1$country))
sta_vent_rate_home1_nobel <- sta_vent_rate_home1 %>%
  ungroup() %>%
  filter(!grepl('BEL', sta_vent_rate_home1$country))

# all except Belgium
ggplot(data = sta_vent_rate_home1_nobel, aes(x = vent_rate_home, y = Mean, color = factor(study))) +
  geom_point(size = 2.5) +          
  geom_smooth(aes(group = 1), method = "loess", se = TRUE) +
  labs(#title = expression("Ventilation rate versus vs CO"[2]*" concentration per home (bed/night/winter)"),
       x = "Ventilation rate (home) [l/s]",
       y = expression("CO"[2]*" concentration [ppm]")) +     
  theme_minimal()

# only Belgium
ggplot(data = sta_vent_rate_home1_bel, aes(x = vent_rate_home, y = Mean, color = factor(study))) +
  geom_point(size = 1.5) +          
  geom_smooth(aes(group = 1), method = "loess", se = TRUE) +
  labs(title = expression("Ventilation rate (home, only Belgium) versus vs CO"[2]*" concentration per home (bed/night/winter)"),
       x = "Ventilation rate (home) [l/s]",
       y = expression("CO"[2]*" concentration [ppm]")) +     
  theme_minimal()

# liv/day
ggplot(data = sta_vent_rate_home2, aes(x = vent_rate_home, y = Mean, color = factor(study))) +
  geom_point(size = 2.5) +          
  geom_smooth(aes(group = 1), method = "loess", se = TRUE) +
  labs(title = expression("Ventilation rate (home) versus vs CO"[2]*" concentration per home (liv/day/winter)"),
       x = "Ventilation rate (home) [l/s]",
       y = expression("CO"[2]*" concentration [ppm]")) +     
  theme_minimal()

ggplot(data = sta_vent_rate_home2, aes(x = vent_rate_home, y = percs[[98]], color = factor(study))) +
  geom_point(size = 2.5) +          
  geom_smooth(aes(group = 1), method = "loess", se = TRUE) +
  labs(#title = expression("Ventilation rate (home) versus vs CO"[2]*" concentration per home (liv/day/winter)"),
       x = "Ventilation rate (home) [l/s]",
       y = expression("CO"[2]*" concentration [ppm]")) +     
  theme_minimal()



## ventilation rate (room) ---------------------------

sta_vent_rate_room1 <- sta_fil1_che1_room_ag %>%
  filter(!is.na(vent_rate_room)) %>%
  group_by(ID_r, vent_rate_room) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            across(study),
            country = country,
            nr_rooms=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(percs)),
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

print(paste0('There are ', sum(sta_vent_rate_room1$nr_rooms), ' rooms available with a ventilation rate (bed/night)'))


sta_vent_rate_room2 <- sta_fil2_che2_room_ag %>%
  filter(!is.na(vent_rate_room)) %>%
  group_by(ID_r, vent_rate_room) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            across(study),
            country = country,
            nr_rooms=n(),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(percs)),
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

print(paste0('There are ', sum(sta_vent_rate_room2$nr_rooms), ' rooms available with a ventilation rate (liv/day)'))

# filter for Belgium and no Belgium
sta_vent_rate_room1_bel <- sta_vent_rate_room1 %>%
  ungroup() %>%
  filter(grepl('BEL', sta_vent_rate_room1$country))
sta_vent_rate_room1_nobel <- sta_vent_rate_room1 %>%
  ungroup() %>%
  filter(!grepl('BEL', sta_vent_rate_room1$country))

ggplot(data = sta_vent_rate_room1_nobel, aes(x = vent_rate_room, y = Mean, color = factor(study))) +
  geom_point(size = 2.5) +          
  geom_smooth(aes(group = 1), method = "loess", se = TRUE) +
  labs(#title = expression("Ventilation rate versus vs CO"[2]*" concentration per room (bed/night/winter)"),
       x = "Ventilation rate (room) [l/s]",
       y = expression("CO"[2]*" concentration [ppm]")) +     
  theme_minimal()

ggplot(data = sta_vent_rate_room1_bel, aes(x = vent_rate_room, y = Mean, color = factor(study))) +
  geom_point(size = 1.5) +          
  geom_smooth(aes(group = 1), method = "loess", se = TRUE) +
  labs(title = expression("Ventilation rate (room, only Belguim) versus vs CO"[2]*" concentration per room (bed/night/winter)"),
       x = "Ventilation rate (room) [l/s]",
       y = expression("CO"[2]*" concentration [ppm]")) +     
  theme_minimal()


ggplot(data = sta_vent_rate_room2, aes(x = vent_rate_room, y = Mean, color = factor(study))) +
  geom_point(size = 2.5) +          
  geom_smooth(aes(group = 1), method = "loess", se = TRUE) +
  labs(title = expression("Ventilation rate (room) versus vs CO"[2]*" concentration per room (liv/day/winter)"),
       x = "Ventilation rate (room) [l/s]",
       y = expression("CO"[2]*" concentration [ppm]")) +     
  theme_minimal()

ggplot(data = sta_vent_rate_room1, aes(x = vent_rate_room, y = percs[[98]], color = factor(study))) +
  geom_point(size = 2.5) +          
  geom_smooth(aes(group = 1), method = "loess", se = TRUE) +
  labs(#title = expression("Ventilation rate (room) versus vs CO"[2]*" concentration per room (liv/day/winter)"),
       x = "Ventilation rate (room) [l/s]",
       y = expression("CO"[2]*" concentration [ppm]")) +     
  theme_minimal()

# test extract_volume function on meta_r dataset ----------------
room_test <- unique(meta_r$`Additionial room information (e.g., ceiling height, with atrium)`)
for (i in 1:length(room_test)){
  print(paste(sapply(room_test[i], extract_area),'; ', room_test[i]))
}

str_split("2.36 , 12.18 , 33.17 , 28.70 - Height (m) , Floor Area (m2) , Surface Area (m2) , Volume (m3)", ',')[[1]][2]

