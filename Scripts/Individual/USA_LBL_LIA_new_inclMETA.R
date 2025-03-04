## Annnex 86 script for processing and postproc USA LIA data
# Gabriel Rojas, Sep 2024, based on LBL__annex.R (Sascha Hammes) and other postproc scripts

# install the following libraries if needed------------------------
install.packages("remotes") # allows installation from Github
library('remotes')

library(credentials)
git_credential_ask('https://github.com/')
git_credential_update('https://github.com/')
credentials::set_github_pat()

install_github("IEA-EBC-Annex86/annex")
install_github("IEA-EBC-Annex86/annex", ref = "devel")
install.packages("readxl")
install.packages("openxlsx")
# with this new openxlsx package no work around with RDCOMClient needed
install_github("ycphs/openxlsx")

## load required libraries-----------------------------------------
library("annex")
library("readxl")
library("openxlsx")

## functions---------------------------------------------------------
# remove duplicate variables (for same room, check to be included in code) according to given priority (order)
var_rm_dupl <- function(var,config){
  for (vv in 1:(length(var)-1)){
    if (sum(grepl(var[vv], config$column))) {
      config <- config[!config$column %in% var[(vv+1):length(var)],]
      break
      }
  }
  return(config)
}


## --------------------------------------------------------------------
setwd("C:/Daten/A86/AP3_AP4/src")

# define file locations of raw data and annex output files (stat) to be altered
dir_raw1 <- "../data/USA/LIA/raw/doi_10.7941_D1T050__v5/IAQ_Activity_Monitoring/IAQ_Activity_Monitoring"
#dir_raw2 <- "../data/USA/"
dir_out <- "../out/USA/LIA/"
group_size_out <- 8
#sink(paste0(dir_out, "Log_RScript-output.txt"))

## read meta information from files which have not one file per home
ffn_roomnames <- "../data/USA/LIA/prepro/LIA_roomname_conversion.csv"
room_list <- read.csv(ffn_roomnames)
ffn_varnames <- "../data/USA/LIA/prepro/LIA_variablename_conversion.csv"
var_list <- read.csv(ffn_varnames)

ffn_meta1 <- "../data/USA/LIA/raw/doi_10.7941_D1T050__v5/Home_Equipment_Data/Home_Equipment_Data/Home_Characteristics_Data_GR.xlsx"
meta1 <- read_excel(ffn_meta1,sheet="KV_Apart_transp")

# calculate total volume as FloorArea_sqft*CeilingHgt_ft *0.3048^2 in m2
meta1$Vol_tot <- meta1$FloorArea_sqft*meta1$CeilingHeight_ft *0.3048^3 #in m2
# calculate n50 by averaging pres/depress and dividing by volume (avg press and depress if available)
meta1$Q50_Pressurization <- as.numeric(meta1$Q50_Pressurization)
meta1$Q50_Depressurization <- as.numeric(meta1$Q50_Depressurization)
for (ii in 1:nrow(meta1)) {
  if (is.numeric(meta1$Q50_Pressurization[ii]) & is.na(meta1$Q50_Depressurization[ii])) {
    meta1$n50[ii] <- NA
  } else if (is.na(meta1$Q50_Pressurization[ii])) {
    meta1$n50[ii] <- meta1$Q50_Depressurization[ii]  /meta1$Vol_tot[ii]
  } else if (is.na(meta1$Q50_Depressurization[ii])) {
    meta1$n50[ii] <- meta1$Q50_Pressurization[ii] / meta1$Vol_tot[ii]
  } else {
    meta1$n50[ii] <- (meta1$Q50_Depressurization[ii] + meta1$Q50_Pressurization[ii]) / 2 / meta1$Vol_tot[ii]
  }
}

## loop over multiple timeseries (ts) input files-----------------------------
fn_raw1 <- list.files(path=dir_raw1, pattern=".csv")
#fn_raw2 <- list.files(path=dir_raw2, pattern=".csv")
# extract home number from file name
h_Nr <- substr(fn_raw1,10,12)

# Iterate over each element in the list in groups (defined as matrix)
fn_raw1_mat <- matrix(fn_raw1, nrow = group_size_out)
#fn_raw2_mat <- matrix(fn_raw2, nrow = group_size_out)
h_Nr_mat <- matrix(h_Nr, nrow = group_size_out)
var_uni <- list()
config_all <- list()
ii <- 16
#g<-1
for (g in 3:ncol(fn_raw1_mat)) {   #for (f in 1:length(fn_raw1)) {
  print(paste0("##########Starting output group ", g))
  stats <- data.frame()
  ff <- 0
  #f<-1
  for (f in 1:length(h_Nr_mat[,g])) {
    ii<-ii+1
    if (ii<=length(h_Nr)) { # omit any repeated entries in h_Nr_matrix
      ff<-ff+1
      # define home ID
      h_ID <- paste0("H",h_Nr_mat[f,g])
      print(paste0("**********Starting analysis of home: ",h_ID))
      
      # read IAQ raw data file and check&convert timestamp----------
      ffn_raw1 <- paste0(dir_raw1, "/", fn_raw1_mat[f,g])
      raw1 <- read.csv(ffn_raw1)
      raw1$Time <- as.POSIXct(raw1$Time, tz="America/Los_Angeles", format="%m/%d/%y %H:%M")
      print(paste0("Time zone for home ", h_ID, " set to ", attr(raw1$Time, "tzone") ))
      
      # # extract variable, measurement device and room information from column name
      # col_names <- colnames(raw1[,-1])
      # col_nr <- length(col_names)
      # col_var <- unlist(strsplit(col_names, split="_"))[seq(1, col_nr*3, by = 3)]
      # col_dev <- unlist(strsplit(col_names, split="_"))[seq(2, col_nr*3, by = 3)]
      # col_room <- unlist(strsplit(col_names, split="_"))[seq(3, col_nr*3, by = 3)]
      
      # create config1 file -----------
      config1 <- data.frame(column=colnames(raw1[,-1]))
      # define variable and unit
      for (v in var_list$LIA.searchstring) {
        # print(v)
        # print(grepl(v, var_list$LIA.searchstring))
        config1$variable[grepl(v, config1$column)] <- var_list$annex.variable[v==var_list$LIA.searchstring]
        config1$unit[grepl(v, config1$column)] <- var_list$units[v==var_list$LIA.searchstring]
      }
      
      
      # define study and home
      config1$study <- "LIA"
      config1$home <- h_ID
      
      # define room
      config1$room <- NA
      # for (r in 1:length(config1$room)) {
      #   config1$room[r] <- room_list$annex.name[]
      # }
      for (r in room_list$LIA.name) {
        config1$room[grepl(r, config1$column)] <- room_list$annex.name[r==room_list$LIA.name]
      }
      
      # define timestamp column
      config1[nrow(config1)+1,"column"] <- "Time"
      config1[nrow(config1),"variable"] <- "datetime"
      
      # select and/or delete variables/columns to process
      config1 <- config1[!is.na(config1$variable),]         # remove all undefined
      
      # removal of duplicate measurements
      # no matter what removal of variable
      config1 <- config1[!"AVP_IN1_PM25" == config1$column,] #raw data, use adj instead
      config1 <- config1[!"AVP_BR1_PM25" == config1$column,] #raw data, use adj instead
      config1 <- config1[!"CLR_IN1_NO2" == config1$column,] #raw data, use adj instead
      config1 <- config1[!grepl("_Duplicate_",config1$column),] #in some homes, e.g.H932, there are duplicate measurements with AVP (labeled _Duplicate_)
      
      # conditional removal given a priority: different devices have been used in some case in duplicate, discard the less favourable (prio=order in function call)
      config1 <- var_rm_dupl(c("PMin_adj","AVP_IN1_PM25_adj","CLR_IN1_PM25"),config1)
      config1 <- var_rm_dupl(c("AVP_IN1_PM10","CLR_IN1_PM10"),config1)
      config1 <- var_rm_dupl(c("IN1_T","AVP_IN1_T","CLR_IN1_T"),config1)
      config1 <- var_rm_dupl(c("IN1_RH","AVP_IN1_RH","CLR_IN1_RH"),config1)
      
      config1 <- var_rm_dupl(c("AVP_BR1_PM25_adj","CLR_BR1_PM25"),config1)
      config1 <- var_rm_dupl(c("AVP_BR1_PM10","CLR_BR1_PM10"),config1)
      config1 <- var_rm_dupl(c("BR1_T","AVP_BR1_T","CLR_BR_T"),config1)
      config1 <- var_rm_dupl(c("BR1_RH","AVP_BR1_RH","CLR_BR_RH"),config1)
      
      config1 <- var_rm_dupl(c("PMout_adj","CLR_OUT1_PM25","CLR_OUT2_PM25"),config1)
      config1 <- var_rm_dupl(c("CLR_OUT1_PM10","CLR_OUT2_PM10"),config1)
      config1 <- var_rm_dupl(c("OUT_T","CLR_OUT1_T","CLR_OUT2_T"),config1)
      config1 <- var_rm_dupl(c("OUT_RH","CLR_OUT1_RH", "CLR_OUT2_RH"),config1)
      config1 <- var_rm_dupl(c("CLR_OUT1_NO2","CLR_OUT2_NO2","AQS_NO2"),config1)
      config_all[[h_ID]] <- config1
      
      ## run stat analysis on raw 1------------
      annex_check_config(config1)
      prepared_df1 <- annex_prepare(raw1, config1, quiet = TRUE)
      # adapt formula dynamically depending on existing variables (some homes don't have NO2, or other vars)
      var_uni[[ii]] <- unique(config1$variable[!config1$variable=="datetime"])
      form_df1 <- as.formula(paste(paste(var_uni[[ii]], collapse="+"), "~ datetime | study + home + room"))
      annex_df1 <- annex(formula = form_df1, data = prepared_df1, tz = "America/Los_Angeles")
      
      # convert NO2 to allowed units (from ppb to ug/m3) if NO2 exists
      if ("NO2" %in% colnames(annex_df1)) {
        annex_df1$NO2 <- round(annex_df1$NO2/1000 * 46.01 * 101325 / 8.314 / 298.15, 2)
        print(paste0("NO2 values for home ", h_ID, " converted from ppb to ug/m3"))
      }
      
      # # various plot to check data (e.g.warnings about duplicated timestamps)
      # plot(annex_df1$datetime, type="l")
      # plot(annex_df1$datetime, annex_df1$CO2, type="l" )
      # plot(annex_df1$datetime, annex_df1$HCHO, type="l" )
      # plot(annex_df1$datetime, annex_df1$NO2, type="l" )
      # plot(annex_df1$datetime[annex_df1$room=="AMB"], annex_df1$PM25[annex_df1$room=="AMB"], type="l" )
      
      # annex statistical analysis
      stats1 <- annex_stats(annex_df1, format = "long")
      print(paste0("Stats for raw1 data for home ", h_ID, " calculated for: ", paste0(var_uni[[ii]], collapse=" + ") ))
      
      #------------------------------------------------------------------------------------------------
      ## combine stat1, stat2, if more than one raw file was processed and add to multi home stats0------
      stats0 <- stats1 # LIA has only one timeseries raw file
      
      # remove the "all" entries in year and month (if only one year and month) -> revision of annex package needed
      nr_years <- length(unique(stats0$year[!stats0$year=="all"]))
      nr_months <- length(unique(stats0$month[!stats0$month=="all"]))
      if (nr_years==1) {
        idx2rm <- stats0$year == "all"
        stats0 <- stats0[!idx2rm,]
        print(paste0(length(idx2rm)," entries with -all- in column years removed")) }
      if (nr_months==1) {
        idx2rm <- stats0$month == "all"
        stats0 <- stats0[!idx2rm,]
        print(paste0(length(idx2rm)," entries with -all- in column month removed")) } 
      
      # add single home stats0 to multi home stats
      stats <- rbind(stats,stats0)
    }
  }
  
  ffn_out <- paste0(dir_out,"LIA_",h_Nr_mat[1,g],"_",h_Nr_mat[ff,g],"_v4.xlsx")
  annex_write_stats(stats, file = ffn_out, user = 0005) 
  print(paste0("Statistics for ", ffn_out, " with ", ff, " homes written."))
  
  
  ## load result file and write meta data ----------------------------------------
  meta.wb <- loadWorkbook(ffn_out)
  
  # META-study
  meta.study <- readWorkbook(meta.wb, sheet="META-Study")
  meta.study$Contact <- "Nuria Casquero-Modrego, Iain Walker"
  meta.study$Institution <- "Lawrence Berkeley National Lab"
  meta.study$Year.of.first.publication <- 2020
  meta.study$Publications <- "https://doi.org/10.1111/ina.12764"
  meta.study$Links <- "https://doi.org/10.7941/D1T050"
  meta.study$`Additional.information/comments` <- "LIA study (low income apartments) presents pollutant concentrations for code-required mechanical ventilation equipment in 23 low-income apartments at 4 properties constructed or renovated 2013-2017."
  writeData(wb=meta.wb, sheet = "META-Study", x = meta.study, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  
  # META-home
  meta.home <- readWorkbook(meta.wb, sheet="META-Home")
  #h_ID_out <- substr(meta.home$ID,nchar(meta.home$ID)-3,nchar(meta.home$ID)) #works only for this case with fix length Hxxx
  h_ID_out <- unlist(strsplit(meta.home$ID, split="-"))[seq(3,nrow(meta.home)*3,3)]
  h_Nr_out <- as.numeric(substr(h_ID_out,2,nchar(h_ID_out)))
  idx4meta1 <- meta1$KV %in% h_Nr_out
  meta.home$`Location:.Country`<- "USA"
  meta.home$`Location:.City`<- meta1$City[idx4meta1]
  meta.home$Ventilation.type <- "Mechanical ventilation"  
  meta.home$Comment.Vent..Type <- paste0(
    "Type: ", "continous exhaust ventilation",  #meta1$WHV_Type[idx4meta1], 
    "; with fan installed in: ", paste0(meta1$WHV_Exhaust_Fan_Location1[idx4meta1],
                                        ifelse(!is.na(meta1$WHV_Exhaust_Fan_Location2[idx4meta1]), paste0(" + ", meta1$WHV_Exhaust_Fan_Location2[idx4meta1]),""), 
                                        ifelse(!is.na(meta1$WHV_Exhaust_Fan_Location3[idx4meta1]), paste0(" + ", meta1$WHV_Exhaust_Fan_Location3[idx4meta1]),"")),
    "; in operation when research team arrived: ", meta1$WHV_OperatingCurrently[idx4meta1],
    "; further notes on operation control: ", meta1$WHV_OperatingCurrently_Explain[idx4meta1] )
  

  meta.home$`Ventilation.rate.(entire.home;.[l/s])` <- meta1$`MV airflow (L/s)`[idx4meta1]

  meta.home$Method.of.vent..rate.determination <- "Flow-hood / anemometer measurement (in context of study)"
  meta.home$Comment.vent..Rate.determination <- "Sum of bath and kitchen continous extract air (cont. extractin in kitchen only in H902 and H906); Bath exhaust measured using unbalanced hood, Range hood measured using pressure compensation setup"
  meta.home$`Airtightn..[xx]` <- meta1$ACH50[idx4meta1]
  meta.home$`Airtightness.ref.press.[Pa]` <- "50"
  meta.home$Airtightness.normalization.value <- "Building volume specific [1/h]"
  meta.home$Type.of.building <- "Apartment block (AB)"
  meta.home$`Size.of.home./.TFA.[m^2]` <- meta1$`Area (m2)`[idx4meta1] #in m2
  meta.home$Type.of.Occupants <- paste0("Occ.Nr: ", meta1$N_Occupants[idx4meta1], "; low income housing")
  meta.home$Energy.standard <- "California Building Energy Efficiency Standard / Title 24 (>2008)"
  meta.home$`Year.of.contruction./.major.renovation.(four.digit.year)` <- meta1$YearBuilt[idx4meta1]
  meta.home$`Additional.information/comments` <- "More information available in paper and DRYAD repository -> Home_Characteristics_Data.xlsx"
  writeData(wb=meta.wb, sheet = "META-Home", x = meta.home, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  
  # META-room
  meta.room <- readWorkbook(meta.wb, sheet="META-Room")
  r_ID_out <- unlist(strsplit(meta.room$ID, split="-"))[seq(4,nrow(meta.room)*4,4)]
  idx4roomlist <- match(r_ID_out, room_list$annex.name)
  meta.room$`Additionial.room.information.(e.g.,.ceiling.height,.with.atrium)` <- room_list$LIA.description[idx4roomlist]
  meta.room$`Occupancy:.Type`<- "unknown"
  meta.room$Fresh.air.supply.in.measurement.location <- "Other"
  writeData(wb=meta.wb, sheet = "META-Room", x = meta.room, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  
  # META-variable
  meta.var <- readWorkbook(meta.wb, sheet="META-Variable")
  v_ID_out <- unlist(strsplit(meta.var$ID, split="-"))[seq(5,nrow(meta.var)*5,5)]
  r_ID_out <- unlist(strsplit(meta.var$ID, split="-"))[seq(4,nrow(meta.var)*5,5)]
  h_ID_out <- unlist(strsplit(meta.var$ID, split="-"))[seq(3,nrow(meta.var)*5,5)]
  
  ## tbd: assign sensor type based on what was used, see above and config_all
  # for (vv in 1:length(meta.var$ID)){
  #   if (v_ID_out[vv]=="CO2"){
  #     meta.var$Measurement.device[vv] <- "IQAir Air Visual Pro Monitor (AVP)"
  #   }
  # }
    
  meta.var$Measurement.device[v_ID_out=="CO2"] <- "IQAir Air Visual Pro Monitor (AVP)"
  meta.var$Measurement.device[v_ID_out=="T" | v_ID_out=="RH"] <- "IQAir Air Visual Pro Monitor or Onset HOBO U012-013 (indoor) and Onset HOBO U23 Pro v2 (outdoor)"
  meta.var$Measurement.device[v_ID_out=="PM25" & (r_ID_out=="LIV" | r_ID_out=="BED1" | r_ID_out=="BED2")] <- "TSI DustTrak II- 8530 or Thermo pDR-1500 or IQAir Air Visual Pro Monitor"
  meta.var$Measurement.device[v_ID_out=="PM25" & r_ID_out=="AMB"] <- "TSI DustTrak II- 8530"
  meta.var$Measurement.device[v_ID_out=="PM10"] <- "IQAir Air Visual Pro Monitor"
  meta.var$Measurement.device[v_ID_out=="HCHO"] <- "GrayWolf FM-801 (Shinyei Multimode)"
  meta.var$Measurement.device[v_ID_out=="NO2"] <- "Clarity Node or Outdoor data monitored by closest regulatory air monitoring stations"
  meta.var$`Comments.(e.g.,.sensor.location)`<- "Further info see paper incl. supplemental information and README textfile in Dryad data set (KV_IAQ_Activity_Monitoring_README.txt)"
  writeData(wb=meta.wb, sheet = "META-Variable", x = meta.var, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  
  # write meta information to annex xls
  saveWorkbook(meta.wb, ffn_out, overwrite = TRUE)
  print(paste0("Meta information for ", ffn_out, " with ", ff, " homes written."))
}
print(paste0("In total ",g, " output files, with ", ii, " homes written. See ", dir_out))
sink()

sink(paste0(dir_out, "Log_analysed-variables.txt"))
for (jj in 1:length(var_uni)) {
  print(paste("Home H", h_Nr[jj], paste(var_uni[[jj]], collapse=", ")))
}
sink()
