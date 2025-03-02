## Annnex 86 script for processing and postproc USA HENGH data
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

## --------------------------------------------------------------------
setwd("C:/Daten/A86/AP3_AP4/src")

# define file locations of raw data and annex output files (stat) to be altered
dir_raw1 <- "../data/USA/HENGH/raw/doi_10.7941_D1ZS7X__v5/IAQ_Monitoring/IAQ_Monitoring/"
dir_raw2 <- "../data/USA/HENGH/raw/doi_10.7941_D1ZS7X__v5/Airflow/Airflow/"
dir_out <- "../out/USA/HENGH/"
group_size_out <- 10
sink(paste0(dir_out, "Log_RScript-output.txt"))

## read meta information from files which have not one file per home
ffn_roomnames <- "../data/USA/HENGH/prepro/HENGH_roomname_conversion.csv"
room_list <- read.csv(ffn_roomnames)

ffn_meta1 <- "../data/USA/HENGH/raw/doi_10.7941_D1ZS7X__v5/Home_Equipment_Data/Home_Equipment_Data/HENGH_Study_Home_Characteristics.xlsx"
meta1 <- read_excel(ffn_meta1)
# calculate building volume as FloorArea_sqft*CeilingHgt_ft *0.3048^2 in m2 (same result as for Total flow / ACH, tested for 1-2 homes)
meta1$Vol <- meta1$FloorArea_sqft*meta1$CeilingHgt_ft *0.3048^3 #in m2
# calculate n50 by averaging pres/depress and dividing by volume
meta1$n50 <- (meta1$Pressurization_Q50_cfm + meta1$Depressurization_Q50_cfm)/2/meta1$Vol
meta1$FlgAirflo <- FALSE

ffn_meta2 <- "../data/USA/HENGH/raw/doi_10.7941_D1ZS7X__v5/Occupant_Survey/Occupant_Survey/Occupant_Survey_Results.xlsx"
meta2 <- read_excel(ffn_meta2)

## loop over multiple timeseries (ts) input files-----------------------------
fn_raw1 <- list.files(path=dir_raw1, pattern=".csv")
fn_raw2 <- list.files(path=dir_raw2, pattern=".csv")
# extract home number from file name
h_Nr <- substr(fn_raw1,10,12)

# Iterate over each element in the list in groups (defined as matrix)
fn_raw1_mat <- matrix(fn_raw1, nrow = group_size_out)
fn_raw2_mat <- matrix(fn_raw2, nrow = group_size_out)
h_Nr_mat <- matrix(h_Nr, nrow = group_size_out)
var_uni <- list()
ii <- 0
for (g in 1:ncol(fn_raw1_mat)) {   #for (f in 1:length(fn_raw1)) {
  print(paste0("##########Starting output group ", g))
  stats <- data.frame()
  ff <- 0
  for (f in 1:length(h_Nr_mat[,g])) {
    ii<-ii+1
    ff<-ff+1
    if (ii<=length(h_Nr)) { # omit any repeated entries in h_Nr_matrix
      # define home ID
      h_ID <- paste0("H",h_Nr_mat[f,g])
      print(paste0("**********Starting analysis of home: ",h_ID))
      
      # read IAQ raw data file and check&convert timestamp----------
      ffn_raw1 <- paste0(dir_raw1, "/", fn_raw1_mat[f,g])
      raw1 <- read.csv(ffn_raw1)
      raw1$Time <- as.POSIXct(raw1$Time, tz="America/Los_Angeles")
      print(paste0("Time zone for home ", h_ID, " set to ", attr(raw1$Time, "tzone") ))
      
      
      # MOVED below: leave raw as is and better convert annex_df
      # # convert NO2 to allowed units (from ppb to ug/m3)
      # raw1[grep("NO2", colnames(raw1))] <- round(raw1[grep("NO2", colnames(raw1))]/1000 * 46.01 * 101325 / 8.314 / 298.15, 2)
      # print(paste0("NO2 values for home ", h_ID, " converted from ppb to ug/m3"))
      
      # extract variable, measurement device and room information from column name
      col_names <- colnames(raw1[,-1])
      col_nr <- length(col_names)
      col_var <- unlist(strsplit(col_names, split="_"))[seq(1, col_nr*3, by = 3)]
      col_dev <- unlist(strsplit(col_names, split="_"))[seq(2, col_nr*3, by = 3)]
      col_room <- unlist(strsplit(col_names, split="_"))[seq(3, col_nr*3, by = 3)]
      
      # create config1 file -----------
      config1 <- data.frame(column=colnames(raw1[,-1]))
      # define variable and unit
      config1$variable["PM" == col_var] <- "PM25"
      config1$unit["PM" == col_var] <- "ug/m3"
      config1$variable["CO2" == col_var] <- "CO2"
      config1$unit["CO2" == col_var] <- "ppm"
      config1$variable["NO2" == col_var] <- "NO2"
      config1$unit["NO2" == col_var] <- "ug/m3"   # raw is in ppb but annex package doesn't allow yet, so manually converted in this script see above
      config1$variable["FRM" == col_var] <- "HCHO"
      config1$unit["FRM" == col_var] <- "ppb"
      config1$variable["T" == col_var] <- "T"
      config1$unit["T" == col_var] <- "C"
      config1$variable["RH" == col_var] <- "RH"
      config1$unit["RH" == col_var] <- "%"
      # config1$variable["TS" == col_var] <- "T_1"
      # config1$unit["TS" == col_var] <- "C"
      # config1$variable["RHS" == col_var] <- "RH_1"
      # config1$unit["RHS" == col_var] <- "%"
      
      # define study and home
      config1$study <- "HENGH"
      config1$home <- h_ID
      
      # define room
      config1$room <- NA
      # for (r in 1:length(config1$room)) {
      #   config1$room[r] <- room_list$annex.name[]
      # }
      for (r in 1:nrow(room_list)) {
        config1$room[grepl(room_list$HENGH.name[r], col_room)] <- room_list$annex.name[r]
      }
      
      # define timestamp column
      config1[col_nr+1,"column"] <- "Time"
      config1[col_nr+1,"variable"] <- "datetime"
      
      # select and/or delete variables/columns to process
      config1$process <- TRUE
      #config1$process[grepl("FRM_FMM", config1$column)] <- FALSE  #raw Formaldehyde signal, better use adjusted signal, see HENGH_IAQ_Monitoring_README.txt
      #config1$process[grepl("FRM_FLG", config1$column)] <- FALSE  #flag signal, where FA signal has been adjusted, not needed here
      # currently no duplicates allowed even if process set to false -> to be implemented in future releases
      # if (!from %in% c("ug/m3", "mg/m3", "ppm", "ppb"))
      config1 <- config1[!grepl("FRM_FMM", config1$column),]  # formaldehyde raw signal not used, rather the adjusted
      config1 <- config1[!grepl("FRM_FLG", config1$column),]  # formaldehyde flag (when adjustent was done) should not be analysed
      config1 <- config1[!is.na(config1$variable),]         # supply air register T and RH not used (now)
      # in some homes some variable have been measured with two devices, in that case discard one of the two, however be flexible to take either
      #config1 <- config1[!grepl("PDR_IN1", config1$column),]  # some homes have a indoor measurement with PDR also, omitted
      #if (sum(grepl("PDR_OUT", config1$column) | grepl("MET_OUT", config1$column))) { # this OR in combination with sum acts as AND
      #  config1 <- config1[!grepl("PDR_OUT", config1$column),]}  # some homes have a outdoor measurement with MET and/or PDR device, remove PDR if MET is available
      if (sum(c("PM_PDR_OUT","PM_MET_OUT") %in% config1$column)==2) {config1 <- config1[!grepl("PDR_OUT", config1$column),] }
      if (sum(c("PM_PDR_IN1","PM_MET_IN1") %in% config1$column)==2) {config1 <- config1[!grepl("PDR_IN1", config1$column),] }
      if (sum(c("T_HBO_OUT","T_ETC_OUT") %in% config1$column)==2) {config1 <- config1[!grepl("T_ETC_OUT", config1$column),] }
      if (sum(c("T_HBO_LAU","T_ETC_LAU") %in% config1$column)==2) {config1 <- config1[!grepl("T_HBO_LAU", config1$column),] }
      if (sum(c("RH_HBO_OUT","RH_ETC_OUT") %in% config1$column)==2) {config1 <- config1[!grepl("RH_ETC_OUT", config1$column),] }
      if (sum(c("RH_HBO_LAU","RH_ETC_LAU") %in% config1$column)==2) {config1 <- config1[!grepl("RH_HBO_LAU", config1$column),] }
      
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
      
      #--------------------------------------------------------------------------------------------
      ## read Airflow raw data file and check&convert timestamp---------
      ffn_raw2 <- paste0(dir_raw2, "/", fn_raw2_mat[f,g])
      raw2 <- read.csv(ffn_raw2)
      #remove any columns that have no entries, e.g. H038 file has a empty column at end
      raw2 <- raw2[,colSums(is.na(raw2))<nrow(raw2)]
      raw2$Time <- as.POSIXct(raw2$Time, tz="America/Los_Angeles")
      print(paste0("Time zone for home ", h_ID, " set to ", attr(raw2$Time, "tzone") ))
      
      # visualize measurement period of raw 1 and raw 2, if raw 2 is shorter
      raw1_extratime <- as.POSIXct(setdiff(raw1$Time,raw2$Time), origin = "1970-01-01",tz = attr(raw1$Time, "tzone"))
      raw2_extratime<- as.POSIXct(setdiff(raw2$Time,raw1$Time), origin = "1970-01-01",tz = attr(raw1$Time, "tzone"))
      if (length(raw1_extratime)>0) {
        plot(raw1$Time, rep(1,length(raw1$Time)), ylim=c(1,5),  main=h_ID)
        points(raw2$Time, rep(2,length(raw2$Time)), col="blue")
        raw1_extratime <- as.POSIXct(setdiff(raw1$Time,raw2$Time), origin = "1970-01-01",tz = attr(raw1$Time, "tzone"))
        points(raw1_extratime, rep(2,length(raw1_extratime)), col="red")
        print(paste0("=============================> Check: raw2 data file spans a shorter time period than raw1!"))
      } else if (length(raw2_extratime)>=0) {
        print(paste0("Good: raw2 data file spans a longer or equal time period than raw1!"))
      }
      print(paste0(c("raw1-start: ", "raw1-end: "), unlist(range(raw1$Time))))
      print(paste0(c("raw2-start: ", "raw2-end: "), unlist(range(raw2$Time))))
      # points(raw2_extratime, rep(1,length(raw2_extratime)), col="red")
      
      # calculate QCFM_Total and ACH if needed
      # some homes (003, 005, 006, and 029) have no infiltration data (no airleakage measurement) so calc based on MV and flag for meta info
      if(!"QCFM_Total" %in% colnames(raw2)) {
        raw2$QCFM_Total <- raw2$QCFM_MVbalance + raw2$QCFM_MVunbalance
        raw2$ACH <- raw2$QCFM_Total * 0.471947 *3.6 / meta1$Vol[meta1$Home_ID==as.numeric((h_Nr_mat[f,g]))]
        meta1$FlgAirflo[meta1$Home_ID==as.numeric((h_Nr_mat[f,g]))] <- TRUE
        print("QCFM_Total and ACH calculated based on mech vent only!")
        }
      
      # create config2 file----------
      config2 <- data.frame(column=colnames(raw2[,-1]))
      # define variable and unit
      config2$variable[grepl("CFM", config2$column)] <- "Flow"
      config2$unit[grepl("CFM", config2$column)] <- "cfm"
      #******depending on annex version: up to 0.2-13 -> next line only, 0.2-14 or higher -> the 6 lines after
      # config2$variable[grepl("ACH", config2$column)] <- "Other"
      config2$variable[grepl("ACH", config2$column)] <- "Other_1"
      config2$unit[grepl("ACH", config2$column)] <- NA  #"1/h"
      config2$variable[grepl("QCFM_Total", config2$column)] <- "Other_2"
      config2$unit[grepl("QCFM_Total", config2$column)] <- NA #"cfm"
      if("QCFM_Infiltration" %in% colnames(raw2)) {
        config2$variable[grepl("QCFM_Infiltration", config2$column)] <- "Other_3"
        config2$unit[grepl("QCFM_Infiltration", config2$column)] <- NA  #"cfm"
      }
      
      # define study and home
      config2$study <- "HENGH"
      config2$home <- h_ID
      
      # define room
      config2$room <- NA
      for (r in 1:nrow(room_list)) {
        config2$room[grepl(room_list$HENGH.name[r], config2$column)] <- room_list$annex.name[r]
      }
      idx4wholehouse <-config2$column=="ACH" | config2$column=="QCFM_Total" | config2$column=="QCFM_Infiltration"
      config2$room[idx4wholehouse] <- "OTH99"
      
      # define timestamp column
      config2[nrow(config2)+1,"column"] <- "Time"
      config2[nrow(config2),"variable"] <- "datetime"
      
      # select and/or delete variables/columns to process
      config2$process <- TRUE
      #config2$process[grepl("FRM_FMM", config2$column)] <- FALSE  #raw Formaldehyde signal, better use adjusted signal, see HENGH_IAQ_Monitoring_README.txt
      #config2$process[grepl("FRM_FLG", config2$column)] <- FALSE  #flag signal, where FA signal has been adjusted, not needed here
      # currently no duplicates allowed even if process set to false -> to be implemented in future releases
      # if (!from %in% c("ug/m3", "mg/m3", "ppm", "ppb"))
      config2 <- config2[!grepl("RHD_CFM", config2$column),]
      config2 <- config2[!grepl("RHD2_CFM", config2$column),]
      config2 <- config2[!grepl("CDR_CFM", config2$column),]
      config2 <- config2[!grepl("balance", config2$column),]
      #******depending on annex version: up to 0.2-13 -> uncomment next line to delete Infiltration
      #config2 <- config2[!grepl("Infiltration", config2$column),]
    
      # alternative (NOT USED): merge the different raw data frames
      #--------
      # raw <- merge(raw1,raw2, by="Time", all.x=TRUE)
      # raw[-1:-33] <- lapply(raw[,-1:-33], function(col) {
      #   if (all(is.na(col))) return(col)  # Return column if all values are NA
      #   approx(x = raw$Time, y = col, xout = raw$Time, method = "linear", rule = 1)$y
      # })
      # # raw[,-1:-33] <- lapply(raw[,-1:-33], function(col) {
      # #   approx(x = raw$Time, y = col, xout = raw$Time, method = "linear", rule = 1)$y
      # #   })
      # #dt <- as.POSIXct(union(raw1$Time, raw2$Time), tz="America/Los_Angeles")
      # #raw <- union(raw1, raw2)
      
      ## run stat analysis on raw 2------------
      annex_check_config(config2)
      prepared_df2 <- annex_prepare(raw2, config2, quiet = TRUE)
      #******depending on annex version: up to 0.2-13 -> first command, 0.2-14 or higher -> second
      # annex_df2 <- annex(Flow + Other ~ datetime | study + home + room,
      #                    data = prepared_df2, tz = "America/Los_Angeles")
      if("Other_3" %in% config2$variable) {
        annex_df2 <- annex(Flow + Other_1 + Other_2 + Other_3 ~ datetime | study + home + room,
                           data = prepared_df2, tz = "America/Los_Angeles")
      } else {
        annex_df2 <- annex(Flow + Other_1 + Other_2 ~ datetime | study + home + room,
                           data = prepared_df2, tz = "America/Los_Angeles")
      }
      
      # convert flow in cfm to l/s
      annex_df2$Other_2 <- annex_df2$Other_2 * 0.471947
      if("Other_3" %in% colnames(annex_df2)) {annex_df2$Other_3 <- annex_df2$Other_3 * 0.471947}
      print(paste0("Flow in Other_2 (and Other_3) for home ", h_ID, " converted from cfm to l/s"))
      
      # # various plot to check data (e.g.warnings about duplicated timestamps)
      # plot(annex_df2$datetime, annex_df2$Other_1 , type="l", ylim=c(0,1) )
      # plot(annex_df2$datetime, annex_df2$Other_2 , type="l", ylim=c(0,220))
      # lines(annex_df2$datetime, annex_df2$Other_3 , type="l", col="red")
      # plot(annex_df2$datetime[annex_df2$room=="EHA"], annex_df2$Flow[annex_df2$room=="EHA"], type="l" )
      # plot(annex_df2$datetime[annex_df2$room=="SUP"], annex_df2$Flow[annex_df2$room=="SUP"], type="l" )
      
      # annex statistical analysis
      stats2 <- annex_stats(annex_df2, format = "long")
      print(paste0("Stats for raw2 data for home ", h_ID, " calculated."))
      
      #------------------------------------------------------------------------------------------------
      ## combine stat1 and stat2 and add to multi home stats0------
      stats0 <- rbind(stats1,stats2)
      
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
  
  ffn_out <- paste0(dir_out,"HENGH_",h_Nr_mat[1,g],"_",h_Nr_mat[f,g],"_v4.xlsx")
  annex_write_stats(stats, file = ffn_out, user = 0005) 
  print(paste0("Statistics for ", ffn_out, " with ", ff, " homes written."))
  
  
  ## load result file and write meta data ----------------------------------------
  meta.wb <- loadWorkbook(ffn_out)
  
  # META-study
  meta.study <- readWorkbook(meta.wb, sheet="META-Study")
  meta.study$Contact <- "Nuria Casquero-Modrego, Iain Walker"
  meta.study$Institution <- "Lawrence Berkeley National Lab"
  meta.study$Year.of.first.publication <- 2020
  meta.study$Publications <- "https://doi.org/10.1111/ina.12676"
  meta.study$Links <- "https://doi.org/10.7941/D1ZS7X"
  meta.study$`Additional.information/comments` <- "HENGH study collected data in 70 detached houses built in 2011-2017 in compliance with the mechanical ventilation requirements of California's building energy efficiency standards."
  writeData(wb=meta.wb, sheet = "META-Study", x = meta.study, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  
  # META-home
  meta.home <- readWorkbook(meta.wb, sheet="META-Home")
  #h_ID_out <- substr(meta.home$ID,nchar(meta.home$ID)-3,nchar(meta.home$ID)) #works only for this case with fix length Hxxx
  h_ID_out <- unlist(strsplit(meta.home$ID, split="-"))[seq(3,nrow(meta.home)*3,3)]
  h_Nr_out <- as.numeric(substr(h_ID_out,2,nchar(h_ID_out)))
  idx4meta1 <- meta1$Home_ID %in% h_Nr_out
  idx4meta2 <- meta2$`Code number for Home` %in% h_Nr_out
  meta.home$`Location:.Country`<- "USA"
  meta.home$`Location:.City`<- meta1$City[idx4meta1]
  meta.home$Ventilation.type <- "Mechanical ventilation"  
  meta.home$Comment.Vent..Type <- paste0(
    "Type: ", meta1$WHV_System_Installed[idx4meta1], 
    "; with fan installed in: ", meta1$WHV_Fan_Installed[idx4meta1],
    "; Notes: ", meta1$WHV_Notes[idx4meta1] )
  
  #meta.home$`Ventilation.rate.(entire.home;.[l/s])`[h] <- signif(stats$value[stats$home %in% h_ID_out &
  #                                                                             stats$variable=="Other_2" &
  #                                                                             stats$stats=="Mean" & stats$tod=="all"], 3)
  # correct assignment?? safer via loop
  for (h in 1:length(meta.home$ID)) {
    idx4stat <- stats$home==h_ID_out[h] & stats$variable=="Other_2" & stats$stats=="Mean" & stats$tod=="all"
    if (length(stats$month[idx4stat])>1) {idx4stat <- idx4stat & stats$month=="all"}
    if (length(stats$month[idx4stat])>1) {idx4stat <- idx4stat & stats$year=="all"}
    meta.home$`Ventilation.rate.(entire.home;.[l/s])`[h] <- signif(stats$value[idx4stat], 3)
  }
  meta.home$Method.of.vent..rate.determination <- "Other (comment reqd)"
  meta.home$Comment.vent..Rate.determination <- paste0("Mean over full monitoring period, ",
    ifelse(!meta1$FlgAirflo[idx4meta1], "infiltration incl. (estimated based on airleakage measurements)! ", "infiltration NOT incl. (no airleakage measured)! "),
    "Mechanical ventilation based on combinaton of one-time and continuous exhaust air measurements",
    ifelse(meta1$WHV_System_Installed[idx4meta1]=="Supply_Inline", " and supply air fan ratings. ", "."),
    "More info see paper and/or HENGH_Airflow_README.txt")
  meta.home$`Airtightn..[xx]` <- meta1$n50[idx4meta1]
  meta.home$`Airtightness.ref.press.[Pa]`[!meta1$FlgAirflo[idx4meta1]] <- "50"
  meta.home$Airtightness.normalization.value[!meta1$FlgAirflo[idx4meta1]] <- "Building volume specific [1/h]"
  meta.home$Type.of.building <- "Single family house (SFH)"
  meta.home$`Size.of.home./.TFA.[m^2]` <- round(meta1$FloorArea_sqft[idx4meta1] * 0.3048^2, 1) #in m2
  meta.home$Type.of.Occupants <- paste0("Occ.Nr: ", meta2$`6. How many people live in your home?`[idx4meta2], "; Owner-occ: ", ifelse(meta2$`4. Do you own or rent your home?`[idx4meta2]=="Own", "Y", "unknown"))
  meta.home$Energy.standard <- "California Building Energy Efficiency Standard / Title 24 (>2008)"
  meta.home$`Year.of.contruction./.major.renovation.(four.digit.year)` <- meta1$YearBuilt[idx4meta1]
  meta.home$`Additional.information/comments` <- "Location abbr. used in Vent.type: ATT (attic), BA1 (master bathroom), BA2/3 (other bathroom), LAU (laundry room); More information available in paper and DRYAD repository -> HENGH_Study_Home_Characteristics.xlsx"
  writeData(wb=meta.wb, sheet = "META-Home", x = meta.home, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  
  # META-room
  meta.room <- readWorkbook(meta.wb, sheet="META-Room")
  h_ID_out <- unlist(strsplit(meta.room$ID, split="-"))[seq(3,nrow(meta.room)*4,4)]
  r_ID_out <- unlist(strsplit(meta.room$ID, split="-"))[seq(4,nrow(meta.room)*4,4)]
  # h_Nr_out <- as.numeric(substr(h_ID_out,2,nchar(h_ID_out)))
  # idx4meta1 <- meta1$Home_ID %in% h_Nr_out
  meta.room$`Occupancy:.Type`<- "unknown"
  meta.room$Fresh.air.supply.in.measurement.location <- "Other"
  for (r in 1:length(meta.room$ID)){
    if (is.null(room_list$HENGH.description[r_ID_out[r]==room_list$annex.name])) {
      "Could not find description of room ID in room list"}
    else {
      meta.room$`Additionial.room.information.(e.g.,.ceiling.height,.with.atrium)`[r] <- room_list$HENGH.description[r_ID_out[r]==room_list$annex.name] 
    }
    
    idx4stat1 <- stats$home==h_ID_out[r] & stats$room==r_ID_out[r] & stats$variable=="Flow" & stats$stats=="Mean" & stats$tod=="all"
    if (length(stats$month[idx4stat1])>1) {idx4stat1 <- idx4stat1 & stats$month=="all"}
    if (length(stats$month[idx4stat1])>1) {idx4stat1 <- idx4stat1 & stats$year=="all"}
    idx4stat2 <- stats$home==h_ID_out[r] & stats$room==r_ID_out[r] & stats$variable=="Other_2" & stats$stats=="Mean" & stats$tod=="all"
    if (length(stats$month[idx4stat2])>1) {idx4stat2 <- idx4stat2 & stats$month=="all"}
    if (length(stats$month[idx4stat2])>1) {idx4stat2 <- idx4stat2 & stats$year=="all"}
    
    if (sum(idx4stat1)>0) {
      meta.room$`Ventilation.rate.(room;.[l/s])`[r] <- signif(stats$value[idx4stat], 3)
      meta.room$Method.of.vent..rate.determination[r] <- "Flow-hood / anemometer measurement (in context of study)"
      meta.room$Comments[r] <- "Mean over full monitoring period. Exhaust fan airflows were monitored minute by minute using anemometer data loggers and motor sensors. More info see paper (incl. suppl. mat.) and/or HENGH_Airflow_README.txt"
    } else if (sum(idx4stat2)>0) {
      meta.room$`Ventilation.rate.(room;.[l/s])`[r] <- signif(stats$value[idx4stat2], 3)
      meta.room$Comments[r] <- "Mean over full monitoring period. Total ventilation rate was calculated using a superposition adjustment to account for the sub-additivity of unbalanced mechanical airflows with air infiltration. More info see paper (incl. suppl. mat.) and/or HENGH_Airflow_README.txt"
    }
  }
  writeData(wb=meta.wb, sheet = "META-Room", x = meta.room, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  
  # META-variable
  meta.var <- readWorkbook(meta.wb, sheet="META-Variable")
  v_ID_out <- unlist(strsplit(meta.var$ID, split="-"))[seq(5,nrow(meta.var)*5,5)]
  r_ID_out <- unlist(strsplit(meta.var$ID, split="-"))[seq(4,nrow(meta.var)*5,5)]
  meta.var$Variable.unit[v_ID_out=="Other_1"] <- "1/h"
  meta.var$Variable.unit[v_ID_out=="Other_2" | v_ID_out=="Other_3"] <- "l/s"
  meta.var$Measurement.device[v_ID_out=="CO2"] <- "Extech SD-800 Infrared"
  meta.var$Measurement.device[v_ID_out=="T" | v_ID_out=="RH"] <- "Extech CO2 monitor (if CO2 measured in room) otherwise Onset HOBO UX100-011 (indoor) and Onset HOBO U23 Pro v2 (outdoor)"
  meta.var$Measurement.device[v_ID_out=="PM25" & r_ID_out=="LIV"] <- "Met One BT-645 Photometer"
  meta.var$Measurement.device[v_ID_out=="PM25" & r_ID_out=="AMB"] <- "Met One BT-642 Photometer"
  meta.var$Measurement.device[v_ID_out=="HCHO"] <- "GrayWolf FM-801 (Shinyei Multimode)"
  meta.var$Measurement.device[v_ID_out=="NO2"] <- "Aeroqual 500 Series Electrochemical"
  
  idx4flow <- v_ID_out=="Flow" & grepl("BAT",r_ID_out) | r_ID_out=="OTH3"
  meta.var$Measurement.device[idx4flow] <- "TEC Exhaust Fan Flow Meter (nominal flow), motor on/off sensor (Onset HOBO UX90-004), vane anemometer (Digi-Sense WD-20250-22), or plug load logger (Onset HOBO UX120-018)"
  meta.var$`Comments.(e.g.,.sensor.location)`[idx4flow] <-"Fan flow measured with TEC Fan Flow Meter, fan usage monitored minute by minute using anemometer data loggers or motor sensors."
  
  idx4sup <- v_ID_out=="Flow" & r_ID_out=="SUP"
  meta.var$Variable.additional.information[idx4sup] <- "Rated supply fan airflow"
  meta.var$Measurement.device[idx4sup] <- "rated"
  meta.var$`Comments.(e.g.,.sensor.location)`[idx4sup] <- "In homes where a supply fan provide the whole house mechanical ventilation (001, 003, 009, and 010), the supply fan airflow rate was not measured and the rated airflow rate was assumed in the calculation of total ventilation rates."
  idx4eha <- v_ID_out=="Flow" & r_ID_out=="EHA"
  meta.var$Variable.additional.information[idx4eha] <- "Total exhaust fan airflow"
  meta.var$Measurement.device[idx4eha] <- "calculated"
  meta.var$`Comments.(e.g.,.sensor.location)`[idx4eha] <- "Calculated by summing exhaust fan flows (whole house exhaust fan, and other fans in bathroom, range hood, clothes dryer) weighted by their average usage time. More info see paper (incl. suppl. mat.) and/or HENGH_Airflow_README.txt"
  
  meta.var$Measurement.device[grepl("Other",v_ID_out)] <- "calculated"
  meta.var$Variable.additional.information[v_ID_out=="Other_1"] <- "Total air exchange rate (whole house)"
  meta.var$Variable.additional.information[v_ID_out=="Other_2"] <- "Total ventilation airflow (whole house)"
  meta.var$Variable.additional.information[v_ID_out=="Other_3"] <- "Infiltration airflow (whole house)"
  meta.var$`Comments.(e.g.,.sensor.location)`[v_ID_out=="Other_1"] <- "Calculated using total ventilation airflow and the estimated house volume, calculated from the estimated house-average ceiling height and floor area recorded by the field team. More info see paper (incl. suppl. mat.) and/or HENGH_Airflow_README.txt"
  meta.var$`Comments.(e.g.,.sensor.location)`[v_ID_out=="Other_2"] <- "Calculated using a superposition adjustment to account for the sub-additivity of unbalanced mechanical airflows with air infiltration. More info see paper (incl. suppl. mat.) and/or HENGH_Airflow_README.txt"
  meta.var$`Comments.(e.g.,.sensor.location)`[v_ID_out=="Other_3"] <- "Calculated using the measured envelope air leakage flow coefficient and pressure exponent for each home, determined as part of the DeltaQ Test. More info see paper (incl. suppl. mat.) and/or HENGH_Airflow_README.txt"
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

#----------------------------------------------------------end HENGH
int_list <- c(0, 1, 2, 3)

# Iterate over each element in the list
for (element in int_list) {
  # Define the string variables
  prefix <- "lia_data_"
  prefix_config <- "lia_data_config_"
  prefix_output <- "lia_LBL_"
  suffix <- ".txt"
  suffix_output <- "_v3.xlsx"
  
  # Example integer to string conversion
  str_element <- as.character(element)
  
  # Concatenate strings using paste0
  concatenated_string <- paste0(prefix, str_element, suffix)
  concatenated_string_config <- paste0(prefix_config, str_element, suffix)
  concatenated_string_output <- paste0(prefix_output, str_element, suffix_output)

  raw_df <- read.csv(concatenated_string)
  config1 <- read.table(concatenated_string_config,
                       comment.char = "#", sep = "",
                       header = TRUE, na.strings = c("NA", "empty"))
  # see ?read.table for details
  
  # Class and dimension of the objects
  c("raw_df" = is.data.frame(raw_df), "config1" = is.data.frame(config1))
  
  cbind("raw_df" = dim(raw_df), "config1" = dim(config1))
  
  annex_check_config(config1)
  
  raw_df <- transform(raw_df, X = as.POSIXct(X, tz = "UTC"))
  
  class(raw_df$X)
  
  prepared_df <- annex_prepare(raw_df, config1, quiet = TRUE)
  
  annex_df1 <- annex(RH + T + CO2 + PM10 + PM25 + HCHO ~ datetime | study + home + room,
                    data = prepared_df, tz = "Europe/Berlin")
  annex_df2 <- annex(RH + T + CO2 + PM1 + PM10 + PM25 + NO2 + HCHO ~ datetime | study + home + room,
                     data = prepared_df, tz = "Europe/Berlin")
  
  if (element == 0) {
    stats <- annex_stats(annex_df1, format = "long")
  } else {
    stats <- annex_stats(annex_df2, format = "long")
  }
  
  
  annex_write_stats(stats, file = concatenated_string_output, user = 0005) 
  print(element)
}


idx4con <- "PM" %in% col_var 
config1$variable["PM" %in% col_var] <- "PM25"
config1$unit["PM" %in% col_var] <- "ug/m3"
config1$variable["CO2" %in% col_var] <- "CO2"
config1$unit["CO2" %in% col_var] <- "ppm"
config1$variable[grepl("NO2", col_var)] <- "NO2"
config1$unit[grepl("NO2", col_var)] <- "ug/m3"   # raw is in ppb but annex package doesn't allow yet, so manually converted in this script see below
config1$variable[grepl("FRM", col_var)] <- "HCHO"
config1$unit[grepl("FRM", col_var)] <- "ppb"
config1$variable[grepl("T", col_var)] <- "T"
config1$unit[grepl("T", col_var)] <- "C"
config1$variable[grepl("RH", col_var)] <- "RH"
config1$unit[grepl("RH", col_var)] <- "%"
