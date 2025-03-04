# R --vanilla < NL_MarcelLoomans_annex.R
print("-> NL_MarcelLoomans")
library("openxlsx")
library("readxl")
library("dplyr")
library("annex")

## Set the working directory
# Timm
setwd("C:/Users/Timm/Desktop/Atmospheric Sciences/Arbeit")

dir_cfg <- c("Annex86_AP3_data/NLD/NL_ML/prepro/cfg")
dir_dat <- c("Annex86_AP3_data/NLD/NL_ML/raw/vjxx00_2024cor")
dir_add <- c("Annex86_AP3_data/NLD/NL_ML/meta/additional_meta_info")
dir_out <- c("Data(selfmade)/NLD(new)/NL_ML")

# Gabriel
#setwd("C:/Daten/A86/AP3_AP4/src")
#dir_cfg <- c("../data/NLD/NL_ML/prepro/cfg")
#dir_dat <- c("../data/NLD/NL_ML/prepro/data")
#dir_dat <- c("../data/NLD/NL_ML/raw/vjxx00_2024cor")
#dir_add <- c("../data/NLD/NL_ML/prepro/additional_meta_info")

#dir_out <- c("../out/NLD/NL_ML")


dir.create(dir_out, recursive=TRUE)

# List of files (have only the csv file to be read in the folder)
fns <- list.files(path=dir_cfg)[-1]

additional_info <- read.csv(paste0(dir_add,'/', 'additional_meta_info.txt'), na.strings=c("NA", "empty"))

i<-1
for (i in 1:length(fns)) {
  
  name <- unlist(strsplit(fns[i], split=".cfg"))[1]
  homeID <- unlist(strsplit(name, split="_"))[2]
  homeID <- sub("^0+", "", homeID)
  
  print(name)
  
  fn_dat <- paste(dir_dat, "/", name, ".csv", sep="")
  fn_cfg <- paste(dir_cfg, "/", name, ".cfg", sep="")
  
  # fn_dat <- paste(dir_dat, "\\", name, ".csv", sep="")
  # fn_cfg <- paste(dir_cfg, "\\", name, ".cfg", sep="")
  
  raw_df <- read.csv(fn_dat)
  # q&d fix: date colnames comes in as "?..date"
  if (i==1) {colnames(raw_df)[1] <- "date"}
  
  config <- read.csv(fn_cfg, na.strings=c("NA", "empty"))
  idx_row <- nrow(config)
  
  # merge date and time column and apply transform
  raw_df$date <- gsub('/', '-', raw_df$date)
  raw_df <- transform(raw_df, tst = as.POSIXct(paste(date, time), format="%d-%m-%Y %H:%M:%S", tz = "UTC"))
  
  # check if all time stamps ok and remove if not
  sum(is.na(raw_df$tst))
  raw_df <- raw_df[!is.na(raw_df$tst),]
  sum(is.na(raw_df$tst))
  
  ## check config file and run annex analysis
  config <- annex_check_config(config)  #if error check if too many entries were created
  
  prepared_df <- annex_prepare(raw_df, config, quiet=TRUE)
  
  annex_df <- annex(RH + T + CO2 + Flow ~ datetime | study + home + room,
                    data = prepared_df, tz = "Europe/Berlin")
  
  stats <- annex_stats(annex_df, format="wide")
  
  # write results to file
  fn_out <- paste(dir_out, "/", name, ".xlsx", sep="")
  annex_write_stats(stats, file=fn_out, user=10)
  # open & close (read & save) the file using Excel to avoid/fix bug of openxlsx
  #read_save_xlsx(fn_out)
  
  ## load result file and write meta data
  meta.wb <- loadWorkbook(fn_out)
  
  # META-study
  #meta.study <- readWorkbook(meta.wb, sheet="META-Study", rowNames = FALSE, colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
  meta.study <- readWorkbook(meta.wb, sheet="META-Study")
  meta.study$Contact <- "Marcel Loomans"
  meta.study$Institution <- "Eindhoven University of Technology"
  meta.study$Year.of.first.publication <- 2014
  meta.study$Publications <- "Report (not public; https://research.tue.nl/en/publications/rapportage-monitoring-kroeven-2013-2014)"
  meta.study$Links <- "https://cordis.europa.eu/project/id/260058"
  meta.study$`Additional.information/comments` <- "Data obtained from a 2 year (2013-2014) detailed measurement exercise in 10 passive house renovated dwellings. Measurements contained detailed info on the energy demands and on the indoor climate (in three rooms of the dwellings living room [lr] and two bedrooms [br])"
  writeData(wb=meta.wb, sheet = "META-Study", x = meta.study, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  

  
  # META-home
  meta.home <- readWorkbook(meta.wb, sheet="META-Home")
  meta.home$`Location:.Country` <- "NLD"
  meta.home$`Location:.City` <- "Roosendaal"
  meta.home$Ventilation.type <- "Mechanical ventilation"
  if (i %in% c(1, 2, 4, 6, 10)){
    meta.home$Comment.Vent..Type <- "MVHR Syst1" 
  }else{
    meta.home$Comment.Vent..Type <- "MVHR Syst2"
  }
  meta.home$`Ventilation.rate.(entire.home;.[l/s])` <- additional_info$vent_home[i] #already converted in txt file
  meta.home$Method.of.vent..rate.determination <- "Flow-hood / anemometer measurement (in context of study)"
  meta.home$Comment.vent..Rate.determination <- "Position fan derived from pulses"
  meta.home$`Airtightn..[xx]`<- additional_info$airtight[i]
  meta.home$`Airtightness.ref.press.[Pa]` <- 50
  meta.home$Type.of.building <- additional_info$building[i]
  meta.home$`Size.of.home./.TFA.[m^2]` <- 100
  meta.home$Type.of.Occupants <- additional_info$occupants[i]
  meta.home$`Constr..type./.building.materials` <- "brick/concrete"
  meta.home$Energy.standard <- "Passive House (renovation)"
  meta.home$`Year.of.contruction./.major.renovation.(four.digit.year)`<- 2011
  meta.home$`Additional.information/comments` <- "81.2m2 ground floor and 1st floor together. Attic accessible, assessed at ~20 m2. Similar for all houses. MVHR user control via three-level switch (centrally positioned in home), vent. rate provided for highest fan level (3), window opening possible "
  writeData(wb=meta.wb, sheet = "META-Home", x = meta.home, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  
  # META-room
  meta.room <- readWorkbook(meta.wb, sheet="META-Room")
  # for each ID get the column in the additional info that matches the room 
  for (j in 1:3){
    meta.room$`Ventilation.rate.(room;.[l/s])`[j] <- additional_info[which(grepl(strsplit(meta.room$ID[j], '-')[[1]][4], colnames(additional_info), ignore.case = TRUE))][[1]][i]/3.6  #txt file room data in m3/h
  }
  meta.room$Fresh.air.supply.in.measurement.location <- "Through mechanically driven supply air (no recirculated air)"
  meta.room$Method.of.vent..rate.determination <- "Pressure-drop compensated Flow-hood measurement (in context of study)"
  meta.room$Comments <- "vent. rate provided for highest fan level (3)"
  writeData(wb=meta.wb, sheet = "META-Room", x = meta.room, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  
  # META-variable
  meta.var <- readWorkbook(meta.wb, sheet="META-Variable")
  
  v_ID_out <- unlist(strsplit(meta.var$ID, split="-"))[seq(5,nrow(meta.var)*5,5)]
  
  meta.var$Variable.additional.information[v_ID_out=="CO2"] <- "CO2 concentration"
  meta.var$Variable.additional.information[v_ID_out=="RH"] <- "relative humidity"
  meta.var$Variable.additional.information[v_ID_out=="T"] <- "air temperature"
  meta.var$Variable.additional.information[v_ID_out=="Flow"] <- "Supply air flow"
  
  meta.var$Measurement.device[v_ID_out=="CO2"] <- "± (50 + 3% reading) ppm; (2ppm CO2/K range 0-50oC; background corrected, calibrated"
  meta.var$Measurement.device[v_ID_out=="RH"] <- "±2 % (10 – 90% RV)"
  meta.var$Measurement.device[v_ID_out=="T"] <- "±0.5K (15 - 30oC)"
  
  meta.var$`Comments.(e.g.,.sensor.location)`[v_ID_out!='Flow']<- "at wall, multisensor"
  writeData(wb=meta.wb, sheet = "META-Variable", x = meta.var, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  
  # write meta information to annex xls
  saveWorkbook(meta.wb, fn_out, overwrite = TRUE)
}
