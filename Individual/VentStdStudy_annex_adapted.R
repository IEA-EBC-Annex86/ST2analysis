## Original: Andreas Frei
## Quick & dirty adaptions to change for meta analysis: Gabriel Rojas
## Jun 2024

# R --vanilla < VentStdStudy_annex.R
print("-> VentStdStudy")

library("annex")
library("openxlsx")
library("readxl")
library("RDCOMClient")
library("dplyr")

## Set the working directory
setwd("C:/Daten/A86/AP3_AP4/src")
# load helper "functions" 
sapply(dir("helper", full.names=TRUE), source)

##----------------------------------------------------------------
# helper function to map Fresh Air Supply (MeTA room) based on vent type and room
SupAirMap <- function(room, ven, ven2) {
  
  if (grepl("Extract", ven2) | grepl("Exhaust",ven2)) {ven2 <- "MEV"}
  if (grepl("MVHR", ven2)) {ven2 <- "MVHR"}
  
  if (grepl("BED", room)) {room <- "BED"} 
  if (grepl("LIV", room)) {room <- "LIV"} 
  if (grepl("KIT", room)) {room <- "KIT"} 
  if (grepl("BAT", room)) {room <- "BAT"} 
  if (grepl("AMB", room)) {room <- "AMB"} 
  
  case_when(
    ven == "Mechanical ventilation" & ven2 == "MVHR" & (room == "BED" | room == "LIV") ~ "Through mechanically driven supply air (no recirculated air)",
    ven == "Mechanical ventilation" & ven2 == "MVHR" & (room == "KIT" | room == "BAT") ~ "Through mechanically driven overflow (or recirculated air) from adjacent zones",
    ven == "Mechanical ventilation" & ven2 == "MEV" & (room == "BED" | room == "LIV") ~ "Through permanent opening to the exterior (grille, .)",
    ven == "Mechanical ventilation" & ven2 == "MEV" & (room == "KIT" | room == "BAT") ~ "Through mechanically driven overflow (or recirculated air) from adjacent zones",
    ven == "Natural ventilation (designed)" | ven == "Window airing (not designed)" ~ "Through occupant operated window only",
    room =="AMB" ~ ""
  )
}

# dir_cfg <- c("../tmp/VentStdStudy/cfg")
# dir_dat <- c("../tmp/VentStdStudy/data")
# dir_out <- c("../tmp/VentStdStudy/tmp")

# RDCOMClient only works with absolute paths (in my case Win10)!? 
dir_cfg <- c("C:\\Daten\\A86\\AP3_AP4\\data\\GBR\\VentStd_study\\prepro\\cfg")
dir_dat <- c("C:\\Daten\\A86\\AP3_AP4\\data\\GBR\\VentStd_study\\prepro\\data")
dir_out <- c("C:\\Daten\\A86\\AP3_AP4\\out\\GBR\\VentStd")

dir.create(dir_out, recursive=TRUE)

fns <- list.files(path=dir_cfg)

#i<-1
for (i in 1:length(fns)) {
    
    name = unlist(strsplit(fns[i], split=".cfg"))[1]
    
    print(name)
    
    # fn_dat <- paste(dir_dat, "/", name, ".csv", sep="")
    # fn_cfg <- paste(dir_cfg, "/", name, ".cfg", sep="")
    
    fn_dat <- paste(dir_dat, "\\", name, ".csv", sep="")
    fn_cfg <- paste(dir_cfg, "\\", name, ".cfg", sep="")
    
    raw_df <- read.csv(fn_dat)
    config <- read.csv(fn_cfg, na.strings=c("NA", "empty"))
    idx_row <- nrow(config)

    # check timestamp and change as needed
    attr(raw_df$tst, "tzone")
    raw_df$tst <- as.POSIXct(raw_df$tst, format="%Y-%m-%d %H:%M:%S",tz = "Europe/London")
    attr(raw_df$tst, "tzone")
    
    # check if all time stamps ok and remove if not
    sum(is.na(raw_df$tst))
    raw_df <- raw_df[!is.na(raw_df$tst),]
    sum(is.na(raw_df$tst))
    
    ## adaptions to config files
    # first run only: check if Duomo and Aico sensor signal is the same 
    # idxplot <- !is.na(raw_df$living_co2)
    # plot(raw_df$tst[idxplot], raw_df$living_co2[idxplot], type="l")
    # idxplot <- !is.na(raw_df$living_co2_2)
    # lines(raw_df$tst[idxplot], raw_df$living_co2_2[idxplot], col="red")
    # idxplot <- !is.na(raw_df$living_temp)
    # plot(raw_df$tst[idxplot], raw_df$living_temp[idxplot], type="l")
    # idxplot <- !is.na(raw_df$living_temp_2)
    # lines(raw_df$tst[idxplot], raw_df$living_temp_2[idxplot], col="red")
    # idxplot <- !is.na(raw_df$living_rh)
    # plot(raw_df$tst[idxplot], raw_df$living_rh[idxplot], type="l")
    # idxplot <- !is.na(raw_df$living_rh_2)
    # lines(raw_df$tst[idxplot], raw_df$living_rh_2[idxplot], col="red")
    
    # don't use CO2, T, RH from Duomo sensor
    #config$process <- TRUE
    #config$process[grepl("_2",config$column)] <- FALSE
    # but config file still doesn't like it because doubled -> change in future revision, for now delete
    config <- config[!grepl("_2",config$column),]

    ## check config file and run annex analysis
    config <- annex_check_config(config)

    prepared_df <- annex_prepare(raw_df, config, quiet=TRUE)
    
    annex_df <- annex(
        RH + T + CO2 + PM25 + PM10  ~ datetime | study + home + room,
        data=prepared_df, tz="Europe/London"
    )
                      
    stats <- annex_stats(annex_df, format="long")

    # write results to file
    fn_out <- paste(dir_out, "\\", name, ".xlsx", sep="")
    annex_write_stats(stats, file=fn_out, user=13)
    # open & close (read & save) the file using Excel to avoid/fix bug of openxlsx
    read_save_xlsx(fn_out)
    
    ## load result file and write meta data
    meta.wb <- loadWorkbook(fn_out)
    
    # META-study
    #meta.study <- readWorkbook(meta.wb, sheet="META-Study", rowNames = FALSE, colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
    meta.study <- readWorkbook(meta.wb, sheet="META-Study")
    meta.study$Contact <- "Gráinne McGill, Linda Toledo"
    meta.study$Institution <- "University of Strathclyde"
    meta.study$Year.of.first.publication <- 2023
    meta.study$Publications <- "Final Report: Research to identify if changes to guidance in standard 3.14 ventilation in 2015 have been effective in improving ventilation and indoor air quality"
    meta.study$Links <- "https://www.gov.scot/publications/research-identify-changes-guidance-standard-3-14-ventilation-2015-effective-improving-ventilation-indoor-air-quality/documents/"
    writeData(wb=meta.wb, sheet = "META-Study", x = meta.study, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
    
    
    # load file with meta data
    meta.data <- read_excel("../data/GBR/VentStd_study/meta/Ventilation Standards template (metadata)_GR.xlsx", sheet="META-Home")
    meta.data <- meta.data[!is.na(meta.data$ID),] #delete empty lines
    
    # META-home
    meta.home <- readWorkbook(meta.wb, sheet="META-Home")
    meta.home[1,-1] <- meta.data[meta.data$ID==name,-1]
    meta.home$`Location:.Country` <- "GBR"
    if (meta.home$Airtightness.normalization.value=="q50 (m2/h/m2)"){meta.home$Airtightness.normalization.value <- "Envelope area specific in [m3/h/m2]"}
    writeData(wb=meta.wb, sheet = "META-Home", x = meta.home, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
    
    # META-room
    meta.room <- readWorkbook(meta.wb, sheet="META-Room")
    if (i==8) { #home 8 had no trickle vents, see info
      meta.room$Fresh.air.supply.in.measurement.location <- "Other"
      meta.room$Comments <- "mechanical extract ventilation without trickle vent openings"
    } else {
    meta.room$Fresh.air.supply.in.measurement.location <- lapply(meta.room$ID, SupAirMap, ven=meta.home$Ventilation.type[grepl(name,meta.home$ID)], ven2=meta.home$Comment.Vent..Type[grepl(name,meta.home$ID)])
    }
    writeData(wb=meta.wb, sheet = "META-Room", x = meta.room, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
    
    # META-variable
    meta.var <- readWorkbook(meta.wb, sheet="META-Variable")
    #meta.var$Variable.unit[grepl("VOC",meta.var$ID)] <- "ppb"
    # long term sensors (T, RH, CO2)
    meta.var$Measurement.device[(grepl("-T",meta.var$ID)|grepl("-RH",meta.var$ID)|grepl("-CO2",meta.var$ID)) & !grepl("-AMB",meta.var$ID)] <- "HomeLink Environmental Sensors Ei1025, Aico Ltd."
    meta.var$`Comments.(e.g.,.sensor.location)`[(grepl("-T",meta.var$ID)|grepl("-RH",meta.var$ID)|grepl("-CO2",meta.var$ID)) & !grepl("-AMB",meta.var$ID)] <- "Placed in room using non-marking fixing tape"
    # short term sensors (PM, not used: VOC, T, RH, CO2)
    meta.var$Measurement.device[grepl("-PM25",meta.var$ID)|grepl("-PM10",meta.var$ID)] <- "LoRaWAN Wireless IAQ Sensor, Duomo Ltd."
    meta.var$`Comments.(e.g.,.sensor.location)`[grepl("-PM25",meta.var$ID)|grepl("-PM10",meta.var$ID)] <- "Positioned near TV-set"
    writeData(wb=meta.wb, sheet = "META-Variable", x = meta.var, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
    
    # write meta information to annex xls
    saveWorkbook(meta.wb, fn_out, overwrite = TRUE)
}
