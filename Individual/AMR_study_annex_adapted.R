## Original: Andreas Frei
## Quick & dirty adaptions to change for meta analysis: Gabriel Rojas
## Jun 2024
## GR: Jul 2024, adaption after adjusting home numbering (5a, 5b different homes but same occupants)

# R --vanilla < AMR_study_annex.R

print("-> AMR_study")

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
  
  case_when(
    ven == "Mechanical ventilation" & ven2 == "MVHR" & (room == "BED" | room == "LIV") ~ "Through mechanically driven supply air (no recirculated air)",
    ven == "Mechanical ventilation" & ven2 == "MVHR" & (room == "KIT" | room == "BAT") ~ "Through mechanically driven overflow (or recirculated air) from adjacent zones",
    ven == "Mechanical ventilation" & ven2 == "MEV" & (room == "BED" | room == "LIV") ~ "Through permanent opening to the exterior (grille, .)",
    ven == "Mechanical ventilation" & ven2 == "MEV" & (room == "KIT" | room == "BAT") ~ "Through mechanically driven overflow (or recirculated air) from adjacent zones",
    ven == "Natural ventilation (designed)" | ven == "Window airing (not designed)" ~ "Through occupant operated window only",
  )
}

# dir_cfg <- c("../tmp/AMR_study/cfg")
# dir_dat <- c("../tmp/AMR_study/data")
# dir_out <- c("../tmp/AMR_study/tmp")

# RDCOMClient only works with absolute paths (in my case Win10)!? 
dir_cfg <- c("C:\\Daten\\A86\\AP3_AP4\\data\\GBR\\AMR_study\\prepro\\cfg")
dir_dat <- c("C:\\Daten\\A86\\AP3_AP4\\data\\GBR\\AMR_study\\prepro\\data")
dir_out <- c("C:\\Daten\\A86\\AP3_AP4\\out\\GBR\\AMR")

dir.create(dir_out, recursive=TRUE)

fns <- list.files(path=dir_cfg)

#i<-5
for (i in 1:length(fns)) {
    
    name <- unlist(strsplit(fns[i], split=".cfg"))[1]
    homeID <- unlist(strsplit(name, split="_"))[2]
    homeID <- sub("^0+", "", homeID)
    
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
    
    ## adaptions to config files (mostly not needed after pre processing adaptions Jul 2024)
    # # rename PMOther and add units
    # config$variable[config$variable=="PMOther"] <- "PM25"    # might not be needed, now already implemented in Py pre proceccing
    config$unit[config$variable=="PM25"] <- "ug/m3"
    config$unit[config$variable=="VOC"] <- "ug/m3"  # Q&D fix: because annex package doesn`t currently allow ppb for VOC
    # # add absolute humidity to config
    # # watch out if re-run manually: config file will have too many entries
    # config[idx_row+1,"column"] <- "living_ah"
    # config[idx_row+2,"column"] <- "bedroom_ah"
    # config[idx_row+3,"column"] <- "kitchen_ah"
    # config[idx_row+1:3,"variable"] <- "Other"
    # config[idx_row+1:3,"study"] <- config$study[idx_row]
    # #alternative #config <- rbind(config, config[config$variable=="RH",])  #append 3 rows to df, use RH as template
    # #config[idx_row+1:3,"unit"] <- "g/m3"   #currently not allowed to define units
    # config[idx_row+1:3,"home"] <- config$home[idx_row]
    # config[idx_row+1,"room"] <- "LIV"
    # config[idx_row+2,"room"] <- "BED"
    # config[idx_row+3,"room"] <- "KIT"
    
    ## check config file and run annex analysis
    config <- annex_check_config(config)  #if error check if too many entries were created

    prepared_df <- annex_prepare(raw_df, config, quiet=TRUE)
    
    annex_df <- annex(
        RH + T + CO2 + PM25 + VOC + Other ~ datetime | study + home + room,
        data=prepared_df, tz="Europe/London"
    )
                      
    stats <- annex_stats(annex_df, format="wide")
    
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
    meta.study$Contact <- "Gráinne McGill"
    meta.study$Institution <- "University of Strathclyde"
    meta.study$Year.of.first.publication <- 2020
    meta.study$Publications <- "https://doi.org/10.1038/s41598-020-68809-2"
    meta.study$`Additional.information/comments` <- "Funded by AHRC"
    writeData(wb=meta.wb, sheet = "META-Study", x = meta.study, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
    
    
    # load file with meta data
    meta.data <- read_excel("../data/GBR/AMR_study/meta/Metadata template [AMR study].xlsx", sheet="META-Home")
    meta.data <- meta.data[!is.na(meta.data$ID),] #remove empty (NA) line
    
    # META-home
    meta.home <- readWorkbook(meta.wb, sheet="META-Home")
    meta.home[1,2:18] <- meta.data[meta.data$ID==homeID,2:18]
    meta.home$`Location:.Country` <- "GBR"
    meta.home$Airtightness.normalization.value <- "Envelope area specific in [m3/h/m2]"
    writeData(wb=meta.wb, sheet = "META-Home", x = meta.home, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
    
    # META-room
    meta.room <- readWorkbook(meta.wb, sheet="META-Room")
    meta.room$Fresh.air.supply.in.measurement.location <- lapply(meta.room$ID, SupAirMap, ven=meta.home$Ventilation.type[grepl(name,meta.home$ID)], ven2=meta.home$Comment.Vent..Type[grepl(name,meta.home$ID)])
    writeData(wb=meta.wb, sheet = "META-Room", x = meta.room, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
    
    # META-variable
    meta.var <- readWorkbook(meta.wb, sheet="META-Variable")
    meta.var$Variable.additional.information[grepl("Other",meta.var$ID)] <- "Absolute humidity"
    # units
    meta.var$Variable.unit[grepl("Other",meta.var$ID)] <- "g/m3"
    meta.var$Variable.unit[grepl("VOC",meta.var$ID)] <- "ppb"
    # device info
    meta.var$Measurement.device[grepl("-VOC",meta.var$ID)] <- "Foobot"
    meta.var$Measurement.device[grepl("-PM25",meta.var$ID)] <- "Foobot"
    meta.var$Measurement.device[grepl("-T",meta.var$ID)] <- "Tinytag"
    meta.var$Measurement.device[grepl("-RH",meta.var$ID)] <- "Tinytag"
    meta.var$Measurement.device[grepl("-CO2",meta.var$ID)] <- "Tinytag"
    #meta.var$Measurement.device[grepl("-Other",meta.var$ID)] <- "calculated"
    meta.var$`Comments.(e.g.,.sensor.location)`[grepl("-Other",meta.var$ID)] <- "calculated from T, RH"
    
    writeData(wb=meta.wb, sheet = "META-Variable", x = meta.var, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
    
    # write meta information to annex xls
    saveWorkbook(meta.wb, fn_out, overwrite = TRUE)
}
