## IEA EBC Annex 86 processing for Sinfonia
# based on Andreas Frei, adapted Gabriel Rojas
# July 2024

library("annex")
library("dplyr")
library("readxl")
# with this new openxlsx package no work around, install if needed
# library("remotes")
# install_github("ycphs/openxlsx")
library("openxlsx")

## Set the working directory
setwd("C:/Daten/A86/AP3_AP4/src")
setwd("C:/Users/Gabriel/Daten/A86/AP3_AP4/src")


dir_cfg <- c("../data/AUT/sinfonia/prepro/cfg")
dir_dat <- c("../data/AUT/sinfonia/prepro/data")
dir_met <- "../data/AUT/sinfonia/meta"
dir_out <- c("../out/AUT/sinfonia")
dir.create(dir_out, recursive=TRUE)

set_nrperfile <- 20
fns <- matrix(list.files(path=dir_cfg),ncol=set_nrperfile,byrow=TRUE)
fns[10,6:20] <- NA

fn_met <- paste0(dir_met, "/", "sensoren_v2.xlsx")

## load meta data file
data.meta <- read_excel(fn_met, sheet = "sensor", range="M1:AF1529")
data.meta$homeID <- paste0("B",sprintf("%03g", data.meta$BldgID),"H",sprintf("%03g",data.meta$UnitID))

data.meta$VentType <- "Window airing (not designed)"
data.meta$VentType[data.meta$UnitVentType==1 | data.meta$UnitVentType==2] <- "Mechanical ventilation" 
data.meta$VentComment <- NA
data.meta$VentComment[data.meta$UnitVentType==1] <- "central (building) MVHR"
data.meta$VentComment[data.meta$UnitVentType==2] <- "decentral (dwelling) MVHR"
data.meta$VentRateUnitDesign <- round(data.meta$BldVentRate_calc*data.meta$UnitSize*2.5/3.6, digits=1) # in l/s
data.meta$RoomFreshAirSup <- "Through occupant operated window only"
data.meta$RoomFreshAirSup[data.meta$UnitVentType==1 | data.meta$UnitVentType==2] <- "Through mechanically driven supply air (no recirculated air)"
data.meta$RoomComment <- "Measured in dwelling central location, e.g. hallway"
data.meta$RoomComment[data.meta$UnitVentType==1 | data.meta$UnitVentType==2] <- "Measured in dwelling extract air"


i<-1
j<-1
# loop over files
j_tot <- 0
#for (i in 1:nrow(fns)) {
#for (i in 1:2) {
  
  #loop over homes per file
  stats <- data.frame()
  #for (j in 1:sum(!is.na(fns[i,]))) {
  
    name = gsub(".cfg","",fns[i,j])
    print(name)
    
    fn_dat <- paste(dir_dat, "/", name, ".csv", sep="")
    fn_cfg <- paste(dir_cfg, "/", name, ".cfg", sep="")
    
    raw_df <- read.csv(fn_dat)
    raw_df <- transform(raw_df, tst=as.POSIXct(tst, tz="UTC"))
    config <- read.csv(fn_cfg, na.strings=c("NA", "empty"))
    
    idx <- !is.na(raw_df$i0355_CO2Gehalt)
    
    xx <- raw_df$tst[idx]
    summary(xx)
    yy <- raw_df$i0355_CO2Gehalt[idx]
    summary(yy)
    
    raw2 <- raw_df
    
    raw3 <- raw2 %>%
      drop_na(i0355_CO2Gehalt)
    
    plot(xx, yy, type="l", ylim = c(300,1500))
    yy_cdf <- cume_dist(yy)
    
    plot(yy,yy_cdf, xlim=c(300,1000))

    config <- annex_check_config(config)

    # Prepare annex object
    prepared_df <- annex_prepare(raw_df, config, quiet=TRUE)
    annex_df <- annex(RH + T + CO2 ~ datetime | study + home + room, data=prepared_df, tz="UTC")
    
    # run standard analysis                 
    stats <- rbind(stats,annex_stats(annex_df))
    
  }
  
  ## write xls results file for all j homes
  fn_res = paste0(dir_out, "/", gsub(".cfg","",fns[i,1]), "_", gsub(".cfg","",fns[i,j]) ,".xlsx")
  annex_write_stats(stats, file= fn_res, user=1)
    
  ## load result file for writting meta data
  meta.wb <- loadWorkbook(fn_res)
  
  for (j in 1:sum(!is.na(fns[i,]))) {
    name = gsub(".cfg","",fns[i,j])
    
    # META-study
    #meta.study <- readWorkbook(meta.wb, sheet="META-Study", rowNames = FALSE, colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
    if(j==1) {
      meta.study <- readWorkbook(meta.wb, sheet="META-Study")
      meta.study$Contact <- "Gabriel Rojas"
      meta.study$Institution <- "University of Innsbruck"
      meta.study$Year.of.first.publication <- 2020
      meta.study$Publications <- "J. Bliem, Energy demand and indoor climate monitoring in refurbished residential buildings. MA Thesis, Univ. of Innsbruck."
      meta.study$Links <- "http://www.sinfonia-smartcities.eu/; www.sinfonia-smartcities.eu/en/resources; https://ulb-dok.uibk.ac.at/ulbtirolhs/content/titleinfo/5848129?lang=de"
      meta.study$`Additional.information/comments` <- "Note: In mechanically ventilated dwellings, CO2 was measured in the extract air duct (dwellingwise)."
    }
    # META-home (only one home per j, order same as above loop so ok, it would be nicer to subset using home name)
    if (j==1) {meta.home <- readWorkbook(meta.wb, sheet="META-Home")}
    meta.home$`Location:.Country`[j] <- "AUT"
    meta.home$`Location:.City`[j] <- "Innsbruck"
    meta.home$Ventilation.type[j]<- data.meta$VentType[which(data.meta$homeID==name&data.meta$UseName=="CO2Gehalt")]
    meta.home$Comment.Vent..Type[j] <- data.meta$VentComment[which(data.meta$homeID==name&data.meta$UseName=="CO2Gehalt")]
    if (meta.home$Ventilation.type[j]=="Mechanical ventilation") {
      meta.home$`Ventilation.rate.(entire.home;.[l/s])`[j] <- data.meta$VentRateUnitDesign[which(data.meta$homeID==name&data.meta$UseName=="CO2Gehalt")]
      if (meta.home$`Ventilation.rate.(entire.home;.[l/s])`[j]<12 | meta.home$`Ventilation.rate.(entire.home;.[l/s])`[j]>25) {
        print(paste0("Warning: Vent flow was ",as.numeric(meta.home$`Ventilation.rate.(entire.home;.[l/s])`[j])*3.6, " m3/h in home ", name))
      }
    }
    meta.home$Method.of.vent..rate.determination[j] <- "Design values (from ventilation system design)"
    meta.home$Type.of.building[j] <- "Multi-family house (MFH)"
    meta.home$`Size.of.home./.TFA.[m^2]`[j] <- data.meta$UnitSize[which(data.meta$homeID==name&data.meta$UseName=="CO2Gehalt")]
    meta.home$`Year.of.contruction./.major.renovation.(four.digit.year)`[j]<- "tbc: major renovation 2016"
    meta.home$Type.of.Occupants[j] <- "Social housing"
    meta.home$Energy.standard[j] <- data.meta$Bld_energy_performance_cert[which(data.meta$homeID==name&data.meta$UseName=="CO2Gehalt")]
    meta.home$`Additional.information/comments`[j] <- paste0("Heat demand incl vent.: ", data.meta$BldHeatDemand_inclVent[which(data.meta$homeID==name&data.meta$UseName=="CO2Gehalt")], " kWh/(m2a)")
    
    # META-room (only one room per j)
    if (j==1) {meta.room <- readWorkbook(meta.wb, sheet="META-Room")}
    meta.room$`Additionial.room.information.(e.g.,.ceiling.height,.with.atrium)`[j] <- data.meta$RoomComment[which(data.meta$homeID==name&data.meta$UseName=="CO2Gehalt")]
    meta.room$Fresh.air.supply.in.measurement.location[j] <- data.meta$RoomFreshAirSup[which(data.meta$homeID==name&data.meta$UseName=="CO2Gehalt")]
    #meta.room$`Ventilation.rate.(room;.[l/s])`[i] <-
    #meta.room$Method.of.vent..rate.determination[i] <- 
    
    # # META-variable (various entries per j)
    # if (j==1) {meta.var <- readWorkbook(meta.wb, sheet="META-Variable")}
    # meta.var$Measurement.device[grepl("-T",meta.var$ID)] <- "Tinytag"
    # meta.var$Measurement.device[grepl("-RH",meta.var$ID)] <- "Tinytag"
    # meta.var$Measurement.device[grepl("-CO2",meta.var$ID)] <- "Tinytag"
    # #meta.var$Variable.additional.information[grepl("CO2",varname] <- config2$`additional information`[config2$variable==varname]
    # 
  }

  meta.var <- readWorkbook(meta.wb, sheet="META-Variable")
  # mech. ventilated homes
  filhomes <- meta.home$ID[meta.home$Ventilation.type=="Mechanical ventilation"]
  filvar <- (grepl("-T",meta.var$ID) | grepl("-RH",meta.var$ID)) & sub("^(([^-]*-){2}[^-]*).*","\\1",meta.var$ID) %in% filhomes
  meta.var$Measurement.device[filvar] <- "CMa11, Elvaco"
  meta.var$`Comments.(e.g.,.sensor.location)`[filvar] <- "Positioned in central dwelling location, e.g. hallway"
  filvar <- grepl("-CO2",meta.var$ID) & sub("^(([^-]*-){2}[^-]*).*","\\1",meta.var$ID) %in% filhomes
  meta.var$Measurement.device[filvar] <- "LK+ CO2 V, Thermokon"
  meta.var$`Comments.(e.g.,.sensor.location)`[filvar] <- "Measured in extract air duct of dwelling (combination of all extract air terminals)."
  
  # window airing homes
  filhomes <- meta.home$ID[meta.home$Ventilation.type=="Window airing (not designed)"]
  filvar <- (grepl("-T",meta.var$ID) | grepl("-RH",meta.var$ID)) & sub("^(([^-]*-){2}[^-]*).*","\\1",meta.var$ID) %in% filhomes
  meta.var$Measurement.device[filvar] <- "CMa11w, Elvaco"
  filvar <- grepl("-CO2",meta.var$ID) & sub("^(([^-]*-){2}[^-]*).*","\\1",meta.var$ID) %in% filhomes
  meta.var$Measurement.device[filvar] <- "MBS-122/CO2, Pikkerton"
  filvar <- sub("^(([^-]*-){2}[^-]*).*","\\1",meta.var$ID) %in% filhomes
  meta.var$`Comments.(e.g.,.sensor.location)`[filvar] <- "Positioned in central dwelling location, e.g. hallway"

  # write meta information to annex xls
  writeData(wb=meta.wb, sheet = "META-Study", x = meta.study, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  writeData(wb=meta.wb, sheet = "META-Home", x = meta.home, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  writeData(wb=meta.wb, sheet = "META-Room", x = meta.room, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  writeData(wb=meta.wb, sheet = "META-Variable", x = meta.var, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  saveWorkbook(meta.wb, fn_res, overwrite = TRUE)
  print(paste0(fn_res, "with ", j, " homes written."))
  j_tot <- j_tot + j
  
}
print(paste0("Finished: ", f, " result files generated, with a total of ", j_tot, " homes."))
