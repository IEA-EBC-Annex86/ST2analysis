## IEA EBC Annex 86 process DNK (Gabriel Beko) data
## Gabriel Rojas 05-2024, update 07-2024
## _multi: to write multiple home into one xls
## and run multiple raw data files
##----------------------

# install annex package if needed
library('remotes')
install_github("IEA-EBC-Annex86/annex")
#----------------

# load needed packages, possible redundant
library("readxl")
library("annex")
library("openxlsx")
#library("RDCOMClient")
# check if annex package is up to date 0.2-12 and update if needed
sessionInfo()
#----------------

## Set the working directory
setwd("C:/Daten/A86/AP3_AP4/src")
# load helper "functions" 
sapply(dir("helper", full.names=TRUE), source)

## Definitions for loading t,RH,CO2 timeseries datafile
# RDCOMClient (see helper function) only works with absolute paths (in my case Win10)!? 
dir_cfg <- c("C:\\Daten\\A86\\AP3_AP4\\data\\DNK\\IECH\\cfg")
dir_dat <- c("C:\\Daten\\A86\\AP3_AP4\\data\\DNK\\IECH\\data")
dir_dat_ts <- c("C:\\Daten\\A86\\AP3_AP4\\data\\DNK\\IECH\\data\\CO2TRH")
dir_met <- c("C:\\Daten\\A86\\AP3_AP4\\data\\DNK\\IECH\\meta")
dir_out <- c("C:\\Daten\\A86\\AP3_AP4\\out\\DNK\\IECH")
dir.create(dir_out, recursive=TRUE)

#f_dat1 <- "IECH study Converted raw data homes 52-1178.xlsx"
f_cfg1 <- "DNK_IECH_data_config1.xlsx"
f_dat2 <- "IECH study DATASET CaseBase FINAL3.xlsx"
f_cfg2 <- "DNK_IECH_data_config_SVOC2.xlsx"
f_met <- "DATASET CaseBase FINAL3.xlsx"
#f_met2 <- "Room Occupancy.xlsx"    # not needed info included in f_met

#fn_dat1 <- paste0(dir_dat, "\\", f_dat1)
fn_cfg1 <- paste0(dir_cfg, "\\", f_cfg1)
fn_dat2 <- paste0(dir_dat, "\\", f_dat2)
fn_cfg2 <- paste0(dir_cfg, "\\", f_cfg2)
fn_met <- paste0(dir_dat, "\\", f_met)
#fn_met2 <- paste0(dir_dat, "\\", f_met2)

# define ranges
def.range.CO2="E3:E1000"
def.range.T="L3:L1000"
def.range.RH="M3:M1000"
def.range.dt="B3:B1000"

## Load files/sheets that are same/needed for all homes
# load sheet with SVOC and allergens data
data.SVOC <- read_excel(fn_dat2, sheet = "full C-B datatset CORRECT", range="A1:AF501")
data.ACR <- read_excel(fn_dat2, sheet = "full C-B datatset CORRECT", range="R1:S501")

## META data: load sheet/range and create columns with req'd info
meta.ID <- read_excel(fn_met, sheet="Home Inspection data", range="A1:A501" )
meta.ID <- cbind(meta.ID, read_excel(fn_met, sheet="C-B Questionnaire data CORRECT", range="A2:A501", col_names = "ID2"))
meta.ID <- cbind(meta.ID, read_excel(fn_met, sheet="full C-B datatset CORRECT", range="B2:B501", col_names = "ID3"))
# ensure that same rows on different sheets correspond to same IDs
print(paste0("nr of rows where ID in different sheets dont match: ", sum(meta.ID$ID!=meta.ID$ID2)))
print(paste0("nr of rows where ID in different sheets dont match: ", sum(meta.ID$ID!=meta.ID$ID3)))
# read different files and ranges
meta.In_HoVe <- read_excel(fn_met, sheet="Home Inspection data", range="OM1:OV501" )
meta.In_HoVe <- cbind(meta.In_HoVe, read_excel(fn_met, sheet="Home Inspection data", range="NM1:NR501" ))
meta.Qu_HoVe <- read_excel(fn_met, sheet="C-B Questionnaire data CORRECT", range="HG1:HL501" )  #Question 78_1 - 78_6
meta.Qu_HoVe <- cbind(meta.Qu_HoVe, read_excel(fn_met, sheet="C-B Questionnaire data CORRECT", range="HM1:HR501" )) # Question 79_1 - 79_6
meta.Qu_Bld <- read_excel(fn_met, sheet="C-B Questionnaire data CORRECT", range="DS1:DV501" )
meta.Vol_ACR_Occ <- read_excel(fn_met, sheet="full C-B datatset CORRECT", range="AJ1:AL501" )
#meta.Occ <- read_excel(fn_met2, range="A1:B501" )
# change column names for easier handling (tbd: confirm correct interpretation)
names(meta.In_HoVe)[1:10] <- c("Kit_ReHood", "Kit_ExHood", "MEV", "MVHR", "Bath_Gen", "Bath_Gril", "Bath_Fan", "Bath_Win", "Bath_Bal", "Oth")
names(meta.In_HoVe)[11:16] <- c("Room_NoDev", "Room_VenOpen_WallWin", "Room_NatVen_chim", "Room_DoorSlot", "Room_Mech", "Room_Oth")
names(meta.Qu_HoVe)[1:6] <- c("Gri", "Win", "Kit_Exh", "Bath_Exh", "Mech", "Oth")
names(meta.Qu_HoVe)[7:12] <- c("Room_Win", "Room_VenOpenCeil", "Room_VenOpen_WallWin", "Room_ExhVen_CeilWall", "Room_VenSys", "Oth")
names(meta.Qu_Bld)[1:4] <- c("BldType","Size","unknown","ConstYear") 
names(meta.Vol_ACR_Occ)[1:3] <- c("Vol","ACR","Occ") 

## optional (first run) checking ventilation type
# check_ventype(meta.In_HoVe, meta.Qu_HoVe)  # Q&D: run/load function at end script first

## assign ventilation type and comment on home level
#meta.data <- data.frame(ventype=rep(NA,nrow(meta.In_HoVe))) 
meta.data <- data.frame(ID=meta.ID$ID)
#meta.data$ventype <- "Window airing (not designed)" #default-assumption: window ventilated
# mechanical
idx.mech <- meta.In_HoVe$MEV==1 | meta.In_HoVe$MVHR==1
meta.data$ventype[idx.mech] <- "Mechanical ventilation"
meta.data$comventype <- NA
meta.data$comventype[meta.In_HoVe$MEV==1] <- "MEV"
meta.data$comventype[meta.In_HoVe$MVHR==1] <- "Exhaust and supply air system"
print(paste0("Nr with Mechanical Ventilation: ", sum(meta.data$ventype=="Mechanical ventilation", na.rm=TRUE)))

# natural (designed, i.e. has some openings or a chimney)
idx.nat <- (meta.In_HoVe$Room_VenOpen_WallWin==1 |
                meta.In_HoVe$Room_NatVen_chim==1) &
                meta.In_HoVe$MEV==0 &
                meta.In_HoVe$MVHR==0 &
                meta.In_HoVe$Room_Mech==0 &
                meta.In_HoVe$Room_NoDev==0 &
                meta.In_HoVe$Bath_Fan==0
meta.data$ventype[idx.nat] <- "Natural ventilation (designed)"
meta.data$comventype[idx.nat] <- "Home inspections found: Openings in the outer wall or window construction OR Device for natural ventilation (through chimney) in the room AND no fan in the bath AND no MEV, MVHR or other mechanical system in general."
meta.data$roomsupply[meta.In_HoVe$Room_VenOpen_WallWin==1] <- "Through permanent opening to the exterior (grille, .)"
  
print(paste0("Nr with Natural (designed) Ventilation: ", sum(meta.data$ventype=="Natural ventilation (designed)", na.rm=TRUE)))
# hybrid
idx.hyb <- (meta.In_HoVe$Room_VenOpen_WallWin==1 |
              meta.In_HoVe$Room_NatVen_chim==1) &
              meta.In_HoVe$MEV==0 &
              meta.In_HoVe$MVHR==0 &
              meta.In_HoVe$Room_Mech==0 &
              meta.In_HoVe$Room_NoDev==0 &
              meta.In_HoVe$Bath_Fan==1
meta.data$ventype[idx.hyb] <- "Hybrid/mixed mode ventilation"
meta.data$comventype[idx.hyb] <- "Home inspections found: Openings in the outer wall or window construction OR Device for natural ventilation (through chimney) in the room AND a fan in the bath AND no MEV, MVHR or other mechanical system in general."
print(paste0("Nr with Hybrid Ventilation: ", sum(meta.data$ventype=="Hybrid/mixed mode ventilation", na.rm=TRUE)))
# window
idx.win <- meta.In_HoVe$Room_VenOpen_WallWin==0 &
            meta.In_HoVe$Room_NatVen_chim==0 &
            meta.In_HoVe$MEV==0 &
            meta.In_HoVe$MVHR==0 &
            meta.In_HoVe$Room_Mech==0 &
            meta.In_HoVe$Room_NoDev==1
meta.data$ventype[idx.win] <- "Window airing (not designed)"
meta.data$comventype[idx.win] <- "Home inspections found: No openings in the outer wall or window construction AND No device for natural ventilation (through chimney) in the room AND no MEV, MVHR or other mechanical system in general. Additionally it was reported that there was no ventilation device in the room."
print(paste0("Nr with Window Ventilation: ", sum(meta.data$ventype=="Window airing (not designed)", na.rm=TRUE)))
print(paste0("Nr with defined / undefined Venttype: ", sum(!is.na(meta.data$ventype)), " / ", sum(is.na(meta.data$ventype))))

# assign undefined ventilation types on home level
checkundefined_df <- meta.In_HoVe[is.na(meta.data$ventype),]
idx.mech2 <- is.na(meta.data$ventype) & meta.In_HoVe$Room_Mech==1
sum(idx.mech2, na.rm=TRUE)
meta.data$ventype[idx.mech2] <- "Mechanical ventilation"
meta.data$comventype[idx.mech2] <- "Home inspections found some sort of mechanical ventilation in childs room."
idx.nat2 <- is.na(meta.data$ventype) & grepl("*window*", meta.In_HoVe$Room_Oth) & meta.In_HoVe$Bath_Fan==0
sum(idx.nat2, na.rm=TRUE)
meta.data$ventype[idx.nat2] <- "Natural ventilation (designed)"
meta.data$comventype[idx.nat2] <- "Home inspections reported slot in window of childs room."  
checkundefined_df <- meta.In_HoVe[is.na(meta.data$ventype),]
# the rest has partly contradicting reports, but excludes possibility that they have a mechanical system, therefore assign window airing
idx.win2 <- is.na(meta.data$ventype)
meta.data$ventype[idx.win2] <- "Window airing (not designed)"
meta.data$comventype[idx.win2] <- "Home inspection reports not fully clear, but indicate window airing ventilation." 

# Assign fresh air supply on room level
meta.data$roomsupply <- NA
meta.data$comroom <- NA
# all with window airing
meta.data$roomsupply[idx.win|idx.win2] <- "Through occupant operated window only"
# all with exterior grille (natural, hybrid or MEV)
meta.data$roomsupply[meta.In_HoVe$Room_VenOpen_WallWin==1] <- "Through permanent opening to the exterior (grille, .)"
# all with mechanical supply
meta.data$roomsupply[meta.In_HoVe$MVHR==1 & meta.In_HoVe$Room_Mech==1] <- "Through mechanically driven supply air (no recirculated air)"
meta.data$roomsupply[meta.In_HoVe$MVHR==1 & meta.In_HoVe$Room_Mech==0] <- "Other"
meta.data$comroom[meta.In_HoVe$MVHR==1 & meta.In_HoVe$Room_Mech==0] <- "Home inspection reported that there was no mech ventilation in the room despite the report of supply and exhaust system in home."
# all with mechanical in room
meta.data$roomsupply[meta.In_HoVe$MVHR==0 & meta.In_HoVe$Room_Mech==1] <- "Other"
meta.data$comroom[meta.In_HoVe$MVHR==0 & meta.In_HoVe$Room_Mech==1] <- "Home inspection reported mechanical ventilation in room, but not clear if supply or exhaust. No Sup and Exh ventilation reported in home."
# special cases that report MEV but no grille
meta.data$roomsupply[meta.In_HoVe$MEV==1 & meta.In_HoVe$Room_VenOpen_WallWin==0] <- "Other"
meta.data$comroom[meta.In_HoVe$MEV==1 & meta.In_HoVe$Room_VenOpen_WallWin==0] <- "Home inspection reported no vent. opening or grille"
# special cases still undefined that reported "Other" with slit in window
idx.oth <- is.na(meta.data$roomsupply) & grepl("*window*", meta.In_HoVe$Room_Oth)
meta.data$roomsupply[idx.oth] <- "Through permanent opening to the exterior (grille, .)"
meta.data$comroom[idx.oth] <- "Home inspection reported slit in window (in Other)."
# special cases still undefined with chimney natural vent
idx.oth <- is.na(meta.data$roomsupply) & meta.In_HoVe$Room_NatVen_chim==1
meta.data$roomsupply[idx.oth] <- "Other"
meta.data$comroom[idx.oth] <- "Home inspection device for natural ventilation (through  chimney) in room."

print(paste0("Nr. of undefined fresh air supplies in rooms: ", sum(is.na(meta.data$roomsupply))))


# type of building
meta.data$bldtype <- NA
meta.data$bldtype[meta.Qu_Bld$BldType==1] <- "Single family house (SFH)"
meta.data$bldtype[meta.Qu_Bld$BldType==2] <- "Multi-family house (MFH)"
meta.data$addinfo[meta.Qu_Bld$BldType==2] <- "declared as row house/multifamily house (study did not differentiate)"
meta.data$bldtype[meta.Qu_Bld$BldType==3] <- "Apartment block (AB)"
meta.data$bldtype[meta.Qu_Bld$BldType==4] <- "Single family house (SFH)"
meta.data$addinfo[meta.Qu_Bld$BldType==4] <- "declared as farmhouse"
meta.data$bldtype[meta.Qu_Bld$BldType==5] <- "Undefined/Not known"
sum(is.na(meta.data$bldtype))
meta.data$bldtype[is.na(meta.data$bldtype)] <- "Undefined/Not known"
sum(is.na(meta.data$bldtype))
# size of building
meta.data$size <- NA
meta.data$size[meta.Qu_Bld$Size==1] <- "<50 m2"
meta.data$size[meta.Qu_Bld$Size==2] <- "50-69 m2"
meta.data$size[meta.Qu_Bld$Size==3] <- "70-89 m2"
meta.data$size[meta.Qu_Bld$Size==4] <- "90-109 m2"
meta.data$size[meta.Qu_Bld$Size==5] <- "110-139 m2"
meta.data$size[meta.Qu_Bld$Size==6] <- ">= 140 m2"
sum(is.na(meta.data$size))
# year of construction
meta.data$year <- NA
meta.data$year[meta.Qu_Bld$ConstYear==1] <- "<1940"
meta.data$year[meta.Qu_Bld$ConstYear==2] <- "1941-1960"
meta.data$year[meta.Qu_Bld$ConstYear==3] <- "1961-1970"
meta.data$year[meta.Qu_Bld$ConstYear==4] <- "1971-1976"
meta.data$year[meta.Qu_Bld$ConstYear==5] <- "1977-1983"
meta.data$year[meta.Qu_Bld$ConstYear==6] <- "1984-1993"
meta.data$year[meta.Qu_Bld$ConstYear==7] <- "1994-2000"
meta.data$year[meta.Qu_Bld$ConstYear==8] <- ">2000"
sum(is.na(meta.data$year))


## Run Annex 86 analysis ------------------------------------------------------
# Read and prepare config object
config <- read_excel(fn_cfg1)
config <- subset(config, process == TRUE) # Remove all rows process = FALSE
## add SVOC and allergen measurement results
config2 <- read_excel(fn_cfg2)

annex_check_config(config) #optional check
annex_check_config(config2) #check won't be positiv, because lacking datetime (no timeseries) 

## loop over multiple timeseries (ts) input files-----------------------------
fn_ts <- list.files(path=dir_dat_ts)
i_tot <- 0

for (f in 14:length(fn_ts)) { 
  fn_dat1 <- paste0(dir_dat_ts, "\\", fn_ts[f])
  # read the names of the available excel sheets
  def.sheets <- excel_sheets(fn_dat1)
  def.sheets <- def.sheets[!grepl("Sheet",def.sheets)]
  
  stats <- data.frame()
  
  for (i in 1:length(def.sheets)) {   #length(def.sheets)
    ID <- def.sheets[i]
    
    data.df0 <- read_excel(fn_dat1,sheet=ID, range=def.range.dt, col_names="dt")
    data.df1 <- read_excel(fn_dat1,sheet=ID, range=def.range.CO2, col_names="CO2")
    data.df2 <- read_excel(fn_dat1,sheet=ID, range=def.range.T, col_names="T")
    data.df3 <- read_excel(fn_dat1,sheet=ID, range=def.range.RH, col_names="RH")
    data.df <- data.frame(data.df0,data.df1,data.df2,data.df3)
    # remove all the redundant NA lines
    data.df <- data.df[!is.na(data.df$dt),]
    
    # rename the home to include home number from sheet name
    config$home[2:4] <- paste0("Home_",ID)
    
    #plot(df$dt,df$CO2, type="l")
    
    # Prepare annex object
    prepared_df <- annex_prepare(data.df, config, quiet = TRUE)
    annex_df    <- annex(T + RH + CO2 ~ datetime | study + home + room,
                         data = prepared_df, tz = "Europe/Copenhagen")
    
    # run standard analysis
    stats <- rbind(stats, annex_stats(annex_df, format = "wide"))
    
    # remove the "all" entries in year and month (since only one year and month -> revision of annex package needed)
    idx2rm <- stats$year == "all" | stats$month == "all"
    stats <- stats[!idx2rm,]
  
    # add SVOC data manually
    # add new variable names to factor definition (categorical variable)
    stats_no <- nrow(stats)
    if (sum(levels(stats$variable)!=config2$variable[1])){
      levels(stats$variable) <- c(levels(stats$variable), config2$variable)
    }
    # fill out stats for SVOC and allergens data
    idxhome <- data.SVOC$code==ID
    for (j in 1:length(config2$column)){
      # copy first columns with study, home, room. year and month to new row
      stats[stats_no+j,(1:5)] <- stats[stats_no,(1:5)]
      # tod = all
      stats[stats_no+j,"tod"] <- "all"
      # name of pollutant
      stats[stats_no+j,"variable"] <- config2$variable[j]
      # mean values
      stats[stats_no+j,"Mean"] <- data.SVOC[idxhome,config2$column[j]]
      stats[stats_no+j,"N"] <- 1
    }
    
  }
  
  
  ## write xls results file def.sheets[i]
  fn_res = paste0(dir_out, "\\IECH_", def.sheets[1], "_", def.sheets[length(def.sheets)] ,".xlsx")
  annex_write_stats(stats, file = fn_res, user = 011)
  
  # open & close (read & save) the file using Excel to avoid/fix bug of openxlsx
  read_save_xlsx(fn_res)  # Q&D: run/load function at end script first
  
  ## load result file and write meta data ----------------------------------------
  meta.wb <- loadWorkbook(fn_res)
  
  for (i in 1:length(def.sheets)) {   #length(def.sheets)
    ID <- def.sheets[i]
  
    # META-study
    if (i==1) {
      meta.study <- readWorkbook(meta.wb, sheet="META-Study")
      meta.study$Contact <- "Gabriel Bekö"
      meta.study$Institution <- "Danish Technical University (DTU)"
      meta.study$Year.of.first.publication <- 2010
      meta.study$Publications <- "https://doi.org/10.1016/j.buildenv.2010.04.014"
      meta.study$`Additional.information/comments` <- "measurements for 2+ nights in 500 children's bedrooms"
    }
    
    # META-home
    if (i==1) {meta.home <- readWorkbook(meta.wb, sheet="META-Home")}
    meta.home$`Location:.Country`[i] <- "DNK"
    meta.home$`Location:.City`[i] <- "Odense"
    meta.home$Ventilation.type[i]<- meta.data$ventype[meta.data$ID==ID]
    meta.home$Comment.Vent..Type[i] <- meta.data$comventype[meta.data$ID==ID]
    meta.home$Type.of.building[i] <- meta.data$bldtype[meta.data$ID==ID]
    meta.home$`Size.of.home./.TFA.[m^2]`[i] <- meta.data$size[meta.data$ID==ID]
    meta.home$`Year.of.contruction./.major.renovation.(four.digit.year)`[i]<- meta.data$year[meta.data$ID==ID]
    meta.home$`Additional.information/comments`[i] <- meta.data$addinfo[meta.data$ID==ID]
    
    # META-room
    if (i==1) {meta.room <- readWorkbook(meta.wb, sheet="META-Room")}
    meta.room$`Additionial.room.information.(e.g.,.ceiling.height,.with.atrium)`[i] <- paste0("Vol [m3]: ", meta.Vol_ACR_Occ$Vol[meta.data$ID==ID])
    meta.room$`Occupancy:.Type`[i] <- "child"
    meta.room$`Occupancy:.Number`[i] <- meta.Vol_ACR_Occ$Occ[meta.data$ID==ID]
    meta.room$Fresh.air.supply.in.measurement.location[i] <- meta.data$roomsupply[meta.data$ID==ID]
    meta.room$`Ventilation.rate.(room;.[l/s])`[i] <- round(meta.Vol_ACR_Occ$ACR[meta.data$ID==ID] * meta.Vol_ACR_Occ$Vol[meta.data$ID==ID] / 3.6, 2)
    meta.room$Method.of.vent..rate.determination[i] <- "Calculated (e.g. from CO2, passive tracer gas)"
    
    # META-variable
    if (i==1) {meta.var <- readWorkbook(meta.wb, sheet="META-Variable")}
    #rcount <- nrow(meta.var)-1
    for (v in 1:length(meta.var$ID)){
      varname = unlist(strsplit(meta.var$ID, split="-")[[v]][5])
      if (grepl("Other",varname)){
        meta.var$Variable.additional.information[v] <- config2$`additional information`[config2$variable==varname]
        meta.var$Variable.unit[v] <- config2$unit[config2$variable==varname]
        meta.var$Measurement.device[v] <- config2$`meas device`[config2$variable==varname]
        meta.var$`Comments.(e.g.,.sensor.location)`[v] <- config2$`meas comments`[config2$variable==varname]
      }
    }
  
    # optional: check xls file if all required meta information was filled out
    #heck <- annex_validate(fn_res, user = 011)
    
  }
  
  # write meta information to annex xls
  writeData(wb=meta.wb, sheet = "META-Study", x = meta.study, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  writeData(wb=meta.wb, sheet = "META-Home", x = meta.home, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  writeData(wb=meta.wb, sheet = "META-Room", x = meta.room, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  writeData(wb=meta.wb, sheet = "META-Variable", x = meta.var, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  saveWorkbook(meta.wb, fn_res, overwrite = TRUE)
  print(paste0(fn_res, "with ", i, " homes written."))
  i_tot <- i_tot + i
}

print(paste0("Finished: ", f, " result files generated, with a total of ", i_tot, " homes."))

##----------------------------------------------------------------------------------
## Plot (optional) 
# plot(df$dt,df$CO2, type="l")
# plot(df$dt,df$T, type="l")
# plot(df$dt,df$RH, type="l")

## -----------------------------------------------------------------
## optional function checking for ventilation types
# mech ventilation
check_ventype <- function(meta.In_HoVe, meta.Qu_HoVe){
  Nr.MEV <- sum(meta.In_HoVe$MEV==1 & meta.In_HoVe$MVHR==0, na.rm=TRUE)
  Nr.MVHR <- sum(meta.In_HoVe$MEV==0 & meta.In_HoVe$MVHR==1, na.rm=TRUE)
  print(paste0("Nr. of homes with inspected MEV: ", Nr.MEV, " MVHR: ", Nr.MVHR))
  Nr.MEV2 <- sum(meta.Qu_HoVe$Mech==1 | meta.Qu_HoVe$Bath_Exh==1, na.rm=TRUE)
  Nr.MVHR2 <- sum(meta.Qu_HoVe$Mech==1, na.rm=TRUE)
  print(paste0("Nr. of homes with reported MEV: ", Nr.MEV2, " MVHR: ", Nr.MVHR2))
  Nr.MEV3 <- sum(meta.In_HoVe$MEV==1 & meta.In_HoVe$MVHR==0 & (meta.Qu_HoVe$Mech==1 | meta.Qu_HoVe$Bath_Exh==1), na.rm=TRUE)
  Nr.MVHR3 <- sum(meta.In_HoVe$MEV==0 & meta.In_HoVe$MVHR==1 & meta.Qu_HoVe$Mech==1, na.rm=TRUE)
  print(paste0("Nr. with inspected and reported MEV: ", Nr.MEV3, " MVHR: ", Nr.MVHR3))
  
  Nr.r_Mech <- sum(meta.In_HoVe$Room_Mech==1, na.rm=TRUE)
  print(paste0("Nr. with inspected mechanical vent in room: ", Nr.r_Mech))
  Nr.MVHR4 <- sum(meta.In_HoVe$MVHR==1 & meta.In_HoVe$Room_Mech==1, na.rm=TRUE)
  print(paste0("Nr. with inspected mechanical vent in home and room: ", Nr.MVHR4))
  # contradicting info: 6 rooms with mechanical ventilation thereof only one with balanced (or MEV) system in home
  # -> assign only the inspected answers for MEV and MVHR assignment at home level
  
  # natural ventilation
  Nr.gril <- sum(meta.In_HoVe$Bath_Gril==1, na.rm=TRUE)
  print(paste0("Nr. with inspected grilles (bath): ", Nr.gril))
  Nr.gril2 <- sum(meta.Qu_HoVe$Gri==1, na.rm=TRUE)
  print(paste0("Nr. with reported vent opening / grilles: ", Nr.gril2))
  Nr.gril3 <- sum(meta.In_HoVe$Bath_Gril==1 & meta.Qu_HoVe$Gri==1, na.rm=TRUE)
  print(paste0("Nr. with reported and inspected grilles: ", Nr.gril3))
  
  Nr.r_gril <- sum(meta.In_HoVe$Room_VenOpen_WallWin==1, na.rm=TRUE)
  print(paste0("Nr. with inspected vent opening / grilles in wall or window (room): ", Nr.r_gril))
  Nr.r_gril2 <- sum(meta.Qu_HoVe$Room_VenOpen_WallWin==1 | meta.Qu_HoVe$Room_VenOpenCeil ==1 , na.rm=TRUE)
  print(paste0("Nr. with reported vent opening / grilles in wall, window or ceiling (room): ", Nr.r_gril2))
  Nr.r_gril3 <- sum(meta.In_HoVe$Room_VenOpen_WallWin==1 & (meta.Qu_HoVe$Room_VenOpen_WallWin==1 | meta.Qu_HoVe$Room_VenOpenCeil ==1) , na.rm=TRUE)
  print(paste0("Nr. with inspected and reported vent opening / grilles in wall, window or ceil (room): ", Nr.r_gril3))
  
  Nr.rh_gril <- sum(meta.In_HoVe$Bath_Gril==1 & meta.In_HoVe$Room_VenOpen_WallWin==1, na.rm=TRUE)
  print(paste0("Nr. with inspected vent opening / grille in bath and rooms: ", Nr.rh_gril))
  
  Nr.r_nat <- sum(meta.In_HoVe$Room_NatVen_chim==1, na.rm=TRUE)
  print(paste0("Nr. with inspected natural chimney vent (room): ", Nr.r_nat))
  # -> assign to natural ventilation those that have vent openings or chimneys in rooms and no insprected MEV or MVHR nor bath fan 
  Nr.natural <- sum(( meta.In_HoVe$Room_VenOpen_WallWin==1 |
                        meta.In_HoVe$Room_NatVen_chim==1) &
                      meta.In_HoVe$MEV==0 &
                      meta.In_HoVe$MVHR==0 &
                      meta.In_HoVe$Room_Mech==0 &
                      meta.In_HoVe$Room_NoDev==0 &
                      meta.In_HoVe$Bath_Fan==0 , na.rm=TRUE)
  print(paste0("Nr. with inspected vent opening or chimney in rooms and no inspected mech system / bath fan: ", Nr.natural))
  
  # hybrid / mixed mode: similar like nat vent but with bath fans
  Nr.bathfan <- sum(meta.In_HoVe$Bath_Fan==1, na.rm=TRUE)
  print(paste0("Nr. with inspected bath fans: ", Nr.bathfan))
  Nr.IntExh <- sum(meta.In_HoVe$Bath_Fan==1 & meta.In_HoVe$Room_VenOpen_WallWin==1, na.rm=TRUE)
  print(paste0("Nr. with inspected bath fans and vent opening in rooms: ", Nr.IntExh))
  # -> assign to hybrid ventilation those that have vent openings or chimneys in rooms and no inspected MEV or MVHR but a bath fan 
  Nr.hybrid <- sum((meta.In_HoVe$Room_VenOpen_WallWin==1 |
                      meta.In_HoVe$Room_NatVen_chim==1) &
                     meta.In_HoVe$MEV==0 &
                     meta.In_HoVe$MVHR==0 &
                     meta.In_HoVe$Room_Mech==0 &
                     meta.In_HoVe$Room_NoDev==0 &
                     meta.In_HoVe$Bath_Fan==1  , na.rm=TRUE)
  print(paste0("Nr. with inspected vent opening or chimney in rooms and no inspected mech system but a bath fan: ", Nr.hybrid))
  
  
  # window airing
  Nr.b_win <- sum(meta.In_HoVe$Bath_Win==1 &
                    meta.In_HoVe$Bath_Gril==0 &
                    meta.In_HoVe$Bath_Fan==0 &
                    meta.In_HoVe$Bath_Bal==0, na.rm=TRUE)
  print(paste0("Nr. with windows in bath (insp): ", Nr.b_win))
  Nr.rb_win <- sum(meta.In_HoVe$Bath_Win==1 &
                     meta.In_HoVe$Bath_Gril==0 &
                     meta.In_HoVe$Bath_Fan==0 &
                     meta.In_HoVe$Bath_Bal==0 &
                     meta.In_HoVe$Room_NoDev==1, na.rm=TRUE)
  print(paste0("Nr. with windows in bath and no vent devices in rooms(insp): ", Nr.rb_win))
  Nr.r_win <- sum(meta.In_HoVe$Room_NoDev==1, na.rm=TRUE)
  print(paste0("Nr. that have no vent devices in rooms(insp): ", Nr.r_win))
  # -> window airing homes defined as no device in room and no MEV/MVHR/Mech Sys (but bath fan ok)"
  Nr.win <- sum(meta.In_HoVe$Room_VenOpen_WallWin==0 &
                  meta.In_HoVe$Room_NatVen_chim==0 &
                  meta.In_HoVe$MEV==0 &
                  meta.In_HoVe$MVHR==0 &
                  meta.In_HoVe$Room_Mech==0 &
                  meta.In_HoVe$Room_NoDev==1 , na.rm=TRUE)
  print(paste0("Nr. with inspected window vent only in rooms: ", Nr.win))
  
  print(paste0("Total Nr. of homes with defined ventilation type: ", Nr.MEV+Nr.MVHR+Nr.natural+Nr.hybrid+Nr.win))
  
  # # other checks also using questionaire (reported) data
  # Nr.PotNat <- sum(meta.In_HoVe$Bath_Gril==1 & 
  #                    meta.In_HoVe$Bath_Fan==0 & 
  #                    meta.In_HoVe$MEV==0 & meta.In_HoVe$MVHR==0 & 
  #                    meta.Qu_HoVe$Gri==1 & meta.Qu_HoVe$Bath_Exh==0 & 
  #                    meta.Qu_HoVe$Mech==0, na.rm=TRUE)
  # print(paste0("Nr. of homes with potential natural ventilation (reported or inspected grilles and no mech. ventilation): ", Nr.PotNat))
  # remains questionable if those home have "designed natural ventilation concepts"
}

# other one time checks
check_roomsupply <- meta.In_HoVe[meta.In_HoVe$MEV==1,]
check_roomsupply <- meta.In_HoVe[meta.In_HoVe$MVHR==1,]
check_roomsupply2 <- meta.In_HoVe[idx.mech|idx.mech2,]
check_roomsupply2 <- meta.In_HoVe[idx.nat|idx.nat2,]

# check if Room occupancy file data is same as in other dataset
check_Occ <- meta.Occ[order(meta.Occ$`Family code`),]
check_Occ$check <- meta.Vol_ACR$`No.of persons sleeping in room`

print(paste0("nr of rows where IDs dont match: ", sum(check_Occ$`Family code`!= meta.ID$ID)))
print(paste0("nr of rows where room occupancies dont match: ", sum(check_Occ$`No.of persons`!= meta.Vol_ACR$`No.of persons sleeping in room`)))
#-> room cocupancy file not needed.
