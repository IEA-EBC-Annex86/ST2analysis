## IEA EBC Annex 86 pre-process Norway (Maria Justo) data
## Gabriel Rojas 06-2024
## code for new dataset received (email 28.5.2024 + 4.6.2024)
##----------------------

# install annex package if needed
library('remotes')
install_github("IEA-EBC-Annex86/annex")
#----------------

# load needed packages, possible redundant
library("readxl")
library("annex")
library("lubridate") #needed for time zone conversion
library("openxlsx")
library("RDCOMClient")
# check if annex package is up to date 0.2-12 and update if needed
sessionInfo()
#----------------

## Set the working directory
setwd("C:/Daten/A86/AP3_AP4/src")
# load helper "functions" 
sapply(dir("helper", full.names=TRUE), source)

## use new data file received May 28th and June 4th (time is local time, see emial 13.6.)
# first data file received
raw.data0 <- read.csv("../data/NOR/HomeOffice/raw/merged_data.csv",sep=",")
raw.data0$dt <- as.POSIXct(raw.data0$time,format="%d.%m.%Y %H:%M", tz="Europe/Oslo")

# updated data file received
raw.data1 <- read_excel("../data/NOR/HomeOffice/raw/merged_data_v2.xlsx")
attr(raw.data1$time, "tzone") # check to see that it comes in as UTC from xls
raw.data1$dt <- force_tz(raw.data1$time,"Europe/Oslo") #force new time zone without changing values
attr(raw.data1$dt, "tzone") #confirm that it is now changed

# additional data file (hourly avg, as backward average,e.g.15:00 is the average from 14:01 to 15:00)
raw.data2 <- read_excel("../data/NOR/HomeOffice/raw/DATHOURWS_V2.xlsx")
raw.data2$dt <- force_tz(raw.data2$time,"Europe/Oslo") 

## optional: compare different datafiles
idx_sen <- raw.data0$sensor == "1A" & raw.data0$dt<"2020-12-10"
plot(raw.data0$dt[idx_sen],raw.data0$CO2[idx_sen],type="l")
idx_sen1 <- raw.data1$sensor == "1A" & raw.data1$dt<"2020-12-10"
lines(raw.data1$dt[idx_sen1], raw.data1$CO2[idx_sen1], col="blue")
idx_sen2 <- raw.data2$sensor == "A1" & raw.data2$dt<"2020-12-10"
lines(raw.data2$dt[idx_sen2], raw.data2$CO2[idx_sen2], col="red")

plot(raw.data0$dt[idx_sen],raw.data0$PM2.5[idx_sen],type="l")
lines(raw.data1$dt[idx_sen1], raw.data1$PM2.5[idx_sen1], col="blue")
lines(raw.data2$dt[idx_sen2], raw.data2$PM2.5[idx_sen2], col="red")

## choose dataset
# exclude summer data (analyse seperatly), because in merged_data_v2 summer data is 1-hr average winter data 1-min raw data
raw.data <- raw.data1[raw.data1$season=="winter",] #1 min resolution for winter period (sensor <=6) an 1hr resolu for summer data (sensor )

## make meta information a categorical / factor variable (from character)
raw.data$weekday <- as.factor(raw.data$weekday)
raw.data$hour <- as.factor(raw.data$hour)
raw.data$sensor <- as.factor(raw.data$sensor)
raw.data$housetype <- as.factor(raw.data$housetype)
raw.data$Main_room_use <- as.factor(raw.data$Main_room_use)
raw.data$Wood_stove <- as.factor(raw.data$Wood_stove)
raw.data$Pets <- as.factor(raw.data$Pets)
raw.data$Floor_Material <- as.factor(raw.data$Floor_Material)
raw.data$Trickle_Vent_Open <- as.factor(raw.data$Trickle_Vent_Open)
raw.data$Normal_Wind_Stat <- as.factor(raw.data$Normal_Wind_Stat)
raw.data$Bdg_loc <- as.factor(raw.data$Bdg_loc)
raw.data$season <- as.factor(raw.data$season)

## rename home(sensor) names for merged_data_v1: it seems that in winter they were labelled 1A in summer A1, summer misses some homes, why?
levels(raw.data$sensor)
length(levels(raw.data$sensor))
barplot(table(raw.data$sensor))
for (j in levels(raw.data$sensor)) {
  if (grepl("[0-9]",substr(j,1,1))) {
    levels(raw.data$sensor) <- c(levels(raw.data$sensor),paste0(substr(j,2,2),substr(j,1,1)))
    raw.data$sensor[raw.data$sensor==j] <- paste0(substr(j,2,2),substr(j,1,1))
  }
}
raw.data$sensor<-droplevels(raw.data$sensor)
levels(raw.data$sensor)
length(levels(raw.data$sensor))
barplot(table(raw.data$sensor))

# testing/checking: new data has all homes combined: determine break points
plot(raw.data$dt, type="l")
plot(diff(raw.data$dt), type="l")
idx_break <- abs(diff(raw.data$dt))>1800
sum(idx_break)
raw.data$dt[idx_break]

## plot raw data to check if looks correct
# loop over all homes / "sensor" -> might take a bit (30 sec+)
for (j in levels(raw.data$sensor)) {
  idx_sen <- raw.data$sensor == j
  
  plot(raw.data$dt[idx_sen],raw.data$sht_tmp[idx_sen], type="l", main=paste0(c(j, "T, RH")), ylim=c(0,80))
  lines(raw.data$dt[idx_sen],raw.data$sht_humid[idx_sen],  col="blue")

  plot(raw.data$dt[idx_sen],raw.data$CO2[idx_sen], type="l", main=paste0(c(j, "CO2, TVOC, FA")), ylim=c(0,3000))
  lines(raw.data$dt[idx_sen],raw.data$sgp_tvoc[idx_sen],  col="blue")
  lines(raw.data$dt[idx_sen],raw.data$Formaldehyde[idx_sen],  col="green")

  plot(raw.data$dt[idx_sen],raw.data$PM1.0[idx_sen], type="l", main=paste0(c(j, "PM1, 2.5, 10")), ylim=c(0,100))
  lines(raw.data$dt[idx_sen],raw.data$PM2.5[idx_sen], col="blue")
  lines(raw.data$dt[idx_sen],raw.data$PM10[idx_sen], col="green")
  
  plot(raw.data$dt[idx_sen],raw.data$sgp_rawEthanol[idx_sen], type="l", main=paste0(c(j, "raw Ethanol")))
}

# manual plots
ID <- "A1"
idx_sen <- raw.data$sensor == ID
plot(raw.data$dt[idx_sen],raw.data$sht_tmp[idx_sen], type="l", main=paste0(c(ID, "T, RH")), ylim=c(0,80))
lines(raw.data$dt[idx_sen],raw.data$sht_humid[idx_sen],  col="blue")
plot(raw.data$dt[idx_sen],raw.data$Formaldehyde[idx_sen], type="l", main=paste0(c(ID, "Formaldehyde, TVOC")))
lines(raw.data$dt[idx_sen],raw.data$sgp_tvoc[idx_sen],  col="blue")

## first check of merged_data.csv data (only winter)
# data has no big gaps or no NA or unphysical values, BUT...
# some temperature values are unrealistically high: (2A), 2D >35?C!!, 4B >30?C, 5D >30?C
# also very high CO2, TVOC, FA values in 2D
# also very low temp in 5D <<5?C for prolonged period -> exclude that period?
# -> checked with Maria: see Emails

## Run Annex 86 analysis
# Read and prepare config object
config <- read_excel("../data/NOR/HomeOffice/cfg/NOR_data_config3.xlsx")
config <- subset(config, process == TRUE) # Remove all rows process = FALSE
config$variable <- as.factor(config$variable)

# amend config file entries to cover all sensors/homes (config file only defined for 1 home)
config_nr <- nrow(config)
home_nr <- length(levels(raw.data$sensor))

for (j in 1:home_nr) {
  idx_home <- match(levels(raw.data$sensor)[j], raw.data$sensor)
  for (k in 2:config_nr) {
    idx_row <- nrow(config)
    if (j==1) {
      config[k,"home"] <- levels(raw.data$sensor)[j]
      #idx_home <- raw.data$sensor==levels(raw.data$sensor)[j]  # quick & dirty: only use first idx (no double check that only 1 room type)
      config[k,"room"] <- raw.data$Main_room_use[idx_home]      
      
    } else{ 
      config[idx_row+1,] <- config[k,]
      config[idx_row+1,"home"] <- levels(raw.data$sensor)[j]
      
      #idx_home <- raw.data$sensor==levels(raw.data$sensor)[j]  # quick & dirty: only use first idx (no double check that only 1 room type)
      config[idx_row+1,"room"] <- raw.data$Main_room_use[idx_home]
    }
  }
}
config$variable <- as.character(config$variable)  #annex script wants all columns as character

## rename room names to match annex definitions
config$room[config$room=="B"] <- "BED"
config$room[config$room=="HO"] <- "HOO"
config$room[config$room=="LR"] <- "LIV"
config$room[config$room=="LR_K_B"] <- "LAU"
config$room[config$room=="LR_K"] <- "LAU"

## prepare meta data: load file with meta data and rename to match annex definitions
#meta.data <- raw.data2[raw.data2$sensor==ID,]
meta.data <- read_excel("../Data/NOR/HomeOffice/meta/Meta_info2.xlsx", sheet="home")
meta.data$Ventilation <- stringr::str_replace_all(meta.data$Ventilation, "\\s", " ") #convert non-breaking spaces to normal white space, a problem in "NV + EV"
meta.data[sapply(meta.data, is.character)] <- lapply(meta.data[sapply(meta.data, is.character)], as.factor) #converts all char variable to factor variable
# rename ventilation type
meta.data$vent2 <- meta.data$Ventilation
levels(meta.data$vent2)[levels(meta.data$vent2)=="MV"] <- "Mechanical ventilation"
levels(meta.data$vent2)[levels(meta.data$vent2)=="NV"] <- "Natural ventilation (designed)"
levels(meta.data$vent2)[levels(meta.data$vent2)=="NV + EV"] <- "Hybrid/mixed mode ventilation"
# rename building type
meta.data$type2 <- meta.data$Type
levels(meta.data$type2)[levels(meta.data$type2)=="SFH"] <- "Single family house (SFH)"
levels(meta.data$type2)[levels(meta.data$type2)=="MFH"] <- "Multi-family house (MFH)"
levels(meta.data$type2)[levels(meta.data$type2)=="SDH"] <- "Terraced house (TH)"
levels(meta.data$type2)[levels(meta.data$type2)=="A"] <- "Apartment block (AB)"
# provide location info
meta.data$loc2 <- meta.data$Bdg.loc
levels(meta.data$loc2)[levels(meta.data$loc2)=="CC"] <- "Location: city centre"
levels(meta.data$loc2)[levels(meta.data$loc2)=="SF"] <- "Location: suburban forested area"
levels(meta.data$loc2)[levels(meta.data$loc2)=="SNF"] <- "Location: suburban non-forested area"
# define window opening behaviour variable (to be extracted from data file)
meta.data$WinOpenBeh <- as.factor("")
levels(meta.data$WinOpenBeh) <- c("",levels(raw.data$Normal_Wind_Stat))

## run annex analysis
# one home/sensor at a time (otherwise the columns would need to be split up)
stats <- data.frame()
#ID <- "A1"
for (ID in levels(raw.data$sensor)) {
  idx_sen <- raw.data$sensor == ID
  datahome <- raw.data[idx_sen,]
  confighome <- config[config$home==ID,]
  confighome[1,] <- config[1,]  #first line of config file = datetime definition
  
  # extract window behaviour info from data file and store in meta.data variable (for later reference)
  meta.data$WinOpenBeh[meta.data$ID_new==ID] <- datahome$Normal_Wind_Stat[1] #quick & dirty: use first entry without checking if same for all

  # Prepare annex object
  annex_check_config(confighome) #optional check
  
  prepared_df <- annex_prepare(datahome, confighome, quiet = TRUE)
  annex_df    <- annex(RH + T + CO2 + PM1 + PM25 + TVOC + HCHO ~ datetime | study + home + room,
                       data = prepared_df, tz = "Europe/Oslo")
  
  # run standard analysis
  stats_t       <- annex_stats(annex_df, format = "wide")
  head(stats_t)
  
  # remove the "all" entries in year and month if only one year or month (-> revision of annex package needed)
  if (length(levels(stats_t$year)[-grep("all",levels(stats_t$year))])==1){ 
    idx2rm <- stats_t$year == "all"
    stats_t <- stats_t[!idx2rm,]
    }
  if (length(levels(stats_t$month)[-grep("all",levels(stats_t$month))])==1){ 
    idx2rm <- stats_t$month == "all"
    stats_t <- stats_t[!idx2rm,]
  }
  
  stats <- rbind(stats, stats_t)
}

# write xls results file (absolute path needed, for RDCOMClient package, needed to fix openxlsx bug)
fn_out <- paste0("C:\\Daten\\A86\\AP3_AP4\\out\\NOR\\HomeOffice\\NOR_HomeOffice_A1_D5.xlsx")
annex_write_stats(stats, file = fn_out, user = 016)
# open & close (read & save) the file using Excel to avoid/fix bug of openxlsx
read_save_xlsx(fn_out)

## load file again and write meta information
meta.wb <- loadWorkbook(fn_out)

# META-study
#meta.study <- readWorkbook(meta.wb, sheet="META-Study", rowNames = FALSE, colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
meta.study <- readWorkbook(meta.wb, sheet="META-Study")
meta.study$Contact <- "Maria Justo Alonso"
meta.study$Institution <- "SINTEF"
meta.study$Year.of.first.publication <- 2022
meta.study$Publications <- "https://doi.org/10.1016/j.buildenv.2022.109580"
meta.study$Links <- "https://fmezen.com/"
meta.study$`Additional.information/comments` <- "This study originally meant to study IAQ in offices, but was adapted to investigate IAQ in home-office situation during COVID-19 pandemic"
writeData(wb=meta.wb, sheet = "META-Study", x = meta.study, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)

meta.home <- readWorkbook(meta.wb, sheet="META-Home")
meta.room <- readWorkbook(meta.wb, sheet="META-Room")
meta.var <- readWorkbook(meta.wb, sheet="META-Variable")

for (ID in levels(raw.data$sensor)) {
  ID2 <- paste0("-",ID)
  # META-home
  meta.home$'Location:.Country'[grepl(ID2,meta.home$ID)] <- "NOR"
  meta.home$'Location:.City'[grepl(ID2,meta.home$ID)] <- "Trondheim" # see email 13.6.2024 from Maria
  meta.home$'Ventilation.type'[grepl(ID2,meta.home$ID)] <- as.character(meta.data$vent2[meta.data$ID_new==ID])
  meta.home$'Comment.Vent..Type'[grepl(ID2,meta.home$ID)] <- as.character(meta.data$Vent_Comm[meta.data$ID_new==ID])
  #meta.home$'Size.of.home./.TFA.[m^2]'[grepl(ID2,meta.home$ID)] <- meta.data$'Area (m2)'[meta.data$ID_new==ID] #wrong: area only for room 
  meta.home$'Type.of.building'[grepl(ID2,meta.home$ID)] <- as.character(meta.data$type2[meta.data$ID_new==ID])
  meta.home$'Year.of.contruction./.major.renovation.(four.digit.year)'[grepl(ID2,meta.home$ID)] <- as.character(meta.data$'Construction year (renovation)'[meta.data$ID_new==ID])
  meta.home$'Additional.information/comments'[grepl(ID2,meta.home$ID)] <- paste0(meta.data$loc2[meta.data$ID_new==ID],"; Age: if renovated, year in brackets ()")
  
  # META-room
  meta.room$`Additionial.room.information.(e.g.,.ceiling.height,.with.atrium)`[grepl(ID2,meta.room$ID)] <- paste0("Room (also) used as Home-Office", 
                                                                                         "; Trickle vent (if available) open? ", meta.data$`Trickle vent open?`[meta.data$ID_new==ID],
                                                                                         "; Window opening behaviour (AC:always closed, AC,O_W_H:always closed except if too hot, ASO:always slighly open)? ", meta.data$WinOpenBeh[meta.data$ID_new==ID],
                                                                                         "; Room area (m2): ", meta.data$`Area (m2)`[meta.data$ID_new==ID],
                                                                                         "; General occupant number:", meta.data$OccNr_general[meta.data$ID_new==ID],
                                                                                         "; Maximum occupant number:", meta.data$OccNr_max[meta.data$ID_new==ID],
                                                                                         "; Wood stove: ", meta.data$`Wood Stove`[meta.data$ID_new==ID],
                                                                                         "; Pets: ", meta.data$Pets[meta.data$ID_new==ID],
                                                                                         "; Floor mat. (W)ooden flooring or cork, (P)arquets, (C)arpet: ", meta.data$`Floor material`[meta.data$ID_new==ID])
  meta.room$`Occupancy:.Type`[grepl(ID2,meta.room$ID)]  <- "adult"
  meta.room$`Occupancy:.Number`[grepl(ID2,meta.room$ID)]<- meta.data$OccNr_general[meta.data$ID_new==ID]
  meta.room$Fresh.air.supply.in.measurement.location[grepl(ID2,meta.room$ID)] <- as.character(meta.data$FreshAirSupplyRoom[meta.data$ID_new==ID])
  meta.room$Comments[grepl(ID2,meta.room$ID)] <- "Note: Room use and occupation only indicative, as measurements might include holiday periods"
} 
# META-variable (same for all homes)
# CO2, T, RH
meta.var$Measurement.device[grepl("-CO2",meta.var$ID)] <- "Sensirion SCD30"
meta.var$`Comments.(e.g.,.sensor.location)`[grepl("-CO2",meta.var$ID)] <- "placed on the desk next to keyboard"
meta.var$Measurement.device[grepl("-T",meta.var$ID)] <- "Sensirion SCD30"
meta.var$`Comments.(e.g.,.sensor.location)`[grepl("-T",meta.var$ID)] <- "placed on the desk next to keyboard"
meta.var$Measurement.device[grepl("-RH",meta.var$ID)] <- "Sensirion SCD30"
meta.var$`Comments.(e.g.,.sensor.location)`[grepl("-RH",meta.var$ID)] <- "placed on the desk next to keyboard"
# PM
meta.var$Measurement.device[grepl("-PM1",meta.var$ID)] <- "Sensirion SPS30"
meta.var$`Comments.(e.g.,.sensor.location)`[grepl("-PM1",meta.var$ID)] <- "placed on the desk next to keyboard"
meta.var$Measurement.device[grepl("-PM25",meta.var$ID)] <- "Sensirion SPS30"
meta.var$`Comments.(e.g.,.sensor.location)`[grepl("-PM25",meta.var$ID)] <- "placed on the desk next to keyboard"  
# FA
meta.var$Measurement.device[grepl("-HCHO",meta.var$ID)] <- "DART WZ-S formaldehyde module"
meta.var$`Comments.(e.g.,.sensor.location)`[grepl("-HCHO",meta.var$ID)] <- "placed on the desk next to keyboard"
# TVOC
meta.var$Measurement.device[grepl("-TVOC",meta.var$ID)] <- "Sensirion SVM30"
meta.var$`Comments.(e.g.,.sensor.location)`[grepl("-TVOC",meta.var$ID)] <- "placed on the desk next to keyboard"
meta.var$Variable.additional.information[grepl("-TVOC",meta.var$ID)] <- "Low cost sensor signal converted to ug/m3 assuming average molecular weight of 92.14g/mol (equiv. Toluene)"
  
writeData(wb=meta.wb, sheet = "META-Home", x = meta.home, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
writeData(wb=meta.wb, sheet = "META-Room", x = meta.room, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
writeData(wb=meta.wb, sheet = "META-Variable", x = meta.var, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)

# write meta information to annex xls
saveWorkbook(meta.wb, fn_out, overwrite = TRUE)
  
  
##------------------------------------------------------------------------------------
# optional: write the extended meta data summary to file (for better reference)
write.xlsx(meta.data,"../data/NOR/HomeOffice/meta/Meta_info_summary_output.xlsx",overwrite = TRUE)


# optional: check xls file if all required meta information was filled out
check <- annex_validate("../Data/NOR_tbd.xlsx", user = 016)

## compare with paper boxplots to check units
# all homes
dt30 <- cut(raw.data$dt, breaks="30 mins")
FA30 <- aggregate(raw.data$Formaldehyde, by=list(dt30), mean)
FA30$dt <- as.POSIXct(FA30$Group.1)
plot(FA30$dt,FA30$x, type="l")
summary(FA30$x)
summary(FA30$x[FA30$x<1000])
summary(FA30$x*30.02/24.45)

dt60 <-cut(raw.data$dt, breaks="60 mins")
TVOC60 <- aggregate(raw.data$sgp_tvoc, by=list(dt60), mean)
TVOC60$dt <- as.POSIXct(TVOC60$Group.1)
plot(TVOC60$dt,TVOC60$x, type="l")
summary(TVOC60$x)

# specific home (last run)
summary(datahome$Formaldehyde)
summary(datahome$Formaldehyde*30.02/24.45)  # if in ppb
summary(datahome$sgp_tvoc)
summary(datahome$sgp_tvoc*92.14/24.45)

dt30 <- cut(datahome$dt, breaks="30 mins")
FA30 <- aggregate(datahome$Formaldehyde, by=list(dt30), mean)
FA30$dt <- as.POSIXct(FA30$Group.1)
plot(FA30$dt,FA30$x, type="l")
summary(FA30$x)
summary(FA30$x[FA30$x<1000])
summary(FA30$x*30.02/24.45)


### stashed parts
## try differen operation on xls to remove bug, that it can't be opened after save Workbook
# ID<-"C3"
# temp_filename <- paste0("../Data/NOR/NOR_MJusto_",ID,".xlsx")
# testxls <- read_excel(temp_filename, sheet = "META-Study")
# write.xlsx(testxls,temp_filename)
# 
# install.packages('xlsx')
# library("xlsx")
# 
# install.packages("XLConnect")
# library("XLConnect")
# 
# install.packages("rJava")
# 
# temp_filename <- "../Data/NOR/IECH_52.xlsx"
# meta.wb <- loadWorkbook(temp_filename)
# saveWorkbook(meta.wb, temp_filename, overwrite = TRUE)
# 
# for (ID in 1:length(config2$column)){
#   
# }

