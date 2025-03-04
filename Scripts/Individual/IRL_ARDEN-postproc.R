## Annnex 86 examplary post processing script for ARDEN (IRL) study
# Gabriel Rojas, July 2024, based on AMR_study_annex_adapted.R

# install the following libraries if needed------------------------
install.packages("remotes") # allows installation from Github
library('remotes')
install_github("IEA-EBC-Annex86/annex")

install.packages("openxlsx")
install.packages("readxl")
install.packages("RDCOMClient", repos = "http://www.omegahat.net/R")
# or alternatively via devtools
# devtools::install_github("omegahat/RDCOMClient")

## load required libraries-----------------------------------------
library("annex")
library("openxlsx")
library("readxl")
library("RDCOMClient")

# optional: check if annex package is up to date 0.2-12 and update if needed
# sessionInfo()

## -----------------------------------------------------------------
## Helper function to read and save xls file with RDCOMClient package
# load/run before doing the post processing below
read_save_xlsx <-function(filename){
  # temporary solution until bug in openxls is fixed (openxlsx is used for writing xls within annex package) 
  # Create an Excel application instance
  excel_app <- COMCreate("Excel.Application")
  excel_app[["Visible"]] <- FALSE  # Set to TRUE if you want to see the Excel window
  excel_app[["DisplayAlerts"]] <- FALSE  # Suppress any prompts
  # Try to open, save, and close the workbook
  tryCatch({
    # Open the workbook
    workbook <- excel_app[["Workbooks"]]$Open(filename)
    # Save the workbook
    workbook$SaveAs(filename)
    # Close the workbook
    workbook$Close()
    
    print(paste("Excel file has been saved as", filename))
  }, error = function(e) {
    print(paste("An error occurred:", e$message))
  }, finally = {
    # Quit the Excel application
    excel_app$Quit()
  })
}

## --------------------------------------------------------------------
#!!! Remember to set working directory to source file location

# define and read meta info file
ffn_meta <- "C:\\Daten\\A86\\AP3_AP4\\data\\IRL\\Marie_ARDEN\\meta\\ARDEN_meta information_v5.xlsx"
meta.info <- read_excel(ffn_meta)

# define file locations existing annex output files (stat) and where to write post processed ones
# use absolute path (not ideal) for RDCOMClient to work reliably
dir_stat <- "C:\\Daten\\A86\\AP3_AP4\\data\\IRL\\Marie_ARDEN\\stat"
dir_out <- "C:\\Daten\\A86\\AP3_AP4\\data\\IRL\\Marie_ARDEN\\out"

dir.create(dir_out, recursive=TRUE)

# check which files are in the folder
fn_GW_bed <- list.files(path=dir_stat, pattern="_GW_BED")  #fn:filename

# fn_PM_bed <- list.files(path=dir_stat, pattern="_PM25_BED")
# fn_GW_liv <- list.files(path=dir_stat, pattern="_GW_LIV") 
# fn_PM_liv <- list.files(path=dir_stat, pattern="_PM25_LIV")

# loop over all output files / homes
#i<-2
for (i in 1:length(fn_GW_bed)) {
  # define the name of the home based on last part [5] of filename
  home_ID0 <- unlist(strsplit(unlist(strsplit(fn_GW_bed[i], split=".xlsx"))[1], split="_"))[4:5]
  home_ID1 <- paste0(home_ID0[2], "_", home_ID0[1])
  home_nr <- unlist(strsplit(home_ID0[2], split="ARDEN"))[2]
  home_ID2 <- paste0("H",home_nr,"_",home_ID0[1])
  home_ID <- paste0("0014-ARDEN-", home_ID2)   # choose home ID depending on meta info file
  #print(paste0(home_ID, " -> ", home_ID2))
  print(home_ID1)
  
  home_ID3 <- paste0(home_ID0[1], "_", home_ID0[2])
  fn_all <- list.files(path=dir_stat, pattern=home_ID3)
  
  # define full path for base data file
  ffn_GW_bed <- paste0(dir_stat, "\\", fn_GW_bed[i])    #ffn: full filename incl. path
  # only has to be run once every time the output file is generated using the annex script
  read_save_xlsx(ffn_GW_bed)
  
  stat2=data.frame()
  # load other files (PM and LIV) and combine STAT data (and remove unnecessary "all" in year)
  if (sum(grepl("PM25_BED",fn_all))) {
    ffn_PM_bed <- paste0(dir_stat, "\\", fn_all[grep("PM25_BED",fn_all)])
    stat_PM_bed <- read_excel(ffn_PM_bed, sheet="STAT")
    idx2rm <- stat_PM_bed$year == "all"
    stat2 <- rbind(stat2,stat_PM_bed[!idx2rm,])
    }
  if (sum(grepl("GW_LIV",fn_all))) {
    ffn_GW_liv <- paste0(dir_stat, "\\", fn_all[grep("GW_LIV",fn_all)])
    stat_GW_liv <- read_excel(ffn_GW_liv, sheet="STAT")
    if (max(stat_GW_liv$p100)>100000){print("Warning: CO was above 100mg/m3 in LIV, and quality_upper was set to 0")}
    stat_GW_liv$quality_upper <- 0
    idx2rm <- stat_GW_liv$year == "all"
    stat2 <- rbind(stat2,stat_GW_liv[!idx2rm,])
    }
  if (sum(grepl("PM25_LIV",fn_all))) {
    ffn_PM_liv <- paste0(dir_stat, "\\", fn_all[grep("PM25_LIV",fn_all)])
    stat_PM_liv <- read_excel(ffn_PM_liv, sheet="STAT")
    idx2rm <- stat_PM_liv$year == "all"
    stat2 <- rbind(stat2,stat_PM_liv[!idx2rm,])
    }

  # load GW result file to use as base file (to be amended)
  wb <- loadWorkbook(ffn_GW_bed)
  stat_GW_bed <- readWorkbook(wb, sheet="STAT")
  idx2rm <- stat_GW_bed$year == "all"
  stat_GW_bed <- stat_GW_bed[!idx2rm,]
  # fix date format: loadworkbook reads in dates in excel format
  stat_GW_bed$quality_start <- as.Date(stat_GW_bed$quality_start, origin = "1899-12-30") 
  stat_GW_bed$quality_end <- as.Date(stat_GW_bed$quality_end, origin = "1899-12-30")
  # correct incorrect upper bound quality check for CO (annex package needs fix)
  if (max(stat_GW_bed$p100)>100000){print("Warning: CO was above 100mg/m3 in BED, and quality_upper was set to 0")}
  stat_GW_bed$quality_upper <- 0

  stat <- rbind(stat_GW_bed,stat2)
  stat$user <- "0014"
  writeData(wb=wb, sheet = "STAT", x = stat, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  
  ## fill out meta information
  # META-study
  meta.study <- readWorkbook(wb, sheet="META-Study")
  meta.study <- meta.study[1,]  #only use first row (it is only one study, ID should be unique)
  meta.study[,] <- meta.info[1,1:7]
  writeData(wb=wb, sheet = "META-Study", x = meta.study, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  
  # META-home
  meta.home <- readWorkbook(wb, sheet="META-Home")
  #meta.home[,] <- meta.info[meta.info$`ID-META Home`==home_ID,8:25] # to avoid warnings use the following lines
  idxhome <- meta.info$`ID-META Home`==home_ID  
  idxhome[is.na(idxhome)] <- FALSE
  meta.home[,] <- meta.info[idxhome,8:25]
  writeData(wb=wb, sheet = "META-Home", x = meta.home, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  
  # META-room
  meta.room <- readWorkbook(wb, sheet="META-Room")
  rooms <- c("BED","LIV") #loop through the two rooms
  for (j in seq_along(rooms)) {
    room_ID <- paste0(home_ID, "_", rooms[j])
    room_ID <- gsub("_PRE_", "_POST_", room_ID)  # assuming that rooms have same meta info for pre and post
    meta.room[j,] <- meta.info[meta.info$`ID-META Room`==room_ID,26:33]
  }
  writeData(wb=wb, sheet = "META-Room", x = meta.room, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  
  # META-variable
  meta.var <- readWorkbook(wb, sheet="META-Variable")
  meta.var[1:12,1:5] <- meta.info[1:6,34:38]
  meta.var[,1] <- gsub("0014-ARDEN-H01_POST",home_ID,meta.var[,1])
  meta.var[6:12,1] <- gsub("-BED-","-LIV-", meta.var[6:12,1])
  writeData(wb=wb, sheet = "META-Variable", x = meta.var, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  
  # write meta information to annex xls
  ffn_out <- paste0(dir_out, "\\", home_ID1, ".xlsx")
  saveWorkbook(wb, ffn_out, overwrite = TRUE)
  
}

## stashed content, not required
# ## fill out meta information
# # META-study
# meta.study <- readWorkbook(wb, sheet="META-Study")
# meta.study <- meta.study[1,]  #only use first row (it is only one study, ID should be unique)
# meta.study$ID <- gsub("1234","0014", meta.study$ID)
# meta.study$Contact <- "Marie Coggins, Hala Hassan"
# 
# meta.study$Year.of.first.publication <- 2022
# meta.study$Publications <- meta.info$Publications[i]
# meta.study$Links <- ""
# meta.study$`Additional.information/comments` <- meta.info$`Additional information/comments...7`[i]
# writeData(wb=wb, sheet = "META-Study", x = meta.study, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
# 
# # META-home
# meta.home <- readWorkbook(wb, sheet="META-Home")
# meta.home$ID <- gsub("1234","0014", meta.home$ID)
# #meta.home$ID <- paste0("0014-ARDEN-", home_ID2) #optional rename to H01, H02, etc
# 
# meta.home$`Location:.Country` <- "IRL"
# writeData(wb=meta.wb, sheet = "META-Home", x = meta.home, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)

