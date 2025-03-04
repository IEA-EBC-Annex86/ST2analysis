## Annnex 86 post processing script for renaming home ID
# Gabriel Rojas, Aug 2024, based on IRL_ARDEN....R

# install the following libraries if needed------------------------
install.packages("remotes") # allows installation from Github
library('remotes')
install_github("IEA-EBC-Annex86/annex")
install.packages("readxl")
install.packages("openxlsx")
# with this new openxlsx package no work around with RDCOMClient needed
install_github("ycphs/openxlsx")

## load required libraries-----------------------------------------
library("annex")
library("readxl")
library("openxlsx")

## --------------------------------------------------------------------
setwd("C:/Users/Gabriel/Daten/A86/AP3_AP4/src")

# define file locations of annex output files (stat) to be altered
dir_stat <- "../out/IRL/VALIDate/archiv2"
dir_out <- "../out/IRL/VALIDate"

# entries to be changed/added
old_user_study <- "0999-Example01"
user <- "0006"
study <- "VALIDate"
contact <- "James McGrath"
institution <- "Maynooth University"
VOCunit <- "ppb"


# create folder if needed and check which files are in the folder
dir.create(dir_out, recursive=TRUE)
fn_stat <- list.files(path=dir_stat, pattern=".xlsx") 

# loop over all output files / homes------------------------
#i<-1
for (i in 1:length(fn_stat)) {
  # define full path
  ffn_stat <- paste0(dir_stat, "/", fn_stat[i])
  
  # load xlsx file
  wb <- loadWorkbook(ffn_stat)
  stat <- readWorkbook(wb, sheet="STAT")
  
  # changing entries in STAT
  stat$user <- user
  stat$study <- study
  # diverse fixes that might be helpful----------
  # fix date format: loadworkbook reads in dates in excel format
  stat$quality_start <- as.Date(stat$quality_start, origin = "1899-12-30") 
  stat$quality_end <- as.Date(stat$quality_end, origin = "1899-12-30")
  # # correct incorrect upper bound quality check for CO (annex package needs fix)
  # if (max(stat$p100[stat$variable=="CO"])>100000){
  #   print("Warning: CO was above 100mg/m3 in BED, and quality_upper was set to 0")
  #   stat$quality_upper[stat$variable=="CO"] <- 0 }
  writeData(wb=wb, sheet = "STAT", x = stat, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  
  # META-study
  meta.study <- readWorkbook(wb, sheet="META-Study")
  meta.study$ID <- paste0(user,"-",study)
  meta.study$Contact <- contact
  meta.study$Institution <- institution
  writeData(wb=wb, sheet = "META-Study", x = meta.study, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  
  # META-home
  meta.home <- readWorkbook(wb, sheet="META-Home")
  #home <- unlist(strsplit(meta.home$ID, split="-"))
  meta.home$ID <- gsub(old_user_study, paste0(user,"-",study), meta.home$ID)
  writeData(wb=wb, sheet = "META-Home", x = meta.home, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  
  # META-room
  meta.room <- readWorkbook(wb, sheet="META-Room")
  meta.room$ID <- gsub(old_user_study, paste0(user,"-",study), meta.room$ID)
  writeData(wb=wb, sheet = "META-Room", x = meta.room, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  
  # META-variable
  meta.var <- readWorkbook(wb, sheet="META-Variable")
  meta.var$ID <- gsub(old_user_study, paste0(user,"-",study), meta.var$ID)
  meta.var$Variable.unit[grepl("-VOC",meta.var$ID)] <- VOCunit
  writeData(wb=wb, sheet = "META-Variable", x = meta.var, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  
  # write meta information to annex xls
  ffn_out <- paste0(dir_out, "/", fn_stat[i])
  saveWorkbook(wb, ffn_out, overwrite = TRUE)
  
}
