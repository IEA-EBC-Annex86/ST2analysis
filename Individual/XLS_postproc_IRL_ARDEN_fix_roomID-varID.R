## Annnex 86 post processing script for renaming home ID
# Gabriel Rojas, Dec 2024, based on IRL_ARDEN....R

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

dir_stat <- "../out/IRL/ARDEN/archiv3"
dir_out <- "../out/IRL/ARDEN"

# create folder if needed and check which files are in the folder
dir.create(dir_out, recursive=TRUE)
fn_stat <- list.files(path=dir_stat, pattern=".xlsx") 

# loop over all output files / homes
#i<-1
for (i in 1:length(fn_stat)) {
  # define full path
  ffn_stat <- paste0(dir_stat, "/", fn_stat[i])
  
  # load xlsx file
  wb <- loadWorkbook(ffn_stat)
  meta.var <- readWorkbook(wb, sheet="META-Variable")
  meta.room <- readWorkbook(wb, sheet="META-Room")
  
  # fix ID in meta var, script IRL_ARDEN-postproc.R is erroneous, when creating the ID, should be 7:12 instead 6:12
  meta.var$ID[6] <- gsub("-LIV-","-BED-", meta.var$ID[6]) 
  writeData(wb=wb, sheet = "META-Variable", x = meta.var, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
  
  # fix ID in meta room, script IRL_ARDEN-postproc.R is erroneous, when creating the ID
  if(grepl("_PRE",meta.var$ID[1])){
    meta.room$ID <- gsub("_POST", "_PRE",meta.room$ID)
  }
  meta.room$ID <- gsub("_LIV","-LIV", meta.room$ID) 
  meta.room$ID <- gsub("_BED","-BED", meta.room$ID) 
  writeData(wb=wb, sheet = "META-Room", x = meta.room, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)

  # write meta information to annex xls
  ffn_out <- paste0(dir_out, "/", fn_stat[i])
  saveWorkbook(wb, ffn_out, overwrite = TRUE)
  
}
