## Annnex 86 post processing script for copying meta information 
# Gabriel Rojas, Jan 2025

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

# define file locations / names of file with meta information to be copied to file without meta information
ffn_CopyMetaTo <- "../out/ESP/ClimateReady/archiv3/Climate_Ready_10Min.xlsx"
ffn_CopyMetaFrom<- "../out/ESP/ClimateReady/archiv2/ClimateReady_v2_AMB2a.xlsx"
ffn_new <- "../out/ESP/ClimateReady/Climate_Ready_10min_v3.xlsx"
sheets_to_copy <- c("META-Study","META-Home","META-Room","META-Variable")


# load xlsx files
wb_from <- loadWorkbook(ffn_CopyMetaFrom)
wb_to <- loadWorkbook(ffn_CopyMetaTo)

# correct user ID if needed
UserID_new <- "0009"
UserID_old <- "0005"
ChangeUserID <- TRUE

if (ChangeUserID){
  stat_to <- readWorkbook(wb_to, sheet="STAT")
  stat_to$user <- UserID_new
  writeData(wb=wb_to, sheet = "STAT", x = stat_to, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
}
  
for (sh in sheets_to_copy){
  message("Copying: ", sh)
  meta_from <- readWorkbook(wb_from, sheet=sh)
  meta_to <- readWorkbook(wb_to, sheet=sh)
  if (ChangeUserID){
    meta_to$ID <- gsub(UserID_old, UserID_new, meta_to$ID) 
  }
  
  # Dynamically align column types
  common_cols <- intersect(names(meta_to), names(meta_from))
  for (col in common_cols) {
    if (class(meta_from[[col]]) != class(meta_to[[col]])) {
      class(meta_to[[col]]) <- class(meta_from[[col]])
    }
  }
  
  # write meta info from meta_from dataframe to meta_to dataframe
  meta_to <- meta_to %>%
    rows_update(meta_from, by="ID", unmatched ="ignore")
  
  # alternative with base, unfinished
  # idx <- match(meta_to$ID, meta_from$ID)
  # meta_to[idx,-1] <- meta_from[,-1]
  
  writeData(wb=wb_to, sheet = sh, x = meta_to, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
}

saveWorkbook(wb_to, ffn_new, overwrite = TRUE)

  
