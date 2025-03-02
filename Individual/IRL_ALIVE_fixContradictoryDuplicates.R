## A86 fix IRL- ALIVE output files as there are contradictory Meta entries for houses 1, 2, 3, 5, 8
# Timm F feb 2025, from script of Gabriel R

library(openxlsx)
library(stringr)
library(purrr)

fn <- list.files(path="C:/Users/Timm/Desktop/Atmospheric Sciences/Arbeit/Annex86_AP3_out/IRL/ALIVE", pattern="*.xlsx", full.names=TRUE)

sheets <- c(1,2,3,5,8)

file_names <- map_chr(str_split(fn, "/"), ~ .x[length(.x)])

fn_1 <- fn[grepl("1", file_names)]
fn_2 <- fn[grepl("2", file_names)]
fn_3 <- fn[grepl("3", file_names)]
fn_5 <- fn[grepl("5", file_names)]
fn_8 <- fn[grepl("8", file_names)]

for (f in fn_1){
  wb <- loadWorkbook(f)
  meta.home <- readWorkbook(wb, sheet="META-Home")
  if (meta.home$ID == '0015-ALIVE-House_1'){
    meta.home$Ventilation.type <- "Natural ventilation (designed)"
    meta.home$Comment.Vent..Type <- "Extract fans in kitchen and bathroom"
    meta.home$Type.of.Occupants <- "2 Adults and 1 child"
    meta.home$`Year.of.contruction./.major.renovation.(four.digit.year)` <- 2015
    meta.home$Type.of.building <- "Single family house (SFH)"
    writeData(wb=wb, sheet = "META-Home", x = meta.home, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
    saveWorkbook(wb, f, overwrite = TRUE)
  }
}

for (f in fn_2){
  wb <- loadWorkbook(f)
  meta.home <- readWorkbook(wb, sheet="META-Home")
  if (meta.home$ID == '0015-ALIVE-House_2'){
    meta.home$Type.of.Occupants <- "2 adults and 2 children"
    writeData(wb=wb, sheet = "META-Home", x = meta.home, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
    saveWorkbook(wb, f, overwrite = TRUE)
  }
}

for (f in fn_3){
  wb <- loadWorkbook(f)
  meta.home <- readWorkbook(wb, sheet="META-Home")
  if (meta.home$ID == '0015-ALIVE-House_3'){
    meta.home$`Year.of.contruction./.major.renovation.(four.digit.year)` <- 2014
    meta.home$Type.of.Occupants <- "adult and child"
    writeData(wb=wb, sheet = "META-Home", x = meta.home, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
    saveWorkbook(wb, f, overwrite = TRUE)
  }
}

for (f in fn_5){
  wb <- loadWorkbook(f)
  meta.home <- readWorkbook(wb, sheet="META-Home")
  if (meta.home$ID == '0015-ALIVE-House_5'){
    meta.home$`Location:.City` <- 'Ennis'
    writeData(wb=wb, sheet = "META-Home", x = meta.home, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
    saveWorkbook(wb, f, overwrite = TRUE)
  }
}

for (f in fn_8){
  wb <- loadWorkbook(f)
  meta.home <- readWorkbook(wb, sheet="META-Home")
  if (meta.home$ID == '0015-ALIVE-House_8'){
    meta.home$Comment.Vent..Type <- "Extract fan in kitchen and bathroom"
    meta.home$Type.of.Occupants <- "adult"
    meta.home$Energy.standard <- "BER A2"
    writeData(wb=wb, sheet = "META-Home", x = meta.home, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
    saveWorkbook(wb, f, overwrite = TRUE)
  }
}

