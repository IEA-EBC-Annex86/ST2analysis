## Annex 86 processing of CISC data from Sonia
# Oct 2024, Timm Freundorfer, adaptions Gabriel Rojas

print("-> ESP_Sonia_Garcia")
library("openxlsx")
library("readxl")
library("dplyr")
library("annex")

find_start_row <- function(file_path, sheet, n_rows = 30) {
  # Read first 'n_rows' rows
  preview_data <- read_excel(file_path, sheet = sheet, range = cell_rows(1:n_rows))
  
  # Loop through rows to find the first non-NA row
  for (i in 1:nrow(preview_data)) {
    if (any(grepl('Date', preview_data[i, ]))) {
      print(preview_data[i, ])
      return(c(i, colnames(preview_data[2])) )
    }
  }
  
  return(c(NA, colnames(preview_data[2])))  # Return NA if no data found in the first 'n_rows' rows
}

## Set the working directory
# Timm
# setwd("C:/Users/Timm/Desktop/Atmospheric Sciences/Arbeit")
# dir_cfg <- c("Annex86_AP3_data/ESP/CISC/prepro/cfg")
# dir_dat <- c("Annex86_AP3_data/ESP/CISC/raw/datos_Sonia_Garcia")
# dir_out <- c("Data(selfmade)/ESP/CISC")

# Gabriel
setwd("C:/Daten/A86/AP3_AP4/src")
dir_cfg <- c("../data/ESP/CISC/prepro/cfg")
dir_dat <- c("../data/ESP/CISC/raw/datos_Sonia_Garcia")
dir_out <- c("../out/ESP/CISC")

dir.create(dir_out, recursive=TRUE)

# List of files (have only the csv file to be read in the folder)
fns <- list.files(path=dir_dat)

##extract room names-------------------------------------------------
# # just ran once to extract room names to manually generate a mapping
# ro_df <- data.frame()
# for (i in 1:length(fns)){
#   ro_df[i,1] <- paste(dir_dat, "/", fns[i], sep="")
#   tmp_sht_names <- excel_sheets(ro_df[i,1])
#   ro_df[i,2] <- length(tmp_sht_names)
#   for (j in 1:ro_df[i,2]){
#     ro_df[i,j+2] <- tmp_sht_names[j]
#   }
# }
# write.csv(ro_df, file="../data/ESP/CISC/meta/roomlisting.csv")
#----end extract room names

## load room name mapping file---------------------------
room_map <- read.csv2(file="../data/ESP/CISC/meta/roomlisting_v2.csv")
room_map <- room_map[1:22,] #remove unnecessary lines


raw_df <- data.frame('Date' = character(), 'Time' = character(), 'CO2' = numeric(), 'Temperature' = numeric(), 'Humidity' = numeric(), 'home' = character())
# config <- data.frame('column'=character(), 'variable'=character(),'study'=character(), 'unit'=character(), 'home'=character(), 'room'=character())
# config[1,1:2] <- c('tst','datetime')

# Load in the data
# cfg_row <- 2
#i<-1
for (i in 1:length(fns)){  #length(fns)
  # each file
  file_path <- paste(dir_dat, "/", fns[i], sep="")
  print(paste0("Reading ", file_path))
  sheet_names <- excel_sheets(file_path)
  num_sheets <- length(sheet_names)
  home <- paste0('V',strsplit(fns[i], '_')[[1]][2])
  for(j in 1:num_sheets){
    # each sheet
    tmpreturn = find_start_row(file_path, sheet = j)
    k <- as.numeric(tmpreturn[1])+1
    tmp <- read.xlsx(file_path, sheet = j, startRow = k, colNames = TRUE, detectDates = TRUE)
    tmp$home <- home
    tmp$device <- ifelse(grepl("Woehler", tmpreturn[2]), tmpreturn[2], "tbd")
    #tmp$room <- sheet_names[j]
    
    idxrow_room_map <- match(file_path,room_map$V1) #should be same as i, but not necessarily
    idxcol_room_map <- match(sheet_names[j],room_map[idxrow_room_map,])
    tmp$room <- room_map[idxrow_room_map,idxcol_room_map+6] #in room_map the mapped room name is 6 columns further to the right
    print(paste0("Mapped ", room_map[idxrow_room_map,idxcol_room_map+6], " to ", room_map[idxrow_room_map,idxcol_room_map] ))
    
    # trim dataset to add to raw_df
    if (any(grep('Data[ .]Set',names(tmp)))){
      tmp <- tmp %>% select(-matches("Data[ .]Set")) 
    }
    
    if (any(grep('Dew[ .]Point',names(tmp)))){
      tmp <- tmp %>% select(-matches("Dew[ .]Point"))
    }
    
    if (any(grep('tiempo',names(tmp)))){
      tmp <- tmp %>% rename_with(~ ifelse(grepl("tiempo", .), "Time", .))
    }
    
    if (any(grep('HR',names(tmp)))){
      tmp <- tmp %>% rename_with(~ ifelse(grepl("HR", .), "Humidity", .))
    }
    
    if (any(grep('X6|X7|X8|X9|X10|X11|X12', names(tmp)))){
      tmp <- tmp %>% select(-matches('X6|X7|X8|X9|X10|X11|X12'))
    }
    
    names(tmp) <- gsub("[:.]", "", names(tmp))  # Removes ':' and '.' from column names
    names(tmp)[names(tmp) == "relHumidity"] <- "Humidity"  # Fix "relHumidity" to "Humidity"
    
    # calculate h/m/s
    tmp$Time <- as.numeric(tmp$Time) * 86400  # 86400 seconds in a day
    tmp <- na.omit(tmp)
    time_value <- as.POSIXct(tmp$Time, origin = tmp$Date, tz = "UTC")  # Adjust timezone as necessary
    tmp$Time <- format(time_value, "%H:%M:%S")
    
    tmp$Date <- as.character(tmp$Date)
    tmp$Humidity <- as.numeric(tmp$Humidity)
    tmp$Temperature <- as.numeric(tmp$Temperature)
    tmp$CO2 <- as.numeric(tmp$CO2)
    
    raw_df <- bind_rows(raw_df, tmp) 
    
    ## create config file: not needed anymore (prepared_df create directly)
    # config[cfg_row,1:6] <- c('Temperature','T','CISC','C', tmp$home, tmp$room)
    # config[cfg_row+1,1:6] <- c('Humidity','RH','CISC','%', tmp$home, tmp$room)
    # config[cfg_row+2,1:6] <- c('CO2','CO2','CISC','ppm', tmp$home, tmp$room)
    # cfg_row <- cfg_row + 3
  }
}

#raw_df$room <- as.character(raw_df$room)
unique(raw_df$room)

# calculate datetime
raw_df <- transform(raw_df, tst = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S", tz = "Europe/Berlin"))
sum(is.na(raw_df$tst))

idxplot <- raw_df$home=="V01" & raw_df$room=="BAT"
tt<-raw_df$tst[idxplot]
plot(raw_df$tst[idxplot],raw_df$CO2[idxplot],type="l")

plot(tt)
idxsummer <- tt>"2018-06-01"
head(tt[idxsummer])

## assigning sensor: needs adaption
# raw_df$device <- 'xxx'
# for (i in 1:nrow(raw_df)){
#   if (grepl('Woehler', raw_df$room[i], ignore.case = TRUE)){
#     raw_df$device[i] <- 'Woehler'
#   }
# }

#prepare prepared_df------------------------------------------------
#raw_df is already in correct format, just rename the colnames and add study
prepared_df <- raw_df[,-c(1,2,7)] #remove col: Date, Time and device
prepared_df$study <- 'CISC'
colnames(prepared_df) <- c('CO2','T','RH','home','room','datetime','study')


annex_df <- annex(RH + T + CO2 ~ datetime | study + home + room,
                  data = prepared_df, tz = "Europe/Berlin")

stats <- annex_stats(annex_df, format="wide")

fn_out <- paste(dir_out, "/", 'CISC_v3', ".xlsx", sep="")
annex_write_stats(stats, file=fn_out, user=18)

meta1.wb <- loadWorkbook(fn_out)
meta2.wb <- loadWorkbook("../data/ESP/CISC/meta/CISC_v2_GR.xlsx")

# META-study
meta1.study <- readWorkbook(meta1.wb, sheet="META-Study")
meta2.study <- readWorkbook(meta2.wb, sheet="META-Study")
meta1.study[meta1.study$ID==meta2.study$ID,] <- meta2.study[meta1.study$ID==meta2.study$ID,]
writeData(wb=meta1.wb, sheet = "META-Study", x = meta1.study, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)

# META-home
meta1.home <- readWorkbook(meta1.wb, sheet="META-Home")
meta2.home <- readWorkbook(meta2.wb, sheet="META-Home")
meta1.home[meta1.home$ID==meta2.home$ID,] <- meta2.home[meta1.home$ID==meta2.home$ID,]
writeData(wb=meta1.wb, sheet = "META-Home", x = meta1.home, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)

# META-room
meta1.room <- readWorkbook(meta1.wb, sheet="META-Room")
meta2.room <- readWorkbook(meta2.wb, sheet="META-Room")
for (ii in 1:nrow(meta1.room)){
  if(sum(meta1.room$ID[ii]==meta2.room$ID)){
    meta1.room[ii,2:ncol(meta1.room)] <- meta2.room[meta1.room$ID[ii]==meta2.room$ID,2:ncol(meta1.room)]
  }
}
writeData(wb=meta1.wb, sheet = "META-Room", x = meta1.room, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)

# META-variable
meta1.var <- readWorkbook(meta1.wb, sheet="META-Variable")
meta2.var <- readWorkbook(meta2.wb, sheet="META-Variable")
h_ID_out <- unlist(strsplit(meta1.var$ID, split="-"))[seq(3,nrow(meta1.var)*5,5)]
r_ID_out <- unlist(strsplit(meta1.var$ID, split="-"))[seq(4,nrow(meta1.var)*5,5)]
for (ii in 1:nrow(meta1.var)){
  if(sum(meta1.var$ID[ii]==meta2.var$ID)){
    meta1.var[ii,2:ncol(meta1.var)] <- meta2.var[meta1.var$ID[ii]==meta2.var$ID,2:ncol(meta1.var)]
  }
  meta1.var$Measurement.device[ii] <- raw_df$device[h_ID_out[ii]==raw_df$home & r_ID_out[ii]==raw_df$room][1]
}
writeData(wb=meta1.wb, sheet = "META-Variable", x = meta1.var, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)

# write meta information to annex xls
saveWorkbook(meta1.wb, fn_out, overwrite = TRUE)
