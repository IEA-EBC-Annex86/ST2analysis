## Original: Andreas Frei
## Meta data addition: Lena Mayr
## Aug 2024


# R --vanilla < AT_IBO_annex.R
print("-> AT_IBO")

# Load necessary libraries -----------------------------------------------------
library("openxlsx")
library("readxl")
library("stringr")


# Set working directory --------------------------------------------------------
cwd <- "C:\\Users\\lenag\\seadrive_root\\Lena Sop\\Shared with groups"
setwd(cwd)


# Load meta data ---------------------------------------------------------------
p_meta_wb <- loadWorkbook(".\\Annex86_AP3_out\\AUT\\Lueftung3\\P00F_PF11F.xlsx")
k_meta_wb <- loadWorkbook(".\\Annex86_AP3_out\\AUT\\Lueftung3\\K50_KH10F.xlsx")
meta_data <- read.xlsx(".\\Annex86_AP3_data\\AUT\\Lueftung3\\meta\\meta_LM_2024_08_06.xlsx", rowNames = T, detectDates = T)


# Format meta data to match annex format ---------------------------------------
meta_data$mites_derf1 <- as.numeric(gsub("<", "", meta_data$mites_derf1)) # remove < sign
meta_data$mites_derp1 <- as.numeric(gsub("<", "", meta_data$mites_derp1))
meta_data$voc_livingroom <- as.numeric(gsub("<", "", meta_data$voc_livingroom))
meta_data$voc_bedroom <- as.numeric(gsub("<", "", meta_data$voc_bedroom))


# Create list of objects, homes,... --------------------------------------------
measurementnames <- rownames(meta_data)
p_measurementnames <- grep("^P", basename(measurementnames), value = TRUE)
k_measurementnames <- grep("^K", basename(measurementnames), value = TRUE)

remove_suffix <- function(string) {
  if (grepl("_2$", string)) {
    return(sub("_2$", "", string))
  } else if (grepl("F$", string)) {
    return(sub("F$", "", string))
  }
  return(string)
}

homenames <- unique(sapply(measurementnames, remove_suffix))
p_homenames <- grep("^P", basename(homenames), value = TRUE)
k_homenames <- grep("^K", basename(homenames), value = TRUE)

roomnames <- c("BED", "LIV", "SUP", "AMB", "LAU", "OTH")


# Function to add meta data to STAT --------------------------------------------
create_new_stat <- function(a_wbsheet, a_measurementnames, a_room, a_varname, a_var, a_sd=NULL, a_perc5=NULL, a_perc95=NULL){
  dates <- meta_data[a_measurementnames, "date_measurement"]
  new_stat <- data.frame(matrix(NA, nrow = length(a_measurementnames), ncol = length(colnames(a_wbsheet))))
  colnames(new_stat) <- colnames(a_wbsheet)
  new_stat$user <- "0012"
  new_stat$study <- "Lueftung3"
  new_stat$home <- sapply(a_measurementnames, remove_suffix)
  new_stat$room <- a_room
  new_stat$year <- sub("^([0-9]{4})-[0-9]{2}-[0-9]{2}$", "\\1", dates)
  new_stat$month <- sub("^[0-9]{4}-([0-9]{2})-[0-9]{2}$", "\\1", dates)
  new_stat$tod <- "07-23"
  new_stat$variable <- a_varname
  new_stat$Mean <- unlist(meta_data[a_measurementnames, a_var])
  if (!is.null(a_sd)) {
    new_stat$Sd <- meta_data[a_measurementnames, a_sd]
  }
  if (!is.null(a_perc5)) {
    new_stat$p05 <- meta_data[a_measurementnames, a_perc5]
  }
  if (!is.null(a_perc95)) {
    new_stat$p95 <- meta_data[a_measurementnames, a_perc95]
  }
  a_wbsheet <- rbind(a_wbsheet, new_stat)
  return(a_wbsheet)
}


add_metadata_stat <- function(a_wb, a_measurementnames){
  tryCatch({
    meta.stat <- readWorkbook(a_wb, sheet = "STAT")
    meta.stat$room <- "BED"
    meta.stat$study <- "Lueftung3"
    meta.stat$home <- sapply(meta.stat$home, remove_suffix)
    cols <- colnames(meta.stat)
    
    # Add temperature
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "LIV", "T", "temp_livingroom")
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "BED", "T_1", "temp_bedroom")
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "SUP", "T", "temp_ventilationoutlet")
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "AMB", "T", "temp_ambient")
    
    # Add relative humidity
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "LIV", "RH", "rh_livingroom")
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "BED", "RH_1", "rh_bedroom")
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "SUP", "RH", "rh_ventilationoutlet")
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "AMB", "RH", "rh_ambient")
    
    # Add fungi
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "LIV", "Fungi", "fungi_livingroom")
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "BED", "Fungi", "fungi_bedroom")
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "SUP", "Fungi", "fungi_ventilationoutlet")
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "AMB", "Fungi", "fungi_ambient")
    
    # Add Ions
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "AMB", "Ions(-)", "negions_ambient_mean", "negions_ambient_sd", "negions_ambient_5perc", "negions_ambient_95perc")
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "AMB", "Ions(+)", "posions_ambient_mean", "posions_ambient_sd", "posions_ambient_5perc", "posions_ambient_95perc")
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "BED", "Ions(-)", "negions_bedroom_mean", "negions_bedroom_sd", "negions_bedroom_5perc", "negions_bedroom_95perc")
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "BED", "Ions(+)", "posions_bedroom_mean", "posions_bedroom_sd", "posions_bedroom_5perc", "posions_bedroom_95perc")
    
    # Add Mites
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "LAU", "Mites (Der p1)", "mites_derp1")
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "LAU", "Mites (Der f1)", "mites_derf1")
    
    # Add HCHO
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "LIV", "HCHO", "hcho_livingroom")
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "BED", "HCHO", "hcho_bedroom")
    
    # Add VOC
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "LIV", "TVOC", "voc_livingroom")
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "BED", "TVOC", "voc_bedroom")
    
    # Add Radon
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "LIV", "RAD", "radon_livingroom")
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "BED", "RAD", "radon_bedroom")
    meta.stat <- create_new_stat(meta.stat, a_measurementnames, "OTH", "RAD", "radon_otherroom")
    
    # Order dataframe by home
    meta.stat <- meta.stat[order(meta.stat$home, meta.stat$var), ]
    
    writeData(wb=a_wb, sheet = "STAT", x = meta.stat, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
    return(a_wb)
  }, error = function(e) {
    print(paste("An error occurred while adding metadata to STAT:", e$message))
  })
}


# Function to add meta data to META-Study --------------------------------------
add_metadata_study <- function(a_wb){
  tryCatch({
    meta.study <- readWorkbook(a_wb, sheet = "META-Study")
    meta.study$ID <- "0012-Lueftung3"
    meta.study$Contact <- "Peter Tappler"
    meta.study$Institution <- "IBO Innenraumanalytik OG"
    meta.study$Year.of.first.publication <- "2015"
    meta.study$Publications <- "doi:10.3390/ijerph121114132"
    meta.study$Links <- "https://www.ibo.at/forschung/referenzprojekte/data/lueftung-30/"
    meta.study$`Additional.information/comments` <- "This study extensively measured IAQ in 60 conventional homes and 60 Passive-House like homes with MVHR. Further details in the project report (see link) and in doi:10.3390/ijerph14030314"
    writeData(wb=a_wb, sheet = "META-Study", x = meta.study, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
    return(a_wb)
  }, error = function(e) {
    print(paste("An error occurred while adding metadata to Study:", e$message))
  })
}


# Function to add data to Meta-Home --------------------------------------------
add_metadata_home <- function(a_wb, a_homenames, a_hometype){
  tryCatch({
    meta.home <- readWorkbook(a_wb, sheet = "META-Home")
    ids <- paste("0012-Lueftung3", a_homenames, sep="-")
    meta.home <- meta.home[1:length(ids),]
    meta.home$ID <- ids
    meta.home$`Location:.Country`<- "AUT"
    meta.home$`Location:.City`<- str_extract(meta_data[a_homenames, "object_address"], "(?<=\\d{4} )([^,]+)")
    meta.home$`Airtightn..[xx]` <-gsub("\\.", ",", as.character(unlist(meta_data[a_homenames, "n50"])))
    meta.home$`Airtightness.ref.press.[Pa]` <- "50"
    meta.home$Airtightness.normalization.value <- "Building volume specific [1/h]"
    meta.home$Type.of.building <- ifelse(meta_data[a_homenames, "type_home"] == 'EFH', 'SFH', meta_data[a_homenames, "type_home"])
    if (a_hometype == "P"){
      meta.home$Ventilation.type <- "Mechanical ventilation"
      meta.home$Comment.Vent..Type <- "MVHR"
    }
    if (a_hometype == "K"){
      meta.home$Ventilation.type <- "Window airing (not designed)"
    }
    
    writeData(wb=a_wb, sheet = "META-Home", x = meta.home, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
    meta.home <- readWorkbook(a_wb, sheet = "META-Home")
    meta.home[!meta.home$ID %in% ids, ] <- "" # clear empty rows
    writeData(wb=a_wb, sheet = "META-Home", x = meta.home, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
    return(a_wb)
  }, error = function(e) {
    print(paste("An error occurred while adding metadata to Home:", e$message))
  })
}


# Function to add data to Meta-Room --------------------------------------------
add_metadata_room <- function(a_wb, a_homenames, a_hometype){
  tryCatch({
    meta.room <- readWorkbook(a_wb, sheet = "META-Room")
    
    meta.room <- meta.room[rep(1:nrow(meta.room), each = length(roomnames)), ]
    room_IDs <- list()
    for (i in a_homenames){
      for (j in roomnames){
        room_IDs <- append(room_IDs, paste("0012-Lueftung3", i, j, sep="-"))
      }
    }
    meta.room <- meta.room[1:length(room_IDs),]
    meta.room$ID <- room_IDs
    oth_rows <- str_detect(meta.room$ID, ".*OTH.*")
    meta.room$`Additionial.room.information.(e.g.,.ceiling.height,.with.atrium)`[oth_rows] <- unlist(meta_data[paste0(a_homenames, "F"),"radon_otherroomname"])
    occupants_number <- unlist(meta_data[a_homenames, "people_bedroom"])
    meta.room$`Occupancy:.Number`[str_detect(meta.room$ID, ".*BED.*") == TRUE] <- occupants_number
    if (a_hometype == "P"){
      meta.room$Method.of.vent..rate.determination <- "Flow-hood / anemometer measurement (in context of study)"
      meta.room$Fresh.air.supply.in.measurement.location <- "Through mechanically driven supply air (no recirculated air)"
    }
    if (a_hometype == "K"){
      meta.room$Method.of.vent..rate.determination <- "Active tracer gas measurement (in context of study)"
      meta.room$Fresh.air.supply.in.measurement.location <- "Through occupant operated window only"
    }
    
    # Set acr according to hometype
    if (a_hometype == "P"){
      acr <- as.numeric(meta_data[a_homenames, "ach_volumeflow"])
    }
    if (a_hometype == "K"){
      acr <- as.numeric(meta_data[a_homenames, "ach_tracer"])
    }
    
    # Extract and unlist room size
    roomsize <- as.numeric (meta_data[a_homenames, "roomsize"])

    # Initialize new_df with the appropriate length
    new_df <- data.frame(
      info = character(length(roomsize)),
      acr = acr,
      roomsize = roomsize,
      ventrate = numeric(length(roomsize)),
      comment = character(length(roomsize))
    )
    
    # Handle missing roomsize values
    missing_indices <- is.na(new_df$roomsize)
    new_df$roomsize[missing_indices] <- 37.5
    new_df$comment[missing_indices] <- "Ventilation rate in l/s calculated from measured air exchange rate and assumed room size (not documented in this case) of 37,5m³."
    new_df$comment[!missing_indices] <- "Ventilation rate in l/s calculated from measured air exchange rate and documented room size."
    new_df$info[!missing_indices] <- paste0("room volume: ", gsub("\\.", ",", as.character(new_df$roomsize[!missing_indices])), "m³")
    
    # Calculate ventilation rate
    new_df$ventrate <- new_df$acr * new_df$roomsize / 3.6
    
    # Identify rows to update in meta.room
    bed_rows <- str_detect(meta.room$ID, ".*BED.*")
    
    meta.room$`Additionial.room.information.(e.g.,.ceiling.height,.with.atrium)`[bed_rows] <- new_df$info
    meta.room$Comments[bed_rows] <- new_df$comment
    meta.room$`Ventilation.rate.(room;.[l/s])`[bed_rows] <- gsub("\\.", ",", as.character(new_df$ventrate))
    
    writeData(wb=a_wb, sheet = "META-Room", x = meta.room, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
    meta.room <- readWorkbook(a_wb, sheet = "META-Room")
    meta.room[!meta.room$ID %in% room_IDs, ] <- "" # clear empty rows
    writeData(wb=a_wb, sheet = "META-Room", x = meta.room, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
    
    return(a_wb)
  }, error = function(e) {
    print(paste("An error occurred while adding metadata to Room:", e$message))
  })
}


# Function to add data to META-Variable ----------------------------------------
create_new_var <- function(a_wbsheet, a_homenames, a_rooms, a_var, a_info, a_unit, a_device, a_comment){
  # Create all combinations of homenames and rooms
  combinations <- expand.grid(a_homenames, a_rooms)
  
  # Initialize new dataframe with the correct number of rows
  new_var <- data.frame(matrix(NA, nrow = nrow(combinations), ncol = length(colnames(a_wbsheet))))
  colnames(new_var) <- colnames(a_wbsheet)
  
  # Populate the new dataframe
  new_var$ID <- paste("0012-Lueftung3", combinations$Var1, combinations$Var2, a_var, sep = "-")
  new_var$`Variable.additional.information` <- a_info
  new_var$`Variable.unit` <- a_unit
  new_var$`Measurement.device` <- a_device
  new_var$`Comments.(e.g.,.sensor.location)` <- a_comment
  
  # Combine with the existing dataframe
  a_wbsheet <- rbind(a_wbsheet, new_var)
  return(a_wbsheet)
}

add_metadata_var <- function(a_wb, a_homenames){
  tryCatch({
    meta.var <-readWorkbook(a_wb, sheet="META-Variable")
    meta.var$ID <- gsub("0012-AT_IBO", "0012-Lueftung3", meta.var$ID)
    meta.var$ID <- gsub("OTH", "BED", meta.var$ID)
    meta.var$ID <- gsub("F-", "-", meta.var$ID)
    meta.var <- meta.var[!duplicated(meta.var), ]
    
    # Add variables
    meta.var <- create_new_var(meta.var, a_homenames, c("LIV", "SUP", "AMB"), "T", "<Additional variable information>", "C", "<Additional Information>", "<Measurement Device Info>")
    meta.var <- create_new_var(meta.var, a_homenames, "BED", "T_1", "<Additional variable information>", "C", "<Additional Information>", "<Measurement Device Info>")
    meta.var <- create_new_var(meta.var, a_homenames, c("LIV", "SUP", "AMB"), "RH", "<Additional variable information>", "%", "<Additional Information>", "<Measurement Device Info>")
    meta.var <- create_new_var(meta.var, a_homenames, "BED", "RH_1", "<Additional variable information>", "%", "<Additional Information>", "<Measurement Device Info>")
    meta.var <- create_new_var(meta.var, a_homenames, c("LIV", "BED", "SUP", "AMB"), "Fungi", "<Additional variable information>", "CFU/m3", "Microbial air sampler (MAS-100, Merck)", "A microbial air sampler operating on the impaction method was used to detect viable yeasts and molds at a nominal airflow rate of 100 liters per minute (± 2.5%). Dichloran-Glycerin (DG18) agar was used as the culture medium for isolating and counting cultivable molds. After sampling, the media were incubated for 3 to 7 days at 25°C (±1°C).")
    meta.var <- create_new_var(meta.var, a_homenames, c("BED", "AMB"), "Ions(-)", "<Additional variable information>", "", "<Additional Information>", "<Measurement Device Info>")
    meta.var <- create_new_var(meta.var, a_homenames, c("BED", "AMB"), "Ions(+)", "<Additional variable information>", "", "<Additional Information>", "<Measurement Device Info>")
    meta.var <- create_new_var(meta.var, a_homenames, "LAU", "Mites (Der p1)", "<Additional variable information>", "ug/g", "Enzyme-linked immunosorbent assay (Indoor Biotechnologies)", "Dust samples were collected from bedroom mattresses and living room textiles for allergen analysis. For values below the detection limit (typically 0,1 ug/g), the detection limit was reported.")
    meta.var <- create_new_var(meta.var, a_homenames, "LAU", "Mites (Der f1)", "<Additional variable information>", "ug/g", "Enzyme-linked immunosorbent assay (Indoor Biotechnologies)", "Dust samples were collected from bedroom mattresses and living room textiles for allergen analysis. For values below the detection limit (typically 0,1 ug/g), the detection limit was reported.")
    meta.var <- create_new_var(meta.var, a_homenames, c("LIV", "BED"), "HCHO", "<Additional variable information>", "ug/m3", "<Additional Information>", "Sampling was conducted in the center of the room at a height between 1.2 and 1.5 meters.")
    meta.var <- create_new_var(meta.var, a_homenames, c("LIV", "BED"), "TVOC", "<Additional variable information>", "ug/m3", "<Additional Information>", "Sampling was conducted in the center of the room at a height between 1.2 and 1.5 meters. For values below the detection limit (typically 20 ug/m3), the detection limit was reported.")
    meta.var <- create_new_var(meta.var, a_homenames, c("LIV", "BED", "OTH"), "RAD", "<Additional variable information>", "Bq/m3", "Passive nuclear track detector (RSKS, Radosys)", "Radon measurements were taken in the three most occupied rooms over a year, with the annual average for each object calculated as the arithmetic mean of the individual concentrations.")
    
    meta.var <- meta.var[order(meta.var$ID), ]
    writeData(wb=a_wb, sheet = "META-Variable", x = meta.var, rowNames = FALSE, colNames = FALSE, startRow = 2, startCol = 1)
    return(a_wb)
  }, error = function(e){
    print(paste("An error occurred while adding metadata to Variable:", e$message))
  })
}


# Add meta data to workbooks ---------------------------------------------------
p_meta_wb <- add_metadata_stat(p_meta_wb, p_measurementnames)
k_meta_wb <- add_metadata_stat(k_meta_wb, k_measurementnames)

p_meta_wb <- add_metadata_study(p_meta_wb)
k_meta_wb <- add_metadata_study(k_meta_wb)

p_meta_wb <- add_metadata_home(p_meta_wb, p_homenames, "P")
k_meta_wb <- add_metadata_home(k_meta_wb, k_homenames, "K")

p_meta_wb <- add_metadata_room(p_meta_wb, p_homenames, "P")
k_meta_wb <- add_metadata_room(k_meta_wb, k_homenames, "K")

p_meta_wb <- add_metadata_var(p_meta_wb, p_homenames)
k_meta_wb <- add_metadata_var(k_meta_wb, k_homenames)


# Save workbooks ---------------------------------------------------------------
saveWorkbook(k_meta_wb, ".\\Annex86_AP3_out\\AUT\\Lueftung3\\K50_KH10F.xlsx", overwrite = TRUE)
saveWorkbook(p_meta_wb, ".\\Annex86_AP3_out\\AUT\\Lueftung3\\P00F_PF11F.xlsx", overwrite = TRUE)

