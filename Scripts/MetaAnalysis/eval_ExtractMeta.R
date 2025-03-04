#' extract information from meta information
library(stringr)

# old functions (by Timm) now don't work any more: why?
# updated by chatGPT assisted version, see below
# old versions-------------

# function to extract the volume information from the comment column in meta_r
extract_volume_old <- function(text) {
  
  # extract all numbers from the comment strings
  vol_string <- str_extract_all(text, "\\d{1,3}(?:[.,]\\d+)*")[[1]]
  vol <- tail(vol_string, 1)
  
  # check if vol is empty string
  if (length(vol)){
    # check if vol is NA
    if (is.na(vol))
      return(NA)
    
    # check if third entry of values is empty or only has spaces
    # for case "2.42 , , ,  - Height (m) , Floor Area (m2) , Surface Area (m2) , Volume (m3)"
    # third entry as proxy for Volume, as Surface Area is also missing when Volume is missing
    if (grepl("^\\s*$", strsplit(text, c(','))[[1]][3]))
      return(NA)
    
    # check if first read volume is 2 and return NA as proxy for case
    # for case "Room (also) used as Home-Office; [...] Room area (m2): [...]
    # for first entry value unlikely to be exactly 2?
    if (vol_string[1] == 2)
      return(NA)
    
    # check if last entry in vol_string is '3' for case: "Area: 16.5 m2, Volume: 40 m3"
    # and remove last entry from vol_string to get the volume value
    if(vol == '3'){
      vol_string <- vol_string[-length(vol_string)]
      vol <- tail(unlist(str_extract_all(vol_string, "\\d+(?:\\.\\d+)?")), 1)
    }
    
    # if last value '2', extract all values with decimal point and get last one for volume
    # for case: "1.96 , 4.31 , 16.44 , 8.43 , Height (m) , Floor Area (m2) , Surface Area (m2) , Volume (m3)"
    if(vol == '2'){
      vol <- tail(unlist(str_extract_all(vol_string, "\\d+\\.\\d+")), 1)
    }
  }
  
  # Replace commas with dots for decimal consistency
  vol <- str_replace(vol, ",", ".")
  
  return(as.numeric(vol))
}

# function to extract the area information from the comment column in meta_r
extract_area_old <- function(text) {
  
  area <- NA
  a_string <- list()
  
  if (length(area)){
    if (grepl('Area:', text)){
      # 
      a_string <- str_extract_all(text, "\\d{1,3}(?:[.,]\\d+)*")[[1]]
      area <- a_string[1]
    }
    
    if (grepl('Floor Area |Room area', text)){
      #
      a_string <- str_extract_all(text, "\\d{1,3}(?:[.,]\\d+)*")[[1]]
      area <- a_string[2]
      if (grepl('-', str_split(text, ',')[[1]][1])){
        area <- a_string[3]
      }
      if (grepl('Home-Office', text))
        area <- a_string[2]
    }
    
    if (length(na.omit(area=='2'))){
      if (area =='2')
        area <- NA
      else if (area == '1')
        area <- NA
    }
  }
  
  # Replace commas with dots for decimal consistency
  area <- str_replace(area, ",", ".")
  
  return(area)
}


# new versions-----------
# Function to extract volume from a text string
extract_volume <- function(text) {
  # Validate input
  if (is.na(text) || !is.character(text)) {
    return(NA)
  }
  
  # Normalize special characters (e.g., replace `m?` with `m3`)
  text <- str_replace_all(text, "m?", "m3")
  
  # Define regex pattern for volume values with and without units
  # this is a less strict pattern, extracting numbers also if there is neither pattern "vol", etc. nor unit 
  #pattern <- "(?i)(?:vol(?:ume)?:?\\s*|\\b)(\\d{1,3}(?:[.,]\\d+)?)(?:\\s*(?:m3|m\\^3|m3)\\b)?"
  # this pattern is more strict, always requires a unit
  pattern <- "(?i)(?:vol(?:ume)?:?\\s*|\\b)(\\d{1,3}(?:[.,]\\d+)?)\\s*(?:m3|m\\^3|m3)\\b"
  
  # Extract all matches
  matches <- str_match_all(text, pattern)[[1]]
  
  # Check the number of matches
  if (length(matches[, 2]) == 0) {
    return(NA)  # No matches found
  } else if (length(matches[, 2]) > 1) {
    message("Multiple volume matches found: ", paste(matches[, 2], collapse = ", "),
            ". Returning the first match: ", matches[1, 2])
  }
  
  # Extract the first numeric match
  vol <- matches[1, 2]
  
  # Check if the match was missing a unit
  if (!grepl("(?:m3|m\\^3|m3)", text)) {
    message("Volume match found without a unit. Assuming 'm3'. See: ", text)
  }
  
  # Replace commas with dots for consistent numeric representation
  vol <- str_replace(vol, ",", ".")
  
  return(as.numeric(vol))
}

# Function to extract area from a text string
extract_area <- function(text) {
  # Validate input
  if (is.na(text) || !is.character(text)) {
    return(NA)
  }
  
  # Normalize special characters (e.g., replace `m?` with `m3`)
  text <- str_replace_all(text, "m?", "m2")
  
  # Define regex pattern for volume values with and without units
  # this is a less strict pattern, extracting numbers also if there is neither pattern "vol", etc. nor unit 
  #pattern <- "(?i)(?:vol(?:ume)?:?\\s*|\\b)(\\d{1,3}(?:[.,]\\d+)?)(?:\\s*(?:m3|m\\^3|m3)\\b)?"
  # this pattern is more strict, always requires a unit
  pattern <- "(?i)(?:ar(?:ea)?:?\\s*|\\b)(\\d{1,3}(?:[.,]\\d+)?)\\s*(?:m2|m\\^2|m2)\\b"
  
  # Extract all matches
  matches <- str_match_all(text, pattern)[[1]]
  
  # Check the number of matches
  if (length(matches[, 2]) == 0) {
    return(NA)  # No matches found
  } else if (length(matches[, 2]) > 1) {
    message("Multiple area matches found: ", paste(matches[, 2], collapse = ", "),
            ". Returning the first match: ", matches[1, 2])
  }
  
  # Extract the first numeric match
  ar <- matches[1, 2]
  
  # Check if the match was missing a unit
  if (!grepl("(?:m2|m\\^2|m2)", text)) {
    message("Area match found without a unit. Assuming 'm2'. See: ", text)
  }
  
  # Replace commas with dots for consistent numeric representation
  ar <- str_replace(ar, ",", ".")
  
  return(as.numeric(ar))
}

# Function to check if Ventilaton type has been defined (a must have entry)
check_vent_type <- function(df){
  idxNoVent <- !df$vent_type %in% c("Window airing (not designed)", "Natural ventilation (designed)", "Hybrid/mixed mode ventilation", "Mechanical ventilation")
  message("There are ", sum(idxNoVent, na.rm=TRUE), " data points where ventilation type is NOT defined.")
  if(sum(idxNoVent, na.rm=TRUE)>0){
    message("See:")
    print(unique(paste(df$study[idxNoVent],df$home[idxNoVent], sep="-" )))
  }
  df$vent_type[idxNoVent] <- NA
  
  # alternative with dplyr pipe, with glue package a message could be generated
  #data1 <- data %>%
  #  filter(data$`Ventilation type` %in% c("Window airing (not designed)", "Natural ventilation (designed)", "Hybrid/mixed mode ventilation", "Mechanical ventilation"))
  
  return(df)
}

