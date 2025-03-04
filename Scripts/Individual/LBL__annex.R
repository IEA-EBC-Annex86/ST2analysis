library("annex")

int_list <- c(0, 1, 2, 3, 4, 5, 6, 10, 11, 12)

# Iterate over each element in the list
for (element in int_list) {
  # Define the string variables
  prefix <- "HENGH_data_"
  prefix_config <- "HENGH_data_config_"
  prefix_output <- "hengh_LBL_"
  suffix <- ".txt"
  suffix_output <- "_v3.xlsx"
  
  # Example integer to string conversion
  str_element <- as.character(element)
  
  # Concatenate strings using paste0
  concatenated_string <- paste0(prefix, str_element, suffix)
  concatenated_string_config <- paste0(prefix_config, str_element, suffix)
  concatenated_string_output <- paste0(prefix_output, str_element, suffix_output)
  
  
  raw_df <- read.csv(concatenated_string)
  config <- read.table(concatenated_string_config,
                       comment.char = "#", sep = "",
                       header = TRUE, na.strings = c("NA", "empty"))
  # see ?read.table for details
  
  # Class and dimension of the objects
  c("raw_df" = is.data.frame(raw_df), "config" = is.data.frame(config))
  
  cbind("raw_df" = dim(raw_df), "config" = dim(config))
  
  annex_check_config(config)
  
  raw_df <- transform(raw_df, X = as.POSIXct(X, tz = "UTC"))
  
  class(raw_df$X)
  
  prepared_df <- annex_prepare(raw_df, config, quiet = TRUE)
  
  annex_df <- annex(RH + T + CO2 + PM25 + HCHO + NO2 ~ datetime | study + home + room,
                    data = prepared_df, tz = "Europe/Berlin")
  
  stats <- annex_stats(annex_df, format = "long")
  
  annex_write_stats(stats, file = concatenated_string_output, user = 0005) 
  print(element)
}

int_list <- c(0, 1, 2, 3)

# Iterate over each element in the list
for (element in int_list) {
  # Define the string variables
  prefix <- "lia_data_"
  prefix_config <- "lia_data_config_"
  prefix_output <- "lia_LBL_"
  suffix <- ".txt"
  suffix_output <- "_v3.xlsx"
  
  # Example integer to string conversion
  str_element <- as.character(element)
  
  # Concatenate strings using paste0
  concatenated_string <- paste0(prefix, str_element, suffix)
  concatenated_string_config <- paste0(prefix_config, str_element, suffix)
  concatenated_string_output <- paste0(prefix_output, str_element, suffix_output)

  raw_df <- read.csv(concatenated_string)
  config <- read.table(concatenated_string_config,
                       comment.char = "#", sep = "",
                       header = TRUE, na.strings = c("NA", "empty"))
  # see ?read.table for details
  
  # Class and dimension of the objects
  c("raw_df" = is.data.frame(raw_df), "config" = is.data.frame(config))
  
  cbind("raw_df" = dim(raw_df), "config" = dim(config))
  
  annex_check_config(config)
  
  raw_df <- transform(raw_df, X = as.POSIXct(X, tz = "UTC"))
  
  class(raw_df$X)
  
  prepared_df <- annex_prepare(raw_df, config, quiet = TRUE)
  
  annex_df1 <- annex(RH + T + CO2 + PM10 + PM25 + HCHO ~ datetime | study + home + room,
                    data = prepared_df, tz = "Europe/Berlin")
  annex_df2 <- annex(RH + T + CO2 + PM1 + PM10 + PM25 + NO2 + HCHO ~ datetime | study + home + room,
                     data = prepared_df, tz = "Europe/Berlin")
  
  if (element == 0) {
    stats <- annex_stats(annex_df1, format = "long")
  } else {
    stats <- annex_stats(annex_df2, format = "long")
  }
  
  
  annex_write_stats(stats, file = concatenated_string_output, user = 0005) 
  print(element)
}