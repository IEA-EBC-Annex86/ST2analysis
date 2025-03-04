## IEA EBC Annex 86 Post-preprocessing
## Part: Functions
## Original: Reto Stauffer
## Lena Mayr
## Nov 2024

' 
Comments:
In the analysis of sinfonia data, it was found that the minimum CO2 concentrations
are unrealistically low (<< 400 ppm), which should be the minimum value. Thus, 
after preprocessing, data is cleaned and shifted to higher values.

'


# Sanity checks ----------------------------------------------------------------

#' @title Check if a variable is numeric and contains non-NA values.
#'
#' @description
#' Validates that the input variable is numeric and contains at least one non-NA value. 
#' If these conditions are not met, the function throws an error.
#'
#' @param a_var Numeric vector. The input variable to check.
#' 
#' @return The input variable if it is numeric and contains non-NA values.
#' 
check_numeric <- function(a_var) {
  stopifnot("Variable must be numeric." = inherits(a_var, "numeric"))
  stopifnot("Variable contains only NA values." = !all(is.na(a_var)))
  return(a_var)
}


#' @title Check if a variable is of class POSIXct and contains non-NA values.
#' 
#' @description
#' Checks whether the input variable is of class POSIXct and contains at least one non-NA value. 
#' If these conditions are not met, the function throws an error.
#' 
#' @param a_var POSIXct vector. The input variable to check.
#' 
#' @return The input variable if it is of class POSIXct and contains non-NA values.
#' 
check_posixct <- function(a_var) {
  stopifnot("Variable must have POSIXct format." = inherits(a_var, "POSIXct"))
  stopifnot("Variable contains only NA values." = !all(is.na(a_var)))
  return(a_var)
}


#' @title Check if a data frame is valid and contains expected columns.
#'
#' @description
#' This function validates that the input data frame is non-empty, contains all expected columns, 
#' and verifies that specified columns have the correct types (POSIXct for time stamps and numeric for certain columns).
#' If these conditions are not met, an error is thrown.
#'
#' @param a_df Data frame. The input data frame to check.
#' @param a_expectedcols Character vector. Expected column names (must be present in the data frame).
#' @param a_timecol Character value. A string representing the column name that contains time stamps. 
#' @param a_numericcols Character vector. Names of columns that should contain numeric values.
#' 
#' @return The modified data frame if all checks pass. If any check fails, an error is thrown.
#' 
check_df <- function(a_df, a_expectedcols, a_timecol, a_numericcols) {
  stopifnot("Argument `a_df` must be a data.frame." = is.data.frame(a_df))
  stopifnot("Dataframe `a_df` is empty." = (nrow(a_df) > 0 && ncol(a_df) > 0))
  stopifnot("Missing expected columns." = all(a_expectedcols %in% colnames(a_df)))
  
  # Check time column
  a_df[[a_timecol]] <- check_posixct(a_df[[a_timecol]])
  
  # Check numeric columns
  for (i in a_numericcols) {
    a_df[[i]] <- check_numeric(a_df[[i]])
  }
  
  return(a_df)
}


# Functions for cleaning the data ----------------------------------------------

#' @title Remove values outside specified bounds.
#' 
#' @description
#' This function takes a numeric vector and replaces all values that are outside the specified lower and upper bounds 
#' with NA. It checks each element of the vector and applies the boundary condition to it.
#' 
#' @param a_data Numeric vector. The input vector that will be processed.
#' @param a_lower Numeric value. The lower boundary below which values are replaced with NA.
#' @param a_higher Numeric value. The upper boundary above which values are replaced with NA.
#' 
#' @return The modified numeric vector and the number of values outside the specified bounds replaced by NA.
#' 
remove_outsidebounds <- function(a_data, a_lower, a_higher) {
  na_count_before <- sum(is.na(a_data))
  a_data[a_data < a_lower | a_data > a_higher] <- NA
  na_count_after <- sum(is.na(a_data))
  na_introduced <- na_count_after - na_count_before
  
  return(list(data = a_data, na_introduced = na_introduced))
}


#' @title Remove single spikes based on consecutive observations
#' 
#' @description
#' This function searches for single spikes in a time series by checking three consecutive observations: the current, previous, and next values. 
#' The function calculates the absolute differences between the current and previous observations, and between the next and current observations. 
#' If the differences meet the specified conditions and the time between observations is less than the specified maximum time difference (a_dtmax), 
#' it will mark the current observation as a spike (replacing it with NA). 
#' The function avoids detecting spikes around missing data by ensuring that the time difference between observations is within the specified bounds.
#'
#' @param a_time Numeric vector. Represents the time stamps of the observations. 
#' @param a_data Numeric vector. Contains the data values that are checked and modified.
#' @param a_dtmax Numeric value. Specifies the maximum allowed time difference between consecutive observations.
#' @param a_tau1 Numeric value. Specifies the minimum threshold for the absolute differences in the data values for being considered a spike.
#' @param a_tau2 Numeric value. Specifies the maximum allowed absolute difference between the first and third observations to confirm a spike.
#' 
#' @return The modified input data and the number of values replaced by NA (identified as spikes).
#' 
remove_single_spikes <- function(a_time, a_data, a_dtmax, a_tau1, a_tau2){
  na_count_before <- sum(is.na(a_data))
  n <- length(a_time)
  for (i in 2:(n-1)){
    dt1 <- a_time[i] - a_time[i - 1]
    dt2 <- a_time[i+1] - a_time[i]
    
    # Skip if time differences are too large
    if(dt1 > a_dtmax | dt2 > a_dtmax){
      next
    }
    
    # Skip if any data value is missing
    if(is.na(a_data[i-1]) | is.na(a_data[i]) | is.na(a_data[i+1])){
      next
    }
    
    # Calculate differences in data values
    dx1 <- a_data[i] - a_data[i-1]
    dx2 <- a_data[i+1] - a_data[i]
    
    # Skip if the differences do not indicate a spike (both increasing or both decreasing)
    if((dx1 >= 0 & dx2 >= 0) | (dx1 <= 0 & dx2 <= 0)){
      next
    }
    
    # Check for a spike using the third difference
    dx3 <- a_data[i-1] - a_data[i+1]
    
    # Replace with NA if conditions are met
    if(abs(dx1) >= a_tau1 & abs(dx2) >= a_tau1 & abs(dx3) < a_tau2){
      a_data[i] <- NA
    }
  }
  na_count_after <- sum(is.na(a_data))
  na_introduced <- na_count_after - na_count_before
  
  return(list(data = a_data, na_introduced = na_introduced))
}


#' @title Remove dynamic spikes from time series data
#' 
#' @description
#' This function identifies and removes dynamic spikes in time series data by examining consecutive observations. 
#' The function checks for large differences between consecutive values and, if a spike is detected, replaces a range of data points 
#' with NA based on a specified width. It uses time differences and thresholds for data differences to decide whether to remove 
#' a section of the data. The function ensures that the time difference between consecutive points does not exceed a specified maximum time.
#' 
#' @param a_time Numeric vector. Contains the time stamps of the observations.
#' @param a_data Numeric vector. Contains the data values that are checked and modified.
#' @param a_width Numeric value. Specifies the number of consecutive points to examine for each observation.
#' @param a_dtmax Numeric value. Specifies the maximum allowed time difference between consecutive observations.
#' @param a_tau1 Numeric value. Specifies the threshold for detecting significant differences between consecutive data points.
#' @param a_tau2 Numeric value. Specifies the threshold for comparing the first and last observations in a spike range.
#' 
#' @return The modified input data and the number of values replaced by NAs (identified as spikes).
#' 
remove_dynamic_spikes <- function(a_time, a_data, a_width, a_dtmax, a_tau1, a_tau2){
  na_count_before <- sum(is.na(a_data))
  n <- length(a_time)
  
  # Iterate over the data, examining each possible spike
  for (i in 1:(n - a_width)) {
    
    # Skip if current or next data point is NA
    if (is.na(a_data[i]) || is.na(a_data[i + 1])) {
      next
    }
    
    # Skip if the difference between consecutive points is smaller than tau1
    if (abs(a_data[i] - a_data[i + 1]) < a_tau1) {
      next
    }
    
    # Check if time difference between consecutive points is too large
    dt <- a_time[i + 1] - a_time[i]
    if (dt > a_dtmax) {
      next
    }
    
    k <- -9
    
    # Check the subsequent points for spikes
    for (j in 1:a_width) {
      dt <- a_time[i + j + 1] - a_time[i + j]
      
      # Skip if time difference is too large or if any data point is NA
      if (is.na(a_data[i + j]) || is.na(a_data[i + j + 1]) || dt > a_dtmax) {
        k <- -9
        break
      } else if (abs(a_data[i + j] - a_data[i + j + 1]) > a_tau1) {
        k <- j
      }
    }
    
    # If a spike is detected, mark the data within the spike range as NA
    if (k > 0) {
      if (abs(a_data[i] - a_data[i + k + 1]) < a_tau2) {
        a_data[(i + 1):(i + k)] <- NA
      }
    }
  }
  na_count_after <- sum(is.na(a_data))
  na_introduced <- na_count_after - na_count_before
  
  return(list(data = a_data, na_introduced = na_introduced))
}


#' @title Remove constant periods in time series data
#' 
#' @description
#' This function detects and removes periods in time series data where consecutive values are constant for a specified duration. 
#' If the absolute difference between consecutive values is smaller than a given threshold (a_tau) for a number of consecutive data points 
#' (specified by a_sus), those values are replaced with NA. It also ensures that the time difference between consecutive observations 
#' does not exceed a specified maximum time difference (a_dtmax).
#' 
#' @param a_time Numeric vector. Contains time stamps of the observations.
#' @param a_data Numeric vector. Contains input data values that are checked and modified.
#' @param a_dtmax Numeric value. Specifies the maximum allowed time difference between consecutive observations. 
#' @param a_sus Numeric value. Specifies the number of consecutive points with values differing by less than a_tau that need to be replaced with NA.
#' @param a_tau Numeric value. Specifies the minimum absolute difference between consecutive data points to be considered constant. 
#' 
#' @return The modified input data and the number of values replaced by NA (identified constant periods).
#' 
remove_constant_periods <- function(a_time, a_data, a_dtmax, a_sus, a_tau = 1e-7){
  na_count_before <- sum(is.na(a_data))
  n <- length(a_time)
  counter <- 0
  
  for (i in 2:n) {
    # Calculate time difference between consecutive points
    dt <- a_time[i] - a_time[i - 1]
    
    # If the time difference exceeds the maximum allowed, reset the counter and continue
    if (dt > a_dtmax) {
      counter <- 0
      next
    }
    
    # If the data points are not NA and the absolute difference is less than tau, increase counter
    if (!is.na(a_data[i - 1]) && !is.na(a_data[i]) && abs(a_data[i - 1] - a_data[i]) < a_tau) {
      counter <- counter + 1
    } else {
      counter <- 0
    }
    
    # If the counter exceeds or equals the specified number of consecutive points (a_sus), mark those points as NA
    if (counter >= a_sus) {
      a_data[(i - counter):i] <- NA
    }
  }
  na_count_after <- sum(is.na(a_data))
  na_introduced <- na_count_after - na_count_before
  
  return(list(data = a_data, na_introduced = na_introduced))
}


# Function for shifting the data -----------------------------------------------

#' @title Shift data to ensure 0.5 quantile exceeds target value
#' 
#' @description
#' This function shifts the data values so that the 0.5th percentile (quantile) of the data 
#' exceeds a specified target value. The data is shifted by adding an offset, which is the 
#' difference between the target and the current 0.5th percentile. If the data cannot be shifted 
#' to meet the target, an error is thrown.
#' 
#' @param a_data Numeric vector. The data values that need to be shifted.
#' @param a_target Numeric value. The target value for the 0.5th percentile of the data.
#' 
#' @return The shifted data values where the 0.5th percentile exceeds the target value and the offset.
#' 
shift_data <- function(a_data, a_target){
  # Calculate 0.5th percentile (0.5 quantile)
  p0.5 <- as.numeric(quantile(a_data, probs = 0.005, na.rm = TRUE))

  # Shift data if 0.5th percentile is smaller than the target
  if (p0.5 < a_target){
    offset <- a_target - p0.5
    a_data <- a_data + offset
  }
  else{
    offset <- 0
  }

  # Check if the data was shifted correctly (tolerance margin to avoid precision errors)
  if (quantile(a_data, probs = 0.005, na.rm = TRUE) < (a_target - 1e-8)){
    stop("Error: Data was not shifted correctly, 0.5th percentile must be at least the target value.")
  }
  
  # Return shifted data
  return(list(data = a_data, offset = offset))
}


# Function for plotting the changed time series --------------------------------

#' @title Plot original and changed data for CO2 concentration over time
#' 
#' @description
#' This function takes two data frames — one with the original CO2 concentration values and another with the modified values — 
#' and generates a plot showing both data series over time. The plot is saved as a PNG file and can be displayed. 
#' The original data is shown in black, and the modified data is shown in red.
#' 
#' @param a_origdf Data frame. Contains original data and must have a time column (tst) and a CO2 concentration column (co2).
#' @param a_changeddf Data frame. Contains modified data and must have a time column (tst) and a CO2 concentration column (co2).
#' @param a_header Character. A string for the plot's main title.
#' @param a_filename Character. A string specifying the filename (including path) for saving the plot (e.g., "plot.png").
#' 
#' @return This function generates a plot of the original and changed CO2 concentrations over time and saves it as a PNG file. 
#' 
plot_df <- function(a_origdf, a_changeddf, a_header, a_filename) {
  # Merge the original and changed data frames by the 'tst' column
  df_merged <- merge(a_origdf, a_changeddf, by = "tst", all = FALSE, suffixes = c("_orig", "_changed"))
  
  # Remove rows where both CO2 values are NA
  df_merged <- df_merged[!(is.na(df_merged$co2_orig) & is.na(df_merged$co2_changed)), ]
  
  # Create and save the plot as a PNG file
  png(a_filename, width = 800, height = 600)
  plot(df_merged$tst, df_merged$co2_orig, col = "black", 
       main = a_header, xlab = "Time", ylab = "CO2 concentration [ppm]", 
       type = "l", lwd = 2)  # 'type = "l"' to plot lines, 'lwd' for line width
  points(df_merged$tst, df_merged$co2_changed, col = "red", pch = 19)  # Add points for changed data
  dev.off()  # Save and close the PNG device
  
  # Return invisible to prevent plot object from printing
  return(invisible())
}

