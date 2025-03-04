## Original: Andreas Frei
## Adaptions: Lena Mayr
## Jul 2024

' 
Comments:
Absolute paths were used due to RDOMClient package and SeaShare. 
Unfortunately, this limits portability and flexibility, consequently, the paths 
will need to be modified when reusing the code.

'

# R --vanilla < AT_IBO_annex.R
print("-> AT_IBO")


# Load necessary libraries -----------------------------------------------------
library("annex")
library("openxlsx")
library("RDCOMClient")


# Set working directory --------------------------------------------------------
cwd <- "C:\\Users\\lenag\\seadrive_root\\Lena Sop\\Shared with groups"
setwd(cwd)


# Create directories -----------------------------------------------------------
dir_cfg <- paste0(cwd, "\\Annex86_AP3_data\\AUT\\Lueftung3\\prepro\\cfg")
dir_dat <- paste0(cwd, "\\Annex86_AP3_data\\AUT\\Lueftung3\\prepro\\data")
dir_out <- paste0(cwd, "\\Annex86_AP3_out\\AUT\\Lueftung3")


# Create list of CO2 measurements ----------------------------------------------
measurementnames <- gsub("\\.csv$", "", list.files(path=dir_dat))
p_measurementnames <- grep("^P", basename(measurementnames), value = TRUE)
k_measurementnames <- grep("^K", basename(measurementnames), value = TRUE)


# Names of output files and paths ----------------------------------------------
p_fn_out <- "P00F_PF11F"
k_fn_out <- "K50_KH10F"

p_path_out <- paste0(dir_out, "\\", p_fn_out, ".xlsx")
k_path_out <- paste0(dir_out, "\\", k_fn_out, ".xlsx")


# Helper function to avoid openxlsx bug ----------------------------------------
read_save_xlsx <-function(a_filepath){
  # temporary solution until bug in openxls is fixed (openxlsx is used for writing xls within annex package) 
  # Create an Excel application instance
  excel_app <- COMCreate("Excel.Application")
  excel_app[["Visible"]] <- FALSE  # Set to TRUE if you want to see the Excel window
  excel_app[["DisplayAlerts"]] <- FALSE  # Suppress any prompts
  # Try to open, save, and close the workbook
  tryCatch({
    # Open the workbook
    workbook <- excel_app[["Workbooks"]]$Open(a_filepath)
    # Save the workbook
    workbook$SaveAs(a_filepath)
    # Close the workbook
    workbook$Close()
    
    print(paste("Excel file has been saved as", a_filepath))
  }, error = function(e) {
    print(paste("An error occurred:", e$message))
  }, finally = {
    # Quit the Excel application
    excel_app$Quit()
  })
}


# Function to write multiple homes into one file using annex -------------------
multi_annex <- function(a_list, a_outputname){
  all_stats <- list()
  # Calculate stats for each home
  for (i in 1:length(a_list)){
    name <- a_list[i]
    fn_dat <- paste(dir_dat, "\\", name, ".csv", sep="")
    fn_cfg <- paste(dir_cfg, "\\", name, ".cfg", sep="")
    raw_df <- read.csv(fn_dat)
    config <- read.csv(fn_cfg, na.strings=c("NA", "empty"))
    config <- annex_check_config(config)
    raw_df <- transform(raw_df, tst=as.POSIXct(tst, tz="UTC"))
    prepared_df <- annex_prepare(raw_df, config, quiet=TRUE)
    annex_df <- annex(RH + T + CO2 ~ datetime | study + home + room, data=prepared_df, tz="Europe/Berlin")
    stats <- annex_stats(annex_df, format="long")
    all_stats[[name]] <- stats
  }
  # combine all homes and write into one excel-sheet
  combined_stats <- do.call(rbind, all_stats)
  annex_write_stats(combined_stats, file=paste(dir_out, "\\", a_outputname, ".xlsx", sep=""), user=12)
}


# Run annex and create file for passive and conventional homes -----------------
multi_annex(p_measurementnames, p_fn_out)
multi_annex(k_measurementnames, k_fn_out)


# Open and save workbooks ------------------------------------------------------
read_save_xlsx(p_path_out)
read_save_xlsx(k_path_out)
