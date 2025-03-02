#' read in annex result files

# function to add columns with NAs in case some older xls output files have missing columns,
# e.g. Meta_home didn't have "Additional comments" at the beginning, based on ChatGPT proposal
safe_rbind <- function(df1, df2) {
  # Get all column names from both data frames
  all_cols <- union(names(df1), names(df2))
  
  # Add missing columns with NA to both data frames
  for (col in setdiff(all_cols, names(df1))) {
    df1[[col]] <- rep(NA, nrow(df1))
  }
  for (col in setdiff(all_cols, names(df2))) {
    df2[[col]] <- rep(NA, nrow(df2))
  }
  
  # Ensure column order is the same before rbind
  df1 <- df1[all_cols]
  df2 <- df2[all_cols]
  
  # Bind the data frames
  return(rbind(df1, df2))
}

read_annex <- function(dir_dat){
  
  # create list with existing studies and files
  studies <- list()
  ffn_all <- c()
  country_match <- tibble(country = character(), folder = character(), study = character())
  message("Checking for annex analysis files in folder ", dir_dat)
  for (c in countries) {
    # find subfolders in country folder
    t_names <- list.dirs(path = paste0(dir_dat, c), full.names= FALSE, recursive = FALSE)
    t_paths <- list.dirs(path = paste0(dir_dat, c), full.names= TRUE, recursive = FALSE)
    for (i in 1:length(t_names)) {
      studies[[c]][[t_names[i]]]$paths <- t_paths[i]
      studies[[c]][[t_names[i]]]$ffn <- list.files(path = t_paths[i], full.names = TRUE, "*.xlsx")
      #reads in the excel sheet an additional to match the country to the study (nessecary?)
      country_match <- add_row(country_match, country = c, study = unique(read_excel(studies[[c]][[t_names[i]]]$ffn[1], sheet = 'STAT')$study),  folder = t_names[i])
      ffn_all <- c(ffn_all, studies[[c]][[t_names[i]]]$ffn)
    }
  }
  country_match <- na.omit(country_match)
  # tbd: add possibility to select studies (subfolders) maybe through external config (xlsfile)
  # a code at the beginning could generate this file
  
  # read data
  stats <- data.frame()
  meta_s <- data.frame()
  meta_h <- data.frame()
  meta_r <- data.frame()
  meta_v <- data.frame()
  for (i_c in 1:length(studies)) {
    message("Country: ", names(studies)[i_c])
    for (i_n in 1:length(studies[[i_c]])) {
      message("Loading data from study: ", names(studies[[i_c]])[i_n])
      for (i_f in 1:length(studies[[i_c]][[i_n]]$ffn)) {
        
        # read STAT data and check for NAs 
        tmp_read <- read_excel(studies[[i_c]][[i_n]]$ffn[i_f], sheet = 'STAT', na="NA")
        idx_na <- is.na(tmp_read$study)
        if (sum(idx_na)) {
          message("!!Removing ", sum(idx_na), " rows with undefined study in STAT in ", studies[[i_c]][[i_n]]$ffn[i_f], ".")
          tmp_read <- tmp_read[!idx_na,]
        }
        stats <- rbind(stats, tmp_read)
        
        # read META-Study data and check for NAs 
        tmp_read <- read_excel(studies[[i_c]][[i_n]]$ffn[i_f], sheet = 'META-Study', na="NA")
        idx_na <- is.na(tmp_read$ID)
        if (sum(idx_na)) {
          message("!!Removing ", sum(idx_na), " rows with undefined ID in META-Study in ", studies[[i_c]][[i_n]]$ffn[i_f], ".")
          tmp_read <- tmp_read[!idx_na,]
        }
        meta_s <- rbind(meta_s, tmp_read)
        
        # read META-Home data and check for NAs 
        tmp_read <- read_excel(studies[[i_c]][[i_n]]$ffn[i_f], sheet = 'META-Home', na="NA") #[1:17]
        idx_na <- is.na(tmp_read$ID)
        if (sum(idx_na)) {
          message("!!Removing ", sum(idx_na), " rows with undefined ID in META-Home in ", studies[[i_c]][[i_n]]$ffn[i_f], ".")
          tmp_read <- tmp_read[!idx_na,]
        }
        
        meta_h <- safe_rbind(meta_h, tmp_read)
        
        # read META-Room data and check for NAs 
        tmp_read <- read_excel(studies[[i_c]][[i_n]]$ffn[i_f], sheet = 'META-Room', na="NA")
        idx_na <- is.na(tmp_read$ID)
        if (sum(idx_na)) {
          message("!!Removing ", sum(idx_na), " rows with undefined ID in META-Room in ", studies[[i_c]][[i_n]]$ffn[i_f], ".")
          tmp_read <- tmp_read[!idx_na,]
        }
        meta_r <- rbind(meta_r, tmp_read)
        
        # read META-Variable data and check for NAs 
        tmp_read <- read_excel(studies[[i_c]][[i_n]]$ffn[i_f], sheet = 'META-Variable', na="NA")
        idx_na <- is.na(tmp_read$ID)
        if (sum(idx_na)) {
          message("!!Removing ", sum(idx_na), " rows with undefined ID in ", studies[[i_c]][[i_n]]$ffn[i_f], ".")
          tmp_read <- tmp_read[!idx_na,]
        }
        meta_v <- rbind(meta_v, tmp_read)
        
        message("Statistics and meta data of ", studies[[i_c]][[i_n]]$ffn[i_f], " has been read")
        
      }
    }
  }
  
  data <- list(stats=stats,meta_s=meta_s,meta_h=meta_h,meta_r=meta_r,meta_v=meta_v)
  return(data)
}

remove_duplicates <- function(df,data_name,dir_tmp){
  # Check for duplicates (in ID columns) and check if the other columns are identical
  # ID duplicates can occur e.g. for META-Study and homes, if studies and/or homes are read from multiple annex files)
  message("Checking duplicates in ", data_name)
  check_dupl <- df %>%
    group_by(ID) %>%  # Group by the selected columns
    filter(n() > 1) %>%     # Keep only groups with duplicates
    mutate(nr = n()) %>%
    mutate(all_ident = all(duplicated(across(everything())) | duplicated(across(everything()), fromLast = TRUE))) %>%
    arrange(ID) %>%
    ungroup()
  if (any(!check_dupl$all_ident)){
    message("!!*** There are duplicated IDs with contradicting entries, see: ")
    print(unique(check_dupl$ID[!check_dupl$all_ident]))
    # output differing entries
    diff_entries <- unique(check_dupl[!check_dupl$all_ident,])
    dir.create(dir_tmp, showWarnings =FALSE, recursive=TRUE)
    write_csv(diff_entries, paste0(dir_tmp,"/contradicting_duplicates_",data_name,".csv"))
    message("Check file contradicting_duplicates_",data_name, ".csv and fix contradictions.")
    message("Using first entry only, even if there were contradictions found (Warnings above)!")
  }
  message(data_name, ": Reducing the following number of duplicates to single (first) entry: ",sum(unique(check_dupl[c("ID","nr")])[[2]]) )
  #print(unique(check_dupl[c("ID","nr")]))
  df <- df[!duplicated(df$ID), ]
  return(df)
}