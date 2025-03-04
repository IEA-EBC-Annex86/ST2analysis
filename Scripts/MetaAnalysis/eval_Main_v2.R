## Annex 86 Meta Analysis
# Timm Freundorfer, Gabriel Rojas
# Jan 2025

# packages-----
#library(remotes)
#install.packages("mvtnorm")
#install_cran("gamlss", repos = "https://gamlss-dev.r-universe.dev")#, force = TRUE)
#install.packages("gamlss2", repos = c("https://gamlss-dev.R-universe.dev", "https://cloud.R-project.org"))

library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(tidyr)
library(openxlsx)
library(readr)
library(gamlss)
library(gamlss2)
library(lubridate)
library(ggpubr)
library(stats)

#meta overview
library(gtools)
library(reshape2)

library(formatR)
library(wordcloud)
library(tm)


## settings-------------------
who <- 'G'
switch(who,
       'T'={dir_src <- 'C:/Users/Timm/Desktop/Atmospheric Sciences/Arbeit'
       dir_dat <- './Annex86_AP3_out/'
       dir_tmp <- '../tmp/'},
       'G'={dir_src <- 'C:/Daten/A86/AP3_AP4/src'
       dir_dat <- '../out/'
       dir_tmp <- '../tmp/'},
       'G2'={dir_src <- 'C:/Users/Gabriel/Daten/A86/AP3_AP4/src'
       dir_dat <- '../out/'
       dir_tmp <- '../tmp/'},
       'L'={dir_src <- 'C:/Users/lenag/seadrive_root/Lena Sop/Shared with me/Lena/1_MetaAnalysis/Meta/src'
       dir_dat <- 'C:/Users/lenag/seadrive_root/Lena Sop/Shared with groups/Annex86_AP3_out/'
       dir_tmp <- 'C:/Users/lenag/seadrive_root/Lena Sop/Shared with me/Lena/1_MetaAnalysis/Meta/tmp/'},
       'C'={dir_src <- ''
       dir_dat <- ''
       dir_tmp <- '../tmp/'},
       'Sim'={dir_src <- 'D:/_USER/Gabriel/annex-86'
       dir_dat <- './out/'
       dir_tmp <- '../tmp/'}
)
setwd(dir_src)

source("eval_ReadData.R")
source("eval_MixDist.R")
source("eval_ExtractMeta.R")
source("eval_Analyse.R")
source("eval_MetaOverview.R")

#sink(paste0("../tmp/", "Log_Evaluation_Script_17-12-24.txt"),type="output",split=TRUE)
#sink()

# define bin size for ecdfs
#Perc_bins0 <- as.numeric(unlist(str_split_i(colnames(sta_fil1_che1[,24:128]),pattern="p",i=2)))
Perc_bins <- c(seq(0,100),0.5, 2.5,97.5,99.5)
Perc_bins <- Perc_bins[order(Perc_bins)]
CO2_bins <- seq(300, 5000, length.out = 150)
CO2_midbins <- CO2_bins[-length(CO2_bins)] + diff(CO2_bins)/2
PM25_bins <- seq(0, 600, length.out = 200)
T_bins <- seq(-20, 40, length.out = 61)
RH_bins <- seq(0, 100, length.out = 101)
HCHO_bins <- seq(0, 100, length.out = 101)
TVOC_bins <- seq(0, 3000, length.out = 151)
VOC_bins <- seq(0, 3000, length.out = 151)
Radon_bins <- seq(0, 3000, length.out = 151)

# define quality limits which entries to exclude in %
set_qual_lim <- c(1,1) # for lower and upper bound

#Choose countries to load datasets from
countries <- c('AUT', 'CHE', 'DNK', 'ESP', 'FRA', 'GBR', 'IRL', 'MEX', 'NLD', 'NOR', 'USA')
#countries <- c('AUT')
#countries <- c('AUT', 'BEL')
#countries <- c('IRL')
#------------------

## Step 1: Read data and prepare all-in-one data frame-----------------------------
data0 <- read_annex(dir_dat) #includes the removal of undefined rows (NAs)
data <- data0 #for development: keep original dataset, otherwise could be deleted
saveRDS(data0, file="../tmp/data0.Rds")
#data <- readRDS("../tmp/data0.Rds")


## Step 1a: Data checks and structuring + basic cleaning ------------
# remove duplicates in META data
data$meta_s <- remove_duplicates(data$meta_s,"META-Study",dir_tmp)
data$meta_h <- remove_duplicates(data$meta_h,"META-Home",dir_tmp)
data$meta_r <- remove_duplicates(data$meta_r,"META-Room",dir_tmp)
data$meta_v <- remove_duplicates(data$meta_v,"META-Variable",dir_tmp)

# split the ID in the meta data as it is in the stats data
# !! use too_few = "debug" or too_many = "debug" if error or too_many = "drop"/"merge"
data$meta_s <- separate_wider_delim(data$meta_s,ID,delim="-",names=c("user","study"), cols_remove = FALSE)
data$meta_h <- separate_wider_delim(data$meta_h,ID,delim="-",names=c("user","study","home"), cols_remove = FALSE)
data$meta_r <- separate_wider_delim(data$meta_r,ID,delim="-",names=c("user","study","home","room"), cols_remove = FALSE) 
# the following gives problems because there are entries with "-" e.g. Der-p1 or so in L?ftung3
data$meta_v <- separate_wider_delim(data$meta_v,ID,delim="-",names=c("user","study","home","room","variable"), cols_remove = FALSE)
#data$meta_v <- separate_wider_delim(data$meta_v,ID,delim="-",names=c("user","study","home","room","variable"), cols_remove = FALSE, too_many = "drop")

# rename columns for easier use/handling
colnames(data$meta_h)[colnames(data$meta_h) =="Location: Country"] <- "country"
colnames(data$meta_h)[colnames(data$meta_h) =="Location: City"] <- "city"
colnames(data$meta_h)[colnames(data$meta_h) =="Ventilation type"] <- "vent_type"
colnames(data$meta_h)[colnames(data$meta_h) =="Comment Vent. Type"] <- "vent_type_comment"
colnames(data$meta_h)[colnames(data$meta_h) =="Size of home / TFA [m^2]"] <- "home_area"
colnames(data$meta_h)[colnames(data$meta_h) =="Ventilation rate (entire home; [l/s])"] <- "vent_rate_home"
colnames(data$meta_r)[colnames(data$meta_r) =="Ventilation rate (room; [l/s])"] <- "vent_rate_room"

# make sure entries in numeric columns are really numeric
data$stats$Mean <- as.numeric(data$stats$Mean)

message("There are ", ifelse(is.numeric(data$meta_h$vent_rate_home),sum(is.na(data$meta_h$vent_rate_home)), 
                             sum(grepl("<Rate Room in [l/s]>", data$meta_h$vent_rate_home)) + sum(grepl("", data$meta_h$vent_rate_home))), " of ", length(data$meta_h$vent_rate_home), " missing or NA entries in vent_rate_room")
data$meta_h$vent_rate_home <- as.numeric(data$meta_h$vent_rate_home)
message("There are ", sum(is.na(data$meta_h$vent_rate_home)), " of ", length(data$meta_h$vent_rate_home), " NA entries in vent_rate_home.")

message("There are ", ifelse(is.numeric(data$meta_r$vent_rate_room),sum(is.na(data$meta_r$vent_rate_room)), 
                             sum(grepl("<Rate Room in [l/s]>", data$meta_r$vent_rate_room)) + sum(grepl("", data$meta_r$vent_rate_room))), " of ", length(data$meta_r$vent_rate_room), " missing or NA entries in vent_rate_room")
data$meta_r$vent_rate_room <- as.numeric(data$meta_r$vent_rate_room)
message("There are ", sum(is.na(data$meta_r$vent_rate_room)), " of ", length(data$meta_r$vent_rate_room), " NA entries in vent_rate_home.")

# check if must have entries (vent_type) are defined correctly and set to NA if not
data$meta_h <- check_vent_type(data$meta_h)

# combine all percentile data columns into one nested data frame (for better structure/overview)
data$stats$perc <- data$stats %>% select(matches("^p[0-9\\.]+$"))
data$stats <- data$stats %>% select(-matches("^p[0-9\\.]+$"))

## Step 1b: data enrichment--------------------
# better here before left joint because more effective
# maybe those variables will be added later to annex package, for now needs to be added here in post proc

# create col N_rel in stats (fraction of the month where data was analysed)
# create room label combining same rooms with different numbers
data$stats <- data$stats %>% 
  mutate(N_rel = data_abu(N,NAs,year,month,tod,interval_Median),
         room0 = factor(substr(data$stats$room, 1,3)),
         .keep = "all") %>%
  relocate(perc,.after = last_col()) %>%
  relocate(room0,.after = room)
#data$stats$room0 <- factor(substr(data$stats$room, 1,3))

# # create cdf for all entries,if possible: needed? would be cleaner and more structured
# data$stats <- data$stats %>%
#   #filter(if_all(p00:p100, ~!is.na(.x))) %>%
#   filter(rowSums(is.na(across(perc))) <= 2) %>%  # alternative less strict,not tested
#   mutate(cdf = per2cdf(perc,Perc_bins,CO2_bins), .keep = "all") %>%
#   relocate(cdf,.before = perc)
# 
# data$stats <- data$stats %>%
#   mutate(cdf = if_else(rowSums(is.na(across(perc))) <= 2, 
#                       per2cdf(perc,Perc_bins,CO2_bins),
#                       NA), .keep = "all") %>%
#   relocate(cdf,.before = perc)

# add room volume and room area to meta_r data frame
tmp_AddRoomInfo <- data$meta_r$`Additionial room information (e.g., ceiling height, with atrium)`
data$meta_r$room_vol <- sapply(tmp_AddRoomInfo, extract_volume, USE.NAMES = FALSE)
data$meta_r$room_area <- sapply(tmp_AddRoomInfo, extract_area, USE.NAMES = FALSE)

#-------------------

## Step 1c: Create all-in-one data frame including some data cleaning -----------------------------------

# create all-in-one data frame "stats" that also incl all meta data
stats <- data$stats %>%
  left_join(data$meta_h, by=c("user","study","home"), suffix = c("_stats","_home"), relationship = "many-to-one") %>%
  left_join(data$meta_r, by=c("user","study","home","room"), suffix = c("_home","_room"), relationship = "many-to-one") 

# check if still NAs in dataset (there shouldn't with read.annex function)
if(sum(is.na(stats$study))) {
  message("Warning: ", sum(is.na(stats$study)), ' rows have study not defined (NA). Removing them.')
  stats <- stats %>% drop_na(study)
}

#--------------------

## Step 1d: Optional extract META info overview to external file-----
META_all <- META_Overview(stats)
write.csv(META_all, paste0(dir_tmp,'/META_all_', Sys.Date(), '.csv'))
#tbd improve output format

## Step 1b_old: Further data enrichment -----------------
# tbd: **** check what is still needed (copied from orig eval)
meta_h$study <- str_split_i(meta_h$ID, '-', i=2)
meta_h$city_out_T <- meta_h$`Location: City`
meta_h$city_out_T[meta_h$study == "Lueftung3"] <- 'Wien'
# add ID columns
stats$ID_h <- paste0(stats$user,"-",stats$study,"-",stats$home)
stats$ID_r <- paste0(stats$user,"-",stats$study,"-",stats$home,"-",stats$room)
# add date column: ?needed? and add 'all' for NA?
stats$date <- as.Date(paste(stats$year, stats$month, "01", sep = "-"), format = "%Y-%m-%d")
# add vent type/rate columns (not the most efficient, maybe change)
stats$vent_type <- meta_h$`Ventilation type`[match(stats$ID_h, meta_h$ID)]
stats$vent_rate_home <- as.numeric(meta_h$`Ventilation rate (entire home; [l/s])`)[match(stats$ID_h, meta_h$ID)]
stats$vent_rate_room <- as.numeric(meta_r$`Ventilation rate (room; [l/s])`)[match(stats$ID_r, meta_r$ID)]
# add timeframe column in days from measurements with either month or year == 'all'
stats$timeframe <- NA
stats[stats$month == 'all' | stats$year == 'all', ]$timeframe <- difftime(stats[stats$month == 'all' | stats$year == 'all', ]$quality_end, stats[stats$month == 'all' | stats$year == 'all', ]$quality_start, units = 'd')
# add energy standard column if available
stats$energy_standard <- meta_h$`Energy standard`[match(stats$ID_h, meta_h$ID)]
stats$building_type <- meta_h$`Type of building`[match(stats$ID_h, meta_h$ID)]
stats$home_size <- meta_h$`Size of home / TFA [m^2]`[match(stats$ID_h, meta_h$ID)]
stats$room_vol <- as.numeric(meta_r$room_vol)[match(stats$ID_r, meta_r$ID)]
stats$room_area <- as.numeric(meta_r$room_area)[match(stats$ID_r, meta_r$ID)]
stats$occupant <- meta_h$`Type of Occupants`[match(stats$ID_h, meta_h$ID)]
stats$vent_type_comment <- meta_h$`Comment Vent. Type`[match(stats$ID_h, meta_h$ID)]
stats$city <- meta_h$`Location: City`[match(stats$ID_h, meta_h$ID)]
# add countries to the corresponding studies
stats <- stats %>%
  left_join(country_match, by = "study")

#----------------

## Step 1e: create / reorder factor levels ----
# vent_type: window airing first (for regression with dummys)
#stats$vent_type <- factor(stats$vent_type)
#levels(stats$vent_type)
stats$vent_type <- factor(stats$vent_type, levels=c("Window airing (not designed)", "Natural ventilation (designed)", "Hybrid/mixed mode ventilation", "Mechanical ventilation"))
levels(stats$vent_type)

# month: "all" first
#stats$month <- factor(stats$month)
#levels(stats$month)
stats$month <- factor(as.numeric(as.character(stats$month)), levels=c("all", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
levels(stats$month)

#study: order alphabetically and TBD: set sum coding (effect coding)
stats$study <- factor(stats$study)
levels(stats$study)
#contrasts(stats$study) <- contr.sum(length(levels(stats$study)))
#contrasts(stats$study)

# factor energy standard: but currently many different wordings for similar things
#CO2_bed_win$`Energy standard` <- as.factor(CO2_bed_win$`Energy standard`)

## Step 1f: Save & Clean----
# Save----
stats0 <- stats
saveRDS(stats, file="../tmp/stats.Rds")
# remove all entries with no value in "Mean" column
message("There are ", sum(is.na(stats$Mean)), " datapoints, with no MEAN VALUE. Will be removed. ", "See: ")
print(unique(paste(stats$study[is.na(stats$Mean)],stats$home[is.na(stats$Mean)],stats$room[is.na(stats$Mean)] , sep="-" )))
stats <- subset(stats, !is.na(stats$Mean))
# remove all rows with N < 30
message('Removing ', nrow(subset(stats, N < 30)), ' rows from dataframe with 30 or less measured datapoints')
stats <- subset(stats, N >= 30)
# remove entries with undefined vent_type
# even though the check_vent_type was run without any finds, there might be one now, if some stats rows could not be assigned a home, e.g. IRL/Validate data
message("Removing ", sum(is.na(stats$vent_type)), " entries with undefined (NA) ventilation type.")
stats <- stats[!is.na(stats$vent_type),]
# clean if data quality (below/above limits)
stats <- checkQual(stats, set_qual_lim)

## Step 2: Filter old----------------------------------

# Filter1: CO2 in BED at night in winter (Months 11-3)
# NA warnings due to conversion to numeric with some "all" -> can be ignored
fil1 <- stats$variable=="CO2" & grepl("BED", stats$room) & stats$month!="all" &
  (as.numeric(stats$month)>=11 | as.numeric(stats$month)<=3) & stats$tod=="23-07"
print(paste0(sum(fil1), " datapoints for this filter"))
sta_fil1 <- stats[fil1,]
META_all_fil1 <- META_Overview(sta_fil1, meta_h, meta_r)

# Filter2: CO2 in LIV during the day in winter (Months 11-3)
fil2 <- stats$variable=="CO2" & grepl("LIV", stats$room) & stats$month!="all" &
  (as.numeric(stats$month)>=11 | as.numeric(stats$month)<=3) & stats$tod=="07-23"
print(paste0(sum(fil2), " datapoints for this filter"))
sta_fil2 <- stats[fil2,]

# Filter3: CO2 in BED at night at any month of year
fil3 <- stats$variable=="CO2" & grepl("BED", stats$room) & stats$month!="all" & stats$tod=="23-07"
print(paste0(sum(fil3), " datapoints for this filter"))
sta_fil3 <- stats[fil3,]

# FilterA: CO2 in any room for full day at any month of year
filA <- stats$variable=="CO2" & stats$month!="all" & stats$tod=="all"
print(paste0(sum(filA), " datapoints for this filter"))
sta_filA <- stats[filA,]


## Step 2: Filter----

CO2_stat <- stats %>%
  filter(variable=="CO2" & year!="all") %>%
  group_by(room0, vent_type) %>%
  summarize(M=mean(Mean), SD=sd(Mean), N=n())

CO2 <- stats %>%
  filter(variable=="CO2" & year!="all" & month!="all" & tod!="all")
sum(CO2$year=="all")
sum(CO2$month=="all")
sum(CO2$tod=="23-07" | CO2$tod=="07-23")
ggboxplot(CO2, x="room0", y="Mean", fill="vent_type")
ggboxplot(CO2, x="vent_type", y="Mean", fill="study")

CO2_24h <- stats %>%
  filter(variable=="CO2" & year!="all" & month!="all" & tod=="all")
sum(CO2$year=="all")
sum(CO2$month=="all")
sum(CO2$tod=="23-07" | CO2$tod=="07-23")
ggboxplot(CO2, x="room0", y="Mean", fill="vent_type")
ggboxplot(CO2, x="vent_type", y="Mean", fill="study")

CO2_bed_liv <- CO2 %>%
  filter(room0=="BED" | room0=="LIV")

# filter bedrooms at night
CO2_bed <- stats %>%
  filter(variable=="CO2" & room0=="BED" & year!="all" & month!="all" & tod=="23-07")
ggboxplot(CO2_bed, x="study", y="Mean", fill="vent_type")
ggboxplot(CO2_bed, x="vent_type", y="Mean", fill="study")
ggboxplot(CO2_bed, x="vent_type", y="perc$p95", fill="study")

# filter bedroom in winter time
CO2_bed_win <- CO2_bed %>%
  filter(as.numeric(month)>=11 | as.numeric(month)<=3)
ggboxplot(CO2_bed_win, x="study", y="Mean", fill="vent_type")
ggboxplot(CO2_bed_win, x="vent_type", y="Mean", fill="study")
ggboxplot(CO2_bed_win, x="vent_type", y="perc$p95", fill="study")

## Step 3: Aggregation to room level----
CO2_bed_ag <- aggRoom(CO2_bed)

CO2_bed_win_ag <- aggRoom(CO2_bed_win)

## First statistical tests with entire data set----
# Comparison of distributions----
oneway.test(Mean ~ vent_type, data = stats, var.equal = TRUE) #assuming equal variance
oneway.test(Mean ~ vent_type, data = stats, var.equal = FALSE)
oneway.test(Mean ~ vent_type, data = stats, na.option = "na.omit", var.equal = FALSE)
oneway.test(Mean ~ vent_type, data = CO2_bed_ag, var.equal = FALSE)


# test clustering---------------
CO2_bed_s <- scale(CO2_bed$perc) # scaling needed here?
plot(Perc_bins, CO2_bed$perc[1,], type="l",ylim=c(0,4000))
lines(Perc_bins, CO2_bed_s[1,], col="red")
plot(Perc_bins, CO2_bed_s[1,], type="l")

km3 <- kmeans(CO2_bed$perc, 3)
km3$tot.withinss
col_vec=rainbow(3)

# plot clusters
ho <- 1000
plot(Perc_bins, CO2_bed$perc[ho,], type="l",col=col_vec[km3$cluster[ho]],ylim=c(0,4000))
for (ho in ho:(ho+500)){
  lines(Perc_bins, CO2_bed$perc[ho,], col=col_vec[km3$cluster[ho]])
}

# generate statistics for different clusters
km_sum <- CO2_bed %>%
  group_by(km3$cluster) %>%
  summarize(M=mean(Mean), n=n(),
            n_mech=sum(grepl("Mechanical", vent_type)),
            n_win=sum(grepl("Window", vent_type)),
            n_hyb=sum(grepl("Hybrid", vent_type)),
            n_nat=sum(grepl("Natural", vent_type))) %>%
  mutate(x_mech=n_mech/sum(n_mech),
         x_win=n_win/sum(n_win),
         x_hyb=n_hyb/sum(n_hyb),
         x_nat=n_nat/sum(n_nat))


## test linear regressions

# standardized lm testing (Lena) -----------------
create_lm_model <- function(data, formula_str) {
  myform <- as.formula(formula_str)
  model <- lm(formula = myform, data = data)
  print(summary(model))
  
  # Retrieve the dependent variable (y) dynamically from the formula
  dependent_var <- as.character(myform[[2]])
  predictors <- all.vars(myform)[-1]  # Exclude dependent variable
  
  # QQ-Plot of Residuals
  qqplot <- ggplot(data.frame(residuals = residuals(model)), aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line() +
    xlab("Theoretical") +
    ylab("Sample") +
    ggtitle("QQ-Plot of Residuals with 95% Percentiles") +
    theme_minimal()
  print(qqplot)
  
  # Residuals vs. Fitted Values Plot
  residuals_vs_fitted <- ggplot(data.frame(fitted = fitted(model), residuals = residuals(model)),
                                aes(x = fitted, y = residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
    ggtitle("Residuals vs. Fitted") +
    xlab("Fitted Values") +
    ylab("Residuals") +
    theme_minimal()
  print(residuals_vs_fitted)
  
  # Scale-Location Plot
  scale_location <- ggplot(data.frame(fitted = fitted(model), sqrt_resid = sqrt(abs(residuals(model)))),
                           aes(x = fitted, y = sqrt_resid)) +
    geom_point() +
    geom_smooth(method = "loess", col = "red") +
    ggtitle("Scale-Location Plot") +
    xlab("Fitted Values") +
    ylab("v|Residuals|") +
    theme_minimal()
  print(scale_location)
  
  # Cook's Distance Plot
  cooksD <- cooks.distance(model)
  cooks_threshold <- 4 / length(cooksD)
  high_cooks_indices <- which(cooksD > cooks_threshold)
  
  if (length(high_cooks_indices) > 0) {
    cat("High Cook's Distance Observations (Index):", high_cooks_indices, "\n")
  } else {
    cat("No observations with high Cook's Distance found.\n")
  }
  
  cooks_distance <- ggplot(data.frame(index = seq_along(cooksD), cooksD = cooksD), 
                           aes(x = index, y = cooksD)) +
    geom_bar(stat = "identity") +
    geom_point(data = data.frame(index = high_cooks_indices, cooksD = cooksD[high_cooks_indices]), 
               aes(x = index, y = cooksD), color = "red", size = 3) +
    ggtitle("Cook's Distance Plot") +
    xlab("Index") +
    ylab("Cook's Distance") +
    theme_minimal()
  print(cooks_distance)
  
  # Regression line or boxplot
  first_predictor <- names(model$model)[2]
  second_predictor <- ifelse(length(names(model$model)) > 2, names(model$model)[3], NA)
  
  if (length(predictors) > 1 && is.numeric(data[[first_predictor]]) && is.character(data[[second_predictor]])) {
    plot_data <- ggplot(data, aes_string(x = first_predictor, y = dependent_var, color = second_predictor)) +
      geom_point() +
      geom_smooth(method = "lm", aes_string(group = second_predictor), se = FALSE) +
      ggtitle("Scatter Plot with Regression Line by Group") +
      labs(x = first_predictor, y = dependent_var, color = second_predictor) +
      theme_minimal()
  } else {
    if (is.character(data[[first_predictor]])) {
      plot_data <- ggplot(data, aes_string(x = first_predictor, y = dependent_var)) +
        geom_boxplot(aes(group = .data[[first_predictor]]), fill = "lightblue", alpha = 0.5) +
        geom_jitter(width = 0.1, size = 1, alpha = 0.7) +
        ggtitle("Boxplot for Factor Predictor with Jitter") +
        labs(x = first_predictor, y = dependent_var) +
        theme_minimal()
    } else {
      plot_data <- ggplot(data, aes_string(x = first_predictor, y = dependent_var)) +
        geom_point() +
        geom_smooth(method = "lm", aes(group = 1), col = "red", se = FALSE) + 
        ggtitle("Scatter Plot with Regression Line") +
        labs(x = first_predictor, y = dependent_var) +
        theme_minimal()
    }
  }
  
  print(plot_data)
  
  return(model)
}


# Data and model formulas
mydata <- CO2_bed
mydata <- CO2_bed %>%
  mutate(delCO2inv = 1/(Mean-400))
mydata <- CO2_bed_win_ag # CO2 data in bedroom, winter at night
model_formulas <- c("delCO2inv ~ vent_rate_room")
model_formulas <- c("Mean ~ vent_type",
                    "Mean ~ vent_type + `Occupancy: Number`",
                    "Mean ~ vent_type + `Occupancy: Number` + `Type of building`") 
model_formulas <- c("Mean ~ vent_rate_room",
                    "Mean ~ vent_rate_room + vent_type",
                    "Mean ~ vent_rate_room + vent_type + room_vol",
                    "Mean ~ vent_rate_room + vent_type + room_vol + `Occupancy:Number`") 

# Create linear models
for (i in model_formulas){
  print(paste("Processing linear model:", i))
  create_lm_model(mydata, i)
}



# standarized lm testing-----
mydata <- CO2_bed_win
mydata <- CO2_bed
mydata <- CO2_bed_liv
mydata <- CO2[CO2$room0=="BED",]
mydata <- CO2

myform <- as.formula("Mean ~ 0 + vent_type")
myform <- as.formula("Mean ~ `Energy standard`")
myform <- as.formula("Mean ~ 0 + `Location: Country`")
myform <- as.formula("Mean ~ 0 + Type of building`")
myform <- as.formula("Mean ~ vent_rate_home")
myform <- as.formula("Mean ~ vent_type:vent_rate_home")
myform <- as.formula("Mean ~ 0 + study")
myform <- as.formula("Mean ~ 0 + month")
myform <- as.formula("Mean ~ 0 + tod")
myform <- as.formula("Mean ~ 0 + room0 + month + tod + vent_type")
#myform <- as.formula("Mean ~ 0 + vent_type + (1+vent_type|study)")

myform <- as.formula("perc$p95 ~ 0 + vent_type")
myform <- as.formula("perc$p95 ~ vent_type:vent_rate_home")
myform <- as.formula("perc$p95 ~ vent_type:vent_rate_room")
myform <- as.formula("perc$p95 ~ 0 + vent_type + vent_type:vent_rate_room")
myform <- as.formula("perc$p95 ~ 0 + month")
myform <- as.formula("perc$p95 ~ 0 + tod")

myform <- as.formula("perc$p95 ~ `Year of contruction / major renovation (four digit year)")

model1 <- lm(formula = myform, data=mydata)
summary(model1)
out <- tidy(model1)
plot(formula=myform, data=mydata )   # ylim=c(0,3000)
abline(model1, col="red")
abline(model1$coefficients[c(1,3)], col="blue")
abline(model1$coefficients[c(1,4)], col="green")
ggqqplot(residuals(model1))

# different regression models--------
# room type ----
mydata <- CO2_24h
myform <- as.formula("Mean ~ room0")
model_all <- lm(formula=myform, data=mydata)
summary(model_all)
plot(formula=myform, data=mydata )
ggqqplot(residuals(model_all))
#plot(model_all)

mycount <- mydata %>%
  group_by(room0) %>%
  summarise(N_count = n(),
            N2_count = n()/2,
            N3_count = length(ID_room[tod=="07-23"]),
            N4_count = length(ID_room[tod=="23-07"]),
            R_count = length(unique(ID_room)))

ggplot(mydata, aes(x = as.factor(room0), y = Mean)) +
  #geom_violin(fill = "lightblue", color = "black") +
  geom_boxplot() +  # Boxplot f?r Gruppenvergleich
  #geom_jitter(width = 0.2, alpha = 0.5) +  # Punkte f?r die individuellen Daten
  stat_summary(fun = mean, geom = "point", color = "black", size = 3, shape = 1) +  # Mittlere Werte
  #stat_summary(fun = length, geom = "text", aes(label = ..y..), vjust = 0, color = "blue") +  # Anzahl der Punkte
  geom_text(data = mycount, aes(x = as.factor(room0), y = 100, label = paste0("N=",N_count)),  # Feste y-Position (z.B. 5)
            size = 3) +
  geom_text(data = mycount, aes(x = as.factor(room0), y = -70, label = paste0("n=",R_count)),  # Feste y-Position (z.B. 5)
            size = 3) +
  #stat_count(aes(label = ..count..), geom = "text", vjust = -1.5, color = "blue") +
  geom_hline(yintercept = 2000, linetype = "dashed", color = "black") +  # Cutoff-Grenze
  annotate("text", x = 1.5, y = 2000, label = "outliers above not shown", hjust=-1, vjust = -0.5, color = "black") +  # Hinweistext
  geom_point(data = mydata[mydata$Mean > 2000, ], aes(x = as.factor(room0), y = 2000), shape = 24, fill = "red", size = 3) +  # Dreieck als abgeschnittene Werte
  theme_minimal() +
  coord_cartesian(ylim=c(0,2000)) +
  labs(x = "Measurement location",
       y = expression("Mean of CO"[2]~ "concentration"))

message("total measurement points: ", sum(mycount$N_count))
message("fraction of BED and LIV: ", sum(mycount$R_count[mycount$room0=="BED" | mycount$room0=="LIV"])/sum(mycount$R_count))



# tod: only bed OR liv ----
mydata <- CO2_bed_liv
mydata <- mydata %>%
  mutate(interaction_group = interaction(room0, tod, sep = ":"))
myform <- as.formula("Mean ~ 0 + room0 + tod:room0")
model_tod <- lm(formula=myform, data=mydata)
summary(model_tod)
plot(formula=myform, data=mydata )
ggqqplot(residuals(model_tod))

mycount <- mydata %>%
  group_by(interaction_group) %>%
  summarise(N_count = n(),
            N2_count = n()/2,
            N3_count = length(ID_room[tod=="07-23"]),
            N4_count = length(ID_room[tod=="23-07"]),
            R_count = length(unique(ID_room)))

ggplot(mydata, aes(x = interaction_group, y = Mean)) +
  #geom_violin(fill = "lightblue", color = "black") +
  geom_boxplot() +  # Boxplot f?r Gruppenvergleich
  #geom_jitter(width = 0.2, alpha = 0.5) +  # Punkte f?r die individuellen Daten
  stat_summary(fun = mean, geom = "point", color = "black", size = 3, shape = 1) +  # Mittlere Werte
  #stat_summary(fun = length, geom = "text", aes(label = ..y..), vjust = 0, color = "blue") +  # Anzahl der Punkte
  geom_text(data = mycount, aes(x = interaction_group, y = 175, label = paste0("N=",N_count)),  # Feste y-Position (z.B. 5)
            size = 3) +
  geom_text(data = mycount, aes(x = interaction_group, y = 75, label = paste0("n=",R_count)),  # Feste y-Position (z.B. 5)
            size = 3) +
  #stat_count(aes(label = ..count..), geom = "text", vjust = -1.5, color = "blue") +
  geom_hline(yintercept = 2000, linetype = "dashed", color = "black") +  # Cutoff-Grenze
  annotate("text", x = 0.5, y = 2000, label = "outliers above not shown", hjust=-1, vjust = -0.5, color = "black") +  # Hinweistext
  geom_point(data = mydata[mydata$Mean > 2000, ], aes(x = interaction_group, y = 2000), shape = 24, fill = "red", size = 3) +  # Dreieck als abgeschnittene Werte
  theme_minimal() +
  coord_cartesian(ylim=c(0,2000)) +
  labs(x = "Measurement location and time of day",
       y = expression("Mean of CO"[2]~ "concentration"))

# tod: studywise for bed AND liv ----
mydata <- CO2_bed_liv %>%
  group_by(ID_home) %>%
  filter(any(room0=="BED") & any(room0=="LIV")) %>%
  ungroup()
mydata <- mydata %>%
  group_by(ID_home,room0) %>%
  mutate(tod_diff = mean(Mean[tod=="23-07"], na.rm=TRUE) - mean(Mean[tod=="07-23"], na.rm=TRUE)) %>%
  ungroup()
mydata <- mydata %>%
  mutate(interaction_group = interaction(study, room0, sep = ":"))

myform <- as.formula("tod_diff ~ 0 + interaction_group")
model_tod <- lm(formula=myform, data=mydata)
summary(model_tod)
ggqqplot(residuals(model_tod))

# count the number of used data points N and distinct rooms n
mycount <- mydata %>%
  group_by(interaction_group) %>%
  summarise(N_count = n(),
            N2_count = n()/2,
            N3_count = length(ID_room[tod=="07-23"]),
            N4_count = length(ID_room[tod=="23-07"]),
            R_count = length(unique(ID_room)))

# Extract p-values for each interaction_group
p_values <- tidy(model_tod) %>%
  select(term, p.value) %>%
  mutate(interaction_group = str_remove(term, "interaction_group")) %>%  # Remove "interaction_group"
  #mutate(interaction_group = substr(term, 19, nchar(term))) %>%
  select(interaction_group, p.value)  # Reorder columns

ggplot(mydata, aes(x = interaction_group, y = tod_diff)) +
  #geom_violin(fill = "lightblue", color = "black") +
  geom_boxplot() +  # Boxplot f?r Gruppenvergleich
  #geom_jitter(width = 0.2, alpha = 0.5) +  # Punkte f?r die individuellen Daten
  stat_summary(fun = mean, geom = "point", color = "black", size = 3, shape = 1) +  # Mittlere Werte
  #stat_summary(fun = length, geom = "text", aes(label = ..y..), vjust = 0, color = "blue") +  # Anzahl der Punkte
  geom_text(data = mycount, aes(x = interaction_group, y = 1100, label = paste0("N=",N_count)),  # Feste y-Position (z.B. 5)
            size = 3) +
  geom_text(data = mycount, aes(x = interaction_group, y = 1300, label = paste0("n=",R_count)),  # Feste y-Position (z.B. 5)
            size = 3) +
  geom_text(data = p_values, aes(x = interaction_group, y = 900, label = paste0("p=",signif(p.value,3))),  # Feste y-Position (z.B. 5)
            size = 3) +
  #geom_hline(yintercept = 1000, linetype = "dashed", color = "black") +  # Cutoff-Grenze
  #annotate("text", x = -1, y = 0, label = "outliers >1000 ppm not shown", hjust=-1, vjust = -0.5, color = "black") +  # Hinweistext
  #geom_point(data = mydata[mydata$tod_diff > 1000, ], aes(x = interaction_group, y = 1000), shape = 24, fill = "red", size = 3) +  # Dreieck als abgeschnittene Werte
  theme_minimal() +
  coord_cartesian(ylim=c(0,1000)) +
  labs(x = "Study and measurement location",
       y = expression("Night-Day difference of mean CO"[2]~ "concentration")) +
  theme(axis.title.x = element_text(hjust=0.25)) +
  coord_flip()

# tod: studywise for bed and liv and vent type----
mydata <- CO2_bed_liv %>%
  group_by(ID_home) %>%
  filter(any(room0=="BED") & any(room0=="LIV")) %>%
  ungroup()
mydata <- mydata %>%
  group_by(ID_home,room0) %>%
  mutate(tod_diff = mean(Mean[tod=="23-07"], na.rm=TRUE) - mean(Mean[tod=="07-23"], na.rm=TRUE)) %>%
  ungroup()
mydata <- mydata %>%
  mutate(interaction_group = interaction(study, room0, vent_type, sep = ":"))

myform <- as.formula("tod_diff ~ 0 + interaction_group")
model_tod <- lm(formula=myform, data=mydata)
summary(model_tod)

ggplot(mydata, aes(x = interaction_group, y = tod_diff)) +
  geom_boxplot() + # Boxplot f?r Gruppenvergleich
  coord_flip()


# monthly variation: only bed at night---------------------
# # include "all" months as first level in month catergory so it becomes intercept
# mydata <- stats %>%
#   filter(variable=="CO2" & room0=="BED" & year!="all" & tod=="23-07")
# myform <- as.formula("Mean ~  month")
# mymodel <- lm(formula=myform, data=mydata)
# summary(mymodel)
# plot(formula=myform, data=mydata)
# ggqqplot(residuals(mymodel))

mydata <- stats %>%
  filter(variable=="CO2" & room0=="BED" & year!="all" & month!="all" & tod=="23-07") %>%
  #mutate(Mean_diff = Mean - mean(Mean[month=="8"], na.rm=TRUE))
  mutate(Mean_diff = Mean - mean(Mean, na.rm=TRUE))
# alternative: try without certain study
mydata <- stats %>%
  filter(study!="IECH" & variable=="CO2" & room0=="BED" & year!="all" & tod=="23-07") %>%
  mutate(Mean_diff = Mean - mean(Mean, na.rm=TRUE))

myform <- as.formula("Mean_diff ~ 0 +  month")
mymodel <- lm(formula=myform, data=mydata)
summary(mymodel)
#ggqqplot(residuals(mymodel))

# count the number of used data points N and distinct rooms n
mycount <- mydata %>%
  group_by(month) %>%
  summarise(N_count = n(),
            R_count = length(unique(ID_room)))

# Extract p-values for each interaction_group
myres <- tidy(mymodel, conf.int = TRUE) %>%
  #select(term, estimate, p.value) %>%
  mutate(interaction_group = str_remove(term, "month"))  # Remove "interaction_group"
  
ggplot(mydata, aes(x = month, y = Mean_diff)) +
  geom_boxplot() +  # Boxplot f?r Gruppenvergleich
  stat_summary(fun = mean, geom = "point", color = "black", size = 3, shape = 1) +  # Mittlere Werte
  geom_text(data = mycount, aes(x = month, y = -800, label = paste0("N=",N_count)), size = 3) +
  geom_text(data = mycount, aes(x = month, y = -900, label = paste0("n=",R_count)), size = 3) +
  geom_text(data = myres, aes(x = interaction_group, y = -700, label = paste0("p=",signif(p.value,3))), size = 3) +
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black") +  # Cutoff-Grenze
  annotate("text", x = 1 , y = 1000, label = "outliers >1000 ppm not shown", hjust=-1, vjust = -0.5, color = "black") +  # Hinweistext
  geom_point(data = mydata[mydata$Mean_diff > 1000, ], aes(x = month, y = 1000), shape = 24, fill = "red", size = 3) +  # Dreieck als abgeschnittene Werte
  theme_minimal() +
  coord_cartesian(ylim=c(-1000,1000)) +
  labs(x = "Month",
       y = expression("Difference in mean CO"[2]~ "concentration (monthly - yearly)"))
  #theme(axis.title.x = element_text(hjust=0))

ggplot(myres, aes(x = as.numeric(interaction_group), y = estimate)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0.2) +
  theme_minimal() +
  coord_cartesian(ylim=c(-1000,1000)) +
  labs(x = "Month",
       y = expression("Monthly - Yearly difference in CO"[2]~ "concentration"))


# monthly variation: only bed at night, studywise ---------------------

mydata <- stats %>%
  filter(variable=="CO2" & room0=="BED" & year!="all" & month!="all" & tod=="23-07") %>%
  group_by(study) %>%
  mutate(Mean_diff = Mean - mean(Mean, na.rm=TRUE)) %>%
  ungroup()
# alternative: calculate the diff same as before with overall average as reference
mydata <- stats %>%
  filter(variable=="CO2" & room0=="BED" & year!="all" & month!="all" & tod=="23-07") %>%
  mutate(Mean_diff = Mean - mean(Mean, na.rm=TRUE))

mydata <- mydata %>%
  mutate(interaction_group = interaction(month, study, sep = ":"))

myform <- as.formula("Mean_diff ~ 0 + interaction_group")
mymodel <- lm(formula=myform, data=mydata)
summary(mymodel)

# count the number of used data points N and distinct rooms n
mycount <- mydata %>%
  group_by(interaction_group) %>%
  summarise(N_count = n(),
            R_count = length(unique(ID_room)))

# Extract p-values for each interaction_group
myres <- tidy(mymodel, conf.int = TRUE) %>%
  #select(term, estimate, p.value) %>%
  mutate(interaction_group = str_remove(term, "interaction_group")) %>%  # Remove "interaction_group"
  mutate(int_gr_temp = interaction_group) %>%
  separate(int_gr_temp, into = c("month","study"), sep = ":", convert = TRUE)

ggplot(myres, aes(x = as.numeric(month), y = estimate, color = study, group = study)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0.2) +
  theme_minimal() +
  coord_cartesian(ylim=c(-1000,1000)) +
  labs(x = "Month",
       y = expression("Monthly - Yearly difference in CO"[2]~ "concentration"))

ggplot(myres, aes(x = factor(month), y = estimate)) +
  geom_boxplot() +
  facet_wrap(~ study, ncol = 2, scales = "free_y") +  # Stack vertically
  theme_minimal() +
  #coord_cartesian(ylim=c(-1000,1000)) +
  labs(x = "Month",
       y = expression("Monthly - Yearly difference in CO"[2]~ "concentration"))


# study: only bed at night
mydata <- CO2_bed
myform <- as.formula("Mean ~ study")
#contrasts(mydata$study) <- contr.sum(nlevels(mydata$study))
model_study <- lm(formula=myform, data=mydata)
summary(model_study)
plot(formula=myform, data=mydata)
ggqqplot(residuals(model_study))

mydata <- CO2_bed_ag
myform <- as.formula("Mean ~ study")
model_study <- lm(formula=myform, data=mydata)
summary(model_study)
plot(formula=myform, data=mydata)
ggqqplot(residuals(model_study))

# study + month: only bed at night
mydata <- CO2_bed[CO2_bed$study!="IECH",]
myform <- as.formula("Mean ~ month:study")
#contrasts(mydata$study) <- contr.sum(nlevels(mydata$study))
model_stu_mon <- lm(formula=myform, data=mydata)
summary(model_stu_mon)
plot(formula=myform, data=mydata)
ggqqplot(residuals(model_stu_mon))

# vent_type: only bed at night
mydata <- CO2_bed_win_ag
#myform <- as.formula("perc$X98 ~ vent_type")
myform <- as.formula("Mean~ vent_type")
model_vent_type <- lm(formula=myform, data=mydata)
summary(model_vent_type)
plot(formula=myform, data=mydata)
ggqqplot(residuals(model_vent_type))

# vent_rate:
mydata <- CO2_bed_win_ag
myform <- as.formula("Mean~ vent_type")
model_vent_type <- lm(formula=myform, data=mydata)
summary(model_vent_type)
plot(formula=myform, data=mydata)
ggqqplot(residuals(model_vent_type))



# ANOVA
oneway.test(Mean ~ study, data = CO2_bed_win, var.equal = FALSE)
#pairwise.t.test(Mean ~ study, data = CO2_bed_win)

# mixed effect model
#tbd


## Step xx: Ambient Temp analysis-------------

T_out <- stats %>%
  filter(variable == 'T',  room == 'AMB') %>%
  group_by(date, ID_h) %>% 
  summarise(avg_T = mean(Mean), study = unique(study))

T_out <- stats %>%
  filter(variable == 'T',  room == 'AMB') %>%
  group_by(user, study, home, year, month, tod) %>% 
  summarise(avg_T = mean(Mean), study = unique(study), .groups = "drop")

T_out2 <- stats %>%
  filter(variable == 'T',  room == 'AMB') %>%
  group_by(user, study, home, year, month, tod) %>% 
  mutate(avg_T = mean(Mean), study = unique(study)) %>%
  ungroup()

T_out3 <- stats %>%
  filter(variable == 'T',  room == 'AMB') %>%
  group_by(user, study, home, year, month, tod)




## corr analysis
cor




## Timm section -------------------------

# filter ambient T from annex excel sheets
T_out <- stats %>%
  filter(variable == 'T',  room == 'AMB') %>%
  group_by(date, ID_h) %>% 
  summarise(avg_T = mean(Mean), study = unique(study))

# omit all outside temps for date == NA (aggregated)
T_out <- na.omit(T_out)

T_out <- T_out %>%
  ungroup()

# load in external ambient T from wonderground
T_out_extern <- read.csv("../data/T_out.csv")
T_out_extern <- T_out_extern %>%
  filter(Study != 'Lueftung3')

# load in external ambient T from ZAMG for Lueftung3
T_out_Lueftung3 <- read.csv('../data/Lueftung3_TAMB_201010_201312.csv')
# Convert 'time' to datetime and extract year-month
T_out_Lueftung3 <- T_out_Lueftung3 %>%
  mutate(
    time = gsub("\\+00:00", "", time),
    time = gsub("T", " ", time),
    time = ymd_hm(time),
    year = format(time, "%Y"),
    month = format(time, "%m")
  )

# read in station city mapping
station_ID_Synop_Lueftung3 <- read.csv('../data/Lueftung3_Stationen_ZAMG.txt', sep = ':')

T_out_Lueftung3 <- T_out_Lueftung3 %>%
  left_join()


station_city_mapping <- station_ID_Synop_Lueftung3 %>%
  group_by(station) %>%
  summarize(City = paste(City, collapse = "; "))

# Group by station and Date, then calculate the mean temperature
monthly_avg_lueftung<- T_out_Lueftung3 %>%
  group_by(station, Date) %>%
  summarize(avg_T = mean(T, na.rm = TRUE), .groups = "drop",
            Study = 'Lueftung3',
            Country = 'at') %>%
  left_join(station_city_mapping, by = "station")

monthly_avg_lueftung_expanded <- monthly_avg_lueftung %>%
  separate_rows(City, sep = "; ") %>%
  # Remove any leading/trailing whitespace that might have been created
  mutate(City = trimws(City))

T_out_extern <- bind_rows(T_out_extern, monthly_avg_lueftung_expanded[,-1])

# slower version with loop
# for (n in 1:nrow(T_out_extern)){
#   stdy <- T_out_extern[n, ]$Study
#   city <- T_out_extern[n, ]$City
#   date <- as.Date(paste(T_out_extern[n, ]$Date, "-01", sep = ""), format = "%Y-%m-%d")
#   avg_T <- T_out_extern[n, ]$AvgT
#   
#   tmp <- meta_h$ID[meta_h$`Location: City` == city & meta_h$study == stdy]
#   tmp <- na.omit(tmp)
#   
#   if(length(tmp) == 0)
#     print(n)
#   
#   for (i in 1:length(tmp)){
#     T_out <- T_out %>% 
#       add_row(date = date, ID_h = tmp[i], avg_T = avg_T, study = stdy)
#   }
#   
# }


## mapping external data to original T_out dataframe 
# First create the mapping between city/study and ID_h
id_mapping <- stats %>%
  select(ID=ID_home, City=`Location: City`, study)

id_mapping <- meta_h %>%
  select(ID = ID, City = `Location: City`, study) %>%
  filter(!is.na(ID))

## TODO: Put result into a function
# Create all combinations at once
result <- T_out_extern %>%
  # Convert date once for all rows
  mutate(date = as.Date(paste(Date, "-01", sep = ""), format = "%Y-%m-%d")) %>%
  # Join with the ID mapping to get all relevant IDs for each city/study combo
  left_join(id_mapping, 
            by = c("City" = "City", "Study" = "study"),
            relationship = "many-to-many") %>%
  # Select only the columns we need for the final output
  select(date, ID_h = ID, avg_T = AvgT, study = Study) %>%
  # Remove any rows where no ID was found
  filter(!is.na(ID_h))

# Combine with existing T_out data
T_out <- bind_rows(T_out, result)

missing_matches <- T_out_extern %>%
  anti_join(id_mapping, by = c("City" = "City", "Study" = "study")) %>%
  rownames()

print(missing_matches)


## calculating mean temperature during the night
# Filter data for nighttime hours (23:00 to 07:00)
monthly_avg_lueftung_nighttime <- T_out_Lueftung3 %>%
  # Extract the hour from the Date-Time column (assuming it is in POSIXct or POSIXlt format)
  mutate(Hour = format(time, "%h")) %>%
  # Keep only rows where Hour is between 23 and 7
  filter(Hour >= 23 | Hour <= 7) %>%
  group_by(station, Date) %>%
  summarize(
    AvgT = mean(T, na.rm = TRUE),
    .groups = "drop",
    Study = 'Lueftung3',
    Country = 'at'
  ) %>%
  left_join(station_city_mapping, by = "station")

# Expand cities if multiple cities are listed per station
monthly_avg_lueftung_nighttime_expanded <- monthly_avg_lueftung_nighttime %>%
  separate_rows(City, sep = "; ") %>%
  mutate(City = trimws(City))  # Remove any extra whitespace

# Create all combinations at once
result <- monthly_avg_lueftung_nighttime_expanded %>%
  # Convert date once for all rows
  mutate(date = as.Date(paste(Date, "-01", sep = ""), format = "%Y-%m-%d")) %>%
  # Join with the ID mapping to get all relevant IDs for each city/study combo
  left_join(id_mapping, 
            by = c("City" = "City", "Study" = "study"),
            relationship = "many-to-many") %>%
  # Select only the columns we need for the final output
  select(date, ID_h = ID, avg_T = AvgT, study = Study) %>%
  # Remove any rows where no ID was found
  filter(!is.na(ID_h))

##Temperature dataframe with averaged values for Lueftung3 during the night
T_out_nighttime <- data.frame(result)


## create T_out_minT
T_out_minT <- read.csv("T_out_MinT.csv")

# Create all combinations at once
result <- T_out_minT %>%
  # Convert date once for all rows
  mutate(date = as.Date(paste(Date, "-01", sep = ""), format = "%Y-%m-%d")) %>%
  # Join with the ID mapping to get all relevant IDs for each city/study combo
  left_join(id_mapping, 
            by = c("City" = "City", "Study" = "study"),
            relationship = "many-to-many") %>%
  # Select only the columns we need for the final output
  select(date, ID_h = ID, min_T_max = MinTMax, min_T_avg = MinTAvg, min_T_min = MinTMin, study = Study) %>%
  # Remove any rows where no ID was found
  filter(!is.na(ID_h))

# Temperature dataframe from wonderground with minimum temperatures (Max, Avg, Min)
T_out_minT <- data.frame(result)




which(is.na(sta_fil3$interval_Max))
# drop faulty data, which give error in cdf2per (don't know why or where it comes from)
sta_fil3 <- sta_fil3 %>% drop_na(interval_Max)
# over all ID_h and date to get one entry per home per date 
sta_fil3_ag_AMBT <- sta_fil3 %>%
  group_by(ID_h, date) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~weighted.mean(.x, N_rel, na.rm=TRUE)),
            across(study),
            across(vent_type),
            across(home_size, ~as.numeric(.x)),
            across(room_vol),
            across(room_area),
            across(country),
            N_count = 1,
            across(vent_rate_home:vent_rate_room),
            cdfs=mix_cdf_CO2(pick(p00:p100),N_rel,cdf = FALSE),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins)) %>%
  distinct(.keep_all = TRUE)

print(paste0('There are ', nrow(subset(sta_fil3_ag_AMBT, is.na(vent_type))), ' homes with missing ventilation type'))

# match the amb t to the ID_h's, however a bit messy, as the amb t in Lodenareal gets the ID_h 'General'
T_out_Loden <- T_out[grepl('Lodenareal', T_out$ID_h), ]
Loden_df <- merge(sta_fil3_ag_AMBT, T_out_Loden, by = c('date', 'study'))
merged_df <- merge(sta_fil3_ag_AMBT, T_out, by = c('date', 'ID_h'))

cols_to_add <- setdiff(names(merged_df), names(Loden_df))
Loden_df[cols_to_add] <- NA

cols_to_add <- setdiff(names(Loden_df), names(merged_df))
merged_df[cols_to_add] <- NA

Loden_df <- Loden_df[, names(merged_df)]
final_df <- rbind(Loden_df, merged_df)

# final_df cosmetics (add ID_h and study columns together)
# Combine the ID_h and study columns, removing NA values
final_df$ID_h_combined <- ifelse(is.na(final_df$ID_h), final_df$ID_h.x, 
                                 ifelse(is.na(final_df$ID_h.x), final_df$ID_h, 
                                        paste0(final_df$ID_h, final_df$ID_h.x)))

final_df$study_combined <- ifelse(is.na(final_df$study), final_df$study.x, 
                                  ifelse(is.na(final_df$study.x), final_df$study, 
                                         paste0(final_df$study, final_df$study.x)))
# Remove the redundant columns
final_df <- final_df %>%
  select(-ID_h, -ID_h.x, -ID_h.y, -study, -study.x, -study.y)

# Rename the combined columns to original names if needed
final_df <- final_df %>%
  rename(ID_h = ID_h_combined, study = study_combined)


## mechanically vented
final_df_mech <- final_df %>%
  filter("Mechanical ventilation" == vent_type)

## window vented
final_df_wind <- final_df %>%
  filter("Window airing (not designed)" == vent_type)

## non-mechanically vented
final_df_non_mech <- final_df %>%
  filter("Window airing (not designed)" == vent_type | 
           "Natural ventilation (designed)" == vent_type |
           "Hybrid/mixed mode ventilation" == vent_type )


#trying out different fits
x <- subset(final_df, select = c(Mean, avg_T))
tmp <- data.frame(avg_T = seq(min(x$avg_T), max(x$avg_T), length.out = 201))

lm <- lm(Mean ~ avg_T, data = final_df)
fit_lm <- predict(lm, newdata = tmp)

g1 <- gamlss2(Mean ~ s(avg_T, k = 20), data = x)
fit_g1 <- predict(g1, newdata = tmp)$mu

g2 <- gamlss2(Mean ~ s(avg_T, k = 20) | s(avg_T, k = 20), data = x)
fit_g2 <- predict(g2, newdata = tmp)$mu

fit_l1 <- predict(loess(Mean ~ avg_T, span = 0.75, data = x), newdata = tmp)
fit_l2 <- predict(loess(Mean ~ avg_T, span = 1.00, data = x), newdata = tmp)
fit_l3 <- predict(loess(Mean ~ avg_T, span = 5.00, data = x), newdata = tmp)

y <- rbind(data.frame(model = "lm", avg_T = tmp, mu = fit_lm),
           data.frame(model = "het gamlss2", avg_T = tmp, mu = fit_g1),
           data.frame(model = "hom gamlss2", avg_T = tmp, mu = fit_g2),
           data.frame(model = "loess 0.75", avg_T = tmp, mu = fit_l1),
           data.frame(model = "loess 1.00", avg_T = tmp, mu = fit_l2),
           data.frame(model = "loess 5.00", avg_T = tmp, mu = fit_l2))

gg1 <- ggplot() +
  geom_line(aes(x = avg_T, y = mu, group = model, color = model), data = y, linewidth = 0.75) +
  theme_minimal()
gg1

gg2 <- ggplot() +
  geom_point(aes(x = avg_T, y = Mean), data = x) +
  geom_line(aes(x = avg_T, y = mu, group = model, color = model), data = y, linewidth = 0.75) +
  theme_minimal()

gg2

# plot scatterplot
ggplot(data = final_df, aes(x = avg_T, y = Mean, color = factor(study))) +
  geom_point(size = 1) +          
  geom_smooth(aes(group = 1),method = "loess", se = TRUE) +
  labs(title = expression("Ambient T versus vs CO"[2]*" per home"),
       x = "Outside Temperature [°C]",
       y = expression("CO"[2]*" concentration [ppm]")) +     
  theme_minimal()

print(paste0('There are ', sum(final_df$N_count), ' different combinations of dates and homes with outside temperatures'))

# plot scatterplot (mechanically vented)
ggplot(data = final_df_mech, aes(x = avg_T, y = Mean, color = factor(study))) +
  geom_point(size = 1) +          
  geom_smooth(aes(group = 1),method = "loess", se = TRUE) +
  labs(title = expression("Ambient T versus vs mechanically vented CO"[2]*" per home"),
       x = "Outside Temperature [°C]",
       y = expression("CO"[2]*" concentration [ppm]")) +     
  theme_minimal()

print(paste0('There are ', sum(final_df_mech$N_count), ' different combinations of dates and homes with outside temperatures for mechanically ventilated homes'))


# plot scatterplot (window vented)
ggplot(data = final_df_wind, aes(x = avg_T, y = Mean, color = factor(study))) +
  geom_point(size = 1) +          
  geom_smooth(aes(group = 1),method = "loess", se = TRUE) +
  labs(title = expression("Ambient T versus vs window vented CO"[2]*" per home"),
       x = "Outside Temperature [°C]",
       y = expression("CO"[2]*" concentration [ppm]")) +     
  theme_minimal()

print(paste0('There are ', sum(final_df_wind$N_count), ' different combinations of dates and homes with outside temperatures for window ventilated homes'))


# plot scatterplot (non-mechanically vented)
ggplot(data = final_df_non_mech, aes(x = avg_T, y = Mean, color = factor(study))) +
  geom_point(size = 1) +          
  geom_smooth(aes(group = 1),method = "loess", se = TRUE) +
  labs(title = expression("Ambient T versus vs non-mechanically vented CO"[2]*" per home"),
       x = "Outside Temperature [°C]",
       y = expression("CO"[2]*" concentration [ppm]")) +     
  theme_minimal()

print(paste0('There are ', sum(final_df_mech$N_count), ' different combinations of dates and homes with outside temperatures for non-mechanically ventilated homes'))



# ambient T categories

final_df <- final_df %>%
  mutate(temp_category = case_when(
    avg_T < 0 ~ "< 0",
    avg_T < 5 ~ "0-5",
    avg_T < 10 ~ "5-10",
    avg_T < 15 ~ "10-15",
    avg_T < 20 ~ "15-20",
    avg_T < 25 ~ "20-25",
    avg_T < 30 ~ "25-30",
    avg_T > 30 ~ "> 30"
  ))

# create pdfs for each temp category
final_df_cat <- final_df %>%
  group_by(temp_category) %>%
  summarise(across(year:month, ~min(.x)),
            #across(tod:variable, ~first_and_check(.x)),
            across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
            across(quality_start, ~min(.x)),
            across(quality_end, ~max(.x)),
            across(interval_Median, ~mean_and_check(.x)),
            across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
            across(Mean, ~mean(.x, na.rm=TRUE)),
            nr_yrs=n_distinct(year),
            cdfs=mix_cdf_CO2(pick(cdfs), cdf = TRUE)/100,
            pdfs=data.frame(t(diff(t(cdfs)))),
            percs=cdf2per(cdfs,CO2_bins,Perc_bins))

correct_order <- c("< 0", "0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "> 30")

# Use mutate() to convert temp_cat to a factor with the specified order
final_df_cat_ordered <- final_df_cat %>%
  mutate(temp_category = factor(temp_category, levels = correct_order, ordered = TRUE)) %>%
  arrange(temp_category)

max_y <- list()
for (i in 1:nrow(final_df_cat_ordered)){
  max_y[i] = max(smooth.spline(CO2_midbins, as.numeric(unlist(final_df_cat_ordered[i, ]$pdfs)), spar = 0.4)$y)
}

# TODO: add way to account for different number of categories

# plot ecdfs

colors = c( "red", "blue", "green", 'yellow', 'orange', 'purple', 'cyan', 'magenta' )

plot(CO2_bins, final_df_cat_ordered[1, ]$cdfs, type="l", main = paste0('ecdfs for temp [°C] categories'), col = colors[1], xlab = expression('CO'[2]*' concentration [ppm]'), ylab = 'Cumulative probability [-]')
lines(CO2_bins, final_df_cat_ordered[2, ]$cdfs, col = colors[2])
lines(CO2_bins, final_df_cat_ordered[3, ]$cdfs, col = colors[3])
lines(CO2_bins, final_df_cat_ordered[4, ]$cdfs, col = colors[4])
lines(CO2_bins, final_df_cat_ordered[5, ]$cdfs, col = colors[5])
lines(CO2_bins, final_df_cat_ordered[6, ]$cdfs, col = colors[6])
lines(CO2_bins, final_df_cat_ordered[7, ]$cdfs, col = colors[7])
lines(CO2_bins, final_df_cat_ordered[8, ]$cdfs, col = colors[8])
legend("bottomright", legend = correct_order, col = colors, lwd = 2)
grid()

# plot epdfs
plot(smooth.spline(CO2_midbins, as.numeric(unlist(final_df_cat_ordered[1, ]$pdfs)), spar = 0.4), type="l", ylim = c(0, max(unlist(max_y))), main = paste0('epdfs for temp [°C] categories'), col = colors[1], xlab = expression('CO'[2]*' concentration [ppm]'), ylab = 'Probability density')
lines(smooth.spline(CO2_midbins, as.numeric(unlist(final_df_cat_ordered[2, ]$pdfs)), spar = 0.4), col = colors[2])
lines(smooth.spline(CO2_midbins, as.numeric(unlist(final_df_cat_ordered[3, ]$pdfs)), spar = 0.4), col = colors[3])
lines(smooth.spline(CO2_midbins, as.numeric(unlist(final_df_cat_ordered[4, ]$pdfs)), spar = 0.4), col = colors[4])
lines(smooth.spline(CO2_midbins, as.numeric(unlist(final_df_cat_ordered[5, ]$pdfs)), spar = 0.4), col = colors[5])
lines(smooth.spline(CO2_midbins, as.numeric(unlist(final_df_cat_ordered[6, ]$pdfs)), spar = 0.4), col = colors[6])
lines(smooth.spline(CO2_midbins, as.numeric(unlist(final_df_cat_ordered[7, ]$pdfs)), spar = 0.4), col = colors[7])
lines(smooth.spline(CO2_midbins, as.numeric(unlist(final_df_cat_ordered[8, ]$pdfs)), spar = 0.4), col = colors[8])
legend("topright", legend = correct_order, col = colors, lwd = 2)
grid()


