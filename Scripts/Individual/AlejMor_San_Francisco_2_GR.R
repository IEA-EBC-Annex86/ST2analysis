## code by Alejandro Moreno for Annex 86
## adapted Gabriel Rojas May 2024

#read library
library("annex")
library("readxl")
library("lubridate") #needed for time zone conversion

#setwd("C:/Users/alexa/OneDrive - University of Strathclyde/Annex_86/Data_analysis/For_analysis")
setwd(".")


## read data
raw_df_SF <- as.data.frame(read_excel("../data/Div_AlejandroMoreno/SanFrancisco.xlsx", sheet = 2))
raw_df_SC <- as.data.frame(read_excel("../data/Div_AlejandroMoreno/Scotland.xlsx", sheet = 2))

# check time zone: read_excel always in UTC?? is there a way to directly define tz? Tbd: ask Reto, had same issue
attr(raw_df_SF$Date, "tzone")
attr(raw_df_SC$Date, "tzone")

# convert time zone
raw_df_SF$Date <- force_tz(raw_df_SF$Date,"US/Central") # should it not be America/Los_Angeles?
attr(raw_df_SF$Date, "tzone")
raw_df_SC$Date <- force_tz(raw_df_SC$Date,"Europe/London")
attr(raw_df_SC$Date, "tzone")
# alternativ way according to https://stackoverflow.com/questions/15575713/modifying-timezone-of-a-posixct-object-without-changing-the-display
# didn't work for me
# raw_df_SF$Date <- as.POSIXct(as.numeric(raw_df_SF$Date), origin=as.POSIXct("1970-01-01",tz="US/Central"),tz="US/Central")

# check for -99
sum(raw_df_SF==-99,na.rm=TRUE) 
sum(raw_df_SC==-99,na.rm=TRUE) 

# loop over all columns and convert all -99 to NA (and define as numeric if reqd)
for(i in 2:ncol(raw_df_SF)) { 
  raw_df_SF[,i] <- as.numeric(raw_df_SF[,i])
  idx2excl <- raw_df_SF[,i]==-99
  print(sum(idx2excl,na.rm=TRUE)) 
  raw_df_SF[,i][idx2excl] <- NA
}

# check if all -99 removed
sum(raw_df_SF==-99,na.rm=TRUE) 

plot(raw_df_SF$Date, raw_df_SF$"PM L-SF-PH", type="l")
plot(raw_df_SF$Date, raw_df_SF$"Ambient - PM2.5", type="l")

#redo for scotland
for(i in 2:ncol(raw_df_SC)) { 
  raw_df_SC[,i] <- as.numeric(raw_df_SC[,i])
  idx2excl <- raw_df_SC[,i]==-99
  print(sum(idx2excl,na.rm=TRUE)) 
  raw_df_SC[,i][idx2excl] <- NA
}

#annex config
config_SF <- as.data.frame(read_excel("../data/Div_AlejandroMoreno/SanFrancisco.xlsx", sheet = 1))
config_SF
config_SC <- as.data.frame(read_excel("../data/Div_AlejandroMoreno/Scotland.xlsx", sheet = 1))


#is this config valid?
annex_check_config(config_SF)
annex_check_config(config_SC)

#Prepare the data set
prepared_df_SF <- annex_prepare(raw_df_SF, config_SF)
head(prepared_df_SF)
prepared_df_SC <- annex_prepare(raw_df_SC, config_SC)

#Perform analysis
#1. Create 'annex': fucntion annex()
#2. Calculate statistics: function a

annex_df_SF <- annex(T + RH + PM25 + VOC ~ datetime | study + home + room,
                  data = prepared_df_SF,
                  tz = "US/Central")

class(annex_df_SF)
head(annex_df_SF)

stats_SF <- annex_stats(annex_df_SF)
class(stats_SF)

# do annex analysis for Scotland
annex_df_SC <- annex(T + RH + PM25 + VOC ~ datetime | study + home + room,
                     data = prepared_df_SC,
                     tz = "Europe/London")
stats_SC <- annex_stats(annex_df_SC)

# Write data into final output
annex_write_stats(stats_SF, file = "../data/Div_AlejandroMoreno/final_SF3_GRtest.xlsx", user = 25)
annex_write_stats(stats_SC, file = "../data/Div_AlejandroMoreno/final_SC3_GRtest.xlsx", user = 25)


#annex_validate
annex_validate("../data/Div_AlejandroMoreno/final_SF3_GRtest.xlsx", user = 25)
annex_validate("../data/Div_AlejandroMoreno/final_SC3_GRtest.xlsx", user = 25)

annex_variable_definition()
annex_countries()

