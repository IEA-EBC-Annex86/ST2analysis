library("annex")

raw_df <- read.csv("C:/Users/Timm/Desktop/Atmospheric Sciences/Arbeit/Data(selfmade)/ESP/ClimateReady/PrePro/ClimateReady_data.txt")
config <- read.table("C:/Users/Timm/Desktop/Atmospheric Sciences/Arbeit/Data(selfmade)/ESP/ClimateReady/PrePro/ClimateReady_data_config.txt",
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

annex_df <- annex(RH + T + CO2 + PM25 ~ datetime | study + home + room,
                  data = prepared_df, tz = "Europe/Berlin")
                  
stats <- annex_stats(annex_df, format = "long")

annex_write_stats(stats, file = "Climate_Ready_10Min.xlsx", user = 0005) 
