# R --vanilla < LBL_D1T050_annex.R
print("-> LBL_D1T050")

library("annex")

dir_cfg <- c("../tmp/D1T050/cfg-annex")
dir_dat <- c("../tmp/D1T050/IAQ_Activity_Monitoring")
dir_out <- c("../tmp/D1T050/tmp/")
dir.create(dir_out, recursive=TRUE)

fns <- list.files(path=dir_cfg)

for (i in 1:length(fns)) {
    
    name = unlist(strsplit(fns[i], split=".cfg"))[1]
    
    print(name)
    
    fn_dat <- paste(dir_dat, "/", name, ".csv", sep="")
    fn_cfg <- paste(dir_cfg, "/", fns[i], sep="")
    
    raw_df <- read.csv(fn_dat)
    config <- read.csv(fn_cfg, na.strings = c("NA", "empty"))

    config <- annex_check_config(config)

    raw_df <- transform(raw_df, Time = as.POSIXct(Time, tz="UTC", format="%m/%d/%y %H:%M"))

    prepared_df <- annex_prepare(raw_df, config, quiet = TRUE)
    
    annex_df <- annex(
        CO2 + PM25 + PM10 + RH + T ~ datetime | study + home + room,
        data = prepared_df, tz = "America/Los_Angeles"
    )
                      
    stats <- annex_stats(annex_df, format = "long")

    annex_write_stats(stats, file=paste(dir_out, name, ".xlsx", sep=""), user=1234)
}
