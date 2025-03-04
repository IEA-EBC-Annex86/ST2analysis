
# check data quality if below or above limits
checkQual <- function(sta,lim){
  idx <- (sta$quality_lower > lim[1] | sta$quality_upper > lim[2])
  message(sum(idx,na.rm=TRUE), " datapoints are below/above the quality limit and are removed. See:")
  print(unique(sta$ID_room[idx]))
  return(sta[!idx,])
}


# aggregate over room ID to have one row per home-room----------------
aggRoom<- function(sta, per_bin,cdf_bin){
  # tbd: add a check in case the month entries can not be converted, e.g. if a "all" is present
  # here a chatGPT proposal ---- 
    # # Check if the variable is a factor, convert it to character first
    # if (is.factor(x)) {
    #   x <- as.character(x)
    # }
    # # Try converting to numeric
    # numeric_x <- suppressWarnings(as.numeric(x))
    # # Identify non-numeric entries (NAs introduced by coercion)
    # if (any(is.na(numeric_x) & !is.na(x))) {
    #   warning("Some entries could not be converted to numeric and are set to NA!")
    # }
    #----
  sta$year <- as.numeric(sta$year)
  sta$month <- as.numeric(sta$month)
  sta_ag <- sta %>%
    { remove_obs <- filter(., !((N-NAs)>=2 & N_rel>0))
      if (nrow(remove_obs)>0) {
        message("Warning: ", nrow(remove_obs), 
              " observations were removed for aggregation, because the cdfs could not be calculated (because they did not fulfill N>=2 and N_rel>0). See: ")
        print(unique(remove_obs$ID_room)) }
      filter(., (N-NAs)>=2 & N_rel>0) 
    } %>%
    group_by(user,study,home,room,tod,variable) %>%
    summarise(across(room0, ~first_and_check(.x)),
              across(year:month, ~first(.x)),
              across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
              across(quality_start, ~min(.x)),
              across(quality_end, ~max(.x)),
              interval_Min = min(interval_Min),
              interval_Q1 = NA,
              interval_Median = median_and_check(interval_Median),
              interval_Mean = weighted.mean(interval_Mean, N_rel, na.rm=TRUE),
              interval_Q3 = NA,
              interval_Max = max(interval_Max),
              across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
              Mean = weighted.mean(Mean, N_rel, na.rm=TRUE),
              Sd = NA,
              across(ID_home:last_col(), ~first_and_check(.x)),
              cdfs = mix_cdf(perc,per_bin,cdf_bin,N_rel,cdf=FALSE),
              perc = cdf2per(cdfs,cdf_bin,per_bin),
              N_rel = sum(N_rel, na.rm=TRUE),
              .groups="drop") %>%
    #mutate(perc = cdf2per(cdfs,CO2_bins,Perc_bins)) %>%
    relocate(N_rel,.after = Sd) %>%
    relocate(cdfs,.after = perc)
  return(sta_ag)
}

# CO2aggregate over room ID to have one row per home-room----------------
aggRoomCO2<- function(sta){
  # tbd: add a check in case the month entries can not be converted, e.g. if a "all" is present
  # here a chatGPT proposal ---- 
  # # Check if the variable is a factor, convert it to character first
  # if (is.factor(x)) {
  #   x <- as.character(x)
  # }
  # # Try converting to numeric
  # numeric_x <- suppressWarnings(as.numeric(x))
  # # Identify non-numeric entries (NAs introduced by coercion)
  # if (any(is.na(numeric_x) & !is.na(x))) {
  #   warning("Some entries could not be converted to numeric and are set to NA!")
  # }
  #----
  sta$year <- as.numeric(sta$year)
  sta$month <- as.numeric(sta$month)
  sta_ag <- sta %>%
    group_by(user,study,home,room,tod,variable) %>%
    summarise(across(year:month, ~min(.x)),
              across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
              across(quality_start, ~min(.x)),
              across(quality_end, ~max(.x)),
              interval_Min = min(interval_Min),
              interval_Q1 = NA,
              interval_Median = median_and_check(interval_Median),
              interval_Mean = weighted.mean(interval_Mean, N_rel, na.rm=TRUE),
              interval_Q3 = NA,
              interval_Max = max(interval_Max),
              across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
              Mean = weighted.mean(Mean, N_rel, na.rm=TRUE),
              Sd = NA,
              across(ID_home:last_col(), ~first_and_check(.x)),
              cdfs = mix_cdf_CO2(perc,N_rel,cdf=FALSE),
              perc = cdf2per(cdfs,CO2_bins,Perc_bins),
              N_rel = sum(N_rel, na.rm=TRUE),
              .groups="drop") %>%
    #mutate(perc = cdf2per(cdfs,CO2_bins,Perc_bins)) %>%
    relocate(N_rel,.after = Sd) %>%
    relocate(cdfs,.after = perc)
  return(sta_ag)
}

# aggregate over room ID to have one row per home-room----------------
aggRoom_old <- function(sta){
  sta$year <- as.numeric(sta$year)
  sta$month <- as.numeric(sta$month)
  sta_ag <- sta %>%
    group_by(ID_room) %>%
    summarise(across(year:month, ~min(.x)),
              #across(tod:variable, ~first_and_check(.x)),
              across(quality_lower:quality_upper, ~weighted.mean(.x, N-NAs)),
              across(quality_start, ~min(.x)),
              across(quality_end, ~max(.x)),
              across(interval_Median, ~mean_and_check(.x)),
              across(Nestim:NAs, ~sum(.x, na.rm=TRUE)),
              across(Mean, ~weighted.mean(.x, N_rel, na.rm=TRUE)),
              study,
              across(vent_type),
              across(home_area, ~as.numeric(.x)),
              across(room_vol),
              across(room_area),
              across(country),
              across(city),
              across(vent_type_comment),
              N_count = 1,
              #across(vent_rate_home:vent_rate_room),
              #cdfs=mix_cdf_CO2(pick(perc$p00:p100),N_rel,cdf=FALSE),
              cdfs=mix_cdf_CO2(perc,N_rel,cdf=FALSE),
              #cdfs=mix_cdf_CO2(pick(matches("^p\\d+(\\.\\d+)?$")),N_rel,cdf=FALSE) ) %>%
              #cdfs = pick(matches("^p\\d+(\\.\\d+)?$"))) %>%
              percs=cdf2per(cdfs,CO2_bins,Perc_bins),
              .groups="drop") %>%
  distinct(.keep_all = TRUE) 
  return(sta_ag)
}

# aggregate over home IDs: shouldn't be needed-----------
aggHome <- function(sta){
  sta_ag <- sta %>%
    group_by(ID_h) %>%
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
              across(vent_type_comment),
              N_count = 1,
              across(vent_rate_home:vent_rate_room),
              cdfs=mix_cdf_CO2(pick(p00:p100),N_rel,cdf = FALSE),
              percs=cdf2per(cdfs,CO2_bins,Perc_bins)) %>%
    distinct(.keep_all = TRUE)
}