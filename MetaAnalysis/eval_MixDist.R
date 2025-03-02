#' functions for aggregating percentiles data / mixture distributions

# percentiles to ecdf conversion
per2cdf <-  function(per,per_bin,cdf_bin){
  # if (is.vector(per)) {per <- matrix(per, nrow=1)}
  # else {per <- as.matrix(per)}
  approx_cdf <- function(x) {approx(x,per_bin,cdf_bin,ties="ordered",rule=2)$y}
  cdf <- t(apply(X=per, MARGIN=1, FUN=approx_cdf))
  return(data.frame(cdf))
}

# ecdf to percentile conversion
cdf2per <-  function(cdf,cdf_bin,per_bin){
  # if (is.vector(cdf)) {cdf <- matrix(cdf, nrow=1)}
  # else {cdf <- as.matrix(cdf)}
  approx_per <- function(x) {approx(x,cdf_bin,per_bin,ties="ordered",rule=2)$y}
  per <- t(apply(X=cdf, MARGIN=1, FUN=approx_per))
  return(data.frame(per))
}


# calculating mixture distributions
mix_cdf <- function(df,per_bin,cdf_bin,wei,cdf = FALSE) {
  if(cdf == FALSE){
    if(missing(wei)) {
      #unweighted
      cdf_mix <- colMeans(per2cdf(df,per_bin,cdf_bin), na.rm=TRUE)
    } else {
      #weighted
      cdf_mix <- apply(per2cdf(df,per_bin,cdf_bin),MARGIN=2,FUN=weighted.mean,w=wei)
    }
    return(data.frame(t(cdf_mix)))
  }
  if(cdf == TRUE){
    if(missing(wei)) {
      #unweighted
      cdf_mix <- colMeans(df, na.rm=TRUE)
    } else {
      #weighted
      cdf_mix <- apply(df ,MARGIN=2,FUN=weighted.mean,w=wei)
    }
    return(data.frame(t(cdf_mix)))
  }
}
# CO2 calculating mixture distrubtions 
mix_cdf_CO2 <- function(df,wei,cdf = FALSE) {
  if(cdf == FALSE){
    if(missing(wei)) {
      #unweighted
      cdf_mix <- colMeans(per2cdf(df,Perc_bins,CO2_bins), na.rm=TRUE)
    } else {
      #weighted
      cdf_mix <- apply(per2cdf(df,Perc_bins,CO2_bins),MARGIN=2,FUN=weighted.mean,w=wei)
    }
    return(data.frame(t(cdf_mix)))
  }
  if(cdf == TRUE){
    if(missing(wei)) {
      #unweighted
      cdf_mix <- colMeans(df, na.rm=TRUE)
    } else {
      #weighted
      cdf_mix <- apply(df ,MARGIN=2,FUN=weighted.mean,w=wei)
    }
    return(data.frame(t(cdf_mix)))
  }
}

# check if all entries are same
first_and_check <- function(x) {
  if (length(unique(x))>1) {
    message("Warning: more than one entry in variables that should be the same in the room! Returned as NA")
    return(NA)}
  else {
    return(first(x))}
}

# check if same and calculate mean (for sample interval)
mean_and_check <- function(x) {
  if (length(unique(x))>1) {
    message("Warning: different sample intervals have been aggregated!")}
  return(mean(x))
}

# check if same and calculate median (for median sample interval)
median_and_check <- function(x) {
  if (length(unique(x))>1) {
    message("Warning: different sample intervals have been aggregated!")}
  return(median(x))
}

# old: to be discontinued; check if same and calculate mean (for sample interval)
mean_and_check <- function(x) {
  if (length(unique(x))>1) {
    message("Warning: different sample intervals have been aggregated!")}
  return(mean(x))
}


# calculate data abundance (N_rel = fraction of month with data) for weighting
data_abu <- function(N,NAs,year,month,tod,int) {
  nr_days <- as.numeric(lubridate::days_in_month(as.Date(paste(year, month,"01", sep = "-"), format = "%Y-%m-%d")))
  #nr_hours <- switch(tod, '23-07'=8, '07-23'=16, 'all'=24)
  nr_hours <- as.numeric(ifelse(tod=='23-07', 8, ifelse(tod=='07-23', 16, ifelse(tod=='all', 24, 0))))
  nr_datapts_per_hour <- 3600/int 
  return((N-NAs)/(nr_days*nr_hours*nr_datapts_per_hour))
}

# function to remove data points / where vent_type is undefined
DelNAVentType <- function(sta){
  idxNoVent <- !sta$vent_type %in% c("Window airing (not designed)", "Natural ventilation (designed)", "Hybrid/mixed mode ventilation", "Mechanical ventilation")
  message("There were ", sum(idxNoVent, na.rm=TRUE), " data points (rooms) where ventilation type was NOT defined. -> Now deleted")
  if (any(grepl('ID_r', names(sta))))
    print(paste0("See: ", unique(sta$ID_r[idxNoVent])))
  if (any(grepl('ID_h', names(sta))))
    print(paste0("See: ", unique(sta$ID_h[idxNoVent])))
  return(sta[!idxNoVent,])
}


