# Function to apply the running mean method on the model inputs 
# param: data_x_running_mean_t, dataframe to use for the runningMean 
# param: daily_window, number of days to compute the running mean 
# param: showMsg (if T shows the function messages)
# return: data.frame with runningMean output
# rdname: runningMean


runningMean <- function(data_x_running_mean_t = df, daily_window = 10 ) {
  
  unique_hour = sort(unique(data_x_running_mean_t$HOUR))
  
  data_running_mean_t = data_x_running_mean_t[1,]
  
  for (cy_hour_ref in unique_hour) {
    pos_hour = which(data_x_running_mean_t$HOUR == cy_hour_ref)
    
    data_x_running_mean = data_x_running_mean_t[pos_hour,]
    rm(pos_hour)
    # window of time 
    lunghezza_finestra = daily_window - 1# remotion of one day since the computation starts from the 1st
    data_running_mean = data_x_running_mean[1,]
    
    for ( ciclo_inizio_mm in seq(2,nrow(data_x_running_mean)) ) {
      # definition of positions to compute the running mean by making a counter in reverse
      posizioni_rm = (ciclo_inizio_mm - lunghezza_finestra):ciclo_inizio_mm
      
      pos_meno1 = which(posizioni_rm < 1)
      if ( length(pos_meno1) > 0 ) posizioni_rm = posizioni_rm[-1*pos_meno1]
      rm(pos_meno1)
      
      # cycle for computing the mean of the dataset's variables
      media_1 = data_x_running_mean[1,]
      for ( ciclo_variabili in colnames(media_1) ) {
        # dataset creation with values to use
        data_1 = data_x_running_mean[posizioni_rm,ciclo_variabili]
        if ( !is.numeric(data_1) ) {
          media_1[ciclo_variabili] = NA
          next
        }
        # NA remotion
        pos_na = which(is.na(data_1) == 1 )
        if ( length(pos_na) > 0 ) data_1 = data_1[-1*pos_na]
        rm(pos_na)
        # if there are missing values it puts NA, otherwise it computes the mean
        if ( length(data_1) == 0 ) {
          media_1[ciclo_variabili] = NA
        } else {
          media_1[ciclo_variabili] = mean(data_1)
        }
        rm(data_1)
      }
      rm(ciclo_variabili)
      media_1$TIMESTAMP_START = data_x_running_mean$TIMESTAMP_START[ciclo_inizio_mm]
      data_running_mean = rbind(data_running_mean,media_1)
      rm(media_1)
      rm(posizioni_rm)
    }
    rm(ciclo_inizio_mm)
    rm(lunghezza_finestra)
    rm(data_x_running_mean)
    data_running_mean_t = rbind(data_running_mean_t,data_running_mean)
  }
  
  data_running_mean_t = data_running_mean_t[-1,]
  
  library(lubridate)
  data_running_mean_t$YEAR = year(ymd_hm(as.character(data_running_mean_t$TIMESTAMP_START)))
  data_running_mean_t$MONTH = month(ymd_hm(as.character(data_running_mean_t$TIMESTAMP_START)))
  data_running_mean_t$DAY = day(ymd_hm(as.character(data_running_mean_t$TIMESTAMP_START)))
  data_running_mean_t$HOUR = hour(ymd_hm(as.character(data_running_mean_t$TIMESTAMP_START)))
  data_running_mean_t$MINUTE = minute(ymd_hm(as.character(data_running_mean_t$TIMESTAMP_START)))
  
  data_running_mean_t = data_running_mean_t[order(data_running_mean_t$TIMESTAMP_START),]
  
  return(data_running_mean_t)
}
