# Function to create daily dataset from half hourly data
# param: df, dataframe with subdaily values
# param: nrWindow, number of positions before and after the reference time to compute the average
# param: hourReferenceT, reference time for the dailyDownscaling function
# param: approccio, dailyDownscaling function approach
# 4 types of approaches:
#      1: 'noon' approach computes an average of conditions around midday (hour_reference_t +/- nr_window )
#      2: 'daily' approach computes an average of average daytime conditions
#      3: computes an average of daytime conditions around the point of maximum radiation (using SW_IN_POT)
#      4: computes an average of averaged values from the first approach (1)
# to be noted: 
#   for the '3 hours' approach, it is necessary to run the dailyDownscaling function as it is 
#   (i.e. default command to digit in the 'main script': dataDaily = dailyDownscaling(df = dataHhReduc) 

# param: showMsg (if T shows the function messages)
# return: a data.frame with 'downscaled' data
# rdname: dailyDownscaling

dailyDownscaling <- function(df = dfIn,nrWindow = 1,
                             hourReferenceT = c(10,12,15),approccio = 1) {
 
  # source('headerControl.R')

  colMandatory = c('YEAR','MONTH','DAY','HOUR','MINUTE','NIGHT','TIMESTAMP_START')
  if (approccio == 3)
    colMandatory = c(colMandatory,"SWINPOT")

  if ( headerControl(df = df,colMandatory = colMandatory) == 0)
    stop(headerControl(df = df,colMandatory = colMandatory,showMsg = T))

  dfDayT = df[1,]    

 
  for (cicloAnno in sort(unique(df$YEAR))) {
    for (cicloMesi in seq(1,12)) {
      for (cicloGiorni in seq(1,31)) {
        
        posDay = which(df$YEAR == cicloAnno & df$MONTH == cicloMesi &
                        df$DAY == cicloGiorni)
        if (length(posDay) == 0) next
       
        for (hourReference in hourReferenceT) {
          # approccio 1
          if (approccio == 1 || approccio == 4 ) {
            posReference = which(df$YEAR == cicloAnno & df$MONTH == cicloMesi & df$DAY == cicloGiorni &
                                  df$NIGHT == 0 & df$HOUR == hourReference & df$MINUTE == 0)
            if (length(posReference) == 0) next
            windowDay = seq(posReference - nrWindow,posReference + nrWindow)
          }
          # approccio 2
          if (approccio == 2) {
            posReference = which(df$YEAR == cicloAnno & df$MONTH == cicloMesi & df$DAY == cicloGiorni &
                                    df$NIGHT == 0 & df$HOUR == 12 & df$MINUTE == 0)
            windowDay = which(df$YEAR == cicloAnno & df$MONTH == cicloMesi & df$DAY == cicloGiorni &
                                 df$NIGHT == 0)
          }
          # approccio 3
          if (approccio == 3) {
            posReferenceMax = which(df$YEAR == cicloAnno & df$MONTH == cicloMesi & df$DAY == cicloGiorni &
                                    df$NIGHT == 0 & is.na(df$SWINPOT) == 0)
            if ( length(posReferenceMax) > 0 ) {
              
              posMaxSWINPOT = which(df$SWINPOT[posReferenceMax] == max(df$SWINPOT[posReferenceMax],na.rm = T)[1])
              
              posReference = posReferenceMax[posMaxSWINPOT[1]]
              windowDay = seq(posReference - nrWindow,posReference + nrWindow)
            } else {
              posReference = which(df$YEAR == cicloAnno & df$MONTH == cicloMesi & df$DAY == cicloGiorni &
                                      df$NIGHT == 0 & df$HOUR == 12 & df$MINUTE == 0)
              windowDay = NA
            }
          }

          
          dfDay = df[1,]
          for (col in colnames(df)) {
            if ( is.na(sum(windowDay)) ) {
              dfDay[1,col] = NA  
              next
            }
            tmp = df[windowDay,col]
            if (!is.numeric(tmp) ) {
              dfDay[1,col] = NA
              next
            }
            
            posNa = which(is.na(tmp) == 1)
            if (length(posNa) > 0) tmp = tmp[-1*posNa]
            if (length(tmp) == 0) {
              dfDay[1,col] = NA
            } else {
              dfDay[1,col] = mean(tmp)
            }
            rm(tmp,posNa)
          }
          rm(col)
          dfDay$TIMESTAMP_START = df$TIMESTAMP_START[posReference]
          dfDayT = rbind(dfDayT,dfDay)     
          rm(dfDay,windowDay,posReference)
          if (approccio == 2) break;
          if (approccio == 3) break;
        }
      }
    }
  }
  dfDayT = dfDayT[-1,]

  dfDayT$YEAR = year(ymd_hm(as.character(dfDayT$TIMESTAMP_START)))
  dfDayT$MONTH = month(ymd_hm(as.character(dfDayT$TIMESTAMP_START)))
  dfDayT$DAY = day(ymd_hm(as.character(dfDayT$TIMESTAMP_START)))
  dfDayT$HOUR = hour(ymd_hm(as.character(dfDayT$TIMESTAMP_START)))
  dfDayT$MINUTE = minute(ymd_hm(as.character(dfDayT$TIMESTAMP_START)))

  dfDayT = dfDayT[order(dfDayT$TIMESTAMP_START),]

  # approccio 4
  if (approccio == 4) {
    df = dfDayT
    dfDayT = df[1,]     
    while ( nrow(df) > 0 ) {  
      windowDay = which(   
        df$YEAR == df$YEAR[1] &
        df$MONTH == df$MONTH[1] &
        df$DAY == df$DAY[1]
      )
     
      dfDay = df[1,]
      for (col in colnames(df)) {   
        if ( is.na(sum(windowDay)) ) {
          dfDay[1,col] = NA
          next
        }
        tmp = df[windowDay,col]
        if (!is.numeric(tmp) ) {
          dfDay[1,col] = NA
          next
        }
        
        posNa = which(is.na(tmp) == 1)
        if (length(posNa) > 0) tmp = tmp[-1*posNa]
        if (length(tmp) == 0) {
          dfDay[1,col] = NA
        } else {
          dfDay[1,col] = mean(tmp)
        }
        rm(tmp,posNa)
      }
      rm(col)
      dfDay$TIMESTAMP_START = df$TIMESTAMP_START[windowDay[1]]
      dfDayT = rbind(dfDayT,dfDay)
      df = df[-1*windowDay,]   
      rm(dfDay,windowDay)
    }
    dfDayT = dfDayT[-1,]
    
    dfDayT$YEAR = year(ymd_hm(as.character(dfDayT$TIMESTAMP_START)))
    dfDayT$MONTH = month(ymd_hm(as.character(dfDayT$TIMESTAMP_START)))
    dfDayT$DAY = day(ymd_hm(as.character(dfDayT$TIMESTAMP_START)))
    dfDayT$HOUR = hour(ymd_hm(as.character(dfDayT$TIMESTAMP_START)))
    dfDayT$MINUTE = minute(ymd_hm(as.character(dfDayT$TIMESTAMP_START)))

    dfDayT$HOUR = 12
    dfDayT$MINUTE = 0

    dfDayT$TIMESTAMP_START = dfDayT$MINUTE + (dfDayT$HOUR*(10^2)) + (dfDayT$DAY*(10^4)) +
      (dfDayT$MONTH*(10^6)) + (dfDayT$YEAR*(10^8))

    dfDayT = dfDayT[order(dfDayT$TIMESTAMP_START),]
  }

  return(dfDayT)
}

