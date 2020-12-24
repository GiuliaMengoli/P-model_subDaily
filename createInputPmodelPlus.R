#  Imports fAPAR and EDDY dataset files to create the input data.frame for pmodelPlus
#  param: file_fapar name (or full path) of the fAPAR file to import
#  param: format_fapar, format of the file_fapar ('beni2' default)
#  param: file_eddy name (or full path) of the eddy covariance file
#  param: format_eddy, format of the file_eddy ('fluxnet2015' default)
#  param: showMsg (if T shows the function messages)
#  return: a data.frame with required inputs for pmodelPlus
#  rdname: createInputPmodelPlus

createInputPmodelPlus <- function(file_fapar = file_fapar,format_fapar = 'beni2',
                      file_eddy = file_eddy,
                      format_eddy = 'fluxnet2015',
                      elevation_site = NA,
                      method_gf_fAPAR = method_gf_fAPAR,
                      showMsg = F) {

  # imports file eddy
  data_eddy = fileOr2pModelPlus(file_in = file_eddy,showMsg = showMsg,
                               file_format = format_eddy)
  # imports file fapar
  data_fapar = fileOr2pModelPlus(file_in = file_fapar,showMsg = showMsg,
                                file_format = format_fapar)

  # synchronization of the two datasets
  cat(sprintf('\nsyncronize fAPAR and EDDY time series\n'))
  start_time = max(c(min(data_eddy$YEAR),min(data_fapar$YEAR)))
  end_time = min(c(max(data_eddy$YEAR),max(data_fapar$YEAR)))
  cat(sprintf('start time: %d\n',start_time))
  cat(sprintf('end time: %d\n',end_time))

  for (cyds in c('fAPAR','EDDY')) {
    for (trange in c('less','greater')) {
      if (trange == 'less') id_time = start_time
      if (trange == 'greater') id_time = end_time

      if (trange == 'less') {
        if (cyds == 'fAPAR')
          to_del = which(data_fapar$YEAR < id_time)
        if (cyds == 'EDDY')
          to_del = which(data_eddy$YEAR < id_time)
      }
      if (trange == 'greater') {
        if (cyds == 'fAPAR')
          to_del = which(data_fapar$YEAR > id_time)
        if (cyds == 'EDDY')
          to_del = which(data_eddy$YEAR  > id_time)
      }
      if (showMsg)
        cat(sprintf('%s dataset: %d value %s than %d\n',
                cyds,length(to_del),trange,id_time))
      if (length(to_del) > 0) {
        if (cyds == 'fAPAR') data_fapar = data_fapar[-1*to_del,]
        if (cyds == 'EDDY')  data_eddy = data_eddy[-1*to_del,]
      }
      rm(to_del,id_time)
    }
    rm(trange)
  }
  rm(cyds)

  # mergind of the two datasets (using TIMESTAMP_ok)
  cat(sprintf('\nEDDY dataset nr row = %d\n',nrow(data_eddy)))
  cat(sprintf('fAPAR dataset nr row = %d\n\n',nrow(data_fapar)))

  # add rows

  for (cyref in c('EDDY','fAPAR')) {
    if (cyref == 'EDDY') {
      df_ref = data_eddy
      df_cmp = data_fapar
    }
    if (cyref == 'fAPAR') {
      df_ref = data_fapar
      df_cmp = data_eddy
    }

    time_missing = c()
    for (cytr in df_cmp$TIMESTAMP_START_ok) {
      pos_time_missing = which(df_ref$TIMESTAMP_START_ok == cytr)
      if (length(pos_time_missing) == 0)
        time_missing = c(time_missing,cytr)
    }
    rm(cytr,pos_time_missing)

    if (showMsg)
      cat(sprintf('add %d row%s to the dataset %s\n',
                  length(time_missing),
                  ifelse(length(time_missing) == 0, '','s'),
                  cyref))

    if (length(time_missing) > 0) {
      df_to_add = data.frame('TIMESTAMP_START_ok' = time_missing)

      for (col in colnames(df_ref)) {
        if (col == 'YEAR') next
        if (col == 'MONTH') next
        if (col == 'DAY') next
        if (col == 'HOUR') next
        if (col == 'MINUTE') next
        if (col == 'TIME') next
        if (col == 'TIMESTAMP_START') next
        if (col == 'TIMESTAMP_START_ok') next
        df_to_add[,col] = NA
      }
      rm(col)

      df_to_add$TIMESTAMP_START = df_to_add$TIMESTAMP_START_ok
      df_to_add$TIME = ymd_hm(df_to_add$TIMESTAMP_START_ok)
      df_to_add$YEAR = year(df_to_add$TIME)
      df_to_add$MONTH = month(df_to_add$TIME)
      df_to_add$DAY = day(df_to_add$TIME)
      df_to_add$HOUR = hour(df_to_add$TIME)
      df_to_add$MINUTE = minute(df_to_add$TIME)

      df_ref = rbind(df_ref,df_to_add)
      df_ref = df_ref[order(df_ref$TIMESTAMP_START_ok),]
      rm(df_to_add)
      if (cyref == 'fAPAR') data_fapar = df_ref
      if (cyref == 'EDDY')  data_eddy = df_ref
    }
    rm(time_missing)
    rm(df_ref,df_cmp)
  }
  rm(cyref)

  cat(sprintf('\nEDDY dataset nr row = %d\n',nrow(data_eddy)))
  cat(sprintf('fAPAR dataset nr row = %d\n\n',nrow(data_fapar)))

  cat(sprintf('create pModel input data.frame\n'))
  data_fapar = data_fapar[,c('TIMESTAMP_START_ok','fapar')]

  dataPmodelPlus = merge(data_eddy,data_fapar,by.x='TIMESTAMP_START_ok',
                         by.y='TIMESTAMP_START_ok')

  cat(sprintf('add the elevation_site column to the pModel input data.frame'))
  dataPmodelPlus$z_v = elevation_site

  if (showMsg) {
    cat(sprintf('variable in pModel input data.frame:\n'))
    cat(sprintf('%s\n',colnames(dataPmodelPlus)))
  }

  # since in fluxnet the missing value is indicated with -9999
  cat('\n')
  for ( col in colnames(dataPmodelPlus) ) {
    if ( !is.numeric(dataPmodelPlus[,col]) ) next
    pos_na = which(dataPmodelPlus[,col] < -9990)
    if ( length(pos_na) > 0 ) {
      dataPmodelPlus[pos_na,col] = NA
      cat(sprintf('column: %s;%d values less than -9990 changed in NA\n',
                  col,length(pos_na)))
    }
    rm(pos_na)
  }
  rm(col)

  # replace with NA 
  cat('\n')
  for (col_no_neg in c('PPFD','VPD','CO2','fapar','LAI')) {
    if (col_no_neg %in% colnames(dataPmodelPlus) ) {
      pos_neok = which(dataPmodelPlus[,col_no_neg] < 0)
      if(length(pos_neok) > 0) {
        dataPmodelPlus[pos_neok,col_no_neg] = NA
        cat(sprintf('column: %s; %d negative values changed in NA\n',
                  col_no_neg,length(pos_neok)))
      }
      rm(pos_neok)
    }
  }
  rm(col_no_neg)
  cat('\n')

  # cat(sprintf('\nfill fAPAR method: %s\n',method_gf_fAPAR))
  dataPmodelPlus$fapar = gapFilling(
    v = dataPmodelPlus$fapar,v_name = 'fapar',
    vYear = dataPmodelPlus$YEAR,
    vMonth = dataPmodelPlus$MONTH,
    vDay = dataPmodelPlus$DAY,
    vHour = dataPmodelPlus$HOUR,
    vMinute = dataPmodelPlus$MINUTE,
    approccio = method_gf_fAPAR,showMsg = showMsg)

  # In the NIGHTTIME period the PPFD is zero ----
  # CONDICTION FOR THE NIGHT HOURS***********************************************************************
  # using the 'night' column to set the values of some columns equal to zero
  # when 'night' column is == to 1 the values of the columns: PPFD, fapar, e LAI have to be equal to zero.
  # NB: GPP_DT_CUT_REF is equal to zero when is night (according to the NIGHT column)

  for (col_night_zero in c('PPFD','fapar','LAI')) {
    if (!(col_night_zero %in% colnames(dataPmodelPlus))) next
    pos0 = which(dataPmodelPlus$NIGHT == 1 &
                   is.na(dataPmodelPlus[,col_night_zero]) == 0)
    if (length(pos0) > 0) {
      if (showMsg)
        cat(sprintf('add %d zero value%s to %s if NIGHT == 1\n',
                    length(pos0),
                    ifelse(length(pos0) == 1,'','s'),col_night_zero))
      dataPmodelPlus[pos0,col_night_zero] = 0
    }
    rm(pos0)
  }
  rm(col_night_zero)


  return(dataPmodelPlus)

}
