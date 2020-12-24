#  Imports input dataset files (fAPAR & fluxnet)
#  files: FLUXNET2015 (half-hourly) and fAPAR (format beni2)
#  and creates the provisional input data.frame for pmodelPlus
#  param: file_in name (or full path) of the file to import
#  param: file_format if 'fluxnet2015' (default) it imports fluxnet data; if 'beni2' it imports bani2 data
#  param: showMsg (if T shows the function messages)
#  return: a data.frame with required inputs for pmodelPlus
#  rdname: fileOr2pModelPlus


fileOr2pModelPlus <- function(file_in = file_in,file_format='fluxnet2015',showMsg = F) {

  library(lubridate,warn.conflicts = F)

  # check if the file exists
  if (!file.exists(file_in))
    stop(sprintf('file: %s NOT EXISTS',file_in))

  if (showMsg)
    cat(sprintf('\nimport file: %s\nformat: %s\n',file_in,file_format))

  # import original dataset
  data_or = read.csv(file_in)

  if (showMsg)
    cat(sprintf('\nimport file: %s ...OK\n',file_in,file_format))


  # rename the subset columns to extract
  if (file_format == 'fluxnet2015') {
    df_rename = rbind(
      data.frame('original_name' = "TIMESTAMP_START",'new_name' = "TIMESTAMP_START"),
      data.frame('original_name' = "TA_F",'new_name' = "Ta"),
      data.frame('original_name' = "SW_IN_POT",'new_name' = "SW_IN_POT"),
      data.frame('original_name' = "SW_IN_F",'new_name' = "SW_IN"),
      data.frame('original_name' = "VPD_F",'new_name' = "VPD"),
      data.frame('original_name' = "PPFD_IN",'new_name' = "PPFD"),
      data.frame('original_name' = "P_F",'new_name' = "P"),
      data.frame('original_name' = "CO2_F_MDS",'new_name' = "CO2"),
      data.frame('original_name' = "CO2_F_MDS_QC",'new_name' = "CO2_QC"),
      data.frame('original_name' = "NIGHT",'new_name' = "NIGHT"),
      data.frame('original_name' = "NEE_CUT_REF_QC",'new_name' = "NEE_CUT_REF_QC"),
      data.frame('original_name' = "NEE_CUT_REF",'new_name' = "NEE_CUT_REF"),
      data.frame('original_name' = "GPP_DT_CUT_REF",'new_name' = "GPP_DT_CUT_REF"),
      data.frame('original_name' = "GPP_DT_CUT_05",'new_name' = "GPP_DT_CUT_05"),
      data.frame('original_name' = "GPP_DT_CUT_95",'new_name' ="GPP_DT_CUT_95")
    )
  } else if (file_format == 'beni2') {
    df_rename = rbind(
      data.frame('original_name' = 'TIME','new_name' = 'TIMESTAMP_START'),
      data.frame('original_name' = 'fapar_spl','new_name' = 'fapar')
    )
    
  }else {
    stop(sprintf('file_format: %s NOT EXISTS',file_format))
  }
  df_rename$original_name = as.character(df_rename$original_name)
  df_rename$new_name = as.character(df_rename$new_name)

  # check if the columns are present in the file
  col_old = c()
  col_new = c()
  for (col in seq(1,nrow(df_rename))) {
    old_name = df_rename$original_name[col]
    new_name = df_rename$new_name[col]

    if (!(old_name %in% colnames(data_or)))
      stop(sprintf('variable: %s missing in fluxnet file\n',old_name))

    if (showMsg)
      cat(sprintf('variable: %s in fluxnet file checked\n',old_name))

    if (old_name != new_name)
      cat(sprintf('rename fluxnet variable: %s in %s\n',old_name,new_name))

    col_old = c(col_old,old_name)
    col_new = c(col_new,new_name)

    rm(old_name,new_name)
  }
  rm(col)

  data = data_or[,col_old]
  colnames(data) = col_new

  # change -9999 with NA
  if (showMsg) cat('\n')

  for (col in colnames(data)) {
    pos_na = which(data[,col] < -9990)
    if (length(pos_na) == 0) next
    if (length(pos_na) == nrow(data))
      stop(sprintf('all values in variable: %s are missing\n',col))
    data[pos_na,col] = NA
    if (showMsg)
      cat(sprintf('variable: %s changed %d values in NA\n',col,length(pos_na)))
  }
  rm(col)

  cat(sprintf('\nadd column TIME (convert TIMESTAMP_START to Date (POSIXct)\n'))
  if (file_format == 'fluxnet2015') {
    data$TIME = ymd_hm(data$TIMESTAMP_START)#YYYYMMDDHHMM
  } else if (file_format == 'beni2') {
    data$TIME = ymd(data$TIMESTAMP_START)#YYYYMMDD
  }

  cat(sprintf('add column YEAR\n'))
  data$YEAR = year(data$TIME)
  cat(sprintf('add column MONTH\n'))
  data$MONTH = month(data$TIME)
  cat(sprintf('add column DAY\n'))
  data$DAY = day(data$TIME)
  cat(sprintf('add column HOUR\n'))
  data$HOUR = hour(data$TIME)
  cat(sprintf('add column MINUTE\n'))
  data$MINUTE = minute(data$TIME)
  cat(sprintf('add column WEEK\n'))
  data$WEEK = week(data$TIME)

  cat(sprintf('add column TIMESTAMP_START_ok (YYYYMMDDHHmm) \n'))

  data$TIMESTAMP_START_ok = data$MINUTE + (data$HOUR * (10^2)) +
    (data$DAY * (10^4)) + (data$MONTH * (10^6))  + (data$YEAR * (10^8))

  return(data)
}
