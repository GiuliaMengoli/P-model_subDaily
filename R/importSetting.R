# Function to import the setting file for the pmodelPlus
# Description: imports function settings from settings files
# param: file_setting name (or completed path) of the setting file
# Param: showMsg (if T shows the function messages)
# return: a data.frame with the informations included in the setting file
# rdname: importSetting


importSetting <- function(file_setting = file_setting,showMsg = F) {

  if (showMsg)
    cat(sprintf('import file: %s\n',file_setting))

  dati_settaggi = read.csv(file_setting,comment.char = '#',
                           header = F,stringsAsFactors = F)
  if (showMsg)
    cat(sprintf('import file: %s ...OK\n\n',file_setting))

  df_setting = data.frame('file_setting' = file_setting)

  if (showMsg)
    cat(sprintf('check items\n'))

  # check items in the file
  for (set_item in c('site_name',"elevation_site","input_pals","iabs_method",
                     "fAPAR_method","phi0_method","daily_method","daily_window",
                     "xi_acclimated","method_gf_replicate","file_eddy","file_meris",
                     "file_pals_meteo","file_pals_gpp","file_pals_LAI",
                     'method_gf_fAPAR','method_dd','method_dd_href','method_dd_win_size',
                     'method_gf_replicate','formula2_memory_effect',
                     'input_memory_effect','initialization_memory_effect',
                     "dir_output")) {

    pos_set = which(dati_settaggi$V1 == set_item)

    if (length(pos_set) == 0) {
      is.mandatory = 1
      if (set_item == "file_pals_meteo") is.mandatory = df_setting[,"input_pals"]
      if (set_item == "file_pals_gpp")   is.mandatory = df_setting[,"input_pals"]
      if (set_item == "file_pals_LAI")   is.mandatory = df_setting[,"input_pals"]

      if (is.mandatory) {
        stop(sprintf('missing mandatory item: %s\n',set_item))
      } else {
        warning(sprintf('missing item: %s\n',set_item))
      }
      rm(is.mandatory)
    } else {
      df_setting[,set_item] = trimws(dati_settaggi$V2[pos_set])
    }
    rm(pos_set)
  }
  rm(set_item)

  if (showMsg)
    cat(sprintf('check items ...OK\n\n'))

  # conversion of numerical values
  for (num_item in c('input_pals','elevation_site','phi0_method',
                     'daily_window','method_dd','formula2_memory_effect',
                     'input_memory_effect','initialization_memory_effect',
                     'method_dd_href','method_dd_win_size')){

    df_setting[,num_item] = tryCatch(
      {as.numeric(df_setting[,num_item])},
      error = function(e) {
        stop('%s is not numeric\n',df_setting[,num_item])
      },
      warning = function(w) {
        stop('%s is not numeric\n',df_setting[,num_item])
      })
  }

  if (is.na(df_setting$phi0_method)) df_setting$phi0_method = NA

  # save in a given directory
  df_setting$dir_output = paste0(df_setting$dir_output,'/',df_setting$site_name,'/')
  dir.create(df_setting$dir_output,showWarnings = F)

  # convert slash to backslash
  for (col in colnames(df_setting)) {
    if (is.character(df_setting[,col]))
      df_setting[,col] = gsub('\\\\','/',df_setting[,col])
  }
  rm(col)

  if (showMsg)
    cat(sprintf('create output directory: %s\n\n',df_setting$dir_output))


  if (showMsg)
    for (col in colnames(df_setting))
      cat(sprintf('%s: %s\n',col,df_setting[,col]))

  for (col in colnames(df_setting))
    if (is.factor(df_setting[,col]))
      df_setting[,col] = as.character(df_setting[,col])

  return(df_setting)
}
