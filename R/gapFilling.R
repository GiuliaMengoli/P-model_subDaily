# Function to fill in the missing data of a vector according to the settings
# param: v, data vector to be filled
# param: vYear, vector of the years
# param: vMonth, vector of the months
# param: vDay, vector of the days
# param: vHour, vector of the hours
# param: vMinute, vector of the minutes
# param: approccio (approach) if 'constant' replicates measured data on missing values;
#                             if 'linear' it applies linear regression to fill in missing values among measured data
# param: showMsg (if T shows the function messages)
# return: a vector with required inputs for pmodelPlus
# rdname: gapFilling

gapFilling = function(v = vettore,v_name = NA,
         vYear = vYear, vMonth = vMonth, vDay = vDay,
         vHour = vHour, vMinute = vMinute, showMsg = F,
         approccio = 'constant') {

  # check if there are values in v 
  if ( sum(is.na(v)) == length(v) ) {
    warning('variable not gap filled\n') 
    return(v)
  }
  str_msg = ''
  if (!is.na(v_name))
    str_msg = paste0(str_msg,sprintf('GAP FILLING variable: %s\n',v_name))

  str_msg = paste0(str_msg,
     sprintf('approach: %s\nmissing value%s: %s\n',
             approccio,
             ifelse(sum(is.na(v)) == 1,'','s'),sum(is.na(v))))

  if (showMsg)
    cat(str_msg)

  if (approccio == 'constant') {
    # 
    # for (cyr in seq(2,length(v))) {
    # 
    #   if (is.na(v[cyr]))
    #     v[cyr] = v[cyr-1]
    # }
    # rm(cyr)
    posValori = which(is.na(v) == 0)
    for (contaV in posValori) {
     
      yearToUse  = vYear[contaV]
      monthToUse = vMonth[contaV]
      dayToUse   = vDay[contaV]
      vToUse = v[contaV]

      posToUse = which(
        vYear == yearToUse &
        vMonth == monthToUse &
        vDay == dayToUse)

      if ( length(posToUse) > 0 )
        v[posToUse] = vToUse

      rm(yearToUse,monthToUse,dayToUse)
      rm(vToUse,posToUse)
    }
    rm(contaV)
  }
  #approccio: 'linear' 
  if (approccio == 'linear') {
    posValori = which(is.na(v) == 0)
    for (contaV in seq(2,length(posValori))) {
      # slope computetation 
      x1 = posValori[contaV - 1]
      x2 = posValori[contaV]
      y1 = v[x1]
      y2 = v[x2]
      slope = (y2 - y1) / (x2-x1)
      intercept = y1 - (slope*x1)
      itp = (slope * seq(x1,x2)) + intercept
      v[seq(x1,x2)] = itp
    }
  }

  if (showMsg)
    cat(sprintf('GAP FILLING COMPLETED\nmissing value%s:%d\n',
               ifelse(sum(is.na(v)) == 1,'','s'),sum(is.na(v))))

  return(v)

}
