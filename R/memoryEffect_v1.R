# Function to apply the weighted mean method to obtain acclimated responses
# param: dfReplicate, dataframe to use for the memoryEffect (dataframe su cui applicare il memory effect)
# param: dataDaily, daily dataset to use as calendar
# param: alpha, smooting factor in time, that is included in the exponential moving average (EMA)
# param: formula2_memory_effect; old setting: apply formula 2 (if equal to 1, it is applied)
# param: memory_all; old setting, not used: if equal to 0, Vcmax25 (and Jcmax25) are used; if equal to 1, VcmaxNew (JcmaxNew) are used
# param: initialization; old setting, not used
# param: showMsg (if T shows the function messages)
# return: data.frame with memoryEffect output
# rdname: memoryEffect

memoryEffect <- function(dfReplicate,dataDaily,alpha = 0.1,formula2_memory_effect=1,
                         memory_all = 0,initialization=0) {
  # Constants for the Arrhenius eq.
  Ha = 65330
  Haj = 43900
  Rgas = 8.314
  tkOpt = dfReplicate[,'TempOpt'] + 273.15
  tk = dfReplicate[,'tk']

  # Optimal values of Vcmax and Jmax at 25 C (V25, opt), here denoted as Vcmax25 and Jcmax25
  # Vcmax25 and Jmax25, obtained by their optimal values and the reciprocal of the exponential part of the Arrhenius equation
  dfReplicate[,'Vcmax25'] = dfReplicate[,'vcmaxOpt'] * exp ( (Ha/Rgas)*(1/tkOpt - 1/298.15))   
  dfReplicate[,'Jcmax25'] = dfReplicate[,'JmaxOpt']  * exp ( (Haj/Rgas)*(1/tkOpt - 1/298.15))  

  # creation of empty columns to fill in 
  # Acclimated responses of Vcmax and Jmax (denoted here with the 'New' term)
  dfReplicate[,'VcmaxNew'] = NA  
  dfReplicate[,'JcmaxNew'] = NA

  # On the very first day available in the dataset, VcmaxNew (JcmaxNew) is given by Vcmax25 (Jcmax25) only
  posPrimoGiorno = which(dfReplicate[,'YEAR'] == dataDaily[,'YEAR'][1] &
                         dfReplicate[,'MONTH'] == dataDaily[,'MONTH'][1] &
                         dfReplicate[,'DAY'] == dataDaily[,'DAY'][1])

  dfReplicate[,'VcmaxNew'][posPrimoGiorno] = dfReplicate[,'Vcmax25'][posPrimoGiorno]
  dfReplicate[,'JcmaxNew'][posPrimoGiorno] = dfReplicate[,'Jcmax25'][posPrimoGiorno]

  # # posizioniDatiMancanti = which(is.na(dfReplicate[,'VcmaxNew']) == 1)
  # 
  # posSecondoGiorno = which(dfReplicate[,'YEAR'] ==  dataDaily[,'YEAR'][2] &
  #                            dfReplicate[,'MONTH'] ==  dataDaily[,'MONTH'][2] &
  #                            dfReplicate[,'DAY'] ==  dataDaily[,'DAY'][2])
  # if (initialization == 1) {
  #   
  #   dfReplicate[,'VcmaxNew'][posSecondoGiorno] = dfReplicate[,'Vcmax25'][posPrimoGiorno]
  #   dfReplicate[,'JcmaxNew'][posSecondoGiorno] = dfReplicate[,'Jcmax25'][posPrimoGiorno]
  # }
  # if (initialization == 0) {
  #    
  #    dfReplicate[,'VcmaxNew'][posSecondoGiorno] = dfReplicate[,'Vcmax25'][posSecondoGiorno]
  #    dfReplicate[,'JcmaxNew'][posSecondoGiorno] = dfReplicate[,'Jcmax25'][posSecondoGiorno]
  # }
  #
  # rm(posSecondoGiorno)
  # rm(posPrimoGiorno)

  # Loop
  for (cicloDaily in seq(2,nrow(dataDaily))) {      

    # these are necessary for V25,opt [t], please see below
    posGiorno1 = which(dfReplicate[,'YEAR'] ==  dataDaily[,'YEAR'][(cicloDaily-2)] &
                          dfReplicate[,'MONTH'] ==  dataDaily[,'MONTH'][(cicloDaily-2)] &
                          dfReplicate[,'DAY'] ==  dataDaily[,'DAY'][(cicloDaily-2)])

    # rows for t=0; these are necessary for V25[t-1]
    posGiorno2 = which(dfReplicate[,'YEAR'] ==  dataDaily[,'YEAR'][(cicloDaily-1)] &
                          dfReplicate[,'MONTH'] ==  dataDaily[,'MONTH'][(cicloDaily-1)] &
                          dfReplicate[,'DAY'] ==  dataDaily[,'DAY'][(cicloDaily-1)])

    # rows for t=1; these are necessary for V25,opt [t]
    posGiorno3 = which(dfReplicate[,'YEAR'] == dataDaily[,'YEAR'][cicloDaily] &
                       dfReplicate[,'MONTH'] == dataDaily[,'MONTH'][cicloDaily] &
                       dfReplicate[,'DAY'] ==  dataDaily[,'DAY'][cicloDaily])

    cat(sprintf('exponential; year = %d; month = %d; day = %d\n',
                dfReplicate[,'YEAR'][posGiorno3[1]],
                dfReplicate[,'MONTH'][posGiorno3[1]],
                dfReplicate[,'DAY'][posGiorno3[1]]))

    # EMA application 
    for (cicloVariabili in c(1,2)) {
      if (cicloVariabili == 1) {
        cicloVariabili1 = 'Vcmax25'   
        cicloVariabili2 = 'VcmaxNew'    
      }
      if (cicloVariabili == 2) {
        cicloVariabili1 = 'Jcmax25'   
        cicloVariabili2 = 'JcmaxNew'    
      }

      # working on the index
      posGiorno1 = posGiorno3     # therefore, giorno1 is giorno3
      # data extraction to compute the exponential moving average (EMA) 
      giorno1 = dfReplicate[posGiorno1,cicloVariabili1]         # V25,opt [t]
      giorno2 = dfReplicate[posGiorno2,cicloVariabili2]         # V25[t-1]
      
      if (sum(is.na(giorno2)) == length(giorno2))
        giorno2 = dfReplicate[posGiorno2,cicloVariabili1]


      # EMA application to obtain the new updated values of Vcmax25 and Jmax25
      # equation: V25[t]= V25[t-1]*(1- alpha) + alpha * V(25,opt)[t]
      giorno3 = (giorno2 * (1 - alpha) + (alpha * giorno1))   # where giorno1 is giorno3!


      # formula 2 (an attempt)--> however, this has to be applied in the main script, not here
      if (cicloVariabili1 == 'Vcmax25') HaX = Ha              
      if (cicloVariabili1 == 'Jcmax25') HaX = Haj             

      if (formula2_memory_effect)
        giorno3 = giorno3 * exp((HaX/Rgas)*(1/298.15 - 1/tk[posGiorno3]))  # (formula2)

      
      if (cicloVariabili1  == 'Vcmax25')                           
        dfReplicate[posGiorno3,'VcmaxNew'] = giorno3

      if (cicloVariabili1  == 'Jcmax25')                           
        dfReplicate[posGiorno3,'JcmaxNew'] = giorno3

      rm(HaX)
    }
    rm(cicloVariabili)

    rm(giorno1,giorno2,giorno3,posGiorno1,posGiorno2,posGiorno3)
  }
  rm(cicloDaily)

  return(dfReplicate)

}
