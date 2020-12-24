# MAIN SCRIPT FOR THE RUNNING MEAN METHOD
rm(list = ls())
cat('\014')

setwd('C:/pModelPlus/pModelPlus')

# functions:
source ('./R/importSetting.R')          # imports function settings from settings files
source ('./R/fileOr2pModelPlus.R')      # imports input dataset files (MODIS fAPAR & fluxnet)
source ('./R/createInputPmodelPlus.R')  # creates the input dataset by merging the fluxnet2015 and MODIS fAPAR dataset
source ('./R/pmodelPlus.R')             # applies the P model according to the settings
source ('./R/dailyDownscaling.R')       # creates daily dataset from half hourly data
source ('./R/gapFilling.R')             # fills in the missing data according to the settings
source ('./R/headerControl.R')          # checks that the mandatory variables exist in the dataset
source ('./R/runningMean.R')            # applies the running mean to the model inputs


# Settings files:
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/First_paper/BE-Vie_test_3a_2014.csv'      # MODIS, 'MAX: noon approach'   
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/First_paper/BE-Vie_test_3b_2014.csv'      # MODIS, 'MEAN: daily approach'   
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/First_paper/BE-Vie_test_3H_2014.csv'      # MODIS, '3 hours approach'     


# 1) Max vs Daily approaches: for all sites
# BE-Vie
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/First_paper/BE-Vie_test_3a_2014.csv'      # MODIS, 'MAX'   
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/First_paper/BE-Vie_test_3b_2014.csv'      # MODIS, 'MEAN'  
# USA
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/First_paper/US-UMB_test_3a.csv'           # MODIS, 'MAX'  
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/First_paper/US-UMB_test_3b.csv'           # MODIS, 'MEAN'
# Hyy
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/First_paper/FI-Hyy_test_3d_2014.csv'      # MODIS, 'MAX'     
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/First_paper/FI-Hyy_test_3e_2014.csv'      # MODIS, 'MEAN'   
# CH-Cha
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/First_paper/CH_Cha_test_3a.csv'           # MODIS, 'MAX'    
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/First_paper/CH_Cha_test_3b.csv'           # MODIS, 'MEAN'    
# GF-Guy
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/First_paper/GF_Guy_test_3a.csv'           # MODIS, 'MAX'    
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/First_paper/GF_Guy_test_3b.csv'           # MODIS, 'MEAN'    

# 2) 3-HOUR approach (run the right 'example command', see below:line 128)
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/First_paper/BE-Vie_test_3H_2014.csv'      # MODIS, '3H'   
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/First_paper/FI-Hyy_test_3H_2014.csv'      # MODIS, '3H'   
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/First_paper/GF_Guy_test_3H.csv'           # MODIS, '3H'   
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/First_paper/US-UMB_test_3H.csv'           # MODIS, '3H'  
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/First_paper/CH_Cha_test_3H.csv'           # MODIS, '3H'  



df_settaggio = importSetting(file_setting = file_settaggio,showMsg = T)

file_data = paste0(df_settaggio$dir_output,'data_input_',df_settaggio$site_name,'.csv')
file_modelloOr = paste0(df_settaggio$dir_output,"modelloOr_",
                        df_settaggio$site_name,'#',basename(df_settaggio$file_setting))
file_modello_mm = paste0(df_settaggio$dir_output,"modelloMm_",
                         df_settaggio$site_name,'#',basename(df_settaggio$file_setting))
file_modello_mmME = paste0(df_settaggio$dir_output,"modelloMmME_",
                           df_settaggio$site_name,'#',basename(df_settaggio$file_setting))
file_dfReplicate_mmME = paste0(df_settaggio$dir_output,"dfReplicateMmME_",
                               df_settaggio$site_name,'#',basename(df_settaggio$file_setting))

file_finale = paste0(df_settaggio$dir_output,"dataOr_",
                     df_settaggio$site_name,'#',basename(df_settaggio$file_setting))

# IMPORT DATA and save
# shortcut (if input data already exist-->createInputPmodelPlus)
# if (file.exists(file_data)) {
#   data = read.csv(file_data)
#   data[,'TIME'] = ymd_hms(data[,'TIME'])
# } else {
data = createInputPmodelPlus(
  file_fapar = df_settaggio$file_meris,
  file_eddy = df_settaggio$file_eddy,
  format_fapar = 'beni2',
  format_eddy = 'fluxnet2015',
  elevation_site = df_settaggio$elevation_site,
  method_gf_fAPAR = df_settaggio$method_gf_fAPAR,
  showMsg = T)

# dir.create(df_settaggio$dir_output,showWarnings = F,recursive = T)
# write.csv(x = data,row.names = F,file = file_data)
# }
#
# # shortcut
# if (file.exists(file_modelloOr)) {
#   modelloOr = read.csv(file_modelloOr)
#   data[,'TIME'] = ymd_hms(data[,'TIME'])
# } else {

# ONLY FOR GF-Guy
# pos_time = which ( year (data$TIME) == 2014)   # 2014
# data = data[pos_time,]
# range(data$TIME)

# APPLY the pmodelPlus function to original data (half-hourly timestep) and save the results ----
modelloOr = pmodelPlus(data = data,
                       iabsMethod = df_settaggio$iabs_method,
                       fAPARMethod = df_settaggio$fAPAR_method,
                       phi0Method = df_settaggio$phi0_method )

# write.csv(x = modelloOr,file = file_modelloOr,
#           row.names = F,quote = F)
# }
#
# # shortcut
# if (file.exists(file_modello_mm)) {
#   modelloMm = read.csv(file_modello_mm)
#   modelloMm[,'TIME'] = ymd_hms(modelloMm[,'TIME'])
# } else {
# RENAME DATASET
dataHh = modelloOr

# Columns extraction (time and inputs)
dataHhReduc = dataHh[,c('TIMESTAMP_START','YEAR','MONTH','DAY','HOUR','MINUTE','fapar',
                        'NIGHT','Ta','PPFD','VPD','fAPAR','CO2','LAI','SW_IN_POT')]


#  only for GF-Guy
# dataHhReduc = dataHhReduc[-105121,]

# APPLY the dailyDownscaling function (according to setting file: noon conditions)----
dataDaily = dailyDownscaling(df = dataHhReduc,
                             nrWindow = df_settaggio$method_dd_win_size,
                             hourReferenceT = df_settaggio$method_dd_href,
                             approccio = df_settaggio$method_dd)

# example for the 3-hour approach
# dataDaily = dailyDownscaling(df = dataHhReduc)


# APPLY the runningMean function to the model inputs----
dataDaily = runningMean(data_x_running_mean_t = dataDaily, daily_window = 15)    # days: 3, 7, 10, 15, 30, 45, 60

# ADD elevation and time columns
dataDaily[,'z_v'] = df_settaggio$elevation_site

dataDaily[,'TIME'] = ymd_hm(dataDaily[,'TIMESTAMP_START'])

# RUN the pmodelPlus function on average daily inputs----
modelloMm = pmodelPlus(dataDaily,
                       iabsMethod = df_settaggio$iabs_method,
                       fAPARMethod = df_settaggio$fAPAR_method,
                       phi0Method = df_settaggio$phi0_method)

# write.csv(x = modelloMm,file = file_modello_mm,row.names = F,quote = F)
# }
#

# if (file.exists(file_dfReplicate_mmME)) {
#   dfReplicate = read.csv(file_dfReplicate_mmME)
#   dfReplicate[,'TIME'] = ymd_hm(dfReplicate[,'TIMESTAMP_START'])
# } else {

# FROM daily to half hourly: 'dfReplicate' dataframe creation----
dfReplicate = modelloOr[,c('TIMESTAMP_START',"YEAR","MONTH","DAY","HOUR","MINUTE")]

dfReplicate[,'KOpt'] = NA
dfReplicate[,'GammaStarOpt'] = NA
dfReplicate[,'viscosityOpt'] = NA

dfReplicate[,'xiOpt'] = NA
dfReplicate[,'ciOpt'] =NA

dfReplicate[,'VPDOpt'] = NA
dfReplicate[,'CO2Opt'] = NA
dfReplicate[,'caOpt'] = NA
dfReplicate[,'IabsOpt'] = NA
dfReplicate[,'TempOpt'] = NA


# xiAcclimated option: 'off'/'on'
dfReplicate[,'vcmaxOpt'] = NA     
dfReplicate[,'JmaxOpt'] = NA      


modelloMm2 = modelloMm

while (nrow(modelloMm2) > 0 ) {
  posValues = which(
      dfReplicate[,'YEAR'] == modelloMm2[,'YEAR'][1] &
      dfReplicate[,'MONTH'] == modelloMm2[,'MONTH'][1] &
      dfReplicate[,'DAY'] == modelloMm2[,'DAY'][1] &
      dfReplicate[,'HOUR'] == modelloMm2[,'HOUR'][1] &
      dfReplicate[,'MINUTE'] == modelloMm2[,'MINUTE'][1]
  )
  dfReplicate[,'KOpt'][posValues] = modelloMm2[,'kmPa'][1]
  dfReplicate[,'GammaStarOpt'][posValues] = modelloMm2[,'GammaStarM'][1]
  dfReplicate[,'viscosityOpt'][posValues] = modelloMm2[,'viscosityH2oStar'][1]
  dfReplicate[,'xiOpt'][posValues] = modelloMm2[,'xiPaM'][1]                    # xiAcclimated: 'off'
  # dfReplicate[,'xiOpt'][posValues] = modelloMm2[,'xiPa'][1]
  dfReplicate[,'ciOpt'][posValues] = modelloMm2[,'ciM'][1]                      # xiAcclimated: 'off'
  # dfReplicate[,'ciOpt'][posValues] = modelloMm2[,'ci'][1]
  dfReplicate[,'VPDOpt'][posValues] = modelloMm2[,'VPDPa'][1]
  dfReplicate[,'CO2Opt'][posValues] = modelloMm2[,'CO2'][1]
  dfReplicate[,'caOpt'][posValues] = modelloMm2[,'ca'][1]
  dfReplicate[,'IabsOpt'][posValues] = modelloMm2[,'Iabs'][1]
  # if xiAcclimated is ON vcmaxOpt and JmaxOpt will be overwritten
  dfReplicate[,'vcmaxOpt'][posValues] = modelloMm2[,'vcmaxPmodelM1'][1]  # xiAcclimated: 'off'
  dfReplicate[,'JmaxOpt'][posValues] = modelloMm2[,'JmaxM1'][1]          # xiAcclimated: 'off'


  # AVERAGE TEMPERATURE
  dfReplicate[,'TempOpt'][posValues] = modelloMm2[,'Ta'][1]

  modelloMm2 = modelloMm2[-1,] 
}

# gapFilling function (set what type of approach 'i.e. constant' in the file setting)----
for (col in colnames(dfReplicate)) {
  if ( col == 'TIMESTAMP_START') next
  if ( col == 'YEAR') next
  if ( col == 'MONTH') next
  if ( col == 'DAY') next
  if ( col == 'HOUR') next
  if ( col == 'MINUTE') next
  cat(sprintf('%s gap = %d\n',col,sum(is.na(dfReplicate[,col]))))
  dfReplicate[,col] = gapFilling(v = dfReplicate[,col],
                                 vYear = dfReplicate[,'YEAR'],
                                 vMonth = dfReplicate[,'MONTH'],
                                 vDay = dfReplicate[,'DAY'],
                                 vHour = dfReplicate[,'HOUR'],
                                 vMinute = dfReplicate[,'MINUTE'],
                                 approccio = df_settaggio$method_gf_replicate)
  cat(sprintf('%s post gap = %d\n',col,sum(is.na(dfReplicate[,col]))))
}
rm(col)

# OPTIMAL Vcmax and Jmax----
# Vcmax with acclimated 'xiPa', 'ci' and 'phi0' (ON)
if (df_settaggio$xi_acclimated == 'off') {
  cat(sprintf('vcmaxOpt and JmaxOpt replicated\n'))
  dfReplicate[,'ci'] = NA
  dfReplicate[,'xiPa'] = NA
} else {

  cat(sprintf('vcmaxOpt and JmaxOpt calculated\n'))

  # Intrinsic quantum efficiency of photosynthesis (phi0)
  if (is.na(df_settaggio$phi0_method) ) {

    phi0 =(1/8) *(0.352+0.022*modelloOr[,'Ta'] -
                    0.00034*modelloOr[,'Ta']^(2) )      # Temperature dependence function of phi0 (Bernacchi et al.,2003)
  } else {
    phi0 = phi0Method
  }

  c=0.41                        # cost factor for electron transport capacity

  # acclimated xiPa (parameter that determines the sensitivity of ci/ca to VPD)
  betha = 146

  dfReplicate[,'xiPa'] = sqrt ( (betha * (dfReplicate[,'KOpt'] +
                                            dfReplicate[,'GammaStarOpt']) ) /
                                      (1.6 * dfReplicate[,'viscosityOpt']) )                      # [Pa^1/2]

  # acclimated ci (with acclimated xiPa, and adjusted with the actual VPD)
  dfReplicate[,'ci'] = ( dfReplicate[,'xiPa'] * dfReplicate[,'caOpt'] +
                           dfReplicate[,'GammaStarOpt']*sqrt(modelloOr[,'VPDPa']))/(
                             dfReplicate[,'xiPa']+sqrt(modelloOr[,'VPDPa']))


  # OPTIMAL Vcmax
  dfReplicate[,'vcmaxOpt']  = phi0 * dfReplicate[,'IabsOpt'] *
    ((dfReplicate[,'ci'] + dfReplicate[,'KOpt']) / (dfReplicate[,'ci'] +
                                                              2*dfReplicate[,'GammaStarOpt'])) *
    sqrt (1-(c * (dfReplicate[,'ci']+2*dfReplicate[,'GammaStarOpt'])/(
      dfReplicate[,'ci']-dfReplicate[,'GammaStarOpt']))^(2/3))                                        # [micromol/m2s]

  # OPTIMAL Jmax
  dfReplicate[,'JmaxOpt']  = ( 4 * phi0 * dfReplicate[,'IabsOpt'] ) / sqrt (
    1 / ( 1 - ( c * ( dfReplicate[,'ci'] + 2*dfReplicate[,'GammaStarOpt'] ) /
                  ( dfReplicate[,'ci'] - dfReplicate[,'GammaStarOpt'] ) ) ^ (2.0/3.0) ) - 1 )           #[micromol/m2s]
}

# Optimal and actual temperature conversion
dfReplicate[,'topt'] = dfReplicate[,'TempOpt'] + 273.15                               # [K]   
dfReplicate[,'tk'] = modelloOr[,'Ta'] + 273.15                                        # [K]

# INSTANTANEOUS Vcmax and Jmax----
# The Arrhenius equation constants:
Ha = 65330                       # J mol-1 
Haj = 43900
Rgas = 8.314                     # J/mol*K


dfReplicate[,'vcmaxAdjusted'] = dfReplicate[,'vcmaxOpt'] * exp( (Ha / Rgas)*
                                                  (1/dfReplicate[,'topt'] - 1/dfReplicate[,'tk']))

dfReplicate[,'JmaxAdjusted'] = dfReplicate[,'JmaxOpt'] * exp( (Haj / Rgas)*
                                                 (1/dfReplicate[,'topt'] - 1/dfReplicate[,'tk']))

rm(Rgas,Ha,Haj)


# CALCULATE the assimilation rate: Ac-----
if (df_settaggio$xi_acclimated == 'off') {
  Ac <- dfReplicate[,'vcmaxAdjusted']*(modelloOr[,'ciM'] - modelloOr[,'GammaStarM']) /
    (modelloOr[,'ciM'] + modelloOr[,'kmPa'])                                              #[micromol/m2s]
} else {
  # acclimated Ac with the acclimated xiPa term
  Ac = dfReplicate[,'vcmaxAdjusted']*(
    dfReplicate[,'ci'] - modelloOr[,'GammaStarM']) /
    (dfReplicate[,'ci'] + modelloOr[,'kmPa'])                                             #[micromol/m2s]
}


# CALCULATE the assimilation rate: AJ----
# Intrinsic quantum efficiency of photosynthesis [mol Co2/mol photons]
if (is.na(df_settaggio$phi0_method) ) {
  phi0 =(1/8) *(0.352+0.022*modelloOr[,'Ta'] - 0.00034*modelloOr[,'Ta']^(2) )      
} else {
  phi0 = phi0_method
}

# electron transport rate (Smith equation)
dfReplicate[,'J'] =  (4 *phi0*modelloOr[,'Iabs'])/sqrt(1 + (
  (4*phi0*modelloOr[,'Iabs'])/(dfReplicate[,'JmaxAdjusted']))^(2))

if ( df_settaggio$xi_acclimated == 'off' ) {
  AJ = (dfReplicate[,'J']/4)*(modelloOr[,'ciM'] - modelloOr[,'GammaStarM'])/(
    modelloOr[,'ciM'] + 2*modelloOr[,'GammaStarM'])    
} else {# acclimated AJ with the acclimated xiPa term
  AJ = (dfReplicate[,'J']/4)*(dfReplicate[,'ci'] -
                                modelloOr[,'GammaStarM'])/(dfReplicate[,'ci'] + 2*modelloOr[,'GammaStarM'])    #[micromol/m2s]
}

# Rename Ac, AJ and save variables in a df.
dfReplicate[,'Ac1Opt'] = Ac
dfReplicate[,'AJp1Opt'] = AJ
rm(Ac, AJ, phi0)

giuliaModelOutput = dfReplicate[,c(
  'Ac1Opt','AJp1Opt','caOpt','ci','ciOpt','CO2Opt',
  'GammaStarOpt','IabsOpt','J','JmaxOpt',
  'JmaxAdjusted','KOpt','TempOpt','vcmaxAdjusted',
  'vcmaxOpt','VPDOpt',
  'xiPa')]

# COMPUTE GPPp as minimum value between Ac and AJ ----
giuliaModelOutput$GPPpOpt = NA

cat(sprintf('Calculate GPPpOpt, please wait ....\n'))
for ( cy1 in seq(1,nrow(giuliaModelOutput)) ) {
  tmp = c(giuliaModelOutput[,'Ac1Opt'][cy1],giuliaModelOutput[,'AJp1Opt' ][cy1])
  posNa = which(is.na(tmp) == 1)
  if ( length(posNa) > 0 ) tmp = tmp[-1*posNa]
  rm(posNa)
  if (length(tmp) == 2 )
    giuliaModelOutput[,'GPPpOpt'][cy1] = min(tmp)
}
rm(cy1,tmp)

cat(sprintf('Calculate GPPpOpt .... OK\n'))

# CREATE and SAVE a data.frame with model outputs ----

dataOr = cbind(modelloOr,giuliaModelOutput)

# save all results
write.csv(x = dataOr, file = file_finale,row.names = F,quote = F)

cat(sprintf('\nwrite file:\n\t%s\n\n',file_finale))

# rm(datasetOr, giuliaModelOutput, dfGiuliaModelOutput)
# rm(modelloOr,giuliaModelOutput)
