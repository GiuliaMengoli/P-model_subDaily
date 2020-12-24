# MAIN SCRIPT FOR THE 'WEIGHTED MEAN' METHOD
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
source ('./R/memoryEffect_v1.R')        # applies the weighted mean method to obtain acclimated responses


# Settings files:
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/First_paper_v1/BE-Vie_settaggi_test_3a.csv'   # MODIS, 2014
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/First_paper/FI-Hyy_settaggi_test_3a.csv'      # MODIS, 2014 (iniz..1)
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/First_paper_v1/GF_Guy_settaggi_test_3b.csv'   # MODIS, several years, iniz=0
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/CH_Cha_settaggi_test_3c.csv'                  # MODIS, 2014, iniz..
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/First_paper_v1/CH_Cha_settaggi_test_3c.csv'   # MODIS, 2014, iniz=0
# file_settaggio = 'C:/pModelPlus/pModelPlus/data/First_paper_v1/US-UMB_settaggi_test_3c.csv'   # MODIS, 2014, iniz=0

file_settaggio = 'C:/Users/giuli/Desktop/Settings_files/BE-Vie_settaggi_test_3a.csv'   # MODIS, 2014

# NB: CHANGE ROW 53 FOR INITIALIZATION (CH & FI)

df_settaggio = importSetting(file_setting = file_settaggio,showMsg = T)

file_data = paste0(df_settaggio$dir_output,'data_input_',df_settaggio$site_name,
                   '#',basename(df_settaggio$file_setting))
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

# # ONLY FOR GF-Guy
# pos_time = which ( year (data$TIME) == 2014)   # 2014
# data = data[pos_time,]
# range(data$TIME)

# # shortcut
# if (file.exists(file_modelloOr)) {
#   modelloOr = read.csv(file_modelloOr)
#   data[,'TIME'] = ymd_hms(data[,'TIME'])
# } else {
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

# APPLY the dailyDownscaling function (according to setting file: noon conditions)----
dataDaily = dailyDownscaling(df = dataHhReduc,
                             nrWindow = df_settaggio$method_dd_win_size,
                             hourReferenceT = df_settaggio$method_dd_href,
                             approccio = df_settaggio$method_dd)

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

# # Dataframe creation for the VcmaxOpt and JmaxOpt computation----
# if (file.exists(file_modello_mmME)) {
#   modelloMmMemEff = read.csv(file_modello_mmME)
#   modelloMmMemEff[,'TIME'] = ymd_hm(modelloMmMemEff[,'TIMESTAMP_START'])
# } else {
dataDailyHh = modelloOr[1,]

for (cyr in seq(1,nrow(modelloMm))) {
  pos1 = which(
    modelloOr[,'YEAR'] == modelloMm[,'YEAR'][cyr] &
      modelloOr[,'MONTH'] == modelloMm[,'MONTH'][cyr] &
      modelloOr[,'DAY'] == modelloMm[,'DAY'][cyr] &
      modelloOr[,'HOUR'] == modelloMm[,'HOUR'][cyr] &
      modelloOr[,'MINUTE'] == modelloMm[,'MINUTE'][cyr]
  )
  dataDailyHh = rbind(dataDailyHh,
                      modelloOr[pos1,])
  rm(pos1)
}
rm(cyr)

dataDailyHh = dataDailyHh[-1,]

modelloMmMemEff = modelloMm[,c('TIMESTAMP_START',"YEAR","MONTH","DAY","HOUR","MINUTE")]

modelloMmMemEff[,'TempOpt'] = modelloMm[,'Ta']
modelloMmMemEff[,'KOpt'] = modelloMm[,'kmPa']
modelloMmMemEff[,'GammaStarOpt'] = modelloMm[,'GammaStarM']
modelloMmMemEff[,'viscosityOpt'] = modelloMm[,'viscosityH2oStar']
modelloMmMemEff[,'caOpt'] = modelloMm[,'ca']

modelloMmMemEff[,'xiOpt'] = modelloMm[,'xiPaM']
modelloMmMemEff[,'ciOpt'] = modelloMm[,'ciM']
modelloMmMemEff[,'VPDOpt'] = modelloMm[,'VPDPa']
modelloMmMemEff[,'CO2Opt'] = modelloMm[,'CO2']

modelloMmMemEff[,'IabsOpt'] = modelloMm[,'Iabs']
# if xiAcclimated is ON vcmaxOpt and JmaxOpt are overwritten
modelloMmMemEff[,'vcmaxOpt'] = modelloMm[,'vcmaxPmodelM1']
modelloMmMemEff[,'JmaxOpt'] = modelloMm[,'JmaxM1']


# Vcmax with acclimated 'xiPa' & 'phi0' (ON)
if (df_settaggio$xi_acclimated == 'off') {
  cat(sprintf('vcmaxOpt and JmaxOpt replicated\n'))
  modelloMmMemEff[,'ci'] = NA
  modelloMmMemEff[,'xiPa'] = NA
} else {
  
  cat(sprintf('vcmaxOpt and JmaxOpt calculated\n'))
  
  # Intrinsic quantum efficiency of photosynthesis (phi0)
  if (is.na(df_settaggio$phi0_method) ) {
    
    phi0 =(1/8) *(0.352+0.022*modelloMmMemEff[,'TempOpt'] -          # Temperature dependence function of phi0 (Bernacchi et al.,2003)
                    0.00034*modelloMmMemEff[,'TempOpt']^(2) )     
  } else {
    phi0 = phi0Method
  }
  
  c=0.41                        # cost factor for electron transport capacity
  
  # acclimated xiPa (parameter that determines the sensitivity of ci/ca to VPD)
  betha = 146
  
  modelloMmMemEff[,'xiPa'] = sqrt ( (betha * (modelloMmMemEff[,'KOpt'] +
                                                modelloMmMemEff[,'GammaStarOpt']) ) /
                                      (1.6 * modelloMmMemEff[,'viscosityOpt']) )                      # [Pa^1/2]
  
  # acclimated ci (with acclimated xiPa, and adjusted with the actual VPD)
  modelloMmMemEff[,'ci'] = ( modelloMmMemEff[,'xiPa'] * modelloMmMemEff[,'caOpt'] +
                               modelloMmMemEff[,'GammaStarOpt']*sqrt(dataDailyHh[,'VPDPa']))/(
                                 modelloMmMemEff[,'xiPa']+sqrt(dataDailyHh[,'VPDPa']))
  
  # OPTIMAL Vcmax
  modelloMmMemEff[,'vcmaxOpt']  = phi0 * modelloMmMemEff[,'IabsOpt'] *
    ((modelloMmMemEff[,'ci'] + modelloMmMemEff[,'KOpt']) / (modelloMmMemEff[,'ci'] +
                                                              2*modelloMmMemEff[,'GammaStarOpt'])) *
    sqrt (1-(c * (modelloMmMemEff[,'ci']+2*modelloMmMemEff[,'GammaStarOpt'])/(
      modelloMmMemEff[,'ci']-modelloMmMemEff[,'GammaStarOpt']))^(2/3))                                  # [micromol/m2s]
  
  # OPTIMAL Jmax
  modelloMmMemEff[,'JmaxOpt']  = ( 4 * phi0 * modelloMmMemEff[,'IabsOpt'] ) / sqrt (
    1 / ( 1 - ( c * ( modelloMmMemEff[,'ci'] + 2*modelloMmMemEff[,'GammaStarOpt'] ) /
                  ( modelloMmMemEff[,'ci'] - modelloMmMemEff[,'GammaStarOpt'] ) ) ^ (2.0/3.0) ) - 1 )   # [micromol/m2s]
}

modelloMmMemEff[,'tk'] = dataDailyHh[,'Ta'] + 273.15                                                    # [K]
rm(c,betha,phi0)

# APPLY the memoryEffect function----
modelloMmMemEff = memoryEffect(modelloMmMemEff,  dataDaily, alpha = 0.067,                    
                               formula2_memory_effect = df_settaggio$formula2_memory_effect,   
                               memory_all = df_settaggio$input_memory_effect,
                               initialization=df_settaggio$initialization_memory_effect)
# write.csv(x = modelloMmMemEff,file = file_modello_mmME,row.names = F,quote = F)
# rm(dataDailyHh)
# }
# to obtain acclimated Vcmax25 and Jmax25
modelloMm[,'JcmaxNew'] = modelloMmMemEff['JcmaxNew']
modelloMm[,'VcmaxNew'] = modelloMmMemEff['VcmaxNew']

modelloMm[,'Vcmax25'] = modelloMmMemEff['Vcmax25']
modelloMm[,'Jcmax25'] = modelloMmMemEff['Jcmax25']

modelloMm[,'vcmaxOpt'] = modelloMmMemEff[,'vcmaxOpt']
modelloMm[,'JmaxOpt'] = modelloMmMemEff[,'JmaxOpt']

modelloMm[,'ci'] = modelloMmMemEff[,'ci']
modelloMm[,'xiPa'] = modelloMmMemEff[,'xiPa']

#rm(modelloMmMemEff)

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

dfReplicate[,'Vcmax25'] = NA
dfReplicate[,'Jcmax25'] = NA

dfReplicate[,'vcmaxOpt'] = NA
dfReplicate[,'JmaxOpt'] = NA

dfReplicate[,'JcmaxNew'] = NA
dfReplicate[,'VcmaxNew'] = NA

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
  dfReplicate[,'xiOpt'][posValues] = modelloMm2[,'xiPa'][1]
  dfReplicate[,'ciOpt'][posValues] = modelloMm2[,'ci'][1]
  dfReplicate[,'VPDOpt'][posValues] = modelloMm2[,'VPDPa'][1]
  dfReplicate[,'CO2Opt'][posValues] = modelloMm2[,'CO2'][1]
  dfReplicate[,'caOpt'][posValues] = modelloMm2[,'ca'][1]
  dfReplicate[,'IabsOpt'][posValues] = modelloMm2[,'Iabs'][1]
  # if xiAcclimated is ON vcmaxOpt and JmaxOpt are overwritten
  dfReplicate[,'vcmaxOpt'][posValues] = modelloMm2[,'vcmaxOpt'][1]
  dfReplicate[,'JmaxOpt'][posValues] = modelloMm2[,'JmaxOpt'][1]
  
  dfReplicate[,'VcmaxNew'][posValues] = modelloMm2[,'VcmaxNew'][1]
  dfReplicate[,'JcmaxNew'][posValues] = modelloMm2[,'JcmaxNew'][1]
  
  dfReplicate[,'Vcmax25'][posValues] = modelloMm2[,'Vcmax25'][1]
  dfReplicate[,'Jcmax25'][posValues] = modelloMm2[,'Jcmax25'][1]
  
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


# INSTANTANEOUS Vcmax and Jmax----
# The Arrhenius equation constants:
Ha = 65330
Haj = 43900
Rgas = 8.314

dfReplicate[,'tk'] = modelloOr[,'Ta'] + 273.15   

# Update Vcmax25 and Jmax25 at noon
tk_noon = dfReplicate[,'tk'] + NA
tk_noon2 = dfReplicate[,'tk'] + NA
cyr = 1
pos_noon1 = which(dfReplicate$YEAR == dfReplicate$YEAR[cyr] &
                    dfReplicate$MONTH  == dfReplicate$MONTH[cyr] &
                    dfReplicate$DAY == dfReplicate$DAY[cyr] &
                    dfReplicate$MINUTE == 0 &
                    dfReplicate$HOUR == 12)
valore_noon = dfReplicate[pos_noon1, 'VcmaxNew']
valore_noon2 = dfReplicate[pos_noon1, 'JcmaxNew']

tk_noon[cyr] = valore_noon*exp((Ha/Rgas)*(1/298.15 - 1/dfReplicate[cyr,'tk']))  
tk_noon2[cyr] = valore_noon2*exp((Haj/Rgas)*(1/298.15 - 1/dfReplicate[cyr,'tk']))  


for(cyr in seq(2,nrow(dfReplicate))){
  if ( dfReplicate$MINUTE[cyr] == 0 & dfReplicate$HOUR[cyr] == 12)
    valore_noon = dfReplicate[cyr, 'VcmaxNew']
  if ( dfReplicate$MINUTE[cyr] == 0 & dfReplicate$HOUR[cyr] == 12)
    valore_noon2 = dfReplicate[cyr, 'JcmaxNew']
  
  tk_noon[cyr] = valore_noon*exp((Ha/Rgas)*(1/298.15 - 1/dfReplicate[cyr,'tk']))  
  tk_noon2[cyr] = valore_noon2*exp((Haj/Rgas)*(1/298.15 - 1/dfReplicate[cyr,'tk']))  
}

# because plants cannot see the future...
cyr = 1
pos_noon1 = which(dfReplicate$YEAR == dfReplicate$YEAR[cyr] &
                    dfReplicate$MONTH  == dfReplicate$MONTH[cyr] &
                    dfReplicate$DAY == dfReplicate$DAY[cyr] &
                    dfReplicate$MINUTE == 0 &
                    dfReplicate$HOUR == 12)
tk_noon[1:(pos_noon1 -1)] = NA
tk_noon2[1:(pos_noon1 -1)] = NA


dfReplicate[,'vcmaxAdjusted'] = tk_noon

dfReplicate[,'JmaxAdjusted'] = tk_noon2

rm(tk_noon, tk_noon2, cyr,valore_noon,valore_noon2)

# Arrhenius eq
# dfReplicate[,'vcmaxAdjusted'] = dfReplicate[,'VcmaxNew'] *
#   exp((Ha/Rgas)*(1/298.15 - 1/dfReplicate[,'tk']))  
#
# dfReplicate[,'JmaxAdjusted'] = dfReplicate[,'JcmaxNew'] *
#   exp((Haj/Rgas)*(1/298.15 - 1/dfReplicate[,'tk']))  

# write.csv(x = dfReplicate,row.names = F,
#           file = file_dfReplicate_mmME)
# }

# CALCULATE the assimilation rate: Ac-----
# acclimated 'xiPa'
betha = 146
dfReplicate[,'xiPa'] = sqrt ( (betha * (dfReplicate[,'KOpt'] +
                                          dfReplicate[,'GammaStarOpt']) ) /
                                (1.6 * dfReplicate[,'viscosityOpt']) )             # [Pa^1/2]

# acclimated ci (adjusted with the actual VPD)
dfReplicate[,'ci'] = ( dfReplicate[,'xiPa'] * dfReplicate[,'caOpt'] +
                         dfReplicate[,'GammaStarOpt']*sqrt(modelloOr[,'VPDPa']))/(
                           dfReplicate[,'xiPa'] + sqrt(modelloOr[,'VPDPa']))

# acclimated Ac with the acclimated xiPa term
Ac = dfReplicate[,'vcmaxAdjusted']*(
  dfReplicate[,'ci'] - modelloOr[,'GammaStarM']) /
  (dfReplicate[,'ci'] + modelloOr[,'kmPa'])                                         #[micromol/m2s]

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

if ( df_settaggio$xi_acclimated == 'off' ) {# without acclimated xi
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
  'xiPa','Vcmax25', 'Jcmax25')]

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
#rm(modelloOr,giuliaModelOutput)
