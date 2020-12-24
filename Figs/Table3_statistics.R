# Script for statistic analysis
# to generate a summary of model performance statistics
rm(list = ls())
# set directory
setwd('C:/giulia_model/')

# loding of packages
library(lubridate)
library(ggplot2)
library(cowplot)
library(scales)
library("hydroGOF")


# Table 3 with BE, bias error, and running mean approach
# 1) Import dataset
# dataOr = read.csv ('C:/Users/giuli/Desktop/Paper_fig/Fig_2/Fig_2_BE-Vie/dataOr_BE-Vie#BE-Vie_test_3a_2014.csv')
# dataOr = read.csv ('C:/Users/giuli/Desktop/Paper_fig/Fig_2/Fig_2_CH-Cha/dataOr_CH_Cha#CH_Cha_test_3a.csv')
# dataOr = read.csv ('C:/Users/giuli/Desktop/Paper_fig/Fig_2/Fig_2_FI-Hyy/dataOr_FI-Hyy#FI-Hyy_test_3d_2014.csv')
# dataOr = read.csv ('C:/Users/giuli/Desktop/Paper_fig/Fig_2/Fig_2_US-UMB/dataOr_US-UMB#US-UMB_test_3a.csv')
# dataOr = read.csv ('C:/Users/giuli/Desktop/Paper_fig/Fig_2/Fig_2_GF-Guy/dataOr_GF-Guy#GF_Guy_test_3a.csv')

dataOr$TIME = ymd_hms(as.character(dataOr$TIME))
range(dataOr$TIME)

# 2) apply the Growing season filter and save G.S. info----
# set the approach
growing_season_approach = 'vertical'   #or 'horizontal'

dataOr$grow = 0

AllYears <- unique(dataOr$YEAR)

# percentual threshold for the growing season (ref: Lasslop et al., 2012)
th_perc = 0.2

if (exists('growing_season_info')) rm(growing_season_info)

for (j in AllYears) {
  
  pos_year = which(dataOr$YEAR == j)
  dataOr_year = dataOr[pos_year,]
  
  
  top <- quantile(dataOr_year$GPP_DT_CUT_REF, 0.95, na.rm = T)
  toe <- quantile(dataOr_year$GPP_DT_CUT_REF, 0.05, na.rm = T)   # we then set this as our zero for the threshold
  
  grow = which( dataOr_year$GPP_DT_CUT_REF > (toe + (th_perc * (top - toe))) )
  
  if (growing_season_approach == 'vertical')
    dataOr_year$grow[seq(min(grow),max(grow))] = 1  
  
  if (growing_season_approach == 'horizontal')
    dataOr_year$grow[grow] = 1
  
  dataOr[pos_year,] = dataOr_year
  
  pos_growing_season = which(dataOr_year$grow == 1)
  
  if (exists('growing_season_info')) {
    growing_season_info = rbind(growing_season_info,
                                data.frame('approach' = growing_season_approach,
                                           'threshold' = (toe + (th_perc * (top - toe))),
                                           'YEAR' = j,
                                           'TIME_start' = dataOr_year$TIME[min(pos_growing_season)],
                                           'TIME_start_label' = paste0(c(day(dataOr_year$TIME[min(pos_growing_season)]),
                                                                         month.abb[month(dataOr_year$TIME[min(pos_growing_season)])]),collapse = '-'),
                                           'TIME_end'   = dataOr_year$TIME[max(pos_growing_season)],
                                           'TIME_end_label' = paste0(c(day(dataOr_year$TIME[max(pos_growing_season)]),
                                                                       month.abb[month(dataOr_year$TIME[max(pos_growing_season)])]),collapse = '-')
                                ))
  } else {
    growing_season_info = data.frame('approach' = growing_season_approach,
                                     'threshold' = (toe + (th_perc * (top - toe))),
                                     'YEAR' = j,
                                     'TIME_start' = dataOr_year$TIME[min(pos_growing_season)],
                                     'TIME_start_label' = paste0(c(day(dataOr_year$TIME[min(pos_growing_season)]),
                                                                   month.abb[month(dataOr_year$TIME[min(pos_growing_season)])]),collapse = '-'),
                                     'TIME_end'   = dataOr_year$TIME[max(pos_growing_season)],
                                     'TIME_end_label' = paste0(c(day(dataOr_year$TIME[max(pos_growing_season)]),
                                                                 month.abb[month(dataOr_year$TIME[max(pos_growing_season)])]),collapse = '-')
    )
  }
  rm(dataOr_year,pos_year)
  rm(grow,top,toe)
}
rm(j,AllYears, th_perc) 

# 3) apply 'filters' to remove data points flagged as medium or ----
#    poor-quality gap fill (ref:  Pastorello et al., 2020)
#    and to remove the no-growing season and NA 

# A) NA 
pos_na = which(is.na(dataOr$GPPpOpt) == 1)
if (length(pos_na) > 0 ) dataOr = dataOr[-1*pos_na,]
pos_na = which(is.na(dataOr$GPP_DT_CUT_REF) == 1)
if (length(pos_na) > 0 ) dataOr = dataOr[-1*pos_na,]
rm(pos_na)

# B) QC = 2 & QC = 3 e QC = NA (using NEE_CUT_REF_QC)
pos_na = which(dataOr$NEE_CUT_REF_QC == 2)
if (length(pos_na) > 0 ) dataOr = dataOr[-1*pos_na,]
pos_na = which(dataOr$NEE_CUT_REF_QC == 3)
if (length(pos_na) > 0 ) dataOr = dataOr[-1*pos_na,]
pos_na = which(is.na(dataOr$NEE_CUT_REF_QC) == 1)
if (length(pos_na) > 0 ) dataOr = dataOr[-1*pos_na,]
rm(pos_na)

# C) growing season
pos_grw = which(dataOr$grow == 0)
if (length(pos_grw) > 0) dataOr = dataOr[-1*pos_grw,]
rm(pos_grw)

# 4) Compute metrics over the growing season----

# Create a df with metrics
r_week = c()
rmse_week = c()
nrmse_week = c()
week = c()
nr_week = c()
BE_week = c()

for (cyweek in seq(min(dataOr$WEEK), max(dataOr$WEEK))) {
  
  righe_week = which(dataOr$WEEK == cyweek & is.na(dataOr$GPPpOpt) == 0  & is.na(dataOr$GPP_DT_CUT_REF) == 0  )
  if (length(righe_week) < 5)
    next
  
  GPPpOpt = dataOr$GPPpOpt[righe_week]
  GPP_DT_CUT_REF = dataOr$GPP_DT_CUT_REF[righe_week]
  
  rmse_week = c(rmse_week,  rmse(GPPpOpt, GPP_DT_CUT_REF))
  nrmse_week = c(nrmse_week, nrmse(GPPpOpt, GPP_DT_CUT_REF, norm = "sd")  )
  r_week = c(r_week, unname(cor.test(GPPpOpt, GPP_DT_CUT_REF)$estimate) )    
  # BE_week = c(BE_week, mean (GPPpOpt - GPP_DT_CUT_REF))
  BE_week = c(BE_week, (sum (GPPpOpt - GPP_DT_CUT_REF))/length(righe_week))  # ref: Moffat et al., 2007
  week = c(week, cyweek )
  
  nr_week = c(nr_week,length(righe_week))
  
}
r_week = r_week*r_week            

df_stat_week1 = data.frame(nr_week ,week,rmse_week,nrmse_week, r_week, BE_week)

rm (cyweek, nr_week, nrmse_week,r_week, righe_week, rmse_week, week, BE_week)
rm (GPP_DT_CUT_REF, GPPpOpt)

# Calculate median RMSE, R2 and BE ----

rmse <- df_stat_week1$rmse_week    
r <- df_stat_week1$r_week          # R squared
nr_week <- df_stat_week1$nr_week
week <- df_stat_week1$week
BE <- df_stat_week1$BE_week

year <- rep (2014, times = 52)                # number of weeks: BE-Vie & GF-Guy: 52; USA: 37; FI: 38; CH: 52

# # weighted mean approach
# alpha <- rep (0.0167, times =52)            # alpha: 0.33, 0.143, 0.1, 0.067, 0.033, 0.022, 0.0167
# df <- data.frame(year,alpha, week, nr_week, rmse, r, BE )
# rm(rmse,r,nr_week,week,year, BE)

# running  mean approach
days <- rep (15, times =52)                   # Days: 3, 7, 10, 15, 30, 45, 60
df <- data.frame(year,days, week, nr_week, rmse, r, BE )
rm(rmse,r,nr_week,week,days, BE)

# MEDIAN values
rmse <- df$rmse
r <- df$r
BE <- df$BE

median (rmse)
median (r)
median(BE)
# check metrics
# sort(rmse)
# sort(r)

# save dataframe with stat (each site and approach separately)
# df_stat_BE <- df
# write.csv (df_stat_BE, 'C:/Users/giuli/Desktop/Paper_fig/Table_3_run_m_BE/df_stat_BE_2014_GS_QC_15d.csv', row.names = F,quote = F)

# Examination of the RMSE distribution to determine a threshold----

setwd('C:/pModelPlus/pModelPlus')
source ('./R/plot_hist.R')         # Histograms

# BE-vie site example
# file_name = 'C:/Users/giuli/Desktop/Paper_fig/Table_3_run_m_BE/df_stat_BE_2014_GS_QC_15d.csv'

df = read.csv(file_name,dec = '.')
df = read.csv(file_name,dec = '.', sep=',')   

plot_ok = plot_hist(var_values = df$rmse,bin_width = 0.5,var_name = 'rmse')

plot_ok$mp1
plot_ok$df

ggplot(df) +
  geom_text(aes(x=rmse,y=r,label=week),size=4)

# estimation of 'good' weeks according to a threshold (based on the median RMSE distribution of each site)----
pos_TH_qc = which((df$rmse) < 4.56)     # BE-Vie: 4.56; FI-Hyy:5.06 ; GF-Guy: 7.34; US-UMB: 7.08; CH-Cha: 7.78
df_TH = df[pos_TH_qc,]


