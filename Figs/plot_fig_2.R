# Script to generate FIGURE 2
rm(list = ls())

library(ggplot2)
library(lubridate)
library(cowplot)

Sys.setlocale("LC_TIME", "English")

dir_in = 'C:/Users/giuli/Desktop/Paper_fig/'

setwd(dir_in)
# Load the three datasets with the P model outputs according to the 3 approaches, for one site
# BE-Vie
df_set_gen = rbind(
  data.frame('nome_file' = 'dataOr_BE-Vie#BE-Vie_test_3a_2014.csv','approccio' = 'NOON'),
  data.frame('nome_file' = 'dataOr_BE-Vie#BE-Vie_test_3H_2014.csv','approccio' =  '3HOUR'), 
  data.frame('nome_file' = 'dataOr_BE-Vie#BE-Vie_test_3b_2014.csv','approccio' = 'DAILY')
)
# FI-Hyy
# df_set_gen = rbind(
#   data.frame('nome_file' = 'dataOr_FI-Hyy#FI-Hyy_test_3d_2014.csv','approccio' = 'NOON'),
#   data.frame('nome_file' = 'dataOr_FI-Hyy#FI-Hyy_test_3H_2014.csv','approccio' =  '3HOUR'), 
#   data.frame('nome_file' = 'dataOr_FI-Hyy#FI-Hyy_test_3e_2014.csv','approccio' = 'DAILY')
# )
# GF-Guy
# df_set_gen = rbind(
#   data.frame('nome_file' = 'dataOr_GF-Guy#GF_Guy_test_3a.csv','approccio' = 'NOON'),
#   data.frame('nome_file' = 'dataOr_GF-Guy#GF_Guy_test_3H_2014.csv','approccio' =  '3HOUR'), 
#   data.frame('nome_file' = 'dataOr_GF-Guy#GF_Guy_test_3b.csv','approccio' = 'DAILY')
# )
# US-UMB
# df_set_gen = rbind(
#   data.frame('nome_file' = 'dataOr_US-UMB#US-UMB_test_3a.csv','approccio' = 'NOON'),
#   data.frame('nome_file' = 'dataOr_US-UMB#US-UMB_test_3H.csv','approccio' =  '3HOUR'),
#   data.frame('nome_file' = 'dataOr_US-UMB#US-UMB_test_3b.csv','approccio' = 'DAILY')
# )
# CH-Cha
# df_set_gen = rbind(
#   data.frame('nome_file' = 'dataOr_CH_Cha#CH_Cha_test_3a.csv','approccio' = 'NOON'),
#   data.frame('nome_file' = 'dataOr_CH_Cha#CH_Cha_test_3H.csv','approccio' =  '3HOUR'), 
#   data.frame('nome_file' = 'dataOr_CH_Cha#CH_Cha_test_3b.csv','approccio' = 'DAILY')
# )

df_set_gen[,1] = as.character(df_set_gen[,1])
df_set_gen[,2] = as.character(df_set_gen[,2])

# file importation, and select a given period of time
if (exists('data_or_season')) rm(data_or_season)

for (filename in df_set_gen$nome_file) {
  # import file created
  data_or = read.csv(paste0(dir_in,filename))
  
  # time problem after the importation of csv file
  data_or$TIME = ymd_hms(as.character(data_or$TIME))
  
  # week column
  data_or$WEEK = week(data_or$TIME)
  
  #*****************************************
  # # year 2014, week 24
  pos_day = c( which(data_or$YEAR == 2014 & data_or$WEEK == 34 ))
  data_or_season1 = data_or[pos_day,]
  
  colnames(data_or_season1) = gsub('VPDPa','VPD_Pa',colnames(data_or_season1))
  colnames(data_or_season1) = gsub('Opt','_opt',colnames(data_or_season1))
  colnames(data_or_season1) = gsub('Ac_1_opt',"Ac1_opt",colnames(data_or_season1))
  
  # columns extraction
  data_or_season1 = data_or_season1[,c('TIME','WEEK','YEAR','Iabs','Iabs_opt','Ta','Temp_opt',
       'VPD_Pa','VPD_opt','AJp1_opt','Ac1_opt','GPP_DT_CUT_REF','GPPp_opt','NEE_CUT_REF_QC',
       'GPP_DT_CUT_95','GPP_DT_CUT_05')]
  
  # add the approach columns (i.e. approccio)
  data_or_season1$method = df_set_gen$approccio[which(df_set_gen$nome_file == filename)]
  
  if (exists('data_or_season')) {
    data_or_season = rbind(data_or_season,data_or_season1)
  } else {
    data_or_season = data_or_season1
  }
  rm(pos_day,data_or_season1,data_or)
}
rm(filename)

df_plot_Iabs = data_or_season[,c('TIME','Iabs','Iabs_opt','method')]
df_plot_Ta = data_or_season[,c('TIME','Ta','Temp_opt','method')]
df_plot_VPD = data_or_season[,c('TIME','VPD_Pa','VPD_opt','method')]
df_plot_Ac_AJ = data_or_season[,c('TIME','AJp1_opt','Ac1_opt','method')]
df_plot_GPP = data_or_season[,c('TIME','GPP_DT_CUT_REF','GPPp_opt','method')]

# creation of one plot, df_plot
# standardize headings
colnames(df_plot_Iabs) = c('Time','Orig','Opt','method')
colnames(df_plot_Ta) = c('Time','Orig','Opt','method')
colnames(df_plot_VPD) = c('Time','Orig','Opt','method')
colnames(df_plot_Ac_AJ) = c('Time','Orig','Opt','method')
colnames(df_plot_GPP) = c('Time','Orig','Opt','method')

# add something to distinguish datasets
df_plot_Iabs$var_group = 'Iabs'
df_plot_Ta$var_group = 'Ta'
df_plot_VPD$var_group = 'VPD'
df_plot_Ac_AJ$var_group = 'Ac_AJ'
df_plot_GPP$var_group = 'GPP'
  
# add GPP uncertanity columns
df_plot_Iabs$y_max = NA
df_plot_Iabs$y_min = NA

df_plot_Ta$y_max = NA
df_plot_Ta$y_min = NA

df_plot_VPD$y_max = NA
df_plot_VPD$y_min = NA

df_plot_Ac_AJ$y_max = NA
df_plot_Ac_AJ$y_min = NA

df_plot_GPP$y_max = data_or_season$GPP_DT_CUT_95
df_plot_GPP$y_min = data_or_season$GPP_DT_CUT_05

# datasets concatenation
df_plot = rbind(df_plot_Iabs,df_plot_Ta,df_plot_VPD, df_plot_Ac_AJ,   
                df_plot_GPP)
rm(df_plot_Iabs,df_plot_Ta,df_plot_VPD,df_plot_Ac_AJ)         
rm(df_plot_GPP)
  
df_plot$var_group = as.factor(df_plot$var_group)
df_plot$var_group = factor(df_plot$var_group,levels = c("Iabs","Ta","VPD","Ac_AJ","GPP"))  
levels(df_plot$var_group)

# models inputs plot
cym = sort(unique(df_plot$method))[1]
conta_lettere = 0
lista_plot = list()
for (cyv in c('Iabs','Ta','VPD')) {
  righe = which(df_plot$method == cym & df_plot$var_group == cyv)
  
  df_plot2 = df_plot[righe,]
  for (col in colnames(df_plot2))
    if (is.factor(df_plot2[,col]))
      df_plot2[,col] = droplevels(df_plot2[,col])
  rm(col)

  conta_lettere = conta_lettere + 1
  df_plot_letters = data.frame('var_group' = cyv,'xv' = min(df_plot2$Time),'yv' = Inf,   
                               'lettera' = sprintf('(%s)',LETTERS[conta_lettere]))
  t_margin = 0
  if (conta_lettere == 1) t_margin = 0.5
  mpt = ggplot(df_plot2) +
    geom_line(aes(x = Time, y = Orig,color = 'Orig'), size = 1) +   #0.9, 0.7
    geom_text(data = df_plot_letters,aes(x = xv, y = yv, label = lettera, fontface =2),vjust=1.5,size=9) +  
    theme_bw() + theme(legend.title = element_blank(),
                     axis.text = element_text(size = 25),
                     strip.placement = 'outside',
                     strip.background = element_blank(),
                     strip.text = element_text(size = 25),
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank(),
                     legend.position = 'None',          
                     legend.text=element_text(size=25),
                     plot.margin = margin(t_margin,0,0,0,'lines'),
                     panel.spacing = unit(0, "lines")) +
    scale_color_manual(values = c('Orig' = 'darkcyan','Opt' = 'darkgray', 'Ac' = 'darkorchid2','AJ' = 'green3',  'GPPp' = 'red', 'GPPo' = 'gray33' )) + 
  facet_wrap(~ var_group,
             scales = 'free_y',
             ncol = 1,strip.position = 'left')             
  
  # y_tick1 = ggplot_build(mpt)$layout$panel_params[[1]]$y$breaks
  # y_tick1 = y_tick1[-1*seq(1,length(y_tick1),2)]
  # 
  # mpt = mpt +
  #   scale_y_continuous(breaks = y_tick1)
  
  # Differt sites (each site separately)
  # BE-Vie
  if (cyv == 'Iabs')
    mpt = mpt + scale_y_continuous(breaks = c(250,750,1250))    
  if (cyv == 'Ta')
    mpt = mpt + scale_y_continuous(breaks = c(8,12,16))
  if (cyv == 'VPD')
    mpt = mpt + scale_y_continuous(breaks = c(0,400,800))

  if (cyv != 'VPD') {
    mpt = mpt +
      theme(axis.text.x = element_blank() )
  
  # FI-Hyy
  # if (cyv == 'Iabs')
  #   mpt = mpt + scale_y_continuous(breaks = c(250,600,950))    
  # if (cyv == 'Ta')
  #   mpt = mpt + scale_y_continuous(breaks = c(8,12,16))
  # if (cyv == 'VPD')
  #   mpt = mpt + scale_y_continuous(breaks = c(0,400,800))
  # 
  # if (cyv != 'VPD') {
  #   mpt = mpt +
  #     theme(axis.text.x = element_blank() )
  
  # # GF-Guy
  # if (cyv == 'Iabs')
  #   mpt = mpt + scale_y_continuous(breaks = c(500,1000,1500,2000))    
  # if (cyv == 'Ta')
  #   mpt = mpt + scale_y_continuous(breaks = c(24,27,30))
  # if (cyv == 'VPD')
  #   mpt = mpt + scale_y_continuous(breaks = c(0,500,1000,1500))
  # 
  # if (cyv != 'VPD') {
  #   mpt = mpt +
  #     theme(axis.text.x = element_blank() )
  
  # US-UMB
  # if (cyv == 'Iabs')
  #   mpt = mpt + scale_y_continuous(breaks = c(500,1000,1500))    
  # if (cyv == 'Ta')
  #   mpt = mpt + scale_y_continuous(breaks = c(10,15,20,25))
  # if (cyv == 'VPD')
  #   mpt = mpt + scale_y_continuous(breaks = c(0,500,1000))
  # 
  # if (cyv != 'VPD') {
  #   mpt = mpt +
  #     theme(axis.text.x = element_blank() )
  
  # CH-Cha
  # if (cyv == 'Iabs')
  #   mpt = mpt + scale_y_continuous(breaks = c(500,1000,1500))    
  # if (cyv == 'Ta')
  #   mpt = mpt + scale_y_continuous(breaks = c(10,15,20))
  # if (cyv == 'VPD')
  #   mpt = mpt + scale_y_continuous(breaks = c(0,500,1000))
  # 
  # if (cyv != 'VPD') {
  #   mpt = mpt +
  #     theme(axis.text.x = element_blank() )
   } 
   
  lista_plot[[length(lista_plot)+1]] = mpt
}
# plot_grid(plotlist = lista_plot,ncol = 1,align = 'v')

# Apporaches plot
for (cym in c("DAILY","3HOUR","NOON")) {     
  if (exists('df_plot3')) rm(df_plot3)    
  
  for (cyv in c("Ac_AJ","GPP")) {           
    righe = which(df_plot$method == cym & df_plot$var_group == cyv)
    
    df_plot2 = df_plot[righe,]
    for (col in colnames(df_plot2))
      if (is.factor(df_plot2[,col]))
        df_plot2[,col] = droplevels(df_plot2[,col])
    rm(col)
    if (cyv == "Ac_AJ") {
      df_plot2$color1 = 'AJ'
      df_plot2$color2 = 'Ac'
    }
    if (cyv == "GPP") {
      df_plot2$color1 = 'GPPo'
      df_plot2$color2 = 'GPPp'
    }
    
    conta_lettere = conta_lettere + 1    
    if (exists('df_plot3')) {
      df_plot_letters = rbind(df_plot_letters,
          data.frame('var_group' = cyv,'xv' = max(df_plot2$Time),'yv' = Inf,
               'lettera' = sprintf('(%s)',letters[conta_lettere])))           
      df_plot_letters2 = rbind(df_plot_letters2,
           data.frame('var_group' = cyv,'xv' = min(df_plot2$Time),'yv' = Inf,
                  'lettera' = sprintf('%s',cym)) )
      df_plot3 = rbind(df_plot3,df_plot2)
    } else {
      df_plot_letters = data.frame('var_group' = cyv,'xv' = max(df_plot2$Time),'yv' = Inf,
                                 'lettera' = sprintf('(%s)',letters[conta_lettere]))
      df_plot_letters2 = data.frame('var_group' = cyv,'xv' = min(df_plot2$Time),'yv' = Inf,
                                   'lettera' = sprintf('%s',cym))                            
      df_plot3 = df_plot2
    }
  }
  
  mpt = ggplot(df_plot3) +
    geom_ribbon(aes(x = Time, ymax = y_max,ymin = y_min),color = 'grey',alpha = 0.3) +
    geom_line(aes(x = Time, y = Orig,color = color1), size = 1) +   #0.9, 0.7
    geom_line(aes(x = Time, y = Opt,color = color2), size = 1) +   #0.9, 0.7
    # geom_text(data = df_plot_letters,aes(x = xv, y = yv, label = lettera),vjust=1.2,size=12) +
    geom_text(data = df_plot_letters2,aes(x = xv, y = yv, label = lettera, fontface =2),vjust=1.5,size=9) +    
    theme_bw() + theme(legend.title = element_blank(),
                       axis.text = element_text(size = 25),  
                       strip.placement = 'outside',          
                       strip.background = element_blank(),
                       strip.text = element_text (size = 25), 
                       axis.title.y = element_blank(),
                       axis.title.x = element_blank(),    
                       legend.position = 'None',
                       legend.text=element_text(size=25, face= "bold"),
                       plot.margin = margin(1,1,1,1,'lines'), 
                       panel.spacing = unit(0, "lines")) +      
    scale_color_manual(values = c('Orig' = 'darkcyan','Opt' = 'darkgray', 'Ac' = 'darkorchid2','AJ' = 'green3',  'GPPp' = 'red', 'GPPo' = 'gray33' )) + 
    facet_wrap(~ var_group,
               scales = 'free_y',
               ncol = 1,strip.position = 'left')     
  
    #  # BE-Vie***
    if (cyv == "Ac_AJ") {                                         
      mpt = mpt +                                                  
        scale_y_continuous(breaks = seq(0,25,5),limits = c(0,25))  
    } else {
      mpt = mpt +
        scale_y_continuous(breaks = seq(5,30,10),limits = c(0,30))
    }
    if (cym == "NOON") {                                
      mpt = mpt + theme(legend.position = 'bottom')    
    } else {                                            
      # mpt = mpt + theme(axis.text.x = element_blank())  
    }
    # 
  #   # FI-Hyy
  # if (cyv == "Ac_AJ") {                                        
  #   mpt = mpt +                                                 
  #     scale_y_continuous(breaks = seq(0,20,5),limits = c(0,20))  
  # } else {
  #   mpt = mpt +
  #     scale_y_continuous(breaks = seq(5,25,10),limits = c(0,25))
  # }
  # if (cym == "NOON") {                                
  #   mpt = mpt + theme(legend.position = 'bottom')     
  # } else {                                            
  #   # mpt = mpt + theme(axis.text.x = element_blank())  
  # }
  
  #  # GF-Guy***
  # if (cyv == "Ac_AJ") {                                        
  #   mpt = mpt +                                                  
  #     scale_y_continuous(breaks = seq(0,35,5),limits = c(0,35))  
  # } else {
  #   mpt = mpt +
  #     scale_y_continuous(breaks = seq(5,40,10),limits = c(0,40))
  # }
  # if (cym == "NOON") {                                
  #   mpt = mpt + theme(legend.position = 'bottom')     
  # } else {                                            
  #   # mpt = mpt + theme(axis.text.x = element_blank())  
  # }
  
  # US-UMB
  # if (cyv == "Ac_AJ") {                                         
  #   mpt = mpt +                                                  
  #     scale_y_continuous(breaks = seq(0,35,5),limits = c(0,35)) 
  # } else {
  #   mpt = mpt +
  #     scale_y_continuous(breaks = seq(5,40,10),limits = c(0,40))
  # }
  # if (cym == "NOON") {                                
  #   mpt = mpt + theme(legend.position = 'bottom')     
  # } else {                                           
  #   # mpt = mpt + theme(axis.text.x = element_blank())  
  # }
  
  # CH-Cha
  # if (cyv == "Ac_AJ") {                                         
  #   mpt = mpt +                                                  
  #     scale_y_continuous(breaks = seq(0,45,5),limits = c(0,45))  
  # } else {
  #   mpt = mpt +
  #     scale_y_continuous(breaks = seq(5,45,10),limits = c(0,45))
  # }
  # if (cym == "NOON") {                                
  #   mpt = mpt + theme(legend.position = 'bottom')    
  # } else {                                            
  #   # mpt = mpt + theme(axis.text.x = element_blank()) 
  # }
  
  # Plot list
    lista_plot[[length(lista_plot)+1]] = mpt   
}

mp_all = plot_grid(plotlist = lista_plot,ncol = 1,align = 'v',rel_heights = c(1,1,1,2,2,2.5))  

ggsave(filename = 'plot_BE-Vie_Fig.2.jpg',plot = mp_all,height = 20,width = 20,dpi = 600)

   