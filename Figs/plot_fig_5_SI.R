# Script to generate FIGURE 5 ( in supplementary information)
rm(list = ls())

library(ggplot2)
library(cowplot)
dir_in = 'C:/Users/giuli/Desktop/Stat_IQR_v1/'

setwd(dir_in)

lf = list.files(path = dir_in,full.names = T,recursive = T,pattern = '.csv')


# Import files (obtained using the weighted mean approach)
df_stat = data.frame('site' = NA, 'alpha' = NA, 'th25' = NA, 'th50' = NA, 'th75' = NA, 'stat' = NA)
for (cyf in lf) {
  cat(sprintf('%s\n',cyf))
  
  df = read.csv(cyf)
  
  # RMSE
  tmp_rmse = data.frame('site' = unlist(strsplit(dirname(cyf),'/'))[7],    # 4
                        'alpha' = unlist(strsplit(basename(cyf),'_'))[7],
                        'th25' = unname(quantile(df$rmse,probs = 0.25,na.rm = T)),
                        'th50' = unname(quantile(df$rmse,probs = 0.50,na.rm = T)),
                        'th75' = unname(quantile(df$rmse,probs = 0.75,na.rm = T)))
  tmp_rmse$stat = 'rmse'
  
  # R squared
  tmp_r = data.frame('site' = unlist(strsplit(dirname(cyf),'/'))[7],    #4
                     'alpha' = unlist(strsplit(basename(cyf),'_'))[7],
                     'th25' = unname(quantile(df$r,probs = 0.25,na.rm = T)),
                     'th50' = unname(quantile(df$r,probs = 0.50,na.rm = T)),
                     'th75' = unname(quantile(df$r,probs = 0.75,na.rm = T)))
  tmp_r$stat = 'r'
  
  # Bias error
  tmp_BE = data.frame('site' = unlist(strsplit(dirname(cyf),'/'))[7],    #4
                      'alpha' = unlist(strsplit(basename(cyf),'_'))[7],
                      'th25' = unname(quantile(df$BE,probs = 0.25,na.rm = T)),
                      'th50' = unname(quantile(df$BE,probs = 0.50,na.rm = T)),
                      'th75' = unname(quantile(df$BE,probs = 0.75,na.rm = T)))
  tmp_BE$stat = 'BE'
  
  df_stat = rbind(df_stat, tmp_r, tmp_rmse, tmp_BE                                 
                  # data.frame('site' = unlist(strsplit(dirname(cyf),'/'))[7],    #4
                  
  )
  rm(df)
  cat(sprintf('%s ... ok\n',cyf))
}
rm(cyf)

df_stat = df_stat[-1,]

df_stat$alpha2 = NA
df_stat$alpha2[which(df_stat$alpha == '0.33.csv')] = '3 days'
df_stat$alpha2[which(df_stat$alpha == '0.143.csv')] = '7 days'
df_stat$alpha2[which(df_stat$alpha == '0.1.csv')] = '10 days'
df_stat$alpha2[which(df_stat$alpha == '0.067.csv')] = '15 days'
df_stat$alpha2[which(df_stat$alpha == '0.033.csv')] = '30 days'
df_stat$alpha2[which(df_stat$alpha == '0.022.csv')] = '45 days'
df_stat$alpha2[which(df_stat$alpha == '0.0167.csv')] = '60 days'

# order the elements on the y axis
df_stat$alpha2 = as.factor(df_stat$alpha2)
df_stat$alpha2 = factor(x = df_stat$alpha2,levels = c('3 days','7 days','10 days','15 days',
                                                      '30 days','45 days','60 days'))
df_stat$x_pos = NA
conta_x = 0

for (cy_d in c('3 days','7 days','10 days','15 days','30 days','45 days','60 days')) {
  conta_x = conta_x + 1
  #conta_s = - 2
  conta_s = 0
  for (cys in sort(unique(df_stat$site))) {
    df_stat$x_pos[which(df_stat$site == cys & df_stat$alpha2 == cy_d)] = conta_x + (conta_s/5)
    #conta_s = conta_s + 1
  }
  rm(cys,conta_s)
}

# Plot 
mp1 = ggplot(df_stat[df_stat$stat == 'rmse',]) +
  geom_line(aes(x = x_pos, y = th50,color=site),size = 1.5) +                  
  geom_point(aes(x = x_pos, y = th50,color=site),size = 3, shape=16) + 
  # geom_errorbar(aes(x = x_pos,ymin = th25, ymax = th75,color=site),width=0.2,size=1) +
  # geom_text(aes(x = x_pos,y = th50, label = sprintf('%.2f',th50)),color='black') +            
  theme_bw() + 
  theme(legend.position = 'top',legend.direction = 'horizontal',
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size=20),legend.text = element_text(size=20),
        axis.title.x = element_blank(),legend.title = element_blank()) +
  ylab('RMSE') +
  scale_x_continuous(breaks = seq(1,7),
                      labels = c('3 days','7 days','10 days','15 days','30 days','45 days','60 days'))

mp2 = ggplot(df_stat[df_stat$stat == 'r',]) +
  geom_line(aes(x = x_pos, y = th50,color=site),size = 1.5) +                  
  geom_point(aes(x = x_pos, y = th50,color=site),size = 3, shape=16) + 
  # geom_errorbar(aes(x = x_pos,ymin = th25, ymax = th75,color=site),width=0.2,size=1) +
  # geom_text(aes(x = x_pos,y = th50, label = sprintf('%.2f',th50)),color='black') +           
  theme_bw() + 
  theme(legend.position = 'none',legend.direction = 'horizontal',
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size=20),legend.text = element_text(size=20),
        axis.title.x = element_blank(),legend.title = element_blank()) +
  ylab('R squared') +
  scale_x_continuous(breaks = seq(1,7),
                      labels = c('3 days','7 days','10 days','15 days','30 days','45 days','60 days'))

mp3 = ggplot(df_stat[df_stat$stat == 'BE',]) +
  geom_line(aes(x = x_pos, y = th50,color=site),size = 1) +                 
  geom_point(aes(x = x_pos, y = th50,color=site),size = 3, shape=16) + 
  # geom_errorbar(aes(x = x_pos,ymin = th25, ymax = th75,color=site),width=0.2,size=1) +
  # geom_text(aes(x = x_pos,y = th50, label = sprintf('%.2f',th50)),color='black') +          
  theme_bw() + 
  theme(legend.position = 'none',legend.direction = 'horizontal',
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size=20),legend.text = element_text(size=20),
        axis.title.x = element_blank(),legend.title = element_blank()) +
  ylab('Bias') +
  scale_x_continuous(breaks = seq(1,7),
                      labels = c('3 days','7 days','10 days','15 days','30 days','45 days','60 days'))

mp4 = plot_grid(mp1,mp2,mp3, ncol = 1, align = 'v')

ggsave(plot = mp4,filename = 'Plot_Fig.5_SI.png',width = 13,height = 20,dpi = 600)

