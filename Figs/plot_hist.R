# Function to generate histogrms
plot_hist <- function(var_values = NA, bin_width = NA, var_name = NA) {
  # INPUT
  #   var_values: vector with original data
  #   bin_width: classes sizes
  #   var_name: variable name
  # OUTPUT
  #   list('mp1': plot,
  #        'df': data.frame 
  
  library(ggplot2)
  
  # df = read.csv2(file_name,dec = '.')

  # classes sizes cration
  var_bin = seq(min(var_values),max(var_values),by = bin_width)
  
  # number of weeks for a given class 
  df_per_plot = data.frame('nr_values_per_class' = NA, 'x_laby' = NA)
  
  for (cy_var_bin in seq(1, (length(var_bin)-1) ) ) {
    # RMSE
    pos_class = c(which(var_values > var_bin[cy_var_bin] & 
                        var_values < var_bin[cy_var_bin+1]),
                  which(var_values == var_bin[cy_var_bin]))
    
    df_per_plot = rbind(df_per_plot, data.frame('nr_values_per_class' = length(pos_class),
       'x_laby' = paste( round(var_bin[cy_var_bin],2),
                        "\u2264 ",var_name," <", round(var_bin[cy_var_bin+1],2) )
    ))
    rm(pos_class)
  }
  df_per_plot = df_per_plot[-1,]
  
  pos_class = which(var_values > var_bin[cy_var_bin+1])
  
  df_per_plot = rbind(df_per_plot, data.frame('nr_values_per_class' = length(pos_class),
     'x_laby' = paste(var_name," \u2265 ", round(var_bin[cy_var_bin+1],2) )
  ))
  
  x_laby = df_per_plot$x_laby
  df_per_plot$x_laby = as.factor(df_per_plot$x_laby)
  df_per_plot$x_laby = factor(df_per_plot$x_laby,levels = x_laby)
  
  mp1 = ggplot(df_per_plot) + 
    geom_bar(aes(x = x_laby,y = nr_values_per_class),stat="identity",position="dodge") +
    geom_text(aes(x = x_laby,y = nr_values_per_class,label=nr_values_per_class),vjust=-0.5) +
    theme(axis.text.x = element_text(angle = -90,vjust = 0.25),
          axis.title.x = element_blank())
  
  return(list('mp1' = mp1,'df'=df_per_plot))
}
