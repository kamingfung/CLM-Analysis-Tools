### ============= testing NH3 data from DNLM ============= 
# loading libraries
# pkg.list = c("ncdf4", "ggplot2", "reshape2", "abind", "scales", "RColorBrewer", "ggthemes")
library(ncdf4)            # for reading nc files
library(scales)           # for prettier scalebar in the plots
library(ggplot2)          # for pretty plots
library(reshape2)         # for reshaping data.frame for plotting data
library(abind)            # for n-deminsional matrix merging
library(RColorBrewer)     # for prettier color in plots
library(ggthemes)         # for prettier themes in plots
library(ggpubr)           # for arranging mulitple plots
library(dplyr)            # for data tidying
library(viridis)          # for viridis color option

### ============= Specifying parameters ============= 
# specify data directory
data.dir = "_RDS/"

# specify output directory
output.dir = "_png/"

# specific whether to plot the mean or total
plot.stats = "Annual Mean"
# plot.stats = "Annual Total"

# name of cases to be plotted [vector of strings]
plot.case.name = c("test30", "test32")

# name of variables to be plotted [vector of strings]
plot.var.id = c("TOTECOSYSC", "TOTECOSYSN")

# unit conversion
var.unit = expression("Pg")
unit.convertor = 10^-15 # unit conversation coefficient after multipled by area
cal.grid.total = TRUE
per.second.to.per.month = FALSE

days_in_month = c(31, 28, 31, 30, 31, 30, 
                  31, 31, 30, 31, 30, 31)

### ============= Collecting the plot data =============

# initilizing data.frame for storing plot data
plot.data = NULL

# reading RDS
for (case.name in plot.case.name) {
  
  for (vid in plot.var.id) {
    
    # reading the RDS file
    file.name = list.files(path = paste(data.dir, sep = "/"), 
                           pattern = paste0(case.name, ".",vid,"\\.",".*\\.RDS"), 
                           full.names = T)
    
    # extracting variable
    new.data = readRDS(file.name)  # initial unit
    
    # if calculating grid total
    if (cal.grid.total) new.data$value = new.data$value * new.data$land.area
    
    # if convert flux variable to state
    if (per.second.to.per.month) new.data$value = new$value * 60*60*24*days_in_month[new.data$month]
    
    # converting unit
    new.data$value = new.data$value * unit.convertor # converted unit
    
    # extracting data to plot
    if (plot.stats == "Annual Mean") {
      plot.data = rbind(plot.data, 
                        new.data %>%
                          group_by(year, case.name, var.id, month) %>%
                          summarize(global.total = sum(value, na.rm = T)) %>%
                          group_by(year, case.name, var.id) %>%
                          summarize(global.total = mean(global.total, na.rm = T)) 
      )
      
    } else if (plot.stats == "Annual Total") {
      plot.data = rbind(plot.data,
                        new.data %>%
                          group_by(year, case.name, var.id, month) %>%
                          summarize(global.total = sum(value, na.rm = T)) %>%
                          group_by(year, case.name, var.id) %>%
                          summarize(global.total = sum(global.total, na.rm = T)) 
      )
      
    }
    
    # removing new.data to release memory
    rm(new.data)
    
  } # end of vid loop
  
} # end of case loop


### ============= Plotting the data =============


# plotting the metric variables
g = ggplot(data = plot.data, mapping = aes(x = year, y = global.total))

# using line plots
g = g + geom_line(aes(color = case.name))

# separating N and C plots
g = g + facet_wrap(~var.id, scales = "free_y")

# formating x-axis label
g = g + scale_x_continuous(labels = function(x) sprintf("%0.00f", x))

# giving the plot titles
g = g + labs(titles = paste(case.name, collapse = " & "),
             substitle = plot.stats,
             x = "Year",
             y = var.unit)

# applying some styles to the plot, e.g. bolding fonts, aligning and omitting useless labels
g = g + theme(plot.title = element_text(face = "bold", hjust = 0.5),
              plot.subtitle = element_text(hjust = 1),
              legend.position = "top")

# showing the plot
g

ggsave(path = output.dir,
       filename = paste(paste(plot.case.name, collapse = "."),
                        paste(plot.var.id, collapse = "."), 
                        plot.stats, "png", sep = "."), 
       width = 10, height = 6,
       limitsize = FALSE, dpi = 600)

