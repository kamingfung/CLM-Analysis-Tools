### ============ Loading libraries ============
library(scales)           # for prettier scalebar in the plots
library(ggplot2)          # for pretty plots
library(reshape2)         # for reshaping data.frame for plotting data
library(RColorBrewer)     # for prettier color in plots
library(ggpubr)           # for arranging mulitple plots
library(dplyr)            # for data manipulation
library(tidyr)            # for tidying up data

### ============= Specifying parameters ============= 
# specify data directory
data.dir = "_data/"

# specify output directory
output.dir = "_png/"

# range of years of data to be plotted [vector of integers]
plot.year = 2007:2008

# specific what average to be plotted [monthly; seasonal and annual; annaul]
# plot.period = "monthly"
# plot.period = "seasonal"
plot.period = "annual"

# specific type of plots [uncomments only one of the following lines]
# plot.type = "single.plot"         # 1 case, 1 variable
plot.type = "difference"          # 2 cases, 2 variables

# name of cases to be plotted [vector of strings]
plot.case.name = c("test30", "test32")

# name of variables to be plotted [vector of strings]
plot.var.id = c("TOTECOSYSC", "TOTECOSYSC")

# range of map to show
plot.lon = c(-180, 180)
plot.lat = c(-60, 90)

# mass conversion from [g] to ...
var.unit = "Pg"
mass.convertor = 1e-15 # unit conversation coefficient

area.unit = "grid" # or "m^-2"

### ============= Preprocessing =============
# adding area unit
if (area.unit == "m^2") {
  plot.unit = bquote(.(var.unit)~"m"^-2)
  
} else if (area.unit == "grid") {
  plot.unit = bquote(.(var.unit)~"grid"^-1)
}

### ============= Collecting the plot data =============

# number of plot to produce
if (plot.type == "single.plot") {
  nplot = 1 
}else{    
  nplot = 3 
}

# initilizing data.frame for storing plot data
plot.data = NULL

# reading nc files and extracting them to plot.data
for (i in 1:min(2, nplot)) {
  
  # reading the nc file
  file.name = list.files(path = paste(data.dir, plot.case.name[i], sep = "/"),
                         pattern = paste0(plot.case.name[i],".", plot.var.id[i],"\\.",".*\\.RDS"), 
                         full.names = T)
  
  # extracting variable of the years interested
  new.data = readRDS(file.name) %>%
    filter(year >= min(plot.year) & year <= max(plot.year))
  
  # converting default m^-2 to ...
  if (area.unit == "m^2") {
    area.convertor = 1
    
  } else if (area.unit == "grid") {
    area.convertor = new.data$land.area # m^-2 -> grid total
    
  }
  
  # calculating total value from a grid
  new.data$value = new.data$value * area.convertor * mass.convertor # converted unit
  
  # adding infos to data frame
  new.data$case.name = plot.case.name[i]
  new.data$var.id = plot.var.id[i]
  new.data$plot.i = i
  plot.data = rbind(plot.data, new.data)
  rm(new.data)
  
}

# adding plot information to new columns
# period for averaging
plot.data$period = NA

if (plot.period == "monthly"){
  plot.data$period = sprintf("%02d", plot.data$month)
  
} else if (plot.period == "seasonal"){
  month_to_season = c("DJF", "DJF", "MAM",
                      "MAM", "MAM", "JJA",
                      "JJA", "JJA", "SON",
                      "SON", "SON", "DJF")
  plot.data$period = month_to_season[plot.data$month]
  
} else if (plot.period == "annual") {
  plot.data$period = "annual"
  
}

#==============================================================#
###### ===== Plotting the graphs ====== 
for (yyyy in plot.year) {
  for (avg.period in unique(plot.data$period)) {
    
    gg = NULL
    
    # looping over each plot
    for (i in 1:nplot) {
      
      # only extracting useful data from the data set loaded
      sub.data = subset(plot.data, 
                        lon >= plot.lon[1] & lon <= plot.lon[2] & 
                          lat >= plot.lat[1] & lat <= plot.lat[2] &
                          year == yyyy &
                          period == avg.period)
      
      # calculating annual total
      sub.data = sub.data %>% 
        group_by(lon, lat, year, case.name, var.id, plot.i, period) %>% 
        summarize(value=sum(value))
      
      # making data for the "difference" plot
      if (i == 3) {
        
        diff.data = sub.data %>%
          group_by(lon, lat, year, period) %>%
          summarize(value = value[2] - value[1])
        
        diff.data$case.name = plot.type
        diff.data$var.id = "2nd plot from 1st plot"
        diff.data$plot.i = 3
        
        diff.type = "minus"
        
        sub.data = diff.data
      }
      
      # making marks and breaks for colorbar
      limits = quantile(x = sub.data$value,
                        probs = c(0.01,0.99), na.rm = T)
      
      zlim = c(min(0, limits[1]), max(0, limits[2]))
      
      data.breaks = seq(from = zlim[1], to = zlim[2], 
                        length.out = ifelse(or(zlim[1] == 0, zlim[2] == 0), 6, 11))
      
      # extracting data for plotting
      sub.data = subset(sub.data, plot.i == i)
      
      # defining basic plot x, y and z (here we use filled raster)
      g = ggplot(data = sub.data, mapping = aes(x = lon, y = lat))
      
      # using non-interpolated raster
      g = g + geom_raster(aes(fill = value), interpolate = F)
      
      g = g + scale_fill_gradient2(low = "#0571b0", mid = "#f7f7f7", high = "#ca0020",
                                   midpoint = 0, na.value = NA, oob = squish,
                                   breaks = data.breaks,
                                   labels = format(data.breaks, digits = 3),
                                   limits = zlim,
                                   guide = guide_colorbar(title = plot.unit, 
                                                          keyheight = 0.75,
                                                          keywidth = 0.25,
                                                          nbin = length(data.breaks),
                                                          raster = F))
      
      # adding country borders to the plot
      g = g + borders(database = "world", size = 0.25)
      
      # giving the plot titles
      g = g + labs(titles = sub.data$case.name,
                   subtitle = sprintf(fmt = "%s | %s | %s | %s %s", 
                                      sub.data$var.id, 
                                      paste0(plot.period, "_total"),
                                      ifelse(avg.period == "annual", yyyy, paste0(yyyy, "_", avg.period)),
                                      format(ifelse(plot.type == "difference", 
                                                    sum(sub.data$value, na.rm = T),
                                                    mean(sub.data$value, na.rm = T) ),
                                             digits = 3, scientific = F,
                                             big.mark = ","),
                                      var.unit) )
      
      # zooming in the plot
      g = g + coord_fixed(xlim = plot.lon, ylim = plot.lat, expand = FALSE)
      
      # adding "E" to x-axis label
      g = g + scale_x_continuous(label = unit_format(unit = "E", sep = ""))
      
      # adding "N" to y-axis label
      g = g + scale_y_continuous(label = unit_format(unit = "N", sep = ""))
      
      # applying some styles to the plot, e.g. bolding fonts, aligning and omitting useless labels
      g = g + theme(plot.title = element_text(face = "bold", hjust = 0.5),
                    plot.subtitle = element_text(hjust = 1),
                    axis.title = element_blank(),
                    legend.justification = c(0,0.5),
                    legend.text.align = 1)
      
      # storing the plot in a large list of plot
      gg[[i]] = g
      
    } # end of plot loop
    
    if (plot.type == "single.plot") {
      png(path = output.dir,
          filename = paste0(output.dir, paste(plot.case.name, plot.var.id, 
                                              yyyy, avg.period,
                                              'png', 
                                              sep = ".")),
          width = 1800, height = 1800 * nplot * abs(diff(plot.lat)/diff(plot.lon)), res = 300)
      print(ggarrange(plotlist = gg, nrow = nplot))
      dev.off()
    } else {
      png(filename = paste0(output.dir, paste(plot.case.name[1], plot.var.id[1], diff.type, 
                                              plot.case.name[2], plot.var.id[2], 
                                              yyyy, avg.period, 
                                              'png', 
                                              sep = ".")),
          width = 1800, height = 1800 * nplot * abs(diff(plot.lat)/diff(plot.lon)), res = 300)
      print(ggarrange(plotlist = gg, nrow = nplot, align = "v"))
      dev.off()
    }
    
    
  } # end of period-loop
} # end of year-loop

