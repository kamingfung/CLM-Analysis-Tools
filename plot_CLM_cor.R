### ============ Loading libraries ============
library(scales)           # for prettier scalebar in the plots
library(ggplot2)          # for pretty plots
library(reshape2)         # for reshaping data.frame for plotting data
library(RColorBrewer)     # for prettier color in plots
library(ggpubr)           # for arranging mulitple plots
library(dplyr)            # for data tidying
library(tidyr)            # for data tidying

### ============= Specifying parameters ============= 
# specify data directory
data.dir = "./_data/"

# range of years of data to be plotted [vector of integers]
plot.year = 2007

# specific what average to be plotted [monthly; seasonal and annual; annaul]
# plot.period = "monthly"
# plot.period = "seasonal"
plot.period = "annual"

# range of map to show
plot.lon = c(-180, 180)
plot.lat = c(-60, 90)

# name of cases to be plotted [vector of strings]
plot.case.name = c("test30", "test30")

# name of variables to be plotted [vector of strings]
plot.var.id = c("TOTECOSYSC", "TOTECOSYSN")

# unit conversion
var.unit = "Tg"
unit.convertor = 1e-12 # unit conversation coefficient

# aggregating over area
area.unit = "grid"        # options: {m^2, km^2, ha, grid}

# aggregating over time
# !!! USE THIS FOR FLUX VARIABLES ONLY!!!
per.second.to.per.month = FALSE # options: {TRUE, FALSE}

### ============= Preprocessing the plots =============
# adding area unit
if (area.unit == "m^2") {
  plot.unit = bquote(.(var.unit)~"m"^-2)
  
} else if (area.unit == "km^2") {
  plot.unit = bquote(.(var.unit)~"km"^-2)
  
} else if (area.unit == "ha") {
  plot.unit = bquote(.(var.unit)~"ha"^-1)
  
} else if (area.unit == "grid") {
  plot.unit = bquote(.(var.unit)~"grid"^-1)
}

# adding s^-1 / month^-1 / season^-1 / year^-1
if (!per.second.to.per.month) {
  plot.unit = bquote(.(plot.unit)~"s"^-1)
} else { 
  if (plot.period == "seasonal" & plot.stats == "total") {
    plot.unit = bquote(.(plot.unit)~"season"^-1)
    
  } else if (plot.period == "annual" & plot.stats == "total") {
    plot.unit = bquote(.(plot.unit)~"year"^-1)
    
  } else if (plot.period == "month" & plot.stats == "total") {
    plot.unit = bquote(.(plot.unit)~"month"^-1)
    
  } else {
    plot.unit = bquote(.(plot.unit)~"month"^-1)
    
  }
}

### ============= Collecting the plot data =============

# initilizing data.frame for storing plot data
plot.data = NULL

# reading nc files and extracting them to plot.data
for (i in 1:2) {
  
  # reading the nc file
  file.name = list.files(path = paste(data.dir, plot.case.name[i], sep = "/"), 
                         pattern = paste0(".*.", plot.var.id[i],"\\.",".*\\.RDS"), 
                         full.names = T)
  
  # extracting variable
  new.data = readRDS(file.name) %>%
    filter(year <= max(plot.year), year >= min(plot.year))
  
  # converting default m^-2 to ...
  if (area.unit == "m^2") {
    grid.area = 1
    
  } else if (area.unit == "km^2") {
    grid.area = (10^-3)^-2 # m^-2 -> km^-2
    
  } else if (area.unit == "ha") {
    grid.area = (10^-2)^-2 # m^-2 -> ha^-1
    
  } else if (area.unit == "grid") {
    grid.area = new.data$land.area # m^-2 -> grid total
    
  }
  
  # if convert flux variable to state
  if (per.second.to.per.month) {
    days_in_month = c(31, 28, 31, 30, 31, 30, 
                      31, 31, 30, 31, 30, 31)
    time.converter = 60 * 60 * 24 * days_in_month[new.data$month] # second in a month
  } else {
    time.converter = 1
  }
  
  # calculating total value from a grid
  new.data$value = new.data$value * grid.area * time.converter * unit.convertor # converted unit
  
  # adding infos to data frame
  new.data$var.id = plot.var.id[i]
  new.data$plot.i = i
  new.data$dataset = paste0("set.", i)
  plot.data = rbind(plot.data, new.data)
  rm(new.data)
  
}

#==============================================================#
###### ===== Plotting the graphs ====== 

cor.data = plot.data %>%
  filter(lon >= plot.lon[1] & lon <= plot.lon[2] & 
           lat >= plot.lat[1] & lat <= plot.lat[2]) %>%
  group_by(lon, lat, dataset) %>%
  summarise(value = list(value)) %>%
  spread(dataset, value) %>%
  group_by(lon, lat) %>%
  summarize(r.sq = (tryCatch(cor.test(unlist(set.1),unlist(set.2))$estimate, error = function(e) NA))^2,
            p = tryCatch(cor.test(unlist(set.1),unlist(set.2))$p.value, error = function(e) NA),
            value = sum(unlist(set.1)))

cor.data$p = cut(cor.data$p, breaks = c(0,0.05,1))
cor.data$r.sq = cut(cor.data$r.sq, breaks = c(0,0.5 ,1))

sub.data = cor.data

# making marks and breaks for colorbar
limits = quantile(x = sub.data$value,
                  probs = c(0,1), na.rm = T)

zlim = c(min(0, limits[1]), max(0, limits[2]))

data.breaks = seq(from = zlim[1], to = zlim[2], 
                  length.out = ifelse(or(zlim[1] == 0, zlim[2] == 0), 5, 9))

# select the unit for the plot
legend.unit = plot.unit

# defining basic plot x, y and z (here we use filled raster)
g = ggplot(data = sub.data, mapping = aes(x = lon, y = lat))

# using non-interpolated raster
g = g + geom_raster(aes(fill = value), interpolate = F)

g = g + scale_fill_gradientn(colors = rev(brewer.pal(11,"RdBu"))[ifelse(zlim[1]==0,6,1):ifelse(zlim[2]==0,6,11)],
                             na.value = NA, oob = squish,
                             breaks = data.breaks,
                             labels = format(data.breaks, digits = 3),
                             limits = zlim,
                             guide = guide_colorbar(title = legend.unit, 
                                                    keyheight = 0.75,
                                                    keywidth = 0.75,
                                                    nbin = length(data.breaks),
                                                    raster = F))

# adding country borders to the plot
g = g + borders(database = "world", size = 0.25)

# showing comparison results
g = g + geom_point(aes(shape = p, alpha = r.sq), size = 0.05)
g = g + scale_shape_manual(values = c(3, NA),
                           breaks = c("(0,0.05]"), 
                           labels = c("< 0.05"),
                           guide = guide_legend(title = "p-value"))
g = g + scale_alpha_manual(values = c(0.25, 1),
                           breaks = levels(sub.data$r.sq),
                           guide = guide_legend(title = bquote("R"^2)))


# giving the plot titles
g = g + labs(title = paste(plot.case.name, plot.var.id, collapse = " against ", sep = "."),
             subtitle = sprintf(fmt = "%s | %s | %s | %s",
                                plot.case.name[1], 
                                plot.var.id[1],
                                paste0(plot.period, "_total"),
                                unique(plot.data$year)[1]))

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

png(filename = paste0("_png/", 
                      paste(plot.case.name[1], plot.var.id[1], "vs", 
                            plot.case.name[2], plot.var.id[2], 
                            "correlation", 
                            'png', 
                            sep = ".")),
    width = 2700, height = 2700 * 1 * abs(diff(plot.lat)/diff(plot.lon)), res = 300)
print(ggarrange(plotlist = list(g), nrow = 1, align = "v"))
dev.off()

