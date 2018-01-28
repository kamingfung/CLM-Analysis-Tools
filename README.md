# CLM_analysis_tools
This project contains some tools I wrote to analyze the simulation results of Community Land Model 4.5, i.e. a batch of history files in ncdf4 format. (You can download Panoply from NASA to check the data out.)

## Sample Data
I included some sample data sets from my testing CLM simulation cases, namely _"test30"_ and _"test32"_, in the directory named `_data`. In each of the case sub-folder, there are two `.RDS` files storing extracts simulation results, which are the total ecosystem carbon (C) and nitrogen (N) from January 2007 to December 2008. These results are for demonstrating how the 2 codes in the top folders could be used. You are advised to study the data structure before using the below tools.

## Tools written in R
You are recommended to download Rstudio and open the `.Rproj` for easier set up before using the below `.R` files. Make sure you have then installed the newest developer's version of ggplot2 package following the comment in this post: https://gist.github.com/kohske/1150934

### Plotting variables on a map
It helps to visualize the spatial variability of a variable with a heat map. `plot_CLM_map.R` helps you to aggregate variables from simulation results annually, seasonally or monthly. You may choose to plot maps from a signal case or plot multiple maps from two cases and together with their difference in each grid point, i.e. 3 maps in total. Maps will be stored in folder `_png`.

### Plotting variables against time
Sometimes you may wonder how variables evolve over time. `plot_var_annaul.R` gather variables from cases and use line plots to show temporal variabilities of variables. Again, outputted plots will be stored in folder `_png`.

### Plotting comparison between two datasets
Correlation analysis is useful to investigate rather variables are linearly related to each other. `plot_CLM_cor.R` aims to help you finish the task. Outputs of this script are maps showing annual total value in each grid and put a marker on that grid if monthly variable values of the two datasets in that gridcell is correlated with a p-value smaller than 0.05. If the correlation has a R-sqaure value greater than 0.5, the marker will be displayed in a darker color.

#
Hope you would find these tools useful.


fkm
