# Read in datafile containing dates of observations
data = read.csv("sample_data.csv", sep = ";", header = T, fileEncoding = "UTF-8")

head(data)
# year month day
# 2018     5  10
# 2013     6   7
# 2014     5  14
# 2015     5   8
# 2019     6   2
# 2017     5  13

# Read in R file with function
source("phenology_diagram_generator.R")

# Execute function
result = phenology_graph(
  id = 1,
  day = day,
  month = month,
  year = year,
  observations = data,
  quantiles_boxplot = c(0.1, 0.2, 0.5, 0.8, 0.9),
  title = "Orchis anthropophora"
)

# Show graph
result$graph

# Get circular mean day of year
result$mean # 145.1942

# Get bootstrap 95% confidence interval of the mean
result$ci_min # 143.7551
result$ci_max # 146.588

# Get Mean Resultant Length (MRL)
# The MRL is a measure of spread of circular data: 
# A value of 1 means all data is on a singular day
# A value of 0 means data is evenly spread throughout the year
result$MRL # 0.9815351

# Get the day of year of the peak of the circular density
result$peak_day # 144.8904

# Show boxplot
result$boxplot

# Get the quantiles and their values used for the graph
result$quantiles
# quantiles      circ_quartiles
# 0.006154958       174.3581
# 0.500000000       145.0000
# 0.993845042       114.0000

# Get the quantiles and their values used for the boxplot
result$boxplot_quantiles
# quantiles_boxplot    box_quartiles
#               0.1           159
#               0.2           154
#               0.5           145
#               0.8           135
#               0.9           132

# Get the number of observations
result$n_obs # 248
