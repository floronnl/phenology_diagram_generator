######################################################################################
# Phenology diagram & graph generator
# Author: Dion van der Hak
#
# Required data: A dataframe containing observations with the date of observation 
# in day, month, year with at least 25 observations
######################################################################################

# Libraries
require(ggplot2)
require(scales)
require(circular)

##
# This function takes day, month, year of observation and generates output containing:
# 1. A density graph
# 2. The circular mean
# 3. The lower and upper bound of the confidence interval of the circular mean
# 4. The mean resultant length of the circular data
# 5. The daynumber of the peak of the density graph
# 6. A boxplot of the data
# 7. Quantile data of given quantiles for the graph and the boxplot
# 8. The number of observations
##
flower_graph = function(id,                          # Identifier
                        day,                         # Day of the observation
                        month,                       # Month of the observation
                        year,                        # Year of the observation
                        observations = NULL,         # Data.frame containing the above columns (optional)
                        stepSize = 7,                # Bin size
                        startDay = 1,                # The start day of the graph, all obervations before this will be ignored
                        endDay = 365,                # The end day of the graph, all observations after this will be ignored
                        quantiles = c((1-MRL)/3, 0.5, 1-(1-MRL)/3), # Quantiles to be calculated and used for the graph. You can also use a function or formula for the quantiles.
                        quantiles_boxplot = c(0.1, 0.25, 0.5, 0.75, 0.9), # Quantiles used by the boxplot. You can also use a function or formula for the quantiles.
                        title = NULL                 # Title of the plot
                        ) {
  
  # Allow for pointing to columns in data.frames directly
  id = eval(substitute(id), observations, parent.frame())
  day = eval(substitute(day), observations, parent.frame())
  month = eval(substitute(month), observations, parent.frame())
  year = eval(substitute(year), observations, parent.frame())
  quantiles = deparse(substitute(quantiles))
  quantiles_boxplot = deparse(substitute(quantiles_boxplot))
  title = eval(substitute(title), observations, parent.frame())
  
  # Check input
  n_obs = max(length(id), length(day), length(month), length(year), nrow(observations))
  stopifnot(length(id) > 0,
            length(day) > 0, 
            length(month) > 0,
            length(year) > 0,
            is.numeric(day),
            is.numeric(month),
            is.numeric(year),
            is.numeric(stepSize),
            is.numeric(startDay),
            is.numeric(endDay),
            n_obs > 24
  )
  
  
  # Filter data
  obs_time = day + sapply(month, FUN = Days_from_month)
  df = data.frame(id, obs_time)
  df = df[df$obs_time >= startDay & df$obs_time <= endDay,]
  stopifnot(nrow(df) > 4)
  
  
  # Make data circular
  circular_time = ((df$obs_time - startDay) / (endDay - startDay)) * 2 * pi
  df = data.frame(df, circular_time)
  circ_data = circular(df$circular_time, units = "radians", rotation = "clock")
  
  
  # Mean
  circ_mean = mean(circ_data)[[1]]
  while(circ_mean < 0) { circ_mean = circ_mean + 2 * pi }
  circ_mean = circ_mean / pi / 2 * (endDay - startDay) + startDay
  
  
  # CI of mean
  ci_result = mle.vonmises.bootstrap.ci(circ_data, alpha = 0.05)
  ci_max = as.numeric(ci_result$mu.ci[1])
  while(ci_max < 0) { ci_max = ci_max + 2 * pi }
  ci_max = ci_max / pi / 2 * (endDay - startDay) + startDay
  ci_min = as.numeric(ci_result$mu.ci[2]) 
  while(ci_min < 0) { ci_min = ci_min + 2 * pi }
  ci_min = ci_min / pi / 2 * (endDay - startDay) + startDay
  
  
  # Mean Resultant Length (MRL)
  MRL = rho.circular(circ_data)
  
  
  # Peak
  circ_density = density.circular(circ_data, bw = 25)
  peak_x = as.numeric(circ_density$x[circ_density$y == max(circ_density$y)])
  while(peak_x < 0) { peak_x = peak_x + 2 * pi }
  peak_x = peak_x / pi / 2 * (endDay - startDay) + startDay
  
  
  # Quartiles
  quantiles = pmax(eval(parse(text = quantiles)), 0)
  circ_quartiles = as.numeric(quantile.circular(circ_data, probs = quantiles))
  while(min(circ_quartiles) < 0) { circ_quartiles = circ_quartiles + 2 * pi }
  circ_quartiles = circ_quartiles / pi / 2 * (endDay - startDay) + startDay
  
  # Boxplot quartiles
  quantiles_boxplot = eval(parse(text = quantiles_boxplot))
  stopifnot(length(quantiles_boxplot) == 5)
  box_quartiles = as.numeric(quantile.circular(circ_data, probs = quantiles_boxplot))
  while(min(box_quartiles) < 0) { box_quartiles = box_quartiles + 2 * pi }
  box_quartiles = box_quartiles / pi / 2 * (endDay - startDay) + startDay
  
  # Plot
  monthBreaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
  monthNames = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  monthdf = data.frame(monthNames, monthBreaks)
  monthdf = monthdf[monthdf$monthBreaks >= startDay & monthdf$monthBreaks <= endDay,]
  
  # We plot part of the dataframe in front and behind the dataframe. The purpose of this is to make sure the density plot is also circular.
  plotdf = rbind(df, df, df)
  plot_offset = endDay - startDay + 1
  plotdf$obs_time = plotdf$obs_time + rep(c(-plot_offset, 0, plot_offset), each = nrow(df))
  
  p = ggplot(plotdf, aes(x = obs_time)) + geom_density(aes(y = ..count.. * 10), fill = "grey", adjust = 1/2)
  p = p + coord_cartesian(xlim = c(startDay, endDay), expand = 0)
  p = p + scale_x_continuous("Month", breaks = monthdf$monthBreaks, labels = monthdf$monthNames, limits = c(-plot_offset / 5, plot_offset * 1.2))
  p = p + ylab("Observations / week")
  p = p + geom_vline(xintercept = circ_mean)
  p = p + geom_vline(xintercept = ci_min, linetype = "dotted")
  p = p + geom_vline(xintercept = ci_max, linetype = "dotted")
  p = p + geom_vline(xintercept = circ_quartiles, color = "grey33")
  p = p + theme_bw()
  if(!is.null(title)) { p = p + labs(title = title) + theme(plot.title = element_text(hjust = 0.5)) }
  
  # Boxplot
  q = ggplot(df, aes(y = obs_time, x = 1))
  q = q + stat_summary(fun.data = bp.vals, fun.args = list(quantiles, box_quartiles), geom = "boxplot")
  q = q + scale_y_continuous("Month", breaks = monthdf$monthBreaks, labels = monthdf$monthNames, limits = c(startDay, endDay))
  q = q + xlim(0, 2)
  q = q + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  q = q + coord_flip()
  if(!is.null(title)) { q = q + labs(title = title) + theme(plot.title = element_text(hjust = 0.5)) }
  
  
  # Return
  return(list("graph" = p, 
              "mean" = circ_mean, 
              "ci_min" = min(ci_min, ci_max), 
              "ci_max" = max(ci_min, ci_max),
              "MRL" = MRL,
              "peak_day" = peak_x,
              "boxplot" = q,
              "quantiles" = data.frame(quantiles, circ_quartiles),
              "boxplot_quantiles" = data.frame(quantiles_boxplot, box_quartiles),
              "n_obs" = n_obs
              ))
  
  

  
  
}




Days_from_month = function(month) {
  sum(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[1:month-1])
}


bp.vals = function(x, probs, box_quartiles) {
  r = box_quartiles
  names(r) = c("ymin", "lower", "middle", "upper", "ymax")
  return(r)
}





