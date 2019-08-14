library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)

# generates line graphs or bar plots (as applicable) of the results provided based on:
# dtable - data frame of results, with columns for each parameter to split/graph by, and each category of results
# params - vector of column names to split/graph:
#   the lines/bars will be colored based on the last column listed
#   the x-axis will be based on the second to last column listed
#   the data will be split into graphs based on all the other columns (it will create one graph for each combination of values)
# fname - the beginning of the name of all the outputted files
#   it will add on all the names of the parameters split by and their values for each graph
# testvars - list of vectors of column names to plot together (y values) on the same graph
#   can be used to color the graph or for the x-axis if included in params as "category"
# testlabels - vector of labels for the y-axis of each graph, should be the same length as testvars
# catlabels - vector of labels for the colors of each graph, should be the same length as testvars
plotResults = function(dtable, params, fname, testvars, testlabels, catlabels, scatter = F, 
                       linelabels = c(), shapelabels = c(), savelv = 2){
  if(length(params) == savelv){
    # loop through each set of measures to crete the corresponding plots
    for(v in 1:length(testvars)) {
      # format the data for ggplot
      plotd = gather(dtable, category, measure, testvars[[v]])
      # make sure nothing is infinite
      is.na(plotd) = sapply(plotd, is.infinite)
      # grab the x axis variable for convenience
      plotd$x = plotd[, params[length(params)-1]]
      # split the category to see if there are actually multiple
      cats = strsplit(params[length(params)], " ")[[1]]
      # the category can be split by color
      plotd$c = ""
      # by shape
      plotd$s = ""
      # and by linetype
      plotd$l = ""
      # I'm going to have an index here to increment
      j = 1
      # if the data is going to be split into shapes, use the first category for that
      if(length(shapelabels) > 0){
        plotd$s = factor(plotd[, cats[1]])
        j = j+1
        # also, if there's only one shape label given use that
        if(length(shapelabels) == 1){
          slab = shapelabels[1]
        } else {
          # otherwise, use the label corresponding to this test variable
          slab = shapelabels[v]
        }
      } else {
        slab = ""
      }
      # if there are more categories left and this is going to be split into linetypes, do so
      if(length(cats) >= j & length(linelabels) > 0){
        plotd$l = factor(plotd[, cats[j]])
        j = j+1
        # also set the line label
        if(length(linelabels) == 1){
          llab = linelabels[1]
        } else {
          llab = linelables[v]
        }
      } else {
        llab = ""
      }
      # if there are more categories left, combine them for use as the color
      if(length(cats) >= j) {
        for(i in j:length(cats)){
          plotd$c = paste(plotd$c, plotd[, cats[i]])
        }
        # also set the color label
        if(length(catlabels) == 1){
          clab = catlabels[1]
        } else {
          clab = catlables[v]
        }
      } else {
        clab = ""
      }
      # turn the color into a factor
      plotd$c = factor(plotd$c)
      # if this isn't a scatter plot average the relevant measures so there aren't duplicates
      if(!scatter){
        plotd = summarize(group_by(group_by_at(plotd, params[1:(length(params)-2)]), x, c, s, l, add = T),
                          N = length(measure), mean = mean(measure, na.rm = T),
                          sd = sd(measure, na.rm = T), se = sd / sqrt(N))
        plotd = as.data.frame(plotd)
      }
      # plot the results
      # create a scatter plot if scatter is true
      if(scatter){
        plot = ggplot(plotd, aes(x = x, y = measure, colour = c)) + geom_point(aes(shape = s)) +
          ylab(testlabels[v]) + xlab(params[length(params)-1]) + labs(colour = params[length(params)]) +
          guides(colour=guide_legend(title=clab), shape=guide_legend(title=slab)) +
          geom_smooth(method = 'lm', se = F)
      } else if(is.numeric(plotd$x)){
        # otherwise, if the x-axis is numeric, turn this into a linegraph
        plot = ggplot(plotd, aes(x = x, y = mean, colour = c)) +
          geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) + geom_line(aes(linetype = l)) +
          geom_point(aes(shape = s)) + ylab(testlabels[v]) + xlab(params[length(params)-1]) +
          labs(colour = params[length(params)]) +
          guides(colour=guide_legend(title=clab), shape=guide_legend(title=slab), linetype=guide_legend(title = llab))
      } else {
        # otherwise, turn it into a bargraph
        plot = ggplot(plotd, aes(x = x, y = mean, fill = c)) +
          geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
          geom_bar(stat = "identity", position = "dodge") + ylab(testlabels[v]) + xlab(params[length(params)-1]) + 
          labs(fill = params[length(params)]) + guides(fill=guide_legend(title=clab))
      }
      # if the save level is higher than 2, make a grid of grids for all the remaining parameters
      # if the save level is 3, just make a single row
      if(savelv == 3){
        plot = plot + facet_grid(cols = vars(plotd[, params[(length(params)-2)]]))
      } else if (savelv > 3) {
        # otherwise, make a grid from the last two parameters
        plot = plot + facet_grid(rows = vars(plotd[, params[(length(params)-2)]]),
                          cols = vars(plotd[, params[(length(params)-3)]]))
      }
      ggsave(paste(fname, testlabels[v], ".png", sep = ""), plot = plot)
    }
  } else {
    # otherwise, recurse on all values of this parameter
    for(v in unique(dtable[, params[1]])){
      # update the filename to pass it
      f = paste(fname, params[1], v, sep = "")
      # get the corresponding plot/grid of plots
      plotResults(dtable[dtable[, params[1]] == v, ], params[-1], f, testvars, testlabels, catlabels,
                    scatter, linelabels = linelabels, shapelabels = shapelabels, savelv = savelv)
    }
  }
}