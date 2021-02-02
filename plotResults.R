  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(gridExtra)
  
  # generates line graphs or bar plots (as applicable) of the results provided based on:
  # dtable - data frame of results, with columns for each parameter to split/graph by, and each category of results
  # params - vector of column names to split/graph:
  #   one graph will be created for each combination of values in the columns listed
  #   the second to last column listed will be used to create the x-axis
  #   the last string in the vector may be multiple column names separated by spaces
  #     if shapelabels have been provided, the first column name is used to determine the shape of the points
  #     if linelabels have been provided, the next column name is used to determine the line pattern
  #     any remaining column names are concatenated and used to color the lines/points/bars
  # fname - the beginning of the name of all the outputted files
  #   each graph's name will consist of fname, followed by the names and values of the parameters used to split the data
  # testvars - list of vectors of column names to plot together (as y-values) on the same graph
  #   can be used to color the graph or for the x-axis if included in params as "category"
  # testlabels - vector of labels for the y-axis of each graph, should be one or the same length as testvars
  # catlabels - vector of labels for the colors of each graph, should be one or the same length as testvars
  # scatter - whether a scatter plot is returned, or the data is turned into a barplot/linegraph
  # linelabels - vector of labels for the line patterns of each graph, should be one or the same length as testvars
  # shapelables - vector of labels for the point shapes of each graph, should be one or the same length as testvars
  # savelv - how many parameter combinations should be included in the same facetted plot, maximum 4
  # pointsize - whether points should be scaled according to the number of values averaged to create them
  # errorbars - whether plots should include error bars
  # colorlegend, linelegend, shapelegend - whether to actually display the legends for each attribute
  # aggregate - whether entries with the same provided params are averaged over or all displayed
  # fitline - whether a scatterplot has a line of fit
  # width, height - dimensions for the graph created, default to the size of the current plotting window
  plotResults = function(dtable, params, fname, testvars, testlabels, catlabels, scatter = F, 
                         linelabels = c(), shapelabels = c(), savelv = 2, pointsize = F, errorbars = F,
                         colorlegend = T, linelegend = T, shapelegend = T, aggregate = T, fitline = F,
                         width = par("din")[1], height = par("din")[2]){
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
        sleg = F
        if(length(shapelabels) > 0){
          plotd$s = factor(plotd[, cats[1]])
          j = j+1
          # also if we're actually doing a shape legend, create that
          if(shapelegend){
          # if there's only one shape label given use that
            if(length(shapelabels) == 1){
              slab = shapelabels[1]
            } else {
              # otherwise, use the label corresponding to this test variable
              slab = shapelabels[v]
            }
            sleg = guide_legend(title = slab)
          }
        }
        # if there are more categories left and this is going to be split into linetypes, do so
        lleg = F
        if(length(cats) >= j & length(linelabels) > 0){
          plotd$l = factor(plotd[, cats[j]])
          j = j+1
          # also create the line legend if there's going to be one
          if(linelegend){
            if(length(linelabels) == 1){
              llab = linelabels[1]
            } else {
              llab = linelables[v]
            }
            lleg = guide_legend(title = llab)
          }
        }
        # if there are more categories left, use them to color the graph
        cleg = F
        if(length(cats) >= j) {
          for(i in j:length(cats)){
            plotd$c = paste(plotd$c, plotd[, cats[i]])
          }
          # also set the color label if there's going to be a legend
          if(colorlegend){
            if(length(catlabels) == 1){
              clab = catlabels[1]
            } else {
              clab = catlables[v]
            }
            cleg = guide_legend(title = clab)
          }
        }
        # turn the color into a factor
        plotd$c = factor(plotd$c)
        # if the data isn't being aggregated, average the relevant measures so there aren't duplicates
        if(aggregate){
          plotd = summarize(group_by(group_by_at(plotd, params[1:(length(params)-2)]), x, c, s, l, add = T),
                            N = length(measure), mean = mean(measure, na.rm = T),
                            sd = sd(measure, na.rm = T), se = sd / sqrt(N))
          plotd = as.data.frame(plotd)
        } else {
          plotd = mutate(plotd, N = 1, mean = measure, sd = 0, se = 0)
        }
        # also toggle error bars by getting rid of standard error
        if(!errorbars){
          plotd$se = 0
        }
        # plot the results
        # TODO - give it the option to tell it what type of color to use
        if(is.numeric(plotd$x)){
          # create a scatter plot if scatter is true
          if(scatter){
            plot = ggplot(plotd, aes(x = x, y = mean, color = c, shape = s)) + geom_point() +
              ylab(testlabels[v]) + xlab(params[length(params)-1]) + labs(colour = params[length(params)]) +
              guides(colour=cleg, shape=sleg) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) #+ scale_color_viridis_d()
            # adds a fitline if that option is turned on
            if(fitline){
              plot = plot + geom_smooth(method = 'lm', se = F)
            }
          } else {
          # otherwise turn this into a linegraph
            # if the points are scaled by number of points, do that here
            ps = F
            if(pointsize){
              plotd$ps = plotd$N/sum(plotd$N)
            }
            plot = ggplot(plotd, aes(x = x, y = mean, colour = c)) +
              geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
              geom_line(aes(linetype = l), position = position_dodge(.5)) +
              geom_point(aes(shape = s, size = ps), position = position_dodge(.5)) +
              ylab(testlabels[v]) + xlab(params[length(params)-1]) +
              labs(colour = params[length(params)]) +
              guides(colour=cleg, shape=sleg, linetype=lleg, size = "none")
          }
        } else {
          if(!aggregate){
            # if this is a factor unaggregated plot, make a box plot
            plot = ggplot(plotd, aes(x = x, y = measure)) + geom_boxplot() + 
              geom_jitter(aes(colour = c, shape = s, alpha = 1/length(plotd)), width = .01) + ylab(testlabels[v]) + xlab(params[length(params)-1]) +
              labs(colour = params[length(params)]) + guides(colour=cleg, shape=sleg, alpha = "none")
          } else {
          # otherwise, turn it into a bargraph
            plot = ggplot(plotd, aes(x = x, y = mean, fill = c, color = c)) +
              geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(.9)) +
              geom_bar(stat = "identity", position = "dodge") + ylab(testlabels[v]) + xlab(params[length(params)-1]) + 
              labs(fill = params[length(params)]) + guides(fill=cleg, color = "none") + geom_point()
          }
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
        # save the plot with the given file name and options to change height and width
        ggsave(paste(fname, testvars[v], ".png", sep = ""), plot = plot, width = width, height = height)
      }
    } else {
      # otherwise, recurse on all values of this parameter
      for(v in unique(dtable[, params[1]])){
        # update the filename to pass it
        f = paste(fname, params[1], v, sep = "")
        # get the corresponding plot/grid of plots
        plotResults(dtable[dtable[, params[1]] == v, ], params[-1], f, testvars, testlabels, catlabels,
                    scatter, linelabels = linelabels, shapelabels = shapelabels, savelv = savelv,
                    colorlegend = colorlegend, linelegend = linelegend, shapelegend = shapelegend,
                    errorbars = errorbars, pointsize = pointsize, aggregate = aggregate, fitline = fitline,
                    width = width, height = height)
      }
    }
  }