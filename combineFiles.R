library(stringr)

combineFiles = function(fnames, paramIDs=c(), endstrings=c(), paramnames=c(), sep = ",", filetypes = c(""),
                        readheader = F){
  dtable = data.frame()
  for(f in 1:length(fnames)){
    # just combine the ones that have the provided strings (or, if no strings have been provided, combine all)
    if(any(str_detect(fnames[f], filetypes))){
      d = read.table(fnames[f], header = TRUE, comment.char = "%", sep = sep)
      # find all the params and endstrings
      if(length(c(paramIDs, endstrings)) > 0){
        splits = unlist(str_locate_all(fnames[f], c(paramIDs, endstrings)))
        # store the starts and ends separately
        starts = splits[c(T, F)]
        ends = splits[c(F, T)]
        # get the order of all the splits
        splitorder = order(starts)
        # I think I have to go through and determine which ones are actually there to make this work
        params = c()
        for(p in 1:length(paramIDs)){
          # if this param is actually in the string, add it to the list
          if(str_detect(fnames[f], paramIDs[p])){
            params = c(params, paramnames[p])
          }
        }
        # loop through all the splits in order
        for(s in 1:length(splitorder)){
          # only use it if it's actually a parameter, not just an end string (and also not already a column)
          if(splitorder[s] < length(params)+1 & !is.element(params[splitorder[s]], names(d))){
            # grab the value between the end of this split and the beginning of the next
            val = substr(fnames[f], ends[splitorder[s]]+1, starts[splitorder[s+1]]-1)
            if(!is.na(as.numeric(val))) { val = as.numeric(val) }
            # create a new column with the designated name and value
            d[, params[splitorder[s]]] = val
          }
        }
      }
      # once that's done, I can add the remaining parameters straight from the header
      if(readheader){
        header = read.table(text = gsub(",|:", "\n", readLines(fnames[f], n = 1)), sep = "=", strip.white = T, skip = 1)
        # check each row to add the value to the table
        for(r in 1:nrow(header)){
          if(!is.element(toString(header[r,1]), names(d))){
            if(!is.na(as.numeric(toString(header[r,2])))){
              d[,toString(header[r,1])] = as.numeric(toString(header[r,2]))
            } else{
              d[,toString(header[r,1])] = toString(header[r,2])
            }
          }
        }
      }
      # now merge this dataframe with the previous, accounting for any columns they may not share
      for(n in names(dtable)){
        if(!is.element(n, names(d))){
          # TODO - maybe the answer is to replace this with an empty string instead
          d[,n] = NA
        }
      }
      if(nrow(dtable) > 0){
        for(n in names(d)){
          if(!is.element(n, names(dtable))){
            dtable[,n] = NA
          }
        }
      }
      dtable = rbind(dtable, d)
    }
  }
  # return the complete table
  return(dtable)
}