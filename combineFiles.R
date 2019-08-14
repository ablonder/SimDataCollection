library(stringr)

combineFiles = function(fnames, paramIDs, endstrings, paramnames){
  dtable = data.frame()
  for(f in 1:length(fnames)){
    d = read.table(fnames[f], header = TRUE, comment.char = "%", sep = "\t")
    # find all the params and endstrings
    splits = unlist(str_locate_all(fnames[f], c(paramIDs, endstrings)))
    # store the starts and ends separately
    starts = splits[c(T, F)]
    ends = splits[c(F, T)]
    # get the order of all the splits
    splitorder = order(starts)
    # loop through all the splits in order
    for(s in 1:length(splitorder)){
      # only use it if it's actually a parameter, not just an end string
      if(splitorder[s] < length(paramnames)+1){
        # grab the value between the end of this split and the beginning of the next
        val = substr(fnames[f], ends[splitorder[s]]+1, starts[splitorder[s+1]]-1)
        if(!is.na(as.numeric(val))) { val = as.numeric(val) }
        # create a new column with the designated name and value
        d[, paramnames[splitorder[s]]] = val
      }
    }
    # now merge this dataframe with the previous
    dtable = rbind(dtable, d)
  }
  # return the complete table
  return(dtable)
}