shrink <- function(mat, n_cols, itval=NA, netname = NA, metric, metric_type='network', network_level =NA){

  if(!metric_type %in% c('network', 'species', 'modularity')){
    stop('acceptable metric types are \'network\', \'species\', \'modularity\'')
  }

  #Decide which rows to keep and which to discard
  #cat('ncol mat = ', ncol(mat), '\n')
  to_keep <- sample(seq(1, ncol(mat)), n_cols)
  #cat('n_cols to use is ', n_cols, '\n')
  #Make a dataframe of all of their metadata
  #print(netname)
  shrunkmeta <- data.frame(strsplit(colnames(mat), split = '-'))
  shrunkmeta <- t(shrunkmeta)
  shrunkmeta <- as.data.frame(shrunkmeta)
  colnames(shrunkmeta) <- c('Species', 'ID')

  #Make a column of all the original column names
  shrunkmeta$orig <- colnames(mat)
  shrunkmeta$orig <- as.factor(shrunkmeta$orig)

  #Make a column showing which were included in the iteration
  shrunkmeta$included <- rep(NA, nrow(shrunkmeta))
  shrunkmeta$included[to_keep] <- 'included'
  shrunkmeta$included[-to_keep] <- 'not_included'
  shrunkmeta$included <- as.factor(shrunkmeta$included)

  #Optional formatting
  if(!is.na(itval)){
    shrunkmeta$iteration <- rep(itval, nrow(shrunkmeta))
    shrunkmeta$n_used <- rep(n_cols, nrow(shrunkmeta))

  }
  if(!is.na(netname)){
    shrunkmeta$netnames <- rep(netname, nrow(shrunkmeta))
  }

  rownames(shrunkmeta) <- NULL

  #Shrink the matrix to the desired columns

  shrunkmat <- mat[,to_keep]

  #Rename the columns so that everything after the hyphen is removed
  colnames(shrunkmat) <- gsub('-.+', '', colnames(shrunkmat))
  #merge the duplicate columns (drop=F is required so that it can still rowsum columns which are unique)
  shrunkmat <- sapply(unique(colnames(shrunkmat)), function(x) rowSums(shrunkmat[,grepl(x, colnames(shrunkmat)), drop = F]))


  #Calculate the metric
  if(metric_type=='network'){
    metricval <- bipartite::networklevel(shrunkmat, index = metric)

    #Add it to the metadata
    shrunkmeta$metricval <- rep(metricval, nrow(shrunkmeta))

  }else if(metric_type=='species'){
    spvals <- bipartite::specieslevel(shrunkmat, index = metric, level = network_level)

    #Add it to the metadata
    shrunkmeta$metricval <- spvals

  }else if(metric_type=='modularity'){
    mod <- bipartite::computeModules(shrunkmat, steps = 1E6)@likelihood

    #Add it to the metadata
    shrunkmeta$metricval <- rep(mod, nrow(shrunkmeta))

  }

    #Add the metrics name to the df
    shrunkmeta$metricused <- rep(metric, nrow(shrunkmeta))

  return(shrunkmeta)

}
