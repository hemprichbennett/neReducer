shrink <- function(mat, n_cols, itval=NA, netname = NA){

  #Decide which rows to keep and which to discard
  cat('ncol mat = ', ncol(mat), '\n')
  to_keep <- sample(seq(1, ncol(mat)), n_cols)

  #Make a dataframe of all of their metadata
  print(netname)
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

  if(!is.na(itval)){
    shrunkmeta$iteration <- rep(itval, nrow(shrunkmeta))
    print(itval)
    shrunkmeta$netnames <- rep(netname, nrow(shrunkmeta))
  }

  rownames(shrunkmeta) <- NULL

  #Shrink the matrix to the desired columns

  shrunkmat <- mat[,to_keep]

  #Rename the columns so that everything after the hyphen is removed
  colnames(shrunkmat) <- gsub('-.+', '', colnames(shrunkmat))
  #merge the duplicate columns (drop=F is required so that it can still rowsum columns which are unique)
  shrunkmat <- sapply(unique(colnames(shrunkmat)), function(x) rowSums(shrunkmat[,grepl(x, colnames(shrunkmat)), drop = F]))


  outlist <- list(shrunkmat, shrunkmeta)
  names(outlist) <- c('shrunkmat', 'shrunkmeta')
  return(outlist)
}
