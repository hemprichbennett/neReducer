
trialmat <- matrix(sample(c(0,1),100, replace = T), nrow = 10)
colnames(trialmat) <- c('Hice-001', 'Hice-002',	'Rhtr-001',	'Rhtr-008',	 'Keha-0005', 'Keha-01A', 'Hice-005', 'Rhtr-010', 'Hidi-001', 'Hice-087')

shrink <- function(mat, n_cols){

  #Decide which rows to keep and which to discard
  to_keep <- sample(seq(1, ncol(mat)), n_cols)
  print(to_keep)
  #Make a dataframe of all of their metadata
  batmetadata <- data.frame(strsplit(colnames(mat), split = '-'))
  batmetadata <- t(batmetadata)
  batmetadata <- as.data.frame(batmetadata)
  colnames(batmetadata) <- c('Species', 'ID')
  #print(batmetadata)
  batmetadata$orig <- colnames(mat)
  batmetadata$orig <- as.factor(batmetadata$orig)

  batmetadata$included <- rep(NA, nrow(batmetadata))
  batmetadata$included[to_keep] <- 'included'
  batmetadata$included[-to_keep] <- 'not_included'
  batmetadata$included <- as.factor(batmetadata$included)
  return(batmetadata)
}

a <- shrink(mat = trialmat, n_cols = 4)

