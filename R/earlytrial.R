library(DataExplorer)
library(reshape2)
trialmat1 <- matrix(sample(c(0,1),100, replace = T), nrow = 10)
colnames(trialmat1) <- c('Hice-001', 'Hice-002',	'Rhtr-001',	'Rhtr-008',	 'Keha-0005', 'Keha-01A', 'Hice-005', 'Rhtr-010', 'Hidi-001', 'Hice-087')
trialmat1
trialmat2 <- matrix(sample(c(0,1),100, replace = T), nrow = 10)
colnames(trialmat2) <- c('Hidy-001', 'Hidy-002',	'Rhtr-001',	'Rhtr-008',	 'Keha-0005', 'Keha-01A', 'Hidy-005', 'Rhtr-010', 'Hidi-001', 'Hidy-087')

input_list <- list('trialmat1' = trialmat1, 'trialmat2' = trialmat2)

#write.csv(trialmat1, '~/Desktop/example_matrix.csv')

source('R/shrink.R')




#A little function to pass values to shrink in a slightly easier manner
setup <- function(iteration, net, netname=NA, n_col){
  #cat('iteration is', iteration, '\n')
  #cat('n_col is', n_col, '\n')
  #iteration <- 1
  #cat('netname is', netname, '\n')

  return(shrink(net, n_cols = n_col, itval = iteration, netname = netname, metric = 'connectance'))
}



#Inputting a list

#A horrendous giant nested apply, but it works
n_iterations <- 100
list_output <-  sapply(seq(1,n_iterations), function(it)
  sapply(seq_along(input_list), function(y)
    lapply(seq(5,ncol(input_list[[y]])-1), function(n) setup(iteration = it, net = input_list[[y]], netname = names(input_list)[y], n_col = n))) )


bigdf <- do.call(rbind, list_output)




#Inputting a single matrix


mat_output <- sapply(seq(1,n_iterations), function(it)
    sapply(seq(5,ncol(trialmat1)), function(n) setup(iteration = it, net = trialmat1, n_col = n)))


tem <- lapply(seq(1, n_iterations), function(it) lapply(seq(5,ncol(trialmat1)-1), function(n) setup(iteration = it, net = trialmat1, n_col = n)))

outdf <- do.call(rbind, lapply(tem, function(x) do.call(rbind, x)))
