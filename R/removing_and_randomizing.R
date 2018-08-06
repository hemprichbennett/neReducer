removing_and_randomizing <- function(network, index, network_level = 'higher', sums_to_preserve = 'both', datatype='list', nreplicates= 1000){


  #' calculates how much of a network's perceived structure is due to the influence of a given species
  #'
  #' This function removes each species in turn, randomizing the resulting matrix and returning the
  #' desired metric value for the resulting random subnetworks
  #'
  #' @param network Your input object
  #' @param index The network-level metric, from bipartite, which you desire to be calculated
  #' @param network_level The network level to be removing and randomizing. Currently only 'higher' works
  #' @param sums_to_preserve preserve sums of columns, rows, or both
  #' @param datatype is it a matrix of interactions, or a list of matrixes?
  #' @param nreplicates How many iterations of the randomisation should be done per species removal
  #' @return gives a dataset of values when you remove a species from a network, and the values obtained
  #' when randomising this subnetwork
  #' @examples
  #' mat <- matrix(nrow =10, ncol =10, sample(c(0,1), 100, prob = c(0.6, 0.4), replace = T))
  #' removing_and_randomizing(network = mat, index = 'modularity', datatype = 'matrix')
  #' @export

  call_reduce <- function(net, ind, network_level = 'higher', sums_to_preserve = 'both', datatype='list'){
    if(!datatype %in% c('matrix', 'list')){
      stop('error, datatype can only be matrix or list')
    }
    if(datatype == 'matrix'){
      out <- lapply(colnames(net), function(x) reduce_and_randomize(sp = x, net = net, index_used = ind))
      out <- do.call(rbind, out)
    }
    if(datatype == 'list'){
      #Make a list of lists, where each item at the first level corresponds to a network, at the second to an output from reduce_and_randomize
      out_list_1 <- lapply(nets, function(n) lapply(colnames(n), function(x) reduce_and_randomize(sp = x, net = n, index_used = ind)))

      #Collapse down the nested list to a regular list
      out_list_2 <- lapply(out_list_1, function(x) do.call(rbind, x))

      #Give the list items their network names prior to rbinding them
      for(i in 1:length(out_list_2)){
        out_list_2[[i]]$network <- names(out_list_2)[i]
      }

      out <- do.call(rbind, out_list_2)
      colnames(out)[2] <- 'actual'
    }
    return(out)
  }

  reduce_and_randomize <-function(sp, net, index_used, network_level = 'higher', sums_to_preserve = 'both'){
    #This function removes a single specified column, randomizes the matrix, calculates a given metric,
    #Then returns the values

    #Remove the desired column from the network
    sp_col <- which(colnames(net)==sp)
    sub_net <- net[,-sp_col]
    #Calculate the 'actual' value for when that species is missing

    if(index_used != 'modularity'){
      actual <- networklevel(sub_net, index = index_used, level = network_level)
      #Calculate the random values
      rand_vals <- replicate(nreplicates, bipartite::networklevel(vegan::permatswap(sub_net, fixedmar=sums_to_preserve,mtype="count",times=1, method="quasiswap")$perm[[1]],
                                                           index = index_used, level = network_level))
    }
    if(index_used == 'modularity'){

      actual <- slot(bipartite::computeModules(web=sub_net), 'likelihood')


      rand_vals <- replicate(nreplicates,slot(bipartite::computeModules(web = vegan::permatswap(sub_net, fixedmar=sums_to_preserve,mtype="count",times=1, method="quasiswap")$perm[[1]]), 'likelihood'))

    }

    quants <- quantile(rand_vals, probs=c(0.025, 0.975))

    df <- data.frame(rand_vals, 'actual' = as.numeric(actual), sp, 'lower'= as.numeric(quants[1]), 'upper'= as.numeric(quants[2]), index_used)

    return(df)
  }

  outdf <- call_reduce(net = network, ind = index, network_level = network_level, sums_to_preserve = sums_to_preserve, datatype = datatype)

  return(outdf)
}
