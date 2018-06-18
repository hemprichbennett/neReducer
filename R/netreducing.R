netreducing <- function(input, input_type, n_iterations=100, min_nodes, metric_chosen, type_chosen, level = NA, collapse_cols =T){
  #' calculate variance in network level metrics caused by sample size used
  #'
  #' Acts as a wrapper for the \code{\link{shrink}} function. IMPORTANT: the columns of your matrix must be named speciesname-samplename,
  #' with a hyphen separating them. E.g. species1-sample1, species2-sampleb, etc etc. The hyphen is used to split the names and so is (currently)
  #' non-negotiable.
  #'
  #' @param input Your input object
  #' @param input_type The type of your input object. Can be either 'matrix' or 'list'
  #' @param n_iterations The number of iterations you wish to perform per number of nodes selected
  #' @param min_nodes The minimum number of columns to be allowed in the matrix tested. For example, if you input a matrix with ten
  #' columns and set a min_nodes of 5, the function will test your network with 5, 6, 7, 8, and 9 nodes.
  #' @param metric_chosen The metric you would like to analyse. Can be any value allowed by bipartite for the networklevel
  #' or specieslevel functions, or if 'modularity' is chosen, it will use computemodules to calculate modularity
  #' @param type_chosen The type of metric desired. Can either be 'network' for a networklevel metric, 'species' for
  #' a specieslevel metric, or 'modularity' for modularity
  #' @param level The network level desired to be analysed.
  #' @param collapse_cols Should the columns be collapsed by species? I.e. is this a species-based or individual-based analysis
  #' @return Produces a dataframe showing the changes in a given metric when varying the number of nodes in your matrix
  #' @export
  #' @examples pending



  if(!input_type %in% c('matrix', 'list')){
    stop('input_type can only be matrix or list')
  }

  if(type_chosen=='species' & !level %in% c('higher', 'lower')){
    stop('error, for species level metrics you must specify \'level\' as either higher or lower')
  }





  #A little function to pass values to shrink in a slightly easier manner
  setup <- function(iteration, net, netname=NA, n_col){
    if(collapse_cols==T){
      return(shrink(net, n_cols = n_col, itval = iteration, netname = netname, metric = metric_chosen, metric_type = type_chosen, network_level = level, collapse = T))
    }
    if(collapse_cols==F){
      return(shrink(net, n_cols = n_col, itval = iteration, netname = netname, metric = metric_chosen, metric_type = type_chosen, network_level = level, collapse = F))
    }


  }



  #if a list is input

  #A horrendous giant nested apply, but it works
  if(input_type=='list'){
    list_output <-  sapply(seq(1,n_iterations), function(it)
      sapply(seq_along(input), function(y)
        lapply(seq(min_nodes,ncol(input[[y]])-1), function(n) setup(iteration = it, net = input[[y]], netname = names(input)[y], n_col = n))) )
    #trial, see the dim of the output items
    #lapply(list_output, function(x) print(dim(x)))
    #make it into a dataframe
    #for(i in 1: length(list_output)){print(dim(list_output[[i]]))}
    bigdf <- do.call(rbind, lapply(list_output, function(x) do.call(rbind, x)))
    #bigdf <- do.call(rbind, list_output)
    return(bigdf)
    #return(list_output)
  }





  #If a single matrix is input
  if(input_type == 'matrix'){
    tem <- lapply(seq(1, n_iterations), function(it) lapply(seq(min_nodes,ncol(input)-1), function(n) setup(iteration = it, net = input, n_col = n)))

    #make it into a dataframe
    outdf <- do.call(rbind, lapply(tem, function(x) do.call(rbind, x)))
    return(outdf)
  }



}


