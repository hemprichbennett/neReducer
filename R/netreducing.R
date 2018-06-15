netreducing <- function(input, input_type, n_iterations=100, min_nodes, metric_chosen, type_chosen, level = NA){
  #' calculate variance in network level metrics caused by sample size used
  #'
  #' Acts as a wrapper for the \code{\link{shrink}} function
  #'
  #' @param input Your input
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

    return(shrink(net, n_cols = n_col, itval = iteration, netname = netname, metric = metric_chosen, metric_type = type_chosen, network_level = level))
  }



  #if a list is input

  #A horrendous giant nested apply, but it works
  if(input_type=='list'){
    list_output <-  sapply(seq(1,n_iterations), function(it)
      sapply(seq_along(input), function(y)
        lapply(seq(min_nodes,ncol(input[[y]])-1), function(n) setup(iteration = it, net = input[[y]], netname = names(input)[y], n_col = n))) )

    #make it into a dataframe
    bigdf <- do.call(rbind, list_output)
    return(bigdf)
  }





  #If a single matrix is input
  if(input_type == 'matrix'){
    tem <- lapply(seq(1, n_iterations), function(it) lapply(seq(min_nodes,ncol(input)-1), function(n) setup(iteration = it, net = input, n_col = n)))

    #make it into a dataframe
    outdf <- do.call(rbind, lapply(tem, function(x) do.call(rbind, x)))
    return(outdf)
  }



}


