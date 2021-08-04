#' Monte Carlo Simulation Function
#'
#' This function computes a p-value using a monte carlo simulation with block sampling.  
#' @param distancemat A distance matrix. The output of distance_mat()
#' @param grouplabels A set of labels(indices) for one of the two groups in experiment. 
#' @param levels   Two-column matrix encoding index information for two-level point-cloud grouping. First column is whole-block shuffling partition and second column is within-block shuffling partition. 
#' @param N	      The number of permutations to be performed. 
#' @keywords 
#' @export
#' @examples
#' montecarlo_sim_block()



montecarlo_sim_block <- function(distancemat, grouplabels,levels, N){
  
  
  dist <- vector()
  
  
  F_observed <- jointloss(distancemat,grouplabels)
  
  dist[1] <- F_observed
  
  
  Z <- 1
  
  
  n1 <- length(grouplabels)
  
  
  n2 <- nrow(distancemat) - length(grouplabels)
  
  
  total <- n1+n2
  
  
  for(i in 1:N-1){
    
    
    
    newlabels <- vector()
    
    
    lev_ind <- list()
    
    for(i in 1:max(levels[,2])){
      
      lev_ind[[i]] <- unique(levels[which(levels[,2] == i),1])
      
    }
    
    new_label_ind <- c()
    for(i in 1:max(levels[,2])){
      
      len <- length(lev_ind[[i]])
      
      new_label_ind <- c(new_label_ind,sample(lev_ind[[i]],len/2))
      
    }
    
    newlabels <- which(levels[,1] %in% new_label_ind)
    
    #print(newlabels)
    
    F_sim <- jointloss(distancemat, newlabels)
    
    
    dist[i+1] <- F_sim
    
    
    if(F_sim <= F_observed) {
      
      
      
      Z <- Z+1
      
      
      
    }
    
    
    
    
    
  }
  
  
  p_value <- Z/(N+1)
  
  
  return(p_value)
  
  
  
}


 
