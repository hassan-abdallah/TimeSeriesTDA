#' Joint Loss Function
#'
#' This function computes our test statistic under the given labelling scheme. 
#' @param distancemat A distance matrix, the output of distance_mat().
#' @param grouplabels A vector of labels for one of the two groups.  
#' @keywords 
#' @export
#' @examples
#' jointloss()



jointloss <- function(distancemat,grouplabels){
  
  dim_seq <- 1:nrow(distancemat)
  
  labels <- list()
  
  labels[[1]] <- grouplabels
  
  labels[[2]] <- dim_seq[-grouplabels]
  
  solution <- 0   
  
  
  for(m in 1:2){
    
    sum1 <- 0
    
    n_m <- length(labels[[m]])
    
    for(i in labels[[m]]){
      
      sum2 <- 0 
      
      for(j in labels[[m]]){
        
        sum2 <- sum2 + distancemat[i,j]
        
      }
      
      sum1 <- sum1 + sum2 
    }
    
    solution <- solution + (1/(2*n_m*(n_m-1)))*sum1 
  }
  
  return(solution)
  
}