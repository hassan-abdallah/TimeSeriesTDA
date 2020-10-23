#' Monte Carlo Simulation Function
#'
#' This function computes a p-value using a monte carlo simulation with block sampling.  
#' @param distancemat A distance matrix. The output of distance_mat()
#' @param grouplabels A set of labels(indices) for one of the two groups in experiment. 
#' @param N	      The number of permutations to be performed. 
#' @param blocks      The number of exchangeable blocks. Usually the number of epochs in an experiment.
#' @param blocksize   The size of each block, only give one value. 
#' @keywords 
#' @export
#' @examples
#' montecarlo_sim_block()



montecarlo_sim_block <- function(distancemat, grouplabels, N,blocks,blocksize){


  dist <- vector()


  F_observed <- jointloss(distancemat,grouplabels)

  dist[1] <- F_observed
  

  Z <- 1
 

  n1 <- length(grouplabels)
  

  n2 <- nrow(distancemat) - length(grouplabels)
 

  total <- n1+n2
 

  for(i in 1:N-1){
  

    segs <- sample(1:(blocks/2),blocks/4)


    newlabels <- vector()
   

    for(i in segs){

      if(i == 1){

        newlabels <- c(newlabels,1:(blocksize))

      } else {

        newlabels <- c(newlabels,((i-1)*blocksize+1):(((i-1)*blocksize)+(blocksize)))

      }

    }


    segs <- sample(((blocks/2)+1):blocks,blocks/4)
    

    for(i in segs){


      if(i == 1){

        newlabels <- c(newlabels,1:(blocksize))

      } else {

        newlabels <- c(newlabels,((i-1)*blocksize+1):(((i-1)*blocksize)+(blocksize)))

      }

    }

    

    F_sim <- jointloss(distancemat, newlabels)


    dist[i+1] <- F_sim


    if(F_sim <= F_observed) {

     

      Z <- Z+1

      

    }

   

    

  }
  

  p_value <- Z/(N+1)


  return(p_value)

  

}

 