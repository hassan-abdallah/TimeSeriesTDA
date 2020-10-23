#' Distance Matrix Function
#'
#' This function computes a distance matrix between all persistence diagrams using the Wasserstein distance. 
#' @param diags A list of persistence diagrams. 
#' @keywords 
#' @export
#' @examples
#' distance_mat()



distance_mat <- function(diags) {

  dim <- length(diags)

  dist_mat <- matrix(nrow = dim, ncol = dim)

  for(i in 1:(length(diags)-1)){

    for(j in (i+1):length(diags)){

      dist_mat[i,j] <- wasserstein(diags[[i]],diags[[j]],2)

      dist_mat[j,i] <- dist_mat[i,j]

      dist_mat[i,i] <- 0 

    }

   

  }

 dist_mat[length(diags),length(diags)] <- 0 

 return(dist_mat)

}