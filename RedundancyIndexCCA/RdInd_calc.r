# canonical correlation analysis 
###!!!! the matrix with the highest number of variables must be in the second place (to double check)
#' RdInd_calc
#'
#' @param inputs matrix of model inputs or factors used in the analysis
#' @param outputs matrix of model outputs
#'
#' @return a matrix with the redundancy indeces: for each model output there is the variance explained by each inputs
#' @export 
#'
#' @examples
RdInd_calc <- function(inputs, outputs){
  require(CCA)
  ccaRes <- cc(inputs, outputs)
  
  ####calculate the redundancy index using the 
  # the canonical loadings of model outputs
  # the canonical cross-loadings of model inputs
  # canonical correlations
  Rc <- ccaRes$cor
  ne <- ncol(outputs)
  nFactors <- ncol(inputs)
  ncolX <- min(ne,nFactors)
  
  RdOut <- matrix(0,ne,ncolX)
  RdIn <- matrix(0,ne,nFactors)
  for(j in 1:ne) RdOut[j,] <- (ccaRes$scores$corr.Y.yscores[j,])^2 * (Rc)^2
  
  xx <- array(NA,dim=c(ne,ncolX,nFactors))
  for(j in 1:ne){
    for(k in 1:ncolX){
      xx[j,k,]=RdOut[j,k]*ccaRes$scores$corr.X.yscores[,k]^2
    }
  }
  RdIn <- apply(xx,c(1,3),sum)
  colnames(RdIn) <- paste("input",1:nFactors)
  rownames(RdIn) <- paste("output",1:ne)
  return(RdIn)  
}
