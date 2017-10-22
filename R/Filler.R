#' PCA.Filler
#'
#' Takes a matrix with NAs and returns a matrix without NAs.
#' Takes data.mat and calculates and returns predicted data.mat. Uses by default a 2 component PCA model.
#' No centering or scaling. Supports NAs and similar. Rownames/colnames are retained.
#' Columns/rows with only NAs (or similar) are removed.
#'
#' @usage PCA.Filler(data.mat, num.of.pcs = 2)
#' @param data.mat a numeric matrix or data frame. Long, wide and squared matrix supported. NAs supported.
#' @param num.of.pcs is an integer specifying the number of components to be used. Default 2.
#' @examples data.mat <- airquality[, 1:4]
#' data.mat
#' PCA.Filler(data.mat)
#' @author Berntsson, Martin <martin.berntsson@eon.se> and de Saint-Aubain, Philip Anton <philip-anton.desaint-aubain@knowit.dk>
#' @export
PCA.Filler <- function(data.mat, num.of.pcs = 2 ){


  # Start with testing the input #
  ################################

  # Make sure that input is matrix
  data.mat <- as.matrix(data.mat)

  # Test which columns that contain only finites
  data.mat[!is.finite(data.mat)] <- NA
  data.mat<- data.mat[rowSums(!is.na(data.mat)) > 0, colSums(!is.na(data.mat)) > 0]

  # Test number
  if(length(num.of.pcs) != 1) {
    stop("num.of.pcs must be a number !")
  }
  # Finite
  if(!is.finite(num.of.pcs)){
    stop("num.of.pcs must be a number !")
  }
  # test integer
  if (num.of.pcs %% 1 != 0) {
    stop("num.of.pcs must be an integer  !")
  }

  if ( min(dim(data.mat)) < num.of.pcs ) {
    stop("Too many PCs !")
  }
  if ( num.of.pcs < 1 ) {
    stop("Too few PCs !")
  }


  # Warning if you enter the code, it is at your own risk. There might be dragons.

  # Principal component analysis
  pca.inf <- mixOmics::pca(data.mat, ncomp = num.of.pcs, scale = F, center = F)


  t <- matrix(pca.inf$variates[[1]], ncol=dim(pca.inf$variates[[1]])[2])
  p <- t(matrix(pca.inf$loadings[[1]], ncol=dim(pca.inf$loadings[[1]])[2]))

  data.mat<- t %*% p


  # Secret easter egg
  if(sample(1:100,1) == 100){
    cat(rep(("OPLS rocks ! \U002665 "), 65))
  }


return(data.mat)


}



