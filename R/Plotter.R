#' PCA.Plotter
#'
#' Makes the basic PCA plots on a given matrix. Scores and loadings scatterplots of PC1 vs PC2.
#' Centers and UV-scales the variables, makes a 2 component PCA. Supports NAs but requires variance
#' in all variables. Presents R2 for each component. Uses the package "mixOmics". Rownames/colnames
#' are used as point labels.
#'
#' @usage PCA.Plotter(data.mat)
#' @param data.mat a numeric matrix or data frame. Long, wide and squared matrix supported. NAs supported. Variance required for all variables.
#' @examples set.seed(5)
#' num.of.variables <- 14
#' num.of.observations <- 34
#' rn <- rnorm(num.of.variables * num.of.observations)
#' data.mat <- matrix(rn, nrow = num.of.observations, ncol = num.of.variables)
#' PCA.Plotter(data.mat)
#' @author Berntsson, Martin <martin.berntsson@eon.se> and de Saint-Aubain, Philip Anton <philip-anton.desaint-aubain@knowit.dk>
#' @export
PCA.Plotter <- function(data.mat){

  # Start with testing the input #
  ################################

  # Test which columns that are scaleable (has a variance and it is > 0)
  col.var <- apply(data.mat, MARGIN = 2, FUN = var, na.rm = TRUE)
  not.scaleable <- which(col.var == 0 | is.na(col.var))


  # if there is columns that is not scaleable
  L <- length(not.scaleable)
  if(L > 0){
    # Remove the columns that are not scaleable and give a warning
    data.mat <- data.mat[, -not.scaleable]
    warning(paste0("The variable(s)/column(s) ", not.scaleable, " cannot be used in the calculation and has been removed"))
  }
  ################################


  # Warning if you enter the code, it is at your own risk. There might be dragons.

  # Principal component analysis
  pca.res <- mixOmics::pca(data.mat, ncomp = 2, scale = TRUE, center = TRUE)

  # Define Colors
  colorGreen <- rgb(red = 188, green = 238, blue = 104, alpha = 150, maxColorValue = 255)
  colorGray <- "lightgray"

  # Plot parameters
  op <- par(no.readonly = TRUE)
  pch <- 15
  par(mfrow = c(1, 2))

  # Plot Scores
  ###############

  # Scores - limits
  limS1 <- signif(max(abs(range(pca.res$x[,1]))) * 1.1, 2)
  limS2 <- signif(max(abs(range(pca.res$x[,2]))) * 1.1, 2)

  # Plot
  par(cex=1)
  plot(pca.res$x[,1], pca.res$x[,2], col = colorGreen, pch = pch, xlim = c(-limS1, limS1), ylim = c(-limS2, limS2), axes = FALSE, xlab = "", ylab = "")

  # Axes and grid
  axis(side = 1, at = c(-limS1, -limS1/2 , 0, limS1/2, limS1))
  axis(side = 2, at = c(-limS2, -limS2/2 , 0, limS2/2, limS2))

  abline(h = c(-limS2, -limS2/2, 0, limS2/2, limS2), col = colorGray, lty = 3)
  abline(v = c(-limS1, -limS1/2, 0, limS1/2, limS1), col = colorGray, lty = 3)
  grid(2, 2, col = 'black', lty = 1)

  # Title and text
  title(main = 'Scores', xlab = "t[1]", ylab = "t[2]")
  par(cex = 0.6)
  text(pca.res$x[,1], pca.res$x[,2], pca.res$names$sample, pos = 4)

  # Show explained variance
  #################
  mtext(paste0("R2X[1]=",signif(pca.res$explained_variance[1],2), "            R2X[2]=",signif(pca.res$explained_variance[2],2)), side=1, line=4.2, font=1, cex=1)

  # Plot Loadings
  #################

  # Loadings - limits
  limL1 <- signif(max(abs(range(pca.res$rotation[,1]))) * 1.1, 2)
  limL2 <- signif(max(abs(range(pca.res$rotation[,2]))) * 1.1, 2)

  # Plot
  par(cex=1)
  plot(pca.res$rotation[,1], pca.res$rotation[,2], col = colorGreen, pch = pch, xlim = c(-limL1, limL1), ylim = c(-limL2, limL2), axes = FALSE, xlab = "", ylab = "")

  # Axes and grid
  axis(side = 1, at = c(-limL1, -limL1/2 , 0, limL1/2, limL1))
  axis(side = 2, at = c(-limL2, -limL2/2 , 0, limL2/2, limL2))

  abline(h = c(-limL2, -limL2/2, 0, limL2/2, limL2), col = colorGray, lty = 3)
  abline(v = c(-limL1, -limL1/2, 0, limL1/2, limL1), col = colorGray, lty = 3)
  grid(2, 2, col = 'black', lty = 1)

  # Title and text
  title(main = 'Loadings', xlab = "p[1]", ylab = "p[2]")
  par(cex = 0.6)
  text(pca.res$rotation[,1], pca.res$rotation[,2], pca.res$names$X, pos = 4)

  # reset plot parameters
  par(op)

  # Secret easter egg
  if(sample(1:100,1) == 100){
    cat(rep(("We love PCA \U002665 "), 65))
  }
}



