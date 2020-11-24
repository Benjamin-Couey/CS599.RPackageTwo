#' Performs basic principal component analysis on the given data matrix
#'
#' @param data.mat The matrix of data to perform PCA on
#'
#' @return A list which contains the following elements is returned:
#'             rotation: the matrix of weights for the original variables
#'             lambda: the matrix of lambda values
#'
#' @export
#'
#' @examples
#' test.matrix <- as.matrix( iris[ 1:50, 1:4 ] )
#' PCA.fit <- PCA( test.matrix )
#' PCA.fit$rotation
#' PCA.fit$lambda
PCA <- function( data.mat ) {
  # First, standardize the data matrix so all variables will have an equal
  # impact on the PCA calculation
  stand.mat <- scale( data.mat, center = TRUE, scale = FALSE )

  # Run single value decomposition on the standardized data matrix
  single.val.decomp <- svd( stand.mat )

  # Calculate the principal components as ud and return the rotation matrix r
  list(
    "rotation"=single.val.decomp$v,
    "lambda"=single.val.decomp$u %*% diag( single.val.decomp$d )
  )
}
