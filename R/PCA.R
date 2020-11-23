# Description - Performs basic principal component analysis on the given data
#               matrix
# Arguments - data.mat : The data set to run PCA on
# Returns - A list which contains the following elements is returned:
#                   rotation: the matrix of weights for the original variables
#                   lambda: the matrix of lambda values
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
