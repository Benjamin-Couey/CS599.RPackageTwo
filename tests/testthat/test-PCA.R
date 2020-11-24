test_that("PCA returns a number of features by number of components rotation
          matrix and a number of data points by number of components lambda
          matrix", {
  test.matrix <- as.matrix( iris[ 1:50, 1:4 ] )
  PCA.fit <- PCA( test.matrix )
  max.components <- min( nrow( test.matrix ), ncol( test.matrix ) )
  rotation.mat <- PCA.fit$rotation
  lambda.mat <- PCA.fit$lambda

  expect_equal( nrow( rotation.mat ), ncol( test.matrix ) )
  expect_equal( ncol( rotation.mat ), max.components )

  expect_equal( nrow( lambda.mat ), nrow( test.matrix ) )
  expect_equal( ncol( lambda.mat ), max.components )
})
