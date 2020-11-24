test_that("DYNPROG returns a max.segs by signal length cost matrix and a
            max.segs by max.segs changepoint matrix", {
  signal <- c( runif(25, min=0, max=2), runif(25, min=2, max=5) )
  num.max.segs <- 10
  DYNPROG.fit <- DYNPROG( signal, num.max.segs )
  cost.mat <- DYNPROG.fit$cost
  change.mat <- DYNPROG.fit$change

  expect_equal( nrow( cost.mat ), num.max.segs )
  expect_equal( ncol( cost.mat ), length( signal ) )
  expect_equal( nrow( change.mat ), num.max.segs )
  expect_equal( ncol( change.mat ), num.max.segs )
})
