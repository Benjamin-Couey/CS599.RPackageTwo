#' Determines the optimal points in a signal to split so as to minimize cost.
#'
#' @param signal The sequence of values to split
#' @param max.segs The maximum number of segments the sequence should be split
#'                     into
#'
#' @return A list which contains the following elements is returned:
#'             cost: A max.segs by signal length matrix where the element at
#'             position [i,j] is the optimal cost for splitting the signal,
#'             up to index j, into i segments.
#'             change: A max.segs by max.segs triangular matrix where the
#'             element in position [i,j] is the index of the jth optimal
#'             changepoint when splitting the whole signal into i segments.
#'
#' @export
#'
#' @examples
#' signal <- c( runif(25, min=0, max=2), runif(25, min=2, max=5) )
#' DYNPROG.fit <- DYNPROG( signal, 10 )
#' DYNPROG.fit$cost
#' DYNPROG.fit$change
DYNPROG <- function( signal, max.segs ) {
  # Initialize the matrix which will contain the optimal costs
  cost.mat <- matrix( nrow = max.segs, ncol = length( signal ) )

  # Initialize the matrix which will contain the optimal changepoints
  change.mat <- matrix( nrow = max.segs, ncol = max.segs )

  # Calculate the cumulative sum and cumulative sum of squares for
  # use in the optimal cost function
  Q.vec <- c( 0, cumsum( signal^2 ) )
  S.vec <- c( 0, cumsum( signal ) )

  # Initialize the first row of the cost matrix with the optimal cost of
  cost.mat[ 1, ] <- optimal.cost( Q.vec, S.vec, 1, 1:length( signal ) )

  # Initialize the diagonal of the change matrix with the last index of the
  # signal
  diag( change.mat ) <- length( signal )

  for( iteration in 2:max.segs ) {

    # Obtain the costs from the previous iteration
    previous.cost <- cost.mat[ iteration-1, ]

    # Handle the trivial case
    cost.mat[ iteration, 1:iteration-1 ] <- 0

    for( stopping.index in iteration:length( signal ) ) {

      # t is the maximum index we will consider for this iteration
      # The range of 1 to t will be split at the changepoint t'
      # All values to the 'left' of t' will be taken from the previous row of
      # cost.mat . All values to the 'right' of t' will be calculated with
      # optimal.cost
      changepoints <- 1:(stopping.index-1)

      # Calculate the new cost for indices past t' for all t' less than t
      new.cost <- optimal.cost( Q.vec, S.vec, changepoints+1, stopping.index )

      # Add the appropriate portion of the previous iteration and save it to the
      # cost matrix
      sums.vec <- previous.cost[ changepoints ] + new.cost

      cost.mat[ iteration, stopping.index ] <-
        min( sums.vec )
    }

    # Now, calculate the changepoints for this number of segments

    # Initialize the change
    change.index <- 1

    while( change.index < iteration ) {
      # Start with the last set of changepoints, and determine which one gave the
      # best result. Save that index in the change mat
      change.mat[ iteration, iteration - change.index  ] <- which.min( sums.vec )

      # Recalculate the best changepoint for the portion of the signal to the
      # 'left' of the changepoint we just saved.
      stopping.index <- change.mat[ iteration, iteration - change.index  ]

      changepoints <- 1:(stopping.index-1)

      # Calculate the new cost for indices past t' for all t' less than t
      new.cost <- optimal.cost( Q.vec, S.vec, changepoints+1, stopping.index )

      # Add the appropriate portion of the previous iteration and save it to the
      # cost matrix
      sums.vec <- previous.cost[ changepoints ] + new.cost

      # Iterate the offset, so we will consider the changepoint we just computed
      # next iteration
      change.index <- change.index + 1
    }

  }

  return( list( cost=cost.mat, change=change.mat ) )
}

#' A helper function which determines the optimal cost for a given subsignal
#'
#' @param Q.vec The cumulative sum of squares for the full signal
#' @param S.vec The cumulative sum for the full signal
#' @param start.index The first index in the subsignal, inclusive
#' @param end.index The last index in the subsignal, inclusive
#'
#' @return The optimal cost for the subsignal
#'
#' @export
#'
#' @examples
#' signal <- c( runif(25, min=0, max=2),
#'              runif(25, min=2, max=5) )
#' Q.vec <- c( 0, cumsum( signal^2 ) )
#' S.vec <- c( 0, cumsum( signal ) )
#' cost <- optimal.cost( Q.vec, S.vec, 1, length( signal ) )
optimal.cost <- function( Q.vec, S.vec, start.index, end.index ) {
  end <- end.index + 1
  return( Q.vec[ end ] - Q.vec[ start.index ]
          - ( S.vec[ end ] - S.vec[ start.index ] )^2 / ( end - start.index ) )
}
