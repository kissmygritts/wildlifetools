#' Generate a random movement simmulation
#'
#' @return a randomly generated movement simulation function
#' @export
#'
#' @examples
#' f <- randMoveSim()
#' f[[1]]()
randMvmtSim <- function() {
  sten <- function()  return(c(runif(1, 0, 100), runif(1, 0, 50)))
  nlocs <- function() return(runif(1, 100, 250) %>% ceiling())
  hrand <- function() return(runif(1, .5, .99))
  rrand <- function() return(runif(1, 0, .75))
  murand <- function() return(c(runif(1, 0, .2), runif(1, 0, .2)))
  rsig <- function() {
    r <- function() {
      runif(1, 0, 2)
    }

    c(r(), r(), r(), r()) %>% matrix(ncol = 2)
  }

  moveFunList <- list(
    bb = function () adehabitatLT::simm.bb(date = 1:nlocs(),
                                           begin = sten(),
                                           end = sten(),
                                           id = 'bb'),
    brown = function() adehabitatLT::simm.brown(date = 1:nlocs(),
                                                x0 = sten(),
                                                id = 'brown'),
    crw = function() adehabitatLT::simm.crw(date = 1:nlocs(),
                                            x0 = sten(),
                                            h = hrand(),
                                            r = rrand(),
                                            id = 'crw'),
    mba = function() adehabitatLT::simm.mba(date = 1:nlocs(),
                                            x0 = sten(),
                                            mu = murand(),
                                            id = 'mba')
  )

  return(moveFunList[runif(1, 0, 4) %>% ceiling()])
}
