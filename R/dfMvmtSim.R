#' Generate data.frame from movement simulation
#'
#' @param n an integer. The number of random movement simulations to run.
#'
#' @return a \code{data.frame} of all the simulations.
#' @export
#'
#' @examples
#' df <- dfMvmtSim(5)
#' View(df)
dfMvmtSim <- function(n) {
  dat <- data.frame()

  for (i in 1:n) {
    sim <- randMvmtSim()
    spdf <- adehabitatLT::ltraj2spdf(sim[[1]]())
    df <- cbind(spdf@coords, spdf@data) %>%
      dplyr::select(x, y, date) %>%
      dplyr::mutate(id = i,
                    method = names(sim))
    dat <- rbind(dat, df)
  }

  return(dat)
}
