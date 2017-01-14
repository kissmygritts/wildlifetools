#' Plot animal trajectories
#'
#' @param df the \code{data.frame} to plot
#' @param xyi a vector of the x coordinates, y coordinates and id for each animal
#'
#' @return
#' @export
#'
#' @examples
#' df <- dfMvmtSim(2)
#' ggTraj(df, c('x', 'y', 'id'))
ggTraj <- function(df, xyi = c('x', 'y', 'id')) {
  x <- xyi[1]
  y <- xyi[2]
  id <- xyi[3]
  df[, id] <- factor(df[, id])

  ggplot2::ggplot(df, ggplot2::aes_string(x = x, y = y, color = id)) +
    ggplot2::geom_point(alpha = .4) +
    ggplot2::geom_path(ggplot2::aes_string(group = id), alpha = .3) +
    ggplot2::theme_void() +
    ggthemes::scale_color_gdocs() +
    ggplot2::coord_equal()
}
