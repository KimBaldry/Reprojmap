#' @title Transform a raster for plotting
#'
#' @author Kimberlee Baldry
#' @description This function changes the coordinate extent of a raster from (extent 0, 360)
#' to (extent -180, 180).
#'
#' @note In development
#'
#' @return A new raster that has been reprojected
#' @param rast The raster to be transformed
#' @param plot_extent plot extent. Maybe the extent of a raster to be plotted?
#' @param proj the projection the plot will be in
#'
#' @export

change_lon = function(x){
  x1 <- raster::crop(x, extent(0, 180, -90, -30))
  x2 <- raster::crop(x, extent(180, 360, -90, -30))
  xmin(x2) = -180
  xmax(x2) = 0
  x <- base::merge(x1, x2)
}
