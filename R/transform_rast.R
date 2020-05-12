#' @title Transform a raster for plotting
#'
#' @author Kimberlee Baldry
#' @description This function transforms a raster for plotting in a new projection space.
#'
#' @note In development
#'
#' @return A new raster that has been reprojected
#' @param rast The raster to be transformed
#' @param plot_extent plot extent in plot projection. Maybe the extent of a raster to be plotted?
#' @param proj the projection the plot will be in
#'
#' @export


transform_rast = function(rast,proj,plot_extent, rf = 2){

# equal area grid for plot extent
extent_ster = plot_extent
emptyrast = raster(extent_ster,nrow = nrow(rast)*rf, ncol = nrow(rast)*rf)
projection(emptyrast) <- proj

# transform to new projection and interpolate to grid
rastdf_ster = raster::projectRaster(rast,emptyrast,method = "bilinear", crs = proj)
data_ster_rast = as(rastdf_ster,"SpatialPointsDataFrame")
data_ster_rast = data.frame(data_ster_rast)

return(data_ster_rast)}

