#' @title Create a boarder and coastline polygons for a plot
#'
#' @author Kimberlee Baldry
#' @description This function returns data frames for a plot
#' boarder and coastlined that can be used in ggplot, based on a plot extent and projection.
#' This works for stereographic and curved segment plots
#'
#' @note In development
#'
#' @return Two data frames that coresponds to the plot boarder and coastlines
#' @param extent plot extent. Maybe the extent of a raster to be plotted?
#' @param proj_str the projection the plot will be in
#' @param resolution the resolution of the data to be plotted
#' @param scale_res a scalar to help woth increasing the boarder resolution
#' @param circle is the boarder a circle?
#'
#' @export



map_plot_proj = function(extent,proj_str,resolution, scale_res = 100, circle = F){
  if(circle){
    clip_lat = round(extent@ymax)
    trim_points = data.frame("x" = seq(-180,179.5,0.25), "y" = rep(clip_lat,length(seq(-180,179.5,0.25))))
    coordinates(trim_points) <- ~x+y
    projection(trim_points) <- "+proj=longlat +datum=WGS84"
    border_ster = spTransform(trim_points,proj_str)
    extent_ster = extent(border_ster)
    border_ster = as.data.frame(border_ster)[-1,]
    trim_poly = Polygons(list(Polygon(border_ster)), "ID")
    border_ster_poly = SpatialPolygons(list(trim_poly), proj4string = CRS(proj_str))
  }else{
  # set up plot border
  plot_poly = data.frame("x" = c(extent@xmin,extent@xmin,extent@xmax,extent@xmax),
                         "y"= c(extent@ymin,extent@ymax,extent@ymax, extent@ymin))
  border_points = rbind(plot_poly[1,],
                        data.frame("x" = plot_poly[1,"x"],"y" = seq(plot_poly[1,"y"],plot_poly[2,"y"],by = round((extent@ymax-extent@ymin)/(scale_res*resolution), digits = 2))),
                        plot_poly[2,],
                        data.frame("x" = seq(plot_poly[2,"x"],plot_poly[3,"x"],by = round((extent@xmax-extent@xmin)/(scale_res*resolution), digits = 2)),"y" = plot_poly[2,"y"]),
                        plot_poly[3,],
                        data.frame("x" = plot_poly[3,"x"],"y" = seq(plot_poly[3,"y"],plot_poly[4,"y"],by = -round((extent@ymax-extent@ymin)/(scale_res*resolution), digits = 2))),
                        plot_poly[4,],
                         data.frame("x" = seq(plot_poly[4,"x"],plot_poly[1,"x"],by = -round((extent@xmax-extent@xmin)/(scale_res*resolution), digits = 2)),"y" = plot_poly[4,"y"]))
  # add latlon projection
  coordinates(border_points) = ~x+y
  projection(border_points) = "+proj=longlat +datum=WGS84"
  # transform to new projection
  border_ster = spTransform(border_points,proj_str)
  # new extent
  extent_ster = extent(border_ster)
  # make spatial polygon
  border_ster = data.frame(border_ster)
  colnames(border_ster) = c("x","y")
  border_ster = data.frame(polysimplify(border_ster)[[1]])[-1,]
  border_ster_poly = Polygons(list(Polygon(border_ster)), "ID")
  border_ster_poly = SpatialPolygons(list(border_ster_poly), proj4string = CRS(proj_str))}


  ### Set up coastlines for plots **** change to call from a package
  # import coastlines
  coastlines = Reprojmap::coastlines
  # reproject coastlines
  coastlines = spTransform(coastlines,proj_str)
  # convert to spatial polygons
  coastlines = as(coastlines, "SpatialPolygons")
  # extract coordinates and simplify polygons that are within plot bounds to remove self intersction
  coastlines = lapply(c(1:length(coastlines)) ,function(x){data.frame("x" = coastlines@polygons[[x]]@Polygons[[1]]@coords[,1], "y" = coastlines@polygons[[x]]@Polygons[[1]]@coords[,2])})
  coast_polys1 = lapply(c(1:length(coastlines)),function(x){
    if(length(which(coastlines[[x]]$x < extent_ster@xmax)) > 1 & length(which(coastlines[[x]]$x > extent_ster@xmin)) > 1 &
       length(which(coastlines[[x]]$y < extent_ster@ymax)) > 1 & length(which(coastlines[[x]]$y  > extent_ster@ymin)) > 1  ){
      SpatialPolygons(list(Polygons(list(Polygon(coastlines[[x]])),"ID")), proj4string = CRS(proj_str))}else{NA}})
  coast_polys1 =coast_polys1[which(is.na(coast_polys1) ==F)]
  # compute intersecting polygon for each indevidual polygon - by doing by individual polygon we prevent the polygons breaking
  coast_polys2 = lapply(c(1:length(coast_polys1)) ,function(x){rgeos::gIntersection(coast_polys1[[x]],border_ster_poly, checkValidity = 2L)})
  coast_polys3 = lapply(c(1:length(coast_polys2)), function(x){if(is.null(coast_polys2[[x]]) == F){coast_polys2[[x]]}else{NA}})
  coast_polys3 =coast_polys3[which(is.na(coast_polys3) ==F)]
  # set up polygon IDs
  for(sp in c(1:length(coast_polys3))){ coast_polys3[[sp]]@polygons[[1]]@ID = as.character(sp)}
  # convert cropped polygons to spatial polygons
  coast_polys4 = SpatialPolygons(lapply(coast_polys3, function(x){x@polygons[[1]]}))
  coast_polysdf = as(coast_polys4, "SpatialPolygonsDataFrame")
  coast_polysdf@data$dummy = c(1:length(coast_polysdf@data$dummy))
  coast_polysdf = as(coast_polysdf, "SpatialLinesDataFrame")
  coast_polysdf = as(coast_polysdf, "SpatialPointsDataFrame")
  coast_polysdf = data.frame(coast_polysdf)[,c("dummy","x","y")]
  colnames(coast_polysdf)[1] = "group"
  # return list including border and coastlines
  out = list()
  out$border = border_ster
  out$coastlines = coast_polysdf
  return(out)
}
