#' @title Get package data
#'
#' @author Kimberlee Baldry
#' @description This function returns data for ice shelves and coastlines
#'
#' @note In development
#'
#' @return A shape object
#' @param type IceShelf or CoastLines
#'
#' @export



get_data = function(type = "IceShelf"){
  data.path = system.file("data", package = "Reprojmap")
  if(type  == "IceShelf" ){ name = ""}
  if(type == "CoastLines"){name = "Coastlines_USGS_gen01.rda"}
  load(file.path(data.path,name))
}

