
#' Ggplot choropleths by numerical variable
#'
#' @name gg_choropleth_GnmNum
#' @param data A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm-Num
#' @export
#' @examples
#' gg_choropleth_GnmNum(sampleData("Gnm-Num", nrow = 10))
gg_choropleth_GnmNum <- function(data = NULL, ...){

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- geomagic_prep(data = data, opts = opts)

  g <- gg_basic_choropleth(l) +
        coord_map(gg_projections(l$projections))
  g
}
