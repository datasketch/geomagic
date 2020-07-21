
#' Ggplot choropleths by categorical variable
#'
#' @name gg_choropleth_GnmCatNum
#' @param data A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm-Cat-Num
#' @export
#' @examples
#' gg_choropleth_GnmCatNum(sample_data("Gnm-Cat-Num", nrow = 1000))
gg_choropleth_GnmCatNum <- function(data = NULL, ...){

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- geomagic_prep(data = data, opts = opts)

  g <- gg_basic_choropleth(l) +
    coord_map(gg_projections(l$projections)) +
    add_ggmagic_theme(l$theme) +
    gg_graticule(l$graticule) +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption, fill =  l$titles$legend) +
    geom_text(data = l$centroides, aes(lon, lat, label = labels),
              check_overlap = TRUE, size = l$text$size,
              colour = l$text$colour, family = l$text$family) +
    gg_palette(opts = l$legend)

  g
  #add_branding_bar(g, l$theme)
}



#' Ggplot choropleths by categorical variable
#'
#' @name gg_choropleth_GnmCat
#' @param data A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm-Cat
#' @export
#' @examples
#' gg_choropleth_GnmCat(sample_data("Gnm-Cat", nrow = 10))
gg_choropleth_GnmCat <- gg_choropleth_GnmCatNum
