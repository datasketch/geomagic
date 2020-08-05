
#' Ggplot choropleths by numerical variable
#'
#' @name gg_choropleth_GcdNum
#' @param data A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd-Num
#' @export
#' @examples
#' gg_choropleth_GcdNum(sample_data("Gcd-Num", nrow = 10))
gg_choropleth_GcdNum <- function(data = NULL, ...){

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- geomagic_prep(data = data, opts = opts, by = "id")

  g <- gg_basic_choropleth(l) +
    do.call("coord_map", gg_projections(l$projections)) +
    add_ggmagic_theme(l$theme) +
    gg_graticule(l$graticule) +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption, fill =  l$titles$legend)

  if (l$text$show) {
    g  <- g + geom_text(data = l$centroides, aes(lon, lat, label = labels),
              check_overlap = TRUE, size = l$text$size,
              colour = l$text$colour, family = l$text$family)
  }
   g <- g + gg_palette(opts = l$legend)

  add_branding_bar(g, l$theme)
}


#' Ggplot choropleths by numerical variable
#'
#' @name gg_choropleth_Gcd
#' @param data A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd
#' @export
#' @examples
#' gg_choropleth_Gcd(sample_data("Gcd", nrow = 10))
gg_choropleth_Gcd <- gg_choropleth_GcdNum
