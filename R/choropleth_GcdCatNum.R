
#' Ggplot choropleths by categorical variable
#'
#' @name gg_choropleth_GcdCatNum
#' @param data A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd-Cat-Num
#' @export
#' @examples
#' gg_choropleth_GcdCatNum(sample_data("Gcd-Cat-Num", nrow = 1000))
gg_choropleth_GcdCatNum <- function(data = NULL, ...){

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- geomagic_prep(data = data, opts = opts, by_col = "id")

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
#' @name gg_choropleth_GcdCat
#' @param data A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd-Cat
#' @export
#' @examples
#' gg_choropleth_GcdCat(sample_data("Gcd-Cat", nrow = 10))
gg_choropleth_GcdCat <- gg_choropleth_GcdCatNum
