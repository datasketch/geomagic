
#' Ggplot bubbless by categorical variable
#'
#' @name gg_bubbles_GnmCatNum
#' @param data A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm-Cat-Num
#' @export
#' @examples
#' gg_bubbles_GnmCatNum(sample_data("Gnm-Cat-Num", nrow = 10))
gg_bubbles_GnmCatNum <- function(data = NULL, ...) {

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- geomagic_prep(data = data, opts = opts)

  g <- gg_basic_bubbles(l)  +
    coord_map(gg_projections(l$projections)) +
    add_ggmagic_theme(l$theme) +
    gg_graticule(l$graticule) +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption, size =  l$titles$legend) +
    geom_text(data = l$centroides, aes(lon, lat, label = labels),
              check_overlap = TRUE, size = l$text$size,
              colour = l$text$colour, family = l$text$family) +
    gg_palette(opts = l$legend, map_type = "bubbles")

  g
}


#' Ggplot bubbless by numerical variable
#'
#' @name gg_bubbles_Gnm
#' @param data A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm
#' @export
#' @examples
#' gg_bubbles_GnmCat(sample_data("Gnm-Cat", nrow = 10))
gg_bubbles_GnmCat <- gg_bubbles_GnmCatNum
