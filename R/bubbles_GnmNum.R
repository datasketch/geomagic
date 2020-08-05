
#' Ggplot bubbless by numerical variable
#'
#' @name gg_bubbles_GnmNum
#' @param data A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm-Num
#' @export
#' @examples
#' gg_bubbles_GnmNum(sample_data("Gnm-Num", nrow = 10))
gg_bubbles_GnmNum <- function(data = NULL, ...) {

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- geomagic_prep(data = data, opts = opts)

  g <- gg_basic_bubbles(l)  +
    do.call("coord_map", gg_projections(l$projections)) +
    add_ggmagic_theme(l$theme) +
    gg_graticule(l$graticule) +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption, size =  l$titles$legend)
  if (l$text$show) {
   g <- g + geom_text(data = l$centroides, aes(lon, lat, label = labels),
              check_overlap = TRUE, size = l$text$size,
              colour = l$text$colour, family = l$text$family)
  }
  add_branding_bar(g, opts$theme)
}


#' Ggplot bubbless by numerical variable
#'
#' @name gg_bubbles_Gnm
#' @param data A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm
#' @export
#' @examples
#' gg_bubbles_Gnm(sample_data("Gnm", nrow = 10))
gg_bubbles_Gnm <- gg_bubbles_GnmNum
