
#' Ggplot2 bubbles by numerical variable
#'
#'
#' @name gg_bubbles_GlnGltNum
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gln-Glt-Num
#' @export
#' @examples
#' gg_bubbles_GlnGltNum(sample_data("Gln-Glt-Num", nrow = 10))
gg_bubbles_GlnGltNum <- function(data = NULL, ...) {

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- geomagic_prep(data = data, opts = opts)

  g <- gg_basic_points(l)+
    do.call("coord_map", gg_projections(l$projections)) +
    add_ggmagic_theme(l$theme) +
    gg_graticule(l$graticule) +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption, size =  l$titles$legend) #+
    # geom_text(l$data, aes(a, b, label = labels),
    #           check_overlap = TRUE, size = l$text$size,
    #           colour = l$text$colour, family = l$text$family)

  add_branding_bar(g, opts$theme)
}



#' Ggplot2 bubbles by numerical variable
#'
#'
#' @name gg_bubbles_GlnGlt
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gln-Glt
#' @export
#' @examples
#' gg_bubbles_GlnGlt(sample_data("Gln-Glt", nrow = 10))
gg_bubbles_GlnGlt <- gg_bubbles_GlnGltNum
