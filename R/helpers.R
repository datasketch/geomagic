#' Basic layer choroplets
gg_basic_choropleth <- function(l) {
  #if (is.null(l$data)){
    g <- ggplot(data = l$d) +
      geom_polygon(aes( x = long, y = lat, group = group),
                   fill = l$theme$na_color,
                   color= l$theme$border_color)
  # } else {
  #   g <- ggplot(data = l$d) +
  #     geom_polygon(aes( x = long, y = lat, group = group, fill = b), color="grey")
  # }
  g
}
