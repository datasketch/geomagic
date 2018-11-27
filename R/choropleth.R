#' Choropleth map
#' Choropleth map
#' @name gg_choropleth_map_GcdNum.
#' @param x A code.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gcd-Num
#' @examples
#' NULL

gg_choropleth_map_GcdNum. <- function(
  data = NULL,
  mapName = "world_countries",
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  legend = list(),
  border = list(),
  labelWrap = 12,
  agg = 'sum',
  fill = list(),
  marks = c(".", ","),
  nDigits = NULL,
  projections = list(),
  percentage = FALSE,
  format = c('', ''),
  showText = c(TRUE, TRUE),
  theme = NULL) {

  projectionsDefault <- list(ratio = NULL,
                             type = NULL,
                             orientation = c(90, 0, 0))
  projections <- modifyList(projectionsDefault, projections)

  legendDefault <- list(bins = 7,
                        limit = 0.5,
                        mode = 'quantile',
                        position = 'left',
                        title = NULL,
                        background = 'transparent',
                        fill = 'transparent')
  legend <- modifyList(legendDefault, legend)

  borderDefault <- list(weigth = 0.25,
                        color = '#000000')
  border <- modifyList(borderDefault, border)

  fillDefault <- list(color = NULL,
                      opacity = 0.7,
                      scale = 'continuous',
                      nullColor = '#eeeeee',
                      background = '#ffffff')
  fill <- modifyList(fillDefault, fill)

  mapResults <- layerMap(mapName = mapName,
                         borderColor = border$color,
                         borderWeigth = border$weigth,
                         fillColor = fill$nullColor,
                         fillOpacity = fill$opacity)
  data_map <- mapResults[[1]]
  graph <- mapResults[[2]]

  if (!is.null(data)) {

    f <- fringe(data)
    nms <- getClabels(f)
    data <- f$d
    flab <- legend$title %||% nms[2]
    data$a <- as.character(data$a)
    data_map$a <- as.character(data_map$id)

    data <- data %>%
      group_by(a) %>%
      dplyr::summarise(b = agg(agg, b))

    if (percentage) {
      data$b <- (data[['b']] * 100) / sum(data[['b']], na.rm = TRUE)
    }

    data_graph <- binsLeg(data = data,
                          col = 'b',
                          scale = fill$scale,
                          mode = legend$mode,
                          bins = legend$bins,
                          percent = percentage,
                          nDigits = nDigits,
                          marks = marks,
                          format = format,
                          colors = fill$color,
                          dataLeft =  data_map
    )


    g <- graph +
      geom_map(data = data_graph, map = data_graph,
               aes(map_id = id, x = long, y = lat, fill = bins),
               color = border$color, size = 0.25)

    if (fill$scale == 'continuous') {
      g <- g + scale_fill_gradientn(
        aesthetics = "fill",
        colours = as.character(unique(data_graph$color)),
        labels =  as.character(unique(data_graph$labels[!is.na(data_graph$labels)])),
        breaks =  as.numeric(unique(data_graph$breaks[!is.na(data_graph$breaks)])),
        limits = c(min(data_graph$breaks, na.rm = T) - legend$limit, max(data_graph$breaks, na.rm = T) + legend$limit))
    } else {
      g <- g + scale_fill_manual(values = as.character(unique(data_graph$color)))
    }
    g <- g +
      labs(x = "",
           y = "",
           title = title,
           subtitle = subtitle,
           caption = caption,
           fill= flab)

  } else {
    g <- graph
  }

  if (!is.null(projections$ratio)) g <- g + coord_equal(ratio= projections$ratio)
  if (!is.null(projections$type)) g <- g  + coord_map(projections$type, orientation = projections$orientation)

  g + theme(legend.position= legend$position,
            plot.background = element_rect(fill = fill$background, linetype = 'blank'),
            panel.background = element_rect(fill = fill$background,
                                            colour = fill$background,
                                            size = 1.5,
                                            linetype = 'blank'),
            legend.background = element_rect(colour = legend$background,
                                             fill = legend$fill))
}
