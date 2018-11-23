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
gg_choropleth_map_GcdNum <- function(
                              data = NULL,
                              mapName = "world_countries",
                              title = NULL,
                              subtitle = NULL,
                              caption = NULL,
                              legend = list(),
                              border = list(),
                              labelWrap = 12,
                              fill = list(),
                              marks = c(".", ","),
                              nDigits = NULL,
                              projections = NULL,
                              percentage = FALSE,
                              format = c('', ''),
                              showText = c(TRUE, TRUE),
                              theme = NULL) {

  legendDefault <- list(bins = 7,
                        position = "bottomleft",
                        title = NULL)
  legend <- modifyList(legendDefault, legend)

  borderDefault <- list(weigth = 0.25,
                        color = '#000000')
  border <- modifyList(borderDefault, border)

  fillDefault <- list(color = '#EEF1F2',
                      opacity = 0.7,
                      scale = 'no',
                      nullColor = '#C2C4C4' )
  fill <- modifyList(fillDefault, fill)

  mapResults <- layerMap(mapName = mapName)
  data_map <- mapResults[[1]]
  graph <- mapResults[[2]]


  #if (!is.null(data))

  f <- fringe(data)
  nms <- getClabels(f)
  flab <- opts$fillLabel %||% nms[2]
  data <- f$d

  data$a <- as.character(data$a)
  data_map$a <- as.character(data_map$id)

  data <- data %>% group_by(a) %>% dplyr::summarise(total = mean(b))

  data_graph <- dplyr::inner_join(data, data_map, by = "a")


  levels(data_graph$bins)


  graph2 <- graph +
    geom_map(data = data_graph, map = data_graph,
             aes(map_id = id, x = long, y = lat, fill = bins),
             color = border$color, size = 0.25)

  graph2 + scale_fill_manual(values = as.character(unique(data_graph$color)))


  graph2 + scale_fill_gradientn(
    colours = c('#000000', '#FDF123', '#FFFFFF'),
    labels = c('$-250', 'cero', 'hia', '250', '500'),
    breaks = c(-250,  0, 200, 250, 500))


  dta2 <- binsLeg(data_graph, 'total', 'discrete', 'otra', 5, NULL, NULL, NULL )


   g <- graph +
    geom_map(data = dta2, map = dta2,
             aes(map_id = id, x = long, y = lat, fill = bins),
             color = border$color, size = 0.25)

 g + scale_fill_manual(values = as.character(unique(dta2$color)),
                       labels = c('lab1', 'lab2', 'lab3', 'lab4', 'aff'))

}
