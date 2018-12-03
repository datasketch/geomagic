#' Bubbles map
#' Bubbles map
#' @name gg_bubbles_map_GcdNum.
#' @param x A code.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gcd-Num
#' @examples
#' NULL
gg_bubbles_map_GcdNum. <- function(data = NULL,
                                   mapName = "world_countries",
                                   title = NULL,
                                   subtitle = NULL,
                                   caption = NULL,
                                   minSize = 0.1,
                                   maxSize = 10,
                                   legend = list(),
                                   border = list(),
                                   titleStyle = list(),
                                   labelWrap = 12,
                                   agg = 'sum',
                                   fill = list(),
                                   marks = c(".", ","),
                                   nDigits = NULL,
                                   projections = list(),
                                   percentage = FALSE,
                                   format = c('', ''),
                                   theme = NULL) {


  if (!mapName %in% availableGeodata()) {
    stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
  }

  projectionsDefault <- list(ratio = NULL,
                             type = NULL,
                             orientation = c(90, 0, 0))
  projections <- modifyList(projectionsDefault, projections)

  legendDefault <- list(position = 'left',
                        bins = 5,
                        title = NULL,
                        background = 'transparent',
                        fill = 'transparent',
                        showLeg = TRUE)
  legend <- modifyList(legendDefault, legend)

  borderDefault <- list(weigth = 0.25,
                        color = '#000000')
  border <- modifyList(borderDefault, border)

  titleStyleDefault <- list( color = '#000000',
                             sizeTitle = 13,
                             sizeSubtitle = 11,
                             sizeCaption = 9)
  titleStyle <- modifyList(titleStyleDefault, titleStyle)

  fillDefault <- list(color = NULL,
                      naColor = 'orange',
                      opacity = 0.7,
                      scale = 'continuous',
                      nullColor = '#eeeeee',
                      background = '#ffffff',
                      showText = c(FALSE,FALSE),
                      optText = 'code',
                      propText = 'onlyData',
                      sizeText = 1)
  fill <- modifyList(fillDefault, fill)

  mapResults <- layerMap(mapName = mapName,
                         borderColor = border$color,
                         borderWeigth = border$weigth,
                         fillColor = fill$nullColor,
                         fillOpacity = fill$opacity)
  data_map <- mapResults[[1]]
  graph <- mapResults[[2]]
  centroides <- mapResults[[3]]


  if(!is.null(data)){
    f <- fringe(data)
    nms <- getClabels(f)
    data <- f$d
    flab <- legend$title %||% nms[2]
    data$a <- as.character(toupper(tolower(data$a)))
    data_map$a <- as.character(data_map$id)

    nDig <- 2
    if (!(is.null(nDigits))) nDig <- nDigits

    data <- data  %>%
      dplyr::group_by(a) %>%
      dplyr::summarise(b = ifelse(sum(is.na(b) == length(b)), b, agg(agg, b)))

    if (percentage) {
      data$b <- (data[['b']] * 100) / sum(data[['b']], na.rm = TRUE)
    }

    centroides$a <- centroides$id
    data_graph <- dplyr::inner_join(data, centroides, by = "a")
    data_graph$c <- '1'
    data_graph <- fillColors(data_graph, 'c', fill$color, 'discrete', NA, NA, labelWrap, F)
    data_graph <- data_graph %>% drop_na(b)
    breaks <- round(as.vector(quantile(unique(data_graph$b), probs = c(0, cumsum(rep((1/(legend$bins-1)), (legend$bins-1)))))), nDig)
    labels <- paste0( format[1] ,format(breaks, trim = T,  big.mark = marks[1], decimal.mark = marks[2]), format[2])
    g <- graph + geom_point(data = data_graph,
                            aes(x=lon, y=lat, size=b),
                            color = as.character(unique(data_graph$color)),
                            alpha = fill$opacity) +
      scale_size(
        name = flab,
        range = c(minSize, maxSize),
                 breaks = breaks,
                 labels = labels)
    if (!legend$showLeg) g <- g + guides(size = legend$showLeg)

  }else{
    g <- graph
  }

  if (!is.null(projections$ratio)) g <- g + coord_equal(ratio= projections$ratio)
  if (!is.null(projections$type)) g <- g  + coord_map(projections$type, orientation = projections$orientation)

  g <- g + theme(legend.position= legend$position,
                 plot.title = element_text(color= titleStyle$color, size= titleStyle$sizeTitle),
                 plot.subtitle = element_text(color=titleStyle$color, size= titleStyle$sizeSubtitle),
                 plot.caption = element_text(color= titleStyle$color, size= titleStyle$sizeCaption),
                 plot.background = element_rect(fill = fill$background, linetype = 'blank'),
                 panel.background = element_rect(fill = fill$background,
                                                 colour = fill$background,
                                                 size = 1.5,
                                                 linetype = 'blank'),
                 legend.background = element_rect(colour = legend$background,
                                                  fill = legend$fill))
  if (sum(fill$showText) != 0) {
    if (fill$optText == 'code') centroides$name <- centroides$id
    if (!is.null(data)) {
      data$b <- round(data$b, ifelse(is.null(nDigits), 2, nDigits))
      centroides <- left_join(centroides, data, by = c('id' = 'a'))
      if (sum(fill$showText) == 2) centroides$name <- paste0(centroides$name, '\n', centroides$b)
      if (sum(fill$showText) == 1 && fill$showText[1]) centroides$name <- centroides$name
      if (sum(fill$showText) == 1 && fill$showText[2]) centroides$name <- centroides$b
    }

    if (fill$propText == 'all') {
      g <- g + geom_text(data = centroides,
                         aes(label = name, x = lon, y = lat,
                             check_overlap = TRUE), size = fill$sizeText)
    } else if (fill$propText == 'onlyData') {
      if(!is.null(data)){
        dat_text <- data.frame(id = data$a)
        dat_text$id <- as.character(dat_text$id)
        dat_text <- dat_text %>% dplyr::inner_join(., centroides, by = c("id"))
        g <- g + geom_text(data = dat_text,
                           aes(label = name, x = lon, y = lat,
                               check_overlap = TRUE), size = fill$sizeText)
      }else{
        g <- g
      }
    } else
      if (!is.null(data)) {
        dat_text <- data.frame(id = data$a)
        dat_text <- sample_frac(dat_text, fill$propText)
        dat_text$id <- as.character(dat_text$id)
        dat_text <- dat_text %>% dplyr::inner_join(., centroides, by = c("id"))
        g <- g + geom_text(data = dat_text,
                           aes(label = name, x = lon, y = lat,
                               check_overlap = TRUE), size = fill$sizeText)
      } else {
        g <- g
      }
  }

  g + labs(x = "",
           y = "",
           title = title,
           subtitle = subtitle,
           caption = caption)

}



#' Bubbles map
#' Bubbles map
#' @name gg_bubbles_map_GcdNum.
#' @param x A code.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gcd-Num
#' @examples
#' NULL
gg_bubbles_map_GcdCatNum. <- function(
                                   data = NULL,
                                   mapName = "world_countries",
                                   title = NULL,
                                   subtitle = NULL,
                                   caption = NULL,
                                   minSize = 0.3,
                                   maxSize = 10,
                                   legend = list(),
                                   border = list(),
                                   titleStyle = list(),
                                   labelWrap = 12,
                                   agg = 'sum',
                                   fill = list(),
                                   marks = c(".", ","),
                                   nDigits = NULL,
                                   projections = list(),
                                   percentage = FALSE,
                                   format = c('', ''),
                                   theme = NULL) {

  if (!mapName %in% availableGeodata()) {
    stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
  }

  projectionsDefault <- list(ratio = NULL,
                             type = NULL,
                             orientation = c(90, 0, 0))
  projections <- modifyList(projectionsDefault, projections)

  legendDefault <- list(position = 'left',
                        bins = 5,
                        title = c(NULL, NULL),
                        showLeg = c(TRUE, TRUE),
                        background = 'transparent',
                        fill = 'transparent')
  legend <- modifyList(legendDefault, legend)

  borderDefault <- list(weigth = 0.25,
                        color = '#000000')
  border <- modifyList(borderDefault, border)

  titleStyleDefault <- list( color = '#000000',
                             sizeTitle = 13,
                             sizeSubtitle = 11,
                             sizeCaption = 9)
  titleStyle <- modifyList(titleStyleDefault, titleStyle)

  fillDefault <- list(color = NULL,
                      naColor = 'orange',
                      opacity = 0.7,
                      scale = 'continuous',
                      nullColor = '#eeeeee',
                      background = '#ffffff',
                      showText = c(FALSE,FALSE),
                      optText = 'code',
                      propText = 'onlyData',
                      sizeText = 1)
  fill <- modifyList(fillDefault, fill)

  mapResults <- layerMap(mapName = mapName,
                         borderColor = border$color,
                         borderWeigth = border$weigth,
                         fillColor = fill$nullColor,
                         fillOpacity = fill$opacity)
  graph <- mapResults[[2]]
  centroides <- mapResults[[3]]

  if (!is.null(data)) {

    f <- fringe(data)
    nms <- getClabels(f)
    data <- f$d
    flab1 <- legend$title[1] %||% nms[2]
    flab2 <- legend$title[2] %||% nms[3]
    data$a <- as.character(toupper(tolower(data$a)))


    data <- data  %>%
      group_by(a, b) %>%
      dplyr::summarise(c = agg(agg, c)) %>%
      arrange(-c) %>%
      mutate(ind = 1:length(b)) %>%
      filter(ind == 1) %>%
      select(a, b, c)


    if (percentage) {
      data$c <- (data[['c']] * 100) / sum(data[['c']], na.rm = TRUE)
    }

    nDig <- 2
    if (!is.null(nDigits)) nDig <- nDigits

    centroides$a <- centroides$id
    data_graph <- dplyr::inner_join(data, centroides, by = "a")
    data_graph <- fillColors(data_graph, 'b', fill$color, 'discrete', NA, NA, labelWrap, F)
    data_graph <- data_graph %>% drop_na(c)
    breaks <- round(as.vector(quantile(unique(data_graph$c), probs = c(0, cumsum(rep((1/(legend$bins-1)), (legend$bins-1)))))), nDig)
    labels <- paste0( format[1] ,format(breaks, trim = T,  big.mark = marks[1], decimal.mark = marks[2]), format[2])
    g <- graph + geom_point(data = data_graph,
                            aes(x=lon, y=lat, size=c, color = b ),
                            alpha = fill$opacity) +
      scale_size(
        name = flab2,
        range = c(minSize, maxSize),
        breaks = breaks,
        labels = labels) +
      scale_color_manual(
        name = flab1,
        values = as.character(unique(data_graph$color)),
                        na.value = fill$naColor)
    if (!legend$showLeg[1]) g <- g + guides(color = legend$showLeg[1])
    if (!legend$showLeg[2]) g <- g + guides(size = legend$showLeg[2])


  }else{
    g <- graph
  }

  if (!is.null(projections$ratio)) g <- g + coord_equal(ratio= projections$ratio)
  if (!is.null(projections$type)) g <- g  + coord_map(projections$type, orientation = projections$orientation)

  g <- g + theme(legend.position= legend$position,
                 plot.title = element_text(color= titleStyle$color, size= titleStyle$sizeTitle),
                 plot.subtitle = element_text(color=titleStyle$color, size= titleStyle$sizeSubtitle),
                 plot.caption = element_text(color= titleStyle$color, size= titleStyle$sizeCaption),
                 plot.background = element_rect(fill = fill$background, linetype = 'blank'),
                 panel.background = element_rect(fill = fill$background,
                                                 colour = fill$background,
                                                 size = 1.5,
                                                 linetype = 'blank'),
                 legend.background = element_rect(colour = legend$background,
                                                  fill = legend$fill))
  if (sum(fill$showText) != 0) {
    if (fill$optText == 'code') centroides$name <- centroides$id
    if (!is.null(data)) {
      data$b <- round(data$b, ifelse(is.null(nDigits), 2, nDigits))
      centroides <- left_join(centroides, data, by = c('id' = 'a'))
      if (sum(fill$showText) == 2) centroides$name <- paste0(centroides$name, '\n', centroides$b)
      if (sum(fill$showText) == 1 && fill$showText[1]) centroides$name <- centroides$name
      if (sum(fill$showText) == 1 && fill$showText[2]) centroides$name <- centroides$b
    }

    if (fill$propText == 'all') {
      g <- g + geom_text(data = centroides,
                         aes(label = name, x = lon, y = lat,
                             check_overlap = TRUE), size = fill$sizeText)
    } else if (fill$propText == 'onlyData') {
      if(!is.null(data)){
        dat_text <- data.frame(id = data$a)
        dat_text$id <- as.character(dat_text$id)
        dat_text <- dat_text %>% dplyr::inner_join(., centroides, by = c("id"))
        g <- g + geom_text(data = dat_text,
                           aes(label = name, x = lon, y = lat,
                               check_overlap = TRUE), size = fill$sizeText)
      }else{
        g <- g
      }
    } else
      if (!is.null(data)) {
        dat_text <- data.frame(id = data$a)
        dat_text <- sample_frac(dat_text, fill$propText)
        dat_text$id <- as.character(dat_text$id)
        dat_text <- dat_text %>% dplyr::inner_join(., centroides, by = c("id"))
        g <- g + geom_text(data = dat_text,
                           aes(label = name, x = lon, y = lat,
                               check_overlap = TRUE), size = fill$sizeText)
      } else {
        g <- g
      }
  }

  g + labs(x = "",
           y = "",
           title = title,
           subtitle = subtitle,
           caption = caption)

}
