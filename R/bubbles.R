#' Bubbles map
#' Bubbles map
#' @name gg_bubbles_GcdNum
#' @param x A code.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gcd-Num
#' @examples
#' NULL
gg_bubbles_GcdNum <- function(data = NULL,
                              mapName = "world_countries",
                              opts = NULL) {
  if(!mapName %in% availableMaps())
    stop("No map with that name, check available maps with availableMaps()")

  opts <- getOpts(opts = opts)

  mapResults <- layerMap(mapName = mapName,
                         borderColor = opts$border_color,
                         borderWeigth =opts$border_width,
                         fillColor = opts$default_color,
                         fillOpacity = opts$opacity)

  graph <- mapResults[[2]]
  centroides <- mapResults[[3]]

  if(!is.null(data)){
    f <- fringe(data)
    nms <- getClabels(f)
    data <- f$d
    flab <- opts$legend_title %||% nms[2]
    data$a <- as.character(toupper(tolower(data$a)))

    nDig <- 2
    if (!(is.null(opts$nDigits))) nDig <- opts$nDigits

    data <- data  %>%
      dplyr::group_by(a) %>%
      dplyr::summarise(b = ifelse(sum(is.na(b) == length(b)), b, agg(opts$agg, b)))

    if (opts$percentage) {
      data$b <- (data[['b']] * 100) / sum(data[['b']], na.rm = TRUE)
    }

    centroides$a <- centroides$id
    data_graph <- dplyr::inner_join(data, centroides, by = "a")
    data_graph$c <- '1'
    data_graph <- fillColors(data_graph, 'c', opts$color, 'discrete', NA, NA, opts$labelWrap, F)
    data_graph <- data_graph %>% drop_na(b)
    breaks <- round(as.vector(quantile(unique(data_graph$b), probs = c(0, cumsum(rep((1/(opts$nLevels-1)), (opts$nLevels-1)))))), nDig)
    labels <- paste0( opts$format[1] ,format(breaks, trim = T,  big.mark = opts$marks[1], decimal.mark = opts$marks[2], nsmall = nDig), opts$format[2])
    g <- graph + geom_point(data = data_graph,
                            aes(x=lon, y=lat, size=b),
                            color = as.character(unique(data_graph$color)),
                            alpha = opts$opacity) +
      scale_size(
        name = flab,
        range = c(opts$min_radius, opts$max_radius),
        breaks = breaks,
        labels = labels)
    if (!opts$legend_show) g <- g + guides(size = opts$legend_show)

  }else{
    g <- graph
  }

  if (!is.null(opts$projection_ratio)) g <- g + coord_equal(ratio = opts$projection_ratio)
  if (!is.null(opts$projectionName)) g <- g  + coord_map(opts$projectionName, orientation = opts$projection_orientation)

  g <-  g + theme(legend.position= opts$legend_position,
                  plot.title = element_text(color= opts$title_color, size= opts$title_size),
                  plot.subtitle = element_text(color= opts$subtitle_color, size= opts$subtitle_size),
                  plot.caption = element_text(color= opts$caption_color, size= opts$caption_size),
                  plot.background = element_rect(fill = opts$background, linetype = 'blank'),
                  panel.background = element_rect(fill = opts$background,
                                                  colour = opts$background,
                                                  size = 1.5,
                                                  linetype = 'blank'),
                  legend.text=element_text(color=opts$legend_color),
                  legend.background = element_rect(colour = opts$legend_borderColor,
                                                   fill = opts$legend_background))
  if (sum(opts$text_show) != 0) {
    if (opts$text_option == 'code') centroides$name <- centroides$id
    if (!is.null(data)) {
      data$b <- round(data$b, ifelse(is.null(opts$nDigits), 2, opts$nDigits))
      centroides <- left_join(centroides, data, by = c('id' = 'a'))
      if (sum(opts$text_show) == 2) centroides$name <- paste0(centroides$name, '\n', centroides$b)
      if (sum(opts$text_show) == 1 && opts$text_show[1]) centroides$name <- centroides$name
      if (sum(opts$text_show) == 1 && opts$text_show[2]) centroides$name <- centroides$b
    }

    if (opts$text_proportion == 'all') {
      g <- g + geom_text(data = centroides,
                         aes(label = name, x = lon, y = lat,
                             check_overlap = TRUE), size = opts$text_size)
    } else if (opts$text_proportion == 'onlyData') {
      if(!is.null(data)){
        dat_text <- data.frame(id = data$a)
        dat_text$id <- as.character(dat_text$id)
        dat_text <- dat_text %>% dplyr::inner_join(., centroides, by = c("id"))
        g <- g + geom_text(data = dat_text,
                           aes(label = name, x = lon, y = lat,
                               check_overlap = TRUE), size = opts$text_size)
      }else{
        g <- g
      }
    } else
      if (!is.null(data)) {
        dat_text <- data.frame(id = data$a)
        dat_text <- sample_frac(dat_text, opts$text_proportion)
        dat_text$id <- as.character(dat_text$id)
        dat_text <- dat_text %>% dplyr::inner_join(., centroides, by = c("id"))
        g <- g + geom_text(data = dat_text,
                           aes(label = name, x = lon, y = lat,
                               check_overlap = TRUE), size = opts$text_size)
      } else {
        g <- g
      }
  }

  if (opts$graticule) {
    g <- g + theme(panel.grid.major = element_line(colour = opts$graticule_color, linetype = "dashed",
                                                   size = opts$graticule_weight))
  }

  g + labs(x = "",
           y = "",
           title = opts$title,
           subtitle = opts$subtitle,
           caption = opts$caption)

}



#' Bubbles map
#' @name gg_bubbles_GcdNum
#' @param x A code.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gcd-Num
#' @examples
#' NULL
gg_bubbles_GcdCatNum <- function(data = NULL,
                                 mapName = "world_countries",
                                 opts = NULL) {

  if(!mapName %in% availableMaps())
    stop("No map with that name, check available maps with availableMaps()")

  opts <- getOpts(opts = opts)

  mapResults <- layerMap(mapName = mapName,
                         borderColor = opts$border_color,
                         borderWeigth =opts$border_width,
                         fillColor = opts$default_color,
                         fillOpacity = opts$opacity)

  graph <- mapResults[[2]]
  centroides <- mapResults[[3]]

  if (!is.null(data)) {

    f <- fringe(data)
    nms <- getClabels(f)
    data <- f$d
    flab1 <- opts$legend_title[1] %||% nms[2]
    flab2 <- opts$legend_title[2] %||% nms[3]
    data$a <- as.character(toupper(tolower(data$a)))


    data <- data  %>%
      group_by(a, b) %>%
      dplyr::summarise(c = agg(opts$agg, c)) %>%
      arrange(-c) %>%
      mutate(ind = 1:length(b)) %>%
      filter(ind == 1) %>%
      dplyr::select(a, b, c)


    if (opts$percentage) {
      data$c <- (data[['c']] * 100) / sum(data[['c']], na.rm = TRUE)
      opts$format[2] <- '%'
    }

    nDig <- 2
    if (!is.null(opts$nDigits)) nDig <- opts$nDigits

    centroides$a <- centroides$id
    data_graph <- dplyr::inner_join(data, centroides, by = "a")
    data_graph <- fillColors(data_graph, 'b', opts$color, 'discrete', NA, NA, opts$labelWrap, F)
    data_graph <- data_graph %>% drop_na(c)
    breaks <- round(as.vector(quantile(unique(data_graph$c), probs = c(0, cumsum(rep((1/(opts$nLevels-1)), (opts$nLevels-1)))))), nDig)
    labels <- paste0( opts$format[1] ,format(breaks, trim = T,  big.mark = opts$marks[1], decimal.mark = opts$marks[2], nsmall = nDig), opts$format[2])
    g <- graph + geom_point(data = data_graph,
                            aes(x=lon, y=lat, size=c, color = b ),
                            alpha = opts$opacity) +
      scale_size(
        name = flab2,
        range = c(opts$min_radius, opts$max_radius),
        breaks = breaks,
        labels = labels) +
      scale_color_manual(
        name = flab1,
        values = as.character(unique(data_graph$color)),
        na.value = opts$naColor)
    if (!opts$legend_show[1]) g <- g + guides(color = opts$legend_show[1])
    if (!opts$legend_show[2]) g <- g + guides(size = opts$legend_show[2])


  }else{
    g <- graph
  }

  if (!is.null(opts$projection_ratio)) g <- g + coord_equal(ratio = opts$projection_ratio)
  if (!is.null(opts$projectionName)) g <- g  + coord_map(opts$projectionName, orientation = opts$projection_orientation)

  g <-  g + theme(legend.position= opts$legend_position,
                  plot.title = element_text(color= opts$title_color, size= opts$title_size),
                  plot.subtitle = element_text(color= opts$subtitle_color, size= opts$subtitle_size),
                  plot.caption = element_text(color= opts$caption_color, size= opts$caption_size),
                  plot.background = element_rect(fill = opts$background, linetype = 'blank'),
                  panel.background = element_rect(fill = opts$background,
                                                  colour = opts$background,
                                                  size = 1.5,
                                                  linetype = 'blank'),
                  legend.text=element_text(color=opts$legend_color),
                  legend.background = element_rect(colour = opts$legend_borderColor,
                                                   fill = opts$legend_background))
  if (sum(opts$text_show) != 0) {
    if (opts$text_option == 'code') centroides$name <- centroides$id
    if (!is.null(data)) {
      data$c <- round(data$c, ifelse(is.null(opts$nDigits), 2, opts$nDigits))
      centroides <- left_join(centroides, data, by = c('id' = 'a'))
      if (sum(opts$text_show) == 2) centroides$name <- paste0(centroides$name, '\n', centroides$c)
      if (sum(opts$text_show) == 1 && opts$text_show[1]) centroides$name <- centroides$name
      if (sum(opts$text_show) == 1 && opts$text_show[2]) centroides$name <- centroides$b
    }

    if (opts$text_proportion == 'all') {
      g <- g + geom_text(data = centroides,
                         aes(label = name, x = lon, y = lat,
                             check_overlap = TRUE), size = opts$text_size)
    } else if (opts$text_proportion == 'onlyData') {
      if(!is.null(data)){
        dat_text <- data.frame(id = data$a)
        dat_text$id <- as.character(dat_text$id)
        dat_text <- dat_text %>% dplyr::inner_join(., centroides, by = c("id"))
        g <- g + geom_text(data = dat_text,
                           aes(label = name, x = lon, y = lat,
                               check_overlap = TRUE), size = opts$text_size)
      }else{
        g <- g
      }
    } else
      if (!is.null(data)) {
        dat_text <- data.frame(id = data$a)
        dat_text <- sample_frac(dat_text, opts$text_proportion)
        dat_text$id <- as.character(dat_text$id)
        dat_text <- dat_text %>% dplyr::inner_join(., centroides, by = c("id"))
        g <- g + geom_text(data = dat_text,
                           aes(label = name, x = lon, y = lat,
                               check_overlap = TRUE), size = opts$text_size)
      } else {
        g <- g
      }
  }

  if (opts$graticule) {
    g <- g + theme(panel.grid.major = element_line(colour = opts$graticule_color, linetype = "dashed",
                                                   size = opts$graticule_weight))  }

  g + labs(x = "",
           y = "",
           title = opts$title,
           subtitle = opts$subtitle,
           caption = opts$caption)

}





#' Ggplot bubbles by latitud and longitud
#'
#' @name gg_bubbles_GlnGltNum
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gln-Glt-Num
#' @export
#' @examples
#' gg_bubbles_GlnGltNum(sampleData("Gln-Glt-Num", nrow = 10))

gg_bubbles_GlnGltNum <- function(data = NULL,
                                 mapName = "world_countries",
                                 opts = NULL) {

  if(!mapName %in% availableMaps())
    stop("No map with that name, check available maps with availableMaps()")

  opts <- getOpts(opts = opts)

  mapResults <- layerMap(mapName = mapName,
                         borderColor = opts$border_color,
                         borderWeigth =opts$border_width,
                         fillColor = opts$default_color,
                         fillOpacity = opts$opacity)

  graph <- mapResults[[2]]
  centroides <- mapResults[[3]]

  if(!is.null(data)){
    f <- fringe(data)
    nms <- getClabels(f)
    data <- f$d
    flab <- opts$legend_title %||% nms[3]
    data <- data %>% drop_na(a, b)



    nDig <- 2
    if (!(is.null(opts$nDigits))) nDig <- opts$nDigits

    data <- data  %>%
      dplyr::group_by(a, b) %>%
      dplyr::summarise(c = agg(opts$agg, c))

    opts$nLevels <- ifelse(length(unique(data$c)) == 1, 1, opts$nLevels)

    if (opts$percentage) {
      data$c <- (data[['c']] * 100) / sum(data[['c']], na.rm = TRUE)
    }

    data$ind <- 1
    data_graph <- fillColors(data, 'ind', opts$color, 'discrete', NA, NA, opts$labelWrap, F)
    breaks <- round(as.vector(quantile(unique(data_graph$c), probs = c(0, cumsum(rep((1/(opts$nLevels-1)), (opts$nLevels-1)))))), nDig)
    labels <- paste0( opts$format[1] , format(breaks, trim = T,  big.mark = opts$marks[1], decimal.mark = opts$marks[2], nsmall = nDig), opts$format[2])
    g <- graph + geom_point(data = data_graph,
                            aes(x=a, y=b, size=c),
                            color = as.character(unique(data_graph$color)),
                            alpha = opts$opacity) +
      scale_size(
        name = flab,
        range = c(opts$min_radius, opts$max_radius),
        breaks = breaks,
        labels = labels)
    if (!opts$legend_show) g <- g + guides(size = opts$legend_show)

  }else{
    g <- graph
  }

  if (!is.null(opts$projection_ratio)) g <- g + coord_equal(ratio = opts$projection_ratio)
  if (!is.null(opts$projectionName)) g <- g  + coord_map(opts$projectionName, orientation = opts$projection_orientation)

  g <-  g + theme(legend.position= opts$legend_position,
                  plot.title = element_text(color= opts$title_color, size= opts$title_size),
                  plot.subtitle = element_text(color= opts$subtitle_color, size= opts$subtitle_size),
                  plot.caption = element_text(color= opts$caption_color, size= opts$caption_size),
                  plot.background = element_rect(fill = opts$background, linetype = 'blank'),
                  panel.background = element_rect(fill = opts$background,
                                                  colour = opts$background,
                                                  size = 1.5,
                                                  linetype = 'blank'),
                  legend.text=element_text(color=opts$legend_color),
                  legend.background = element_rect(colour = opts$legend_borderColor,
                                                   fill = opts$legend_background))

  if (sum(opts$text_show) != 0) {
    if (opts$text_option == 'code') centroides$name <- centroides$id
    if (!is.null(data)) {
      data$c <- round(data$c, ifelse(is.null(opts$nDigits), 2, opts$nDigits))
      if (opts$text_show[2]) {
        g <- g + geom_text(data = data,
                           aes(label = c, x = a, y = b,
                               check_overlap = TRUE), size = opts$text_size)}
      if (opts$text_show[1]) {
        g <- g + geom_text(data = centroides,
                           aes(label = name, x = lon, y = lat,
                               check_overlap = TRUE), size = opts$text_size)}

    } else{
      g <- g + geom_text(data = centroides,
                         aes(label = name, x = lon, y = lat,
                             check_overlap = TRUE), size = opts$text_size)
    }
  }

  if (opts$graticule) {
    g <- g + theme(panel.grid.major = element_line(colour = opts$graticule_color, linetype = "dashed",
                                                   size = opts$graticule_weight))
  }

  g + labs(x = "",
           y = "",
           title = opts$title,
           subtitle = opts$subtitle,
           caption = opts$caption)

}




#' Ggplot bubbles by latitud and longitud
#'
#' Ggplot bubbles by latitud and longitud
#'
#' @name gg_bubbles_GlnGlt
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gln-Glt
#' @export
#' @examples
#' gg_bubbles_GlnGlt(sampleData("Gln-Glt", nrow = 10))

gg_bubbles_GlnGlt <-function(data = NULL,
                             mapName = "world_countries",
                             opts = NULL) {

  opts <- getOpts(opts = opts)

  if (!is.null(data)) {
    f <- fringe(data)
    nms <- getClabels(f)
    data <- f$d
    if (opts$count) {
      data <- data  %>%
        dplyr::group_by(a, b) %>%
        dplyr::summarise(conteo = n())
    } else {
      data$conteo <- 1:dim(data)[1]
    }
  }

  g <- gg_bubbles_GlnGltNum(data = data, mapName = mapName, opts = NULL)

  if (!opts$count) g <- g + guides(fill=FALSE)

  g
}


#' Bubbles map
#' @name gg_bubbles_GlnGltCatNum
#' @param x A code.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gcd-Num
#' @examples
#' NULL
gg_bubbles_GlnGltCatNum <- function(data = NULL,
                                    mapName = "world_countries",
                                    opts = NULL) {

  if(!mapName %in% availableMaps())
    stop("No map with that name, check available maps with availableMaps()")

  opts <- getOpts(opts = opts)

  mapResults <- layerMap(mapName = mapName,
                         borderColor = opts$border_color,
                         borderWeigth =opts$border_width,
                         fillColor = opts$default_color,
                         fillOpacity = opts$opacity)

  graph <- mapResults[[2]]
  centroides <- mapResults[[3]]


  if (!is.null(data)) {
    f <- fringe(data)
    nms <- getClabels(f)
    data <- f$d
    flab1 <- opts$legend_title[1] %||% nms[3]
    flab2 <- opts$legend_title[2] %||% nms[4]

    data <- data  %>%
      group_by(a, b, c) %>%
      dplyr::summarise(d = agg(opts$agg, d))


    if (opts$percentage) {
      data$d <- (data[['d']] * 100) / sum(data[['d']], na.rm = TRUE)
      opts$format[2] <- '%'
    }

    nDig <- 2
    if (!is.null(opts$nDigits)) nDig <- opts$nDigits

    data_graph <- fillColors(data, 'c', opts$color, 'discrete', NA, NA, opts$labelWrap, F)
    #data_graph <- data_graph %>% drop_na(d)
    breaks <- round(as.vector(quantile(unique(data_graph$d), probs = c(0, cumsum(rep((1/(opts$nLevels-1)), (opts$nLevels-1)))))), nDig)
    labels <- paste0( opts$format[1] ,format(breaks, trim = T,  big.mark = opts$marks[1], decimal.mark = opts$marks[2], nsmall = nDig), opts$format[2])
    g <- graph +
      geom_point(data = data_graph,
                 aes(x=a, y=b, size=d, color = c ),
                 alpha = opts$opacity) +
      scale_color_manual(
        name = flab1,
        values = as.character(unique(data_graph$color)),
        na.value = opts$naColor)

    if (length(breaks) != length(unique(labels))) {
      g <- g
    } else {
      g <- g +
        scale_size(
          name = flab2,
          range = c(opts$min_radius, opts$max_radius),
          breaks = breaks,
          labels = labels)
    }


    if (!opts$legend_show[1]) g <- g + guides(color = opts$legend_show[1])
    if (!opts$legend_show[2]) g <- g + guides(size = opts$legend_show[2])


  }else{
    g <- graph
  }

  if (!is.null(opts$projection_ratio)) g <- g + coord_equal(ratio = opts$projection_ratio)
  if (!is.null(opts$projectionName)) g <- g  + coord_map(opts$projectionName, orientation = opts$projection_orientation)

  g <-  g + theme(legend.position= opts$legend_position,
                  plot.title = element_text(color= opts$title_color, size= opts$title_size),
                  plot.subtitle = element_text(color= opts$subtitle_color, size= opts$subtitle_size),
                  plot.caption = element_text(color= opts$caption_color, size= opts$caption_size),
                  plot.background = element_rect(fill = opts$background, linetype = 'blank'),
                  panel.background = element_rect(fill = opts$background,
                                                  colour = opts$background,
                                                  size = 1.5,
                                                  linetype = 'blank'),
                  legend.text=element_text(color=opts$legend_color),
                  legend.background = element_rect(colour = opts$legend_borderColor,
                                                   fill = opts$legend_background))

  if (sum(opts$text_show) != 0) {
    if (opts$text_option == 'code') centroides$name <- centroides$id
    if (!is.null(data)) {
      data$c <- round(data$c, ifelse(is.null(opts$nDigits), 2, opts$nDigits))
      if (opts$text_show[2]) {
        g <- g + geom_text(data = data,
                           aes(label = c, x = a, y = b,
                               check_overlap = TRUE), size = opts$text_size)}
      if (opts$text_show[1]) {
        g <- g + geom_text(data = centroides,
                           aes(label = name, x = lon, y = lat,
                               check_overlap = TRUE), size = opts$text_size)}

    } else{
      g <- g + geom_text(data = centroides,
                         aes(label = name, x = lon, y = lat,
                             check_overlap = TRUE), size = opts$text_size)
    }
  }

  if (opts$graticule) {
    g <- g + theme(panel.grid.major = element_line(colour = opts$graticule_color, linetype = "dashed",
                                                   size = opts$graticule_weight))
  }

  g + labs(x = "",
           y = "",
           title = opts$title,
           subtitle = opts$subtitle,
           caption = opts$caption)

}


#' Bubbles map
#' @name gg_bubbles_GlnGltCat
#' @param x A code.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gcd-Num
#' @examples

gg_bubbles_GlnGltCat <- function(data = NULL,
                                 mapName = "world_countries",
                                 opts = NULL) {
  opts <- getOpts(opts = opts)

  if (!is.null(data)) {
    f <- fringe(data)
    nms <- getClabels(f)
    data <- f$d
    if (opts$count) {
      data <- data  %>%
        dplyr::group_by(a, b, c) %>%
        dplyr::summarise(conteo = n())
    } else {
      data$conteo <- 1:dim(data)[1]
    }
  }

  g <- gg_bubbles_GlnGltCatNum(data = data, mapName = mapName, opts = opts)
  if (!opts$count) g <- g + guides(fill=FALSE)


  g

}





#' Bubbles map
#' @name gg_bubbles_GnmNum
#' @param x A code.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gnm-Num
#' @examples

gg_bubbles_GnmNum <- function(data = NULL,
                              mapName = "world_countries",
                              opts = NULL) {

  if(!mapName %in% availableMaps())
    stop("No map with that name, check available maps with availableMaps()")

  opts <- getOpts(opts = opts)

  mapResults <- layerMap(mapName = mapName,
                         borderColor = opts$border_color,
                         borderWeigth =opts$border_width,
                         fillColor = opts$default_color,
                         fillOpacity = opts$opacity)

  graph <- mapResults[[2]]
  centroides <- mapResults[[3]]

  if(!is.null(data)){
    f <- fringe(data)
    nms <- getClabels(f)
    data <- f$d
    flab <- opts$legend_title %||% nms[2]
    data$a <- as.character(toupper(tolower(data$a)))

    nDig <- 2
    if (!(is.null(opts$nDigits))) nDig <- opts$nDigits

    data <- data  %>%
      dplyr::group_by(a) %>%
      dplyr::summarise(b = ifelse(sum(is.na(b) == length(b)), b, agg(opts$agg, b)))


    if (opts$percentage) {
      data$b <- (data[['b']] * 100) / sum(data[['b']], na.rm = TRUE)
    }

    centroides$a <- toupper(centroides$name)
    data_graph <- dplyr::inner_join(data, centroides, by = "a")
    data_graph$c <- '1'
    data_graph <- fillColors(data_graph, 'c', opts$color, 'discrete', NA, NA, opts$labelWrap, F)
    data_graph <- data_graph %>% drop_na(b)
    breaks <- round(as.vector(quantile(unique(data_graph$b), probs = c(0, cumsum(rep((1/(opts$nLevels-1)), (opts$nLevels-1)))))), nDig)
    labels <- paste0( opts$format[1] ,format(breaks, trim = T,  big.mark = opts$marks[1], decimal.mark = opts$marks[2], nsmall = nDig), opts$format[2])
    g <- graph + geom_point(data = data_graph,
                            aes(x=lon, y=lat, size=b),
                            color = as.character(unique(data_graph$color)),
                            alpha = opts$opacity) +
      scale_size(
        name = flab,
        range = c(opts$min_radius, opts$max_radius),
        breaks = breaks,
        labels = labels)
    if (!opts$legend_show) g <- g + guides(size = opts$legend_show)

  }else{
    g <- graph
  }

  if (!is.null(opts$projection_ratio)) g <- g + coord_equal(ratio = opts$projection_ratio)
  if (!is.null(opts$projectionName)) g <- g  + coord_map(opts$projectionName, orientation = opts$projection_orientation)

  g <- g + theme(legend.position= opts$legend_position,
                 plot.title = element_text(color= opts$title_color, size= opts$title_size),
                 plot.subtitle = element_text(color= opts$subtitle_color, size= opts$subtitle_size),
                 plot.caption = element_text(color= opts$caption_color, size= opts$caption_size),
                 plot.background = element_rect(fill = opts$background, linetype = 'blank'),
                 panel.background = element_rect(fill = opts$background,
                                                 colour = opts$background,
                                                 size = 1.5,
                                                 linetype = 'blank'),
                 legend.text=element_text(color=opts$legend_color),
                 legend.background = element_rect(colour = opts$legend_borderColor,
                                                  fill = opts$legend_background))
  if (sum(opts$text_show) != 0) {
    if (opts$text_option == 'code') centroides$name <- centroides$id
    if (!is.null(data)) {
      data$b <- round(data$b, ifelse(is.null(opts$nDigits), 2, opts$nDigits))
      centroides <- left_join(centroides, data, by = c('id' = 'a'))
      if (sum(opts$text_show) == 2) centroides$name <- paste0(centroides$name, '\n', centroides$b)
      if (sum(opts$text_show) == 1 && opts$text_show[1]) centroides$name <- centroides$name
      if (sum(opts$text_show) == 1 && opts$text_show[2]) centroides$name <- centroides$b
    }

    if (opts$text_proportion == 'all') {
      g <- g + geom_text(data = centroides,
                         aes(label = name, x = lon, y = lat,
                             check_overlap = TRUE), size = opts$text_size)
    } else if (opts$text_proportion == 'onlyData') {
      if(!is.null(data)){
        dat_text <- data.frame(id = data$a)
        dat_text$id <- as.character(dat_text$id)
        dat_text <- dat_text %>% dplyr::inner_join(., centroides, by = c("id"))
        g <- g + geom_text(data = dat_text,
                           aes(label = name, x = lon, y = lat,
                               check_overlap = TRUE), size = opts$text_size)
      }else{
        g <- g
      }
    } else
      if (!is.null(data)) {
        dat_text <- data.frame(id = data$a)
        dat_text <- sample_frac(dat_text, opts$text_proportion)
        dat_text$id <- as.character(dat_text$id)
        dat_text <- dat_text %>% dplyr::inner_join(., centroides, by = c("id"))
        g <- g + geom_text(data = dat_text,
                           aes(label = name, x = lon, y = lat,
                               check_overlap = TRUE), size = opts$text_size)
      } else {
        g <- g
      }
  }


  if (opts$graticule) {
    g <- g + theme(panel.grid.major = element_line(colour = opts$graticule_color, linetype = "dashed",
                                                   size = opts$graticule_weight))
  }

  g + labs(x = "",
           y = "",
           title = opts$title,
           subtitle = opts$subtitle,
           caption = opts$caption)

}



#' Bubbles map
#' @name gg_bubbles_GnmNum.
#' @param x A code.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gnm-Num
#' @examples
gg_bubbles_GnmCatNum <- function(data = NULL,
                                 mapName = "world_countries",
                                 opts = NULL)
{

  if(!mapName %in% availableMaps())
    stop("No map with that name, check available maps with availableMaps()")

  opts <- getOpts(opts = opts)

  mapResults <- layerMap(mapName = mapName,
                         borderColor = opts$border_color,
                         borderWeigth =opts$border_width,
                         fillColor = opts$default_color,
                         fillOpacity = opts$opacity)

  graph <- mapResults[[2]]
  centroides <- mapResults[[3]]

  if (!is.null(data)) {

    f <- fringe(data)
    nms <- getClabels(f)
    data <- f$d
    flab1 <- opts$legend_title[1] %||% nms[2]
    flab2 <- opts$legend_title[2] %||% nms[3]
    data$a <- str_to_title(iconv(as.character(tolower(data$a)), to="ASCII//TRANSLIT"))
    nDig <- 2
    if (!is.null(opts$nDigits)) nDig <- opts$nDigits

    data <- data  %>%
      group_by(a, b) %>%
      dplyr::summarise(c = round(agg(opts$agg, c), nDig)) %>%
      arrange(-c) %>%
      mutate(ind = 1:length(b)) %>%
      filter(ind == 1) %>%
      dplyr::select(a, b, c)


    if (opts$percentage) {
      data$c <- (data[['c']] * 100) / sum(data[['c']], na.rm = TRUE)
      opts$format[2] <- '%'
    }



    centroides$a <- str_to_title(iconv(as.character(tolower(centroides$name)), to="ASCII//TRANSLIT"))
    data_graph <- dplyr::inner_join(data, centroides, by = "a")
    data_graph <- fillColors(data_graph, 'b', opts$color, 'discrete', NA, NA, opts$labelWrap, F)
    data_graph <- data_graph %>% drop_na(c)
    breaks <- round(as.vector(quantile(unique(data_graph$c), probs = c(0, cumsum(rep((1/(opts$nLevels-1)), (opts$nLevels-1)))))), nDig)
    labels <- paste0( opts$format[1] ,format(breaks, trim = T,  big.mark = opts$marks[1], decimal.mark = opts$marks[2]), opts$format[2])
    g <- graph + geom_point(data = data_graph,
                            aes(x=lon, y=lat, size=c, color = b ),
                            alpha = opts$opacity) +
      scale_size(
        name = flab2,
        range = c(opts$min_radius, opts$max_radius),
        breaks = breaks,
        labels = labels) +
      scale_color_manual(
        name = flab1,
        values = as.character(unique(data_graph$color)),
        na.value = opts$naColor)
    if (!opts$legend_show[1]) g <- g + guides(color = opts$legend_show[1])
    if (!opts$legend_show[2]) g <- g + guides(size = opts$legend_show[2])


  }else{
    g <- graph
  }

  if (!is.null(opts$projection_ratio)) g <- g + coord_equal(ratio = opts$projection_ratio)
  if (!is.null(opts$projectionName)) g <- g  + coord_map(opts$projectionName, orientation = opts$projection_orientation)

  g <-  g + theme(legend.position= opts$legend_position,
                  plot.title = element_text(color= opts$title_color, size= opts$title_size),
                  plot.subtitle = element_text(color= opts$subtitle_color, size= opts$subtitle_size),
                  plot.caption = element_text(color= opts$caption_color, size= opts$caption_size),
                  plot.background = element_rect(fill = opts$background, linetype = 'blank'),
                  panel.background = element_rect(fill = opts$background,
                                                  colour = opts$background,
                                                  size = 1.5,
                                                  linetype = 'blank'),
                  legend.text=element_text(color=opts$legend_color),
                  legend.background = element_rect(colour = opts$legend_borderColor,
                                                   fill = opts$legend_background))

  if (sum(opts$text_show) != 0) {
    if (opts$text_option == 'code') centroides$name <- centroides$id
    if (!is.null(data)) {
      data$c <- round(data$c, ifelse(is.null(opts$nDigits), 2, opts$nDigits))
      centroides <- left_join(centroides, data, by = c('id' = 'a'))
      if (sum(opts$text_show) == 2) centroides$name <- paste0(centroides$name, '\n', centroides$c)
      if (sum(opts$text_show) == 1 && opts$text_show[1]) centroides$name <- centroides$name
      if (sum(opts$text_show) == 1 && opts$text_show[2]) centroides$name <- centroides$b
    }

    if (opts$text_proportion == 'all') {
      g <- g + geom_text(data = centroides,
                         aes(label = name, x = lon, y = lat,
                             check_overlap = TRUE), size = opts$text_size)
    } else if (opts$text_proportion == 'onlyData') {
      if(!is.null(data)){
        dat_text <- data.frame(id = data$a)
        dat_text$id <- as.character(dat_text$id)
        dat_text <- dat_text %>% dplyr::inner_join(., centroides, by = c("id"))
        g <- g + geom_text(data = dat_text,
                           aes(label = name, x = lon, y = lat,
                               check_overlap = TRUE), size = opts$text_size)
      }else{
        g <- g
      }
    } else
      if (!is.null(data)) {
        dat_text <- data.frame(id = data$a)
        dat_text <- sample_frac(dat_text, opts$text_proportion)
        dat_text$id <- as.character(dat_text$id)
        dat_text <- dat_text %>% dplyr::inner_join(., centroides, by = c("id"))
        g <- g + geom_text(data = dat_text,
                           aes(label = name, x = lon, y = lat,
                               check_overlap = TRUE), size = opts$text_size)
      } else {
        g <- g
      }
  }


  if (opts$graticule) {
    g <- g + theme(panel.grid.major = element_line(colour = opts$graticule_color, linetype = "dashed",
                                                   size = opts$graticule_weight))
  }

  g + labs(x = "",
           y = "",
           title = opts$title,
           subtitle = opts$subtitle,
           caption = opts$caption)

}
