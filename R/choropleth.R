#' Choropleth map
#' Choropleth map
#' @name gg_choropleth_GcdNum
#' @param x A code.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gcd-Num
#' @examples
#'gg_choropleth_GcdNum()
#'gg_choropleth_GcdNum(data = sampleData("Gcd-Num"))
#'gg_choropleth_GcdNum(data = sampleData("Gcd-Num", 1000), mapName = "american_countries")
gg_choropleth_GcdNum <- function( data = NULL,
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

  data_map <- mapResults[[1]]
  graph <- mapResults[[2]]
  centroides <- mapResults[[3]]

  if (!is.null(data)) {
    f <- fringe(data)
    nms <- getClabels(f)
    data <- f$d
    flab <- opts$legend_title %||% nms[2]
    data$a <- as.character(toupper(tolower(data$a)))
    data_map$a <- as.character(data_map$id)

    data <- data  %>%
      dplyr::group_by(a) %>%
      dplyr::summarise(b = ifelse(sum(is.na(b) == length(b)), b, agg(opts$agg, b)))

    if (opts$percentage) {
      data$b <- (data[['b']] * 100) / sum(data[['b']], na.rm = TRUE)
    }

    data_graph <- binsLeg(data = data,
                          col = 'b',
                          scale = opts$scale,
                          mode = opts$legend_mode,
                          bins = as.numeric(opts$nLevels),
                          percent = opts$percentage,
                          nDigits = opts$nDigits,
                          marks = opts$marks,
                          format = c(opts$prefix, opts$suffix),
                          colors = opts$colors,
                          dataLeft =  data_map
    )


    g <- graph +
      geom_map(data = data_graph, map = data_graph,
               aes(map_id = id, x = long, y = lat, fill = bins),
               color = opts$border_color, size = 0.25, alpha = opts$opacity)

    if (opts$scale == 'continuous') {
      g <- g + scale_fill_gradient(
        aesthetics = "fill",
        na.value = opts$na_color,
        colours = as.character(unique(data_graph$color)),
        labels =  as.character(unique(data_graph$labels[!is.na(data_graph$labels)])),
        breaks =  as.numeric(unique(data_graph$breaks[!is.na(data_graph$breaks)])),
        limits = c(min(data_graph$breaks, na.rm = T) - opts$legend_limit, max(data_graph$breaks, na.rm = T) + opts$legend_limit))
    } else {
      g <- g + scale_fill_manual(
        values = as.character(unique(data_graph$color)),
        na.value = opts$na_color)
    }

    g <- g +
      labs(x = "",
           y = "",
           title = opts$title,
           subtitle = opts$subtitle,
           caption = opts$caption,
           fill= flab)

  } else {
    g <- graph + labs(x = "",
                      y = "",
                      title = opts$title,
                      subtitle = opts$subtitle,
                      caption = opts$caption)
  }

  if (!is.null(opts$projection_ratio)) g <- g + coord_equal(ratio = opts$projection_ratio)
  if (!is.null(opts$projection_name)) g <- g  + coord_map(opts$projection_name, orientation = opts$projection_orientation)

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

  g
}



#' Ggplot choropleths by geographical code
#'
#' Ggplot choropleths by geographical code
#'
#' @name gg_choropleth_Gcd
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gcd
#' @export
#' @examples
#' gg_choropleth_Gcd(sampleData("Gcd", nrow = 10))
gg_choropleth_Gcd <- function(data = NULL,
                              mapName = "world_countries",
                              opts = NULL) {

  opts <- getOpts(opts = opts)

  if (!is.null(data)) {
    f <- fringe(data)
    nms <- getClabels(f)
    data <- f$d
    if (opts$count) {
      data <- data  %>%
        dplyr::group_by(a) %>%
        dplyr::summarise(conteo = n())
    } else {
      data <- data %>% distinct(a)
      data$conteo <- 1:dim(data)[1]
    }
  }

  g <- gg_choropleth_GcdNum(data = data, mapName = mapName, opts = opts)

  if (!opts$count) g <- g + guides(fill=FALSE)

  g
}




#' Choropleth map
#' Choropleth map
#' @name gg_choropleth_GcdCat
#' @param x A code.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gcd-Cat
#' @examples


gg_choropleth_GcdCat <- function(data = NULL,
                                 mapName = "world_countries",
                                 opts = NULL) {

  if (!mapName %in% availableGeodata()) {
    stop("Pick an available map for the mapName argument (geodata::availableGeodata())")
  }

  opts <- getOpts(opts = opts)

  mapResults <- layerMap(mapName = mapName,
                         borderColor = opts$border_color,
                         borderWeigth =opts$border_width,
                         fillColor = opts$default_color,
                         fillOpacity = opts$opacity)

  data_map <- mapResults[[1]]
  graph <- mapResults[[2]]
  centroides <- mapResults[[3]]


  if (!is.null(data)) {

    f <- fringe(data)
    nms <- getClabels(f)
    data <- f$d
    flab <- opts$legend_title %||% nms[2]
    data$a <- as.character(toupper(tolower(data$a)))
    data_map$a <- as.character(data_map$id)


    data <- data  %>%
      group_by(a, b) %>%
      summarise(conteo = n())


    if (opts$count) {
      data <- data %>% dplyr::select(a, b = conteo)
      g <- gg_choropleth_GcdNum(data = data, mapName = mapName, opts = opts)
    } else {
      data <- data %>%
        dplyr::arrange(-conteo) %>%
        dplyr::mutate(ind = 1:length(b)) %>%
        dplyr::filter(ind == 1) %>%
        dplyr::select(a, b)
      data_graph <- dplyr::left_join(data, data_map, by = "a")
      data_graph <- fillColors(data_graph, 'b' , colors = opts$colors, colorScale = opts$scale, highlightValue = NULL, highlightValueColor = NULL, numeric = F, labelWrap = 12)
      data_graph$b <- as.factor(data_graph$b)

      g <- graph +
        geom_map(data = data_graph, map = data_graph,
                 aes(map_id = id, x = long, y = lat, fill = b),
                 color = opts$border_color, size = 0.25, alpha = opts$opacity) +
        scale_fill_manual(values = as.character(unique(data_graph$color)),
                          na.value = opts$na_color)
      if (sum(opts$text_show) != 0) {
        if (opts$text_option == 'code') centroides$name <- centroides$id
        if (!is.null(data)) {
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
    }
    g <-  g +
      labs(x = "",
           y = "",
           title = opts$title,
           subtitle = opts$subtitle,
           caption = opts$caption,
           fill= flab)
  } else {
    g <- graph +
      labs(x = "",
           y = "",
           title = opts$title,
           subtitle = opts$subtitle,
           caption = opts$caption)
    if (sum(opts$text_show) != 0) {
      if (opts$text_option == 'code') centroides$name <- centroides$id
      g <- g + geom_text(data = centroides,
                         aes(label = name, x = lon, y = lat,
                             check_overlap = TRUE), size = opts$text_size)
    }
  }


  if (!is.null(opts$projection_ratio)) g <- g + coord_equal(ratio = opts$projection_ratio)
  if (!is.null(opts$projection_name)) g <- g  + coord_map(opts$projection_name, orientation = opts$projection_orientation)

  g <- g + theme(legend.position= opts$legend_position,
                 plot.title = element_text(color= opts$title_color, size= opts$title_size),
                 plot.subtitle = element_text(color= opts$subtitle_color, size= opts$subtitle_size),
                 plot.caption = element_text(color= opts$caption_color, size= opts$caption_size),
                 plot.background = element_rect(fill = opts$background, linetype = 'blank'),
                 panel.background = element_rect(fill = opts$background,
                                                 colour = opts$background,
                                                 size = 1.5,
                                                 linetype = 'blank'),
                 legend.text = element_text(color = opts$legend_color),
                 legend.background = element_rect(colour = opts$legend_borderColor,
                                                  fill = opts$legend_background))



  if (opts$graticule) {
    g <- g + theme(panel.grid.major = element_line(colour = opts$graticule_color, linetype = "dashed",
                                                   size = opts$graticule_weight))
  }
  g
}



#' Choropleth map
#' Choropleth map
#' @name gg_choropleth_GnmNum.
#' @param x A code.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gnm-Num
#' @examples

gg_choropleth_GnmNum <- function(data = NULL,
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

  data_map <- mapResults[[1]]
  graph <- mapResults[[2]]
  centroides <- mapResults[[3]]

  centroides$name <- str_to_title(iconv(as.character(tolower(centroides$name)), to="ASCII//TRANSLIT"))

  if (!is.null(data)) {

    f <- fringe(data)
    nms <- getClabels(f)
    data <- f$d
    flab <- opts$legend_title %||% nms[2]
    data$a <- str_to_title(iconv(as.character(tolower(data$a)), to="ASCII//TRANSLIT"))
    data_map$a <- str_to_title(tolower(as.character(data_map$name)))

    data <- data  %>%
      dplyr::group_by(a) %>%
      dplyr::summarise(b = ifelse(sum(is.na(b) == length(b)), b, agg(opts$agg, b)))

    if (opts$percentage) {
      data$b <- (data[['b']] * 100) / sum(data[['b']], na.rm = TRUE)
    }

    data_graph <- binsLeg(data = data,
                          col = 'b',
                          scale = opts$scale,
                          mode = opts$legend_mode,
                          bins = as.numeric(opts$nLevels),
                          percent = opts$percentage,
                          nDigits = opts$nDigits,
                          marks = opts$marks,
                          format = opts$format,
                          colors = opts$colors,
                          dataLeft =  data_map
    )


    g <- graph +
      geom_map(data = data_graph, map = data_graph,
               aes(map_id = id, x = long, y = lat, fill = bins),
               color = opts$border_color, size = 0.25, alpha = opts$opacity)

    if (opts$scale == 'continuous') {
      g <- g + scale_fill_gradientn(
        aesthetics = "fill",
        na.value = opts$na_color,
        colours = as.character(unique(data_graph$color)),
        labels =  as.character(unique(data_graph$labels[!is.na(data_graph$labels)])),
        breaks =  as.numeric(unique(data_graph$breaks[!is.na(data_graph$breaks)])),
        limits = c(min(data_graph$breaks, na.rm = T) - opts$legend_limit, max(data_graph$breaks, na.rm = T) + opts$legend_limit))
    } else {
      g <- g + scale_fill_manual(
        values = as.character(unique(data_graph$color)),
        na.value =opts$na_color)
    }
    g <- g +
      labs(x = "",
           y = "",
           title = opts$title,
           subtitle = opts$subtitle,
           caption = opts$caption,
           fill= flab)

  } else {
    g <- graph +
      labs(x = "",
           y = "",
           title = opts$title,
           subtitle = opts$subtitle,
           caption = opts$caption)
  }
  if (!is.null(opts$projection_ratio)) g <- g + coord_equal(ratio = opts$projection_ratio)
  if (!is.null(opts$projection_name)) g <- g  + coord_map(opts$projection_name, orientation = opts$projection_orientation)

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
                 legend.background = element_rect(color = opts$legend_borderColor,
                                                  fill = opts$legend_background))

  if (sum(opts$text_show) != 0) {
    if (opts$text_option == 'code') centroides$name <- centroides$id
    if (!is.null(data)) {
      data$b <- round(data$b, ifelse(is.null(nDigits), 2, nDigits))
      centroides <- left_join(centroides, data, by = c('name' = 'a'))
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

  g
}

#' Ggplot choropleths by geographical code
#'
#' Ggplot choropleths by geographical code
#'
#' @name gg_choropleth_Gnm
#' @param x A data.frame
#' @return leaflet viz
#' @section ctypes: Gnm
#' @export
#' @examples
#' gg_choropleth_Gnm(sampleData("Gnm", nrow = 10))
gg_choropleth_Gnm <- function(data = NULL,
                              mapName = "world_countries",
                              opts = NULL)
{

  opts <- getOpts(opts = opts)

  if (!is.null(data)) {
    f <- fringe(data)
    nms <- getClabels(f)
    data <- f$d
    if (opts$count) {
      data <- data  %>%
        dplyr::group_by(a) %>%
        dplyr::summarise(conteo = n())
    } else {
      data <- data %>% distinct(a)
      data$conteo <- 1:dim(data)[1]
    }
  }

  g <- gg_choropleth_GnmNum(data = data, mapName = mapName, opts = opts)

  if (!opts$count) g <- g + guides(fill=FALSE)

  g
}


#' Choropleth map
#' Choropleth map
#' @name gg_choropleth_GnmCat
#' @param x A code.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gnm-Cat
#' @examples


gg_choropleth_GnmCat <- function(data = NULL,
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

  data_map <- mapResults[[1]]
  graph <- mapResults[[2]]
  centroides <- mapResults[[3]]

  centroides$name <- str_to_title(iconv(as.character(tolower(centroides$name)), to="ASCII//TRANSLIT"))

  if (!is.null(data)) {

    f <- fringe(data)
    nms <- getClabels(f)
    data <- f$d
    flab <- opts$legend_title %||% nms[2]
    data$a <- str_to_title(iconv(as.character(tolower(data$a)), to="ASCII//TRANSLIT"))
    data_map$a <- str_to_title(tolower(as.character(data_map$name)))


    data <- data  %>%
      group_by(a, b) %>%
      summarise(conteo = n())


    if (opts$count) {
      data <- data %>% dplyr::select(a, b = conteo)
      g <- gg_choropleth_GnmNum(data = data, mapName = mapName, opts = opts)
    } else {
      data <- data %>%
        arrange(-conteo) %>%
        dplyr::mutate(ind = 1:length(b)) %>%
        dplyr::filter(ind == 1) %>%
        dplyr::select(a, b)
      data_graph <- dplyr::left_join(data, data_map, by = "a")
      data_graph <- fillColors(data_graph, 'b' , colors = opts$colors, colorScale = opts$scale, highlightValue = NULL, highlightValueColor = NULL, numeric = F, labelWrap = 12)
      data_graph$b <- as.factor(data_graph$b)

      g <- graph +
        geom_map(data = data_graph, map = data_graph,
                 aes(map_id = id, x = long, y = lat, fill = b),
                 color = opts$border_color, size = 0.25, alpha = opts$opacity) +
        scale_fill_manual(values = as.character(unique(data_graph$color)),
                          na.value = opts$na_color)
      g <- g +
        labs(x = "",
             y = "",
             title = opts$title,
             subtitle = opts$subtitle,
             caption = opts$caption,
             fill= flab)

      if (sum(opts$text_show) != 0) {
        if (opts$text_option == 'code') centroides$name <- centroides$id
        if (!is.null(data)) {
          centroides <- left_join(centroides, data, by = c('name' = 'a'))
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
    }
  } else {
    g <- graph +  labs(x = "",
                       y = "",
                       title = opts$title,
                       subtitle = opts$subtitle,
                       caption = opts$caption)
    if (sum(opts$text_show) != 0) {
      if (opts$text_option == 'code') centroides$name <- centroides$id
      g <- g + geom_text(data = centroides,
                         aes(label = name, x = lon, y = lat,
                             check_overlap = TRUE), size = opts$text_size)
    }
  }

  if (!is.null(opts$projection_ratio)) g <- g + coord_equal(ratio = opts$projection_ratio)
  if (!is.null(opts$projection_name)) g <- g  + coord_map(opts$projection_name, orientation = opts$projection_orientation)

  if (opts$legend_show[1]) {
  g <- g + theme(legend.position= opts$legend_position,
                 legend.text=element_text(color=opts$legend_color),
                 legend.background = element_rect(colour = opts$legend_borderColor,
                                                  fill = opts$legend_background))
  } else {
    g <- g +  guides(fill=FALSE)
  }

  g <- g + theme( plot.title = element_text(color= opts$title_color, size= opts$title_size),
                  plot.subtitle = element_text(color= opts$subtitle_color, size= opts$subtitle_size),
                  plot.caption = element_text(color= opts$caption_color, size= opts$caption_size),
                  plot.background = element_rect(fill = opts$background, linetype = 'blank'),
                  panel.background = element_rect(fill = opts$background,
                                                  colour = opts$background,
                                                  size = 1.5,
                                                  linetype = 'blank'))
  if (opts$graticule) {
    g <- g + theme(panel.grid.major = element_line(colour = opts$graticule_color, linetype = "dashed",
                                                   size = opts$graticule_weight))  }
  g

}

