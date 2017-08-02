
#' Choropleth of Colombia's deptos
#' Choropleth of Colombia's deptos
#' @name gg_choropleth_co_GnmNum.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gnm-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_choropleth_co_GnmNum. <- function(data, titleLabel = "", subtitle = "", caption = "", reverse = FALSE, text_size = 2,
                                     fillLabel = NULL, leg_pos = "right", text = FALSE, prop_text = 0.5,
                                     color_map = "gray", color_frontier = "white", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flab <- fillLabel %||% nms[2]
  data <- f$d

  options(warn=-1)
  data$a <- as.character(data$a)
  data$a[data$a == "5"] <- "05"
  data$a[data$a == "7"] <- "07"
  data$a[data$a == "8"] <- "08"

  data_deptos <- suppressMessages(read_csv(system.file("geo/deptos_co.csv", package = "gggeomagic"), col_names = TRUE))
  names(data_deptos)[which(names(data_deptos) == "id")] <- "a"
  data_complete <- data.frame(a = unique(data_deptos$a), stringsAsFactors = FALSE)
  data <- suppressMessages(dplyr::inner_join(data_complete, data))

  data_centroids <- suppressMessages(read_csv(system.file("geo/centroids_deptos_co.csv", package = "gggeomagic"), col_names = TRUE))

  data_graph <- dplyr::inner_join(data, data_deptos, by = "a")
  names(data_graph)[which(names(data_graph) == "a")] <- "id"
  names(data_deptos)[which(names(data_deptos) == "a")] <- "id"

  graph <- ggplot() +
    geom_map(data = data_deptos, map = data_deptos,
             aes(map_id = id, x = long, y = lat, group = group), fill = color_map,
             color = color_frontier, size = 0.25) + coord_map() +
    expand_limits(x = data_deptos$long, y = data_deptos$lat)
  graph <- graph +
    geom_map(data = data_graph, map = data_graph,
             aes(map_id = id, x = long, y = lat, group = group, fill = b),
             color = color_frontier, size = 0.25) + coord_map() +
    expand_limits(x = data_graph$long, y = data_graph$lat) + theme_ds() + theme_ds_clean()

  if(text){
    if(prop_text == "all"){
      graph <- graph + geom_text(data = data_centroids,
                                 aes(label = name_id, x = x, y = y,
                                     check_overlap = TRUE), size = text_size)
    }else{
      if(is.vector(prop_text) & class(prop_text) == "character"){
        prop_text <- data.frame(name_id = prop_text)
        prop_text <- prop_text %>% dplyr::inner_join(., data_centroids, by = c("name_id"))
        graph <- graph + geom_text(data = prop_text,
                                   aes(label = name_id, x = x, y = y,
                                       check_overlap = TRUE), size = text_size)
      }else{
        data_centroids <- sample_n(data_centroids, dim(data_centroids)[1] * prop_text)
        graph <- graph + geom_text(data = data_centroids,
                                   aes(label = name_id, x = x, y = y,
                                       check_overlap = TRUE), size = text_size)
      }
    }
  }
  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }
  graph <- graph + labs(x = "", y = "", title = titleLabel, subtitle = subtitle, caption = caption) +
    theme(legend.position=leg_pos)

  options(warn=0)

  graph
}


#' Sketch map departments
#' Sketch map departments
#' @name gg_sketchmap_depto_GnmNum.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gnm-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_sketchmap_depto_GnmNum. <- function(depto_ = depto_, color_map = "gold", color_frontier = "black"){

  options(warn = -1)
  data_mpios <- suppressMessages(read_csv(system.file("geo/mpios_depto_co.csv",
                                                      package = "gggeomagic"), col_names = TRUE))


  data_mpios <- data_mpios %>% filter(depto == depto_)
  graph <- ggplot() +
    geom_map(data = data_mpios, map = data_mpios,
             aes(map_id = id, x = long, y = lat, group = group), fill = color_map,
             color = color_frontier, size = 0.25) +
    coord_map() +
    expand_limits(x = data_mpios$long, y = data_mpios$lat) +
    theme_ds_clean()
  graph

}


#' Choropleth by filtering deptos
#' Choropleth by filtering deptos
#' @name gg_choropleth_depto_GnmNum.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gnm-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_choropleth_depto_GnmNum. <- function(data, titleLabel = "", subtitle = "", caption = "", depto_ = "05", reverse = FALSE, prop_text = 0.1,
                                        fillLabel = NULL, leg_pos = "right", text = FALSE, text_size = 2,
                                        color_map = "gray", color_frontier = "white", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flab <- fillLabel %||% nms[2]
  data <- f$d

  options(warn=-1)
  data_mpios <- suppressMessages(read_csv(system.file("geo/mpios_depto_co.csv", package = "gggeomagic"), col_names = TRUE))
  names(data_mpios)[which(names(data_mpios) == "id")] <- "a"
  data_mpios <- data_mpios %>% filter(depto == depto_)
  data_complete <- data.frame(a = unique(data_mpios$a))
  data <- suppressMessages(dplyr::inner_join(data_complete, data))

  data_centroids <- suppressMessages(read_csv(system.file("geo/centroids_mpios_depto_co.csv", package = "gggeomagic"), col_names = TRUE))

  data_graph <- dplyr::inner_join(data, data_mpios, by = "a")
  names(data_graph)[which(names(data_graph) == "a")] <- "id"
  names(data_mpios)[which(names(data_mpios) == "a")] <- "id"

  graph <- ggplot() +
    geom_map(data = data_mpios, map = data_mpios,
             aes(map_id = id, x = long, y = lat, group = group), fill = color_map,
             color = color_frontier, size = 0.25) + coord_map() +
    expand_limits(x = data_mpios$long, y = data_mpios$lat)

  graph <- graph +
    geom_map(data = data_graph, map = data_graph,
             aes(map_id = id, x = long, y = lat, group = group, fill = b),
             color = color_frontier, size=0.25) + coord_map() +
    expand_limits(x = data_graph$long, y = data_graph$lat)

  if(text){
    data_centroids <- data_centroids %>% filter(depto == depto_)
    if(prop_text == "all"){
      graph <- graph + geom_text(data = data_centroids,
                                 aes(label = name_id, x = x, y = y,
                                     check_overlap = TRUE), size = text_size)
    }else{
      if(is.vector(prop_text) & class(prop_text) == "character"){
        prop_text <- data.frame(name_id = prop_text)
        prop_text <- prop_text %>% dplyr::inner_join(., data_centroids, by = c("name_id"))
        graph <- graph + geom_text(data = prop_text,
                                   aes(label = name_id, x = x, y = y,
                                       check_overlap = TRUE), size = text_size)
      }else{
        data_centroids <- sample_n(data_centroids, dim(data_centroids)[1] * prop_text)
        graph <- graph + geom_text(data = data_centroids,
                                   aes(label = name_id, x = x, y = y,
                                       check_overlap = TRUE), size = text_size)
      }
    }
  }

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }

  graph <- graph + labs(x = "", y = "", title = titleLabel, subtitle = subtitle, caption = caption) +
    theme_ds() + theme_ds_clean() + theme(legend.position=leg_pos)

  options(warn=0)

  graph
}

#' Choropleth of Latam
#' Choropleth of Latam
#' @name gg_choropleth_latam_GnmNum.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gnm-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_choropleth_latam_GnmNum. <- function(data, titleLabel = "", subtitle = "", caption = "", reverse = FALSE,
                                        fillLabel = NULL, leg_pos = "right",
                                        color_map = "gray", color_frontier = "white", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flab <- fillLabel %||% nms[2]
  data <- f$d

  options(warn=-1)
  data_latam <- suppressMessages(read_csv(system.file("geo/latam.csv", package = "gggeomagic"), col_names = TRUE))
  data_latam$id <- as.character(data_latam$id)
  names(data_latam)[which(names(data_latam) == "id")] <- "a"
  data_complete <- data.frame(a = unique(data_latam$a))
  data <- suppressMessages(dplyr::inner_join(data_complete, data))

  data_graph <- dplyr::inner_join(data, data_latam, by = "a")
  names(data_graph)[which(names(data_graph) == "a")] <- "id"
  names(data_latam)[which(names(data_latam) == "a")] <- "id"

  graph <- ggplot() +
    geom_map(data = data_latam, map = data_latam,
             aes(map_id = id, x = long, y = lat, group = group), fill = color_map,
             color = color_frontier, size = 0.25) + coord_map() +
    expand_limits(x = data_latam$long, y = data_latam$lat)
  graph <- graph +
    geom_map(data = data_graph, map = data_graph,
             aes(map_id = id, x = long, y = lat, group = group, fill = b),
             color = color_frontier, size = 0.25) + coord_map() +
    expand_limits(x = data_graph$long, y = data_graph$lat)

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }

  graph <- graph + labs(x = "", y = "", title = titleLabel, subtitle = subtitle, caption = caption) +
    theme_ds() + theme_ds_clean() + theme(legend.position=leg_pos)

  options(warn=0)

  graph
}

#' Points inside Colombia's deptos map
#' Points inside Colombia's deptos map
#' @name gg_bubble_co_Gnm.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gnm
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_co_Gnm. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL, prop_text = 0.5,
                              color_point = "red", leg_pos = "right", text = FALSE, text_size = 2,
                              color_map = "gray", color_frontier = "white", scale_point = 0.25,
                              alpha = 0.5, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flab <- fillLabel %||% nms[3]
  data <- f$d

  options(warn=-1)
  data_deptos <- suppressMessages(read_csv(system.file("geo/deptos_co.csv", package = "gggeomagic"), col_names = TRUE))

  data_centroids <- suppressMessages(read_csv(system.file("geo/centroids_deptos_co.csv", package = "gggeomagic"), col_names = TRUE))

  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(count = n())

  graph <- ggplot(data_deptos) +
    geom_map(map = data_deptos,
             aes(map_id = id, x = long, y = lat, group = group), fill = color_map,
             color=color_frontier, size = 0.25) +
    expand_limits(x = data_deptos$long, y = data_deptos$lat) +
    coord_fixed()

  graph <- graph + geom_point(data = data_graph, aes(x = a, y = b),
                              size = data_graph$count * scale_point,
                              colour = color_point, alpha = alpha) + coord_map()  +
    labs(x = "", y = "", title = titleLabel, subtitle = subtitle, caption = caption) + theme_ds() + theme_ds_clean() +
    theme(legend.position=leg_pos)

  if(text){
    if(prop_text == "all"){
      graph <- graph + geom_text(data = data_centroids,
                                 aes(label = name_id, x = x, y = y,
                                     check_overlap = TRUE), size = text_size)
    }else{
      if(is.vector(prop_text) & class(prop_text) == "character"){
        prop_text <- data.frame(name_id = prop_text)
        prop_text <- prop_text %>% dplyr::inner_join(., data_centroids, by = c("name_id"))
        graph <- graph + geom_text(data = prop_text,
                                   aes(label = name_id, x = x, y = y,
                                       check_overlap = TRUE), size = text_size)
      }else{
        data_centroids <- sample_n(data_centroids, dim(data_centroids)[1] * prop_text)
        graph <- graph + geom_text(data = data_centroids,
                                   aes(label = name_id, x = x, y = y,
                                       check_overlap = TRUE), size = text_size)
      }
    }
  }
  options(warn=0)

  graph
}

#' Points inside Latam map
#' Points inside Latam map
#' @name gg_bubble_latam_Gnm.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gnm
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_latam_Gnm. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                 color_point = "red", leg_pos = "right",
                                 color_map = "gray", color_frontier = "white", scale_point = 0.25,
                                 alpha = 0.5, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flab <- fillLabel %||% nms[3]
  data <- f$d

  options(warn=-1)
  data_latam <- suppressMessages(read_csv(system.file("geo/latam.csv", package = "gggeomagic"), col_names = TRUE))

  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(count = n())

  graph <- ggplot(data_latam) +
    geom_map(map = data_latam,
             aes(map_id = id, x = long, y = lat, group = group), fill = color_map,
             color=color_frontier, size = 0.25) +
    expand_limits(x = data_latam$long, y = data_latam$lat) +
    coord_fixed()

  graph <- graph + geom_point(data = data_graph, aes(x = a, y = b),
                              size = data_graph$count * scale_point,
                              colour = color_point, alpha = alpha) + coord_map() + coord_fixed()  +
    labs(x = "", y = "", title = titleLabel, subtitle = subtitle, caption = caption) + theme_ds() + theme_ds_clean() +
    theme(legend.position=leg_pos)
  options(warn=0)

  graph
}

#' Points inside Colombia's mpios map
#' Points inside Colombia's mpios map
#' @name gg_bubble_depto_Gnm.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gnm
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_depto_Gnm. <- function(data, titleLabel = "", subtitle = "", caption = "", depto_ = "05", leg_pos = "right", prop_text = 0.1,
                                 fillLabel = NULL, color_point = "red", text = FALSE, text_size = 2,
                                 color_map = "gray", color_frontier = "white", scale_point = 0.25,
                                 alpha = 0.5, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flab <- fillLabel %||% nms[2]
  data <- f$d

  options(warn=-1)
  data_mpios <- suppressMessages(read_csv(system.file("geo/mpios_depto_co.csv", package = "gggeomagic"), col_names = TRUE))
  data_mpios <- data_mpios %>% filter(depto == depto_)

  data_centroids <- suppressMessages(read_csv(system.file("geo/centroids_mpios_depto_co.csv", package = "gggeomagic"), col_names = TRUE))

  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(count = n())
  graph <- ggplot() +
    geom_map(data = data_mpios, map = data_mpios,
             aes(map_id = id, x = long, y = lat, group = group), fill = color_map,
             color = color_frontier, size = 0.25) +
    expand_limits(x = data_mpios$long, y = data_mpios$lat) + coord_map()

  graph <- graph + geom_point(data = data_graph, aes(x = a, y = b),
                              size = data_graph$count * scale_point,
                              colour = color_point, alpha = alpha) + coord_map() + coord_fixed()  +
    labs(x = "", y = "", title = titleLabel, subtitle = subtitle, caption = caption) + theme_ds() + theme_ds_clean() +
    theme(legend.position=leg_pos)

  if(text){
    data_centroids <- data_centroids %>% filter(depto == depto_)
    if(prop_text == "all"){
      graph <- graph + geom_text(data = data_centroids,
                                 aes(label = name_id, x = x, y = y,
                                     check_overlap = TRUE), size = text_size)
    }else{
      if(is.vector(prop_text) & class(prop_text) == "character"){
        prop_text <- data.frame(name_id = prop_text)
        prop_text <- prop_text %>% dplyr::inner_join(., data_centroids, by = c("name_id"))
        graph <- graph + geom_text(data = prop_text,
                                   aes(label = name_id, x = x, y = y,
                                       check_overlap = TRUE), size = text_size)
      }else{
        data_centroids <- sample_n(data_centroids, dim(data_centroids)[1] * prop_text)
        graph <- graph + geom_text(data = data_centroids,
                                   aes(label = name_id, x = x, y = y,
                                       check_overlap = TRUE), size = text_size)
      }
    }
  }
  options(warn=0)

  graph
}

#' Points inside Colombia's deptos map
#' Points inside Colombia's deptos map
#' @name gg_bubble_co_GnmNum.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gnm-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_co_GnmNum. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL, text = FALSE, text_size = 2,
                                 leg_pos = "right", color_point = "red", prop_text = 0.5,
                                 color_map = "gray", color_frontier = "white", scale_point = 0.25,
                                 alpha = 0.5, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flab <- fillLabel %||% nms[3]
  data <- f$d

  options(warn=-1)
  data_deptos <- suppressMessages(read_csv(system.file("geo/deptos_co.csv", package = "gggeomagic"), col_names = TRUE))

  data_centroids <- suppressMessages(read_csv(system.file("geo/centroids_deptos_co.csv", package = "gggeomagic"), col_names = TRUE))

  graph <- ggplot(data_deptos) +
    geom_map(map = data_deptos,
             aes(map_id = id, x = long, y = lat, group = group), fill = color_map,
             color=color_frontier, size = 0.25) +
    expand_limits(x = data_deptos$long, y = data_deptos$lat) + coord_fixed()

  graph <- graph + geom_point(data = data, aes(x = a, y = b),
                              size = data$c * scale_point,
                              colour = color_point, alpha = alpha) + coord_map() + coord_fixed()  +
    labs(x = "", y = "", title = titleLabel, subtitle = subtitle, caption = caption) + theme_ds() + theme_ds_clean() +
    theme(legend.position=leg_pos)

  if(text){
    if(prop_text == "all"){
      graph <- graph + geom_text(data = data_centroids,
                                 aes(label = name_id, x = x, y = y,
                                     check_overlap = TRUE), size = text_size)
    }else{
      if(is.vector(prop_text) & class(prop_text) == "character"){
        prop_text <- data.frame(name_id = prop_text)
        prop_text <- prop_text %>% dplyr::inner_join(., data_centroids, by = c("name_id"))
        graph <- graph + geom_text(data = prop_text,
                                   aes(label = name_id, x = x, y = y,
                                       check_overlap = TRUE), size = text_size)
      }else{
        data_centroids <- sample_n(data_centroids, dim(data_centroids)[1] * prop_text)
        graph <- graph + geom_text(data = data_centroids,
                                   aes(label = name_id, x = x, y = y,
                                       check_overlap = TRUE), size = text_size)
      }
    }
  }
  options(warn=0)

  graph
}

#' Points inside Latam map
#' Points inside Latam map
#' @name gg_bubble_latam_GnmNum.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gnm-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_latam_GnmNum. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                    leg_pos = "right", color_point = "red",
                                    color_map = "gray", color_frontier = "white", scale_point = 0.25,
                                    alpha = 0.5, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flab <- fillLabel %||% nms[3]
  data <- f$d

  options(warn=-1)
  data_latam <- suppressMessages(read_csv(system.file("geo/latam.csv", package = "gggeomagic"), col_names = TRUE))

  graph <- ggplot(data_latam) +
    geom_map(map = data_latam,
             aes(map_id = id, x = long, y = lat, group = group), fill = color_map,
             color=color_frontier, size = 0.25) +
    expand_limits(x = data_latam$long, y = data_latam$lat) + coord_fixed()

  graph <- graph + geom_point(data = data, aes(x = a, y = b), size = data$c * scale_point,
                              colour = color_point, alpha = alpha) + coord_map() + coord_fixed()  +
    labs(x = "", y = "", title = titleLabel, subtitle = subtitle, caption = caption) + theme_ds() + theme_ds_clean() +
    theme(legend.position=leg_pos)
  options(warn=0)

  graph
}

#' Points inside Colombia's mpios map
#' Points inside Colombia's mpios map
#' @name gg_bubble_depto_GnmNum.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gnm-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_depto_GnmNum. <- function(data, titleLabel = "", subtitle = "", caption = "", depto_ = "05", leg_pos = "right", prop_text = 0.1,
                                    fillLabel = NULL, color_point = "red", text = FALSE, text_size = 2,
                                    color_map = "gray", color_frontier = "white", scale_point = 0.25,
                                    alpha = 0.5, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flab <- fillLabel %||% nms[2]
  data <- f$d

  options(warn=-1)
  data_mpios <- suppressMessages(read_csv(system.file("geo/mpios_depto_co.csv", package = "gggeomagic"), col_names = TRUE))
  data_mpios <- data_mpios %>% filter(depto == depto_)

  data_centroids <- suppressMessages(read_csv(system.file("geo/centroids_mpios_depto_co.csv", package = "gggeomagic"), col_names = TRUE))

  graph <- ggplot() +
    geom_map(data = data_mpios, map = data_mpios,
             aes(map_id = id, x = long, y = lat, group = group), fill = color_map,
             color = color_frontier, size = 0.25) +
    expand_limits(x = data_mpios$long, y = data_mpios$lat) + coord_map()

  graph <- graph + geom_point(data = data, aes(x = a, y = b), size = data$c * scale_point,
                              colour = color_point, alpha = alpha) + coord_map() + coord_fixed()  +
    labs(x = "", y = "", title = titleLabel, subtitle = subtitle, caption = caption) + theme_ds() + theme_ds_clean() +
    theme(legend.position=leg_pos)

  if(text){
    data_centroids <- data_centroids %>% filter(depto == depto_)
    if(prop_text == "all"){
      graph <- graph + geom_text(data = data_centroids,
                                 aes(label = name_id, x = x, y = y,
                                     check_overlap = TRUE), size = text_size)
    }else{
      if(is.vector(prop_text) & class(prop_text) == "character"){
        prop_text <- data.frame(name_id = prop_text)
        prop_text <- prop_text %>% dplyr::inner_join(., data_centroids, by = c("name_id"))
        graph <- graph + geom_text(data = prop_text,
                                   aes(label = name_id, x = x, y = y,
                                       check_overlap = TRUE), size = text_size)
      }else{
        data_centroids <- sample_n(data_centroids, dim(data_centroids)[1] * prop_text)
        graph <- graph + geom_text(data = data_centroids,
                                   aes(label = name_id, x = x, y = y,
                                       check_overlap = TRUE), size = text_size)
      }
    }
  }

  options(warn=0)

  graph
}

#' Points inside Colombia's deptos map
#' Points inside Colombia's deptos map
#' @name gg_bubble_co_CatGnm.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Gnm
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_co_CatGnm. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                 leg_pos = "right", color_map = "gray",
                                 color_frontier = "white", scale_point = 0.25, alpha = 0.5, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flab <- fillLabel %||% nms[3]
  data <- f$d

  options(warn=-1)
  data_deptos <- suppressMessages(read_csv(system.file("geo/deptos_co.csv", package = "gggeomagic"), col_names = TRUE))

  data_graph <- data %>% dplyr::group_by(a, b, c) %>% dplyr::summarise(count = n())

  graph <- ggplot(data_deptos) +
    geom_map(map = data_deptos,
             aes(map_id = id, x = long, y = lat, group = group), fill = color_map,
             color=color_frontier, size = 0.25) +
    expand_limits(x = data_deptos$long, y = data_deptos$lat) + coord_fixed()

  graph <- graph + geom_point(data = data_graph, aes(x = b, y = c, colour = a,
                                                     size = count * scale_point), alpha = alpha) +
    coord_map() + coord_fixed() + scale_size(guide = 'none') +
    scale_color_manual(values = getPalette()) +
    labs(x = "", y = "", title = titleLabel, subtitle = subtitle, caption = caption) + theme_ds() + theme_ds_clean() +
    theme(legend.position=leg_pos)
  options(warn=0)

  graph
}

#' Points inside Colombia's mpios map
#' Points inside Colombia's mpios map
#' @name gg_bubble_depto_CaGnm.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Gnm
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_depto_CaGnm. <- function(data, titleLabel = "", subtitle = "", caption = "", depto_ = "05", leg_pos = "right",
                                   fillLabel = NULL, text = FALSE, text_size = 2.5, prop_text = 0.1,
                                   color_map = "gray", color_frontier = "white", scale_point = 0.25,
                                   alpha = 0.5, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flab <- fillLabel %||% nms[2]
  data <- f$d

  options(warn=-1)
  data_mpios <- suppressMessages(read_csv(system.file("geo/mpios_depto_co.csv", package = "gggeomagic"), col_names = TRUE))
  data_mpios <- data_mpios %>% filter(depto == depto_)

  data_centroids <- suppressMessages(read_csv(system.file("geo/centroids_mpios_depto_co.csv", package = "gggeomagic"), col_names = TRUE))

  data_graph <- data %>% dplyr::group_by(a, b, c) %>% dplyr::summarise(count = n())

  graph <- ggplot() +
    geom_map(data = data_mpios, map = data_mpios,
             aes(map_id = id, x = long, y = lat, group = group), fill = color_map,
             color = color_frontier, size = 0.25) +
    expand_limits(x = data_mpios$long, y = data_mpios$lat) + coord_map()

  graph <- graph + geom_point(data = data_graph,
                              aes(x = b, y = c, group = a, color = a, size = count * scale_point),
                              alpha = alpha) +
    coord_map() + coord_fixed() + scale_size(guide = 'none') +
    scale_color_manual(values = getPalette())  +
    labs(x = "", y = "", title = titleLabel, subtitle = subtitle, caption = caption) + theme_ds() + theme_ds_clean() +
    theme(legend.position=leg_pos)

  if(text){
    data_centroids <- data_centroids %>% filter(depto == depto_)
    if(prop_text == "all"){
      graph <- graph + geom_text(data = data_centroids,
                                 aes(label = name_id, x = x, y = y,
                                     check_overlap = TRUE), size = text_size)
    }else{
      if(is.vector(prop_text) & class(prop_text) == "character"){
        prop_text <- data.frame(name_id = prop_text)
        prop_text <- prop_text %>% dplyr::inner_join(., data_centroids, by = c("name_id"))
        graph <- graph + geom_text(data = prop_text,
                                   aes(label = name_id, x = x, y = y,
                                       check_overlap = TRUE), size = text_size)
      }else{
        data_centroids <- sample_n(data_centroids, dim(data_centroids)[1] * prop_text)
        graph <- graph + geom_text(data = data_centroids,
                                   aes(label = name_id, x = x, y = y,
                                       check_overlap = TRUE), size = text_size)
      }
    }
  }
  options(warn=0)

  graph
}
