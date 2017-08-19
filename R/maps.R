#' Choropleth map
#' Choropleth map
#' @name gg_choropleth_map_GnmNum.
#' @param x A code.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gnm-Num
#' @examples
gg_choropleth_map_GnmNum. <- function(data = NULL, mapName,
                                      opts = list(titleLabel = "",
                                                  subtitle = "",
                                                  caption = "",
                                                  reverse = FALSE,
                                                  fillLabel = "",
                                                  text = TRUE,
                                                  text_size = 2,
                                                  prop_text = 'only_data',
                                                  leg_pos = "right",
                                                  titleLeg = "",
                                                  color_map = "gray",
                                                  color_frontier = "white")){
  ggmap <- geodataMeta(mapName)
  ggmap$path <- file.path("geodata",ggmap$geoname,paste0(ggmap$basename,".topojson"))
  ggmap$centroides <- file.path("geodata",ggmap$geoname,paste0(ggmap$basename,".csv"))

  tj <- topojson_read(system.file(ggmap$path,package = "geodata"))

  data_map <- fortify(tj) %>% mutate(.id = as.numeric(id)) %>% select(-id)
  data_info <- tj@data %>% mutate(.id = 0:(nrow(.)-1)) #%>% select(-id)


  data_map <- left_join(data_map, data_info)

  graph <- ggplot() +
    geom_map(data = data_map, map = data_map,
             aes(map_id = id, x = long, y = lat, group = group), fill = opts$color_map,
             color = opts$color_frontier, size = 0.25) + coord_map() +
    expand_limits(x = data_map$long, y = data_map$lat)

  graph <- graph + theme_ds() + theme_ds_clean() + coord_equal(ratio=1)

  if(!is.null(data)){

    f <- fringe(data)
    nms <- getClabels(f)
    flab <- opts$fillLabel %||% nms[2]
    data <- f$d

    data$a <- as.character(data$a)
    data_map$a <- as.character(data_map$id)

    data <- data %>% group_by(a) %>% dplyr::summarise(total = mean(b))
    data_graph <- dplyr::inner_join(data, data_map, by = "a")
    #names(data_graph)[which(names(data_graph) == "a")] <- "id"

    graph <- graph +
      geom_map(data = data_graph, map = data_graph,
               aes(map_id = id, x = long, y = lat, group = group, fill = total),
               color = opts$color_frontier, size = 0.25)

  }else{
    graph <- graph
  }

  centroides <- read_csv(system.file(ggmap$centroides,package = "geodata"))

  if(opts$text){
    if(opts$prop_text == "all"){
      graph <- graph + geom_text(data = centroides,
                                 aes(label = name, x = lon, y = lat,
                                     check_overlap = TRUE), size = opts$text_size)
    }else{
      if(opts$prop_text == "only_data"){
        if(!is.null(data)){
          dat_text <- data.frame(id = data$a)
          dat_text$id <- as.character(dat_text$id)
          dat_text <- dat_text %>% dplyr::inner_join(., centroides, by = c("id"))
          graph <- graph + geom_text(data = dat_text,
                                     aes(label = name, x = lon, y = lat,
                                         check_overlap = TRUE), size = opts$text_size)
        }else{
          graph <- graph
        }
      }else{
        if(is.vector(opts$prop_text) & class(opts$prop_text) == "character"){
          dat_text <- data.frame(name = opts$prop_text)
          dat_text <- opts$prop_text %>% dplyr::inner_join(.,centroides, by = c("name"))
          graph <- graph + geom_text(data = dat_text,
                                     aes(label = name, x = lon, y = lat,
                                         check_overlap = TRUE), size = opts$text_size)
        }
      }
    }
  }
  if(opts$reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }

  graph <- graph + labs(x = "", y = "", title = opts$titleLabel, subtitle = opts$subtitle, caption = opts$caption) +
    theme_ds() + theme_ds_clean() + theme(legend.position=opts$leg_pos) +
    guides(fill = guide_legend(title = opts$titleLeg))

  graph
}



#' Bubbles map
#' Bubbles map
#' @name gg_bubbles_map_GnmNum.
#' @param x A code.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gnm-Num
#' @examples
gg_bubbles_map_GnmNum. <- function(data = NULL, mapName,
                                   opts = list(titleLabel = "",
                                               subtitle = "",
                                               caption = "",
                                               fillLabel = "",
                                               prop_text = 'only_data',
                                               color_point = "red",
                                               leg_pos = "right",
                                               titleLeg = "",
                                               text = FALSE,
                                               text_size = 2,
                                               color_map = "gray",
                                               color_frontier = "white",
                                               scale_point = 3,
                                               alpha = 0.5)){

  ggmap <- geodataMeta(mapName)
  ggmap$path <- file.path("geodata",ggmap$geoname,paste0(ggmap$basename,".topojson"))
  ggmap$centroides <- file.path("geodata",ggmap$geoname,paste0(ggmap$basename,".csv"))

  tj <- topojson_read(system.file(ggmap$path,package = "geodata"))

  data_map <- fortify(tj) %>% mutate(.id = as.numeric(id)) %>% select(-id)
  data_info <- tj@data %>% mutate(.id = 0:(nrow(.)-1)) #%>% select(-id)


  data_map <- left_join(data_map, data_info)

  graph <- ggplot() +
    geom_map(data = data_map, map = data_map,
             aes(map_id = id, x = long, y = lat, group = group), fill = opts$color_map,
             color = opts$color_frontier, size = 0.25) + coord_map() +
    expand_limits(x = data_map$long, y = data_map$lat)

  graph <- graph + theme_ds() + theme_ds_clean() + coord_equal(ratio=1)
  centroides <- read_csv(system.file(ggmap$centroides,package = "geodata"))

  if(!is.null(data)){

    f <- fringe(data)
    nms <- getClabels(f)
    flab <- opts$fillLabel %||% nms[2]
    data <- f$d

    data$a <- as.character(data$a)
    data_map$a <- as.character(data_map$id)

    data_graph <- data %>% dplyr::group_by(a) %>% dplyr::summarise(total = sum(b))
    centroides$a <- centroides$id
    data_graph <- dplyr::inner_join(data_graph, centroides, by = "a")

    graph <- graph + geom_point(data = data_graph,
                                aes(x=lon, y=lat, size=total), color = "red", colour = opts$color_point, alpha = opts$alpha) +
      scale_size(range = c(0.2, opts$scale_point))

  }else{
    graph <- graph
  }


  if(opts$text){
    if(opts$prop_text == "all"){
      graph <- graph + geom_text(data = centroides,
                                 aes(label = name, x = lon, y = lat,
                                     check_overlap = TRUE), size = opts$text_size)
    }else{
      if(opts$prop_text == "only_data"){
        if(!is.null(data)){
          dat_text <- data.frame(id = data$a)
          dat_text$id <- as.character(dat_text$id)
          dat_text <- dat_text %>% dplyr::inner_join(., centroides, by = c("id"))
          graph <- graph + geom_text(data = dat_text,
                                     aes(label = name, x = lon, y = lat,
                                         check_overlap = TRUE), size = opts$text_size)
        }else{
          graph <- graph
        }
      }else{
        if(is.vector(opts$prop_text) & class(opts$prop_text) == "character"){
          dat_text <- data.frame(name = opts$prop_text)
          dat_text <- opts$prop_text %>% dplyr::inner_join(.,centroides, by = c("name"))
          graph <- graph + geom_text(data = dat_text,
                                     aes(label = name, x = lon, y = lat,
                                         check_overlap = TRUE), size = opts$text_size)
        }
      }
    }
  }

  graph <-  graph + labs(x = "", y = "", title = opts$titleLabel, subtitle = opts$subtitle, caption = opts$caption, size = opts$titleLeg) +
    theme_ds() + theme_ds_clean() + theme(legend.position=opts$leg_pos)


  graph

}

