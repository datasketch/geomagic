

#' #' Bubbles map
#' #' Bubbles map
#' #' @name gg_bubbles_map_GcdNum.
#' #' @param x A code.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Gcd-Num
#' #' @examples
#' #' NULL
#' gg_bubbles_map_GcdNum. <- function(data = NULL, mapName,
#'                                    opts = list(titleLabel = "",
#'                                                subtitle = "",
#'                                                caption = "",
#'                                                fillLabel = "",
#'                                                prop_text = 'only_data',
#'                                                color_point = "red",
#'                                                leg_pos = "right",
#'                                                titleLeg = "",
#'                                                text = FALSE,
#'                                                text_size = 2,
#'                                                color_map = "gray",
#'                                                color_frontier = "white",
#'                                                scale_point = 3,
#'                                                alpha = 0.5)){
#'
#'   ggmap <- geodataMeta(mapName)
#'   ggmap$path <- file.path("geodata",ggmap$geoname,paste0(ggmap$basename,".topojson"))
#'   ggmap$centroides <- file.path("geodata",ggmap$geoname,paste0(ggmap$basename,".csv"))
#'
#'   tj <- topojson_read(system.file(ggmap$path,package = "geodata"))
#'
#'   data_map <- fortify(tj) %>% mutate(.id = as.numeric(id)) %>% select(-id)
#'   data_info <- tj@data %>% mutate(.id = 0:(nrow(.)-1)) #%>% select(-id)
#'
#'
#'   data_map <- left_join(data_map, data_info)
#'
#'   graph <- ggplot() +
#'     geom_map(data = data_map, map = data_map,
#'              aes(map_id = id, x = long, y = lat, group = group), fill = opts$color_map,
#'              color = opts$color_frontier, size = 0.25) + coord_map() +
#'     expand_limits(x = data_map$long, y = data_map$lat)
#'
#'   graph <- graph + theme_ds() + theme_ds_clean() + coord_equal(ratio=1)
#'   centroides <- read_csv(system.file(ggmap$centroides,package = "geodata"))
#'
#'   if(!is.null(data)){
#'
#'     f <- fringe(data)
#'     nms <- getClabels(f)
#'     flab <- opts$fillLabel %||% nms[2]
#'     data <- f$d
#'
#'     data$a <- as.character(data$a)
#'     data_map$a <- as.character(data_map$id)
#'
#'     data_graph <- data %>% dplyr::group_by(a) %>% dplyr::summarise(total = sum(b))
#'     centroides$a <- centroides$id
#'     data_graph <- dplyr::inner_join(data_graph, centroides, by = "a")
#'
#'     graph <- graph + geom_point(data = data_graph,
#'                                 aes(x=lon, y=lat, size=total), color = "red", colour = opts$color_point, alpha = opts$alpha) +
#'       scale_size(range = c(0.2, opts$scale_point))
#'
#'   }else{
#'     graph <- graph
#'   }
#'
#'
#'   if(opts$text){
#'     if(opts$prop_text == "all"){
#'       graph <- graph + geom_text(data = centroides,
#'                                  aes(label = name, x = lon, y = lat,
#'                                      check_overlap = TRUE), size = opts$text_size)
#'     }else{
#'       if(opts$prop_text == "only_data"){
#'         if(!is.null(data)){
#'           dat_text <- data.frame(id = data$a)
#'           dat_text$id <- as.character(dat_text$id)
#'           dat_text <- dat_text %>% dplyr::inner_join(., centroides, by = c("id"))
#'           graph <- graph + geom_text(data = dat_text,
#'                                      aes(label = name, x = lon, y = lat,
#'                                          check_overlap = TRUE), size = opts$text_size)
#'         }else{
#'           graph <- graph
#'         }
#'       }else{
#'         if(is.vector(opts$prop_text) & class(opts$prop_text) == "character"){
#'           dat_text <- data.frame(name = opts$prop_text)
#'           dat_text <- opts$prop_text %>% dplyr::inner_join(.,centroides, by = c("name"))
#'           graph <- graph + geom_text(data = dat_text,
#'                                      aes(label = name, x = lon, y = lat,
#'                                          check_overlap = TRUE), size = opts$text_size)
#'         }
#'       }
#'     }
#'   }
#'
#'   graph <-  graph + labs(x = "", y = "", title = opts$titleLabel, subtitle = opts$subtitle, caption = opts$caption, size = opts$titleLeg) +
#'     theme_ds() + theme_ds_clean() + theme(legend.position=opts$leg_pos)
#'
#'
#'   graph
#'
#' }
#'
#' #' Bubbles map
#' #' Bubbles map
#' #' @name gg_bubble_GcdLonLat.
#' #' @param x longitud.
#' #' @param y latitud.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: pLon-Lat
#' #' @examples
#' #' NULL
#' gg_bubble_GcdLonLat. <- function(data = NULL, mapName,
#'                                  opts = NULL, ...){
#'   #opts <- parseOptsBb(opts = opts, ...)
#'   ggmap <- geodataMeta(mapName)
#'   ggmap$path <- file.path("geodata",ggmap$geoname,paste0(ggmap$basename,".topojson"))
#'   ggmap$centroides <- file.path("geodata",ggmap$geoname,paste0(ggmap$basename,".csv"))
#'
#'   tj <- topojson_read(system.file(ggmap$path,package = "geodata"))
#'
#'   data_map <- fortify(tj) %>% mutate(.id = as.numeric(id)) %>% select(-id)
#'   data_info <- tj@data %>% mutate(.id = 0:(nrow(.)-1)) #%>% select(-id)
#'
#'
#'   data_map <- left_join(data_map, data_info)
#'
#'   graph <- ggplot() +
#'     geom_map(data = data_map, map = data_map,
#'              aes(map_id = id, x = long, y = lat, group = group), fill = opts$color_map,
#'              color = opts$color_frontier, size = 0.25) + coord_map() +
#'     expand_limits(x = data_map$long, y = data_map$lat)
#'
#'   graph <- graph + theme_ds() + theme_ds_clean() + coord_equal(ratio=1)
#'
#'   if(!is.null(data)){
#'
#'     f <- fringe(data)
#'     nms <- getClabels(f)
#'     flab <- opts$fillLabel %||% nms[2]
#'     data <- f$d
#'
#'     data_graph <- data %>% group_by(a, b) %>% summarise(count = n())
#'
#'     #names(data_graph)[which(names(data_graph) == "a")] <- "id"
#'
#'     graph <- graph + geom_point(data = data_graph, aes(x = a, y = b),
#'                                 size = data_graph$count * opts$scale_point,
#'                                 colour = opts$color_point, alpha = opts$alpha)
#'
#'   }else{
#'     graph <- graph
#'   }
#'
#'   centroides <- read_csv(system.file(ggmap$centroides,package = "geodata"))
#'
#'
#'   if(opts$text){
#'     if(opts$prop_text == "all"){
#'       graph <- graph + geom_text(data = centroides,
#'                                  aes(label = name, x = lon, y = lat,
#'                                      check_overlap = TRUE), size = opts$text_size)
#'     }else{
#'       graph <- graph
#'     }
#'   }
#'
#'   graph <-  graph + labs(x = "", y = "", title = opts$titleLabel, subtitle = opts$subtitle, caption = opts$caption, size = opts$titleLeg) +
#'     theme_ds() + theme_ds_clean() +
#'     theme(legend.position=opts$leg_pos,
#'           plot.background = element_rect(fill = opts$Bcolor,linetype = 'blank'),
#'           panel.background = element_rect(fill = opts$Bcolor,
#'                                           colour = opts$Bcolor,
#'                                           size = 1.5,  linetype = 'blank'),
#'           legend.background = element_rect(colour ='transparent' ,fill = 'transparent'))
#'
#'
#'   graph
#'
#' }
#'
#'
#'
#' #' Sketch map departments
#' #' Sketch map departments
#' #' @name gg_sketchmap_depto_GcdNum.
#' #' @param x A category.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Gcd-Num
#' #' @examples
#' #' NULL
#'  gg_sketchmap_depto_GcdNum. <- function(depto_ = depto_, color_map = "gold", color_frontier = "black"){
#'
#'    options(warn = -1)
#'    data_mpios <- suppressMessages(read_csv(system.file("geodata/col/mpios_depto_col.csv",
#'                                                        package = "geodata"), col_names = TRUE))
#'
#'
#'    data_mpios <- data_mpios %>% filter(depto == depto_)
#'    graph <- ggplot() +
#'      geom_map(data = data_mpios, map = data_mpios,
#'               aes(map_id = id, x = long, y = lat, group = group), fill = color_map,
#'               color = color_frontier, size = 0.25) +
#'      coord_map() +
#'      expand_limits(x = data_mpios$long, y = data_mpios$lat) +
#'      theme_ds_clean()
#'    graph
#'
#'  }
#'

