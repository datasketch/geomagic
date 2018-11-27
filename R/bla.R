
#' #' Choropleth map
#' #' Choropleth map
#' #' @name gg_choropleth_map_GcdCat.
#' #' @param x A code.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Gcd-Cat
#' #' @examples
#' #' NULL
#' gg_choropleth_map_GcdCat. <- function(data = NULL, mapName,
#'                                       opts = NULL, ...){
#'   opts <- parseOpts(opts = opts, ...)
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
#'     data <- data %>% distinct(a, .keep_all = TRUE)
#'     data$a <- as.character(data$a)
#'     data_map$a <- as.character(data_map$id)
#'
#'     nLevels <- length(unique(data$b))
#'
#'     data_graph <- dplyr::inner_join(data, data_map, by = "a")
#'     #names(data_graph)[which(names(data_graph) == "a")] <- "id"
#'
#'     graph <- graph +
#'       geom_map(data = data_graph, map = data_graph,
#'                aes(map_id = id, x = long, y = lat, group = group, fill = factor(b)),
#'                color = opts$color_frontier, size = 0.25)
#'
#'   }else{
#'     graph <- graph
#'   }
#'
#'   centroides <- read_csv(system.file(ggmap$centroides,package = "geodata"))
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
#'   graph <- graph +  scale_fill_continuous(low = opts$lowC, high = opts$highC)
#'
#'   graph <- graph + labs(x = "", y = "", title = opts$titleLabel, subtitle = opts$subtitle, caption = opts$caption,fill=opts$titleLeg) +
#'     theme_ds() + theme_ds_clean() +
#'     theme(legend.position=opts$leg_pos, plot.background = element_rect(fill = opts$Bcolor,linetype = 'blank'),
#'           panel.background = element_rect(fill = opts$Bcolor,
#'                                           colour = opts$Bcolor,
#'                                           size = 1.5,  linetype = 'blank'),
#'           legend.background = element_rect(colour ='transparent' ,fill = 'transparent'))
#'
#'
#'   graph
#' }
#'
#'
#'
#' #' Choropleth map
#' #' Choropleth map
#' #' @name gg_choropleth_map_GcdNum.
#' #' @param x A code.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Gcd-Num
#' #' @examples
#' #' NULL
#' gg_choropleth_map_GcdNum. <- function(data = NULL, mapName,
#'                                       opts = NULL, ...){
#'   opts <- parseOpts(opts = opts, ...)
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
#'     data$a <- as.character(data$a)
#'     data_map$a <- as.character(data_map$id)
#'
#'     data <- data %>% group_by(a) %>% dplyr::summarise(total = mean(b))
#'     data_graph <- dplyr::inner_join(data, data_map, by = "a")
#'     #names(data_graph)[which(names(data_graph) == "a")] <- "id"
#'
#'     graph <- graph +
#'       geom_map(data = data_graph, map = data_graph,
#'                aes(map_id = id, x = long, y = lat, group = group, fill = total),
#'                color = opts$color_frontier, size = 0.25)
#'
#'   }else{
#'     graph <- graph
#'   }
#'
#'   centroides <- read_csv(system.file(ggmap$centroides,package = "geodata"))
#'   centroides$id <- as.character(centroides$id)
#'   centroides$name <- as.character(centroides$name)
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
#'   if(opts$reverse){
#'     graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
#'                                          high = getPalette(type = "sequential")[1])
#'   }else{
#'     graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
#'                                          high = getPalette(type = "sequential")[2])
#'   }
#'
#'   graph <- graph +  scale_fill_continuous(low = opts$lowC, high = opts$highC)
#'
#'   graph <- graph + labs(x = "", y = "", title = opts$titleLabel, subtitle = opts$subtitle, caption = opts$caption,fill=opts$titleLeg) +
#'     theme_ds() + theme_ds_clean() +
#'     theme(legend.position=opts$leg_pos,
#'           plot.background = element_rect(fill = opts$Bcolor,linetype = 'blank'),
#'           panel.background = element_rect(fill = opts$Bcolor,
#'                                           colour = opts$Bcolor,
#'                                           size = 1.5,  linetype = 'blank'),
#'           legend.background = element_rect(colour ='transparent' ,fill = 'transparent'))
#'
#'   graph
#' }
#'
#'
#'
#'
#' #vector <- rnorm(100, 300, 10000)
#'
#'
#' if (scale == 'discrete') {
#'   if (mode == 'quantile') {
#'
#'   } else {
#'
#'   }
#'
#'
#'   data$bins <- map_chr(1:nrow(data), function(y) {
#'     numBus <- data[[col]][y]
#'     ind <- min(which(numBus <= z[-1]))
#'     descLeg[ind]
#'   })
#'   dt <- fillColors(data, 'bins' , colors = colors, colorScale = 'discrete', highlightValue = NULL, highlightValueColor = NULL, numeric = F, labelWrap = 12)
#' } else if (scale == 'continuous') {
#'   data$bins <- data[[col]]
#'   dt <- fillColors(data, 'bins' , colors = colors, colorScale = 'continuous', highlightValue = NULL, highlightValueColor = NULL, numeric = T, labelWrap = 12)
#' } else {
#'   data$bins <- data[[col]]
#'   dt <- fillColors(data, names(data)[1] , colors = colors, colorScale = 'no', highlightValue = highlightValue, highlightValueColor = highlightValueColor, numeric = F, labelWrap = 12)
#' }
#' dt
