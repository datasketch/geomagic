






data <- data.frame(
  Pais = sample(unique(data_map$id), 33, replace = T),
  random = rnorm(33, 70, 300)
)

f <- fringe(data)
nms <- getClabels(f)
data <- f$d
data$a <- as.character(data$a)
data_map$a <- as.character(data_map$id)


data <- binsLeg(data, 'b', scale = 'discrete', mode = 'quantile', bins = 3, colors = NULL, highlightValue = NULL, highlightValueColor = NULL)

data_graph <- dplyr::inner_join(data, data_map, by = c("a"))
#data_graph$bins <- as.numeric(data_graph$bins)

g <- graph +
  geom_map(data = data_graph, map = data_graph,
           aes(map_id = id, x = long, y = lat, fill = bins),
           size = 0.25)
colors <- as.character(unique(data$color))
g + scale_fill_manual(values = colors)

g + scale_fill_continuous(low = 'white', high = 'orange')
g + scale_fill_continuous(low = 'gray', high = 'orange')

g + scale_fill_manual(values = unique(colorNumeric(c('orange', 'gray') , data_graph$bins)(data_graph$bins)))





#' Choropleth map
#' Choropleth map
#' @name gg_choropleth_map_GcdCat.
#' @param x A code.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Gcd-Cat
#' @examples
#' NULL
gg_choropleth_map_Gcd <- function(data = NULL,
                                  mapName = "world_countries",
                                  count = TRUE,
                                  title = NULL,
                                  subtitle = NULL,
                                  caption = NULL,
                                  legend = list(bins = 7,
                                                position = "bottomleft",
                                                title = NULL),
                                  border = list(weigth = 1,
                                                color = '#000000',
                                                opacity = 1),
                                  labelWrap = 12,
                                  fill = list(color = NULL,
                                              scale = 'discrete',
                                              mode = 'quantile',
                                              opacity = 0.7,
                                              nullColor = '#CCCCCC'
                                  ),
                                  marks = c(".", ","),
                                  nDigits = NULL,
                                  projections = NULL,
                                  highlightValue = NULL,
                                  highlightValueColor = '#F9B233',
                                  percentage = FALSE,
                                  format = c('', ''),
                                  showText = c(TRUE, TRUE),
                                  theme = NULL){


  ggmap <- geodataMeta(mapName)
  ggmap$path <- file.path("geodata",ggmap$geoname,paste0(ggmap$basename,".topojson"))
  ggmap$centroides <- file.path("geodata",ggmap$geoname,paste0(ggmap$basename,".csv"))

  tj <- topojson_read(system.file(ggmap$path,package = "geodata"))

  data_map <- fortify(tj) %>% mutate(.id = as.numeric(id)) %>% select(-id)
  data_info <- tj@data %>% mutate(.id = 0:(nrow(.)-1)) #%>% select(-id)


  data_map <- left_join(data_map, data_info)

  graph <- ggplot() +
    geom_map(data = data_map, map = data_map,
             aes(map_id = id, x = long, y = lat, group = group), fill = "#FFFCCC",
             color = "#000000", size = 0.25) +
    theme_map()

  graph


  data <- data.frame(nameP = c('COL', 'MEX', 'MEX', 'ARG'))
  #if(!is.null(data)){

  f <- fringe(data)
  nms <- getClabels(f)
  data <- f$d
  data$a <- as.character(data$a)
  data <- data %>% group_by(a) %>% summarise(b = n())

  descLeg <- binsLeg(vector = data$b, scale = legend$scale, bins = legend$bins)


  if (!count) {
    data <- data %>% distinct(a, .keep_all = TRUE)
    data$b <- data$a
  }

  data <- fillColors(data = data, 'b', colors = fill$color, 'continuous', highlightValue, highlightValueColor, labelWrap, bins = legend$bins, numeric = T)
  data_map$a <- as.character(data_map$id)
  data_graph <- dplyr::inner_join(data, data_map, by = c("a"))
  #names(data_graph)[which(names(data_graph) == "a")] <- "id"



















  g <- graph +
    geom_map(data = data_graph, map = data_graph,
             aes(map_id = id, x = long, y = lat, fill = b),
             size = 0.25)
  g
  g + scale_fill_manual(values = as.character(data$color), labels = descLeg)
  g + scale_fill_continuous(low = "white", high = "black")



}





data <- data.frame(nameP = c('COL', 'MEX', 'MEX', 'ARG'))
if(!is.null(data)){

  f <- fringe(data)
  nms <- getClabels(f)
  #flab <- opts$fillLabel %||% nms[2]
  data <- f$d
  data$a <- as.character(data$a)
  data <- data %>% group_by(a) %>% summarise(b = n())

  if (!count) {
    data <- data %>% distinct(a, .keep_all = TRUE)
    data$b <- data$a
  }

  if (fill$scale == 'no' | (count && fill$scale == 'discrete')) {
    var <- 'a'
    num <- FALSE
  } else {
    var <- 'b'
    num <- TRUE
  }

  data <- fillColors(data = data, var, colors = fill$color, fill$scale, highlightValue, highlightValueColor, labelWrap, bins = legend$bins, numeric = num)

  data_map$a <- as.character(data_map$id)

  if (fill$scale == 'discrete'){
    if (legend$type == 'discrete') {
      data$b <- cut(data$b, legend$bins)
    } else {
      data$b <- as.character(data$b)
    }
  }

  #nLevels <- length(unique(data$b))

  data_graph <- dplyr::inner_join(data, data_map, by = c("a"))
  #names(data_graph)[which(names(data_graph) == "a")] <- "id"
  g <- graph +
    geom_map(data = data_graph, map = data_graph,
             aes(map_id = id, x = long, y = lat, group = group, fill = c),
             size = 0.25)
  if (fill$scale == 'no') {
    g <- g + scale_fill_manual(values = data$a) + theme(legend.position="none")
  } else if (fill$scale == 'discrete') {
    g <- g +  scale_fill_manual(values = getPalette()[1:3] )
  } else {
    g <- g + scale_fill_continuous(low = "white", high = "black")
  }
}else{
  g <- g
}


#expand_limits(x = data_map$long, y = data_map$lat)

#graph <- graph + theme_ds() + theme_ds_clean()

# graph+ coord_equal(ratio=1)  + coord_map("azequalarea")
#
# graph    + coord_map("cylindrical")
# graph  + coord_map("azequalarea", orientation = c(-36.92, 174.6, 0))+ coord_equal(ratio=1)
# graph   + coord_map("lambert", parameters = c(-37, -44))
#
# graph  + coord_equal(ratio=1) + coord_map("ortho")
# # Looking up up at South Pole
# graph  +  coord_map("ortho", orientation = c(-90, 0, 0))
# # Centered on New York (currently has issues with closing polygons)
# graph  + coord_equal(ratio=1)  + coord_map("ortho", orientation = c(41, -74, 0))
#

