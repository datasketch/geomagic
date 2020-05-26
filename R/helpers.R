#' Basic layer choroplets
gg_basic_choropleth <- function(l) {
  if (is.null(l$data)){
    g <- ggplot(data = l$d) +
      geom_polygon(aes( x = long, y = lat, group = group),
                   fill = l$theme$na_color,
                   color= l$theme$border_color)
  } else {
    g <- ggplot(data = l$d) +
      geom_polygon(aes( x = long, y = lat, group = group, fill = b),
                   color= l$theme$border_color)
  }
  g
}

#' Projections
gg_projections <- function(opts_projections) {

  orientation <- c(opts_projections$lat, opts_projections$long, opts_projections$rotation)

  req_one_aditional_parms <- c("cylequalarea", "rectangular", "gall", "perspective", "fisheye",
                               "newyorker", "conic", "bonne", "bicentric", "elliptic", "mecca",
                               "homing")
  req_two_aditional_params <- c("simpleconic", "lambert", "albers", "trapezoidal", "lune")

  if (opts_projections$projection %in% req_one_aditional_parms) {
    l <- list(projection = opts_projections$projection, param = opts_projections$add_params)
  } else if (opts_projections$projection %in% req_one_aditional_parms) {
    l <- list(projection = opts_projections$projection, par=c(30,40))
  } else {
    l <- list(projection = opts_projections$projection)
  }
  c(l, orientation = list(orientation))
}

#' Graticule map
gg_graticule <- function(graticule) {

  if (!graticule$map_graticule) return()
  theme(
    panel.ontop = TRUE,   ## Note: this is to make the panel grid visible in this example
    panel.grid = element_blank(),
    line = element_blank(),
    rect = element_blank(),
    plot.background = element_rect(fill = graticule$background),
    panel.grid.major = element_line())
}

#' labels
geom_labels <- function(nms, tooltip) {
  if (is.null(nms)) stop("Enter names")
  #nms_names <- names(nms)
  # if (is.null(tooltip)) {
  #   l <- map(seq_along(nms), function(i){
  #     paste0(nms_names[i], "_label")
  #   }) %>% unlist()
  #   tooltip <- paste0(l, collapse = " \n ")
  # } else {
  points <- gsub("\\{|\\}", "",
                 stringr::str_extract_all(tooltip, "\\{.*?\\}")[[1]])
  if (identical(points, character())) {
    tooltip <- tooltip
  } else {
    l <- purrr::map(1:length(points), function(i){
      i <- 1
      true_points <-  paste0(names(nms[match(points[i], nms)]),"_label")
      tooltip <<- gsub(points[i], true_points, tooltip, fixed = TRUE)
    })[[length(points)]]
  }
  #}
  tooltip
}
