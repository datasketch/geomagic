#' Basic layer choroplets
gg_basic_choropleth <- function(l) {
  if (is.null(l$data)){
    g <- ggplot(data = l$d) +
      geom_polygon(aes( x = long, y = lat, group = group),
                   fill = l$theme$na_color,
                   color= l$theme$border_color)
  } else {
    if (is(l$d$b, "character")){
      g <- ggplot(data = l$d) +
        geom_polygon(aes( x = long, y = lat, group = group, fill = b, alpha = c),
                     color= l$theme$border_color) +
        scale_alpha_continuous(name = NULL)
    } else {
      g <- ggplot(data = l$d) +
        geom_polygon(aes( x = long, y = lat, group = group, fill = b),
                     color= l$theme$border_color)
    }
  }
  g
}


#' Basic layer bubbles
gg_basic_bubbles <- function(l) {

  g <- ggplot(data = l$d) +
    geom_polygon(aes( x = long, y = lat, group = group),
                 fill = l$theme$na_color,
                 color= l$theme$border_color)
  if (!is.null(l$data)) {

    if (is(l$d$b, "character")){
      g <- g +
        geom_point(data = l$centroides, aes(x = lon, y = lat, size = c, color = b))
    } else {
      g <- g +
        geom_point(data = l$centroides, aes(x = lon, y = lat, size = b), color = l$theme$palette_colors[1])
    }

  }

  g
}

#' Basic layer points
gg_basic_points <- function(l) {
  g <- ggplot(data = l$d) +
    geom_polygon(aes( x = long, y = lat, group = group),
                 fill = l$theme$na_color,
                 color= l$theme$border_color)
  if (!is.null(l$data)) {
    g <- g +
      geom_point(data = l$data, aes(x = a, y = b, size = c), colour = l$theme$palette_colors[1])
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

  if (graticule$map_graticule) {
    theme(
      panel.ontop = TRUE,   ## Note: this is to make the panel grid visible in this example
      panel.grid = element_blank(),
      line = element_blank(),
      rect = element_blank(),
      plot.background = element_rect(fill = graticule$background),
      panel.grid.major = element_line())
  } else {
    theme(
      panel.grid.major = element_blank()
    )
  }
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
      true_points <-  paste0(names(nms[match(points[i], nms)]),"_label")
      tooltip <<- gsub(points[i], true_points, tooltip, fixed = TRUE)
    })[[length(points)]]
  }
  #}
  tooltip
}

#'
gg_palette <- function(opts, map_type=NULL) {
  color_mapping_categorical <-"scale_fill_manual"
  if (!is.null(map_type)){
    if (map_type == "bubbles"){
      color_mapping_categorical <-"scale_color_manual"
      }
  }
  if (opts$color_scale == "Category") {
    color_mapping <- color_mapping_categorical
    l <- list(values = opts$colors, na.value = opts$na_color, name = NULL)
  } else if (opts$color_scale == "Quantile") {
    color_mapping <- color_mapping_categorical
    l <- list()
  } else if (opts$color_scale == 'Bins') {
    color_mapping <- color_mapping_categorical
    l <- list(values = opts$colors, na.value = opts$na_color)
  } else {
    if (length(opts$colors) == 1) opts$colors <- c(opts$colors, "#CCCCCC")
    color_mapping <- "scale_fill_gradient"
    l <- list(low = opts$colors[1],
              high = opts$colors[2],
              na.value = opts$na_color,
              labels = makeup::makeup_format(sample = opts$style$format_num_sample))
  }

  do.call(color_mapping, l)
}


#'
gg_cuts <- function (d, var = "b", sample, bins = 4, prefix, suffix, ...) {
  d <- d %>%
    mutate(cuts = gsub("\\[|\\)||]", "",
                       cut(d[[var]], bins, include.lowest = TRUE, right = FALSE)),
           id_cuts = cut(d[[var]], bins, labels = F, include.lowest = TRUE, right = FALSE)
    ) %>%
    separate(cuts, c("cut_inf", "cut_sup"), sep = ",") %>%
    mutate(cut_inf = makeup::makeup_num(as.numeric(cut_inf), sample, prefix = prefix, suffix = suffix),
           cut_sup = makeup::makeup_num(as.numeric(cut_sup), sample, prefix = prefix, suffix = suffix))%>%
    arrange(id_cuts)
  d[[var]] <- paste0(d$cut_inf, " - ", d$cut_sup)
  d[[var]] <- factor(d[[var]], levels = unique(d[[var]]))
  d
}
