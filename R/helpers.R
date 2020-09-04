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

# Find name or id
#' @export
geoType <- function(data, map_name) {

  f <- homodatum::fringe(data)
  nms <- homodatum::fringe_labels(f)
  d <- homodatum::fringe_d(f)

  lfmap <- geodataMeta(map_name)
  centroides <- data_centroid(lfmap$geoname, lfmap$basename)
  vs <- NULL
  values <- intersect(d[["a"]], centroides[["id"]])

  if (identical(values, character(0))) {
    values <- intersect(d[["a"]], centroides[["name"]])
    if(!identical(values, character())) vs <- "Gnm"
  } else {
    vs <- "Gcd"
  }
  vs
}


# fake data
#' @export
fakeData <- function(map_name = NULL, ...) {
  if (is.null(map_name)) return()
  lfmap <- geodataMeta(map_name)
  centroides <- data_centroid(lfmap$geoname, lfmap$basename)
  d <- data.frame(name =sample(centroides$name, 11), fake_value = rnorm(11, 33, 333))
  d
}

# template datalabels
#' @export
gg_labels <- function(nms, label = NULL) {
  if (is.null(nms)) stop("Enter names")
  nms_names <- names(nms)
  label <- label %||% ""
  if (label == "") {
    l <- map(seq_along(nms), function(i){
      paste0("{",nms_names[i], "_label}")
    }) %>% unlist()
    label <- paste0(l, collapse = "\n")
  } else {
    points <- gsub("\\{|\\}", "",
                   stringr::str_extract_all(label, "\\{.*?\\}")[[1]])
    if (identical(points, character())) {
      label <- label
    } else {
      #i <- 1
      l <- purrr::map(1:length(points), function(i){
        true_points <-  paste0(names(nms[match(points[i], nms)]),"_label")
        label <<- gsub(points[i], true_points, label, fixed = TRUE)
      })[[length(points)]]}
  }
  label
}


# guess ftypes changed cat by Gnm or Gcd
#' @export
guess_ftypes <- function(data, map_name) {
  if (is.null(map_name))
    stop("Please type a map name")
  if (is.null(data)) return()

  data <- fringe(data)
  d <- data$data
  dic <- data$dic
  lfmap <- geodataMeta(map_name)
  centroides <- data_centroid(lfmap$geoname, lfmap$basename)
  centroides$id <- iconv(tolower(centroides$id), to = "ASCII//TRANSLIT")
  centroides$name <- iconv(tolower(centroides$name), to = "ASCII//TRANSLIT")


  l_gcd <- map(names(d), function(i){
    class_var <- data$frtype
    gcd_in <- sum(centroides$id %in% iconv(tolower(d[[i]]), to = "ASCII//TRANSLIT"))
    gcd_in > 0
  })
  names(l_gcd) <- names(d)
  this_gcd <- names(which(l_gcd == TRUE))

  if (identical(this_gcd, character())) {
    l_gnm <- map(names(d), function(i){
      class_var <- data$frtype
      gnm_in <- sum(centroides$name %in%  iconv(tolower(d[[i]]), to = "ASCII//TRANSLIT"))
      gnm_in > 0
    })
    names(l_gnm) <- names(d)
    this_gnm <- names(which(l_gnm == TRUE))
    if (identical(this_gnm, character())) {
      return()
    } else {
      dic$hdType[dic$id %in% this_gnm] <- "Gnm"
    }
  } else {
    dic$hdType[dic$id %in% this_gcd] <- "Gcd"
  }

  dic$hdType

}
