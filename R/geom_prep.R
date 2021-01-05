#' @export
geomagic_prep <- function(data = NULL, opts = NULL, by_col = "name") {

  if (is.null(opts)) return()

  map_name <- opts$extra$map_name
  ggmap <- geodataMeta(map_name)

  palette_colors <-  opts$theme$palette_colors
  palette_type <-  opts$theme$palette_type
  color_scale <- opts$extra$map_color_scale

  ggmap$path <- file.path("geodata",ggmap$geoname,paste0(ggmap$basename,".topojson"))
  ggmap$centroides <- file.path("geodata",ggmap$geoname,paste0(ggmap$basename,".csv"))
  centroides <- read_csv(system.file(ggmap$centroides,package = "geodata"))
  if (is.null(data)) centroides$labels <- centroides[[by_col]]
  tj <- rgdal::readOGR(system.file(ggmap$path,package = "geodata"))


  data_map <- fortify(tj) %>%
    dplyr::mutate(.id = as.numeric(id)) %>% dplyr::select(-id)

  data_info <- tj@data %>%
    dplyr::mutate(.id = 0:(nrow(.)-1)) #%>% dplyr::select(-id)
  data_map <- data_map %>%
    dplyr::left_join( data_info)
  data_map$name_alt <- iconv(tolower(data_map[[by_col]]), to = "ASCII//TRANSLIT")

  if (map_name == "usa_states" | map_name == "usa_counties") {
    if (!opts$extra$map_add_alaska)
      data_map <- data_map[data_map$name != "Alaska",]
  }

  if (!is.null(data)) {

    f <- homodatum::fringe(data)
    nms <- homodatum::fringe_labels(f)
    d <- homodatum::fringe_d(f)
    dic <- homodatum::fringe_dic(f, id_letters = TRUE)
    frtype_d <- f$frtype

    if(frtype_d %in% c("Gcd", "Gnm", "Cat")){
      d <- d %>%
        dplyr::group_by_all() %>%
        dplyr::summarise(b = n())
      ind_nms <- length(nms)+1
      nms[ind_nms] <- 'Count'
      names(nms) <- c(names(nms)[-ind_nms], 'b')
      dic_num <- data.frame(id = "b", label = "Count",
                            hdType= as_hdType(x = "Num"), id_letters = "b")
      dic <- dic %>% bind_rows(dic_num)
    } else if (frtype_d %in% c("Gnm-Cat", "Gcd-Cat", "Cat-Cat")) {

      d <- d %>%
        filter(complete.cases(b)) %>%
        dplyr::group_by_all() %>%
        dplyr::summarise(c = n()) %>%
        dplyr::filter(c == max(c)) %>%
        dplyr::group_by(a) %>%
        dplyr::mutate(d = n(), b = ifelse(d == 1, b, "tie")) %>%
        dplyr::distinct(a, b, c)

        ind_nms <- length(nms)+1
        nms[ind_nms] <- 'Count'
        names(nms) <- c(names(nms)[-ind_nms], 'c')
        dic_num <- data.frame(id = "c", label = "Count", hdType= as_hdType(x = "Num"), id_letters = "c")
        dic <- dic %>% bind_rows(dic_num)

        color_scale <- "Category"

    } else if (frtype_d %in% "Gln-Glt-Cat") {
      d <- d %>%
        dplyr::group_by_all() %>%
        dplyr::summarise(d = n())
      ind_nms <- length(nms)+1
      nms[ind_nms] <- 'Count'
      names(nms) <- c(names(nms)[-ind_nms], 'd')
      dic_num <- data.frame(id = "d", label = "Count",
                            hdType= as_hdType(x = "Num"), id_letters = "d")
      dic <- dic %>% bind_rows(dic_num)
    } else if (frtype_d %in% c("Gcd-Num", "Gnm-Num", "Cat-Num")) {
      d <- summarizeData(d, opts$summarize$agg, to_agg = b, a) %>% drop_na()
    } else if (frtype_d %in% c("Gcd-Cat-Num", "Gnm-Cat-Num", "Gln-Glt-Cat")) {
      d <- summarizeData(d, opts$summarize$agg, to_agg = c, a, b) %>%
        drop_na(a, c) %>%
        dplyr::group_by(a) %>%
        dplyr::filter(c == max(c)) %>%
        dplyr::mutate(d = n(), b = ifelse(d == 1, b, "tie")) %>%
        dplyr::distinct(a, b, c)

      color_scale <- "Category"

    } else if (frtype_d %in% c("Gln-Glt", "Glt-Gln", "Num-Num")) {
      d <- d %>% mutate(c = opts$extra$map_radius) %>% drop_na()
    } else {
      d <- d %>% drop_na()
    }


   cat_var <- dic$id_letters[grepl("Gnm|Gcd|Cat", dic$hdType)]
   l <- map(cat_var, function(i) {
     d[[paste0(i, "_label")]] <<- makeup::makeup_chr(d[[i]], opts$style$format_cat_sample)
   })

   num_var <- dic$id_letters[grepl("Num|Gln|Glt", dic$hdType)]
   l <- map(num_var, function(i) {
     d[[paste0(i, "_label")]] <<- makeup::makeup_num(d[[i]],
                                                     opts$style$format_num_sample,
                                                     suffix = opts$style$suffix,
                                                     prefix = opts$style$prefix)
   })


    if (!identical(grep("Gnm|Gcd|Cat", dic$hdType) == 1, logical(0))) {
      if (opts$extra$map_color_scale == "Bins") {
        d <- gg_cuts(d, "b", sample = opts$style$format_num_sample,
                     bins = opts$extra$map_bins, suffix = opts$style$suffix,
                     prefix = opts$style$prefix)
      }
      d <- d %>%
        mutate(name_alt = iconv(tolower(a), to = "ASCII//TRANSLIT"))
      data_map <- data_map %>% left_join(d, by = "name_alt")
      centroides$name_alt <- iconv(tolower(centroides[[by_col]]), to = "ASCII//TRANSLIT")
      centroides <- centroides %>% left_join(d, by = "name_alt") %>% drop_na()
    } else {
      data_map <- data_map
    }

    data <- d
    centroides <- centroides %>%
      mutate(labels = as.character(glue::glue(gg_labels(nms, label = opts$chart$tooltip))))

    # define color palette based on data type
    var_cat <- "Cat" %in% dic$hdType
    if(!is.null(palette_type)){
      if(!palette_type %in% c("categorical", "sequential", "divergent")){
        warning("Palette type must be one of 'categorical', 'sequential', or 'divergent'; reverting to default.")
        palette_type <- NULL
      }
      if(!var_cat & palette_type == "categorical" | (var_cat & palette_type %in% c("sequential", "divergent"))){
        warning("Palette type might not be suitable for data type.")
      }
    } else {
      if(var_cat){
        palette_type <- "categorical"
      } else {
        palette_type <- "sequential"
      }
    }

    if(is.null(palette_colors)){
      if(palette_type == "categorical"){
        palette_colors <- opts$theme$palette_colors_categorical
      } else if (palette_type == "sequential"){
        palette_colors <- opts$theme$palette_colors_sequential
      } else if (palette_type == "divergent"){
        palette_colors <- opts$theme$palette_colors_divergent
      }
    }

  }

  list(
    d = data_map,
    data = data,
    centroides = centroides,
    titles = list(
      title = opts$title$title,
      subtitle = opts$title$subtitle,
      caption = opts$title$caption,
      legend = opts$title$legend_title
    ),
    theme = opts$theme,
    text = list(
      size = opts$dataLabels$dataLabels_size %||% opts$theme$text_size/5,
      family = opts$theme$text_family,
      colour = opts$dataLabels$dataLabels_color %||% opts$theme$text_color,
      show = opts$dataLabels$dataLabels_show
    ),
    projections = list(projection = opts$extra$map_projection,
                       lat = opts$extra$map_projection_lat,
                       long = opts$extra$map_projection_long,
                       rotation = opts$extra$map_projection_rotation,
                       add_params = opts$extra$map_projection_params),
    graticule = list(map_graticule = opts$extra$map_graticule,
                     background = opts$theme$background_color),
    legend = list(colors = palette_colors,
                  color_scale = color_scale,
                  na_color = opts$theme$na_color)
  )

}
