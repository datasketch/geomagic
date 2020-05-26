#' @export
geomagic_prep <- function(data = NULL, opts = NULL, by_col = "name") {

  if (is.null(opts)) return()

  map_name <- opts$extra$map_name
  ggmap <- geodataMeta(map_name)

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

  if (map_name == "usa_states") {
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
        dplyr::group_by_all() %>%
        dplyr::summarise(c = n())
      ind_nms <- length(nms)+1
      nms[ind_nms] <- 'Count'
      names(nms) <- c(names(nms)[-ind_nms], 'c')
      dic_num <- data.frame(id = "c", label = "Count",
                            hdType= as_hdType(x = "Num"), id_letters = "c")
      dic <- dic %>% bind_rows(dic_num)
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
      d <- summarizeData(d, opts$summarize$agg, to_agg = c, a, b) %>% drop_na(a, c)
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

   general_label <- nms[names(nms) == num_var[1]]
   general_label <- opts$chart$tooltip %||% paste0("{", general_label, "}")

    d <- d %>%
      mutate(labels = ifelse(is.na(a), NA,
                             glue::glue(geom_labels(nms,
                                         tooltip = general_label)))
             )



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

  }
print(centroides)
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
      size = opts$theme$text_size/5,
      family = opts$theme$text_family,
      colour = opts$theme$text_color
    ),
    projections = list(projection = opts$extra$map_projection,
                       lat = opts$extra$map_projection_lat,
                       long = opts$extra$map_projection_long,
                       rotation = opts$extra$map_projection_rotation,
                       add_params = opts$extra$map_projection_params),
    graticule = list(map_graticule = opts$extra$map_graticule,
                     background = opts$theme$background_color),
    legend = list(colors = opts$theme$palette_colors,
                  color_scale = opts$extra$map_color_scale,
                  na_color = opts$theme$na_color)
  )

}
