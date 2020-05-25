#' @export
geomagic_prep <- function(data = NULL, opts = NULL) {

  if (is.null(opts)) return()

  map_name <- opts$extra$map_name
  ggmap <- geodataMeta(map_name)

  ggmap$path <- file.path("geodata",ggmap$geoname,paste0(ggmap$basename,".topojson"))
  ggmap$centroides <- file.path("geodata",ggmap$geoname,paste0(ggmap$basename,".csv"))
  centroides <- read_csv(system.file(ggmap$centroides,package = "geodata"))

  tj <- rgdal::readOGR(system.file(ggmap$path,package = "geodata"))


  data_map <- fortify(tj) %>%
    dplyr::mutate(.id = as.numeric(id)) %>% dplyr::select(-id)

  data_info <- tj@data %>%
    dplyr::mutate(.id = 0:(nrow(.)-1)) #%>% dplyr::select(-id)
  data_map <- data_map %>%
    dplyr::left_join( data_info)

  if (map_name == "usa_states") {
    if (!opts$extra$map_add_alaska)
      data_map <- data_map[data_map$name != "Alaska",]
  }



  data_map

}
