#' @export
availableMaps <- availableGeodata

#' @export
preprocessData <- function(data, mapName){
  if(!is.null(data$code)){
    if(mapName == "col_municipalities"){
      data$code <- sprintf("%05d", as.numeric(data$code))
    }
    if(mapName == "col_departments"){
      data$code <- sprintf("%02d", as.numeric(data$code))
    }
  }
  data
}
