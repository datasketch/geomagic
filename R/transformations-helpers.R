# colores
#' @export
fillColors <- function (data, col, colors, colorScale, highlightValue, highlightValueColor,
                        labelWrap,  numeric = TRUE) {
  cat <- unique(data[[col]])
  highlightValue <- stringr::str_wrap(highlightValue, labelWrap)
  ds <- dsColorsHex(TRUE)
  ad <- c()
  if (!is.null(colors)) {
    cl <- col2rgb(colors)
    colors <- map_chr(1:ncol(cl), function(s) {
      rgb(cl[1, s], cl[2, s], cl[3, s], maxColorValue = 255)
    })
  }
  if (colorScale == "no") {
    if (is.null(colors)) {
      colors <- dsColorsHex()[2]
    }
    fillCol <- data.frame(a = cat,
                          color = rep(colors, length(cat))[1:length(cat)])
    names(fillCol)[1] <- col
  }
  if (colorScale == "discrete") {
    if (is.null(colors)) {
      colors <- dsColorsHex()
    }
    ad <- unlist(map(colors, function(y) {
      l0 <- ds[((grep(substr(y, 2, 2), ignore.case = TRUE, ds) + 6) %% 16) + 1]
      l1 <- paste0(substr(y, 1, 1), l0, substr(y, 3, 7))
      p0 <- ds[((grep(substr(l1, 4, 4), ignore.case = TRUE, ds) + 6) %% 16) + 1]
      p1 <- paste0(substr(l1, 1, 3), p0, substr(l1, 5, 7))
      # p0 <- ds[((grep(substr(ifelse(length(colors) > 1, y, l1), 4, 4), ignore.case = TRUE, ds) + 6) %% 16) + 1]
      # p1 <- paste0(substr(ifelse(length(colors) > 1, y,  l1), 1, 3),
      #              p0,
      #              substr(ifelse(length(colors) > 1, y, l1), 5, 7))
      c(l1, p1)
    }))
  }
  if (colorScale == "continuous") {
    if (is.null(colors)) {
      colors <- dsColorsHex()[c(1, 7, 3, 4)]
    }
    if (length(colors) == 1) {
      l0 <- ds[((grep(substr(colors, 2, 2), ignore.case = TRUE, ds) + 7) %% 16) + 1]
      colors <- c(colors, paste0(substr(colors, 1, 1), l0, substr(colors, 3, 7)))
    }
  }
  if (numeric) {
    fillCol <- data.frame(a = cat,
                          color = c(colors, colorNumeric(c(colors, ad), cat)(cat))[sample(1:length(cat))])
    names(fillCol)[1] <- col
  } else {
    fillCol <- data.frame(a = cat,
                          color = c(colors, colorFactor(c(colors, ad), cat)(cat))[sample(1:length(cat))])
    names(fillCol)[1] <- col
  }

  fillCol <- data %>%
    dplyr::left_join(fillCol)

  if (!is.null(highlightValue) & sum(highlightValue %in% fillCol[[1]]) > 0) {
    wh <- which(fillCol[[1]] %in% highlightValue)
    if (is.null(highlightValueColor)) {
      l0 <- ds[((grep(substr(colors[1], 2, 2), ignore.case = TRUE, ds) + 13) %% 16) + 1]
      highlightValueColor <- paste0(substr(colors[1], 1, 1), l0, substr(colors[1], 3, 7))
    }
    fillCol$color <- as.character(fillCol$color)
    fillCol$color[wh] <- highlightValueColor
  }
  fillCol
}

# ds palette
#' @export
dsColorsHex <- function(hex = FALSE) {
  if (hex) {
    c <- c(0:9, "A", "B", "C", "D", "E", "F")

  } else {
    c <- c("#2E0F35", "#74D1F7", "#B70F7F", "#C2C4C4", "#8097A4",  "#A6CEDE", "#801549",
           "#FECA84", "#ACD9C2", "#EEF1F2")
  }
  c
}


# binds convertions
#' @export
binsLeg <- function(data = NULL,
                    col = 'b',
                    scale = 'discrete',
                    mode = 'quantile',
                    bins = 5,
                    nDigits = NULL,
                    marks = c(',', '.'),
                    format = c(NULL, NULL),
                    colors = NULL,
                    dataLeft = NULL, ...){


  data <- data.frame(a = c('COL', 'COL', 'ARG', 'BRA', 'USA'), b = c(1, 2, 1, 1, 2))
  data <- data[order(data[[col]]),]
  data[[col]] <- round(data[[col]], 2)
  vector <- data[[col]]

  nDig <- 2
  if (!is.null(nDigits)) nDig <- nDigits

  sumBins <- 1/(10^nDig)

  if (scale == 'discrete') {
    if (scale == 'quantile') {
      z <- round(as.vector(quantile(unique(vector), probs = c(0, cumsum(rep((1/bins), bins))))), nDig)
    } else {
      z <- rev(round(seq(max(vector), min(vector), length.out = bins + 1  ), nDig))
    }

    lisBins<- map(1:length(z), function(x){
      d <- c(paste0(format[1], z[x], format[2]), paste0(format[1], z[x+1], format[2]))
      if (x >= 2) {
        d <- c(paste0(format[1], z[x] + sumBins, format[2]), paste0(format[1], z[x+1], format[2]))
      }
      paste(d, collapse = " - ")
    })

    lisBins <- lisBins[-(length(z))]
    descLeg <- lisBins %>% unlist()

    data$bins <- map_chr(1:nrow(data), function(y) {
      numBus <- data[[col]][y]
      ind <- min(which(numBus <= z[-1]))
      descLeg[ind]
    })

    aggDta <- data.frame(bins = setdiff(descLeg, data$bins))
    data <- bind_rows(data, aggDta)
    data_graph <- dplyr::left_join(data, dataLeft, by = "a")
    data_graph <- fillColors(data_graph, 'bins' , colors = colors, colorScale = 'discrete', highlightValue = NULL, highlightValueColor = NULL, numeric = F, labelWrap = 12)
    data_graph$bins <- as.factor(data_graph$bins)
    data_graph$bins <- factor(data_graph$bins, levels = descLeg)

  } else {
      z <- rev(round(seq(max(vector), min(vector), length.out = bins + 1)), nDig)
  }

}


# layerMap and dataMap
#' @export
layerMap <- function(mapName) {
  ggmap <- geodataMeta(mapName)
  ggmap$path <- file.path("geodata",ggmap$geoname,paste0(ggmap$basename,".topojson"))
  ggmap$centroides <- file.path("geodata",ggmap$geoname,paste0(ggmap$basename,".csv"))

  tj <- topojson_read(system.file(ggmap$path,package = "geodata"))

  data_map <- fortify(tj) %>% mutate(.id = as.numeric(id)) %>% select(-id)
  data_info <- tj@data %>% mutate(.id = 0:(nrow(.)-1)) #%>% select(-id)


  data_map <- left_join(data_map, data_info)
  graph <- ggplot() +
    geom_map(data = data_map, map = data_map,
             aes(map_id = id, x = long, y = lat, group = group), fill = fill$color,
             color = border$color, size = border$weigth, alpha= fill$opacity) +
    theme_map()
  result <- list(data_map, graph)
  result
}
