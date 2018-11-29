library(devtools)
load_all()
document()
install()

library(geomagic)


# Examples Choropleth GcdNum------------------------------------------------

gg_choropleth_map_GcdNum.(data = NULL)


data <- data.frame(pais = c('COL', 'COL', 'ARG', 'BRA', 'USA', 'MEX'),
                   total = c(1, 2, 1, 1, 2, NA))
gg_choropleth_map_GcdNum.(data = data,
                          legend = list(bins = 3),
                          marks = c('','.'),
                          fill = list(showText = c(T, T),
                                      optText = 'name',
                                      propText = 'onlyData',  #all, onlyData,and a number
                                      sizeText = 3))

gg_choropleth_map_GcdNum.(data = data,
                          legend = list(bins = 5, limit = 0.1),
                          marks = c('','.'),
                          fill = list(background = "darkblue"))

gg_choropleth_map_GcdNum.(data = data,
                          legend = list(bins = 5,
                                        title = 'blabla'),
                          marks = c('','.'),
                          title = 'título del mapa',
                          subtitle = 'subtitulo del mapa',
                          caption = 'créditos del mapa',
                          projections = list(type = "ortho"))

data <- data.frame(pais = c('COL',  'ARG', 'BRA', 'USA', 'MEX'),
                   total = c(1, 1, 1, 1, 1))
gg_choropleth_map_GcdNum.(data = data,
                          legend = list(bins = 3),
                          marks = c('','.'),
                          projections = list(type = "ortho",
                                             orientation = c(41, -74, 0)))

gg_choropleth_map_GcdNum.(data = data,
                          legend = list(bins = 5),
                          marks = c('','.'),
                          fill = list(color = c('orange', 'black')),
                          percentage = TRUE)

gg_choropleth_map_GcdNum.(data = data,
                          fill = list(scale = 'discrete'),
                          legend = list(bins = 3),
                          nDigits = 2)

gg_choropleth_map_GcdNum.(data = data,
                          fill = list(scale = 'discrete',
                                      color = c('#FD2AAA', '#00FD12')),
                          legend = list(bins = 3,
                                        title = 'legend',
                                        position =  c(0, 0.2)),
                          nDigits = 2,
                          percentage = TRUE)

gg_choropleth_map_GcdNum.(data = data,
                          border = list(color = 'blue'),
                          fill = list(color = c('#FD3AAA'),
                                      opacity = 0.4,
                                      scale = 'discrete'),
                          legend = list(bins = 3), nDigits = 4)

gg_choropleth_map_GcdNum.(data = NULL, mapName = "col_departments")
gg_choropleth_map_GcdNum.(data = NULL,
                          mapName = "col_departments",
                          projections = list(ratio = 1,
                                             type = "mercator"))


gg_choropleth_map_GcdNum.(data = NULL,
                          mapName = "latam_countries",
                          projections = list(type = "mercator",
                                             ratio = 1))
gg_choropleth_map_GcdNum.(data = NULL,
                          mapName = "latam_countries",
                          projections = list(type = "azequalarea",
                                             orientation = c(36.92, 297.6, 0)))

gg_choropleth_map_GcdNum.(data = NULL,
                          mapName = "mex_states",
                          fill = list(
                            opacity = 0.4,
                            showText = TRUE, sizeText = 2))


data <- data.frame(a = c('COL', 'COL', 'ARG', 'BRA', 'BRA', 'BRA', 'BRA', 'USA', 'MEX'))
gg_choropleth_Gcd.(data = data)
gg_choropleth_Gcd.(data = data,
                   fill = list(scale = 'discrete'),
                   legend = list(bins = 3))

gg_choropleth_Gcd.(data = data,
                   count = F)

gg_choropleth_Gcd.(data = data,
                   count = F,
                   fill = list(scale = 'discrete'))



dataGdcCat <- sampleData('Gcd-Cat', 100)
gg_choropleth_map_GcdCat.(data = dataGdcCat)
gg_choropleth_map_GcdCat.(data = dataGdcCat,
                          fill = list(scale = 'discrete',
                                      showText = c(TRUE, TRUE)))



# opts <- list(titleLabel = "HOLA PERRA",
#              subtitle = "",
#              caption = "",
#              reverse = FALSE,
#              fillLabel = NULL,
#              text = TRUE,
#              text_size = 1.8,
#              prop_text = 0.5,
#              leg_pos = "right",
#              titleLeg = '',
#              color_map = "gray",
#              color_frontier = "white",
#              highC = "#a0a336",
#              lowC = "#e1e2e0",
#              Bcolor = "transparent")
#
# df <- data.frame(loc = c('9', '7','2', '3', '4', '5', '1', '11', '1', '10', '12', '13', '14', '15', '16'), num =runif(15))
#
# gg_choropleth_bogota_GcdNum.(data = df, opts = opts)
#
# opts <- list(titleLabel = "",
#              subtitle = "",
#              caption = "",
#              reverse = FALSE,
#              fillLabel = NULL,
#              text = TRUE,
#              text_size = 0.5,
#              prop_text = 'all',
#              leg_pos = "right",
#              titleLeg = '',
#              scale_point = 1.5,
#              color_map = "gray",
#              color_point = 'red',
#              alpha = 0.1,
#              color_frontier = "white",
#              Bcolor = 'red')
#
#
#
# long <- runif(100, -76.937433, -69.635037)
# lat <- runif(100, 2.342537, 6.276997)
# fg <- data.frame(long = long, lat = lat) #num = round(runif(length(lat), 1, 5), 0))
# gg_bubble_GcdLonLat.(data = fg, mapName = 'col_departments', opts = opts)
#
# deptos <- c("05", "08", "11", "13", "15", "17", "18", "19", "20", "23", "25", "27", "41", "44", "47", "50")
# data <- data.frame(id = deptos, num = sample(LETTERS[1:5],16, replace = TRUE))
# gg_choropleth_map_GcdCat.(data = data, mapName = "col_departments", text = FALSE)
#
# gg_choropleth_map_GcdCat.(data = data, mapName = "col_departments",
#                           opts = list(text = FALSE, palette = "amalia_light")
# )
#
#
# gg_choropleth_map_GcdCat.(data = data, mapName = "col_departments",
#                           opts = list(text = FALSE, palette = "viridis")
#                           )
#
#
#
# ###
# gg_choropleth_map_GcdNum.(data = NULL, mapName = "world_countries", opts = opts)
#
#
# deptos <- c("05", "08", "11", "13", "15", "17", "18", "19", "20", "23", "25", "27", "41", "44", "47", "50")
# dataGcdNum <- data.frame(id = deptos, num = runif(length(deptos), 0, 1))
# gg_choropleth_map_GcdNum.(data = dataGcdNum, mapName = "col_departments", text = FALSE)
#
#
#
# # Examples bubbles GcdNum--------------------------------------------------
#
# df_points <-data.frame(ciu = c('BHS', 'VGB', 'BRA', 'BOL', 'COL', 'CHL'), runif(6))
# gg_bubbles_map_GcdNum.(df_points, mapName = "latam_countries")
#
#
# # Examples choropleth with Cat --------------------------------------------
# df_points <-data.frame(ciu = c('USA', 'VGB', 'BRA', 'BOL', 'COL', 'CHL', 'CHL'), color = c('RED', 'RED', 'BLUE', 'GRAY', 'GRAY', 'BLUE', 'GREEN'))
# gg_choropleth_map_GcdCat.(data = df_points, mapName = "world_countries")



# deptos <- c("05", "08", "11", "13", "15", "17", "18", "19", "20", "23", "25", "27", "41", "44", "47", "50")
# dataGcdNum <- data.frame(id = deptos, num = runif(length(deptos), 0, 1))
#
# gg_choropleth_co_GcdNum.(dataGcdNum, color_map = "gold", text_size = 2.5,
#                          text = TRUE, prop_text = "all")
#
# gg_choropleth_co_GcdNum.(dataGcdNum, color_map = "gold", text_size = 2.5,
#                          text = TRUE, prop_text = 0.5)
#
# gg_choropleth_co_GcdNum.(dataGcdNum, color_map = "gold", text_size = 2.5,
#                          text = TRUE, prop_text = c("Valle del Cauca", "Nariño", "Amazonas"))
#
# depto_ <- c("99")
# gg_sketchmap_depto_GcdNum.(depto_)
#
# depto_ <- c("05")
# mpios <- c("05002", "05004", "05021", "05030", "05031", "05034", "05036", "05038", "05040", "05044", "05045", "05051", "05055", "05059",
#            "05079", "05088", "05086", "05091", "05093", "05101", "05107", "05113", "05125", "05129", "05134", "05142", "05145", "05147",
#            "05148", "05150", "05154", "05138", "05172", "05190", "05197", "05206", "05209", "05212", "05120", "05234", "05237", "05240",
#            ",05250", "05264", "05266", "05282", "05284", "05306", "05308", "05313", "05315")
# dataGcdNum2 <- data.frame(id = mpios, num = runif(length(mpios), 0, 1))
#
# gg_choropleth_depto_GcdNum.(dataGcdNum2, depto_ = depto_, color_map = "gold",
#                             text = TRUE, text_size = 2.5)
#
# gg_choropleth_depto_GcdNum.(dataGcdNum2, depto_ = depto_, color_map = "gold",
#                             text = TRUE, text_size = 2.5, prop_text = "all")
#
# gg_choropleth_depto_GcdNum.(dataGcdNum2, depto_ = depto_, color_map = "gold",
#                             text = TRUE, text_size = 2.5, prop_text = c("Mutatá"))
#
# paises <- c("ARG", "CUB", "COL", "CHL")
# dataGcdNum1.1 <- data.frame(id = paises, num = runif(length(paises), 0, 1))
#
# gg_choropleth_latam_GcdNum.(dataGcdNum1.1, color_frontier = 'black', prop_text = 'all')
#
# lat_max <- 6.276997
# long_max <- -69.635037
# lat_min <- 2.342537
# long_min <- -76.937433
#
# long <- runif(100, long_min, long_max)
# lat <- runif(100, lat_min, lat_max)
#
# dataGcdNum3 <- data.frame(long = long, lat = lat) #num = round(runif(length(lat), 1, 5), 0))
#
# gg_bubble_co_Gcd.(dataGcdNum3, color_map = "grey", scale_point = 2,
#                   text =TRUE, text_size = 3)
#
# gg_bubble_co_Gcd.(dataGcdNum3, color_map = "grey", scale_point = 2,
#                   text =TRUE, text_size = 3, prop_text = "all")
#
# gg_bubble_co_Gcd.(dataGcdNum3, color_map = "grey", scale_point = 2,
#                   text =TRUE, text_size = 3, prop_text = "La Guajira")
#
# long <- runif(10, long_min, long_max)
# lat <- runif(10, lat_min, lat_max)
#
# #gg_bubble_latam_Gcd.(dataGcdNum3)
#
# dataGcdNum3.1 <- data.frame(long = long, lat = lat, number = ceiling(runif(length(long), 0, 4))) #num = round(runif(length(lat), 1, 5), 0))
#
# DFD <- data.frame(ciu = c('MEX', 'URY', 'BRA', 'BOL', 'COL', 'CHL'), runif(6))
#
# gg_bubble_latam_GcdNum.(DFD)
#
# gg_bubble_co_GcdNum.(dataGcdNum3.1, color_map = "forestgreen", text = TRUE)
#
# lat_max <- 3.644543
# long_max <- -76.247046
# lat_min <- 3.363461
# long_min <- -76.804436
#
# depto_ <- "76"
# long <- runif(25, long_min, long_max)
# lat <- runif(25, lat_min, lat_max)
#
# dataGcdNum4 <- data.frame(long = long, lat = lat) #num = round(runif(length(lat), 1, 5), 0))
#
# gg_bubble_depto_Gcd.(dataGcdNum4, depto_ = depto_, color_map = "lightgreen", text = TRUE)
#
# long <- runif(10, long_min, long_max)
# lat <- runif(10, lat_min, lat_max)
#
# dataGcdNum4.1 <- data.frame(long = long, lat = lat, number = ceiling(runif(length(long), 0, 3))) #num = round(runif(length(lat), 1, 5), 0))
#
# gg_bubble_depto_GcdNum.(dataGcdNum4.1, depto = depto_, color_map = "lightblue", text = TRUE)
#
#
#
# df_points <-data.frame(ciu = c('BHS', 'VGB', 'BRA', 'BOL', 'COL', 'CHL'), runif(6))
#
# gg_bubble_latam_GcdNum.(df_points)
# #gg_bubble_latam_Gcd.(df_points, scale_point = 5)
#
# type <- c("Desminado militar en operaciones", "Desminado militar en operaciones", "Sospecha de campo minado")
# lat <- c(2.115158, 10.477040, 6.139590)
# long <- c(-74.77349,-73.25066 ,-75.02309)
# df_CaGcd <- data.frame(type = type, long = long, lat = lat)
#
# gg_bubble_co_CatGcd.(df_CaGcd)
#
# type <- c("Sospecha de campo minado", "Sospecha de campo minado", "Accidente por MAP", "Accidente por MAP")
# long <- c(-75.08235, -75.22135, -75.47778, -75.18678)
# lat <- c( 5.951820, 5.611950, 7.489444, 5.838575)
#
# df_CaGcd <- data.frame(type = type, long = long, lat = lat)
# gg_bubble_depto_CaGcd.(df_CaGcd, scale_point = 50)
#
#
# data <- data.frame(id = c('AFG', 'COL', 'SOM'), runif(3))
# gg_choropleth_world_GcdNum.(data, text = TRUE, prop_text = c('China'))
# gg_choropleth_world_GcdNum.(data, text = TRUE)
#
# data <- data.frame(loc = c('9', '7', '1', '20'), num =runif(4))
# gg_choropleth_Locations_GcdNum.(data)
#
#
# data <- data.frame(loc = c('9', '7', '1'), num =runif(3))
# gg_choropleth_Locations_withoutSumapaz_GcdNum.(data)
#
#
# data <- data.frame(loc = c('9', '7', '15', '2'), num =runif(4))
# gg_bubble_locationsBta_Gcd.(data, text = TRUE)
#
#
# gg_bubble_Locations_withoutSumapaz_Gcd.(data,scale_point = 8)
#
# data <- data.frame(id = c('AFG', 'COL', 'SOM'), runif(3))
# gg_bubbles_world_GcdNum.(data)
