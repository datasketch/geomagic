library(devtools)
load_all()
document()
install()

library(gggeomagic)


deptos <- c("05", "08", "11", "13", "15", "17", "18", "19", "20", "23", "25", "27", "41", "44", "47", "50")
dataGnmNum <- data.frame(id = deptos, num = runif(length(deptos), 0, 1))

gg_choropleth_co_GnmNum.(dataGnmNum, color_map = "gold", text_size = 2.5,
                         text = TRUE, prop_text = "all")

gg_choropleth_co_GnmNum.(dataGnmNum, color_map = "gold", text_size = 2.5,
                         text = TRUE, prop_text = 0.5)

gg_choropleth_co_GnmNum.(dataGnmNum, color_map = "gold", text_size = 2.5,
                         text = TRUE, prop_text = c("Valle del Cauca", "Nariño", "Amazonas"))

depto_ <- c("99")
gg_sketchmap_depto_GnmNum.(depto_)

depto_ <- c("05")
mpios <- c("05002", "05004", "05021", "05030", "05031", "05034", "05036", "05038", "05040", "05044", "05045", "05051", "05055", "05059",
           "05079", "05088", "05086", "05091", "05093", "05101", "05107", "05113", "05125", "05129", "05134", "05142", "05145", "05147",
           "05148", "05150", "05154", "05138", "05172", "05190", "05197", "05206", "05209", "05212", "05120", "05234", "05237", "05240",
           ",05250", "05264", "05266", "05282", "05284", "05306", "05308", "05313", "05315")
dataGnmNum2 <- data.frame(id = mpios, num = runif(length(mpios), 0, 1))

gg_choropleth_depto_GnmNum.(dataGnmNum2, depto_ = depto_, color_map = "gold",
                            text = TRUE, text_size = 2.5)

gg_choropleth_depto_GnmNum.(dataGnmNum2, depto_ = depto_, color_map = "gold",
                            text = TRUE, text_size = 2.5, prop_text = "all")

gg_choropleth_depto_GnmNum.(dataGnmNum2, depto_ = depto_, color_map = "gold",
                            text = TRUE, text_size = 2.5, prop_text = c("Mutatá"))

paises <- c("ARG", "CUB", "COL", "CHL")
dataGnmNum1.1 <- data.frame(id = paises, num = runif(length(paises), 0, 1))

gg_choropleth_latam_GnmNum.(dataGnmNum1.1, color_map = "green", reverse = TRUE)

lat_max <- 6.276997
long_max <- -69.635037
lat_min <- 2.342537
long_min <- -76.937433

long <- runif(100, long_min, long_max)
lat <- runif(100, lat_min, lat_max)

dataGnmNum3 <- data.frame(long = long, lat = lat) #num = round(runif(length(lat), 1, 5), 0))

gg_bubble_co_Gnm.(dataGnmNum3, color_map = "grey", scale_point = 2,
                  text =TRUE, text_size = 3)

gg_bubble_co_Gnm.(dataGnmNum3, color_map = "grey", scale_point = 2,
                  text =TRUE, text_size = 3, prop_text = "all")

gg_bubble_co_Gnm.(dataGnmNum3, color_map = "grey", scale_point = 2,
                  text =TRUE, text_size = 3, prop_text = "La Guajira")

long <- runif(10, long_min, long_max)
lat <- runif(10, lat_min, lat_max)

gg_bubble_latam_Gnm.(dataGnmNum3)

dataGnmNum3.1 <- data.frame(long = long, lat = lat, number = ceiling(runif(length(long), 0, 4))) #num = round(runif(length(lat), 1, 5), 0))

gg_bubble_latam_GnmNum.(dataGnmNum3.1)

gg_bubble_co_GnmNum.(dataGnmNum3.1, color_map = "forestgreen", text = TRUE)

lat_max <- 3.644543
long_max <- -76.247046
lat_min <- 3.363461
long_min <- -76.804436

depto_ <- "76"
long <- runif(25, long_min, long_max)
lat <- runif(25, lat_min, lat_max)

dataGnmNum4 <- data.frame(long = long, lat = lat) #num = round(runif(length(lat), 1, 5), 0))

gg_bubble_depto_Gnm.(dataGnmNum4, depto_ = depto_, color_map = "lightgreen", text = TRUE)

long <- runif(10, long_min, long_max)
lat <- runif(10, lat_min, lat_max)

dataGnmNum4.1 <- data.frame(long = long, lat = lat, number = ceiling(runif(length(long), 0, 3))) #num = round(runif(length(lat), 1, 5), 0))

gg_bubble_depto_GnmNum.(dataGnmNum4.1, depto = depto_, color_map = "lightblue", text = TRUE)



points_long <- c(-74.77349, -73.25066, -75.02309, -72.74667, -72.96547, -72.65211, -72.69544, -76.10000, -75.97304, -74.93877)
points_lat <- c(2.115158, 10.477040,6.139590,5.438429,5.166737,5.373123,5.356951,7.887500,2.664088,6.336920)

df_points <- data.frame(long = points_long, lat = points_lat)

gg_bubble_co_Gnm.(df_points, color_map = "forestgreen", text = TRUE)
gg_bubble_latam_Gnm.(df_points, scale_point = 5)

type <- c("Desminado militar en operaciones", "Desminado militar en operaciones", "Sospecha de campo minado")
lat <- c(2.115158, 10.477040, 6.139590)
long <- c(-74.77349,-73.25066 ,-75.02309)
df_CaGnm <- data.frame(type = type, long = long, lat = lat)

gg_bubble_co_CatGnm.(df_CaGnm)

type <- c("Sospecha de campo minado", "Sospecha de campo minado", "Accidente por MAP", "Accidente por MAP")
long <- c(-75.08235, -75.22135, -75.47778, -75.18678)
lat <- c( 5.951820, 5.611950, 7.489444, 5.838575)

df_CaGnm <- data.frame(type = type, long = long, lat = lat)
gg_bubble_depto_CaGnm.(df_CaGnm, scale_point = 50)


data <- data.frame(id = c('AFG', 'COL', 'SOM'), runif(3))
gg_choropleth_world_GnmNum.(data, text = TRUE, prop_text = c('China'))
gg_choropleth_world_GnmNum.(data, text = TRUE)

data <- data.frame(loc = c('9', '7', '1', '20'), num =runif(4))
gg_choropleth_Locations_GnmNum.(data)


data <- data.frame(loc = c('9', '7', '1'), num =runif(3))
gg_choropleth_Locations_withoutSumapaz_GnmNum.(data)


data <- data.frame(loc = c('9', '7', '15', '2'), num =runif(4))
gg_bubble_locationsBta_Gnm.(data, text = TRUE)


gg_bubble_Locations_withoutSumapaz_Gnm.(data,scale_point = 8)

data <- data.frame(id = c('AFG', 'COL', 'SOM'), runif(3))
gg_bubbles_world_GnmNum.(data)
