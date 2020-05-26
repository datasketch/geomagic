
# geom prep example -------------------------------------------------------

geomagic_prep()
gg_choropleth_GnmNum()
gg_choropleth_GnmNum(map_name = "usa_states")
gg_choropleth_GnmNum(map_name = "usa_states",
                     map_projection = "ortho",
                     map_graticule = TRUE, grid_color = "red")
gg_choropleth_GnmNum(map_name = "usa_states", map_add_alaska = TRUE)

df <- sample_data("Gnm-Num")
names(df) <- c("pais", "valor")
gg_choropleth_GnmNum(data = df,
                     title = "Mapa de ejemplo",
                     branding_include = TRUE,  tooltip = "{pais}")

# Gnm-Num examples --------------------------------------------------------

gg_choropleth_GnmNum()
gg_choropleth_GnmNum( map_name = "col_departments",
                      map_graticule = TRUE,
                      grid_color = "#000000")
gg_choropleth_GnmNum(map_name = "col_departments",
                     background_color = "#FEAFEA")

gg_choropleth_GnmNum(map_name = "col_departments",
                     map_projection = "cylindrical",
                     na_color = "#000000")

data <- sample_data("Gnm-Num", 100)
gg_choropleth_GnmNum(data)
gg_choropleth_GnmNum(data, palette_colors = c("#FEAFEA", "#000AAA"))
gg_choropleth_GnmNum(data, map_color_scale = "Quantile")
gg_choropleth_GnmNum(data, map_color_scale = "Bins")
gg_choropleth_GnmNum(data, map_color_scale = "Bins", map_bins = 3)


gg_choropleth_GnmNum(data = sample_data("Gnm-Num", 100),
                     legend_title = "Titulo de legenda")
gg_choropleth_GnmNum(data = sample_data("Gnm-Num", 100),
                     map_graticule = TRUE)

data <- data.frame(Ciudad = c("Cauca", "chocó", "nariño", "nariño"), Val = runif(4, 1, 1000))
gg_choropleth_GnmNum(data, map_name = "col_pacifico",
                     palette_colors = c("#FEAFEA", "#000CCC"))
gg_choropleth_GnmNum(data, map_name = "col_pacifico",
                     tooltip = "{Ciudad}: {Val}",
                     format_cat_sample = "Titulo")
gg_choropleth_GnmNum(data, map_name = "col_pacifico",
                     title = "Pacífico Colombiano",
                     map_color_scale = "Bins", prefix = "$")


# Gcd Num examples  -------------------------------------------------------

gg_choropleth_GcdNum()
map_changes <- c("col_municipalities", "venezuela_states")
availableMaps <- setdiff(availableGeodata(), map_changes)
#map(availableMaps, function(n) {gg_choropleth_GcdNum(map_name = n)})

gg_choropleth_GcdNum(map_name = "bra_states" )
data <- data.frame(State = c("BR.PA", "BR.RS", "BR.RS", "BR.TO", "BR.MT", "BR.MA", "BR.ES"),
                   `Fake population` = runif(7, 20000, 600000))
gg_choropleth_GcdNum(data, map_name = "bra_states")
gg_choropleth_GcdNum(data, map_name = "bra_states",
                     map_color_scale = "Bins",
                     title = "Brazil map",
                     subtitle = "Fake map of Brazil population",
                     caption = "Random data")

gg_choropleth_Gcd(sample_data("Gcd", 300))
gg_choropleth_Gcd(sample_data("Gcd", 3000),
                  topo_fill_opacity = 1,
                  palette_colors = c("#FEAFEA", "#000CCC"))
