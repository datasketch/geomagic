
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
