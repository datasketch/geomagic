
# geom prep example -------------------------------------------------------

geomagic_prep()
gg_choropleth_GnmNum()
gg_choropleth_GnmNum(map_name = "usa_states")
gg_choropleth_GnmNum(map_name = "usa_states",
                     map_projection = "ortho",
                     map_graticule = TRUE, grid_color = "red")
gg_choropleth_GnmNum(map_name = "usa_states", map_add_alaska = TRUE)

df <- sample_data("Gnm-Num")
gg_choropleth_GnmNum(data = df,
                     title = "Mapa de ejemplo",
                     branding_include = TRUE)
