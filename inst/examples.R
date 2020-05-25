
# geom prep example -------------------------------------------------------

geomagic_prep()
gg_choropleth_GnmNum()
gg_choropleth_GnmNum(map_name = "usa_states")
gg_choropleth_GnmNum(map_name = "usa_states", map_add_alaska = TRUE)

df <- sample_data("Gnm-Num")
gg_choropleth_GnmNum(data = df)
