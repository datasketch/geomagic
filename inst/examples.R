library(devtools)
load_all()
document()
install()

library(geomagic)



# Choropleth --------------------------------------------------------------

# Examples GcdNum ---------------------------------------------------------

gg_choropleth_map_GcdNum.(data = NULL, mapName =  "american_countries", fill = list(
  showText = c(TRUE, FALSE),
  propText = 'all'))
gg_choropleth_map_GcdNum.(data = NULL,
                          fill = list(
                            showText = c(TRUE, FALSE),
                            propText = 'all'))
gg_choropleth_map_GcdNum.(data = NULL,
                          fill = list(
                            showText = c(TRUE, FALSE),
                            propText = 'all',
                            optText = 'name'))

dataGN <- sampleData('Gcd-Num')
gg_choropleth_map_GcdNum.(data = dataGN)
gg_choropleth_map_GcdNum.(data = dataGN, nDigits = 0)
gg_choropleth_map_GcdNum.(data = dataGN,
                          title = 'Este es un título',
                          subtitle = 'este es el sutitulo',
                          captio = 'créditos',
                          agg = 'mean', format = c('$', ''))


gg_choropleth_map_GcdNum.(data = dataGN,
                          fill = list(scale = 'discrete',
                                      mode = 'no',
                                      color = c('darkred', 'black'),
                                      optText = 'name',
                                      showText = c(TRUE, FALSE)),
                          legend = list(bins = 4,
                                        title = ''),
                          percentage = TRUE)


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

data <- data.frame(pais = c('COL',  'ARG', 'BRA', 'RUS', 'MEX'),
                   total = c(1, 1, 1, 1, 1))
gg_choropleth_map_GcdNum.(data = data,
                          legend = list(bins = 3),
                          marks = c('','.'),
                          projections = list(type = "ortho",
                                             orientation = c(41, -74, 0)))

gg_choropleth_map_GcdNum.(data = data,
                          legend = list(bins = 5),
                          marks = c('','.'),
                          fill = list(color = c('orange')),
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
gg_choropleth_map_GcdNum.(data = data,
                          mapName = "latam_countries",
                          projections = list(type = "azequalarea",
                                             orientation = c(36.92, 297.6, 0)))

gg_choropleth_map_GcdNum.(data = NULL,
                          mapName = "mex_states",
                          fill = list(
                            opacity = 0.4,
                            showText = TRUE, sizeText = 2))


data <- data.frame(a = c('COL', 'COL', 'ARG', 'BRA', 'BRA', 'BRA', 'BRA', 'USA', 'MEX'))
gg_choropleth_Gcd.(data = NULL)
gg_choropleth_Gcd.(data = data)
gg_choropleth_Gcd.(data = data,
                   fill = list(scale = 'discrete',
                               showText = c(TRUE, FALSE),
                               sizeText = 3,
                               optText = 'name',
                               mode = 'no',
                               background = '#000000',
                               opacity = 0.7,
                               color = c('red', 'yellow', 'green')
                   ),
                   format = c('$', 'ana'),
                   marks = c('.', ','),
                   nDigits = 4,
                   legend = list(bins = 3,
                                 fill = '#ffffff',
                                 background = '#ffffff',
                                 position = c(0, 0.2),
                                 title = 'legend'),
                   percentage = TRUE,
                   projections = list(ratio = 1,
                                      type = "mercator") )


# Examples Gcd ------------------------------------------------------------

gg_choropleth_Gcd.(data = data,
                   count = F)

gg_choropleth_Gcd.(data = data,
                   count = F,
                   fill = list(scale = 'discrete',
                               optText = 'name',
                               sizeText = 3,
                               showText = c(TRUE, FALSE)))




# Examples GcdCat ---------------------------------------------------------

data <- sampleData('Gcd-Cat', 100)
gg_choropleth_map_GcdCat.(data = NULL,
                          title = 'Mapa sin datos',
                          subtitle = 'porque es una prueba',
                          caption = 'datasketch')
gg_choropleth_map_GcdCat.(data = NULL,
                          fill = list(showText = c(TRUE, FALSE)))
gg_choropleth_map_GcdCat.(data = data,
                          fill = list(showText = c(TRUE, FALSE),
                                      optText = 'name'))
gg_choropleth_map_GcdCat.(data = data,
                          fill = list(scale = 'discrete'),
                          projections = list(type = "ortho"))
gg_choropleth_map_GcdCat.(data = data,
                          count = TRUE,
                          fill = list(scale = 'discrete',
                                      showText = c(TRUE, TRUE)))


# Examples Gnm ------------------------------------------------------------

data <- sampleData('Gnm', 100)
gg_choropleth_Gnm.(data = NULL, title = 'Mapa sin datos')
gg_choropleth_Gnm.(data = data)
gg_choropleth_Gnm.(data = data, title = 'Mapa con datos',
                   subtitle = 'porq es una prueba', caption = 'datasketch',
                   mapName = 'africa_countries',
                   border = list(weigth = 0.5,
                                 color = '#FFAA12'),
                   fill = list(color = c('#FCCADF', '#AA12FC'),
                               naColor = '#FDDFCC',
                               opacity = 0.9,
                               scale = 'discrete',
                               nullColor = '#C12AAF',
                               background = '#000000'),
                   titleStyle = list(color = 'green'))



# Examples GnmNum ---------------------------------------------------------


dataGn <- sampleData('Gnm-Num', 100)
gg_choropleth_map_GnmNum.(data = dataGn)
gg_choropleth_map_GnmNum.(data = dataGn, mapName = 'latam_countries' )
gg_choropleth_map_GnmNum.(data = dataGn,
                          mapName = 'latam_countries',
                          projections = list(type = "mercator",
                                             ratio = 1))



# Examples GnmCat ---------------------------------------------------------

dataGc <- sampleData('Gnm-Cat')
gg_choropleth_map_GnmCat.(data = NULL)
gg_choropleth_map_GnmCat.(data = NULL,
                          fill = list(
                            background = '#FDAA11'
                          ))
gg_choropleth_map_GnmCat.(data = dataGc,
                          fill = list(
                            background = '#FDAA11'
                          ))







# Bubbles -----------------------------------------------------------------

# Examples GcdNum ---------------------------------------------------------
gg_bubbles_map_GcdNum.(data = NULL)
dataN <- sampleData('Gcd-Num')
gg_bubbles_map_GcdNum.(data = dataN)


# Examples Gcd ------------------------------------------------------------



# Examples GcdCatNum ------------------------------------------------------
gg_bubbles_map_GcdCatNum.(data = NULL)
dataCN <- sampleData('Gcd-Cat-Num')
gg_bubbles_map_GcdCatNum.(data = dataCN)



# Examples GcdCat ---------------------------------------------------------



# Examples GlnGltNum ------------------------------------------------------
gg_bubbles_map_GlnGltNum.(data = NULL)
dataN <- sampleData('Gln-Glt-Num')
gg_bubbles_map_GlnGltNum.(data = dataN)

# Examples GlnGlt ---------------------------------------------------------
gg_bubbles_map_GlnGlt.(data = NULL)
dataN <- sampleData('Gln-Glt')
gg_bubbles_map_GlnGlt.(data = dataN)



# Examples GlnGltCatNum ---------------------------------------------------
gg_bubbles_map_GlnGltCatNum.(data = NULL)
dataCN <- sampleData('Gln-Glt-Cat-Num')
gg_bubbles_map_GlnGltCatNum.(data = dataCN)


# Examples GlnGltCat ------------------------------------------------------
gg_bubbles_map_GlnGltCat.(data = NULL)
dataC <- sampleData('Gln-Glt-Cat')
gg_bubbles_map_GlnGltCat.(data = dataC)


# Examples GnmNum ---------------------------------------------------------
gg_bubbles_map_GnmNum.(data = NULL)
dataN <- sampleData('Gnm-Num')
gg_bubbles_map_GnmNum.(data = dataN)


# Examples GnmCatNum. -----------------------------------------------------
gg_bubbles_map_GnmCatNum.(data = NULL,
                          fill = list( showText = c(TRUE, FALSE),
                                       propText = 'all'))
dataCN <- sampleData('Gnm-Cat-Num')
gg_bubbles_map_GnmCatNum.(data = dataCN)

