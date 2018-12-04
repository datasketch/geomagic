library(devtools)
load_all()
document()
install()

library(geomagic)


# Examples Choropleth GcdNum------------------------------------------------

gg_choropleth_map_GcdNum.(data = NULL)
gg_choropleth_map_GcdNum.(data = NULL,
                          fill = list(
                            showText = c(TRUE, FALSE),
                            propText = 'all')
                          )

data <- data.frame(pais = c('COL', 'COL', 'ARG', 'BRA', 'USA', 'MEX'),
                   total = c(1, 2, 1, 1, 2, NA))

gg_choropleth_map_GcdNum.(data = data,
                          fill = list(scale = 'discrete',
                                      mode = 'no',
                                      optText = 'name',
                                      showText = c(TRUE, FALSE)),
                          legend = list(bins = 4),
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

gg_choropleth_Gcd.(data = data,
                   count = F)

gg_choropleth_Gcd.(data = data,
                   count = F,
                   fill = list(scale = 'discrete',
                               optText = 'name', #mirar porq no funciona
                               sizeText = 3,
                               showText = c(TRUE, FALSE)))



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






# By name -----------------------------------------------------------------

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

dataGn <- sampleData('Gnm-Num', 100)
gg_choropleth_map_GnmNum.(data = dataGn)
gg_choropleth_map_GnmNum.(data = dataGn, mapName = 'latam_countries' )
gg_choropleth_map_GnmNum.(data = dataGn,
                          mapName = 'latam_countries',
                          projections = list(type = "mercator",
                                             ratio = 1))

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





# Bubbles map Gcd Num -----------------------------------------------------

gg_bubbles_map_GcdNum.(data = NULL)
gg_bubbles_map_GcdNum.(data = NULL,
                       title = 'Esto es un título',
                       subtitle = 'esto es un subtitulo',
                       caption = 'estos son los créditos',
                       mapName = 'arg_provinces',
                       fill = list(showText=c(TRUE, FALSE),
                                   propText = 'all',
                                   optText = 'name',
                                   sizeText = 3),
                       projections = list(ratio = 1,
                                          type = "mercator"))


dataC <- sampleData('Gcd-Num', 1000)
gg_bubbles_map_GcdNum.(data = dataC)
gg_bubbles_map_GcdNum.(data = dataC,
                       mapName = 'latam_countries',
                       legend = list(bins = 3,
                                     title = 'hola'),
                       fill = list(showText=c(TRUE, FALSE),
                                   propText = 'all',
                                   optText = 'name',
                                   sizeText = 3,
                                   color = '#FAC12A'),
                       projections = list(ratio = 1,
                                          type = "mercator")
                      )


gg_bubbles_map_GcdCatNum.(data = NULL)
gg_bubbles_map_GcdCatNum.(data = NULL,
                          title = 'Esto es un título',
                          subtitle = 'Esto es un subtitulo',
                          caption = 'esto son los créditos',
                          border = list(weigth = 0.5,
                                        color = '#FAACCA'),
                          fill = list(showText = c(T, T),
                                      propText = 'all',
                                      #optText = 'name',
                                      sizeText = 3,
                                      color = '#FAC12A',
                                      background = '#FDCAA0'))

dataCN <- sampleData('Gcd-Cat-Num', 10000)
gg_bubbles_map_GcdCatNum.(data = dataCN)
gg_bubbles_map_GcdCatNum.(data = dataCN,
                          legend = list(title = c('titulo 1', 'titulo 2')))
gg_bubbles_map_GcdCatNum.(data = dataCN,
                          legend = list(showLeg = c(TRUE, FALSE)))


gg_bubbles_map_GcdCatNum.(data = dataCN,
                          legend = list(showLeg = c(FALSE, FALSE)))

gg_bubbles_map_GcdCatNum.(data = dataCN,
                          percentage = TRUE,
                          legend = list(
                            bins = 7,
                            fill = '#FDCCAA',
                            background  = '#000000'))

gg_bubbles_map_GcdCatNum.(data = dataCN,
                          marks = c(',', '.'),
                          format = c('$', 'ja'),
                          legend = list(
                            bins = 7,
                            fill = '#FDCCAA',
                            background  = '#000000'),
                          fill = list(showText = c(TRUE, TRUE)))



gg_bubbles_map_GlnGltNum.(data = NULL)

gg_bubbles_map_GlnGltNum.(data = NULL,
                          title = 'Esto es un título',
                          subtitle = 'Esto es un subtitulo',
                          caption = 'esto son los créditos',
                          border = list(weigth = 0.5,
                                        color = '#000000'),
                          fill = list(showText = c(T, T),
                                      propText = 'all',
                                      #optText = 'name',
                                      sizeText = 3,
                                      color = '#FAC12A',
                                      background = '#FAC0C0'))

dataN <- sampleData('Gln-Glt-Num')
gg_bubbles_map_GlnGltNum.(data = dataN)
gg_bubbles_map_GlnGltNum.(data = dataN,
                          minSize = 3,
                          maxSize = 7,
                          legend = list(bins = 3,
                                        title = 'titulo'))

gg_bubbles_map_GlnGltNum.(data = dataN,
                          minSize = 3,
                          maxSize = 7,
                          marks = c(',', 'x'),
                          format = c('$', ''),
                          fill = list(showText = c(F, T),
                                      sizeText = 3))


gg_bubbles_map_GlnGlt.(data = NULL)

gg_bubbles_map_GlnGlt.(data = NULL,
                          title = 'Esto es un título',
                          subtitle = 'Esto es un subtitulo',
                          caption = 'esto son los créditos',
                          border = list(weigth = 0.5,
                                        color = '#000000'),
                          fill = list(showText = c(T, T),
                                      propText = 'all',
                                      #optText = 'name',
                                      sizeText = 3,
                                      color = '#DAFFCC',
                                      background = '#FAC0C0'))

dataN <- sampleData('Gln-Glt')
gg_bubbles_map_GlnGlt.(data = dataN)
gg_bubbles_map_GlnGlt.(data = dataN,
                          minSize = 3,
                          maxSize = 7,
                          legend = list(bins = 3,
                                        title = 'titulo'))

gg_bubbles_map_GlnGlt.(data = dataN,
                       count = TRUE,
                          minSize = 3,
                          maxSize = 7,
                          marks = c(',', 'x'),
                          format = c('$', ''),
                          fill = list(showText = c(F, T),
                                      sizeText = 3))



gg_bubbles_map_GlnGltCatNum.(data = NULL)

gg_bubbles_map_GlnGltCatNum.(data = NULL,
                       title = 'Esto es un título',
                       subtitle = 'Esto es un subtitulo',
                       caption = 'esto son los créditos')

dataCN <- sampleData('Gln-Glt-Cat-Num')
gg_bubbles_map_GlnGltCatNum.(data = dataCN )
