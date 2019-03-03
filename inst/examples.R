library(devtools)
load_all()
document()
install()

library(geomagic)



# Choropleth --------------------------------------------------------------

# Examples GcdNum ---------------------------------------------------------

gg_choropleth_GcdNum()
gg_choropleth_GcdNum(opts = list(graticule = T))
gg_choropleth_GcdNum(mapName =  "american_countries")
gg_choropleth_GcdNum(data = NULL, mapName =  "american_countries",
                         opts = list(
                           showText = c(TRUE, FALSE))
)
gg_choropleth_GcdNum(data = sampleData("Gcd-Num", 1000), mapName =  "american_countries",
                         opts = list(
                           showText = c(TRUE, FALSE))
)

gg_choropleth_Gcd()
gg_choropleth_Gcd(data = sampleData("Gcd"))

gg_choropleth_GcdCat()
gg_choropleth_GcdCat(sampleData("Gcd-Cat"), opts = list(count = F))

gg_choropleth_GcdNum(data = NULL,
                         opts = list(
                           showText = c(TRUE, FALSE),
                           textMap = list(
                             propText = 'all'))
)
gg_choropleth_GcdNum(data = NULL,
                         opts = list(
                           showText = c(TRUE, FALSE),
                           textMap = list(
                             propText = 'all',
                             optText = 'name'))
)

dataGN <- sampleData('Gcd-Num')
gg_choropleth_GcdNum(data = dataGN)
gg_choropleth_GcdNum(data = dataGN, opts = list(nDigits = 0))
gg_choropleth_GcdNum(data = dataGN,
                         opts = list(
                           titles = list(
                             title = list(
                               text = "Este es un título"
                             ),
                             subtitle = list(
                               text = "este es el sutitulo"
                             ),
                             caption = list(
                               text = "créditos"
                             )
                           ),
                           agg = 'mean',
                           format = c('$', '')
                         ))



gg_choropleth_GcdNum(data = dataGN,
                         opts = list(scale = 'discrete',
                                     nLevels = 4,
                                     mode = 'no',
                                     color = c('darkred', 'black'),
                                     showText = c(TRUE, FALSE),
                                     percentage = TRUE)
)


gg_choropleth_GcdNum(data = dataGN,
                         opts = list(
                           marks = c('','.'),
                           nlevels = 3,
                           showText = c(T, T),
                           textMap = list(optText = 'name',
                                          propText = 'onlyData',  #all, onlyData,and a number
                                          size = 3))
)

gg_choropleth_GcdNum(data = dataGN,
                         opts = list(
                           nLevels = 5,
                           limit = 0.1,
                           marks = c('','.'),
                           background = "darkblue"))

gg_choropleth_GcdNum(data = dataGN,
                         opts = list(nLevels = 5,
                                     titles = list(
                                       title = list(
                                         text = "Este es un título"
                                       )),
                                     marks = c('','.'),
                                     nDigits = 0,
                                     projectionName = "ortho"))

data <- data.frame(pais = c('COL',  'ARG', 'BRA', 'RUS', 'MEX'),
                   total = c(1, 1, 1, 1, 1))
gg_choropleth_GcdNum(data = data,
                         opts = list(
                           nLevels = 3,
                           scale = 'discrete',
                           marks = c('','.'),
                           projectionName = "ortho",
                           projectionOpts = list(
                             orientation = c(41, -74, 0))))

gg_choropleth_GcdNum(data = data,
                         opts = list(
                           nLevels = 1,
                           marks = c('','.'),
                           color = c('orange'),
                           percentage = TRUE)
)

gg_choropleth_GcdNum(data = data,
                         opts = list(
                           scale = 'discrete',
                           nDigits = 2))

data <- sampleData('Gcd-Num', 500)
gg_choropleth_GcdNum(data = data,
                         opts = list(
                           nLevels = 5,
                           scale = 'discrete',
                           color = c('#FD2AAA', '#00FD12'),
                           legend = list(
                             position =  c(0, 0.2)),
                           nDigits = 2))


gg_choropleth_GcdNum(data = NULL, mapName = "col_departments")
gg_choropleth_GcdNum(data = NULL,
                         mapName = "col_departments",
                         opts = list(
                           projectionName = "mercator",
                           projectionOpts = list(
                             ratio = 1)))

data <- data.frame(a = c('COL', 'COL', 'ARG', 'BRA', 'BRA', 'BRA', 'BRA', 'USA', 'MEX'))
gg_choropleth_Gcd(data = NULL)
gg_choropleth_Gcd(data = data, mapName =  "american_countries")
gg_choropleth_Gcd(data = data,
                      mapName =  "american_countries",
                      opts = list(scale = 'discrete',
                                  showText = c(TRUE, FALSE),
                                  sizeText = 3,
                                  optText = 'name',
                                  mode = 'no',
                                  opacity = 0.7,
                                  color = c('red', 'yellow', 'green'),
                                  format = c('$', 'ana'),
                                  marks = c('.', ','),
                                  nDigits = 4,
                                  position = c(0, 0.2))
)


data <- sampleData('Gcd-Cat', 100)
gg_choropleth_GcdCat(data = NULL)
gg_choropleth_GcdCat(data = NULL,
                         opts = list(showText = c(TRUE, FALSE)))
gg_choropleth_GcdCat(data = data,
                         opts = list(showText = c(TRUE, FALSE),
                                     textMap = list(
                                       optText = 'name'))
)
gg_choropleth_GcdCat(data = data,
                         opts = list(scale = 'discrete'))
gg_choropleth_GcdCat.(data = data,
                          count = TRUE,
                          fill = list(scale = 'discrete',
                                      showText = c(TRUE, TRUE)))


# Examples Gnm ------------------------------------------------------------

data <- sampleData('Gnm', 100)
gg_choropleth_Gnm(data = NULL,
                      opts = list(
                        titles = list(
                          title = list(
                            text =  'Mapa sin datos'))
                      ))
gg_choropleth_Gnm(data = data)
gg_choropleth_Gnm(data = data,
                      mapName = 'africa_countries',
                      opts = list(
                        titles = list(
                          title = list(
                            text = "Mapa con datos",
                            color = "#FFFCCC"
                          ),
                          subtitle = list(
                            text = "porq es una prueba",
                            color = "#FFFCCC"
                          ),
                          caption = list(
                            text = 'datasketch',
                            color = "#FFFCCC"
                          )
                        ),
                        borderWidth = 0.5,
                        borderColor = '#FFAA12',
                        color = c('#FCCADF', '#AA12FC'),
                        naColor = '#FDDFCC',
                        opacity = 0.9,
                        scale = 'discrete',
                        background = '#000000',
                        legend = list(
                          border = 'green',
                          background = 'transparent',
                          color = '#FFFFFF',
                          title = " "
                        )))

#

# Examples GnmNum ---------------------------------------------------------


dataGn <- sampleData('Gnm-Num', 1000)
gg_choropleth_GnmNum(data = dataGn)
gg_choropleth_GnmNum(data = dataGn, mapName = 'latam_countries' )
gg_choropleth_GnmNum(data = dataGn,
                         mapName = 'latam_countries',
                         opts = list(
                           projectionName = "mercator",
                           projectionOpts = list (
                                        ratio = 1))
                          )



# Examples GnmCat ---------------------------------------------------------

dataGc <- sampleData('Gnm-Cat')
gg_choropleth_GnmCat(data = NULL)
gg_choropleth_GnmCat(data = NULL,
                          opts = list(
                            background = '#FDAA11'
                          ))
gg_choropleth_GnmCat(data = dataGc,
                          opts = list(
                            background = '#FDAA11',
                            nLevels = 3
                          ))


# Bubbles -----------------------------------------------------------------

# Examples GcdNum ---------------------------------------------------------
gg_bubbles_GcdNum(data = NULL)
dataN <- sampleData('Gcd-Num')
gg_bubbles_GcdNum(data = dataN)


# Examples Gcd ------------------------------------------------------------



# Examples GcdCatNum ------------------------------------------------------
gg_bubbles_GcdCatNum(data = NULL)
dataCN <- sampleData('Gcd-Cat-Num')
gg_bubbles_GcdCatNum(data = dataCN)



# Examples GcdCat ---------------------------------------------------------

# Examples GlnGltNum ------------------------------------------------------
gg_bubbles_GlnGltNum(data = NULL)
dataN <- sampleData('Gln-Glt-Num')
gg_bubbles_GlnGltNum(data = dataN)

# Examples GlnGlt ---------------------------------------------------------
gg_bubbles_GlnGlt(data = NULL)
dataN <- sampleData('Gln-Glt')
gg_bubbles_GlnGlt(data = dataN)



# Examples GlnGltCatNum ---------------------------------------------------
gg_bubbles_GlnGltCatNum(data = NULL)
dataCN <- sampleData('Gln-Glt-Cat-Num')
gg_bubbles_GlnGltCatNum(data = dataCN,
                            opts = list(
                              bubbles = list(
                                minSize = 0.1,
                                maxSize = 5
                            )))


# Examples GlnGltCat ------------------------------------------------------
gg_bubbles_GlnGltCat(data = NULL)
dataC <- sampleData('Gln-Glt-Cat')
gg_bubbles_GlnGltCat(data = dataC)
gg_bubbles_GlnGltCat(data = dataC,
                         opts = list(
                           legend = list(
                             show = c(TRUE,FALSE),
                             title = c("lege1", "")
                           )
                         ))
dataC <- sampleData('Gln-Glt-Cat', 1000)
gg_bubbles_GlnGltCat(data = dataC,
                         opts = list(
                           legend = list(
                             title = c("legend1", "legend2")
                           )
                         ))

# Examples GnmNum ---------------------------------------------------------
gg_bubbles_GnmNum(data = NULL)
dataN <- sampleData('Gnm-Num')
gg_bubbles_GnmNum(data = dataN)


# Examples GnmCatNum. -----------------------------------------------------
gg_bubbles_GnmCatNum(data = NULL,
                          opts = list( showText = c(TRUE, FALSE)))
dataCN <- sampleData('Gnm-Cat-Num')
gg_bubbles_GnmCatNum(data = dataCN)

