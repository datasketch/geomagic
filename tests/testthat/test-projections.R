test_that("Projections",{

 # mercator, this not requires additional params

  opts <- dsvizopts::dsviz_defaults()
  data <- NULL
  l <- geomagic_prep(data, opts)
  l$projections$lat <- 45
  l$projections$long <- 90
  opt_proj <- geomagic:::gg_projections(l$projections)
  expect_equal(names(opt_proj), c("projection", "orientation"))

 # cylequalaera, this requires additional params

  opts$extra$map_projection <- "cylequalarea"
  l <- geomagic_prep(data, opts)
  opt_proj <- geomagic:::gg_projections(l$projections)
  expect_equal(names(opts_proj), c("projection", "param", "orientation"))

 # orientation, by default is c(90, 0, mean(range(x)))
 # Here change lat
  opts <- dsvizopts::dsviz_defaults()
  data <- NULL
  opts$extra$map_projection_lat <- 45
  l <- geomagic_prep(data, opts)
  opt_proj <- geomagic:::gg_projections(l$projections)
  expect_equal(opt_proj$orientation, c(45, 0))

  opts <- dsvizopts::dsviz_defaults()
  opts$extra$map_projection_long <- 90
  l <- geomagic_prep(data, opts)
  opt_proj <- geomagic:::gg_projections(l$projections)
  expect_equal(opt_proj$orientation, c(45, 45))


})
