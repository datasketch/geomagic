test_that("Projections",{

 # mercator, this not requierd adictional params

  opts <- dsvizopts::dsviz_defaults()
  data <- NULL
  l <- geomagic_prep(data, opts)
  opt_proj <- geomagic:::gg_projections(l$projections)
  expect_equal(names(opt_proj), c("projection", "orientation"))

  opts$extra$map_projection <- "cylequalarea"
  l <- geomagic_prep(data, opts)
  opt_proj <- geomagic:::gg_projections(l$projections)
})
