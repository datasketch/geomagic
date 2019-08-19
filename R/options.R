getDefaultOpts <- function(...){

  dopts <- list(

    agg = "sum",
    agg_text = NULL,

    border_color = "#2d2d2d",
    border_width = 1, #cambiar borderWidth


    nLevels = 5,
    labelWrap = 12,
    projection_name = NULL,
    projection_ratio = NULL,
    projection_type = NULL,
    projection_orientation = c(90, 0, 0),
    dropNa = FALSE,
    na_color = "#cccccc",
    graticule = FALSE,
    graticule_color = '#cccccc',
    graticule_interval = 50,
    graticule_weight = 1,
    #border_opacity = 1, #cambiar  borderOpacity
    opacity = 0.7,
    stroke = FALSE,
    radius = 7,
    min_radius = 1,
    max_radius = 10,
    title =  NULL,
    title_color = '#000000',
    title_family = 'Ubuntu',
    title_size = 10,
    subtitle = NULL,
    subtitle_color = '#000000',
    subtitle_family = 'Ubuntu',
    subtitle_size = 9,

    caption = NULL,
    caption_color = '#000000',
    caption_family = 'Ubuntu',
    caption_size = 7,
    count = TRUE,
    colors = NULL,

    fill_opacity = 1,

    marks = c(".", ","),
    nDigits = NULL,
    text_show = FALSE,
    text_proportion = 'all', #*
    text_option = 'code',
    text_size = 3,
    legend_show = c(TRUE, TRUE), #!=
    legend_background = 'transparent',
    legend_borderColor = 'transparent',#*
    legend_color = '#000000' ,
    legend_limit = 0.1, #*
    legend_mode = 'quantile', #*
    legend_title = NULL,#*
    legend_size = 13,
    legend_position = 'left',
    percentage = FALSE,
    suffix = NULL,
    prefix = NULL,
    scale = "discrete",
    default_color = "transparent",
    zoom = 5
  )
  dopts
}


getOpts <- function(opts = NULL){

  userOpts <- opts
  defaultOpts <- getDefaultOpts()

  if(!is.null(opts)){
    opts <- modifyList(defaultOpts, userOpts)
  }else{
    opts <- defaultOpts
  }

  opts
}



