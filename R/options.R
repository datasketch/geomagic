
chroroplethOpts <- function(){
  list(titleLabel = "",
       subtitle = "",
       caption = "",
       reverse = FALSE,
       fillLabel = "",
       text = TRUE,
       text_size = 2,
       prop_text = 'only_data',
       leg_pos = "right",
       titleLeg = "",
       lowC = "white",
       highC = "black",
       color_map = "gray",
       color_frontier = "white",
       Bcolor = "white",
       palette = "datasketch")
}

parseOpts <- function(opts = NULL, ...){
  .dotOpts <- list(...)
  if(!is.empty(opts)){
    if(!is.empty(.dotOpts)){
      opts <- modifyList(opts,.dotOpts)
    }
    opts <- modifyList(chroroplethOpts(),opts)
  }else{
    if(!is.empty(.dotOpts)){
      opts <- modifyList(chroroplethOpts(),.dotOpts)
    }else{
      opts <- chroroplethOpts()
    }
  }
  opts
}


BubbleOpts <- function(){
  list(
       titleLabel = "",
       subtitle = "",
       caption = "",
       fillLabel = NULL,
       text = FALSE,
       text_size = 0.5,
       prop_text = 'all',
       leg_pos = "right",
       titleLeg = '',
       scale_point = 1.5,
       color_map = "gray",
       color_point = 'red',
       alpha = 0.1,
       color_frontier = "white",
       Bcolor = "transparent")
}



parseOptsBb <- function(opts = NULL, ...){
  .dotOpts <- list(...)
  if(!is.empty(opts)){
    if(!is.empty(.dotOpts)){
      opts <- modifyList(opts,.dotOpts)
    }
    opts <- modifyList(BubbleOpts(),opts)
  }else{
    if(!is.empty(.dotOpts)){
      opts <- modifyList(BubbleOpts(),.dotOpts)
    }else{
      opts <- BubbleOpts()
    }
  }
  opts
}

