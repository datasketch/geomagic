
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
       color_map = "gray",
       color_frontier = "white",
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
