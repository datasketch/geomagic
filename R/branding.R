add_branding_bar <- function(gg, opts_theme, debug = FALSE){

  if(!opts_theme$branding_include){
    return(gg)
  }
  # message("in branding")
  # str(opts_theme)
  background_color <- opts_theme$background_color
  branding_background_color <- opts_theme$branding_background_color %||% background_color
  logo <- opts_theme$logo %||% dsvizopts::local_logo_path("datasketch", branding_background_color)
  logo_height <- opts_theme$branding_logo_height %||% 15
  branding_position <- "bottom"
  logo_position <- opts_theme$logo_position %||% "right"
  branding_text <- opts_theme$branding_text %||% ""
  branding_text_color <- opts_theme$branding_text_color %||% opts_theme$text_color
  branding_text_size <- opts_theme$branding_text_size %||% 8
  text_family <- opts_theme$text_family

  if(branding_position %in% c("top", "bottom")){
    if(logo_position == "right"){
      logo_x <- logo_hjust <- 1
      logo_y <- logo_vjust <- 0
      text_x <- text_hjust <- 0
      text_y <- text_vjust <- 0
    }
    if(logo_position == "left"){
      logo_x <- logo_hjust <- 0
      logo_y <- logo_vjust <- 0
      text_x <- text_hjust <- 1
      text_y <- text_vjust <- 0
    }
  }

  img_html <- glue::glue("<img src='{logo}' height='{logo_height}'/>")
  logo_grob <- gridtext::richtext_grob(img_html,
                                       x = logo_x,
                                       y = logo_y,
                                       hjust = logo_hjust,
                                       vjust = logo_vjust,
                                       margin = unit(c(5,20,5,20),'pt'),
                                       # padding = unit(c(0,0,5,5),'pt'),
                                       gp = grid::gpar(fontsize = branding_text_size,
                                                       fontfamily = text_family,
                                                       col = branding_text_color
                                       ))
  # grid.draw(logo_grob)
  text_grob <- gridtext::richtext_grob(branding_text,
                                       x = text_x,
                                       y = text_y,
                                       hjust = text_hjust,
                                       vjust = text_vjust,
                                       margin = unit(c(5,20,8,20),'pt'),
                                       # padding = unit(c(5,0,5,5),'pt'),
                                       box_gp = grid::gpar(
                                         col = ifelse(debug,"black","transparent"),
                                         fill = ifelse(debug,"black","transparent"),
                                         alpha = ifelse(debug,0.3,1)),
                                       gp = grid::gpar(fontsize = 8,
                                                       fontfamily = "Ubuntu",
                                                       col = branding_text_color
                                       ))
  # grid.draw(text_grob)
  textTree <- grid::grobTree(text_grob, logo_grob)
  rect <- grid::rectGrob(
    y = 0,
    width = 1,
    height = unit(55, "pt"),
    gp=grid::gpar(fill=ifelse(debug, "red", branding_background_color),
                  col = branding_background_color,
                  alpha= ifelse(debug, 0.3, 1))
  )
  bar <- grid::grobTree(rect, textTree)
  # g <- grid.arrange(gg, bottom = bar)
  ggg <- gridExtra::arrangeGrob(gg, bottom = bar)

  class(ggg) <- c("ggmagic", class(ggg))
  # grid.draw(g)
  ggg
}

#' @export
print.ggmagic <- function(x, ...) {
  grid::grid.draw(x)
}
