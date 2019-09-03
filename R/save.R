# save plot
#' @export
save_geomagic <- function(viz, filename, format = NULL, width = 10, height = 7, ...) {
  format <- file_ext(filename) %||% "png"
  tmp <- paste(tempdir(), 'svg', sep ='.')
  svglite::svglite(tmp, width = width, height = height)
  print(viz)
  dev.off()
  bitmap <- rsvg::rsvg(tmp, height = 500)
  out_file <- paste0(file_path_sans_ext(filename),'.',format)
  if (format == 'png') {
    png::writePNG(bitmap, out_file, dpi = 144) }
  if (format == 'jpeg') {
    jpeg::writeJPEG(bitmap, out_file)}
  if (format == 'svg') {
    rsvg::rsvg_svg(tmp, out_file)}
  if (format == 'pdf') {
    rsvg::rsvg_pdf(tmp, out_file)
  }
}

