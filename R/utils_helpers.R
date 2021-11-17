breasy_blue <- "#0091DF"
breasy_purple <- "#D30F4B"
breasy_grey <- "#383838"
breasy_green <- "#66B512"

font_color <- function (hex_code) {
  ifelse(
    ((grDevices::col2rgb(hex_code)[1] * 0.299) + (grDevices::col2rgb(hex_code)[2] * 0.587) + (grDevices::col2rgb(hex_code)[3] * 0.114) > 186),
    "#000000",
    "#ffffff"
  )
}