#' Write RGB
#'
#' @description This function converts a hex code into a rgba string, with only the alpha value missing
#' @param c Hex code of the colour
#'
#' @export
write_rgb <- function(c) {
  red <- grDevices::col2rgb(c)[1]
  green <- grDevices::col2rgb(c)[2]
  blue <- grDevices::col2rgb(c)[3]

  glue::glue("rgba({red}, {green}, {blue}, ")
}

#' Pick Colour
#' @description This function creates a rgba string based on the variable to be studied
#' @param variable String selecting the variable of study. One of c('confirmed_cases', 'confirmed_deaths', 'confirmed_recovered')
#' @param alpha Alpha value of the final colour
#' @export
pick_colour <- function(variable, alpha = 1, rgb = TRUE) {
  if (variable == "total_cases") colour <- global$colours$orange
  if (variable == "total_deaths") colour <- global$colours$red
  if (variable == "total_recovered") colour <- global$colours$blue

  if (rgb == TRUE) {
    colour <- colour %>% write_rgb()
    return(paste0(colour, alpha, ")"))
  } else {
    return(colour)
  }
}
