#' Add ABMI logo to the bottom of the plot
#'
#' @param plot_name The variable name of the plot you have created
#' @param logo Character; Specify which logo you want (see details below)
#' @param logo_position A numeric vector or unit object specifying x-location of logo; defaults to 0.925 (right justified)
#' @details Valid values for the \code{logo} argument currently are:
#' \itemize{
#' \item "acronym" (default)
#' \item "full"
#' \item Personal file path for a logo image you want to use (needs to be a png file)
#' }
#' @importFrom grid grobTree rasterGrob
#' @importFrom png readPNG
#' @importFrom ggpubr ggarrange
#' @return This function returns your plot with the ABMI logo at the bottom right
#' @export
#'
#' @examples
#' \dontrun{
#' add_logo(p, logo = "full", logo_position = 0.875)
#' }

add_logo <- function (plot_name,
                      logo = "acronym",
                      logo_position = 0.925) {

  # Logo choices
  if(logo == "acronym") {
    logo_image_path = file.path(system.file("extdata", package = 'abmi.themes'), "ABMI-Horizontal-FullColour-acronym.png")
  } else if(logo == "full") {
    logo_image_path = file.path(system.file("extdata", package = 'abmi.themes'), "ABMI-Horizontal-FullColour.png")
  } else {
    logo_image_path = logo
  }

  if(logo == "full" & logo_position >= 0.900) {
    message("With the current position value, your logo will be partially cut off. If you want it in the bottom right, a good default position when using the full logo is 0.825")
  }

  # Make footer
  footer <- grid::grobTree(grid::rasterGrob(png::readPNG(logo_image_path), x = logo_position))

  # Adjust logo height
  if(logo == "full") {
    logo_height <- 0.065
  } else
    logo_height <- 0.045

  # Arrange
  plot_with_logo <- ggpubr::ggarrange(plot_name, footer, ncol = 1, nrow = 2, heights = c(1, logo_height))

  return(plot_with_logo)

}


