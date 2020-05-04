#' Add ABMI logo to the bottom of the plot
#'
#' @param plot_name The variable name of the plot you have created
#' @param logo_image_path File path for the logo image you want to use in the right hand side of your plot, which needs to be a PNG file; defaults to the ABMI's primary horizontal acronym logo that is stored in the data\ folder of this package
#' @param logo_position A numeric vector or unit object specifying x-location of logo; defaults to 0.925 (right justified)
#' @importFrom grid grobTree rasterGrob
#' @importFrom png readPNG
#' @importFrom ggpubr ggarrange
#' @return This function returns your plot with the ABMI logo at the bottom right
#' @export
#'
#' @examples
#' \donttest{
#' add_logo(p)
#' }

add_logo <- function (plot_name,
                      logo_image_path = file.path(system.file("extdata", package = 'abmi.themes'), "horizontal-logo-acronym.PNG"),
                      logo_position = 0.925) {

  # Make footer
  footer <- grid::grobTree(grid::rasterGrob(png::readPNG(logo_image_path), x = logo_position))

  # Arrange
  plot_with_logo <- ggpubr::ggarrange(plot_name, footer, ncol = 1, nrow = 2, heights = c(1, 0.045))

  return(plot_with_logo)

}


