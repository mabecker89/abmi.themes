#' Load ABMI official fonts: Montserrat and Domine
#'
#' @importFrom sysfonts font_families font_add_google
#' @importFrom showtext showtext_auto
#' @export
#'
#' @examples
#' \dontrun{
#' add_abmi_fonts() # call before plotting
#' }
#'
add_abmi_fonts <- function() {

  y <- sysfonts::font_families()

  if(!"Montserrat" %in% y) {
    sysfonts::font_add_google("Montserrat")
  }

  if(!"Domine" %in% y) {
    sysfonts::font_add_google("Domine")
  }

  showtext::showtext_auto()

}