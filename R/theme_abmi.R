#' Add custom ABMI theme to a ggplot graphic
#'
#' @param font Defaults to "Montserrat"; alternative is "Domine"
#' @import ggplot2 showtext
#' @export
#'
#' @examples
#' \donttest{
#' p + theme_abmi()
#' }

theme_abmi <- function(font = "Montserrat") {

  abmi_web_fonts <- c("Montserrat", "Domine")

  if(!font %in% abmi_web_fonts) {
    stop("Please chooose one of the ABMI's official web fonts: Montserrat or Domine.")
  }

  if(font == "Montserrat") {
    if(!"Montserrat" %in% font_families()) {
      font_add_google("Montserrat")
    }
  }

  if(font == "Domine") {
    if(!"Domine" %in% font_families()) {
      font_add_google("Domine")
    }
  }

  showtext::showtext_auto()

  base_size <- 12
  double_base <- base_size * 2
  half_base <- base_size / 2

  ggplot2::theme(

    # Global text formating
    text = ggplot2::element_text(family = font),

    # Title format:
    plot.title = ggplot2::element_text(size = ggplot2::rel(1.5),
                              face = "bold",
                              color = "#242222",
                              lineheight = 1.2),
    # Subtitle format:
    plot.subtitle = ggplot2::element_text(size = ggplot2::rel(1.2),
                                 color = "#242222",
                                 lineheight = 1.2,
                                 margin = ggplot2::margin(half_base, 0, base, 0)),
    # Caption format:
    plot.caption = ggplot2::element_text(size = ggplot2::rel(0.7),
                                color = "#242222",
                                lineheight = 1.2,
                                hjust = 0,
                                margin = ggplot2::margin(base, 0, 0, 0)),

    # Legend format
    legend.position = "right",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(color = "#242222"),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(color = "#242222"),

    # Axis format
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = base)),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = base)),
    axis.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = half_base)),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = half_base)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),

    # Grid lines
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "grey80"),
    panel.grid.major.x = ggplot2::element_blank(),

    # Blank background
    panel.background = ggplot2::element_blank(),

    # Strip background
    strip.background = ggplot2::element_rect(fill = "white"),
    strip.text = ggplot2::element_text(size = ggplot2::rel(0.8), hjust = 0)
  )

}
