#' Add custom ABMI theme to a ggplot graphic
#'
#' @param font Defaults to "Montserrat"; alternative is "Domine"
#' @import ggplot2 sysfonts showtext
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

  showtext_auto()

  base_size <- 12
  double_base <- base_size * 2
  half_base <- base_size / 2

  ggplot2::theme(

    # Global text formating
    text = element_text(family = font),

    # Title format:
    plot.title = element_text(size = rel(1.5),
                              face = "bold",
                              color = "#242222",
                              lineheight = 1.2),
    # Subtitle format:
    plot.subtitle = element_text(size = rel(1.2),
                                 color = "#242222",
                                 lineheight = 1.2,
                                 margin = margin(half_base, 0, base, 0)),
    # Caption format:
    plot.caption = element_text(size = rel(0.7),
                                color = "#242222",
                                lineheight = 1.2,
                                hjust = 0,
                                margin = margin(base, 0, 0, 0)),

    # Legend format
    legend.position = "right",
    legend.text.align = 0,
    legend.background = element_blank(),
    legend.title = element_text(color = "#242222"),
    legend.key = element_blank(),
    legend.text = element_text(color = "#242222"),

    # Axis format
    axis.title.y = element_text(margin = margin(r = base)),
    axis.title.x = element_text(margin = margin(t = base)),
    axis.text = element_text(size = rel(0.8)),
    axis.text.y = element_text(margin = margin(r = half_base)),
    axis.text.x = element_text(margin = margin(t = half_base)),
    axis.ticks = element_blank(),
    axis.line = element_blank(),

    # Grid lines
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.major.x = element_blank(),

    # Blank background
    panel.background = element_blank(),

    # Strip background
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = rel(0.8), hjust = 0)
  )

}
