#' Function to extract ABMI colours as hex codes
#'
#' @param ... Character names of abmi_colours
#'
#' @return A name vector of hex colors
#' @export
#' @examples
#' \donttest{
#' abmi_cols("rhino", "pewter", "locust")
#' }

abmi_cols <- function(...) {

  abmi_colours <- c(

    # Main palette
    `rhino` = "#2D415B",
    `pewter` = "#95A09A",
    `locust` = "#A8AF8C",

    # Secondary colours
    `pearl bush` = "#EAE4D7",
    `indian khaki` = "#C2A492",
    `tonys pink` = "#E8A396",
    `shadow green` = "#97C6C3",
    `nepal` = "#829EBC",
    `fiord` = "#4E5971",
    `mamba` = "#9A839C",

    # Tertiary colours
    `mandy` = "#DC4F55",
    `spice` = "#6E4830",
    `summer green` = "#8BBAA3",
    `tapestry` = "#B4549F",
    `costa del sol` = "#5F682D",
    `marigold` = "#BC862F"

  )

  cols <- c(...)

  if (is.null(cols))
    return (abmi_colours)

  abmi_colours[cols]
}

#' Return function to interpolate an ABMI color palette
#'
#' @param palette Character name of palette in abmi_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette() (e.g. alpha value)
#' @importFrom grDevices colorRampPalette
#' @export
#' @examples
#' \donttest{
#' abmi_pal(palette = "main", reverse = FALSE)
#' }
#' #' @return A list of hex colours

abmi_pal <- function(palette = "main", reverse = FALSE, ...) {

  abmi_palettes <- list(

    `main` = abmi_cols("rhino", "pewter", "locust"),

    `vegetation` = abmi_cols("pewter", "pearl bush", "indian khaki", "tonys pink"),

    `mountains` = abmi_cols("rhino", "pewter", "shadow green", "nepal"),

    `flowers` = abmi_cols("locust", "fiord", "mamba", "pearl bush"),

    `birds` = abmi_cols("rhino", "nepal", "tonys pink", "indian khaki"),

    `vegetation full` = abmi_cols("pewter", "pearl bush", "indian khaki", "tonys pink", "mandy", "spice", "summer green"),

    `flowers full` = abmi_cols("locust", "fiord", "mamba", "pearl bush", "tapestry", "costa del sol", "marigold")

  )

  pal <- abmi_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)

}

#' Colour scale constructor for ABMI colours
#'
#' @param palette Character name of palette in ABMI palettes
#' @param discrete Boolean indicating whether colour aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to \code{discrete_scale()} or \code{scale_color_gradient()}, which are used when discrete is TRUE or FALSE, respectively
#' @import ggplot2
#'
#' @export

scale_color_abmi <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {

  pal <- abmi_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("abmi_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for ABMI colours
#'
#' @param palette Character name of palette in ABMI palettes
#' @param discrete Boolean indicating whether colour aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to \code{discrete_scale()} or \code{scale_fill_gradient()}, which are used when discrete is TRUE or FALSE, respectively
#' @import ggplot2
#'
#' @export

scale_fill_abmi <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {

  pal <- abmi_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("abmi_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}










