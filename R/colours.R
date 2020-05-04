# ABMI colours
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
  `mamba` = "#9A839C"

)

#' Function to extract ABMI colours as hex codes
#'
#' @param ... Character names of abmi_colours (case insensitive, partial matching allowed)
#'
#' @return A name vector of hex colors
#' @export
#' @examples
#' \donttest{
#' abmi_cols("rhino", "pewter", "locust")
#' }

abmi_cols <- function(...) {

  cols <- c(...)

  if (is.null(cols))
    return (abmi_colours)

  cols <- match.arg(tolower(cols), names(abmi_colours),
                    several.ok = TRUE) # allow partial matching
  abmi_colours[cols]
}

# ABMI palettes
abmi_palettes <- list(

  `main` = abmi_cols("rhino", "pewter", "locust"),

  `lichen` = abmi_cols("locust", "indian khaki", "tonys pink", "pearl bush"),

  `mountains` = abmi_cols("rhino", "pewter", "shadow green", "nepal"),

  `flowers` = abmi_cols("pewter", "fiord", "mamba", "pearl bush"),

  `birds` = abmi_cols("rhino", "nepal", "indian khaki", "tonys pink")

)


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

#' Print palette
#'
#' @param x The palette as a character vector of hex codes
#' @param ... Arguments to pass to \code{image()}
#' @importFrom graphics image par
#'
#' @return An image of the requested colour palette
#' @export
#'

print_palette <- function(x, ...) {

  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x, ylab = "", xaxt = "n", yaxt = "n", bty = "n")

}












