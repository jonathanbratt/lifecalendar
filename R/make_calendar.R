# make_calendar -----------------------------------------------------------

#' Make a life calendar graphic
#'
#' This routine produces a life calendar as a ggplot object. A life calendar is
#' a week-by-week representation of an entire (expect) lifetime. At reasonable
#' scales, one hundred years' worth of weeks can fit on a single sheet of paper.
#' The graphic produced by this function is suitable for printing at that scale.
#'
#' Each week is represented by a small "bubble" (think standardized test answer
#' sheet). The first week is the week that the subject was born, and a new row
#' begins with each new birthday week.
#'
#' At minimum, a life calendar is defined by a birth date (the only function
#' parameter without a default value). By default, the bubbles are filled in up
#' to (and including) the current week. This behavior can be controlled by
#' setting the \code{fill_past} parameter. "Future" bubbles are left unfilled by
#' default, but this can be overridden by passing a color to the
#' \code{fill_default} parameter.
#'
#' Optionally, significant past dates/periods can be colored differently. This
#' is accomplished by passing a named list of special dates and/or inteverals to
#' the \code{special_dates} parameter, along with corresponding named lists of
#' colors to the \code{color_values} and/or \code{fill_values} parameters (to
#' specify bubble outline and bubble fill colors, respectively).
#'
#' It is sometimes desirable to add "grid lines" to the array of bubbles by
#' drawing certain bubble outlines more thickly. This is currently the default;
#' to *not* show the grid, set the parameters \code{default_thickness} and
#' \code{grid_thickness} to the same value (suggested 0.6).
#'
#' @inheritParams .make_basic_calendar
#' @inheritParams .add_dates_and_colors
#' @inheritParams .generate_graphic
#'
#' @return A ggplot object graphically representing a life calendar. Weeks are
#'   represented by ellipses, and each year is a single row of ellipses (usually
#'   52, though occasionally 53).
#'
#' @export
#'
#' @examples
make_calendar <- function(birth_date,
                          week_start = 7L,
                          num_years = 100L,
                          special_dates = NULL,
                          color_values = NULL,
                          fill_values = NULL,
                          fill_default = "white",
                          fill_past = "gray60",
                          color_default = "gray40",
                          default_thickness = 0.6,
                          grid_thickness = 0.85,
                          today = lubridate::today(),
                          caption = NULL,
                          ellipse_n = 16L) {
  cal <- .make_basic_calendar(birth_date = birth_date,
                              week_start = week_start,
                              num_years = num_years)
  cal <- .add_dates_and_colors(calendar = cal,
                               special_dates = special_dates,
                               color_values = color_values,
                               fill_values = fill_values,
                               today = today,
                               week_start = week_start)
  return(
    .generate_graphic(calendar = cal,
                      color_values = color_values,
                      fill_values = fill_values,
                      fill_default = fill_default,
                      fill_past = fill_past,
                      color_default = color_default,
                      default_thickness = default_thickness,
                      grid_thickness = grid_thickness,
                      ellipse_n = ellipse_n,
                      caption = caption)
  )
}

