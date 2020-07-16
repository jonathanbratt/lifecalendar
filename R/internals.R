# make_basic_calendar -----------------------------------------------------


#' Make a basic life calendar
#'
#' Construct a tibble of weeks, starting from a given birth date, with columns
#' giving the week start date, year, and week in year. The convention adopted is
#' that each week is represented by its first day, where the canonical first day
#' of the week is chosen by setting the \code{week_start} parameter.
#'
#' This means, for example, that given a birth date that fell on a Monday and
#' weeks starting on Sundays, the first date in the calendar will be the day
#' before the given birth date.
#'
#' @param birth_date A date object specifying when to begin the life calendar.
#' @param week_start Integer; what day is taken as the first day of the week?
#'  (1 = Monday, 7 = Sunday, etc.)
#' @param num_years Integer; the number of years included in the calendar.
#'
#' @return A tibble with a row for each week starting at \code{birth_date} and
#' continuing for \code{num_years} years, with columns:
#' \itemize{
#'  \item{"date"}{The start date of the week}
#'  \item{"year_count"}{The year the week falls in, starting with 1}
#'  \item{"week_in_year"}{The index of the week in the year, starting with 1}
#' }
#' @keywords internal
.make_basic_calendar <- function(birth_date,
                                 week_start,
                                 num_years) {
  # start on first day of birth week
  start_week <- lubridate::floor_date(birth_date,
                                      unit = "week",
                                      week_start = week_start)
  start_month <- lubridate::month(birth_date)
  start_day <- lubridate::day(birth_date)

  week_list <- seq.Date(from = start_week,
                        to = start_week + lubridate::years(num_years),
                        by = "week")

  birthday_week_list <- seq.Date(from = birth_date,
                                 by = "years",
                                 length.out = 100)
  birthday_week_list <- lubridate::floor_date(birthday_week_list,
                                              unit = "week",
                                              week_start = week_start)
  calendar <- tibble::enframe(week_list, name = NULL, value = "date")
  calendar <- dplyr::mutate(
    calendar,
    birthday_week = date %in% birthday_week_list,
    year_count = cumsum(birthday_week)
  )

  calendar <- dplyr::mutate(
    dplyr::group_by(calendar, year_count),
    week_in_year = dplyr::row_number()
  )
  return(
    dplyr::select(
      dplyr::ungroup(calendar),
      date,
      year_count,
      week_in_year
    )
  )
}



# add_dates_and_colors --------------------------------------------------------

#' Assign colors to special dates
#'
#' Given a basic life calendar and a list of special dates with corresponding
#' colors, add fields to the calendar for later use with color aesthetics.
#' Note: a specified date will only be added to the calendar if a corresponding
#' color is specified in either \code{color_values} or \code{fill_values}.
#'
#' @param calendar A basic life calendar object; output tibble from
#'  \code{.make_basic_calendar}.
#' @param special_dates A named list of special dates or intervals.
#' @param color_values A named list of colors (to be used for "color" aesthetic
#'  in calendar graphic). The names should correspond to the names used for
#'  \code{special_dates}.
#' @param fill_values  A named list of colors (to be used for "fill" aesthetic
#' in calendar graphic). The names should correspond to the names used for
#'  \code{special_dates}.
#' @param today A date object specifying when "today" is. This is used later
#'  to color the "past" and "future" differently, if desired.
#' @inheritParams .make_basic_calendar
#'
#' @return The given life calendar tibble, with additional columns:
#' \itemize{
#'  \item{"outline"}{A character column containing either "default", or else
#'   the name of the corresponding special date.}
#'  \item{"fill"}{A character column containing either "default", or "past", or
#'   the name of the corresponding special date.}
#'  \item{"outline_thickness"}{A character column containing either "default" or
#'   "grid" (if the year or week index is a multiple of five; this is for
#'  optionally including a grid pattern in the calendar graphic.)}
#' }
#' Note that each week can have only a single value in each of the color
#' aesthetics columns. If there are overlapping special dates (e.g. an interval
#' that overlaps with another, or a date that falls within an interval), the
#' following rules of priority are followed: date objects always take priority
#' over interval objects, and dates (or intervals) that are specified later
#' in the list of special dates take priority over dates (or intervals)
#' listed sooner.
#'
#' A week is considered "in" an interval if there is any overlap between the
#' week and the interval.
#'
#' @keywords internal
.add_dates_and_colors <- function(calendar,
                                  special_dates,
                                  color_values,
                                  fill_values,
                                  today,
                                  week_start) {
  # We don't have to de-listify dates or intervals here. They are handled fine
  # downstream.
  special_intervals <- purrr::compact(
    purrr::map(
      special_dates,
      function(d) {
        if (inherits(d, "list")) {
          if (all(purrr::map_lgl(d, lubridate::is.interval))) {
            # round start date of all intervals down to beginning of week.
            d <- purrr::map(
              d,
              function(d2) {
                lubridate::int_start(d2) <- lubridate::floor_date(
                  lubridate::int_start(d2),
                  unit = "week",
                  week_start = week_start
                )
                return(d2)
              }
            )
            return(d)
          }
        } else if (lubridate::is.interval(d)) {
          # round start date of interval down to beginning of week.
          lubridate::int_start(d) <- lubridate::floor_date(
            lubridate::int_start(d),
            unit = "week",
            week_start = week_start
          )
          return(d)
        }
        return(NULL)
      }
    )
  )

  special_dates <- purrr::compact(
    purrr::map(
      special_dates,
      function(d) {
        if (inherits(d, "list")) {
          if (all(purrr::map_lgl(d, lubridate::is.Date))) {
            # round all dates down to beginning of week.
            d <- purrr::map(
              d,
              function(d2) {
                d2 <- lubridate::floor_date(
                  d2,
                  unit = "week",
                  week_start = week_start
                )
                return(d2)
              }
            )
            return(d)
          }
        } else if (lubridate::is.Date(d)) {
          # Round date down to beginning of week.
          d <- lubridate::floor_date(
            d,
            unit = "week",
            week_start = week_start
          )
          return(d)
        } else {
          return(NULL)
        }
      }
    )
  )

  calendar <- dplyr::mutate(
    calendar,
    outline = "default",
    fill = ifelse(date <= today, "past", "default"),
    outline_thickness = ifelse(year_count %% 5 == 0 | week_in_year %% 5 == 0,
                               "grid",
                               "default")
  )

  for (special in names(special_intervals)) {
    interval <- special_intervals[[special]]
    if (special %in% names(fill_values)) {
      calendar <- dplyr::mutate(
        calendar,
        fill = ifelse(lubridate::`%within%`(date, interval),
                      special,
                      fill)
      )
    }
    if (special %in% names(color_values)) {
      calendar <- dplyr::mutate(
        calendar,
        outline = ifelse(lubridate::`%within%`(date, interval),
                         special,
                         outline)
      )
    }
  }

  for (special in names(special_dates)) {
    sdate <- special_dates[[special]]
    if (special %in% names(fill_values)) {
      calendar <- dplyr::mutate(
        calendar,
        fill = ifelse(date %in% sdate, # %in% to handle sdate being list
                      special,
                      fill)
      )
    }
    if (special %in% names(color_values)) {
      calendar <- dplyr::mutate(
        calendar,
        outline = ifelse(date %in% sdate, # %in% to handle sdate being list
                         special,
                         outline)
      )
    }
  }
  return(calendar)
}

# generate_graphic --------------------------------------------------------


#' Make a graphical representation of a life calendar
#'
#' Given a life calendar tibble and a list of special dates with corresponding
#' colors, add fields to the calendar for later use with color aesthetics.
#' Note: a specified date will only be added to the calendar if a corresponding
#' color is specified in either \code{color_values} or \code{fill_values}.
#'
#' @param calendar A basic calendar object; output tibble from
#'  \code{.add_dates_and_colors}.
#' @param fill_default Character; color to be used for default week fill.
#' @param fill_past Character; color to be used to fill weeks in the "past".
#' @param color_default Character; color to be used for default outline color.
#' @param default_thickness Numeric; default thickness used to draw week cells.
#' @param grid_thickness Numeric; thickness used to draw "grid" week cells.
#' @param caption Character; caption to included at bottom of graphic.
#' @param ellipse_n Integer; number of polygon vertices to use for approximating
#'  ellipses.
#' @inheritParams .add_dates_and_colors
#'
#' @return A ggplot object graphically representing the given life calendar.
#'  Weeks are represented by ellipses, and each year is a single row of
#'  ellipses (usually 52, though occasionally 53).
#'
#' @keywords internal
.generate_graphic <- function(calendar,
                              fill_values,
                              color_values,
                              fill_default,
                              fill_past,
                              color_default,
                              default_thickness,
                              grid_thickness,
                              caption,
                              ellipse_n) {
  # just check names for overlap, and give a warning.
  if (any(c("default", "past") %in% c(names(fill_values),
                                      names(color_values)))) {
    warning("The labels 'default' and 'past' are used internally.",
            "Using them as custom labels may have unintended consequences.")
  }
  color_values_all <- c("default" = color_default)
  fill_values_all <- c("default" = fill_default, "past" = fill_past)
  size_values <- c("default" = default_thickness, "grid" = grid_thickness)

  for (dtype in names(color_values)) {
    color_values_all[[dtype]] <- color_values[[dtype]]
  }
  for (dtype in names(fill_values)) {
    fill_values_all[[dtype]] <- fill_values[[dtype]]
  }

  pl <- ggplot2::ggplot(calendar) +
    ggforce::geom_ellipse(mapping = ggplot2::aes(x0 = week_in_year,
                                                 y0 = year_count,
                                                 a = .45, b = .4, angle = 0,
                                                 color = outline,
                                                 fill = fill,
                                                 size = outline_thickness),
                          n = ellipse_n) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.position = "none",
                   plot.caption = ggplot2::element_text(size = 12,
                                                        hjust = 0.5)) +
    ggplot2::labs(caption = caption) +
    ggplot2::scale_size_manual(values = size_values) +
    ggplot2::scale_color_manual(values = color_values_all) +
    ggplot2::scale_fill_manual(values = fill_values_all) +
    ggplot2::scale_x_continuous(position = "top",
                                breaks = seq(5, 50, 5),
                                expand = c(0.01,0)) +
    ggplot2::scale_y_reverse(position = "right",
                             breaks = seq(5, 100, 5),
                             expand = c(0.01,0))
  return(pl)
}

