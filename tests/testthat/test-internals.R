test_that("special dates work", {

  # non-listified
  special_dates <- list(
    "john" = lubridate::ymd("1917-11-16"),
    "michael" = lubridate::ymd("1920-10-22"),
    "christopher" = lubridate::ymd("1924-11-21"),
    "ww1" = lubridate::interval(start = lubridate::ymd("1914-07-28"),
                                end = lubridate::ymd("1918-11-11")),
    "ww2" = lubridate::interval(start = lubridate::ymd("1939-09-01"),
                                end = lubridate::ymd("1945-09-02")))

  fill_values <- c(john = "deepskyblue2",
                   michael = "deepskyblue2",
                   christopher = "deepskyblue2",
                   ww1 = "gray10",
                   ww2 = "gray10" )

  testcal <- make_calendar(birth_date = lubridate::ymd("1892-01-03"),
                           special_dates = special_dates,
                           fill_values = fill_values,
                           today = lubridate::ymd("1973-09-02"),
                           ellipse_n = 6,
                           caption = "test")

  testthat::expect_true(all(names(special_dates) %in%
                              unique(testcal$data$fill)))

  # listified:
  special_dates2 <- list(
    "daughter" = lubridate::ymd("1929-06-18"),
    "sons" = list(lubridate::ymd("1917-11-16"),
                  lubridate::ymd("1920-10-22"),
                  lubridate::ymd("1924-11-21")),
    "wars" = list(lubridate::interval(start = lubridate::ymd("1914-07-28"),
                                      end = lubridate::ymd("1918-11-11")),
                  lubridate::interval(start = lubridate::ymd("1939-09-01"),
                                      end = lubridate::ymd("1945-09-02"))),
    "war_service" = lubridate::interval(start = lubridate::ymd("1916-06-06"),
                                        end = lubridate::ymd("1916-11-09")))

  fill_values2 <- c(sons = "deepskyblue2",
                    daughter = "deepskyblue1",
                    war_service = "black",
                    wars = "gray10")

  testcal2 <- make_calendar(birth_date = lubridate::ymd("1892-01-03"),
                           special_dates = special_dates2,
                           fill_values = fill_values2,
                           today = lubridate::ymd("1973-09-02"),
                           ellipse_n = 6,
                           caption = "test")

  testthat::expect_true(all(names(special_dates2) %in%
                              unique(testcal2$data$fill)))

})
