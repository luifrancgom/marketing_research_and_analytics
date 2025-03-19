# Libraries ----
library(timetk)
library(tidyverse)

# Functions ----
## tbl_schedule ----
tbl_schedule <- function(start_date,
                         end_date,
                         meeting_days       = c(2,4),
                         open_day_activity  = 1,
                         open_weeks_actity  = c(1, 4, 7),
                         close_day_activity = 7,
                         close_weeks_actity = c(2, 5, 8),
                         day_exam           = 6,
                         weeks_exam         = c(3, 6, 9),
                         dates_recess       = NULL) {
  
  # Base tibble
  block_dates <- tibble::tibble(index = timetk::tk_make_timeseries(start_date = {{start_date}},
                                                                   end_date = {{end_date}},
                                                                   by = "day"))
  
  # Tibble if there is or there is no recess
  if (is.null(dates_recess)) {
    
    # Adding days
    block_dates <- block_dates |>
      dplyr::mutate(day = lubridate::wday(index, week_start = 1))
    
    # Adding weeks
    block_dates <- block_dates |>
      dplyr::mutate(block_week = lubridate::isoweek(index)) |> 
      dplyr::mutate(block_week = block_week - dplyr::first(block_week) + 1)
    
    # Adding virtual meeting dates 
    block_dates <- block_dates |>
      dplyr::mutate(virtual_meeting = dplyr::if_else(condition = day %in% {{meeting_days}},
                                                     true = 1,
                                                     false = 0))
    # Adding activity dates
    block_dates <- block_dates |>
      dplyr::mutate(activity = dplyr::if_else(condition = (((day == {{open_day_activity}}) & 
                                                              (block_week %in% {{open_weeks_actity}})) |
                                                             ((day == {{close_day_activity}}) & 
                                                                (block_week %in% {{close_weeks_actity}}))), 
                                              true = 1,
                                              false = 0))
    
    # Adding exam dates
    block_dates <- block_dates |>
      dplyr::mutate(exam = dplyr::if_else(condition = ((day == {{day_exam}}) &
                                                         (block_week %in% {{weeks_exam}})),
                                          true = 1,
                                          false = 0))
    
  } else {
    
    # Adding dates recess
    block_dates <- block_dates |>  
      dplyr::mutate(recess = dplyr::if_else(condition = timetk::between_time(index = index,
                                                                             start_date = dates_recess[[1]],
                                                                             end_date = dates_recess[[2]]),
                                            true = 1,
                                            false = 0))

    # Adding days
    block_dates <- block_dates |>
      dplyr::mutate(day = lubridate::wday(index, week_start = 1))
    
    # Adding weeks
    block_dates <- block_dates |>
      dplyr::mutate(block_week = lubridate::isoweek(index)) |> 
      dplyr::mutate(block_week = dplyr::if_else(index >= lubridate::ymd(dates_recess[[1]]),
                                                true = block_week - 1,
                                                false = block_week)) |> 
      dplyr::filter(recess != 1) |> 
      dplyr::mutate(block_week = block_week - dplyr::first(block_week) + 1) |> 
      dplyr::select(-recess)

    # Adding virtual meeting dates 
    block_dates <- block_dates |>
      dplyr::mutate(virtual_meeting = dplyr::if_else(condition = day %in% {{meeting_days}},
                                                     true = 1,
                                                     false = 0))
    # Adding activity dates
    block_dates <- block_dates |>
      dplyr::mutate(activity = dplyr::if_else(condition = (((day == {{open_day_activity}}) & 
                                                              (block_week %in% {{open_weeks_actity}})) |
                                                             ((day == {{close_day_activity}}) & 
                                                                (block_week %in% {{close_weeks_actity}}))), 
                                              true = 1,
                                              false = 0))
    
    # Adding exam dates
    block_dates <- block_dates |>
      dplyr::mutate(exam = dplyr::if_else(condition = ((day == {{day_exam}}) &
                                                         (block_week %in% {{weeks_exam}})),
                                          true = 1,
                                          false = 0))
    
  }
  
  # Return schedule tibble
  return(block_dates)
  
}


tbl_schedule(start_date = "2025-03-24", 
             end_date   = "2025-05-31", 
             dates_recess = )
