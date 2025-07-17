#' tod_plot
#'
#' @description Time-of-day plot for ED arrivals
#' @param df Data frame with datetime and arrival_mode columns
#' @param yrs Numeric vector of years to include
#' @param inp Character vector of selected arrival modes
#' @param times Character vector of time range (e.g., c("08:00", "20:00"))
#' @export
tod_plot <- function(df, yrs = 2021:2024, inp, times) {
  colfun <- grDevices::colorRampPalette(c("#005AA2", "#FFC72C", "#DE5428"))
  outp <- c("Ambulance" = "Ground Ambulance", "Walk-in" = "No Ambulance")
  rg <- range(lubridate::hour(df$datetime))
  if (rg[2] < 23) rg[2] <- rg[2] + 1
  times <- as.numeric(sub(":00", "", times))
  
  first_time <- times[1]
  second_time <- times[2]
  is_overnight <- second_time < first_time
  
  shapes_list <- if (is_overnight) {
    list(
      list(type = "rect", xref = "x", yref = "paper", x0 = 0, x1 = second_time, y0 = 0, y1 = 1, fillcolor = "grey", opacity = 0.2, line = list(width = 0)),
      list(type = "rect", xref = "x", yref = "paper", x0 = first_time, x1 = 24, y0 = 0, y1 = 1, fillcolor = "grey", opacity = 0.2, line = list(width = 0))
    )
  } else {
    list(list(type = "rect", xref = "x", yref = "paper", x0 = first_time, x1 = second_time, y0 = 0, y1 = 1, fillcolor = "grey", opacity = 0.2, line = list(width = 0)))
  }
  
  df |>
    dplyr::mutate(Year = factor(Year)) |>
    dplyr::filter(arrival_mode %in% outp[inp]) |>
    dplyr::group_by(Hour, Year) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
    dplyr::ungroup() |>
    dplyr::group_by(Year) |>
    dplyr::mutate(total_count = sum(count), percentage = (count / total_count) * 100) |>
    plotly::plot_ly(
      x = ~Hour, y = ~percentage, color = ~Year, colors = colfun(4), type = "scatter", mode = "lines+markers",
      text = ~Year,
      hoverinfo = "text",
      hoverlabel = list(namelength = 0),
      hovertemplate = paste(
        "On an average day in %{text}<br>",
        "%{y:.2f}% of patients arrived<br>",
        "between %{x}:00 and %{x}:59"
      )
    ) |>
    plotly::layout(
      shapes = shapes_list,
      height = 250,
      xaxis = list(range = list(rg[1], rg[2]), title = "Hour of the Day", titlefont = list(size = 16), tickfont = list(size = 12), showgrid = FALSE),
      yaxis = list(title = "Average Daily Percentage", hoverformat = ".2f", titlefont = list(size = 16), tickfont = list(size = 12), range = c(0, max(df$percentage, na.rm = TRUE) + 15), showgrid = FALSE),
      legend = list(orientation = "v", entrywidth = 70, yanchor = "top", y = 1, xanchor = "left", x = 0.1, font = list(size = 12)),
      margin = list(l = 50, r = 50, t = 50, b = 50),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
}

#' weekly_arrivals
#'
#' @description Weekly or daily average arrivals by arrival_mode and year.
#' @param df Data frame with `Hour`, `weekdate`, and `arrival_mode`.
#' @param timerange Character vector (e.g. c("0:00","23:00"))
#' @param input Shiny input object containing `arrival_freq`
#' @export
weekly_arrivals <- function(df, timerange = c("0:00","23:00"), input) {
  rg <- as.numeric(lubridate::hm(timerange)) / 3600
  if (rg[2] < 23) rg[2] <- rg[2] + 1
  
  filter_by_time <- function(data, start_hour, end_hour) {
    if (end_hour >= start_hour) {
      dplyr::filter(data, Hour >= start_hour, Hour <= end_hour)
    } else {
      dplyr::filter(data, Hour >= start_hour | Hour <= end_hour)
    }
  }
  
  if (input$arrival_freq == "Weekly") {
    df2 <- df |>
      filter_by_time(start_hour = rg[1], end_hour = rg[2]) |>
      dplyr::group_by(arrival_mode, weekdate) |>
      dplyr::count() |>
      dplyr::ungroup() |>
      dplyr::mutate(Year = as.character(lubridate::year(weekdate))) |>
      dplyr::group_by(arrival_mode, Year) |>
      dplyr::summarise(av_count = mean(n, na.rm = TRUE), .groups = "drop")
  } else {
    all_dates <- seq(min(df$date), max(df$date), by = "day")
    all_dates_df <- base::expand.grid(date = all_dates, arrival_mode = unique(df$arrival_mode))
    
    df2 <- df |>
      filter_by_time(start_hour = rg[1], end_hour = rg[2]) |>
      dplyr::group_by(arrival_mode, date) |>
      dplyr::count() |>
      dplyr::ungroup() |>
      dplyr::right_join(all_dates_df, by = c("arrival_mode", "date")) |>
      dplyr::mutate(n = ifelse(is.na(n), 0, n)) |>
      dplyr::mutate(Year = as.character(lubridate::year(date))) |>
      dplyr::group_by(arrival_mode, Year) |>
      dplyr::summarise(av_count = mean(n, na.rm = TRUE), .groups = "drop")
  }
  
  df2 |>
    dplyr::filter(arrival_mode %in% c("Ground Ambulance", "No Ambulance"), Year != "2020") |>
    tidyr::pivot_wider(names_from = arrival_mode, values_from = av_count)
}

#' edLoad
#'
#' @description Load ED visit data from file or Google Drive
#' @param fname File name or path
#' @param from.google Logical, whether to load from Google Drive
#' @export
edLoad <- function(fname, from.google = FALSE) {
  # if(grepl('sparwood', fname, ignore.case = TRUE)){browser()}
  if (from.google) {
    df <- load_drive_csv_in_folder(fname, folder = "Tarmac Dash ED Data")
  } else {
    df <- data.table::fread(fname, colClasses = list(character = "arrival_datetime"))
  }
  df |>
    dplyr::mutate(
      datetime = parse_ed_time(arrival_datetime),
      date = lubridate::as_date(datetime),
      weekdate = weekdate(date),
      Hour = lubridate::hour(datetime),
      time = strftime(datetime, format = "%H:%M:%S"),
      Year = lubridate::year(date)
    )
}
