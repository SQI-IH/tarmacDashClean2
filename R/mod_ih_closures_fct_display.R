#' Generate closure occurrence plot
#'
#' @param df A data frame with closure records
#' @return A plotly scatter plot object
#' @export
occurencePlot <- function(df) {
  color_palette <- grDevices::colorRampPalette(c("#005AA2", "#00C1D5", "#FFC72C", "#DE5428"))
  plotly::plot_ly(df) |>
    plotly::add_trace(
      x = ~start_date,
      y = ~hospital_city,
      type = "scatter",
      mode = "markers",
      color = ~hospital_city,
      colors = color_palette(length(unique(df$hospital_city))),
      text = ~paste(
        start_date, "<br>", hospital_city, "<br>",
        diversion_reason, "<br>", diversion_type
      ),
      hoverinfo = "text"
    ) |>
    plotly::layout(
      showlegend = FALSE,
      xaxis = list(title = ""),
      yaxis = list(title = "")
    ) |>
    plotly::config(
      toImageButtonOptions = list(
        format = "png",
        filename = "Closure Incidence",
        height = 500,
        width = 1000,
        scale = 2
      )
    )
}

#' Generate cumulative closures plot
#'
#' @param df A data frame with closure records
#' @return A plotly line chart object
#' @export
cumOccurencePlot <- function(df) {
  color_palette <- grDevices::colorRampPalette(c("#005AA2", "#00C1D5", "#FFC72C", "#DE5428"))
  
  df_cum <- df |>
    dplyr::mutate(event = 1) |>
    dplyr::group_by(hospital_city) |>
    dplyr::mutate(cumulative = cumsum(event))
  
  plotly::plot_ly(df_cum) |>
    plotly::add_trace(
      x = ~start_date,
      y = ~cumulative,
      type = "scatter",
      mode = "lines",
      color = ~hospital_city,
      colors = color_palette(length(unique(df$hospital_city))),
      text = ~paste(
        start_date, "<br>", hospital_city, "<br>", cumulative,
        "<br>", diversion_type
      ),
      hoverinfo = "text"
    ) |>
    plotly::layout(
      showlegend = FALSE,
      xaxis = list(title = ""),
      yaxis = list(title = "")
    ) |>
    plotly::config(
      toImageButtonOptions = list(
        format = "png",
        filename = "Closure Cumulative",
        height = 500,
        width = 1000,
        scale = 2
      )
    )
}

#' Generate closure type plot
#'
#' @param df A data frame with closure records
#' @return A plotly stacked bar chart object
#' @export
typePlot <- function(df) {
  color_palette <- grDevices::colorRampPalette(c("#005AA2", "#FFC72C", "#DE5428"))
  
  df_grouped <- df |>
    dplyr::mutate(
      reason_condensed = dplyr::case_when(
        stringr::str_detect(diversion_reason, stringr::regex("phys", ignore_case = TRUE)) ~ "Physician",
        stringr::str_detect(diversion_reason, stringr::regex("md", ignore_case = TRUE)) ~ "Physician",
        stringr::str_detect(diversion_reason, stringr::regex("nurs", ignore_case = TRUE)) ~ "Nurse",
        TRUE ~ "Other"
      )
    ) |>
    dplyr::group_by(hospital_city, reason_condensed) |>
    dplyr::count()
  color_palette <- colorRampPalette(c("#005AA2", "#FFC72C","#DE5428" ))
  
  plotly::plot_ly(df_grouped) |>
    plotly::add_trace(
      x = ~hospital_city,
      y = ~n,
      type = "bar",
      color = ~reason_condensed,
      colors = color_palette(3),
      text = ~paste(n, "closures in", hospital_city, "<br>Reason:", reason_condensed),
      hoverinfo = "text",
      textposition = "none"
    ) |>
    plotly::layout(
      showlegend = FALSE,
      xaxis = list(title = "", categoryorder = "total descending"),
      yaxis = list(title = "# of Closures"),
      barmode = "stack"
    ) |>
    plotly::config(
      toImageButtonOptions = list(
        format = "png",
        filename = "Closure Types",
        height = 500,
        width = 1000,
        scale = 2
      )
    )
}

#' Generate closure reports datatable
#'
#' @param df A data frame with closure records
#' @return A datatable object
#' @export
allReports <- function(df) {
  df |>
    dplyr::mutate(
      start = lubridate::ymd_hm(paste(start_date, start_time)),
      end = lubridate::ymd_hm(paste(anticipated_end_date, end_time))
    ) |>
    dplyr::select(
      start_date, hospital_city, hospital_name,
      primary_hos, secondary_hos, diversion_type, diversion_reason
    ) |>
    dplyr::arrange(dplyr::desc(start_date)) |>
    dplyr::rename(
      "Start" = start_date,
      "city" = hospital_city,
      "Hospital" = hospital_name,
      "Primary" = primary_hos,
      "Secondary" = secondary_hos,
      "Diversion Type" = diversion_type,
      "Diversion Reason" = diversion_reason
    ) |>
    DT::datatable(
      options = list(
        pageLength = 5,
        autoWidth = TRUE,
        dom = "p",
        scrollX = TRUE,
        columnDefs = list(
          list(width = "60px", targets = c(1, 5)),
          list(width = "80px", targets = c(2)),
          list(width = "110px", targets = c(4, 6, 7)),
          list(width = "130px", targets = c(3))
        )
      )
    )
}

#' Create a Leaflet map of hospital closures
#'
#' @param df A data frame with closures and coordinates
#' @return A leaflet map
#' @export
create_incidence_map <- function(df) {
  leaflet::leaflet(df) |>
    leaflet::addTiles() |>
    leaflet::addCircleMarkers(
      lng = ~Longitude,
      lat = ~Latitude,
      radius = ~sqrt(n) * 2,
      popup = ~paste(hospital_city, "<br>", "Closures since July 2024: ", n),
      color = "blue",
      fillOpacity = 0.5
    )
}
