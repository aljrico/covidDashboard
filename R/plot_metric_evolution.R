#' Plot Metric Evolution
#' 
#' @noRd
#' @export
plot_metric_evolution <- function(data, variable){
  
  hover_text <- function(value, variable, d) {
    if (variable == "confirmed_cases") variable_text <- "Infected: "
    if (variable == "confirmed_deaths") variable_text <- "Deaths: "
    if (variable == "confirmed_recovered") variable_text <- "Recovered: "
    value_text <- formatC(value, format = "f", big.mark = ",", digits = 0)
    paste0(
      "</br> <b>", variable_text, "</b>", value_text,
      "</br> <b>", "Date: ", "</b>", d
    )
  }
  
  write_title <- function(variable) {
    if (variable == "confirmed_cases") title <- "Confirmed Cases"
    if (variable == "confirmed_deaths") title <- "Deaths"
    if (variable == "confirmed_recovered") title <- "Recovered"
    title
  }
  data <- data.table::copy(data)
  data.table::setnames(data, old = variable, new = "value")
  
  data %>%
    dplyr::group_by(Date) %>%
    dplyr::summarise(result = sum(as.numeric(value), na.rm = TRUE)) %>%
    dplyr::mutate(Date = lubridate::ymd(Date)) %>%
    plotly::plot_ly(
      type = "scatter", mode = "lines", fill = "tozeroy",
      hoverinfo = "text",
      text = ~ hover_text(result, variable, Date)
    ) %>%
    plotly::add_trace(
      x = ~Date, y = ~result,
      line = list(
        color = pick_colour(variable, rgb = TRUE),
        width = 4
      ),
      fillcolor = pick_colour(variable, rgb = TRUE, alpha = 0.8)
    ) %>%
    plotly::layout(
      showlegend = FALSE,
      title = paste0("Total ", write_title(variable)),
      xaxis = list(title = '', fixedrange = TRUE, showgrid = FALSE),
      yaxis = list(title = '', fixedrange = TRUE, showgrid = FALSE),
      plot_bgcolor = "#FAFAFA",
      paper_bgcolor = "#FAFAFA",
      fig_bgcolor = "#FAFAFA"
    ) %>% 
    plotly::config(displayModeBar = FALSE)
}