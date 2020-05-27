#' evolution_metric_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_evolution_metric_plot_ui <- function(id) {
  ns <- NS(id)
  plotly::plotlyOutput(ns("line_plot"))
}

#' evolution_metric_plot Server Function
#'
#' @noRd
mod_evolution_metric_plot_server <- function(input, output, session, rv, country = NULL, global, variable = "confirmed_cases") {
  ns <- session$ns

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
  
  output$line_plot <- 
    plotly::renderPlotly({
      dat <- data.table::data.table(rv$daily_country)
      data.table::setnames(dat, old = "Country/Region", new = "Country")
      if (!is.null(country)) dat <- dat[Country == country]
      
      data.table::setnames(dat, old = variable, new = "value")
      
      dat %>%
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
          xaxis = list(title = '', fixedrange = TRUE),
          yaxis = list(title = '', fixedrange = TRUE),
          plot_bgcolor = "#FAFAFA"
        ) %>% 
        plotly::config(displayModeBar = FALSE)
    })
}

## To be copied in the UI
# mod_evolution_metric_plot_ui("evolution_metric_plot_ui_1")

## To be copied in the server
# callModule(mod_evolution_metric_plot_server, "evolution_metric_plot_ui_1")
