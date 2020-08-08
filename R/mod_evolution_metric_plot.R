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
  
  output$line_plot <- 
    plotly::renderPlotly({
      dat <- data.table::data.table(rv$daily_country)
      data.table::setnames(dat, old = "location", new = "Country")
      if (!is.null(country)) dat <- dat[Country == country]
      plot_metric_evolution(dat, variable)
    })
}

## To be copied in the UI
# mod_evolution_metric_plot_ui("evolution_metric_plot_ui_1")

## To be copied in the server
# callModule(mod_evolution_metric_plot_server, "evolution_metric_plot_ui_1")
