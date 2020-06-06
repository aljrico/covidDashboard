#' daily_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_daily_plot_ui <- function(id){
  ns <- NS(id)
  plotly::plotlyOutput(ns("daily_plot"))
}
    
#' daily_plot Server Function
#'
#' @noRd 
mod_daily_plot_server <- function(input, output, session, rv, country = NULL, global, variable = "confirmed_cases"){
  ns <- session$ns
  
  output$daily_plot <- 
    plotly::renderPlotly({
      dat <- data.table::data.table(rv$daily_country)
      data.table::setnames(dat, old = "Country/Region", new = "Country")
      if (!is.null(country)) dat <- dat[Country == country]
      plot_metric_daily(dat, variable)
    })
  
}
    
## To be copied in the UI
# mod_daily_plot_ui("daily_plot_ui_1")
    
## To be copied in the server
# callModule(mod_daily_plot_server, "daily_plot_ui_1")
 
