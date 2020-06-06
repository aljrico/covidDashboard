#' country_modal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_country_modal_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, plotly::plotlyOutput(ns('confirmed_cases'))),
      column(4, plotly::plotlyOutput(ns('confirmed_deaths'))),
      column(4, plotly::plotlyOutput(ns('confirmed_recovered'))),
    )
  )
}
    
#' country_modal Server Function
#'
#' @noRd 
mod_country_modal_server <- function(input, output, session, rv){
  ns <- session$ns
  
  modal_ui <- function(){
    fluidRow(
      column(4, plotly::plotlyOutput(ns('confirmed_cases'))),
      column(4, plotly::plotlyOutput(ns('confirmed_deaths'))),
      column(4, plotly::plotlyOutput(ns('confirmed_recovered'))),
    )
  }
  
  country_modal <- function(country){
    ns <- session$ns
    
     modalDialog(
      title = paste0(country, " data"),
      mod_country_modal_ui("country_modal"),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  }
  
  observeEvent(rv$selected_country, {
    dat <- data.table::copy(rv$daily_country)
    dat <- data.table::setnames(dat, old = "Country/Region", new = "Country")
    dat <- dat %>% dplyr::filter(country_code == rv$selected_country)

    variables <- c('confirmed_cases', 'confirmed_deaths', 'confirmed_recovered')
    
    # for(v in variables){
    #   print(v)
    #   output[[v]] <- 
    #     plotly::renderPlotly({
    #       plot_metric_evolution(dat, variable = v)
    #     })
    # }
    
    output$confirmed_cases <- plotly::renderPlotly({plot_metric_evolution(dat, variable = "confirmed_cases")})
    output$confirmed_deaths <- plotly::renderPlotly({plot_metric_evolution(dat, variable = "confirmed_deaths")})
    output$confirmed_recovered <- plotly::renderPlotly({plot_metric_evolution(dat, variable = "confirmed_recovered")})
    
    country_name <- dat$Country[[1]]
    showModal(country_modal(country_name))
    
  })
}
    
## To be copied in the UI
# mod_country_modal_ui("country_modal_ui_1")
    
## To be copied in the server
# callModule(mod_country_modal_server, "country_modal_ui_1")
 
