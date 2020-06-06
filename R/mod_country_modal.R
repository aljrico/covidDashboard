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
      uiOutput(ns('value_boxes'))
    ),
    fluidRow(
      column(4, plotly::plotlyOutput(ns('total_cases'))),
      column(4, plotly::plotlyOutput(ns('total_deaths'))),
      column(4, plotly::plotlyOutput(ns('total_recovered'))),
    ),
    fluidRow(
      column(4, plotly::plotlyOutput(ns('daily_cases'))),
      column(4, plotly::plotlyOutput(ns('daily_deaths'))),
      column(4, plotly::plotlyOutput(ns('daily_recovered'))),
    )
  )
}
    
#' country_modal Server Function
#'
#' @noRd 
mod_country_modal_server <- function(input, output, session, rv){
  ns <- session$ns
  
  country_modal <- function(country){
    ns <- session$ns
    
     modalDialog(
      title = country,
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
    
    output$value_boxes <- renderUI({
      
      value_cases <- sum(as.numeric(dat$cases_change), na.rm = TRUE) %>% formatC(format = "f", big.mark = ",", digits = 0)
      value_deaths <- sum(as.numeric(dat$deaths_change), na.rm = TRUE) %>% formatC(format = "f", big.mark = ",", digits = 0)
      value_recovered <- sum(as.numeric(dat$recovered_change), na.rm = TRUE) %>% formatC(format = "f", big.mark = ",", digits = 0)
      
      
      recovered_box <- shinydashboard::valueBox(value = value_recovered, subtitle = "Total Recovered", color = "blue", icon = icon('tablets'), href = "#")
      recovered_box$children[[1]]$attribs$class <- paste0(recovered_box$children[[1]]$attribs$class, " action-button")
      recovered_box$children[[1]]$attribs$id <- session$ns("recovered_button")
      
      tagList(
        shinydashboard::valueBox(value = value_cases, subtitle = "Total Cases", color = "orange", icon = icon('syringe')),
        shinydashboard::valueBox(value = value_deaths, subtitle = "Total Deaths", color = "red", icon = icon('skull')),
        recovered_box
      )
      
    })
    
    observeEvent(input$recovered_button, {
      print('YAY')
    })

    
    output$total_cases <- plotly::renderPlotly({plot_metric_evolution(dat, variable = "confirmed_cases")})
    output$total_deaths <- plotly::renderPlotly({plot_metric_evolution(dat, variable = "confirmed_deaths")})
    output$total_recovered <- plotly::renderPlotly({plot_metric_evolution(dat, variable = "confirmed_recovered")})
    
    output$daily_cases <- plotly::renderPlotly({plot_metric_daily(dat, variable = "confirmed_cases")})
    output$daily_deaths <- plotly::renderPlotly({plot_metric_daily(dat, variable = "confirmed_deaths")})
    output$daily_recovered <- plotly::renderPlotly({plot_metric_daily(dat, variable = "confirmed_recovered")})
    
    country_name <- dat$Country[[1]]
    showModal(country_modal(country_name))
    
  })
}
    
## To be copied in the UI
# mod_country_modal_ui("country_modal_ui_1")
    
## To be copied in the server
# callModule(mod_country_modal_server, "country_modal_ui_1")
 
