#' country_modal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_country_modal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        actionButton("back_button", " Back to worldwide view", icon = icon("globe-americas"))
      )
    ),
    br(),
    br(),
    uiOutput(ns("value_boxes")),
    fluidRow(
      shinydashboard::box(
        solidHeader = TRUE,
        width = 4,
        plotly::plotlyOutput(ns("total_cases"))
      ),
      shinydashboard::box(
        solidHeader = TRUE,
        width = 4,
        plotly::plotlyOutput(ns("total_deaths"))
      ),
      shinydashboard::box(
        solidHeader = TRUE,
        width = 4,
        plotly::plotlyOutput(ns("total_tests"))
      )
    ),
    fluidRow(
      shinydashboard::box(
        solidHeader = TRUE,
        width = 4,
        plotly::plotlyOutput(ns("daily_cases"))
      ),
      shinydashboard::box(
        solidHeader = TRUE,
        width = 4,
        plotly::plotlyOutput(ns("daily_deaths"))
      ),
      shinydashboard::box(
        solidHeader = TRUE,
        width = 4,
        plotly::plotlyOutput(ns("daily_tests"))
      )
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
    dat <- data.table::setnames(dat, old = "location", new = "Country")
    dat <- dat %>% dplyr::filter(country_code == rv$selected_country)
    
    output$value_boxes <- renderUI({
      
      value_cases <- sum(as.numeric(dat$cases_change), na.rm = TRUE) %>% formatC(format = "f", big.mark = ",", digits = 0)
      value_deaths <- sum(as.numeric(dat$deaths_change), na.rm = TRUE) %>% formatC(format = "f", big.mark = ",", digits = 0)
      value_tests <- sum(as.numeric(dat$tests_change), na.rm = TRUE) %>% formatC(format = "f", big.mark = ",", digits = 0)
      
      
      tests_box <- shinydashboard::valueBox(value = value_tests, subtitle = "Total tests", color = "blue", icon = icon('tablets'), width = 4)
      cases_box <- shinydashboard::valueBox(value = value_cases, subtitle = "Total Cases", color = "orange", icon = icon('syringe'), width = 4)
      deaths_box <- shinydashboard::valueBox(value = value_deaths, subtitle = "Total Deaths", color = "red", icon = icon('skull'), width = 4)
      
      # tests_box$children[[1]]$attribs$class <- paste0(tests_box$children[[1]]$attribs$class, " action-button")
      # tests_box$children[[1]]$attribs$id <- session$ns("tests_button")
      
      tagList(
       cases_box,
       deaths_box,
       tests_box
      )
      
    })

    
    output$total_cases <- plotly::renderPlotly({plot_metric_evolution(dat, variable = "confirmed_cases")})
    output$total_deaths <- plotly::renderPlotly({plot_metric_evolution(dat, variable = "confirmed_deaths")})
    output$total_tests <- plotly::renderPlotly({plot_metric_evolution(dat, variable = "total_tests")})
    
    output$daily_cases <- plotly::renderPlotly({plot_metric_daily(dat, variable = "confirmed_cases")})
    output$daily_deaths <- plotly::renderPlotly({plot_metric_daily(dat, variable = "confirmed_deaths")})
    output$daily_tests <- plotly::renderPlotly({plot_metric_daily(dat, variable = "total_tests")})
    
    country_name <- dat$Country[[1]]
    # showModal(country_modal(country_name))
    
  })
  
}
    
## To be copied in the UI
# mod_country_modal_ui("country_modal_ui_1")
    
## To be copied in the server
# callModule(mod_country_modal_server, "country_modal_ui_1")
 
