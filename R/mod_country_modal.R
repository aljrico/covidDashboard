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
    fluidRow(uiOutput(ns("country_title"))),
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
mod_country_modal_server <- function(input, output, session, rv) {
  ns <- session$ns

  country_data <- reactive({
    req(rv$selected_country)
    if (!is.null(rv$selected_country)) {
      dat <- data.table::copy(rv$daily_country)
      data.table::setDT(dat)
      dat <- dat[country_code == rv$selected_country]

      dat[, first_everything := confirmed_cases + confirmed_deaths]
      first_day <- min(dat[first_everything > 0, Date], na.rm = TRUE)
      dat <- dat[Date >= first_day]
      dat[, first_everything := NULL]
      return(dat)
    }
  })

  output$value_boxes <- renderUI({
    value_cases <- country_data()[Date == max(Date)]$confirmed_cases %>% formatC(format = "f", big.mark = ",", digits = 0)
    value_deaths <- country_data()[Date == max(Date)]$confirmed_deaths %>% formatC(format = "f", big.mark = ",", digits = 0)
    value_tests <- country_data()[Date == max(Date)]$total_tests %>% formatC(format = "f", big.mark = ",", digits = 0)

    tests_box <- shinydashboard::valueBox(value = value_tests, subtitle = "Total tests", color = "blue", icon = icon("tablets"), width = 4)
    cases_box <- shinydashboard::valueBox(value = value_cases, subtitle = "Total Cases", color = "orange", icon = icon("syringe"), width = 4)
    deaths_box <- shinydashboard::valueBox(value = value_deaths, subtitle = "Total Deaths", color = "red", icon = icon("skull"), width = 4)

    tagList(
      cases_box,
      deaths_box,
      tests_box
    )
  })
  
  casePlotter <- PlotterDaily$new(variable = "confirmed_cases")
  deathsPlotter <- PlotterDaily$new(variable = "confirmed_deaths")
  testsPlotter <- PlotterDaily$new(variable = "total_tests")
  
  output$total_cases <- plotly::renderPlotly({
    plot_metric_evolution(country_data(), variable = "confirmed_cases")
  })
  output$total_deaths <- plotly::renderPlotly({
    plot_metric_evolution(country_data(), variable = "confirmed_deaths")
  })
  output$total_tests <- plotly::renderPlotly({
    plot_metric_evolution(country_data(), variable = "total_tests")
  })
  output$daily_cases <- plotly::renderPlotly({
    casePlotter$ingest_data(country_data())
    casePlotter$plot()
  })
  output$daily_deaths <- plotly::renderPlotly({
    deathsPlotter$ingest_data(country_data())
    deathsPlotter$plot()
  })
  output$daily_tests <- plotly::renderPlotly({
    testsPlotter$ingest_data(country_data())
    testsPlotter$plot()
  })
  output$country_title <- shiny::renderUI({
    h2(country_data()$location[1], style = "padding-left: 45px;")
  })
}

#' Country Details Object
#'
#' @noRd
#' @export
CountryDetails <-
  R6::R6Class(
    "CountryDetails",
    public = list(
      ui = function(id) mod_country_modal_ui(id),
      init_server = function(rv) callModule(mod_country_modal_server, "country_modal", rv)
    ),
    private = list(
      server = mod_country_modal_server
    )
  )
