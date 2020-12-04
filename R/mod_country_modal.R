#' Country Details Object
#'
#' @noRd
#' @export
CountryDetails <-
  R6::R6Class(
    "CountryDetails",
    public = list(
      ui = function(id){
        
        daily_plots <- function(){
          ui_pieces <- lapply(private$variables, function(v){
            shinydashboard::box(
              solidHeader = TRUE,
              width = 4,
              shinyWidgets::radioGroupButtons(
                inputId = ns(glue::glue("button_timeframe_daily_{v}")),
                choices = c("Last 90 days", "Full Range"),
                width = "250%"
                ),
              plotly::plotlyOutput(ns(glue::glue("daily_{v}")))
            )
          })
          do.call(tagList, ui_pieces)
        }
        accumulated_plots <- function(){
          ui_pieces <- lapply(private$variables, function(v){
            shinydashboard::box(
              solidHeader = TRUE,
              width = 4,
              plotly::plotlyOutput(ns(glue::glue("accumulated_{v}")))
            )
          })
          do.call(tagList, ui_pieces)
        }
        back_button <- function(){
          actionButton("back_button", "  Back", icon = icon("globe-americas"), class = "back-button")
        }
        
        ns <- NS(id)
        tagList(
          fluidRow(
            column(11, uiOutput(ns("country_title"))), column(1, back_button())
          ),
          br(),
          uiOutput(ns("value_boxes")),
          fluidRow(
            accumulated_plots()
          ),
          fluidRow(
            daily_plots()
          )
        )
      },
      server = function(rv) callModule(private$init_server, "country_modal", rv)
    ),
    private = list(
      variables = c("confirmed_cases", "confirmed_deaths", "total_tests"),
      init_server = function(input, output, session, rv){
        ns <- session$ns
        
        private$switch_listener()
        
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
        
        # Render Accumulated Plots
        lapply(private$variables, function(v){
          output_name <- paste0("accumulated_", v)
            output[[output_name]] <- plotly::renderPlotly({
              plot_metric_evolution(country_data(), variable = v)
            })
        })
        
        # Render Daily Plots
        lapply(private$variables, function(v){
          output_name <- paste0("daily_", v)
          output[[output_name]] <- plotly::renderPlotly({
            variablePlotter <- PlotterDaily$new(variable = v)
            variablePlotter$ingest_data(country_data())
            variablePlotter$plot()
          })
        })

        output$country_title <- shiny::renderUI({
          h2(country_data()$location[1], style = "padding-left: 45px;")
        })
      },
      switch_listener = function(){
        shinyServer(function(input, output, session){
          lapply(private$variables, function(v){
            observeEvent(input[[ns(glue::glue("button_timeframe_daily_{v}"))]], {
              print(v)
            })
        })
        })
      }
    )
  )
