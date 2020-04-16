#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  rv <- reactiveValues()
  rv$selected_variable <- "infected"
  rv$colours <-
    list(
      grey = "#FAFAFA",
      red = "#F07171",
      orange = "#FF9940",
      green = "#BAE67E",
      blue = "#5CCFE6"
    )

  # Load Data
  observe({
    waiter::show_waiter(waiter::spin_folding_cube())
    
    # Raw Data
    confirmed_ts <- load_confirmed()
    death_ts <- load_deaths()
    recovered_ts <- load_recovered()
    
    # Processed Data
    rv$daily_country <- get_daily_country(confirmed_ts, death_ts, recovered_ts)
    rv$total_country <- get_total_country(confirmed_ts, death_ts, recovered_ts)
    rv$map_data <- get_map_data(rv$total_country)
    
    waiter::hide_waiter()
  })

  observeEvent(input$select_infected, {
    rv$selected_variable <- "infected"
  })

  observeEvent(input$select_deaths, {
    rv$selected_variable <- "deaths"
  })

  observeEvent(input$select_recovered, {
    rv$selected_variable <- "recovered"
  })


  callModule(mod_total_cases_server, "total_cases_country", rv$total_country, "confirmed_cases")
  callModule(mod_total_cases_server, "total_deaths_country", rv$total_country, "confirmed_deaths")
  callModule(mod_total_cases_server, "total_recovered_country", rv$total_country, "confirmed_recovered")

  observe({
    callModule(mod_select_buttons_server, "select_buttons", rv)
    callModule(mod_cloropleth_server, "cloropleth", rv)
    callModule(mod_total_table_server, "left_table", rv)
  })
}
