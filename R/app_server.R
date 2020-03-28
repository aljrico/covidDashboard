#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  rv <- reactiveValues()

  # Load Data
  observe({
    waiter::show_waiter(waiter::spin_folding_cube())
    confirmed_ts <- load_confirmed()
    death_ts <- load_deaths()
    recovered_ts <- load_recovered()

    rv$total_country <- get_total_country(confirmed_ts, death_ts, recovered_ts)
    waiter::hide_waiter()
  })

  observeEvent(rv$total_country, {
    callModule(mod_total_cases_server, "total_cases_country", rv$total_country, 'confirmed_cases')
    callModule(mod_total_cases_server, "total_deaths_country", rv$total_country, 'confirmed_deaths')
    callModule(mod_total_cases_server, "total_recovered_country", rv$total_country, 'confirmed_recovered')
    callModule(mod_cloropleth_server, 'cloropleth_infected', rv$total_country)
  })
}
