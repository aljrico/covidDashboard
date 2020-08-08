#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  rv <- reactiveValues()
  # rv$selected_variable <- "infected"

  # Load Data
  observe({
    waiter::show_waiter(waiter::spin_folding_cube(), color = global$colours$dark, "Loading data ...")
    
    # Prepare Data
    global_data <- data_handler$new()
    
    # Processed Data
    rv$world_data <- global_data$world_data
    rv$daily_country <- global_data$daily_country
    rv$total_country <- global_data$total_country
    rv$map_data <- global_data$map_data

    waiter::hide_waiter()
  })

  observe({
    if(!is.null(input$metric_button)) rv$selected_variable <- input$metric_button
  })
  
  observeEvent(rv$selected_country, {
    shinyjs::hide("worldwide_view", animType = "fade", time = 0.5)
    shinyjs::show("country_view", animType = "fade", time = 0.5)
  })
  
  observeEvent(input$back_button, {
    session$sendCustomMessage(type = "resetValue", message = "cloropleth-cloropleth_shape_click")
    rv$selected_country <- NULL
    
    shinyjs::hide("country_view", animType = "fade", time = 0.5)
    shinyjs::show("worldwide_view", animType = "fade", time = 0.5)
  })

  callModule(mod_total_cases_server, "total_cases_country", rv$total_country, "total_cases")
  callModule(mod_total_cases_server, "total_deaths_country", rv$total_country, "total_deaths")
  callModule(mod_total_cases_server, "total_recovered_country", rv$total_country, "total_tests")
  callModule(mod_select_buttons_server, "select_buttons", rv, global)
  callModule(mod_evolution_metric_plot_server, 'total_lineplot_cases', rv, country = NULL, global, variable = "confirmed_cases")
  callModule(mod_evolution_metric_plot_server, 'total_lineplot_deaths', rv, country = NULL, global, variable = "confirmed_deaths")
  callModule(mod_evolution_metric_plot_server, 'total_lineplot_recovered', rv, country = NULL, global, variable = "confirmed_recovered")
  callModule(mod_daily_plot_server, 'dailyplot_cases', rv, country = NULL, global, variable = "confirmed_cases")
  callModule(mod_daily_plot_server, 'dailyplot_deaths', rv, country = NULL, global, variable = "confirmed_deaths")
  callModule(mod_daily_plot_server, 'dailyplot_recovered', rv, country = NULL, global, variable = "confirmed_recovered")
  callModule(mod_cloropleth_server, "cloropleth", rv, global)
  callModule(mod_total_table_server, "left_table", rv)
  callModule(mod_country_modal_server, "country_modal", rv)
  
  observe({
    callModule(mod_daily_table_server, "right_table", rv)
    callModule(mod_header_server, "header", rv)
  })
}
