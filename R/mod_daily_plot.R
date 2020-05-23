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
 
  
  hover_text <- function(value, variable) {
    if (variable == "confirmed_cases") variable_text <- "Infected: "
    if (variable == "confirmed_deaths") variable_text <- "Deaths: "
    if (variable == "confirmed_recovered") variable_text <- "Recovered: "
    value_text <- formatC(value, format = "f", big.mark = ",", digits = 0)
    paste0("</br> <b>", variable_text, "</b>", value_text)
  }
  
  write_title <- function(variable) {
    if (variable == "confirmed_cases") title <- "Confirmed Cases"
    if (variable == "confirmed_deaths") title <- "Deaths"
    if (variable == "confirmed_recovered") title <- "Recovered"
    title
  }
  
  output$daily_plot <- 
    plotly::renderPlotly({
      dat <- data.table::data.table(rv$daily_country)
      data.table::setnames(dat, old = "Country/Region", new = "Country")
      if (!is.null(country)) dat <- dat[Country == country]
      
      data.table::setnames(dat, old = variable, new = "value")
      
      dat %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(result = sum(as.numeric(value), na.rm = TRUE)) %>%
        dplyr::mutate(Date = lubridate::ymd(Date)) %>%
        dplyr::ungroup() %>% 
        dplyr::mutate(change = (result - dplyr::lag(result))) %>% 
        na.omit() %>% 
        plotly::plot_ly(
          type = "bar",
          hoverinfo = "text",
          text = ~ hover_text(change, variable)
        ) %>%
        plotly::add_trace(
          x = ~Date, y = ~change,
          marker = list(
            color = pick_colour(variable, rgb = TRUE)
          )
        ) %>%
        plotly::layout(
          showlegend = FALSE,
          title = paste0("Daily ", write_title(variable)),
          xaxis = list(title = '', fixedrange = TRUE),
          yaxis = list(title = '', fixedrange = TRUE),
          plot_bgcolor = "#FAFAFA"
        ) %>% 
        plotly::config(displayModeBar = FALSE)
    })
  
}
    
## To be copied in the UI
# mod_daily_plot_ui("daily_plot_ui_1")
    
## To be copied in the server
# callModule(mod_daily_plot_server, "daily_plot_ui_1")
 
