#' total_cases UI Function
#'
#' @description This module intends to display a col bar with the total numbers, per country, of deaths, infected or recovered cases.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_total_cases_ui <- function(id) {
  ns <- NS(id)
    plotly::plotlyOutput(ns("total_cases_country"))
}

#' total_cases Server Function
#'
#' @noRd
mod_total_cases_server <- function(input, output, session, total_country, variable) {
  ns <- session$ns
  
  hover_text <- function(value, variable){
    if(variable == 'confirmed_cases') variable_text <- 'Infected: '
    if(variable == 'confirmed_deaths') variable_text <- 'Deaths: '
    if(variable == 'confirmed_recovered') variable_text <- 'Recovered: '
    value_text <- formatC(value, format="f", big.mark=",", digits=0)
    paste0('</br> <b>', variable_text, '</b>', value_text)
  }
  
  write_title <- function(variable){
    if(variable == 'confirmed_cases') title <- 'Confirmed Cases'
    if(variable == 'confirmed_deaths') title <- 'Deaths'
    if(variable == 'confirmed_recovered') title <- 'Recovered'
    title
  }

  observe({
    output$total_cases_country <-
      plotly::renderPlotly({
        total_country %>%
          dplyr::select(c(!!variable, `Country/Region`)) %>% 
          dplyr::ungroup() %>% 
          dplyr::rename(value = !!variable) %>% 
          dplyr::top_n(13, value) %>%  
          plotly::plot_ly(y = ~ reorder(`Country/Region`, value), 
                          x = ~value, orientation = "h",
                          hoverinfo = 'text', 
                          text = ~hover_text(value, variable)) %>%
          plotly::add_bars(marker = list(
            color = pick_colour(variable, 0.8),
            line = list(color = pick_colour(variable, 1), width = 2)
          )) %>%
          plotly::config(displayModeBar = F) %>%
          plotly::layout(
            xaxis = list(title = "", fixedrange = TRUE, showgrid = FALSE),
            yaxis = list(title = "", fixedrange = TRUE, showgrid = FALSE),
            title = write_title(variable)
          ) %>% 
          plotly::layout(
            plot_bgcolor = "#FAFAFA",
            paper_bgcolor = "#FAFAFA"
          )
      })
  })
}
