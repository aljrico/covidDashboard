#' total_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_total_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('total_table'))
  )
}

#' total_table Server Function
#'
#' @noRd
mod_total_table_server <- function(input, output, session, rv) {
  ns <- session$ns

  
  output$total_table <- renderUI({
    
    total_country <- rv$total_country
    
    
    if (rv$selected_variable == "infected") {
      title <- "Confirmed Cases"
      column <- "confirmed_cases"
    } else if (rv$selected_variable == "deaths") {
      title <- "Deaths"
      column <- "confirmed_deaths"
    } else if (rv$selected_variable == "recovered") {
      title <- "Recovered"
      column <- "confirmed_recovered"
    }
    
    total_country$variable <- total_country[[column]]
    
    top_variables <- 
      total_country %>%
      dplyr::group_by(`Country/Region`) %>%
      dplyr::summarise(variable = sum(variable, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::top_n(n = 10, wt = variable) %>%
      dplyr::arrange(desc(variable)) 
    
    tagList(
      # shinydashboard::valueBox(value = paste0('United States: ', prettyNum(1e5, big.interval = 3L)), subtitle = "")
      column(6, align = "center", tags$b("United States: ")),
      column(6, align = "left", tags$p("1,000,000"))
    )
  })

  # 
  # total_country %>%
  #   dplyr::group_by(`Country/Region`) %>%
  #   dplyr::summarise(variable = sum(variable, na.rm = TRUE)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::top_n(n = 10, wt = variable) %>%
  #   dplyr::arrange(desc(variable)) %>%
  #   DT::datatable(
  #     rownames = FALSE,
  #     options = list(
  #       headerCallback = DT::JS(
  #         "function(thead, data, start, end, display){",
  #         "  $(thead).remove();",
  #         "}")
  #     )
  #   ) %>%
  #   DT::formatStyle(
  #     columns = c("variable", "Country/Region"),
  #     # color = rv$colours$dark,
  #     color = "#1F2430",
  #     # backgroundColor = rv$colours$grey
  #     backgroundColor = "#FAFAFA"
  #   )
}