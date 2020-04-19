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
      id <- "confirmed"
    } else if (rv$selected_variable == "deaths") {
      title <- "Deaths"
      column <- "confirmed_deaths"
      id <- "deaths"
    } else if (rv$selected_variable == "recovered") {
      title <- "Recovered"
      column <- "confirmed_recovered"
      id <- "recovered"
    }
    
    total_country$variable <- total_country[[column]]
    column_title <- paste0('Total ', title)
    
    top_variables <- 
      total_country %>%
      dplyr::group_by(`Country/Region`) %>%
      dplyr::summarise(variable = sum(variable, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      # dplyr::top_n(n = 10, wt = variable) %>%
      dplyr::arrange(desc(variable)) %>% 
      as.data.frame()
    
    tagList(
      div(class = "side-heading", column_title),
      lapply(1:10, side_table_row, top_variables = top_variables)
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