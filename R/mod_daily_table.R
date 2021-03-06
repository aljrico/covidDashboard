#' daily_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_daily_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("daily_table"))
  )
}

#' daily_table Server Function
#'
#' @noRd
mod_daily_table_server <- function(input, output, session, rv) {
  ns <- session$ns


  output$daily_table <- renderUI({
    daily_country <- 
      rv$daily_country %>% 
      dplyr::filter(Date == max(Date))

    if (rv$selected_variable == "infected") {
      title <- "Confirmed Cases"
      column <- "cases_change"
      id <- "confirmed"
    } else if (rv$selected_variable == "deaths") {
      title <- "Deaths"
      column <- "deaths_change"
      id <- "deaths"
    } else if (rv$selected_variable == "tests") {
      title <- "Tests"
      column <- "tests_change"
      id <- "tests"
    }

    daily_country$variable <- daily_country[[column]]
    column_title <- paste0("Daily ", title)

    top_variables <-
      daily_country %>%
      dplyr::group_by(location) %>%
      dplyr::summarise(variable = sum(variable, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(desc(variable)) %>%
      as.data.frame()

    tagList(
      div(class = "side-heading", column_title),
      fluidRow(
        div(
          class = "side-table-content-row",
          div(class = "side-table-content-country side-subheading", "Country"),
          div(class = "side-table-content-number side-subheading", "Daily")
        )
      ),
      lapply(1:8, side_table_row, top_variables = top_variables, plus = TRUE)
    )
  })
}

## To be copied in the UI
# mod_daily_table_ui("daily_table_ui_1")

## To be copied in the server
# callModule(mod_daily_table_server, "daily_table_ui_1")
