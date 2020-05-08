#' select_buttons UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_select_buttons_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("three_buttons"))
  )
}

#' select_buttons Server Function
#'
#' @noRd
mod_select_buttons_server <- function(input, output, session, rv, global) {
  ns <- session$ns
  
  button_style <- reactiveValues()
  infected_button <- button_class$new(type = "infected")
  deaths_button <- button_class$new(type = "deaths")
  recovered_button <- button_class$new(type = "recovered")
  

  observeEvent(rv$selected_variable, {
    if (rv$selected_variable == "infected") {
      infected_button$activate()
      deaths_button$deactivate()
      recovered_button$deactivate()
    } else if (rv$selected_variable == "deaths") {
      infected_button$deactivate()
      deaths_button$activate()
      recovered_button$deactivate()
    } else if (rv$selected_variable == "recovered") {
      infected_button$deactivate()
      deaths_button$deactivate()
      recovered_button$activate()
    }
  })

  observe({
    req(rv$selected_variable)
    output$three_buttons <-
      renderUI({
        tagList(
          actionButton("select_infected", "Infected", style = infected_button$style),
          actionButton("select_deaths", "Deaths", style = deaths_button$style),
          actionButton("select_recovered", "Recovered", style = recovered_button$style)
        )
      })
  })
}

#' Public Class to manage the style of 'Select Buttons'
#' 
#' @noRd
button_class <-
  R6::R6Class(
    classname = "button_style",
    public = list(
      active_colour = reactiveValues(),
      inactive_colour = global$colours$dark,
      status = "inactive",
      style = reactiveValues(),
      initialize = function(type) {
        if (type == "infected") self$active_colour <- global$colours$orange
        if (type == "deaths") self$active_colour <- global$colours$red
        if (type == "recovered") self$active_colour <- global$colours$green
        private$create_style()
      },
      activate = function(){
        self$status <- "active"
        private$create_style()
      },
      deactivate = function(){
        self$status <- "inactive"
        private$create_style()
      }
    ),
    private = list(
      create_style = function() {
        if (self$status == "active") {
          colour <- self$active_colour
        } else {
          colour <- self$inactive_colour
        }
        self$style <-
          paste0(
            "color: ", colour, ";"
          )
      }
    )
  )
