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
  tests_button <- button_class$new(type = "tests")
  

  observeEvent(rv$selected_variable, {
    if (rv$selected_variable == "infected") {
      infected_button$activate()
      deaths_button$deactivate()
      tests_button$deactivate()
    } else if (rv$selected_variable == "deaths") {
      infected_button$deactivate()
      deaths_button$activate()
      tests_button$deactivate()
    } else if (rv$selected_variable == "tests") {
      infected_button$deactivate()
      deaths_button$deactivate()
      tests_button$activate()
    }
  })

  observe({
    req(rv$selected_variable)
    output$three_buttons <-
      renderUI({
        tagList(
          actionButton("select_infected", "Infected", style = infected_button$style),
          actionButton("select_deaths", "Deaths", style = deaths_button$style),
          actionButton("select_tests", "tests", style = tests_button$style)
        )
      })
  })
}

#' Public Class to manage the style of 'Select Buttons'
#' @title button_style
#' @docType class
#' @description Public Class to manage the style of 'Select Buttons'
#' @export
#' 
#' @noRd
button_class <-
  R6::R6Class(
    classname = "button_style",
    public = list(
      active_colour = reactiveValues(),
      inactive_colour = character(0),
      status = "inactive",
      style = reactiveValues(),
      initialize = function(type) {
        data('global')
        if (type == "infected") self$active_colour <- global$colours$orange
        if (type == "deaths") self$active_colour <- global$colours$red
        if (type == "tests") self$active_colour <- global$colours$blue
        self$inactive_colour = global$colours$dark
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
          font_weight = 900
        } else {
          colour <- self$inactive_colour
          font_weight = 400
        }
        self$style <-
          paste0(
            "color: ", colour, ";",
            "font-weight: ", font_weight, ";"
          )
      }
    )
  )
