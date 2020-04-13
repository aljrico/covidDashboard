#' select_buttons UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_select_buttons_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('three_buttons'))
  )
}
    
#' select_buttons Server Function
#'
#' @noRd 
mod_select_buttons_server <- function(input, output, session, rv){
  ns <- session$ns
 
  output$three_buttons <- 
    renderUI({
      tagList(
        actionButton("select_infected", "Infected", style = paste0('color: ', rv$colours$orange)),
        actionButton("select_deaths", "Deaths", style = paste0('color: ', rv$colours$red)), 
        actionButton("select_recovered", "Recovered", style = paste0('color: ', rv$colours$green))
      )
    })
}