#' header UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_header_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("header"))
  )
}
    
#' header Server Function
#'
#' @noRd 
mod_header_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$header <- 
    renderUI({
      tagList(
        column(4, h1('Covid-19 Dashboard', id = 'big-heading')), 
        column(6), 
        column(2, div(class = "dateindicator", paste0("Updated: ", rv$last_date), style = "  padding-top: 40px;
  font-weight: 600;"))
      )
    })
}
    
## To be copied in the UI
# mod_header_ui("header_ui_1")
    
## To be copied in the server
# callModule(mod_header_server, "header_ui_1")
 
