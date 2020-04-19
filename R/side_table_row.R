#' Side Table Row
#'
#' This function creates the necessary html to display the tables at the sides of the app
#' 
#' @param top_variables A data.frame with the information to be displayed. First column should be the country name and second column the numeric variable.
#' @param i Row of the data.frame to be shown
#'
side_table_row <- function(i, top_variables, plus = FALSE){
  plus_sign <- ""
  if(plus) plus_sign <- "+"
  fluidRow(
    div(class = "side-table-content-row",
      div(class = "side-table-content-country", tags$b(paste0(top_variables[i,1], ": "))),
      div(class = "side-table-content-number", tags$p(paste0(plus_sign, prettyNum(top_variables[i,2], big.interval = 3L, big.mark = ","))))
    )
  )
}