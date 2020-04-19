#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  
  border_css <- 
    "
    border-right: 2px solid #1f2430;
    border-top: 2px solid #1f2430;
    border-left: 2px solid #1f2430;
    border-bottom: 2px solid #1f2430;
    
    "
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    fluidPage(
      fluidRow(
        mod_header_ui("header")
      ),
      fluidRow(
        column(5),
        column(aligh = 'center', width = 2,
               mod_select_buttons_ui('select_buttons')
        )
      ),
      div(
        class = "map-container",
        div(class = "map", mod_cloropleth_ui("cloropleth")),
        div(class = "side-table left-table", mod_total_table_ui('left_table')),
        div(class = "side-table right-table", mod_daily_table_ui('right_table'))
      ),
      fluidRow(
        shinydashboard::box(solidHeader = TRUE,
          width = 4,
          mod_total_cases_ui("total_cases_country")
        ),
        shinydashboard::box(solidHeader = TRUE,
          width = 4,
          mod_total_cases_ui("total_deaths_country")
        ),
        shinydashboard::box(solidHeader = TRUE,
          width = 4,
          mod_total_cases_ui("total_recovered_country")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    use_googlefont("Ubuntu"),
    favicon(),
    shinyWidgets::useShinydashboard(),
    waiter::use_waiter(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "covidDashboard"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
