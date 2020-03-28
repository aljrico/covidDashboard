#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    fluidPage(
      h1("covidDashboard"),
      fluidRow(
        bs4Dash::bs4Box(
          mod_total_cases_ui("total_cases_country"),
          height = 500,
          width = 4
        ),
        bs4Dash::bs4Box(
          mod_total_cases_ui("total_deaths_country"),
          height = 500,
          width = 4
        ),
        bs4Dash::bs4Box(
          mod_total_cases_ui("total_recovered_country"),
          height = 500,
          width = 4
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
    favicon(),
    waiter::use_waiter(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "covidDashboard"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
