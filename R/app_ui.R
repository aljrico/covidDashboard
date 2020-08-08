#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  sidebar <- function() {
    variable_button <- function(variable) {
      if (variable == "infected") symbol <- "syringe"
      if (variable == "deaths") symbol <- "skull"
      if (variable == "tests") symbol <- "tablets"


      tags$button(
        id = glue::glue("{variable}_button"),
        type = "button",
        class = glue::glue("btn action-button btn-large btn-variable btn-{variable}"),
        icon(symbol, class = "btn-symbol")
      )
    }

    # div(
    #   class = "center",
    #   h3("Map Metric"),
    #   fluidRow(
    #     column(2, variable_button("infected")),
    #     column(2, variable_button("deaths")),
    #     column(2, variable_button("tests"))
    #   )
    # )
    actionLink(
      inputId = "author_website",
      label = "About the Author"
    )
  }
  body <- function() {
    
    worldwide_view <- function(){
      fluidPage(
        div(
          class = "map-container",
          div(class = "map", mod_cloropleth_ui("cloropleth"))
          # div(class = "side-table left-table", mod_total_table_ui("left_table")),
          # div(class = "side-table right-table", mod_daily_table_ui("right_table"))
        ),
        br(),
        fluidRow(
          shinydashboard::box(
            solidHeader = TRUE,
            width = 4,
            mod_total_cases_ui("total_cases_country")
          ),
          shinydashboard::box(
            solidHeader = TRUE,
            width = 4,
            mod_total_cases_ui("total_deaths_country")
          ),
          shinydashboard::box(
            solidHeader = TRUE,
            width = 4,
            mod_total_cases_ui("total_tests_country")
          )
        ),
        fluidRow(
          shinydashboard::box(
            solidHeader = TRUE,
            width = 4,
            mod_evolution_metric_plot_ui("total_lineplot_cases")
          ),
          shinydashboard::box(
            solidHeader = TRUE,
            width = 4,
            mod_evolution_metric_plot_ui("total_lineplot_deaths")
          ),
          shinydashboard::box(
            solidHeader = TRUE,
            width = 4,
            mod_evolution_metric_plot_ui("total_lineplot_tests")
          )
        ),
        fluidRow(
          shinydashboard::box(
            solidHeader = TRUE,
            width = 4,
            mod_daily_plot_ui("dailyplot_cases")
          ),
          shinydashboard::box(
            solidHeader = TRUE,
            width = 4,
            mod_daily_plot_ui("dailyplot_deaths")
          ),
          shinydashboard::box(
            solidHeader = TRUE,
            width = 4,
            mod_daily_plot_ui("dailyplot_tests")
          )
        )
      )
    }
    country_view <- function(){
      fluidPage(
        mod_country_modal_ui("country_modal")
      )
    }
    
    tagList(
      golem_add_external_resources(),
      div(worldwide_view(), id = "worldwide_view"),
      shinyjs::hidden(div(country_view(), id = "country_view"))
    )
  }

  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = tags$img(src = "www/virus.svg", alt = "Coronavirus Logo", class = "covid-logo")),
    shinydashboard::dashboardSidebar(sidebar()),
    shinydashboard::dashboardBody(body()),
    skin = "blue"
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
    use_googlefont("Open Sans"),
    favicon(ext = "png"),
    shinyjs::useShinyjs(),
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
