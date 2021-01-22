#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  
  sidebar <- function() {
    
    timerange_button <- function(){
      
      shinyWidgets::radioGroupButtons(
        label = "Time Range",
        inputId = "radiobutton_timerange",
        choices = c("Last 90 days", "Full Range"),
        direction = "vertical",
        size = "normal"
      )
    }
    
    tagList(
      div(
        class = "text-center",
        style = "color: #1f2430;",
          h3("Control Panel"),
          timerange_button()
      ),
      hr(),
      actionLink(
        inputId = "author_website",
        label = tags$a("Author's Website", href = "https://aljrico.com", target="_blank")
      )
    )
  }
  body <- function() {
    
    worldwide_view <- function(){
      fluidPage(
        fluidRow(h2("Worldwide", style = "padding-left: 45px;")),
        br(),
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
        country_details$ui(id = "country_modal")
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
