#' cloropleth UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cloropleth_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("cloropleth_infected"))
  )
}

#' cloropleth Server Function
#'
#' @noRd
mod_cloropleth_server <- function(input, output, session, total_country) {
  ns <- session$ns

  output$cloropleth_infected <-
    plotly::renderPlotly({

      # light grey boundaries
      l <- list(color = plotly::toRGB("#1F2430"), width = 1)

      # specify map projection/options
      g <- list(
        showframe = FALSE,
        showcoastlines = FALSE,
        projection = list(type = "mercator"),
        lonaxis = list(
          showgrid = FALSE,
          gridwidth = 0.5,
          range = c(-170, 180),
          dtick = 2
        ),
        lataxis = list(
          showgrid = FALSE,
          gridwidth = 0.5,
          range = c(-30, 80),
          dtick = 2
        )
      )

      total_country %>%
        plotly::plot_geo() %>%
        plotly::add_trace(
          z = ~confirmed_cases,
          color = ~confirmed_cases,
          text = ~`Country/Region`,
          locations = ~country_code,
          colors = harrypotter::hp(100, direction = -1)
        ) %>%
        plotly::layout(
          geo = g,
          margin = list(
            t = 0,
            b = 0,
            l = 0,
            r = 0
          )
        ) %>%
        plotly::colorbar(title = "Confirmed \nCases") %>%
        plotly::config(displayModeBar = F) %>%
        plotly::hide_colorbar()
    })
}

## To be copied in the UI
# mod_cloropleth_ui("cloropleth_ui_1")

## To be copied in the server
# callModule(mod_cloropleth_server, "cloropleth_ui_1")
