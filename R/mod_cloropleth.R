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
    leaflet::leafletOutput(outputId = ns("cloropleth"), height = 500)
  )
}

#' cloropleth Server Function
#'
#' @noRd
mod_cloropleth_server <- function(input, output, session, rv, global) {
  ns <- session$ns

  observeEvent(input$cloropleth_shape_click, {
    rv$selected_country <- input$cloropleth_shape_click$id
  })

  output$cloropleth <-
    leaflet::renderLeaflet({

      # Define options
      leaflet_options <- function() {
        leaflet::leafletOptions(
          minZoom = 2,
          controlZoom = FALSE
        )
      }

      # Design legend
      legend_hint <- function() {
          tags$p("Click on a specific country for more details.")
      }

      # Build Cloropleth
      leaflet::leaflet(rv$map_data, options = leaflet_options()) %>%
        leaflet::setMaxBounds(
          lng1 = -120,
          lat1 = -80,
          lng2 = 120,
          lat2 = 90
        ) %>%
        # leaflet::addTiles() %>%
        leaflet::setView(lat = 20, lng = 10, zoom = 2.2) %>%
        add_polygons(rv) %>%
        leaflet::addControl(html = legend_hint(), position = "topright")
      # leaflet::addLayersControl(baseGroups = c("confirmed_cases", "confirmed_deaths", "total_tests"))
      # leaflet::addLegend(
      #   pal = my_palette,
      #   values = ~ rv$selected_variable,
      #   opacity = 0.9,
      #   title = title,
      #   position = "bottomleft"
      # )
    })
}


add_polygons <- function(map, rv) {
  get_quantiles <- function(metric, n = 666) {
    qs <- seq(from = 0, to = 1, by = 1 / n)
    bins <- unique(floor(quantile(log(metric), qs) %>% as.vector()))
    c(bins, Inf)
  }
  
  create_gradient <- function(col1, col2) {
    fn_cols <- grDevices::colorRamp(c(col1, col2), space = "Lab", interpolate = "spline")
    cols <- fn_cols(seq(0, 1, length.out = 10)) / 255
    grDevices::rgb(cols[, 1], cols[, 2], cols[, 3], alpha = 1)
  }

  # Prepare text for the tooltip
  mytext <- paste0(
    "<b> Country: </b> ", rv$map_data$NAME, "<br/>",
    "<hr>",
    "<b> Infected: </b> ", prettyNum(rv$map_data[["confirmed_cases"]], big.mark = ","), "<br/>",
    "<b> Deaths: </b> ", prettyNum(rv$map_data[["confirmed_deaths"]], big.mark = ","), "<br/>",
    "<b> Tests: </b> ", prettyNum(rv$map_data[["total_tests"]], big.mark = ","), "<br/>"
  ) %>%
    lapply(htmltools::HTML)

  colours <- create_gradient(col1 = global$colours$grey, col2 = global$colours$orange)
  my_palette <- leaflet::colorBin(colours, rv$map_data[["confirmed_cases"]], na.color = "white", bins = get_quantiles(rv$map_data[["confirmed_cases"]]))
  my_palette <- leaflet::colorQuantile(colours, rv$map_data[["confirmed_cases"]], na.color = "white", probs = c(0, 0.1, 0.3, 0.6, 0.8, 0.9, 0.925, 0.95, 0.975, 0.99, 1))
  
  
  map %>%
    leaflet::addPolygons(
      layerId = ~country_code,
      fill = "white",
      stroke = TRUE,
      smoothFactor = 1,
      fillOpacity = 1,
      color = "#1F2430",
      weight = 1,
      label = mytext,
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px", "background-color" = "#FAFAFA"),
        textsize = "13px",
        direction = "auto"
      ),
      fillColor = ~ my_palette((rv$map_data[["confirmed_cases"]])),
      highlightOptions = leaflet::highlightOptions(
        color = "#FAFAFA", opacity = 1, weight = 2, fillOpacity = 1,
        bringToFront = TRUE, sendToBack = TRUE
      )
    )
}
