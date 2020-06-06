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
      create_gradient <- function(col1, col2) {
        fn_cols <- grDevices::colorRamp(c(col1, col2), space = "Lab", interpolate = "spline")
        cols <- fn_cols(seq(0, 1, length.out = 10)) / 255
        grDevices::rgb(cols[, 1], cols[, 2], cols[, 3], alpha = 1)
      }
      
      get_quantiles <- function(metric, n = 666){
        qs <- seq(from = 0, to = 1, by = 1/ n)
        bins <- unique(floor(quantile(log(metric), qs) %>% as.vector()))
        c(bins, Inf)
      }

      if (rv$selected_variable == "infected") {
        title <- "Confirmed Cases"
        column <- "confirmed_cases"
        colours <- create_gradient(col1 = global$colours$grey, col2 = global$colours$orange)
        my_bins <- c(0, 1, 1e2, 1e3, 1e4, 1e5, Inf)
      } else if (rv$selected_variable == "deaths") {
        title <- "Deaths"
        column <- "confirmed_deaths"
        colours <- create_gradient(col1 = global$colours$grey, global$colours$red)
        my_bins <- c(0, 1, 1e2, 1e3, 1e4, 2e4, Inf)
      } else if (rv$selected_variable == "recovered") {
        title <- "Recovered"
        column <- "confirmed_recovered"
        colours <- create_gradient(col1 = global$colours$grey, global$colours$blue)
        my_bins <- c(0, 1e2, 1e3, 1e4, 1e5, Inf)
      }

      # Prepare text for the tooltip
      mytext <- paste0(
        "<b> Country: </b> ", rv$map_data$NAME, "<br/>",
        "<b> ", title, ": </b> ", prettyNum(rv$map_data[[column]], big.mark = ","), "<br/>"
      ) %>%
        lapply(htmltools::HTML)

      my_palette <- leaflet::colorBin(colours, rv$map_data[[column]], na.color = "white", bins = get_quantiles(rv$map_data[[column]]))

      # Define options
      leaflet_options <- leaflet::leafletOptions(
        minZoom = 2,
        controlZoom = FALSE
      )

      # Build Cloropleth
      leaflet::leaflet(rv$map_data, options = leaflet::leafletOptions(
        minZoom = 2,
        zoomControl = FALSE
      )) %>%
        leaflet::setMaxBounds(
          lng1 = -120,
          lat1 = -80,
          lng2 = 120,
          lat2 = 90
        ) %>%
        # leaflet::addTiles() %>%
        leaflet::setView(lat = 20, lng = 10, zoom = 2.2) %>%
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
          fillColor = ~ my_palette(log(rv$map_data[[column]])),
          highlightOptions = leaflet::highlightOptions(
            color = "#FAFAFA", opacity = 1, weight = 2, fillOpacity = 1,
            bringToFront = TRUE, sendToBack = TRUE)
        ) 
        # leaflet::addLegend(
        #   pal = my_palette,
        #   values = ~ rv$selected_variable,
        #   opacity = 0.9,
        #   title = title,
        #   position = "bottomleft"
        # )
    })
}
