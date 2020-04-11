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
mod_cloropleth_server <- function(input, output, session, map_data, variable) {
  ns <- session$ns

  output$cloropleth <-
    leaflet::renderLeaflet({
      
      create_gradient <- function(col1, col2){
        fn_cols <- grDevices::colorRamp(c(col1, col2), space = "Lab", interpolate = "spline")
        cols <- fn_cols(seq(0, 1, length.out = 10)) / 255
        grDevices::rgb(cols[, 1], cols[, 2], cols[, 3], alpha = 1)
      }
      
      if(variable == 'infected'){
        title <- 'Confirmed Cases'
        column <- 'confirmed_cases'
        colours <- create_gradient(col1 = "#FAFAFA", "#FF9940")
      }else if(variable == 'deaths'){
        title <- 'Deaths'
        column <- 'confirmed_deaths'
        colours <- create_gradient(col1 = "#FAFAFA", "#F07171")
      }else if(variable == 'recovered'){
        title <- 'Recovered'
        column <- 'confirmed_recovered'
        colours <- create_gradient(col1 = "#FAFAFA", "#5CCFE6")
      }
      
      map_data$variable <- map_data[[column]]
      
      # Prepare text for the tooltip
      mytext <- paste0(
        "<b> Country: </b> ", map_data$NAME, "<br/>",
        "<b> ", title, ": </b> ", prettyNum(map_data$variable, big.mark = ","), "<br/>"
      ) %>%
        lapply(htmltools::HTML)

      # # Prepare the text for tooltips:
      # mytext <- paste0(
      #   "<b> Country: </b> ", map_data$NAME, "<br/>",
      #   "<b> Confirmed Cases: </b>", map_data$confirmed_cases, "<br/>",
      #   "<b> Deaths: </b>", map_data$confirmed_deaths
      # ) %>%
      #   lapply(htmltools::HTML)

      my_bins <- c(0, 1e2, 1e3, 1e4, 1e5, 1e6)
      my_palette <- leaflet::colorBin(colours, map_data$variable, na.color = "#FAFAFA", bins = my_bins)
      
      
      # Build Cloropleth
      leaflet::leaflet(map_data) %>%
        leaflet::addTiles() %>%
        leaflet::setView(lat = 20, lng = 10, zoom = 2.2) %>%
        leaflet::addPolygons(
          fill = '#FAFAFA',
          stroke = FALSE,
          # smoothFactor = 1,
          fillOpacity = 0.75,
          color = "1F2430",
          weight = 0.7,
          label = mytext,
          labelOptions = leaflet::labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px",
            direction = "auto"
          ),
          fillColor = ~ my_palette(variable)
        ) %>%
        leaflet::addLegend(
          pal = my_palette,
          values = ~variable,
          opacity = 0.9,
          title = "Confirmed Cases",
          position = "bottomleft"
        )
    })
}
