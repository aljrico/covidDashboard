#' Plot Metric Daily
#' 
#' @noRd
#' @export
  PlotterDaily <- R6::R6Class(
    "PlotterDaily",
    public = list(
      initialize = function(variable){
        private$variable = variable
      },
      ingest_data = function(data){
        data <- data.table::copy(data)
        data.table::setDT(data)
        
        data.table::setnames(data, old = private$variable, new = "value", skip_absent = TRUE)
        
        private$data <- 
          data %>%
          dplyr::group_by(Date) %>%
          dplyr::summarise(result = sum(as.numeric(value), na.rm = TRUE)) %>%
          dplyr::mutate(Date = lubridate::ymd(Date)) %>%
          dplyr::ungroup() %>% 
          dplyr::mutate(change = (result - dplyr::lag(result))) %>% 
          na.omit() %>% 
          dplyr::filter(Date > max(Date) - 90)
      },
      plot = function(){
        private$data %>% 
          plotly::plot_ly(
            type = "bar",
            hoverinfo = "text",
            text = ~ private$hover_text(change, Date)
          ) %>%
          plotly::add_trace(
            x = ~Date, y = ~change,
            marker = list(
              color = pick_colour(private$variable, rgb = TRUE)
            )
          ) %>%
          plotly::layout(
            showlegend = FALSE,
            title = paste0("Daily ", private$write_title()),
            xaxis = list(title = '', fixedrange = TRUE, showgrid = TRUE),
            yaxis = list(title = '', fixedrange = TRUE, showgrid = TRUE)
          ) %>% 
          plotly::layout(
            plot_bgcolor = "#FAFAFA",
            paper_bgcolor = "#FAFAFA"
          ) %>% 
          plotly::config(displayModeBar = FALSE)
      }
    ),
    private = list(
      variable = NULL,
      data = NULL,
      hover_text = function(value, d) {
        if (private$variable == "confirmed_cases") variable_text <- "Infected: "
        if (private$variable == "confirmed_deaths") variable_text <- "Deaths: "
        if (private$variable == "total_tests") variable_text <- "Tests: "
        value_text <- formatC(value, format = "f", big.mark = ",", digits = 0)
        paste0(
          "</br> Daily <b>", variable_text, "</b>", value_text,
          "</br> <b>", "Date: ", "</b>", d
        )
      },
      write_title = function() {
        if (private$variable == "confirmed_cases") title <- "Confirmed Cases"
        if (private$variable == "confirmed_deaths") title <- "Deaths"
        if (private$variable == "total_tests") title <- "Tests"
        return(title)
      },
      add_units = function(n){
        labels <- ifelse(n < 1000, n,  # less than thousands
                         ifelse(n < 1e6, paste0(round(n/1e3, 1), 'k'),  # in thousands
                                ifelse(n < 1e9, paste0(round(n/1e6, 1), 'M'),  # in millions
                                       ifelse(n < 1e12, paste0(round(n/1e9, 1), 'B'), # in billions
                                              ifelse(n < 1e15, paste0(round(n/1e12, 1), 'T'), # in trillions
                                                     'too big!'
                                              )))))
        return(labels)
      }
    )
  )