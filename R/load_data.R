#' Load Historical Deaths
#' 
#' @noRd
#' @export
load_deaths <- function(){
  deaths_url <- "https://data.humdata.org/hxlproxy/data/download/time_series_covid19_deaths_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv"
  deaths_dt <- 
    data.table::fread(deaths_url)[-1,] %>% 
    dplyr::rename(confirmed_deaths = Value)
}

#' Load Historical Confirmed
#' 
#' @noRd
#' @export
load_confirmed <- function(){
  confirmed_url <- "https://data.humdata.org/hxlproxy/data/download/time_series_covid19_confirmed_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv"
  confirmed_dt <- 
    data.table::fread(confirmed_url)[-1,] %>% 
    dplyr::rename(confirmed_cases = Value)
}


#' Load Historical Recovered
#' 
#' @noRd
#' @export
download_recovered <- function(){
  recovered_url <- "https://data.humdata.org/hxlproxy/data/download/time_series_covid19_recovered_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_recovered_global.csv"
  recovered_dt <- 
    data.table::fread(recovered_url)[-1,] %>% 
    dplyr::rename(confirmed_recovered = Value)
}

#' Load Historical Recovered
#' 
#' @noRd
#' @export
country_codes <- function(){
  country_codes_url <- ('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
  country_codes_dt <- 
    country_codes_url %>% 
    data.table::fread() %>% 
    dplyr::rename(`Country/Region` = COUNTRY, country_code = CODE) %>% 
    dplyr::select(`Country/Region`, country_code)
  
  return(country_codes_dt)
}

#' Extract daily by country
#' 
#' @noRd
#' @export
get_daily_country <- function(confirmed_ts, death_ts, recovered_ts, country_codes_dt){
  
  daily_country <- data.table::copy(world_data)
  daily_country <- daily_country[country != "World"]
  daily_country %>% 
    dplyr::group_by(country, country_code, date) %>% 
    dplyr::summarise(new_cases = sum(as.numeric(new_cases), na.rm = TRUE),
                     new_tests = sum(as.numeric(new_tests), na.rm = TRUE),
                     new_deaths = sum(as.numeric(new_deaths), na.rm = TRUE)) %>% 
    dplyr::arrange(desc(new_cases))
  
  # daily_country <- 
  # confirmed_ts %>% 
  #   dplyr::left_join(death_ts) %>% 
  #   dplyr::left_join(recovered_ts) %>% 
  #   dplyr::group_by(`Country/Region`) %>% 
  #   dplyr::mutate(new_deaths = as.numeric(confirmed_deaths) - dplyr::lead(as.numeric(confirmed_deaths))) %>% 
  #   dplyr::mutate(new_recovered = as.numeric(confirmed_recovered) - dplyr::lead(as.numeric(confirmed_recovered))) %>% 
  #   dplyr::mutate(new_cases = as.numeric(confirmed_cases) - dplyr::lead(as.numeric(confirmed_cases))) %>% 
  #   dplyr::ungroup() %>% 
  #   dplyr::mutate(`Country/Region` = ifelse(`Country/Region` == 'US', 'United States', `Country/Region`)) %>% 
  #   dplyr::left_join(country_codes_dt)
  # 
  # return(daily_country)
}

#' Get Total by Country
#' 
#' @noRd
#' @export
get_total_country <- function(world_data){
  
  total_country <- data.table::copy(world_data)
  total_country <- total_country[country != "World"]
  total_country %>% 
    dplyr::group_by(country, country_code) %>% 
    dplyr::filter(date == max(date)) %>% 
    dplyr::summarise(total_cases = sum(as.numeric(total_cases), na.rm = TRUE),
                     total_recovered = sum(as.numeric(total_recovered), na.rm = TRUE),
                     total_deaths = sum(as.numeric(total_deaths), na.rm = TRUE)) %>% 
    dplyr::arrange(desc(total_cases))
  
  # total_country <- 
  #   confirmed_ts %>% 
  #   dplyr::left_join(death_ts) %>% 
  #   dplyr::left_join(recovered_ts) %>% 
  #   dplyr::group_by(`Country/Region`) %>% 
  #   dplyr::filter(Date == max(Date)) %>% 
  #   dplyr::summarise(confirmed_cases = sum(as.numeric(confirmed_cases), na.rm = TRUE),
  #                    confirmed_recovered = sum(as.numeric(confirmed_recovered), na.rm = TRUE),
  #                    confirmed_deaths = sum(as.numeric(confirmed_deaths), na.rm = TRUE)) %>% 
  #   dplyr::arrange(desc(confirmed_cases)) %>% 
  #   dplyr::ungroup() %>% 
  #   dplyr::mutate(`Country/Region` = ifelse(`Country/Region` == 'US', 'United States', `Country/Region`)) %>% 
  #   dplyr::left_join(country_codes_dt)
}

#' Total data in a map
#' 
#' @noRd
#' @export
get_map_data <- function(total_country){
  data('world_spdf')

  map_data <- 
    world_spdf %>% 
    dplyr::select(NAME, LON, LAT, ISO3, geometry) %>% 
    dplyr::rename(country_code = ISO3) %>% 
    dplyr::mutate(country_code = as.character(country_code)) %>%
    dplyr::inner_join(total_country, by = "country_code") %>% 
    sf::st_as_sf() 
  
  return(map_data)
}

#' Download all world data
#' 
#' @noRd
#' @export
download_world_data <- function(){
  data_url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
  world_data <- data.table::fread(data_url)
  data.table::setnames(
    world_data,
    old = c("location", "iso_code"),
    new = c("country", "country_code")
  )
  
  return(world_data)
}

#' Merge recovered data
#' 
#' @noRd
#' @export
merge_recovered <- function(recovered_ts, world_data){
  
  # Static copy
  dt_recovered <- data.table::copy(recovered_ts)
  dt_world <- data.table::copy(world_data)
  
  # Uniformise column names
  data.table::setnames(dt_recovered, old = "confirmed_recovered", new = "total_recovered")
  data.table::setnames(dt_recovered, old = "Country/Region", new = "country")
  data.table::setnames(dt_recovered, old = "Date", new = "date")
  dt_recovered <- dt_recovered[, c("country", "date", "total_recovered")]
  
  # Create 'new' column
  dt_recovered <- 
    dt_recovered %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(new_recovered = as.numeric(total_recovered) - dplyr::lead(as.numeric(total_recovered))) %>% 
    data.table::data.table()
  
  # Left Join to world data
  dt_world[dt_recovered, on = c("country", "date")]
}

#' Merge recovered data
#' 
#' @noRd
#' @export
data_handler <-
  R6::R6Class(
    classname = "data_handler",
    public = list(
      world_data = NULL,
      last_date = NULL,
      daily_country = NULL,
      total_country = NULL,
      map_data = NULL,
      initialize = function() {
        world_data <- download_world_data()
        recovered_ts <- download_recovered()
        self$world_data <- merge_recovered(recovered_ts, world_data)
        self$last_date <- self$world_data$date %>% max()
        
        # Processed Data
        self$daily_country <- get_daily_country(self$world_data)
        self$total_country <- get_total_country(self$world_data)
        self$map_data <- get_map_data(self$total_country)
      }
    )
  )
