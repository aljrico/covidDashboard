#' Download general data
#'
#'
#'
download_all_data <- function() {
  data_url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
  world_data <-
    data.table::fread(data_url) %>%
    dplyr::mutate(location = ifelse(location == "United States", "US", location)) %>%
    data.table::data.table()
  return(world_data)
}

#' Load Historical Deaths
#'
#' @noRd
#' @export
load_deaths <- function() {
  # deaths_url <- "https://data.humdata.org/hxlproxy/data/download/time_series_covid19_deaths_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv"
  # deaths_dt <-
  #   data.table::fread(deaths_url)[-1,] %>%
  #   dplyr::rename(confirmed_deaths = Value)

  download_all_data() %>%
    dplyr::select(location, date, total_deaths) %>%
    dplyr::rename(confirmed_deaths = total_deaths, Date = date, location = location) %>%
    data.table::data.table()
}

#' Load Historical Confirmed
#'
#' @noRd
#' @export
load_confirmed <- function() {
  # confirmed_url <- "https://data.humdata.org/hxlproxy/data/download/time_series_covid19_confirmed_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv"
  # confirmed_dt <-
  #   data.table::fread(confirmed_url)[-1,] %>%
  #   dplyr::rename(confirmed_cases = Value)

  download_all_data() %>%
    dplyr::select(location, date, total_cases) %>%
    dplyr::rename(confirmed_cases = total_cases, Date = date, location = location) %>%
    data.table::data.table()
}


#' Load Historical tests
#'
#' @noRd
#' @export
load_tests <- function() {
  # tests_url <- "https://data.humdata.org/hxlproxy/data/download/time_series_covid19_tests_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_tests_global.csv"
  # tests_dt <-
  #   data.table::fread(tests_url)[-1,] %>%
  #   dplyr::rename(total_tests = Value)

  dt <-
    download_all_data() %>%
    dplyr::select(location, date, total_tests) %>%
    dplyr::rename(total_tests = total_tests, Date = date, location = location) %>%
    data.table::data.table()

  # Fill missing gaps
  dt[, total_tests := total_tests[1], .(location, cumsum(!is.na(total_tests)))]

  return(dt)
}

#' Load Historical tests
#'
#' @noRd
#' @export
download_country_codes <- function() {
  country_codes_url <- ("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv")
  country_codes_dt <-
    country_codes_url %>%
    data.table::fread() %>%
    dplyr::rename(location = COUNTRY, country_code = CODE) %>%
    dplyr::select(location, country_code)

  return(country_codes_dt)
}

#' Extract daily by country
#'
#' @noRd
#' @export
get_daily_country <- function(confirmed_ts, death_ts, tests_ts, country_codes_dt) {
  daily_country <-
    confirmed_ts %>%
    dplyr::left_join(death_ts) %>%
    dplyr::left_join(tests_ts) %>%
    dplyr::group_by(location) %>%
    dplyr::mutate(deaths_change = as.numeric(confirmed_deaths) - dplyr::lead(as.numeric(confirmed_deaths))) %>%
    dplyr::mutate(tests_change = as.numeric(total_tests) - dplyr::lead(as.numeric(total_tests))) %>%
    dplyr::mutate(cases_change = as.numeric(confirmed_cases) - dplyr::lead(as.numeric(confirmed_cases))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(location = ifelse(location == "US", "United States", location)) %>%
    dplyr::left_join(country_codes_dt)

  return(daily_country)
}

#' Get Total by Country
#'
#' @noRd
#' @export
get_total_country <- function(confirmed_ts, death_ts, tests_ts, country_codes_dt) {
  total_country <-
    confirmed_ts %>%
    dplyr::left_join(death_ts) %>%
    dplyr::left_join(tests_ts) %>%
    dplyr::group_by(location) %>%
    dplyr::filter(Date == max(Date)) %>%
    dplyr::summarise(
      confirmed_cases = sum(as.numeric(confirmed_cases), na.rm = TRUE),
      total_tests = sum(as.numeric(total_tests), na.rm = TRUE),
      confirmed_deaths = sum(as.numeric(confirmed_deaths), na.rm = TRUE)
    ) %>%
    dplyr::arrange(desc(confirmed_cases)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(location = ifelse(location == "US", "United States", location)) %>%
    # dplyr::mutate(unicode_name = tolower(location) %>% stringr::str_replace(' ', '_')) %>%
    # dplyr::mutate(unicode_name = ifelse(unicode_name == 'korea,_south', 'kr', unicode_name)) %>%
    # dplyr::mutate(unicode_name = ifelse(unicode_name == 'france', 'fr', unicode_name)) %>%
    # dplyr::mutate(unicode_name = ifelse(unicode_name == 'spain', 'es', unicode_name)) %>%
    # dplyr::mutate(unicode_name = ifelse(unicode_name == 'france', 'fr', unicode_name)) %>%
    # dplyr::mutate(unicode_name = ifelse(unicode_name == 'germany', 'de', unicode_name)) %>%
    # dplyr::mutate(unicode_name = ifelse(unicode_name == 'united_kingdom', 'uk', unicode_name)) %>%
    # dplyr::mutate(unicode_name = ifelse(unicode_name == 'italy', 'it', unicode_name)) %>%
    # dplyr::mutate(unicode_name = ifelse(unicode_name == 'china', 'cn', unicode_name)) %>%
    # dplyr::mutate(unicode_name = ifelse(unicode_name == 'russia', 'ru', unicode_name)) %>%
    # dplyr::mutate(unicode_name = ifelse(unicode_name == 'serbia', 'rs', unicode_name)) %>%
    # dplyr::mutate(unicode_name = ifelse(unicode_name == 'united_arab_emirates', 'ae', unicode_name)) %>%
    # dplyr::mutate(unicode = emojifont::emoji(unicode_name)) %>%
    dplyr::left_join(country_codes_dt)
}

#' Total data in a map
#'
#' @noRd
#' @export
get_map_data <- function(total_country) {
  data("world_spdf")

  map_data <-
    world_spdf %>%
    dplyr::select(NAME, LON, LAT, ISO3, geometry) %>%
    dplyr::rename(country_code = ISO3) %>%
    dplyr::mutate(country_code = as.character(country_code)) %>%
    dplyr::inner_join(total_country, by = "country_code") %>%
    sf::st_as_sf()

  return(map_data)
}

#' Merge recovered data
#'
#' @noRd
#' @export
DataHandler <-
  R6::R6Class(
    classname = "data_handler",
    public = list(
      raw_data = NULL,
      infected_data = NULL,
      deaths_data = NULL,
      tests_data = NULL,
      country_codes = NULL,
      last_date = NULL,
      daily_country = NULL,
      total_country = NULL,
      map_data = NULL,
      remove_last_date = function(){
        self$raw_data = self$raw_data[date != Sys.Date()]
      },
      get_last_date = function(){
        self$raw_date$Date %>% max()
      },
      get_data = function(){
        # Download basic raw data
        self$raw_data <- download_all_data()
        self$country_codes <- download_country_codes()
        
        # Last date is normally uncomplete, so we remove it
        self$remove_last_date()
        
        # Record whatever last date is
        self$last_date <- self$get_last_date()
        
        # Process Data
        self$infected_data <- handle_confirmed_data(self$raw_data)
        self$deaths_data <- handle_deaths_data(self$raw_data)
        self$tests_data <- handle_tests_data(self$raw_data)
        self$daily_country <- get_daily_country(self$infected_data, self$deaths_data, self$tests_data, self$country_codes)
        self$total_country <- get_total_country(self$infected_data, self$deaths_data, self$tests_data, self$country_codes)
        self$map_data <- get_map_data(self$total_country)
      }
    )
  )

#' Handle confirmed data
#'
#' @noRd
#' @export
handle_confirmed_data <- function(raw_data) {
  raw_data %>%
    dplyr::select(location, date, total_cases) %>%
    dplyr::mutate(location = ifelse(location == "United States", "US", location)) %>%
    dplyr::rename(confirmed_cases = total_cases, Date = date, location = location) %>%
    data.table::data.table()
}

#' Handle deaths data
#'
#' @noRd
#' @export
handle_deaths_data <- function(raw_data) {
  raw_data %>%
    dplyr::select(location, date, total_deaths, new_deaths) %>%
    dplyr::mutate(location = ifelse(location == "United States", "US", location)) %>%
    dplyr::rename(confirmed_deaths = total_deaths, Date = date, location = location) %>%
    data.table::data.table()
}

#' Handle deaths data
#'
#' @noRd
#' @export
handle_tests_data <- function(raw_data) {
  replace_na <- function(dt, value) { # see EDIT later for more elegant solution
    na.replace <- function(v, value = value) {
      v[is.na(v)] <- value
      v
    }
    for (i in names(dt)) {
      eval(parse(text = paste0("dt[,", i, " := na.replace(", i, ")]")))
    }
    return(dt)
  }

  dt <-
    raw_data %>%
    dplyr::select(location, date, total_tests, new_tests) %>%
    dplyr::rename(total_tests = total_tests, Date = date, location = location) %>%
    data.table::data.table()

  # Fill missing gaps
  dt[, total_tests := total_tests[1], .(location, cumsum(!is.na(total_tests)))]

  # Fill NAs
  # dt = replace_na(dt)

  return(dt)
}
