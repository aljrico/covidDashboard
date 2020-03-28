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
load_recovered <- function(){
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
}



#' Get Total by Country
#' 
#' @noRd
#' @export
get_total_country <- function(confirmed_ts, death_ts, recovered_ts){
  
  
  data('country_codes_dt')
  
  total_country <- 
    confirmed_ts %>% 
    dplyr::left_join(death_ts) %>% 
    dplyr::left_join(recovered_ts) %>% 
    dplyr::group_by(`Country/Region`) %>% 
    dplyr::filter(Date == max(Date)) %>% 
    dplyr::summarise(confirmed_cases = sum(as.numeric(confirmed_cases)),
                     confirmed_recovered = sum(as.numeric(confirmed_recovered)),
                     confirmed_deaths = sum(as.numeric(confirmed_deaths))) %>% 
    dplyr::arrange(desc(confirmed_cases)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(`Country/Region` = ifelse(`Country/Region` == 'US', 'United States', `Country/Region`)) %>% 
    # dplyr::mutate(unicode_name = tolower(`Country/Region`) %>% stringr::str_replace(' ', '_')) %>% 
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

  



