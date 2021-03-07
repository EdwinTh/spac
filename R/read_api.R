api_key <- function() {
  readLines("vantage-point-api-key")
}

#' Read API
#'
#' Connect to the alphavantage
read_vp_api <- function(symbol = "TSIA") {
  q <- glue::glue("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol={symbol}&outputsize=compact&apikey={api_key()}&datatype=csv")
  readLines(curl::curl(q))
}

api_string_to_df <- function(s) {
  dplyr::tibble(date   = as.Date(s[1]),
                open   = as.numeric(s[2]),
                high   = as.numeric(s[3]),
                low    = as.numeric(s[4]),
                close  = as.numeric(s[5]),
                volume = as.numeric(s[6]))
}

clean_vp_api_result <- function(res, ticker = "TSIA") {
  spl <- stringr::str_split(res, ",")
  check_av(unlist(spl[1]))
  purrr::map_dfr(spl[-1], api_string_to_df) %>%
    mutate(ticker = ticker) %>% select(ticker, everything())
}

#' Allowed are 5 calls per minute
get_all_vp_data_in_x <- function(x) {
  if (length(x) > 5) {
    stop("Too many tickers in x, alphavantage only allows 5 calls per minute")
  }
  api_data <- map(x, read_vp_api)
  map2_dfr(api_data, x, clean_vp_api_result)
}


get_vp_data <- function(x) {
  if (x <= 5) return(get_all_vp_data_in_x(x))
  groups <- ceiling(1:length(x) / 5)
  res <- vector("list", max(groups))
  for (i in 1:max(groups)) {
    res[[i]] <- get_all_vp_data_in_x(x[groups == i])
    Sys.sleep(60)
    print(glue::glue("Done {i} out of {max(x)} batches"))
  }
  bind_rows(res)
}
