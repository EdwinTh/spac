libs <- function() {
  library(tidyverse)
  library(rvest)
  library(lubridate)
}

check_struct <- function(x, values, res) {
  eq <- x == values
  if ( !all(eq) ) {
    stop(glue::glue("In {res}, expected {values[!eq]}, but saw {x[!eq]} at position(s) {which(!eq)}"))
  }
}

check_av <- function(x) {
  values <- c("timestamp", "open", "high", "low", "close","volume" )
  check_struct(x, values, "alphavantage")
}

check_spacs_table <- function(x) {
  values <- c("Symbol", "Name", "IPO date", "Market cap", "Merger pending?",
              "Leverage factor", "Momentum factor 10", "Momentum factor 200",
              "Last Close Price", "Shares outstanding", "Average trading volume",
              "% traded", "Action")
  check_struct(colnames(x), values, "SPACS table")
}
