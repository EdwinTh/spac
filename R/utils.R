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

check_merged_table <- function(x) {
  values <- c("Post-SPAC Ticker Symbol", "Post-SPAC Company Name", "Merged Company Name (if Different than DeSPAC)",
              "SPAC Name / Ticker", "Merger Completion Date (Announced)", "Post-Completion Events",
              "Total Return (from SPAC IPO price)", "Annualized Return (from SPAC IPO)",
              "Current Price", "% Change Prev", "Market Cap", "Warrant Trading Status",
              "Warrant Link", "SPAC IPO Date", "SPAC IPO Size (M)", "SPAC Initial Target Industry",
              "Notable Leadership / Sponsor", "SPAC Underwriter(s)", "Status",
              "Closing Press Release", "Merger Partner Leadership", "Est. Date Warrants Exercisable")
  check_struct(colnames(x), values, "merged table")
}
