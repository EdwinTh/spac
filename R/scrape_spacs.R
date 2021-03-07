
scrape_spacs <- function() {
  "https://stockmarketmba.com/listofshellcompanies.php" %>%
    read_html() %>%
    html_node("div table") %>%
    html_table() %>%
    as_tibble()
}

to_number <- function(x) {
  str_replace_all(x, "[^\\d.]", "") %>%
    as.numeric()
}

clean_spacs <- function(spacs_table_raw) {
  check_spacs_table(spacs_table_raw)
  janitor::clean_names(spacs_table_raw) %>%
    mutate(
      ipo_date               = mdy(ipo_date),
      market_cap             = to_number(market_cap),
      shares_outstanding     = to_number(shares_outstanding),
      average_trading_volume = to_number(average_trading_volume),
      percent_traded         = to_number(percent_traded)
    ) %>%
    select(-action)
}

create_tbl_current_spacs <- function() {
  current_spacs <- scrape_spacs() %>% clean_spacs()
  saveRDS(current_spacs, "final_data_files/current_spacs.Rds")
}
