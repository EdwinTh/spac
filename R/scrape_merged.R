scrape_merged <- function() {
  "https://sheet2site-staging.herokuapp.com/api/v3/index.php?key=1ataJJQSe-DMwHk5QY7vgjWB-YkXy_aFNEBnO24Juvm8&g=1&e=1&e=1" %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table() %>% `[[`(1) %>%
    as_tibble()
}

clean_merged <- function(merged_table_raw) {
  check_merged_table(merged_table_raw)
  janitor::clean_names(merged_table_raw) %>%
    separate(spac_name_ticker, c("spac_name", "spac_symbol"), "\\(") %>%
    mutate(
      spac_symbol                      = str_replace(spac_symbol, "\\)", ""),
      merger_completion_date_announced = as.Date(merger_completion_date_announced),
      market_cap                       = to_number(market_cap),
      spac_ipo_date                    = as.Date(spac_ipo_date),
      spac_ipo_size_m                  = to_number(spac_ipo_size_m)
    ) %>%
    select(-c(total_return_from_spac_ipo_price, annualized_return_from_spac_ipo,
              current_price, percent_change_prev))
}

create_tbl_merged <- function() {
  current_merged <- scrape_merged() %>% clean_merged()
  saveRDS(current_merged, "final_data_files/current_merged.Rds")
}

