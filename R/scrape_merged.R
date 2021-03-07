scrape_merged <- function() {
  "https://sheet2site-staging.herokuapp.com/api/v3/index.php?key=1ataJJQSe-DMwHk5QY7vgjWB-YkXy_aFNEBnO24Juvm8&g=1&e=1&e=1" %>%
    read_html() %>%
    html_nodes("td")
    html_table()
    as_tibble()
}
