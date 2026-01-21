#' Get All Runs for a Parkrunner
#'
#' Scrapes the "All Runs" page for a given Parkrunner ID.
#'
#' @param id The parkrun ID (numeric or string).
#' @param url The full URL to the parkrunner's "All Runs" page. If provided, this will not override the `id` parameter.
#' @param headers A named character vector of HTTP headers.
#'
#' @return A list of class `parkrun_results` containing the runner's name, ID, and a data frame of results.
#' @export
#' @importFrom stringr str_remove_all str_extract str_trim str_sub
#' @importFrom httr GET add_headers status_code content
#' @importFrom rvest read_html html_elements html_table html_nodes html_attr html_text2
#' @importFrom janitor clean_names
#' @importFrom stringi stri_trans_general
#' @importFrom dplyr mutate
get_all_runs = function(
  id=NULL,
  url=NULL,
  headers = c(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36",
    `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
    `Accept-Language` = "en-US,en;q=0.9",
    `Connection` = "keep-alive"
  )
) {

  if(is.null(id)){
    id = stringr::str_remove_all(as.character(url), "\\D")
  }
  id = stringr::str_remove_all(as.character(id), "\\D")

  response = httr::GET(
    glue::glue("https://www.parkrun.org.uk/parkrunner/{id}/all/"),
    add_headers(.headers = headers)
  )

  tryCatch(
    {
      if (status_code(response) != 200) {
        stop(sprintf("Request failed [%d] for %s", code, url))
      }
      html <- content(response, as = "text", encoding = "UTF-8") |> read_html()
      tables <- html |> html_elements("table#results")
      results = tables[[3]] |> html_table()
      href = tables[[3]] |>
        html_nodes("a") |>
        html_attr("href")

      results$url <- href[seq(3, length(href) + 2, by = 3)]

      results$short <- stringr::str_extract(results$url, "(?<=org.uk/)[^/]+")

      results = results |>
        janitor::clean_names() |>
        mutate(event = stri_trans_general(event, "Latin-ASCII"))

      h2_nodes <- html |> html_elements("h2")

      # Extract plain text
      h2_text <- h2_nodes |> html_text2()

      name <- str_extract(h2_text, "^[^(]+") |> str_trim() # everything before "("
      id <- str_extract(h2_text, "\\(([^)]+)\\)") |>
        str_remove_all("[()]") |>
        str_sub(2) |>
        as.numeric()

      out = list(name = name, id = id, results = results)

      structure(out, class = "parkrun_results")
    },
    error = function(e) {
      message(conditionMessage(e))
      NULL
    },
    warning = function(e) {
      message(conditionMessage(e))
    }
  )
}