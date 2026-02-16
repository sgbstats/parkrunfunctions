#' Get Parkrun Results and Volunteer IDs
#'
#' Scrapes the results table and volunteer IDs from a specific Parkrun event URL.
#'
#' @param url A character string specifying the URL of the parkrun results page. Non-uk prs need a url or update the domain.
#' @param event The parkrun event short name (e.g., "bushy"). Required if `url` is not provided.
#' @param domain The parkrun domain (default is "parkrun.org.uk").
#' @param headers A named character vector of HTTP headers to use for the request.

#' @export
#' @importFrom httr GET add_headers timeout status_code content
#' @importFrom rvest read_html html_element html_table html_nodes html_attr html_text
#' @importFrom dplyr select mutate if_else
#' @importFrom glue glue
get_event_history = function(
  url = NULL,
  event = NULL,
  domain = "parkrun.org.uk",
  headers = c(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36",
    `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
    `Accept-Language` = "en-US,en;q=0.9",
    `Connection` = "keep-alive"
  )
) {
  if (is.null(url)) {
    if (is.null(event)) {
      stop("Either 'url' or both 'event' must be provided.")
    }
    url = glue::glue("https://{domain}/{event}/results/eventhistory/")
  }
  response = httr::GET(url, add_headers(.headers = headers), timeout(15))
  tryCatch(
    {
      if (status_code(response) != 200) {
        stop(sprintf("Request failed [%d] for %s", status_code(response), url))
      }
      html <- content(response, as = "text", encoding = "UTF-8") |> read_html()

      tables <- html |> html_element("table.Results-table")

      results <- tables |>
        html_table() |>
        select("event_no" = 1, "date" = 2, "finishers" = 3, "volunteers" = 4) |>
        mutate(
          volunteers = if_else(
            volunteers == "Unknown",
            NA_character_,
            volunteers
          ),
          event_no = as.integer(event_no),
          date = substr(date, 1, 10) |> as.Date()
        ) |>
        mutate(
          test = gsub("([0-9]+).*$", "\\1", finishers),
          finishers = as.integer(substr(
            test,
            1,
            (str_length(test) - 2) / 2 + 1
          )),
          test2 = gsub("([0-9]+).*$", "\\1", volunteers),
          volunteers = as.integer(substr(
            test2,
            1,
            (str_length(test2) - 2) / 2 + 1
          )),
          .by = event_no
        ) |>
        select(event_no, date, finishers, volunteers)

      extract_segment <- function(url) {
        parts <- strsplit(url, "/", fixed = TRUE)
        vapply(
          parts,
          function(p) if (length(p) >= 4) p[4] else NA_character_,
          FUN.VALUE = character(1)
        )
      }

      structure(
        list(
          "history" = results,
          "name" = extract_segment(url)
        ),
        class = c("parkrun_event_history")
      )
    },
    error = function(e) {
      stop(conditionMessage(e))
    },
    warning = function(e) {
      warning(conditionMessage(e))
    }
  )
}
