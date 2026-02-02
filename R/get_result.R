#' Get Parkrun Results and Volunteer IDs
#'
#' Scrapes the results table and volunteer IDs from a specific Parkrun event URL.
#'
#' @param url A character string specifying the URL of the parkrun results page. Non-uk prs need a url or update the domain.
#' @param event The parkrun event short name (e.g., "bushy"). Required if `url` is not provided.
#' @param event_no The parkrun event number (e.g., 1).
#' @param domain The parkrun domain (default is "parkrun.org.uk").
#' @param headers A named character vector of HTTP headers to use for the request.
#' @param as_hms Return times as hms
#' @param as_Date Return dates as Date
#'
#' @return A list containing two elements:
#' \describe{
#'   \item{results}{A data frame (tibble) with parkrun results (position, parkrunner, time, ag, id).}
#'   \item{volunteers}{A dataframe with volunteer IDs and names.}
#'   \item{date}{The date of the event in ISO format (YYYY-MM-DD).}
#'
#' }
#' @export
#' @importFrom httr GET add_headers timeout status_code content
#' @importFrom rvest read_html html_element html_table html_nodes html_attr html_text
#' @importFrom dplyr select mutate
#' @importFrom stringr str_extract str_trim str_length
#' @importFrom tidyr drop_na
#' @importFrom glue glue
#' @importFrom hms as_hms
#' @importFrom lubridate as_date
#' @importFrom stats time
get_result = function(
  url = NULL,
  event = NULL,
  event_no = NULL,
  domain = "parkrun.org.uk",
  headers = c(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36",
    `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
    `Accept-Language` = "en-US,en;q=0.9",
    `Connection` = "keep-alive"
  ),
  as_hms = FALSE,
  as_Date = FALSE
) {
  if (is.null(url)) {
    if (is.null(event) | is.null(event_no)) {
      stop("Either 'url' or both 'event' and 'event_no' must be provided.")
    }
    url = glue::glue("https://{domain}/{event}/results/{event_no}/")
  }
  response = httr::GET(url, add_headers(.headers = headers), timeout(15))
  tryCatch(
    {
      if (status_code(response) != 200) {
        stop(sprintf("Request failed [%d] for %s", status_code(response), url))
      }
      html <- content(response, as = "text", encoding = "UTF-8") |> read_html()

      event_date <- html |>
        html_element("span.format-date") |>
        html_text() |>
        lubridate::as_date(format = "%d/%m/%Y")

      if (!as_Date) {
        event_date = format(event_date, "%Y-%m-%d")
      }

      tables <- html |> html_element("div.Results.Results")

      all_links <- tables |>
        html_nodes("a") |>
        html_attr("href")
      hyperlinks <- all_links[grepl("/parkrunner/", all_links)] |>
        stringr::str_extract("\\d+(?=[^\\d]*$)")

      results <- tables |> html_table() |> dplyr::select(c(1, 2, 6, 4))

      names(results) = c("pos", "parkrunner", "time", "ag")
      results = results |>
        mutate(
          parkrunner = str_extract(parkrunner, "^[^0-9]*") |> str_trim(),
          time = str_extract(time, "^[0-9:]+"),
          ag = substr(
            ag,
            (gregexpr(pattern = "\\.", ag)[[1]][1] - 2),
            (gregexpr(pattern = "%", ag)[[1]][1] - 1)
          ),
          .by = pos
        ) |>
        drop_na(time) |>
        cbind.data.frame("id" = hyperlinks) |>
        mutate(
          pos = as.integer(pos),
          ag = as.numeric(ag)
        )

      if (as_hms) {
        results = results |>
          mutate(
            time = as_hms(dplyr::if_else(
              stringr::str_length(time) == 5,
              paste0("00:", time),
              time
            ))
          )
      }
      # Extract volunteer URLs
      volunteer_nodes <- html |>
        html_nodes(
          xpath = "//p[contains(., 'We are very grateful to the volunteers who made this event happen')]//a"
        )

      volunteer_urls <- volunteer_nodes |>
        html_attr("href")

      volunteer_ids <- stringr::str_extract(volunteer_urls, "\\d+(?=[^\\d]*$)")

      volunteer_names = volunteer_nodes |>
        html_text()

      structure(
        list(
          results = results,
          volunteers = cbind.data.frame(
            "id" = volunteer_ids,
            "parkrunner" = volunteer_names
          ),
          date = event_date
        ),
        class = "parkrun_results"
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
