#' Get Parkrun Results and Volunteer IDs
#'
#' Scrapes the results table and volunteer IDs from a specific Parkrun event URL.
#'
#' @param url A character string specifying the URL of the parkrun results page.
#' @param event The parkrun event short name (e.g., "bushy"). Required if `url` is not provided.
#' @param event_no The parkrun event number (e.g., 1).
#' @param headers A named character vector of HTTP headers to use for the request.
#'
#' @return A list containing two elements:
#' \describe{
#'   \item{results}{A data frame (tibble) with parkrun results (position, parkrunner, time).}
#'   \item{volunteers}{A dataframe with volunteer IDs and names.}
#'
#' }
#' @export
#' @importFrom httr GET add_headers timeout status_code content
#' @importFrom rvest read_html html_element html_table html_nodes html_attr html_text
#' @importFrom dplyr select mutate
#' @importFrom stringr str_extract str_trim
#' @importFrom tidyr drop_na
#' @importFrom glue glue
get_result = function(
  url=NULL,
  event=NULL,
  event_no=NULL,
  headers = c(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36",
    `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
    `Accept-Language` = "en-US,en;q=0.9",
    `Connection` = "keep-alive"
  )
) {

  if(is.null(url)) {
    if(is.null(event) | is.null(event_no)) {
      stop("Either 'url' or both 'event' and 'event_no' must be provided.")
    }
    url = glue::glue("https://www.parkrun.org.uk/{event}/results/{event_no}/")
  }
  response = httr::GET(url, add_headers(.headers = headers), timeout(15))
  tryCatch(
    {
      if (status_code(response) != 200) {
      stop(sprintf("Request failed [%d] for %s", status_code(response), url))
      }
      html <- content(response, as = "text", encoding = "UTF-8") |> read_html()
      tables <- html |> html_element("div.Results.Results")


      all_links <- tables |>
        html_nodes("a") |>
        html_attr("href")
      hyperlinks <- all_links[grepl("/parkrunner/", all_links)] |> 
        stringr::str_extract( "\\d+(?=[^\\d]*$)")

      results <- tables |> html_table() |> dplyr::select(c(1, 2, 6))

      names(results) = c("pos", "parkrunner", "time")
      results = results |>
        mutate(
          parkrunner = str_extract(parkrunner, "^[^0-9]*") |> str_trim(),
          time = str_extract(time, "^[0-9:]+")
        ) |>
        drop_na(time) |> 
        cbind.data.frame("id"=hyperlinks)

      # Extract volunteer URLs
      volunteer_nodes<- html |>
        html_nodes(xpath = "//p[contains(., 'We are very grateful to the volunteers who made this event happen')]//a")
      
      volunteer_urls <- volunteer_nodes |>
        html_attr("href")

      volunteer_ids <- stringr::str_extract(volunteer_urls, "\\d+(?=[^\\d]*$)")

      volunteer_names=volunteer_nodes |> 
        html_text()
    

      structure(
        list(
          results = results,
          volunteers = cbind.data.frame("id"=volunteer_ids, "parkrunner"=volunteer_names)
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