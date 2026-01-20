#' Get All Results for a Parkrunner
#'
#' Iterates through a parkrunner's history and downloads result files.
#'
#' @param parkrunner An object of class `parkrun_results` (from `get_all_runs`).
#' @param folder Directory to save CSV files.
#' @param skip_errors Logical. If TRUE, skips events listed in `log_file`.
#' @param log_file Path to the error log file.
#' @param ... Additional arguments.
#'
#' @return Returns 0 on completion.
#' @export

get_all_results = function(
  parkrunner,
  folder,
  skip_errors = T,
  log_file = "error_log.txt",
  ...
) {
  if (class(parkrunner) != "parkrun_results") {
    stop("Input must be of class 'parkrun_results'")
  }
  results = parkrunner[["results"]]
  for (i in 1:nrow(results)) {
    event = results$event[i]
    eventno = results$run_number[i]
    url = results$url[i]
    errors = read.csv(log_file, header = T)

    if (event %in% errors$event && skip_errors) {
      next
    }

    file = paste0(folder, event, eventno, ".csv")

    if (file.exists(file)) {
      next
    }

    cat(paste(event, eventno, "\n"))
    tryCatch(
      {
        x = get_results(url = url)

        write.csv(x, file, row.names = F)
        Sys.sleep(25)
      },
      error = function(e) {
        message("❌ Error: ", conditionMessage(e))
        write(paste(event, eventno, sep = ","), file = log_file, append = TRUE)
      },
      warning = function(e) {
        message(conditionMessage(e))
      }
    )
  }
  return(0)
}