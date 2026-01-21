# parkrunfunctions

<!-- badges: start -->
<!-- badges: end -->

The goal of `parkrunfunctions` is to provide a set of tools to scrape and analyse parkrun results from the official website. It allows you to retrieve the entire run history for a specific parkrunner, as well as the results for a specific parkrun event.

## Installation

You can install the development version of parkrunfunctions from [GitHub](https://github.com/) with:

```r
# install.packages("devtools")
devtools::install_github("sgbstats/parkrunfunctions")
```

## Dependencies

This package relies on the following R packages:
- `dplyr`
- `httr`
- `janitor`
- `rvest`
- `stringi`
- `stringr`
- `tidyr`
- `utils`

## Usage

Here's how to use the main functions of the package.

### 1. Get all runs for a specific parkrunner

To get the complete history of a parkrunner, you need their unique parkrun ID. You can find this ID in the URL of their results page (e.g., `https://www.parkrun.org.uk/parkrunner/1674/`).

The `get_all_runs()` function takes a Parkrunner ID and returns a list containing the runner's name, ID, and a data frame of all their results.

```r
# Replace '123456' with the actual Parkrunner ID
parkrunner_id <- 1674
runner_history <- get_all_runs(id = parkrunner_id)

# View the structure of the returned object
print(runner_history$name)
print(head(runner_history$results))
```

### 2. Get the results for a specific event

You can retrieve the results for a single parkrun event using the `get_result()` function. You can either provide the full URL of the results page or specify the `event` name and `event_no`.

```r
# Option 1: Using the URL
event_url <- "https://www.parkrun.org.uk/bushy/results/1/"
event_results <- get_result(url = event_url)

# Option 2: Using event name and number
event_results_alt <- get_result(event = "bushy", event_no = 1)


# The function returns a list with two data frames: results and volunteers
print(head(event_results$results))
print(event_results$volunteers)
```

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.
