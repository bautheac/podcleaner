# Define globals ####
`%>%` <- magrittr::`%>%`
ignore_case <- TRUE
perl <- TRUE


# Functions ####

## clean_entries ####
#' Mutate operation(s) in trades directory dataframe column(s)
#'
#' Attempts to clean entries of the provided trades directory dataframe provided.
#'
#' @param directory A trades directory dataframe. Columns must include `occupation`,
#'   `forename`, `surname`, `address.trade.number`, `address.trade.body`.
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#'
#' @return A dataframe.
#'
#' @examples
#' directory <- data.frame(
#'   page = c("71", "71"),
#'   surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
#'   occupation = c("Wine and spirit mercht — See Advertisement in Appendix.", "Bkr"),
#'   address.trade.number = c("1S20", "I2"),
#'   address.trade.body = c("Londn rd.", "Dixen pl"),
#'   stringsAsFactors = FALSE
#' )
#' trades_clean_entries(directory, verbose = FALSE)
trades_clean_entries <- function(directory, verbose){

  clean <- function(...){
    # Trim extra white spaces ####
    utils_squish_all_columns(directory) %>%

    # Get rid of irrelevant info ####
      utils_clear_irrelevants(globals_regex_irrelevants, ignore_case) %>%

    # Clean occupations
      utils_clean_occupations() %>%

    # Clean names ####
      utils_clean_names() %>%

    # Clean addresses ####
      utils_clean_addresses()
  }

  utils_execute(verbose, clean, data = data, ignore_case = ignore_case)
}

#' Mutate operation(s) in trades directory dataframe column(s)
#'
#' Attempts to clean the provided trades directory dataframe provided.
#'
#' @param directory A trades directory dataframe. Columns must include `occupation`,
#'   `forename`, `surname` and `address.trade.number`, `address.trade.body`.
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#'
#' @return A dataframe.
#'
#' @examples
#' directory <- data.frame(
#'   page = c("71", "71"),
#'   surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
#'   occupation = c("Wine and spirit mercht — See Advertisement in Appendix.", "Bkr"),
#'   address.trade.number = c("1S20", "I2"),
#'   address.trade.body = c("Londn rd.", "Dixen pl"),
#'   stringsAsFactors = FALSE
#' )
#' trades_clean_directory(directory, verbose = FALSE)
#'
#' @export
trades_clean_directory <- function(data, verbose){

  clean <- function(...){
    ## Clean entries ####
    trades_clean_entries(data, verbose = verbose)
  }

  utils_execute(verbose, clean, data = data)
}
