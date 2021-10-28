# Define globals ####
`%>%` <- magrittr::`%>%`
ignore_case <- TRUE
perl <- TRUE


# Functions ####

## trades_clean_entries ####

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
#' \dontrun{
#'   directory <- data.frame(
#'     page = rep("71", 3L),
#'     rank = c("135", "326", "586"),
#'     surname = c("ABOT", "ABRCROMBIE", "BLAI"),
#'     forename = c("Wm.", "Alex", "Jhn Hug"),
#'     occupation = c(
#'       "Wine and spirit mercht — See Advertisement in Appendix.", "Bkr",
#'       "Victualer"
#'     ),
#'     type = rep("OWN ACCOUNT", 3L),
#'     address.trade.number = c("1S20", "I2", "2S0"),
#'     address.trade.body = c("Londn rd.", "Dixen pl", "High St."),
#'     stringsAsFactors = FALSE
#'   )
#'   trades_clean_entries(directory, verbose = FALSE)
#' }
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

  utils_execute(verbose, clean, directory, ignore_case)
}


## trades_clean_directory_plain ####

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
#' \dontrun{
#'   directory <- data.frame(
#'     page = rep("71", 2L),
#'     rank = c("135", "326"),
#'     surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
#'     occupation = c(
#'       "Wine and spirit mercht — See Advertisement in Appendix.", "Bkr"
#'     ),
#'     type = rep("OWN ACCOUNT", 3L),
#'     address.trade.number = c("1S20", "I2"),
#'     address.trade.body = c("Londn rd.", "Dixen pl"),
#'     stringsAsFactors = FALSE
#'   )
#'   trades_clean_directory(directory, verbose = FALSE)
#' }
trades_clean_directory_plain <- function(directory, verbose){

  clean <- function(...){
    ## Clean entries ####
    trades_clean_entries(directory, verbose = verbose)
  }

  utils_execute(verbose, clean, directory = directory)
}


## trades_clean_directory_progress ####

#' Mutate operation(s) in trades directory dataframe column(s)
#'
#' Attempts to clean the provided trades directory dataframe provided. Shows a
#'   progress bar indicating function progression.
#'
#' @param directory A trades directory dataframe. Columns must include `occupation`,
#'   `forename`, `surname` and `address.trade.number`, `address.trade.body`.
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   directory <- data.frame(
#'     page = rep("71", 2L),
#'     rank = c("135", "326"),
#'     surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
#'     occupation = c(
#'       "Wine and spirit mercht — See Advertisement in Appendix.", "Bkr"
#'     ),
#'     type = rep("OWN ACCOUNT", 3L),
#'     address.trade.number = c("1S20", "I2"),
#'     address.trade.body = c("Londn rd.", "Dixen pl"),
#'     stringsAsFactors = FALSE
#'   )
#'   trades_clean_directory(directory, verbose = FALSE)
#' }
trades_clean_directory_progress <- function(directory, verbose){

  directory_split <- split(directory, (1L:nrow(directory) %/% 500L))

  pb <- progress::progress_bar$new(
    format = "  cleaning records [:bar] :percent eta: :eta",
    total = length(directory_split), clear = FALSE, width = 100L
  )

  purrr::map_dfr(directory_split, function(sample) {
    pb$tick(); trades_clean_directory_plain(sample, verbose)
  })
}


## trades_clean_directory ####

#' Mutate operation(s) in trades directory dataframe column(s)
#'
#' Attempts to clean the provided trades directory dataframe provided.
#'
#' @param directory A trades directory dataframe. Columns must include `occupation`,
#'   `forename`, `surname` and `address.trade.number`, `address.trade.body`.
#' @param progress Whether progress should be shown (`TRUE`) or not (`FALSE`).
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#'
#' @return A tibble
#'
#' @examples
#' directory <- tibble::tibble(
#'   page = rep("71", 2L),
#'   rank = c("135", "326"),
#'   surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
#'   occupation = c(
#'     "Wine and spirit mercht — See Advertisement in Appendix.", "Bkr"
#'   ),
#'   type = rep("OWN ACCOUNT", 2L),
#'   address.trade.number = c("1S20", "I2"),
#'   address.trade.body = c("Londn rd.", "Dixen pl")
#' )
#' trades_clean_directory(directory, progress = TRUE, verbose = FALSE)
#'
#' @export
trades_clean_directory <- function(directory, progress = TRUE, verbose = FALSE){

  out <- if (progress) trades_clean_directory_progress(directory, verbose)
  else trades_clean_directory_plain(directory, verbose)

  tibble::tibble(out)
}
