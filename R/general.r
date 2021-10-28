# Define globals ####
`%>%` <- magrittr::`%>%`
ignore_case <- TRUE
perl <- TRUE


# Fix structure ####

## general_move_house_to_address ####

#' Mutate operation(s) in general directory dataframe column(s)
#'
#' For some raw general directory entries 'house' referring to address type lives
#'   in the occupation column as a result of parsing errors
#'   `general_move_house_to_address` attempts to move this information in the
#'   appropriate destination, the `addresses` column.
#'
#' @param directory A general directory dataframe. Columns must include `occupation`
#'   and  `addresses`.
#' @param regex Regex to use for the task provided as a character string.
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   directory <- data.frame(
#'     page = c("71", "71", "71"),
#'     surname = c("ABOT", "ABRCROMBIE", "BLAI"),
#'     forename = c("Wm.", "Alex", "Jn Huh"),
#'     occupation = c(
#'       "Wine and spirit merchant; house", "Baker", "Victualer"
#'     ),
#'     addresses = c("1820 Mary hill", "", "280, High stret"),
#'     stringsAsFactors = FALSE
#'   )
#'   regex <- globals_regex_house_to_address
#'   general_move_house_to_address(directory, regex)
#' }
general_move_house_to_address <- function(directory, regex){
  addresses <- occupation <- NULL
  dplyr::mutate(
    directory,
    addresses = utils_paste_if_found(
      regex, occupation, addresses, ignore_case, "house", addresses, sep = ", "
    ),
    occupation = utils_gsub_if_found(
      regex, occupation, regex, "", occupation, occupation, ignore_case, ignore_case
    )
  ) %>% utils_clean_address(type = "ends")
}


## general_repatriate_occupation_from_address ####

#' Mutate operation(s) in general directory dataframe column(s)
#'
#' For some raw general directory entries occupation information lives in the
#'   addresses column as a result of parsing errors
#'   `general_repatriate_occupation_from_address`
#'   attempts to move this information in the appropriate destination, the
#'   `occupation` column.
#'
#' @param directory A general directory dataframe. Columns must include `occupation` and
#'   `addresses`.
#' @param regex Regex to use for the task provided as a character string.
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   directory <- data.frame(
#'     page = c("71", "71", "71"),
#'     surname = c("ABOT", "ABRCROMBIE", "BLAI"), forename = c("Wm.", "Alex", "Jn Huh"),
#'     occupation = c(
#'       "", "Wine and spirit merchant", ""
#'     ),
#'     addresses = c("bkr; 1820, Mary hill", "", "Victualer; 280, High stret"),
#'     stringsAsFactors = FALSE
#'   )
#'   regex <- globals_regex_occupation_from_address
#'   general_repatriate_occupation_from_address(directory, regex)
#' }
general_repatriate_occupation_from_address <- function(directory, regex){
  addresses <- occupation <- NULL
  dplyr::mutate(
    directory,
    occupation = utils_paste_if_found(
      regex, addresses, occupation, ignore_case, occupation,
      regmatches(addresses, gregexpr(regex, addresses, ignore.case = TRUE, perl)),
      sep = " "
    ),
    addresses = utils_gsub_if_found(
      regex, addresses, regex, "", addresses, addresses, ignore_case, ignore_case
    ),
    dplyr::across(.cols = c(occupation, addresses), .fns = clean_string_ends)
  )
}


## general_split_trade_house_addresses ####

#' Mutate operation(s) in general directory dataframe column(s)
#'
#' Attempts to separate trade and house addresses in the general directory dataframe
#'   provided for entries for which both are provided.
#'
#' @param directory A general directory dataframe. Columns must include `addresses`.
#' @param regex Regex to use for the task provided as a character string.
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   directory <- data.frame(
#'     page = c("71", "71"),
#'     surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
#'     occupation = c("Wine and spirit merchant", "Baker"),
#'     addresses = c(
#'       "18, 20 London Street; ho. 136 Queen Street.",
#'       "12 Dixon Street; res, 29 Anderston Quay."
#'     ),
#'     stringsAsFactors = FALSE
#'   )
#'   regex <- globals_regex_house_split_trade
#'   general_split_trade_house_addresses(directory, regex, verbose = FALSE)
#' }
general_split_trade_house_addresses <- function(directory, regex, verbose){

  load <- function(...){
    addresses <- address.house <- NULL
    dplyr::mutate(
      directory,
      addresses = utils_clean_address_number(addresses)
    ) %>%
      tidyr::separate(
        col = addresses,
        into = c("addresses.trade", "address.house"),
        sep = regex, remove = TRUE, extra = "merge"
      ) %>%
      dplyr::mutate(
        address.house = ifelse(is.na(address.house), "", address.house)
      ) %>% utils_clean_address(type = "ends")
  }

  utils_execute(verbose, load, directory, regex)
}


## general_split_trade_addresses ####

#' Mutate operation(s) in general directory dataframe column(s)
#'
#' Attempts to separate multiple trade addresses in the general directory dataframe
#'   provided for entries for which more than one are provided.
#'
#' @param directory A general directory dataframe. Columns must include `addresses.trade`.
#' @param regex_split Regex to use to split addresses.
#' @param ignore_case_split Boolean specifying whether case should be ignored
#'   (`TRUE`) or not (`FALSE`) for `regex_split` above.
#' @param regex_filter Regex to use to search for address entries with post-split
#'   undesired leftovers.
#' @param ignore_case_filter Boolean specifying whether case should be ignored
#'   (`TRUE`) or not (`FALSE`) for `regex_filter` above.
#' @param regex_match Regex to use to clear address entries from post-split
#'   undesired leftovers.
#' @param ignore_case_match Boolean specifying whether case should be ignored
#'   (`TRUE`) or not (`FALSE`) for `regex_match` above.
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   directory <- data.frame(
#'     page = c("71", "71"),
#'     surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
#'     occupation = c("Wine and spirit merchant", "Baker"),
#'     addresses.trade = c(
#'       "18, 20 London Street, 136 Queen Street; 134, 136 South Portland Street",
#'       "12 Dixon Street, & 29 Anderston Quay; and 265 Argyle Street."
#'     ),
#'     stringsAsFactors = FALSE
#'   )
#'   regex_split <- globals_regex_split_trade_addresses
#'   regex_filter <- globals_regex_and_filter
#'   regex_match <- globals_regex_and_match
#'   general_split_trade_addresses(
#'     directory, regex_split, regex_filter, regex_match,
#'     ignore_case_split = FALSE, ignore_case_filter = TRUE, ignore_case_match = FALSE
#'   )
#' }
general_split_trade_addresses  <- function(
  directory,
  regex_split, ignore_case_split,
  regex_filter, ignore_case_filter,
  regex_match, ignore_case_match
){
  address.trade <- addresses.trade <- NULL
  dplyr::mutate(
    directory,
    address.trade = utils_regmatches_if_not_empty(
      addresses.trade, addresses.trade, regex_split, ignore_case_split
    )
  ) %>% tidyr::unnest(address.trade) %>% dplyr::select(-addresses.trade) %>%
    utils_clean_address(type = "ends")  %>%
    dplyr::mutate(
      address.trade = utils_regmatches_if_found(
        address.trade, regex_filter, address.trade, regex_match, address.trade,
        ignore_case_filter, ignore_case_match, not = FALSE
      ) %>% unlist()
    ) %>%
    utils_clean_address(type = "ends")
}


## general_split_address_numbers_bodies ####

#' Mutate operation(s) in general directory dataframe column(s)
#'
#' Attempts to separate number from body of address entries in general directory
#'   dataframe provided.
#'
#' @param directory A general directory dataframe. Columns must include
#'   `address.trade` and `address.house`.
#' @param regex_split_address_numbers Regex to use to match address number(s).
#' @param regex_split_address_body Regex to use to match address body(/ies).
#' @param regex_split_address_empty Regex to use to match empty address entries.
#' @param ignore_case_filter Boolean specifying whether case should be ignored
#'   (`TRUE`) or not (`FALSE`) for using one of the regexes above as filtering
#'   regex in \code{\link{utils_regmatches_if_found}}.
#' @param ignore_case_match Boolean specifying whether case should be ignored
#'   (`TRUE`) or not (`FALSE`) for using one of the regexes above as matching
#'   regex in \code{\link{utils_regmatches_if_found}}.
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   directory <- data.frame(
#'     page = c("71", "71"),
#'     surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
#'     occupation = c("Wine and spirit merchant", "Baker"),
#'     address.trade = c("18, 20 London Street", "12 Dixon Street"),
#'     address.house = c("136 Queen Street", "265 Argyle Street"),
#'     stringsAsFactors = FALSE
#'   )
#'   regex_split_address_numbers <- globals_regex_split_address_numbers
#'   regex_split_address_body <- globals_regex_split_address_body
#'   regex_split_address_empty <- globals_regex_split_address_empty
#'   general_split_address_numbers_bodies(
#'     directory, regex_split_address_numbers, regex_split_address_body,
#'     regex_split_address_empty, ignore_case_filter = TRUE, ignore_case_match = TRUE
#'   )
#' }
general_split_address_numbers_bodies <- function(
  directory, regex_split_address_numbers, regex_split_address_body,
  regex_split_address_empty, ignore_case_filter, ignore_case_match
){
  address.house <- address.trade <- NULL
  dplyr::mutate(
    directory,
    dplyr::across(
      .cols = c(address.trade, address.house),
      ~ utils_regmatches_if_found(
        string_filter = .x, regex_filter = globals_regex_split_address_numbers,
        string_search = .x, regex_search = globals_regex_split_address_numbers,
        default = "", ignore_case_filter, ignore_case_match, not = FALSE
      ),
      .names = "{.col}.number"
    ),
    dplyr::across(
      .cols = c(address.trade, address.house),
      ~ utils_regmatches_if_found(
        string_filter = .x, regex_filter = globals_regex_split_address_empty,
        string_search = .x, regex_search = globals_regex_split_address_body,
        default = "", ignore_case_filter, ignore_case_match, not = TRUE
      ),
      .names = "{.col}.body"
    )
  ) %>%
    dplyr::select(-c(address.trade, address.house)) %>%
    utils_clean_address(type = "ends")
}


## general_fix_structure ####

#' Mutate operation(s) in general directory dataframe column(s)
#'
#' Attempts to fix the structure of the raw general directory dataframe provided.
#'   For each entry, attempts to fix parsing errors by moving pieces if information
#'   provided in the right columns, attempts to separate trade from house address,
#'   attempt to separate multiple trade addresses, attempt to separate number from
#'   address body.
#'
#' @param directory A general directory dataframe. Columns must include `addresses`.
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   directory <- data.frame(
#'     page = c("71", "71"),
#'     surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
#'     occupation = c("Wine and spirit merchant; house", ""),
#'     addresses = c(
#'       "18, 20 London Street",
#'       "Baker; 12 Dixon Street, & 29 Anderston Quay; and 265 Argyle Street."
#'     ),
#'     stringsAsFactors = FALSE
#'   )
#'   general_fix_structure(directory, verbose = FALSE)
#' }
general_fix_structure <- function(directory, verbose){

  fix <- function(...){
    # Split trade and house addresses when both are provided ####
    # If occupation terminates in "; house" or variant, delete and add "house, "
    # to the beginning of addresses columns.
    general_move_house_to_address(directory, globals_regex_house_to_address) %>%

    # Fix occupation in addresses column when possible: move back to occupation column ####
      general_repatriate_occupation_from_address(
        globals_regex_occupation_from_address
      ) %>%

    # Get rid of "depot", "office", "store", "works" or "workshops" address prefix ####
      utils_remove_address_prefix(globals_regex_address_prefix, ignore_case) %>%

    # Create trade and house addresses by splitting raw addresses on "house" or variant ####
    # If "residence" matches, don't match "house", otherwise match "house".
      general_split_trade_house_addresses(
        globals_regex_house_split_trade, verbose = verbose
      ) %>%

    # Split multiple trade addresses ####
      general_split_trade_addresses(
        regex_split = globals_regex_split_trade_addresses, ignore_case_split = FALSE,
        regex_filter = globals_regex_and_filter, ignore_case_filter = TRUE,
        regex_match = globals_regex_and_match, ignore_case_match = FALSE
      ) %>%

    # Split numbers and address bodies ####
      general_split_address_numbers_bodies(
        globals_regex_split_address_numbers, globals_regex_split_address_body,
        globals_regex_split_address_empty, ignore_case_filter = TRUE,
        ignore_case_match = TRUE
      )
  }

  utils_execute(verbose, fix, directory = directory, ignore_case = ignore_case)
}


# Clean ####

## general_clean_entries ####

#' Mutate operation(s) in general directory dataframe column(s)
#'
#' Attempts to clean entries of the provided general directory dataframe provided.
#'
#' @param directory A general directory dataframe. Columns must include `occupation`,
#'   `forename`, `surname`, `address.house.number`, `address.house.body` and/or
#'   `address.trade.number`, `address.trade.body`.
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   directory <- data.frame(
#'     page = c("71", "71"),
#'     surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
#'     occupation = c("Wine and spirit mercht — See Advertisement in Appendix.", "Bkr"),
#'     address.trade.number = c("1S20", "I2"),
#'     address.house.number = c("13<J", "2G5"),
#'     address.trade.body = c("Londn rd.", "Dixen pl"),
#'     address.house.body = c("Queun sq", "Argul st"),
#'     stringsAsFactors = FALSE
#'   )
#'   general_clean_entries(directory, verbose = FALSE)
#' }
general_clean_entries <- function(directory, verbose){

  clean <- function(...){
    ### Get rid of irrelevant info
    utils_clear_irrelevants(directory, globals_regex_irrelevants, ignore_case) %>%

      ### Clean occupation ####
      utils_clean_occupations() %>%

      ### Clean name ####
      utils_clean_names() %>%

      ### Clean addresses
      utils_clean_addresses()
  }

  utils_execute(verbose, clean, directory = directory, ignore_case = ignore_case)
}


## general_clean_directory_plain ####

#' Mutate operation(s) in general directory dataframe column(s)
#'
#' Attempts to clean the provided general directory dataframe provided.
#'
#' @param directory A general directory dataframe. Columns must include `occupation`,
#'   `forename`, `surname` and `addresses`.
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   directory <- data.frame(
#'     page = rep("71", 2L),
#'     surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
#'     occupation = c("Wine and spirit mercht — See Advertisement in Appendix.", ""),
#'     addresses = c(
#'       "1S20 Londn rd; ho. 13<J Queun sq",
#'       "Bkr; I2 Dixon Street, & 29 Auderstn Qu.; res 2G5 Argul st."
#'     ),
#'     stringsAsFactors = FALSE
#'   )
#'   general_clean_entries(directory, verbose = FALSE)
#' }
general_clean_directory_plain <- function(directory, verbose){

  clean <- function(...){
    # Fix structure ####
    general_fix_structure(directory, verbose = verbose) %>%

    # Clean entries ####
      general_clean_entries(verbose = verbose)
  }

  utils_execute(verbose, clean, directory = directory)
}


## general_clean_directory_progress ####

#' Mutate operation(s) in general directory dataframe column(s)
#'
#' Attempts to clean the provided general directory dataframe provided. Shows a
#'   progress bar indication the progression of the function.
#'
#' @param directory A general directory dataframe. Columns must include `occupation`,
#'   `forename`, `surname` and `addresses`.
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   directory <- data.frame(
#'     page = c("71", "71"),
#'     surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
#'     occupation = c("Wine and spirit mercht — See Advertisement in Appendix.", ""),
#'     addresses = c(
#'       "1S20 Londn rd; ho. 13<J Queun sq",
#'       "Bkr; I2 Dixon Street, & 29 Auderstn Qu.; res 2G5 Argul st."
#'     ),
#'     stringsAsFactors = FALSE
#'   )
#'   general_clean_entries(directory, verbose = FALSE)
#' }
general_clean_directory_progress <- function(directory, verbose){

  directory_split <- split(directory, (1L:nrow(directory) %/% 500L))

  pb <- progress::progress_bar$new(
    format = "  cleaning records [:bar] :percent eta: :eta",
    total = length(directory_split), clear = FALSE, width = 100L
  )

  purrr::map_dfr(directory_split, function(df) {
    pb$tick(); general_clean_directory_plain(df, verbose)
  })
}


## general_clean_directory ####

#' Mutate operation(s) in general directory dataframe column(s)
#'
#' Attempts to clean the provided general directory dataframe provided.
#'
#' @param directory A general directory dataframe. Columns must include `occupation`,
#'   `forename`, `surname` and `addresses`.
#' @param progress Whether progress should be shown (`TRUE`) or not (`FALSE`).
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#'
#' @return A tibble
#'
#' @examples
#' directory <- tibble::tibble(
#'   page = rep("71", 2L),
#'   surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
#'   occupation = c("Wine and spirit mercht — See Advertisement in Appendix.", ""),
#'   addresses = c(
#'     "1S20 Londn rd; ho. 13<J Queun sq",
#'     "Bkr; I2 Dixon Street, & 29 Auderstn Qu.; res 2G5 Argul st."
#'   )
#' )
#' general_clean_directory(directory, progress = TRUE, verbose = FALSE)
#'
#' @export
general_clean_directory <- function(directory, progress = TRUE, verbose = FALSE){

  out <- if (progress) general_clean_directory_progress(directory, verbose)
  else general_clean_directory_plain(directory, verbose)

  tibble::tibble(out)
}

