# Define globals ####
`%>%` <- magrittr::`%>%`
ignore_case <- TRUE
perl <- TRUE


# combine_random_string_if_pattern ####

#' Conditionally return a random string
#'
#' Search for specified pattern in provided string, if found returns a 22
#'   character long random string.
#'
#' @param string A character string..
#' @param regex Character string regex specifying the pattern to look for in
#'   `string`.
#'
#' @return A length 1 character string vector.
#'
#' @examples
#' \dontrun{
#'   combine_random_string_if_pattern("random string", "random")
#'   combine_random_string_if_pattern("random string", "original")
#' }
combine_random_string_if_pattern <- function(string, regex){
  ifelse(
    grepl(regex, string, ignore_case, perl),
    stringi::stri_rand_strings(1L, 22L), string
  )
}


# combine_random_string_if_no_address ####

#' Conditionally return a random string
#'
#' Returns a 22 character long random string if address provided is labelled as
#'   missing.
#'
#' @param address A character string..
#'
#' @return A length 1 character string vector.
#'
#' @examples
#' \dontrun{
#'   combine_random_string_if_no_address(
#'     c("18, 20 London Road", "No trade address found")
#'   )
#' }
combine_random_string_if_no_address <- function(address){
  regex <- "^No.+address\\sfound$"
  combine_random_string_if_pattern(address, regex)
}


# combine_no_trade_address_to_randon_string ####

#' Mutate operation(s) in directory dataframe trade address
#'
#' Replaces missing trade addresses in the provided directory dataframe with
#'   random string. Random string only shows in body of trade address entries.
#'
#' @param directory A directory dataframe. Columns must include `address.trade`,
#'
#' @return A dataframe.
#'
#' @section Details:
#' Prevents unwarranted matches when matching general to trades directory.
#'   Unrelated records with similar name and trade address entry labelled as
#'   missing would be otherwise matched.
#'
#' @examples
#' \dontrun{
#'   directory <- data.frame(
#'     page = rep("71", 2L),
#'     rank = c("135", "326"),
#'     surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
#'     occupation = c("Wine and spirit merchant", "Baker"),
#'     type = rep("OWN ACCOUNT", 3L),
#'     address.trade = c("18, 20, London Road.", "No trade address found"),
#'     stringsAsFactors = FALSE
#'   )
#'   combine_no_trade_address_to_randon_string(directory)
#' }
combine_no_trade_address_to_randon_string <- function(directory){
  address.trade <- NULL

  dplyr::mutate(
    directory,
    address.trade = purrr::map_chr(
      address.trade, ~ combine_random_string_if_no_address(.x)
    )
  )
}


# combine_make_match_string ####

#' Mutate operation(s) in directory dataframe trade address
#'
#' Creates a 'match.string' column in the provided directory dataframe composed
#'   of entries full name and trade address pasted together.
#'   Missing trade address entries are replaced with a random generated string.
#'
#' @param directory A directory dataframe. Columns must include `forename`,
#'   `surname`, `occupation`, `address.trade.number`, `address.trade.body`.
#'
#' @return A dataframe.
#'
#' @section Details:
#' The purpose of the 'match.string' column is to facilitates the matching of
#'   the general to trades directory down the line. It allows to calculate a
#'   string distance metric between each pair of entries and match those falling
#'   below a specified threshold.
#'
#' @seealso \code{\link{combine_match_general_to_trades}} for the matching of
#'   the general to trades directory.
#'
#' @examples
#' \dontrun{
#'   directory <- data.frame(
#'     page = rep("71", 2L),
#'     surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
#'     occupation = c("Wine and spirit merchant", "Baker"),
#'     address.trade = c("18, 20, London Road.", "No trade address found"),
#'     stringsAsFactors = FALSE
#'   )
#'   combine_make_match_string(directory)
#' }
combine_make_match_string <- function(directory){
  address.trade <- name <- NULL

  tidyr::unite(
    directory,
    "address.trade", dplyr::matches("address.trade"),
    sep = ", ", remove = FALSE, na.rm = TRUE
  ) %>%
    tidyr::unite(
      "name", dplyr::matches("name$"), sep = " ", remove = FALSE, na.rm = TRUE
    ) %>%
    utils_clean_ends(name, address.trade) %>%
    combine_no_trade_address_to_randon_string() %>%
    tidyr::unite(
      "match.string", c(dplyr::matches("^name"), "address.trade"), sep = " - ",
      remove = FALSE, na.rm = TRUE
    ) %>%
    dplyr::select(-c(address.trade, name))
}


# combine_has_match_failed ####

#' Check for failed matches
#'
#' Provided with two equal length vectors, returns TRUE for indexes where both
#'   entries are "NA" and FALSE otherwise.
#'
#' @param number A vector of address numbers. Integer or character string.
#' @param body A character string vector of address bodies.
#'
#' @return A boolean vector.
#'
#' @examples
#' \dontrun{
#'   numbers <- c("18, 20", NA)
#'   bodies <- c("London Road.", NA)
#'   combine_has_match_failed(numbers, bodies)
#' }
combine_has_match_failed <- function(number, body){ (is.na(number) & is.na(body)) }


# combine_label_if_match_failed ####

#' Label failed matches
#'
#' Labels failed matches as such.
#'
#' @param type A Character string: "number", "body". Type of column to label.
#' @param ... Further arguments to be passed down to
#'   \code{\link{combine_has_match_failed}}
#'
#' @return A character string vector.
#'
#'
#' @examples
#' \dontrun{
#'   numbers <- c("18, 20", NA)
#'   bodies <- c("London Road.", NA)
#'   combine_label_if_match_failed("number", number = numbers, body = bodies)
#'   combine_label_if_match_failed("body", number = numbers, body = bodies)
#' }
combine_label_if_match_failed <- function(type = c("number", "body"), ...){
  txt <- switch(type, "number" = "", "body" = "Failed to match with general directory")
  args <- list(...)
  dplyr::if_else(combine_has_match_failed(...), txt, args[[type]])
}


# combine_get_address_house_type ####

#' Get house address column type
#'
#' Identifies the type of the house address column provided, number or body.
#'
#' @param column A Character string: ends in "house.number" or "house.body".
#'
#' @return A length 1 character string vector: "number" or "body".
#'
#' @examples
#' \dontrun{
#'   combine_get_address_house_type("address.house.number")
#'   combine_get_address_house_type("address.house.body")
#' }
combine_get_address_house_type <- function(column){
  regmatches(column, regexpr(globals_regex_get_address_house_type, column, perl = perl))
}


# combine_label_failed_matches ####

#' Label failed matches
#'
#' Labels failed matches as such in the provided directory dataframe.
#'
#' @param directory A directory dataframe. Columns must include `address.house.number`,
#'   `address.house.body`.
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   directory <- data.frame(
#'     page = rep("71", 2L),
#'     surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
#'     occupation = c("Wine and spirit merchant", "Baker"),
#'     address.trade.number = c("18, 20", "12"),
#'     address.house.number = c("136", NA),
#'     address.trade.body = c("London Road", "Dixon Place"),
#'     address.house.body = c("Queen Square", NA),
#'     stringsAsFactors = FALSE
#'   )
#'   combine_label_failed_matches(directory)
#' }
combine_label_failed_matches <- function(directory){
  dplyr::mutate(
    directory,
    dplyr::across(
      .cols = dplyr::matches(globals_regex_address_house_body_number),
      ~ combine_label_if_match_failed(
        type = combine_get_address_house_type(dplyr::cur_column()),
        number = address.house.number, body = address.house.body
      )
    )
  )
}


# combine_match_general_to_trades_plain ####

#' Match general to trades directory records
#'
#' Attempts to complement trades directory dataframe with house address
#'   information from the general directory dataframe provided by matching records
#'   from the two datasets using the distance metric specified.
#'
#' @param trades_directory A trades directory dataframe. Columns must include
#'   `surname`, `forename`, `address.trade.number`, `address.trade.body`.
#' @param general_directory A general directory dataframe. Columns must include
#'   `surname`, `forename`, `address.trade.number`, `address.trade.body`,
#'   `address.house.number`, `address.house.body`.
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#' @param ... Further arguments to be passed down to
#'   \code{\link[fuzzyjoin]{stringdist_left_join}}.
#'
#' @return A dataframe.
#'
#' @seealso \code{\link{combine_match_general_to_trades}}.
#'
#' @examples
#' \dontrun{
#'   trades_directory <- data.frame(
#'     page = rep("71", 2L),
#'     rank = c("135", "326", "586"),
#'     surname = c("Abbott", "Abercromby", "Blair"),
#'     forename = c("William", "Alexander", "John Hugh"),
#'     occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
#'     type = rep("OWN ACCOUNT", 3L),
#'     address.trade.number = c("18, 20", "12", "280"),
#'     address.trade.body = c("London Road", "Dixon Place", "High Street"),
#'     stringsAsFactors = FALSE
#'   )
#'   general_directory <- data.frame(
#'     page = rep("71", 2L),
#'     surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
#'     occupation = c("Wine and spirit merchant", "Baker"),
#'     address.trade.number = c("18, 20", ""),
#'     address.house.number = c("136", "29"),
#'     address.trade.body = c("London Road", "Dixon Place"),
#'     address.house.body = c("Queen Square", "Anderston Quay"),
#'     stringsAsFactors = FALSE
#'   )
#'   combine_match_general_to_trades_plain(
#'    trades_directory, general_directory, verbose = FALSE,
#'    method = "osa", max_dist = 5
#'   )
#' }
combine_match_general_to_trades_plain <- function(
  trades_directory, general_directory, verbose, ...
) {

  fun <- function(trades_directory, general_directory, ...) {
    match.string <- NULL

    trades_directory <- combine_make_match_string(trades_directory)
    general_directory <- combine_make_match_string(general_directory) %>%
      dplyr::select(dplyr::matches("^address.house"), match.string)

    combined <- fuzzyjoin::stringdist_left_join(
      x = trades_directory, y = general_directory, by = "match.string", ...
    )

    dplyr::select(combined, -dplyr::matches("match")) %>%
      combine_label_failed_matches()
  }

  utils_execute(verbose, fun, trades_directory, general_directory, ...)
}


# combine_match_general_to_trades_progress ####

#' Match general to trades directory records
#'
#' Attempts to complement trades directory dataframe with house address
#'   information from the general directory dataframe provided by matching records
#'   from the two datasets using the distance metric specified. Shows a progress
#'   bar indicating function progression.
#'
#' @param trades_directory A trades directory dataframe. Columns must include
#'   `surname`, `forename`, `address.trade.number`, `address.trade.body`.
#' @param general_directory A general directory dataframe. Columns must include
#'   `surname`, `forename`, `address.trade.number`, `address.trade.body`,
#'   `address.house.number`, `address.house.body`.
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#' @param ... Further arguments to be passed down to
#'   \code{\link[fuzzyjoin]{stringdist_left_join}}.
#'
#' @return A dataframe.
#'
#' @seealso \code{\link{combine_match_general_to_trades}}.
#'
#' @examples
#' \dontrun{
#'   trades_directory <- data.frame(
#'     page = rep("71", 2L),
#'     rank = c("135", "326", "586"),
#'     surname = c("Abbott", "Abercromby", "Blair"),
#'     forename = c("William", "Alexander", "John Hugh"),
#'     occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
#'     type = rep("OWN ACCOUNT", 3L),
#'     address.trade.number = c("18, 20", "12", "280"),
#'     address.trade.body = c("London Road", "Dixon Place", "High Street"),
#'     stringsAsFactors = FALSE
#'   )
#'   general_directory <- data.frame(
#'     page = rep("71", 2L),
#'     surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
#'     occupation = c("Wine and spirit merchant", "Baker"),
#'     address.trade.number = c("18, 20", ""),
#'     address.house.number = c("136", "29"),
#'     address.trade.body = c("London Road", "Dixon Place"),
#'     address.house.body = c("Queen Square", "Anderston Quay"),
#'     stringsAsFactors = FALSE
#'   )
#'   combine_match_general_to_trades_plain(
#'    trades_directory, general_directory, verbose = FALSE,
#'    method = "osa", max_dist = 5
#'   )
#' }
combine_match_general_to_trades_progress <- function(
  trades_directory, general_directory, verbose, ...
) {

  trades_directory_split <- split(
    trades_directory, (1L:nrow(trades_directory) %/% 100L)
  )

  pb <- progress::progress_bar$new(
    format = "  matching records [:bar] :percent eta: :eta",
    total = length(trades_directory_split), clear = FALSE, width = 100L
  )

  purrr::map_dfr(trades_directory_split, function(df) {
    pb$tick()
    combine_match_general_to_trades_plain(df, general_directory, verbose, ...)
  })
}


# combine_match_general_to_trades ####

#' Match general to trades directory records
#'
#' Attempts to complement trades directory dataframe with house address
#'   information from the general directory dataframe provided by matching records
#'   from the two datasets using the distance metric specified.
#'
#' @param trades_directory A trades directory dataframe. Columns must include
#'   `surname`, `forename`, `address.trade.number`, `address.trade.body`.
#' @param general_directory A general directory dataframe. Columns must include
#'   `surname`, `forename`, `address.trade.number`, `address.trade.body`,
#'   `address.house.number`, `address.house.body`.
#' @param progress Whether progress should be shown (`TRUE`) or not (`FALSE`).
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#' @param ... Further arguments to be passed down to
#'   \code{\link[fuzzyjoin]{stringdist_left_join}}.
#'
#' @return A tibble
#'
#' @examples
#' trades_directory <- tibble::tibble(
#'   page = rep("71", 3L),
#'   rank = c("135", "326", "586"),
#'   surname = c("Abbott", "Abercromby", "Blair"),
#'   forename = c("William", "Alexander", "John Hugh"),
#'   occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
#'   type = rep("OWN ACCOUNT", 3L),
#'   address.trade.number = c("18, 20", "12", "280"),
#'   address.trade.body = c("London Road", "Dixon Place", "High Street")
#' )
#' general_directory <- tibble::tibble(
#'   page = rep("71", 2L),
#'   surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
#'   occupation = c("Wine and spirit merchant", "Baker"),
#'   address.trade.number = c("18, 20", ""),
#'   address.house.number = c("136", "29"),
#'   address.trade.body = c("London Road", "Dixon Place"),
#'   address.house.body = c("Queen Square", "Anderston Quay")
#' )
#' combine_match_general_to_trades(
#'  trades_directory, general_directory, progress = TRUE, verbose = FALSE,
#'  method = "osa", max_dist = 5
#' )
#'
#' @export
combine_match_general_to_trades <- function(
  trades_directory, general_directory, progress = TRUE, verbose = FALSE, ...
){

  out <- if (progress)
    combine_match_general_to_trades_progress(
      trades_directory, general_directory, verbose, ...
    )
  else
    combine_match_general_to_trades_plain(
      trades_directory, general_directory, verbose, ...
    )

  tibble::as_tibble(out)
}






