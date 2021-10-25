# Define globals ####
`%>%` <- magrittr::`%>%`
ignore_case <- TRUE
perl <- TRUE


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
#' combine_random_string_if_pattern("random string", "random")
#' combine_random_string_if_pattern("random string", "original")
combine_random_string_if_pattern <- function(string, regex){
  ifelse(
    grepl(regex, string, ignore_case, perl),
    stringi::stri_rand_strings(1L, 22L), string
  )
}

#' Conditionally return a random string
#'
#' Returns a 22 character long random string if address provided is labeled as
#'   missing.
#'
#' @param address A character string..
#'
#' @return A length 1 character string vector.
#'
#' @examples
#' combine_random_string_if_no_address(c("18, 20 London Road", "No trade address found"))
combine_random_string_if_no_address <- function(address){
  regex <- "^No.+address\\sfound$"
  combine_random_string_if_pattern(address, regex)
}

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
#'   Unrelated records with similar name and trade address entry labeled as
#'   missing would be otherwise matched.
#'
#' @examples
#' directory <- data.frame(
#'   page = c("71", "71"),
#'   surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
#'   occupation = c("Wine and spirit merchant", "Baker"),
#'   address.trade = c("18, 20, London Road.", "No trade address found"),
#'   stringsAsFactors = FALSE
#' )
#' combine_no_trade_address_to_randon_string(directory)
combine_no_trade_address_to_randon_string <- function(directory){
  dplyr::mutate(
    directory,
    address.trade = purrr::map_chr(
      address.trade, ~ combine_random_string_if_no_address(.x)
    )
  )
}

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
#' @seealso \code{\link{match_records}} for the matching of the general to
#'   trades directory.
#'
#' @examples
#' directory <- data.frame(
#'   page = c("71", "71"),
#'   surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
#'   occupation = c("Wine and spirit merchant", "Baker"),
#'   address.trade = c("18, 20, London Road.", "No trade address found"),
#'   stringsAsFactors = FALSE
#' )
#' combine_make_match_string(directory)
combine_make_match_string <- function(directory){
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
#' numbers <- c("18, 20", NA)
#' bodies <- c("London Road.", NA)
#' combine_has_match_failled(numbers, bodies)
combine_has_match_failled <- function(number, body){ (is.na(number) & is.na(body)) }

#' Label failed matches
#'
#' Labels failed matches as such.
#'
#' @param type A Character string: "number", "body". Type of column to label.
#' @param ... Further arguments to be passed down to \code{\link{has_match_failled}}
#'
#' @return A character string vector.
#'
#'
#' @examples
#' numbers <- c("18, 20", NA)
#' bodies <- c("London Road.", NA)
#' combine_label_if_match_failled("number", number = numbers, body = bodies)
#' combine_label_if_match_failled("body", number = numbers, body = bodies)
combine_label_if_match_failled <- function(type = c("number", "body"), ...){
  txt <- switch(type, "number" = "", "body" = "Failled to match with general directory")
  args <- list(...)
  dplyr::if_else(combine_has_match_failled(...), txt, args[[type]])
}

#' Get house address column type
#'
#' Identifies the type of the house address column provided, number or body.
#'
#' @param column A Character string: ends in "house.number" or "house.body".
#'
#' @return A length 1 character string vector: "number" or "body".
#'
#' @examples
#' combine_get_address_house_type("address.house.number")
#' combine_get_address_house_type("address.house.body")
combine_get_address_house_type <- function(column){
  regmatches(column, regexpr(globals_regex_get_address_house_type, column, perl = perl))
}

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
#' directory <- data.frame(
#'   page = c("71", "71"),
#'   surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
#'   occupation = c("Wine and spirit merchant", "Baker"),
#'   address.trade.number = c("18, 20", "12"),
#'   address.house.number = c("136", NA),
#'   address.trade.body = c("London Road", "Dixon Place"),
#'   address.house.body = c("Queen Square", NA),
#'   stringsAsFactors = FALSE
#' )
#' combine_label_failled_matches(directory)
combine_label_failled_matches <- function(directory){
  dplyr::mutate(
    directory,
    dplyr::across(
      .cols = dplyr::matches(globals_regex_address_house_body_number),
      ~ combine_label_if_match_failled(
        type = combine_get_address_house_type(dplyr::cur_column()),
        number = address.house.number, body = address.house.body
      )
    )
  )
}


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
#' @examples
#' trades_directory <- data.frame(
#'   page = c("71", "71"),
#'   surname = c("Abbott", "Abercromby", "Blair"),
#'   forename = c("William", "Alexander", "John Hugh"),
#'   occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
#'   address.trade.number = c("18, 20", "12", "280"),
#'   address.trade.body = c("London Road", "Dixon Place", "High Street"),
#'   stringsAsFactors = FALSE
#' )
#' general_directory <- data.frame(
#'   page = c("71", "71"),
#'   surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
#'   occupation = c("Wine and spirit merchant", "Baker"),
#'   address.trade.number = c("18, 20", ""),
#'   address.house.number = c("136", "29"),
#'   address.trade.body = c("London Road", "Dixon Place"),
#'   address.house.body = c("Queen Square", "Anderston Quay"),
#'   stringsAsFactors = FALSE
#' )
#' combine_match_general_to_trades(
#'  trades_directory, general_directory, method = "osa", verbose = FALSE, max_dist = 5
#' )
#'
#' @export
combine_match_general_to_trades <- function(trades_directory, general_directory, verbose, ...){

  fun <- function(trades_directory, general_directory, ...) {
    trades_directory <- combine_make_match_string(trades_directory)
    general_directory <- combine_make_match_string(general_directory) %>%
      dplyr::select(dplyr::matches("^address.house"), match.string)

    combined <- fuzzyjoin::stringdist_left_join(
      x = trades_directory, y = general_directory, by = "match.string", ...
    )

    dplyr::select(combined, -dplyr::matches("match")) %>%
      combine_label_failled_matches()
  }

  utils_execute(
    verbose, fun, trades_directory = trades_directory,
    general_directory = general_directory, ...
  )
}






