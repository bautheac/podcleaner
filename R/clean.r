# Define globals ####
`%>%` <- magrittr::`%>%`
ignore_case <- TRUE
perl <- TRUE


# Main ####

## Address ####

### clean_address_body ####

#' Clean address entries body
#'
#' Attempts to clean body of address entries provided.
#'
#' @param addresses A character string vector of address(es).
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_address_body(c("1S20 Londun st.", "13<J Dixon rd"))
#' }
clean_address_body <- function(addresses){

  # Pre-clean
  clean <- clean_address_pre_clean(addresses)
  # Places
  clean <- clean_address_places(clean)
  # Worksites
  clean <- clean_address_worksites(clean)
  # Names
  clean <- clean_address_names(clean)
  # Post clean
  clean <- clean_address_post_clean(clean)

  clean
}

### clean_address_number ####

#' Clean address entry numbers
#'
#' Attempts to clean number of address entries provided.
#'
#' @param addresses A character string vector of address(es).
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_address_number(c("1S20 Londun st.", "13<J Dixon rd"))
#' }
clean_address_number <- function(addresses){

  clean <- clean_string_ends(addresses)

  # OCR errors

  ## clean misread numbers
  purrr::pwalk(globals_numbers, function(pattern, replacement, ignore_case){
    clean <<- gsub(pattern, replacement, clean, ignore.case = ignore_case, perl = perl)
  })

  ## get rid of random chars around digits
  clean <- gsub('(?<=\\d)["*-](?=\\d)?', "", clean, ignore.case = FALSE, perl = perl)
  clean <- gsub("(?<=\\d)['](?=\\d)?", "", clean, ignore.case = FALSE, perl = perl)


  # separate 2 3-digit address numbers stuck together
  clean <- gsub(
    "(\\d{2,3})\\s?(\\d{2,3})(?!\\/)", "\\1, \\2", clean,
    ignore.case = ignore_case, perl = perl
  )


  # separate address numbers with commas in place of "&" or "and"
  clean <- gsub(
    "(\\d),?\\s?(?:f|and|&|,)\\s?(\\d)", "\\1, \\2", clean,
    ignore.case = ignore_case, perl = perl
  )


  # harmonise address ranges to first number-second number
  clean <- gsub(
    "(?<=\\d),?\\s?to,?\\s?(?=\\d)", "-", clean,
    ignore.case = ignore_case, perl = perl
  )
  clean <- gsub(
    "(\\d+)\\s(\\d)+", "\\1-\\2", clean, ignore.case = ignore_case, perl = perl
  )


  # separate numbers from words
  clean <- gsub(
    "((?<=\\d))((?=[a-z]))", "\\1 \\2", clean,
    ignore.case = ignore_case, perl = perl
  )


  # delete No. before numbers
  clean <- gsub(
    "No[.\\s]+?(?=\\d)", "", clean, ignore.case = ignore_case, perl = perl
  )


  # delete space or period between digits
  clean <- gsub(
    "(?<=\\d)[\\s.](?=\\d(?!\\/))", "", clean,
    ignore.case = ignore_case, perl = perl
  )

  # fix 1/2 issue
  clean <- gsub("0\\.5", " 1/2", clean, ignore.case = ignore_case, perl = perl)
  clean <- gsub(
    "(?<=\\d)(1\\/2)", " \\1", clean, ignore.case = ignore_case, perl = perl
  )


  clean
}

## Name ####

### clean_title ####

#' Clean entries name title
#'
#' Attempts to clean titles attached to names provided: Captain, Major, etc.
#'
#' @param titles A character string vector of title(s).
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_title(c("  Capt Wm. ABOT", "?-Rev Alex ABRCROMBIE"))
#' }
clean_title <- function(titles){

  clean <- titles

  purrr::pwalk(globals_titles, function(pattern, replacement, ignore_case){
    regex_title <- paste0(
      "[[:punct:][:blank:]]*(?:", pattern, ")[[:punct:][:blank:]]*(?=\\s[\\w\\$])"
    )

    clean <<- gsub(regex_title, replacement, clean, ignore_case, perl = perl)
  })

  clean

}

### clean_forename ####

#' Clean entries forename
#'
#' Attempts to clean provided forename.
#'
#' @param names A character string vector of forename(s).
#'
#' @return A vector of character strings.
#'
#' @section Details:
#' Single letter forenames are standardised to the forename starting with that
#'   letter occurring the most frequently in the dataset. i.e A. -> Alexander,
#'   B. -> Bernard, C. -> Colin, D. -> David, etc.
#'
#' @examples
#' \dontrun{
#'   clean_forename(c("McWm.", "Jas."))
#' }
clean_forename <- function(names){

  clean <- clean_mac(names)
  clean <- clean_forename_separate_words(clean)
  clean <- clean_forename_spelling(clean)
  clean <- clean_forename_punctuation(clean)
  clean <- clean_name_ends(clean)

  clean
}

### clean_surname ####

#' Clean entries surname
#'
#' Attempts to clean provided surname.
#'
#' @param names A character string vector of surname(s).
#'
#' @return A vector of character strings.
#'
#' @section Details:
#' Multiple spelling names are standardised to that of the capital letter header
#'   in the general directory. i.e. Abercrombie, Abercromby -> Abercromby;
#'   Bayne, Baynes -> Bayne; Beattie, Beatty -> Beatty; etc.
#'
#' @examples
#' \dontrun{
#'   clean_surname(c("ABOT", "ABRCROMBIE"))
#' }
clean_surname <- function(names){

  clean <- clean_mac(names)
  clean <- clean_parentheses(clean)
  clean <- clean_surname_spelling(clean)
  clean <- clean_surname_punctuation(clean)
  clean <- clean_name_ends(clean)

  clean
}

## clean_occupation ####

#' Clean entries occupation
#'
#' Attempts to clean provided occupation
#'
#' @param occupations A character string vector of occupation(s).
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_occupation(c("Wine and spirit mercht", "Bkr"))
#' }
clean_occupation <- function(occupations){

  clean <- occupations

  purrr::pwalk(globals_occupations, function(pattern, replacement, ignore_case){
    clean <<- gsub(pattern, replacement, clean, ignore_case, perl = perl)
  })

  clean
}



# Utils ####

## clean_specials ####

#' Clean entries special characters
#'
#' Attempts to clean entries of unwanted special characters
#'
#' @param x A character string vector.
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_specials(c("Wine and spirit \u00bbmerchant", "Mac\u00bb William"))
#' }
clean_specials <- function(x){

  clean <- gsub('[\u00bb\u00a6\u2022"]', "", x, ignore.case = ignore_case, perl = perl)
  clean <- gsub("\u2019", "'", clean, ignore.case = ignore_case, perl = perl)

  clean
}

## clean_parentheses ####

#' Clean entries of in brackets information
#'
#' Attempts to clean entries of unwanted information displayed in brackets.
#'
#' @param x A character string vector.
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_parentheses("Miller street. (Agents).")
#' }
clean_parentheses <- function(x){

  clean <-gsub("(.*?)\\s?\\(.*", "\\1", x, ignore.case = ignore_case, perl = perl)
  clean
}

## clean_mac ####

#' Standardise "Mac" prefix in people's name
#'
#' Attempts to standardise "Mac" prefix in provided name entries.
#'
#' @param names A character string vector of names.
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_mac("McWilliam")
#' }
clean_mac <- function(names){

  # Standardise Mac prefix
  clean <- gsub(
    "\\bMa?c(\\w{2,}\\b)", "Mac \\1", names,
    ignore.case = ignore_case, perl = perl
  )
  clean
}







# Helpers ####

## Address ####

### clean_address_pre_clean ####

#' Pre-cleaning operation for address entries
#'
#' Performs pre-cleaning operations on provided address entries.
#'
#' @param addresses A character string vector of addresses.
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_address_pre_clean(c(": 1S20 Londun st.   -", "13<J st enoch sq,;"))
#' }
clean_address_pre_clean <- function(addresses){

  # Special characters
  clean <- clean_specials(addresses)
  # Clean ends
  clean <- clean_address_ends(clean)
  # Separate words
  clean <- clean_address_attached_words(clean)
  # clean Mac name pre-fixes
  # clean <- address_clean_mac(clean)
  # Saints
  clean <- clean_address_saints(clean)
  # Possessives
  clean <- clean_address_possessives(clean)
  # Suffixes
  clean <- clean_address_suffixes(clean)

  clean
}


### clean_address_post_clean ####

#' Post-cleaning operation for address entries
#'
#' Performs post-cleaning operations on provided address entries.
#'
#' @param addresses A character string vector of addresses.
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_address_post_clean(
#'     c(": 1820 London Street.   -", "136 Saint Enoch Square,;")
#'   )
#' }
clean_address_post_clean <- function(addresses){

  # Others
  clean <- clean_address_others(addresses)
  # Clean ends
  clean <- clean_address_ends(clean)

  clean
}


### clean_address_mac ####

#' Standardise "Mac" prefix in address entries
#'
#' Attempts to standardise "Mac" prefix in provided address entries.
#'
#' @param addresses A character string vector of addresses.
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_address_mac(c("McWilliam", "M'Donald", "M c Fyfe"))
#' }
clean_address_mac <- function(addresses){

  clean <- addresses

  purrr::pwalk(globals_macs, function(pattern, replacement, ignore_case){
    clean <<- gsub(
      pattern, paste0(replacement, " "), clean,
      ignore.case = ignore_case, perl = perl
    )
  })

  clean
}


### clean_address_saints ####

#' Clean "Saint" prefix in address entries
#'
#' Attempts to clean "Saint" prefix in provided address entries.
#'
#' @param addresses A character string vector of addresses.
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_address_saints(c("St Georges st.", "St Enoch sq"))
#' }
clean_address_saints <- function(addresses){

  clean <- gsub(
    paste0(
      "\\bst[\\.,;]*?\\s*?(?=",
      paste0(globals_saints$pattern, collapse = "|"),
      ")"
    ),
    "Saint ",
    addresses, ignore.case = ignore_case, perl = perl
  )

  purrr::pwalk(globals_saints, function(pattern, replacement, ignore_case) {
    clean <<- gsub(
      paste0("Saint\\s", pattern), paste0("Saint ", replacement),
      clean, ignore.case = ignore_case, perl = perl
    )
  })

  clean
}


### clean_address_possessives ####

#' Standardise possessives in address entries
#'
#' Attempts to standardise possessives in provided address entries.
#'
#' @param addresses A character string vector of addresses.
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_address_possessives(c("Adam's Court", "Aird's Lane"))
#' }
clean_address_possessives <- function(addresses){

  clean <- gsub(
    "\\b(\\w+)\\b\\.?'?s'?", "\\1s", addresses,
    ignore.case = ignore_case, perl = perl
  )
  clean <- gsub(
    "(\\b\\w+s\\b)'\\.?", "\\1", clean, ignore.case = ignore_case, perl = perl
  )

  clean
}

### clean_address_ends ####

#' Clean ends in address entries
#'
#' Attempts to clean ends in provided address entries.
#'
#' @param addresses A character string vector of addresses.
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_address_ends(c(" -; 18, 20 London st.", ",,12 Dixon st.$  "))
#' }
clean_address_ends <- function(addresses){

  # Trim white space(s) at start and end as well as multiple white spaces in a row
  clean <- stringr::str_squish(addresses)
  # clean <- gsub("^[[:space:][:punct:]]+", "", address, ignore.case = ignore_case, perl = perl)
  clean <- gsub("^[\\W]+", "", clean, ignore.case = ignore_case, perl = perl)
  clean <- gsub("\\W+$", "", clean, ignore.case = ignore_case, perl = perl)
  clean <- gsub("\\s{2,}", " ", clean, ignore.case = ignore_case, perl = perl)

  # Remove single letter at start or end of address.
  clean <- gsub(
    "(?:^[A-Za-z]\\s|,?\\s[A-Za-z]$)", "", clean, ignore.case = FALSE, perl = perl
  )

  # Place period at the end of address if none
  clean <- gsub("([^.])$", "\\1\\.", clean, ignore.case = ignore_case, perl = perl)

  clean <- stringr::str_squish(clean)

  clean
}

### clean_address_attached_words ####

#' Clean attached words in address entries
#'
#' Attempts to separate attached words in provided address entries.
#'
#' @param addresses A character string vector of addresses.
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_address_attached_words(c("18, 20 LondonSt.", "12 Dixon.st."))
#' }
clean_address_attached_words <- function(addresses){

  # If two words are only separated by a period or comma, replace period with
  # white space
  clean <- gsub(
    "([a-z])[.,]([a-z])", "\\1 \\2", addresses,
    ignore.case = ignore_case, perl = perl
  )

  # if lower case followed by upper case, add space between the two.
  clean <- gsub(
    "([a-z])([A-Z])", "\\1 \\2", clean, ignore.case = FALSE, perl = perl
  )

  clean <- stringr::str_squish(clean)

  clean
}

### clean_address_places ####

#' Clean places in address entries
#'
#' Attempts to clean places in provided address entries: street, road, place,
#'   quay, etc.
#'
#' @param addresses A character string vector of addresses.
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_address_places(c("Bothwell Cir.", "Railway arch."))
#' }
clean_address_places <- function(addresses){

  clean <- addresses

  purrr::pwalk(globals_places_regex, function(pattern, replacement, ignore_case) {
    clean <<- gsub(pattern, replacement,clean, ignore.case = ignore_case, perl = perl
    )
  })

  clean <- gsub(
    "((?<!Mil|Mill|B))(road|street)\\.?", "\\1 \\2", clean,
    ignore.case = ignore_case, perl = perl
  )

  clean <- stringr::str_squish(clean)

  clean
}

### clean_address_worksites ####

#' Clean worksites in address entries
#'
#' Attempts to clean worksites in provided address entries.
#'
#' @param addresses A character string vector of addresses.
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_address_worksites("Woodyd")
#' }
clean_address_worksites <- function(addresses){

  clean <- addresses

  purrr::pwalk(globals_worksites, function(pattern, replacement, ignore_case){
    clean <<- gsub(pattern, replacement, clean, ignore.case = ignore_case, perl = perl)
  })

  clean
}

### clean_address_suffixes ####

#' Clean unwanted suffixes in address entries
#'
#' Attempts to clean unwanted suffixes in provided address entries.
#'
#' @param addresses A character string vector of addresses.
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_address_suffixes("36 Rose street so")
#' }
clean_address_suffixes <- function(addresses){

  clean <- addresses

  purrr::pwalk(globals_suffixes, function(pattern, replacement, ignore_case) {
    clean <<- gsub(pattern, replacement,clean, ignore.case = ignore_case, perl = perl
    )
  })

  clean <- stringr::str_squish(clean)

  clean
}

### clean_address_names ####

#' Clean place names in address entries
#'
#' Attempts to clean place names in provided address entries.
#'
#' @param addresses A character string vector of addresses.
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_address_names(c("18, 20 Londun Street", "136 Dixn Road"))
#' }
clean_address_names <- function(addresses){

  clean <- addresses

  purrr::pwalk(globals_address_names, function(pattern, replacement, ignore_case) {
    clean <<- gsub(pattern, replacement,clean, ignore.case = ignore_case, perl = perl
    )
  })

  clean <- stringr::str_squish(clean)

  clean
}

### clean_address_others ####

#' Miscellaneous cleaning operations in address entries
#'
#' Carries out miscellaneous cleaning operations in provided address entries.
#'
#' @param addresses A character string vector of addresses.
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_address_others(c("18, 20 London. Street, Agents", "136 Dixon Road d"))
#' }
clean_address_others <- function(addresses){

  # Get rid of parasite postfixes
  ## (Agents)
  clean <- gsub(
    "(.*?)\\s\\(?\\bag(?:en)?ts?\\b\\)?.?", "\\1", addresses,
    ignore.case = ignore_case, perl = perl
  )

  # Get rid of parasite punctation
  clean <- gsub("'", "", clean, ignore.case = ignore_case, perl = perl)
  clean <- gsub('\\"\\b', "", clean, ignore.case = ignore_case, perl = perl)
  clean <- gsub("\\b[\\-.]\\s", " ", clean, ignore.case = ignore_case, perl = perl)
  clean <- gsub(
    "\\b([[:punct:]])[[:punct:]]\\s", "\\1 ", clean,
    ignore.case = ignore_case, perl = perl
  )
  clean <- gsub(
    "\\b[[:punct:]]\\s(?=\\w)", " ", clean, ignore.case = ignore_case, perl = perl
  )
  clean <- gsub(
    "(?<=\\s)[[:punct:]]\\s", "\\1", clean, ignore.case = ignore_case, perl = perl
  )
  clean <- gsub(
    "\\s[[:punct:]]\\s", "\\s", clean, ignore.case = ignore_case, perl = perl
  )

  # Separate numbers with comma
  clean <- gsub(
    "(?<=\\d)\\s+(?=\\d)", ", ", clean, ignore.case = ignore_case, perl = perl
  )

  # Remove single letters
  clean <- gsub(
    "(?:\\s[a-z]\\b|\\s\\.[a-z]\\.(?:\\s|$))", "", clean,
    ignore.case = FALSE, perl = perl
  )
  clean <- gsub("\\b\\s[a-z]\\s\\b", "", clean, ignore.case = FALSE, perl = perl)

  # Unwanted processing outcomes
  ## "street" processing sometimes outputs orpheans ", reet,". Replace with comma.
  clean <- gsub(", reet,", ",", clean, ignore.case = ignore_case, perl = perl)

  # If placed between two words, replace period with a comma.
  clean <- gsub("\\b\\.\\s\\b", ", ", clean, ignore.case = ignore_case, perl = perl
  )

  # If place not located at the end of the address and not followed by a word that
  # is itself a place, append a comma.
  clean <- gsub(
    paste0(
      "\\b(",
      paste(globals_places_raw, collapse = "|"),
      ")\\b(?!$|[,\\-]|\\s+?(?:\\(|",
      paste(globals_places_raw, collapse = "|"),
      "))"),
    "\\1, ",
    clean,
    ignore.case = ignore_case, perl = perl
  )

  # If place not located at the beginning of the address and separated from previous
  # word-that is not itself a place-with comma and space, delete comma.
  clean <- gsub(
    paste0(
      "(?<!",
      paste(globals_places_raw, collapse = "|"),
      "),\\s+?(",
      paste(globals_places_raw, collapse = "|"),
      ")"
    ),
    " \\1",
    clean,
    ignore.case = ignore_case, perl = perl
  )

  # Clean address ends
  clean <- clean_string_ends(clean) %>% stringr::str_squish()

  clean
}


## Names ####

### clean_name_ends ####

#' Clean ends in entries names
#'
#' Attempts to clean ends in provided name entries.
#'
#' @param names A character string vector of addresses.
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_name_ends(c("William-", "$Alexander", "John Hugh   "))
#' }
clean_name_ends <- function(names){
  clean <- gsub(
    "\\b\\W*$", "", names, ignore.case = ignore_case, perl = perl
  )
  clean <- gsub(
    "^\\W*\\b", "", clean, ignore.case = ignore_case, perl = perl
  )

  clean
}

### Forename ####

#### clean_forename_separate_words ####

#' Separate double-barrelled forenames
#'
#' Attempts to separate double-barrelled forenames in provided name entries.
#'
#' @param names A character string vector of addresses.
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_forename_separate_words(c("William", "Alexander", "JohnHugh"))
#' }
clean_forename_separate_words <- function(names){

  clean <- gsub(
    "(?<=[a-z.])([A-Z])", " \\1", names, ignore.case = FALSE, perl = perl
  )
  clean <- gsub(
    "([A-Z])([A-Z])", "\\1. \\2 ", clean, ignore.case = FALSE, perl = perl
  )
  clean <- gsub(
    "([A-Za-z])&([A-Z])", "\\1 & \\2", clean, ignore.case = FALSE, perl = perl
  )
  clean <- gsub(
    "([A-Z])\\.([A-Z]+\\b)", "\\1\\. \\2", clean,
    ignore.case = ignore_case, perl = perl
  )
  clean
}

#### clean_forename_punctuation ####

#' Standardise punctuation in forenames
#'
#' Attempts to standardise punctuation in provided name entries.
#'
#' @param names A character string vector of names.
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_forename_punctuation(c("William;", "Miss", "John,Hugh"))
#' }
clean_forename_punctuation <- function(names){

  # Standardise end of string punctuation to a period.
  clean <- gsub("\\W*$", ".", names, ignore.case = ignore_case, perl = perl)
  # # Empty entries remain empty.
  # clean <- gsub("^\\.$", "", clean, ignore.case = ignore_case, perl = perl)

  # Standardise inbetween words to a space
  clean <- gsub(
    "\\b[,'.\\s]+\\b", " ", clean, ignore.case = ignore_case, perl = perl
  )

  # Add period to single capital letters and Mrs/Miss/Misses
  clean <- gsub(
    "(\\b(?:[A-Z]|Mrs|Miss|Misses)\\b)(?!\\.)", "\\1.", clean,
    ignore.case = FALSE, perl = perl
  )

  # Empty entries remain empty.
  clean <- gsub(
    "^[.[:blank:]]+$", "", clean, ignore.case = ignore_case, perl = perl
  )

  clean
}

#### clean_forename_spelling ####

#' Clean forenames spelling
#'
#' Attempts to clean spelling in provided name entries.
#'
#' @param names A character string vector of names.
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_forename_spelling(c("Wm.", "A.", "Jn Huh"))
#' }
clean_forename_spelling <- function(names){

  clean <- names

  # Clean OCR errors, standardise spelling
  purrr::pwalk(globals_forenames, function(pattern, replacement, ignore_case) {
    clean <<- gsub(
      pattern, replacement, clean, ignore.case = ignore_case, perl = perl
    )
  })

  clean
}

### Surname ####

#### clean_surname_punctuation ####

#' Standardise punctuation in forenames
#'
#' Attempts to standardise punctuation in provided name entries.
#'
#' @param names A character string vector of names.
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_surname_punctuation(c("Abbott*", "*Abercromby", "Blair"))
#' }
clean_surname_punctuation <- function(names){

  # Get rid of orphan star characters
  clean <- gsub("\\*", "", names, ignore.case = ignore_case, perl = perl)

  clean
}

#### clean_surname_spelling ####

#' Clean surnames spelling
#'
#' Attempts to clean spelling in provided name entries.
#'
#' @param names A character string vector of names.
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_surname_spelling(c("ABOT junior", "ABRCROMBIE", "BLAI senior"))
#' }
clean_surname_spelling <- function(names){

  clean <- names

  # Clean OCR errors, standardise spelling
  purrr::pwalk(globals_surnames, function(pattern, replacement, ignore_case) {
    clean <<- gsub(
      pattern, replacement, clean, ignore.case = ignore_case, perl = perl
    )
  })

  # Get rid of Junior/Senior postfix
  clean <- gsub(
    "\\s+\\b(?:Junior|Senior)\\b[\\s$]?", "", clean,
    ignore.case = ignore_case, perl = perl
  )

  clean
}

## Others ####

### clean_string_ends ####

#' Clean string ends
#'
#' Attempts to clean ends of strings provided
#'
#' @param strings A character string vector.
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   clean_string_ends(c(" -; 18, 20 Mary hill.*", ",,12 Dixon Street^"))
#' }
clean_string_ends <- function(strings){
  clean <- gsub(
    "^[[:punct:][:space:]]+\\b", "", strings,
    ignore.case = ignore_case, perl = perl
  )
  clean <- gsub(
    "\\b[[:punct:][:space:]]+$", "", clean, ignore.case = ignore_case, perl = perl
  )

  clean
}
