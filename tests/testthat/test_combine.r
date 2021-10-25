# random_string_if_pattern ####
test_that("combine_random_string_if_pattern works for various patterns", {
  set.seed(1)
  expect_equal(
    combine_random_string_if_pattern("random string", "random"),
    "GNZuCtwed3CAgNlUizNmvD"
  )
  expect_equal(
    combine_random_string_if_pattern("random string", "original"),
    "random string"
  )
})

# random_string_if_no_address ####
test_that("combine_random_string_if_no_address works for various patterns", {
  set.seed(1)
  expect_equal(
    combine_random_string_if_no_address(c("18, 20 London Road", "No trade address found")),
    c("18, 20 London Road", "GNZuCtwed3CAgNlUizNmvD")
  )
})

# no_trade_address_to_randon_string ####
test_that("combine_no_trade_address_to_randon_string works in general", {
  directory <- tibble::tibble(
    page = c("71", "71"),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade = c("18, 20, London Road.", "No trade address found")
  )
  set.seed(1)
  out <- tibble::tibble(
    page = c("71", "71"),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade = c("18, 20, London Road.", "GNZuCtwed3CAgNlUizNmvD"),
  )
  expect_equal(combine_no_trade_address_to_randon_string(directory), out)
})

# make_match_string ####
test_that("combine_make_match_string works for various entries", {
  directory <- tibble::tibble(
    page = c("71", "71"),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade = c("18, 20, London Road.", "No trade address found")
  )
  set.seed(1)
  out <- tibble::tibble(
    page = c("71", "71"),
    match.string = c(
      "Abbott William - 18, 20, London Road",
      "Abercromby Alexander - GNZuCtwed3CAgNlUizNmvD"
    ),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker")
  )
  expect_equal(combine_make_match_string(directory), out)
})

# has_match_failled ####
test_that("combine_has_match_failled works in general", {
  numbers <- c("18, 20", NA)
  bodies <- c("London Road.", NA)
  expect_equal(combine_has_match_failled(numbers, bodies), c(FALSE, TRUE))
})

# label_if_match_failled ####
test_that("combine_label_if_match_failled works for both number and address body", {
  numbers <- c("18, 20", NA)
  bodies <- c("London Road.", NA)
  combine_label_if_match_failled("number", number = numbers, body = bodies)
  expect_equal(
    combine_label_if_match_failled("number", number = numbers, body = bodies),
    c("18, 20", "")
  )
  expect_equal(
    combine_label_if_match_failled("body", number = numbers, body = bodies),
    c("London Road.", "Failled to match with general directory")
  )
})

# get_address_house_type ####
test_that("combine_get_address_house_type works for body number and address house body", {

  expect_equal(combine_get_address_house_type("address.house.number"), "number")
  expect_equal(combine_get_address_house_type("address.house.body"), "body")
})

# label_failled_matches ####
test_that("combine_label_failled_matches works for various entries", {
  directory <- tibble::tibble(
    page = c("71", "71"),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade.number = c("18, 20", "12"),
    address.house.number = c("136", NA),
    address.trade.body = c("London Road", "Dixon Place"),
    address.house.body = c("Queen Square", NA)
  )
  out <- tibble::tibble(
    page = c("71", "71"),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade.number = c("18, 20", "12"),
    address.house.number = c("136", ""),
    address.trade.body = c("London Road", "Dixon Place"),
    address.house.body = c("Queen Square", "Failled to match with general directory")
  )
  expect_equal(combine_label_failled_matches(directory), out)
})


# match_general_to_trades ####
test_that("combine_match_general_to_trades works in general", {
  trades_directory <- tibble::tibble(
    page = c("71", "71", "71"),
    surname = c("Abbott", "Abercromby", "Blair"),
    forename = c("William", "Alexander", "John Hugh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
    address.trade.number = c("18, 20", "12", "280"),
    address.trade.body = c("London Road", "Dixon Place", "High Street")
  )
  general_directory <- tibble::tibble(
    page = c("71", "71"),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade.number = c("18, 20", ""),
    address.house.number = c("136", "29"),
    address.trade.body = c("London Road", "Dixon Place"),
    address.house.body = c("Queen Square", "Anderston Quay")
  )
  out <- tibble::tibble(
    page = rep("71", 3L),
    surname = c("Abbott", "Abercromby", "Blair"),
    forename = c("William", "Alexander", "John Hugh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
    address.trade.number = c("18, 20", "12", "280"),
    address.trade.body = c("London Road", "Dixon Place", "High Street"),
    address.house.number = c("136", "29", ""),
    address.house.body = c(
      "Queen Square", "Anderston Quay", "Failled to match with general directory"
    )
  )
  expect_equal(
    combine_match_general_to_trades(
      trades_directory, general_directory, verbose = FALSE, method = "osa", max_dist = 5
    ),
    out
  )
})

# match_general_to_trades ####
test_that("combine_match_general_to_trades works in general", {
  trades_directory <- tibble::tibble(
    page = c("71", "71", "71"),
    surname = c("Abbott", "Abercromby", "Blair"),
    forename = c("William", "Alexander", "John Hugh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
    address.trade.number = c("18, 20", "12", "280"),
    address.trade.body = c("London Road", "Dixon Place", "High Street")
  )
  general_directory <- tibble::tibble(
    page = c("71", "71"),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade.number = c("18, 20", ""),
    address.house.number = c("136", "29"),
    address.trade.body = c("London Road", "Dixon Place"),
    address.house.body = c("Queen Square", "Anderston Quay")
  )
  out <- tibble::tibble(
    page = rep("71", 3L),
    surname = c("Abbott", "Abercromby", "Blair"),
    forename = c("William", "Alexander", "John Hugh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
    address.trade.number = c("18, 20", "12", "280"),
    address.trade.body = c("London Road", "Dixon Place", "High Street"),
    address.house.number = c("136", "29", ""),
    address.house.body = c(
      "Queen Square", "Anderston Quay", "Failled to match with general directory"
    )
  )
  expect_equal(
    combine_match_general_to_trades(
      trades_directory, general_directory, verbose = FALSE, method = "osa", max_dist = 5
    ),
    out
  )
})

