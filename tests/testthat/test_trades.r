# clean_entries ####
test_that("trades_clean_entries works for various entries", {
  directory <- tibble::tibble(
    page = c("71", "71"),
    surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit mercht — See Advertisement in Appendix.", "Bkr"),
    address.trade.number = c("1S20", "I2"),
    address.trade.body = c("Londn rd.", "Dixen pl")
  )
  out <- tibble::tibble(
    page = c("71", "71"),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade.number = c("18, 20", "12"),
    address.trade.body = c("London Road.", "Dixon Place.")
  )
  expect_equal(trades_clean_entries(directory, verbose = FALSE), out)
})


# clean_directory ####
test_that("trades_clean_directory works in general", {
  directory <- tibble::tibble(
    page = c("71", "71"),
    surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit mercht — See Advertisement in Appendix.", "Bkr"),
    address.trade.number = c("1S20", "I2"),
    address.trade.body = c("Londn rd.", "Dixen pl")
  )
  out <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("Abbott", "Abercromby"),
    forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade.number = c("18, 20", "12"),
    address.trade.body = c("London Road.", "Dixon Place."),
  )
  expect_equal(trades_clean_directory(directory, verbose = FALSE), out)
})

# trades_clean_directory_plain ####
test_that("trades_clean_directory_plain works in general", {
  directory <- tibble::tibble(
    page = c("71", "71"),
    surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit mercht — See Advertisement in Appendix.", "Bkr"),
    address.trade.number = c("1S20", "I2"),
    address.trade.body = c("Londn rd.", "Dixen pl")
  )
  out <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("Abbott", "Abercromby"),
    forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade.number = c("18, 20", "12"),
    address.trade.body = c("London Road.", "Dixon Place."),
  )
  expect_equal(trades_clean_directory_plain(directory, verbose = FALSE), out)
})

# trades_clean_directory_progress ####
test_that("trades_clean_directory_progress works in general", {
  directory <- tibble::tibble(
    page = c("71", "71"),
    surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit mercht — See Advertisement in Appendix.", "Bkr"),
    address.trade.number = c("1S20", "I2"),
    address.trade.body = c("Londn rd.", "Dixen pl")
  )
  out <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("Abbott", "Abercromby"),
    forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade.number = c("18, 20", "12"),
    address.trade.body = c("London Road.", "Dixon Place."),
  )
  expect_equal(trades_clean_directory_progress(directory, verbose = FALSE), out)
})

# clean_directory ####
test_that("trades_clean_directory works in general", {
  directory <- tibble::tibble(
    page = c("71", "71"),
    surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit mercht — See Advertisement in Appendix.", "Bkr"),
    address.trade.number = c("1S20", "I2"),
    address.trade.body = c("Londn rd.", "Dixen pl")
  )
  out <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("Abbott", "Abercromby"),
    forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade.number = c("18, 20", "12"),
    address.trade.body = c("London Road.", "Dixon Place."),
  )
  expect_equal(
    trades_clean_directory(directory, progress = TRUE, verbose = FALSE), out
  )
})
