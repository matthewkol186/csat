context("test-internationalization")

test_that("translating error string to French works", {
  language <- 'fr'
  vocab <- translation_map("../../translation.csv")
  trans_string <- i18n("The drug/drug package selected is not used in treatment for the disease selected", language, vocab)
  french_string <- readLines("french_example_1.txt", encoding="UTF-8")
  expect_equal(trans_string, french_string)
})

test_that("concatenating translations works", {
  language <- 'fr'
  vocab <- translation_map("../../translation.csv")
  french_string <- paste(readLines("french_example_2.txt", encoding="UTF-8"), collapse='\n')
  trans_string<-paste(i18n("Meets or Exceeds", language, vocab), "\n",
                   i18n("the Target 65% Threshold", language, vocab), sep='')
  expect_equal(trans_string, french_string)
})
