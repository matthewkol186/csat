context("test-set_WHO_constants")

test_that("threshold values are set to appropriate values", {
  expect_equal(setDiseaseThreshold("Onchocerciasis"), 65)
})

test_that("threshold values are set to appropriate values", {
  expect_equal(setDiseaseThreshold("Trachoma"), 80)
})

test_that("threshold values are set to appropriate values", {
  expect_equal(setDiseaseThreshold("STH"), 75)
})


