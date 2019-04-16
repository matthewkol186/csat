context("test-set_WHO_constants")

test_that("threshold values are set to appropriate values", {
  expect_equal(setDiseaseThreshold("Onchocerciasis"), 65)
})
