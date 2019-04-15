context("test-check_data_validity")

test_that("Lymphatic filariasis with ALB & IVM works", {
  expect_equal(checkDiseaseDrugMatch("Lymphatic Filariasis", "ALB & IVM"), TRUE)
})
test_that("Lymphatic filariasis with Ivermectin", {
  expect_equal(checkDiseaseDrugMatch("Lymphatic Filariasis", "Ivermectin"), TRUE)
})
test_that("Lymphatic filariasis works", {
  expect_equal(checkDiseaseDrugMatch("Lymphatic Filariasis", "ALB & IVM"), TRUE)
})
test_that("Trachoma with Azithromycin works", {
  expect_equal(checkDiseaseDrugMatch("Trachoma", "Azithromycin"), TRUE)
})
