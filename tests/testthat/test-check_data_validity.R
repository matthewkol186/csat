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

test_that("findColsPresent removes columns that don't exist", {
  all_cols <- c("district","cluster","sex", "age","offer", "swallow", "school","coverage")
  district <- c("D1", "D2", "D3")
  cluster <- c("C1", "C2", "C3")
  sex <- c("F", "M", "F")
  swallow <- c("Yes", "No", "Yes")
  school <- c("Yes", "No", "Yes")
  test_frame = data.frame(district, cluster, swallow, school)
  all_cols <- c("district","cluster","sex", "age","offer", "swallow", "school","coverage")
  cols_to_check <- c("age", "offer", "school", "coverage")
  present_cols <- findColsPresent(all_cols, cols_to_check, test_frame)
  expect_equal(present_cols, c("district", "cluster", "sex", "swallow", "school"))
})

test_that("findColsPresent returns all columns if all of them exist", {
  all_cols <- c("district","cluster","sex", "age","offer", "swallow", "school","coverage")
  district <- c("D1", "D2", "D3")
  cluster <- c("C1", "C2", "C3")
  sex <- c("F", "M", "F")
  age <- c(18, 20, 22)
  offer <- c("Yes", "No", "Yes")
  swallow <- c("Yes", "No", "Yes")
  school <- c("Yes", "No", "Yes")
  coverage <- c(0.9, 0.8, 0.8)
  test_frame = data.frame(district, cluster, sex, age, offer, swallow, school, coverage)
  all_cols <- c("district","cluster","sex", "age","offer", "swallow", "school","coverage")
  cols_to_check <- c("age", "offer", "school", "coverage")
  present_cols <- findColsPresent(all_cols, cols_to_check, test_frame)
  expect_equal(present_cols, all_cols)
})
