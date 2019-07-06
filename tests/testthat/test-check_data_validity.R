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

df_colnames <-
  tribble(
    ~district, ~cluster, ~sex, ~swallowed,
    1, 1, "m", "y",
    1, 8, "m", "y",
    2, 2, "f", "n",
    2, 3, "m", "y"
  )

implementation_unit_header_test = "district"
cluster_header_test = "cluster"
cluster_header_test_bad = "clusters"
sex_header_test = "sex"
age_header_test = NULL
age_header_test_bad = "age"
offered_drug_header_test = NULL
swallowed_drug_header_test = "swallowed"
school_attendance_test = NULL
reported_coverage_header_test = NULL

test_that("wrong_cluster", {
  expect_equal(checkUnmatchedHeaders(df_colnames, implementation_unit_header_test, cluster_header_test_bad,
                                     sex_header_test, age_header_test, offered_drug_header_test, swallowed_drug_header_test,
                                     school_attendance_test, reported_coverage_header_test), c("cluster"))
})

test_that("wrong_cluster_age", {
  expect_equal(checkUnmatchedHeaders(df_colnames, implementation_unit_header_test, cluster_header_test_bad,
                                     sex_header_test, age_header_test_bad, offered_drug_header_test, swallowed_drug_header_test,
                                     school_attendance_test, reported_coverage_header_test), c("cluster", "age"))
})

test_that("findColsPresent removes columns that don't exist", {
  all_cols <- c("district","cluster","sex", "age","offer", "swallow", "school","coverage")
  district <- c("D1", "D2", "D3")
  cluster <- c("C1", "C2", "C3")
  sex <- c("F", "M", "F")
  swallow <- c("Yes", "No", "Yes")
  school <- c("Yes", "No", "Yes")
  test_frame = data.frame(district, cluster, swallow, school)
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
  cols_to_check <- c("age", "offer", "school", "coverage")
  present_cols <- findColsPresent(all_cols, cols_to_check, test_frame)
  expect_equal(present_cols, all_cols)
})

# #saving a complete test frame
# all_cols <- c("district","cluster","sex", "age","offer", "swallow", "school","coverage")
# district <- c("D1", "D2", "D3")
# cluster <- c("C1", "C2", "C3")
# sex <- c("F", "M", "F")
# age <- c(18, 20, 22)
# offer <- c("Yes", "No", "Yes")
# swallow <- c("Yes", "No", "Yes")
# school <- c("Yes", "No", "Yes")
# coverage <- c(0.9, 0.8, 0.8)
# complete_test_frame = data.frame(district, cluster, sex, age, offer, swallow, school, coverage)

test_that("rename_cols_works", {
  DISTRICT <- c("D1", "D2", "D3")
  VILLAGE_NUMBER <- c("C1", "C2", "C3")
  SEX <- c("F", "M", "F")
  AGE <- c(18, 20, 22)
  OFFERED_ALB <- c("Yes", "No", "Yes")
  SWALLOWED_ALB <- c("Yes", "No", "Yes")
  REPORTED_COVERAGE <- c(0.9, 0.8, 0.8)

  test_frame = data.frame(DISTRICT, VILLAGE_NUMBER, SEX, AGE, OFFERED_ALB, SWALLOWED_ALB, REPORTED_COVERAGE)

  cols_no_school <- c("district","cluster","sex", "age","offer", "swallow", "coverage")
  implementation_unit_header<-"DISTRICT"
  cluster_header<- "VILLAGE_NUMBER"
  sex_header<- "SEX"
  age_header<-"AGE"
  school_attendance_header<- NULL
  offered_drug_header<- "OFFERED_ALB"
  swallowed_drug_header<- "SWALLOWED_ALB"
  reported_coverage_header<- "REPORTED_COVERAGE"
  function_cols = colnames(renameColumns(test_frame, implementation_unit_header, cluster_header, sex_header, age_header, offered_drug_header, swallowed_drug_header, school_attendance_header,
    reported_coverage_header))
  expect_equal(function_cols, cols_no_school)
})
