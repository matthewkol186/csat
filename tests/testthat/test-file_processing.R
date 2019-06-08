context("test-file_processing")

runUserSurvey <- function(csv_url, country_name, num_implementation_units, drug_name,
                          disease_name, implementation_unit_header, cluster_header, sex_header,
                          offered_drug_header, swallowed_drug_header, reported_coverage_header) {
  # set global variables <- this is probably not ideal
  # todo: remove dependency on global variables
  csv_url <<- csv_url
  country_name <<- country_name
  num_implementation_units <<- num_implementation_units
  drug_name <<- drug_name
  disease_name <<- disease_name
  implementation_unit_header <<- implementation_unit_header
  cluster_header <<- cluster_header
  sex_header <<- sex_header
  offered_drug_header <<- offered_drug_header
  swallowed_drug_header <<- swallowed_drug_header
  reported_coverage_header <<- reported_coverage_header
  # suppress output - random print statements
  invisible(capture.output(doc <- try(wordDocUserSurvey())))
  error <- inherits(doc, "try-error")
}

runCSB <- function(csv_url, country_name, drug_name, disease_name, implementation_unit_header,
                   number_of_subunits, reported_coverage_header) {
  # set global variables <- this is probably not ideal
  # todo: remove dependency on global variables
  csv_url <<- csv_url
  country_name <<- country_name
  drug_name <<- drug_name
  disease_name <<- disease_name
  implementation_unit_header <<- implementation_unit_header
  number_of_subunits <<- number_of_subunits
  reported_coverage_header <<- reported_coverage_header
  # suppress output - random print statements
  invisible(capture.output(doc <- try(wordDocCSB())))
  error <- inherits(doc, "try-error")
}

test_that("MultiDistrictDemo file works", {
  #temporarily change directory
  orig_wd <- getwd()
  setwd("../..")
  csv_url <- "./demo_files/Multidistrict_Demo.csv"
  country_name<- "Murkonia"
  num_implementation_units<-3
  as.numeric(num_implementation_units)
  drug_name<-"Azithromycin"
  disease_name <- "Trachoma"

  implementation_unit_header<-"DISTRICT"
  cluster_header<- "VILLAGE_NUMBER"
  sex_header<- "SEX"
  age_header<-"AGE"
  offered_drug_header<- "OFFERED_ALB"
  swallowed_drug_header<- "SWALLOWED_ALB"
  reported_coverage_header<- "REPORTED_COVERAGE"

  error <- runUserSurvey(csv_url, country_name, num_implementation_units, drug_name,
                disease_name, implementation_unit_header, cluster_header, sex_header,
                offered_drug_header, swallowed_drug_header, reported_coverage_header)
  expect_false(error)
  setwd(orig_wd)
})

test_that("SingleDistrictDemo file works", {
  # temporarily change directory
  orig_wd <- getwd()
  setwd("../..")
  csv_url <- "./demo_files/Single_District_Demo.csv"
  country_name<-"United States"
  implementation_unit_header<-"Georgia"
  drug_name<-"Ivermectin"
  disease_name<-"Onchocerciasis"
  number_of_subunits<-30
  reported_coverage_header<-.7

  error <- runCSB(csv_url, country_name, drug_name, disease_name, implementation_unit_header,
         number_of_subunits, reported_coverage_header)
  expect_false(error)
  setwd(orig_wd)
})
