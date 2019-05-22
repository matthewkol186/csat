############################################################################
# The followings settings are for development.
# Paths need to match the environment that runs this script.
############################################################################
# configure local settings
# source("unshared_settings.R")

setwd("C:/Users/matth/Documents/csat")

############################################################################
# This file gets uploading to the web server
source("R/user_csv_survey.R")
############################################################################

############################################################################
# Set values here in development only.  Will be sent from the web server.

csv_url <- "./tests/testthat/demo_files/Multidistrict_Demo.csv"
country_name<- "Murkonia"
num_implementation_units<-3
as.numeric(num_implementation_units)
drug_name<-"Azithromycin"
disease_name <- "Trachoma"

implementation_unit_header<-"DISTRICT"
cluster_header<- "VILLAGE_NUMBER"
sex_header<- "SEX"
age_header<-"AGE"
#school_attendance_header<-NULL
offered_drug_header<- "OFFERED_ALB"
swallowed_drug_header<- "SWALLOWED_ALB"
reported_coverage_header<- "REPORTED_COVERAGE"

# csv_url <- "Multidistrict_demo_file.csv"
# country_name<- "Murkonia"
# num_implementation_units<-3
# as.numeric(num_implementation_units)
# drug_name<-"Azithromycin"
# disease_name <- "Trachoma"
#
# implementation_unit_header<-"IU"
# cluster_header<- "Cluster"
# sex_header<- "Sex"
# age_header<-"Age"
# # school_attendance_header<-NULL
# offered_drug_header<- "Drug_Offer"
# swallowed_drug_header<- "Drug_Swallow"
# reported_coverage_header<- "Reported_Coverage"

# csv_url <- "Murkonia_Data.csv"
# country_name <- "Murkonia"
# num_implementation_units <-21
# drug_name <- "Ivermectin"
# disease_name <- "Onchocerciasis"
#
# implementation_unit_header <- "DIST"
# cluster_header <- "CLUSTER"
# sex_header <-"SEY"
# age_header <-"AGE"
# #school_attendance_header<-
# offered_drug_header <-"Q201_ALB"
# swallowed_drug_header <-"Q201_DEC"
# reported_coverage_header <-"r_coverage"

############################################################################

############################################################################
# Everything below simulates running the uploaded script from the web server
# attempt to run with variables from user sent from web server
doc<-try(wordDocUserSurvey())

if (inherits(doc, "try-error")) {
        error <- conditionMessage(attr(doc, "condition"))
        # call function here to swap error message with message to user
} else {
        error<- "none"
        filename<-c(paste("CSA_ResultsSummary.docx"))
        writeDoc(doc, file=filename)
}
