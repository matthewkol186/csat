############################################################################
# The followings settings are for development.
# Paths need to match the environment that runs this script.
############################################################################

# set working directory and set variables to mimic call from web server
setwd("C:/Users/matth/Documents/csat")

# # configure local settings
# source("unshared_settings.R")

############################################################################
# This file gets uploading to the web server
source("R/survey_builder.R")
############################################################################

############################################################################
# Set values here in development only.  Will be sent from the web server.

# csv_url<- "emory_try.csv"#"sample_KG.csv"
# country_name<-"United States"
# implementation_unit_header<-"Georgia"
# drug_name<-"Ivermectin"
# disease_name<-"Onchocerciasis"
# number_of_subunits<-30
# reported_coverage_header<-.7

csv_url<- "mebendazole_akoko_LGA_Ondo_20190.csv"
country_name<-"Nigeria"
implementation_unit_header<-"Ondo"
drug_name<-"Mebendazole"
disease_name<-"STH"
number_of_subunits<-30
reported_coverage_header<-1.25

############################################################################

############################################################################
# Everything below simulates running the uploaded script from the web server
# attempt to run with variables from user sent from web server

doc<-try(wordDocCSB())

if (inherits(doc, "try-error")) {
        error <- conditionMessage(attr(doc, "condition"))
        browser()
        # call function here to swap error message with message to user
} else {
        error<- "none"
        filename<-c(paste("CSBA_ResultsSummary.docx"))
        writeDoc(doc, file=filename)
}
