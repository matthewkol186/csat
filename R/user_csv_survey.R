#///////////////////////////////////////////////////////////////#
# Program: Coverage Survey Analysis Tool for Multiple Districts #
# Date Created: 05/11/17                                        #
# Date Updated: 11/9/18                                         #
#                                                               #
# Version:1.0.10                                                #
# Purpose: Create graphics for coverage survey analysis reports #
#          for multiple districts                               #
#                                                               #
# Update from previous version: Age figure will handle groups   #
# without observations & replaced complete cases with           #
# keeping the observations that had all 4 of the required       #
# variables                                                     #
#                                                               #
#                                                               #
# Original Programmer: Lucas Trower 662846587/2230275           #
#                                                               #
# Params:                                                       #
#                                                               #
#   csv_url                                                     #
#   country_name                                                #
#   num_implementation_units                                    #
#   drug_name                                                   #
#   disease_name                                                #
#                                                               #
# Specific to custom data upload:                               #
#                                                               #
#   implementation_unit_header                                  #
#   cluster_header                                              #
#   sex_header                                                  #
#   age_header                                                  #
#   school_attendance_header                                    #
#   offered_drug_header                                         #
#   swallowed_drug_header                                       #
#   reported_coverage_header                                    #
#                                                               #
#///////////////////////////////////////////////////////////////#

library(readr)
library(survey)
library(ggplot2)
library(plyr)
library(ReporteRs)
library(purrr)
library(reshape)
library(PropCIs)

source("R/check_data_validity.R")

wordDoc <- function() {
  dat <- read_csv(csv_url)
  doc <- analyzeData(dat)
  return(doc)
}

# Function used to pull out the mode for district(s), clusters, and reported coverage(s)

mode<-function(x) {
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
}

# set Using either analyzeData1 or analyzeData2 if reported coverage is measured or not
analyzeData <- function(dat) {

  #Importing from csv in place of site uploaded file

  dat<-read_csv(csv_url)

  country<-country_name

  numdist<-as.numeric(num_implementation_units)

  drug<-drug_name

  disease<-disease_name

  #Checking to make sure the drug packages are selected with the appropriate disease
  if (!checkDiseaseDrugMatch(disease, drug)){
    stop("The drug/drug package selected is not used in treatment for the disease selected")
  }

  #Check to see if the column they specfied exists, and rename columns based on user input if they do

  #
  # District
  #

  if (any(colnames(dat)==implementation_unit_header)){
          colnames(dat)[colnames(dat)==implementation_unit_header]<-"district"
  }else{
          stop("Column header for Implementation Unit was not found in the .csv file")
  }

  #
  # Cluster
  #

  if (any(colnames(dat)==cluster_header)){
          colnames(dat)[colnames(dat)==cluster_header]<-"cluster"
  }else{
          stop("Column header for Cluster was not found in the .csv file")
  }

  #
  # Sex
  #

  if (any(colnames(dat)==sex_header)){
          colnames(dat)[colnames(dat)==sex_header]<-"sex"
  }else{
          stop("Column header for Sex was not found in the .csv file")
  }

  #
  # Offered Drug
  #

  if(exists('offered_drug_header')){
          if (any(colnames(dat)==offered_drug_header)){
                  colnames(dat)[colnames(dat)==offered_drug_header]<-"offer"
          }else{
                  stop("Column header for Offered Drug was not found in the .csv file")
          }
  }else{
          dat$offer<-NULL
  }

  #
  # Swallowed Drug
  #

  if (any(colnames(dat)==swallowed_drug_header)){
          colnames(dat)[colnames(dat)==swallowed_drug_header]<-"swallow"
  }else{
          stop("Column header for Swallowed Drug was not found in the .csv file")
  }

  #
  # Age
  #

  if(exists('age_header')){
          if (any(colnames(dat)==age_header)){
    colnames(dat)[colnames(dat)==age_header]<-"age"
          }else{
                  stop("Column header for Age was not found in the .csv file")
          }
  }else{
          dat$age<-NULL
  }

  #
  # School Attendance
  #

  if(exists('school_attendance_header')){
          if (any(colnames(dat)==school_attendance_header)){
                  colnames(dat)[colnames(dat)==school_attendance_header]<-"school"
          }else{
                  stop("Column header for School Attendance was not found in the .csv file")
          }

  }else{
    dat$school<-NULL
  }

  #
  # Reported Coverage
  #

  if(exists('reported_coverage_header')){
          if (any(colnames(dat)==reported_coverage_header)){
                  colnames(dat)[colnames(dat)==reported_coverage_header]<-"coverage"
          }else{
                  stop("Column header for Reported Coverage was not found in the .csv file")
          }

  }else{
    dat$coverage<-NULL
  }

  #If the district variable is a character varaible, this will make all entries
  #upper case in order to avoid errors in case entry from original dataset

  if(is.character(dat$district)){

    dat$district<-toupper(dat$district)

  }

  #If the district variable is a character varaible, this will make all entries
  #upper case in order to avoid errors in case entry from original dataset

  if(is.character(dat$cluster)){

    dat$cluster_header<-toupper(dat$cluster)

  }

  #Passing dat into dat2 to avoid any unecessary changes to the original data

  dat2<-dat

  #Duplicating the information for districts in order to use the Mode function
  #to retrieve the values for labeling purposes

  dat2$district_labs<-dat$district

  #pulls out the value for the districts

  district_labels<-tapply(dat2$district, dat2$district_labs, mode)

  if(length(district_labels)!=numdist){

          stop("Error: The number of Districts/Implementation Units specified
by the user does not match the number detected in the uploaded data.
Please check to make sure there are no variations in the coding of
each district. The program is not case sensitive.")

  }

  #Cluster variable

  dat2$cluster_labs<-dat$cluster

  #Pulls out the values for the clusters

  cluster_names<-tapply(dat2$cluster, dat2$cluster_labs, mode)

  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # subsetting the data set to only have the variables we are interested in       #
  # in order to omit only the observations that are missing these pieces          #
  # of information                                                                #
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # #For some reason this is no longer doing what is needed, but it does not seem to be a problem
  # #I'm leaving it in for future reference in case there are issues with removing certain columns
  #
  #dat2<-dat2[,(!apply(is.na(dat2), 2, any))]

  if (!exists("age", where = dat2) & !exists("offer", where = dat2) & !exists("school", where=dat2) & !exists("coverage", where=dat2)){
    dat2<-dat2[,c("district","cluster","sex", "swallow")]
    dat<-dat[,c("district","cluster","sex", "swallow")]

  }else if (!exists("offer", where = dat2) & !exists("school", where=dat2) & !exists("coverage", where=dat2)){
          dat2<-dat2[,c("district","cluster","sex", "swallow", "age")]
          dat<-dat[,c("district","cluster","sex", "swallow", "age")]

  }else if (!exists("age", where = dat2) & !exists("school", where=dat2) & !exists("coverage", where=dat2)){
          dat2<-dat2[,c("district","cluster","sex", "swallow", "offer")]
          dat<-dat[,c("district","cluster","sex", "swallow", "offer")]

  }else if (!exists("age", where = dat2) & !exists("offer", where = dat2) & !exists("coverage", where=dat2)){
          dat2<-dat2[,c("district","cluster","sex", "swallow", "school")]
          dat<-dat[,c("district","cluster","sex", "swallow", "school")]

  }else if (!exists("age", where = dat2) & !exists("offer", where = dat2) & !exists("school", where=dat2)){
          dat2<-dat2[,c("district","cluster","sex", "swallow", "coverage")]
          dat<-dat[,c("district","cluster","sex", "swallow","coverage")]

  } else if (!exists("offer", where = dat2) & !exists("school", where=dat2)){
    dat2<-dat2[,c("district","cluster","sex", "age", "swallow","coverage")]
    dat<-dat[,c("district","cluster","sex", "age", "swallow","coverage")]

  } else if (!exists("age", where = dat2) & !exists("school", where=dat2)){
    dat2<-dat2[,c("district","cluster","sex","offer", "swallow","coverage")]
    dat<-dat[,c("district","cluster","sex","offer", "swallow","coverage")]

  } else if (!exists("age", where = dat2) & !exists("offer", where = dat2)){
    dat2<-dat2[,c("district","cluster","sex", "swallow", "school","coverage")]
    dat<-dat[,c("district","cluster","sex", "swallow", "school","coverage")]

  } else if (!exists("coverage", where = dat2) & !exists("school", where = dat2)){
          dat2<-dat2[,c("district","cluster","sex", "swallow", "offer", "age")]
          dat<-dat[,c("district","cluster","sex", "swallow", "offer","age")]

  } else if (!exists("coverage", where = dat2) & !exists("age", where = dat2)){
          dat2<-dat2[,c("district","cluster","sex", "swallow", "school", "offer")]
          dat<-dat[,c("district","cluster","sex", "swallow", "school","offer")]

  } else if (!exists("coverage", where = dat2) & !exists("offer", where = dat2)){
          dat2<-dat2[,c("district","cluster","sex", "swallow", "school","age")]
          dat<-dat[,c("district","cluster","sex", "swallow", "school","age")]

  } else if (!exists("school", where=dat2)) {
    dat2<-dat2[,c("district","cluster","sex", "age","offer", "swallow","coverage")]
    dat<-dat[,c("district","cluster","sex", "age","offer", "swallow","coverage")]

  } else if (!exists("offer", where=dat2)) {
    dat2<-dat2[,c("district","cluster","sex", "age", "swallow", "school","coverage")]
    dat<-dat[,c("district","cluster","sex", "age", "swallow", "school","coverage")]

  } else if (!exists("age", where = dat2)) {
          dat2<-dat2[,c("district","cluster","sex","offer", "swallow", "school","coverage")]
          dat<-dat[,c("district","cluster","sex","offer", "swallow", "school","coverage")]

  } else if (!exists("coverage", where = dat2)) {
          dat2<-dat2[,c("district","cluster","sex","offer", "swallow", "school","age")]
          dat<-dat[,c("district","cluster","sex","offer", "swallow", "school","age")]

  } else {
    dat2<-dat2[,c("district","cluster","sex", "age","offer", "swallow", "school","coverage")]
    dat<-dat[,c("district","cluster","sex", "age","offer", "swallow", "school","coverage")]
  }

  #Passing the values for reported coverage from each district into a vector

  if(exists("coverage", where = dat)){

          dat2$coverage_reported<-dat$coverage

  }

  #Creating a vector for reported coverage for figures

  if(exists("coverage", where = dat)){

          r_coverage<-tapply(dat2$coverage_reported, dat2$district, mode)

  }

  if (exists('r_coverage')){

          if (any(0<=r_coverage & r_coverage<=100)){

          }else{
                  stop("Reported coverage was not reported as a decimal between 0 & 1")
          }

  }

  #Omitting any observations with NA values for the required fields

  dat<-subset(dat, (!is.na(dat[,"district"])) & (!is.na(dat[,"cluster"])) &
                      (!is.na(dat[,"sex"])) & (!is.na(dat[,"swallow"])))

  dat2<-subset(dat2, (!is.na(dat2[,"district"])) & (!is.na(dat2[,"cluster"])) &
                      (!is.na(dat2[,"sex"])) & (!is.na(dat2[,"swallow"])))

  #Using either analyzeData1 or analyzeData2 if reported coverage is measured or not

  if (exists("r_coverage")){
    doc<-try(analyzeData1(dat2, country, numdist, r_coverage, drug, disease, district_labels, cluster_names))
  } else{
    doc<-try(analyzeData2(dat2, country, numdist, drug, disease, district_labels, cluster_names))
  }
  return(doc)
}

# Function for when reported coverage is available
analyzeData1<- function(dat2, country, numdist, r_coverage, drug, disease, district_labels, cluster_names){

        #Creating vector that houses the colors for males and females for figures

        colors <- c("#FF9F33", "#428EFC")

        #Selecting threshold based on disease

        setDiseaseThreshold(disease)

        if (disease=="Onchocerciasis" | disease=="Lymphatic Filariasis") {
                threshcol<-"Meets or Exceeds the \n Target 65% Threshold"
        } else if (disease=="Trachoma") {
                threshcol<-"Meets or Exceeds the \n Target 80% Threshold"
        } else threshcol<-"Meets or Exceeds the \n Target 75% Threshold" #Value for STH and Schistosomiasis

        #-----------------------------------------------------------------------#
        #Dropping any observations that do meet our requirements for coding     #
        #-----------------------------------------------------------------------#

        #Only observations with acceptable values for sex will be passed through

        dat5<-subset(dat2, dat2$sex==1 | dat2$sex==2 | dat2$sex==0 | dat2$sex=="Male" |
                             dat2$sex=="MALE" | dat2$sex=="male" | dat2$sex=="M" | dat2$sex== "m" |
                             dat2$sex=="Female" | dat2$sex=="FEMALE" | dat2$sex=="female" | dat2$sex=="F" |
                             dat2$sex=="f")

        #Only observations with acceptable values for offer will be passed through, if measured

        if (exists("offer", where=dat2)){
                dat5<-subset(dat2, dat2$offer==1 |dat2$offer==2 | dat2$offer==0 | dat2$offer=="YES" |
                                     dat2$offer=="Yes" |dat2$offer=="yes" | dat2$offer=="Y" |
                                     dat2$offer=="y" | dat2$offer=="NO" | dat2$offer=="No" |
                                     dat2$offer=="no" | dat2$offer=="N" | dat2$offer=="n")
        }

        #Only observations with acceptable values for swallow will be passed through

        dat5<-subset(dat2, dat2$swallow==1 | dat2$swallow==2 | dat2$swallow==0 | dat2$swallow=="YES" |
                             dat2$swallow=="Yes" |dat2$swallow=="yes" | dat2$swallow=="Y" |
                             dat2$swallow=="y" | dat2$swallow=="NO" | dat2$swallow=="No" |
                             dat2$swallow=="no" | dat2$swallow=="N" | dat2$swallow=="n")

        dat2<-dat5

        ###############

        #-------------------------------------------------------#
        #Creating female and males Interviewed variables        #
        #-------------------------------------------------------#

        #Females (currently acceptable values: 0, 2, F, f, FEMALE, Female, female)
        dat2$Females_Interviewed<-0

        dat2$Females_Interviewed<-ifelse(dat2$sex==2 | dat2$sex==0 | dat2$sex=="F" |
                                                 dat2$sex=="f" | dat2$sex=="FEMALE" |
                                                 dat2$sex=="Female" | dat2$sex=="female",
                                         dat2$Females_Interviewed+1, 0)

        #Males (currently acceptable values: 1, M, m, MALE, Male, male)
        dat2$Males_Interviewed<-0

        dat2$Males_Interviewed<-ifelse(dat2$sex==1 | dat2$sex== "M" | dat2$sex=="m" |
                                               dat2$sex== "MALE" |dat2$sex== "Male" |
                                               dat2$sex== "male",
                                       dat2$Males_Interviewed+1, 0)

        #Creating a sex indicator variable
        dat2$sex_ind<-0

        dat2$sex_ind<-ifelse(dat2$Males_Interviewed==1,dat2$sex_ind+1,0)

        #-----------------------------------------------#
        #Creating female and males Offered variables    #
        #-----------------------------------------------#

        if (exists("offer", where=dat2)){

                #Females
                dat2$Females_Offered<-0

                dat2$Females_Offered<-ifelse(dat2$offer==1 & dat2$Females_Interviewed==1 |
                                                     dat2$offer=="YES" & dat2$Females_Interviewed==1 |
                                                     dat2$offer=="Yes" & dat2$Females_Interviewed==1 |
                                                     dat2$offer=="yes" & dat2$Females_Interviewed==1 |
                                                     dat2$offer=="Y" & dat2$Females_Interviewed==1 |
                                                     dat2$offer=="y" & dat2$Females_Interviewed==1,
                                             dat2$Females_Offered+1, 0)

                dat2$Females_Nooffer<-0

                dat2$Females_Nooffer<-ifelse(dat2$Females_Offered==0 & dat2$Females_Interviewed==1 |
                                                     dat2$Females_Offered=="NO" & dat2$Females_Interviewed==1 |
                                                     dat2$Females_Offered=="No" & dat2$Females_Interviewed==1 |
                                                     dat2$Females_Offered=="no" & dat2$Females_Interviewed==1 |
                                                     dat2$Females_Offered=="N" & dat2$Females_Interviewed==1 |
                                                     dat2$Females_Offered=="n" & dat2$Females_Interviewed==1,
                                             dat2$Females_Nooffer+1, 0)

                #Males
                dat2$Males_Offered<-0

                dat2$Males_Offered<-ifelse(dat2$offer==1 & dat2$Males_Interviewed==1 |
                                                   dat2$offer=="YES" & dat2$Males_Interviewed==1 |
                                                   dat2$offer=="Yes" & dat2$Males_Interviewed==1 |
                                                   dat2$offer=="yes" & dat2$Males_Interviewed==1 |
                                                   dat2$offer=="Y" & dat2$Males_Interviewed==1 |
                                                   dat2$offer=="y" & dat2$Males_Interviewed==1,
                                           dat2$Males_Offered+1, 0)

                dat2$Males_Nooffer<-0

                dat2$Males_Nooffer<-ifelse(dat2$Males_Offered==0 & dat2$Males_Interviewed==1 |
                                                   dat2$Males_Offered=="NO" & dat2$Males_Interviewed==1 |
                                                   dat2$Males_Offered=="No" & dat2$Males_Interviewed==1 |
                                                   dat2$Males_Offered=="no" & dat2$Males_Interviewed==1 |
                                                   dat2$Males_Offered=="N" & dat2$Males_Interviewed==1 |
                                                   dat2$Males_Offered=="n" & dat2$Males_Interviewed==1,
                                           dat2$Males_Nooffer+1, 0)
        }

        #-----------------------------------------------#
        #Creating female and males Swallowed variables  #
        #-----------------------------------------------#

        #Females
        dat2$Females_Swallowed<-0

        dat2$Females_Swallowed<-ifelse(dat2$swallow==1 & dat2$Females_Interviewed==1 |
                                               dat2$swallow=="YES" & dat2$Females_Interviewed==1 |
                                               dat2$swallow=="Yes" & dat2$Females_Interviewed==1 |
                                               dat2$swallow=="yes" & dat2$Females_Interviewed==1 |
                                               dat2$swallow=="Y" & dat2$Females_Interviewed==1 |
                                               dat2$swallow=="y" & dat2$Females_Interviewed==1,
                                       dat2$Females_Swallowed+1, 0)

        dat2$Females_Noswallow<-0

        dat2$Females_Noswallow<-ifelse(dat2$Females_Swallowed==0 & dat2$Females_Interviewed==1 |
                                               dat2$Females_Swallowed=="NO" & dat2$Females_Interviewed==1 |
                                               dat2$Females_Swallowed=="No" & dat2$Females_Interviewed==1 |
                                               dat2$Females_Swallowed=="no" & dat2$Females_Interviewed==1 |
                                               dat2$Females_Swallowed=="N" & dat2$Females_Interviewed==1 |
                                               dat2$Females_Swallowed=="n" & dat2$Females_Interviewed==1,
                                       dat2$Females_Noswallow+1, 0)

        #Males
        dat2$Males_Swallowed<-0

        dat2$Males_Swallowed<-ifelse(dat2$swallow==1 & dat2$Males_Interviewed==1 |
                                             dat2$swallow=="YES" & dat2$Males_Interviewed==1 |
                                             dat2$swallow=="Yes" & dat2$Males_Interviewed==1 |
                                             dat2$swallow=="yes" & dat2$Males_Interviewed==1 |
                                             dat2$swallow=="Y" & dat2$Males_Interviewed==1 |
                                             dat2$swallow=="y" & dat2$Males_Interviewed==1,
                                     dat2$Males_Swallowed+1, 0)

        dat2$Males_Noswallow<-0

        dat2$Males_Noswallow<-ifelse(dat2$Males_Swallowed==0 & dat2$Males_Interviewed==1 |
                                             dat2$swallow=="NO" & dat2$Males_Interviewed==1 |
                                             dat2$swallow=="No" & dat2$Males_Interviewed==1 |
                                             dat2$swallow=="no" & dat2$Males_Interviewed==1 |
                                             dat2$swallow=="N" & dat2$Males_Interviewed==1 |
                                             dat2$swallow=="n" & dat2$Males_Interviewed==1,
                                     dat2$Males_Noswallow+1, 0)

        #---------------------------------------------------------------#
        #Creating the variables interviewed, offered, and swallowed     #
        #---------------------------------------------------------------#

        dat2$Interviewed<-dat2$Females_Interviewed + dat2$Males_Interviewed

        if (exists("offer", where=dat2)){

                dat2$Offered <-dat2$Females_Offered + dat2$Males_Offered
                dat2$No_offer<-dat2$Females_Nooffer + dat2$Males_Nooffer

        }

        dat2$Swallowed<-dat2$Females_Swallowed + dat2$Males_Swallowed
        dat2$No_swallow<-dat2$Females_Noswallow + dat2$Males_Noswallow

        #Only observations with acceptable values for school will be passed through, if measured

        if (exists("school", where=dat2)){
                dat_school<-subset(dat2, dat2$school==1 | dat2$school==0 | dat2$school=="YES" |
                                           dat2$school=="Yes" |dat2$school=="yes" | dat2$school=="Y" |
                                           dat2$school=="y" | dat2$school=="NO" | dat2$school=="No" |
                                           dat2$school=="no" | dat2$school=="N" | dat2$school=="n")
        }

        #-----------------------------------------------------------------------#
        # Creating variables to calculate school vs. non-school coverage        #
        #-----------------------------------------------------------------------#

        if (exists("school", where=dat2)){

                #Creating school indicator variable

                dat_school$school_ind<-0

                dat_school$school_ind<-ifelse(dat_school$school==1 | dat_school$school=="YES" |
                                                dat_school$school=="Yes" |
                                                      dat_school$school=="yes" |
                                                      dat_school$school=="Y" |
                                                      dat_school$school=="y",
                                              dat_school$school_ind+1, dat_school$school_ind+0)

                #Placing the results back into the school variable, in order to avoid
                #changing the code in multiple places

                dat_school$school<-dat_school$school_ind

                #Attended school or not

                school_yes<-tapply(dat_school$school==1, dat_school$district, sum)
                school_no<-tapply(dat_school$school==0, dat_school$district, sum)

                #Those treated within the two categories

                schyescov<-tapply(dat_school$school==1 & dat_school$Swallowed==1, dat_school$district, sum)
                schnocov<-tapply(dat_school$school==0 & dat_school$Swallowed==1, dat_school$district, sum)

                #Calculating coverage by school attendance

                attendyes<-round(100*(schyescov/school_yes),1)
                attendno<-round(100*(schnocov/school_no),1)

        }

        #subset data so that we can get the program reach (number offered/interviewed)
        #and survey coverage (number swallow/interviewed) separately

        if (exists("offer", where=dat2)){

                dat_offer<-dat2[,c("district","Offered","No_offer")]
                dat_fem_offer<-dat2[,c("district","Females_Offered","Females_Nooffer")]
                dat_male_offer<-dat2[,c("district","Males_Offered","Males_Nooffer")]

        }

        dat_swallow<-dat2[,c("district","Swallowed","No_swallow")]
        dat_fem_swallow<-dat2[,c("district","Females_Swallowed","Females_Noswallow")]
        dat_male_swallow<-dat2[,c("district","Males_Swallowed","Males_Noswallow")]

        #Calculating the total number of participants in the survey

        #totpart<-sum(apply(dat_offer[2:3],1,sum))

        #-----------------------------------------------#
        #This will sum up certain variables by district #
        #-----------------------------------------------#

        intv_dist<-tapply(dat2$Interviewed, dat2$district,sum)
        fem_intv<-tapply(dat2$Females_Interviewed, dat2$district, sum)
        male_intv<-tapply(dat2$Males_Interviewed, dat2$district, sum)

        if (exists("offer", where=dat2)){

                off_dist<-tapply(dat2$Offered, dat2$district, sum)
                nooff_dist<-tapply(dat2$No_offer, dat2$district, sum)
                fem_off<-tapply(dat_fem_offer$Females_Offered,dat_fem_offer$district, sum)
                fem_nooff<-tapply(dat_fem_offer$Females_Nooffer,dat_fem_offer$district, sum)
                male_off<- tapply(dat_male_offer$Males_Offered, dat_male_offer$district, sum)
                male_nooff<- tapply(dat_male_offer$Males_Nooffer, dat_male_offer$district, sum)

        }

        swall_dist<-tapply(dat_swallow$Swallowed,dat_swallow$district, sum)
        fem_swall<-tapply(dat_fem_swallow$Females_Swallowed,dat_fem_swallow$district, sum)
        fem_noswall<-tapply(dat_fem_swallow$Females_Noswallow,dat_fem_swallow$district, sum)
        male_swall<-tapply(dat_male_swallow$Males_Swallowed,dat_male_swallow$district, sum)
        male_noswall<-tapply(dat_male_swallow$Males_Noswallow,dat_male_swallow$district, sum)


        #-------------------------------------------------------------#
        # Calculating phat, 95% CI, and the design effect (DEFF)      #
        #-------------------------------------------------------------#

        dat4ci<-dat2[,c("district", "cluster","Swallowed", "No_swallow")]

        fnsurvcov<-function(df){

                design_swallow<-svydesign(ids=~cluster,  data=df)
                result_swallow<-svyciprop(~I(coverage==1),
                                          design_swallow, method="logit", level=0.95)
                ci<-as.vector(attr(result_swallow,"ci"))
                upci<-round(ci[2],3)
                lowci<-round(ci[1],3)
                phat<-round(as.vector(result_swallow),3)
                #get DEFF
                DEFF<-round(as.data.frame(svymean(~factor(coverage, levels=c(1,0)),
                                                  design_swallow, deff="replace"))[1,3],1)

                fnsurvcov<-c(phat, lowci, upci, DEFF)
                return(fnsurvcov)
        }

        #Creating a matrix in order to store the outputs for each district

        output <- matrix(ncol=4, nrow=numdist)

        #for loop used to calculate the phat, 95% CI, and DEFF for each of the districts

        for (p in district_labels){

                sumswall<-subset(dat4ci, district==p & Swallowed==1)

                clustersumswall<-data.frame((tapply(sumswall$Swallowed, sumswall$cluster, sum)))

                clustersumswall$Coverage<-1
                clustersumswall$district<-p
                clustersumswall$cluster <- sort(unique(sumswall$cluster))

                colnames(clustersumswall)<-c("count","coverage","district", "cluster")


                sumnoswall<-subset(dat4ci, district==p & No_swallow==1)

                clustersumnoswall<-data.frame((tapply(sumnoswall$No_swallow, sumnoswall$cluster, sum)))

                clustersumnoswall$Coverage<-0
                clustersumnoswall$district<-p
                clustersumnoswall$cluster <- sort(unique(sumnoswall$cluster))

                colnames(clustersumnoswall)<-c("count","coverage","district", "cluster")

                # Concatenate the two data frames together

                dat_ci95<-rbind(clustersumswall, clustersumnoswall)

                index_swallow<-rep(seq_len(nrow(dat_ci95)),times=dat_ci95$count)

                df_swallow<<-dat_ci95[index_swallow,]

                #placing each district's output into their own row

                output[match(p,district_labels),]<-fnsurvcov(df_swallow)

                print(output)

        }

        cioutput<-data.frame(output)

        # Identifying the ouput information

        colnames(cioutput)<-c("phat", "lowci", "upci", "DEFF")

        # Creating global variables to use since the previous variables for these
        # were global, and this allows me to avoid having to correct them throughout
        # the code

        phat<-cioutput$phat
        lowci<-cioutput$lowci
        upci<-cioutput$upci
        deff<-cioutput$DEFF

        #-----------------------------------------------#
        # Calculating programme reach for each district #
        #-----------------------------------------------#
        if (exists("offer", where=dat2)){
                reach_dist<-round((off_dist/intv_dist),3)
        }

        if (exists("offer", where=dat2)){

                surcovDF<-data.frame(district_labels, paste(r_coverage*100,"%"), paste(phat*100,"%"),
                                     paste(lowci*100, "%"), paste(upci*100,"%"), deff,
                                     paste(reach_dist*100,"%"))

                #Renaming the columns

                colnames(surcovDF) <- c("IU", "Reported Coverage","Survey Coverage",
                                        "Lower 95% Confidence Interval", "Upper 95% Confidence Interval",
                                        "Design Effect", "Programme Reach")

                #Creating the flextable in order to allow for conditional colorization

                surcovFT<-FlexTable(surcovDF, body.par.props = parProperties(padding= 1,
                                                                             text.align = "center"),
                                    header.par.props = parProperties(padding = 3,
                                                                     text.align = "center"),
                                    header.text.props = textProperties(font.weight = "bold",
                                                                       font.family = "Calibri"),
                                    body.text.props = textProperties(font.family = "Calibri"),
                                    header.cell.props = cellProperties(background.color = "#D4D6D8"))

                surcovFT
        }else{
                surcovDF<-data.frame(district_labels, paste(r_coverage*100,"%"), paste(phat*100,"%"),
                                     paste(lowci*100, "%"), paste(upci*100,"%"), deff)

                #Renaming the columns

                colnames(surcovDF) <- c("IU", "Reported Coverage","Survey Coverage",
                                        "Lower 95% Confidence Interval", "Upper 95% Confidence Interval",
                                        "Design Effect")

                #Creating the flextable in order to allow for conditional colorization

                surcovFT<-FlexTable(surcovDF, body.par.props = parProperties(padding= 1,
                                                                             text.align = "center"),
                                    header.par.props = parProperties(padding = 3,
                                                                     text.align = "center"),
                                    header.text.props = textProperties(font.weight = "bold",
                                                                       font.family = "Calibri"),
                                    body.text.props = textProperties(font.family = "Calibri"),
                                    header.cell.props = cellProperties(background.color = "#D4D6D8"))
                surcovFT
        }

        #---------------------------------------------------------#
        # Comparing surveyed coverage with reported coverage      #
        #---------------------------------------------------------#

        if (exists("r_coverage") & length(r_coverage)!=0){
                diff<-round(((100*phat)-(100*r_coverage)),1)
        }

        #-----------------------------------------#
        # Calcluating % reached among the sexes   #
        #-----------------------------------------#

        if (exists("offer", where=dat2)){

                #Females
                reach_pfem<-(round((100*fem_off/fem_intv),1))

                #Males
                reach_pmale<-(round((100*male_off/male_intv),1))

        }

        #-------------------------------------------------------------------------#
        # Calculating % swallowed the drug that were offered among the sexes      #
        #-------------------------------------------------------------------------#

        #Females
        treat_pfem<-(round((100*fem_swall/fem_intv),1))

        #Males
        treat_pmale<-(round((100*male_swall/male_intv),1))

        #----------------------------------------#
        #Compliance rate within each district    #
        #----------------------------------------#

        if (exists("offer", where=dat2)){

                compliance<-(round((100*swall_dist/off_dist),2))

                compli<-data.frame(district_labels, compliance)

        }

        #---------------------------------------------------------------------------------#
        #Plotting the surveyed (with 95% CI) and reported coverage with the WHO threshold #
        #---------------------------------------------------------------------------------#

        #Creating a new dataframe in order to better plot the data

        dat3<-data.frame(district_labels,phat,lowci,upci)

        #Orders the phat values from greatest to least

        dat3$district_labels <- factor(dat3$district_labels, levels = dat3$district_labels[(order(dat3$phat))])

        if (exists("r_coverage") & length(r_coverage)!=0){
                #Plot
                rvsplot<-ggplot(data=dat3, aes(x=district_labels, y=100*phat))+
                        geom_hline(aes(yintercept=thresh, linetype="WHO Target \n Coverage Threshold"),
                                   color="green4")+
                        geom_point(aes(fill="Survey Coverage"))+
                        geom_errorbar(aes(ymin=100*lowci, ymax=100*upci),
                                      width=.3,
                                      position=position_dodge(.9))+
                        geom_point(aes(district_labels,100*r_coverage,color="Reported Coverage"),
                                   size=2, shape=18)+
                        xlab("IU")+
                        ylab(paste("Coverage Results (%) for",drug)) +
                        ggtitle(paste("Reported and Survey Coverage:", "", country)) +
                        theme(
                                legend.position="bottom",
                                plot.title = element_text(hjust = 0.5, size=14, face="bold"),
                                plot.subtitle = element_text(hjust = 0.5, size=14, face="bold"),
                                legend.title = element_blank(),
                                axis.title.y = element_text(face="bold", colour='black', size=10),
                                axis.title.x = element_text(face="bold", colour='black', size=10))+
                        scale_y_continuous(breaks = seq(0, 100, 10))+
                        coord_cartesian(ylim=c(0,100))
                #coord_flip()

                rvsplot

        } else {

                #Plot
                rvsplot<-ggplot(data=dat3, aes(x=district_labels, y=100*phat))+
                        geom_hline(aes(yintercept=thresh, linetype="WHO Target \n Coverage Threshold"),
                                   color="green4")+
                        geom_point(aes(fill="Survey Coverage"))+
                        geom_errorbar(aes(ymin=100*lowci, ymax=100*upci),
                                      width=.3,
                                      position=position_dodge(.9))+
                        xlab("IU")+
                        ylab(paste("Coverage Results (%) for",drug)) +
                        ggtitle(paste("Survey Coverage:", "", country)) +
                        theme(
                                legend.position="bottom",
                                plot.title = element_text(hjust = 0.5, size=14, face="bold"),
                                plot.subtitle = element_text(hjust = 0.5, size=14, face="bold"),
                                legend.title = element_blank(),
                                axis.title.y = element_text(face="bold", colour='black', size=10),
                                axis.title.x = element_text(face="bold", colour='black', size=10))+
                        scale_y_continuous(breaks = seq(0, 100, 10))+
                        coord_cartesian(ylim=c(0,100))
                #coord_flip()

                rvsplot
        }

        #-----------------------------------------------------------------------#
        #Making the table for differences between surveyed and reported (%)     #
        #with conditional coloring for the diff                                #
        #-----------------------------------------------------------------------#

        #Creating new dataframe to manipulate

        phat2<-100*cioutput$phat

        if (exists("r_coverage") & length(r_coverage)!=0){

                repcov<-r_coverage*100

                diffDF<-data.frame(district_labels, phat2, repcov, diff)

                #Adding interpretations for the difference beween surveyed and reported coverage

                diffDF$interp<-ifelse(100*lowci<= repcov & repcov<= 100*upci, "Yes; survey coverage validates \n reported coverage",
                                      ifelse(abs(diffDF[,4])>=0 & abs(diffDF[,4])<=10,"Yes; survey and reported coverage are similar",
                                             ifelse (diffDF[,4]>10 & diffDF[,4]<= 25, "No; survey coverage is greater",
                                                     ifelse(diffDF[,4]>25,"No; survey coverage is much greater",
                                                            ifelse(diffDF[,4]>-25 & diffDF[,4]<= -10, "No; survey coverage is less",
                                                                   "No; survey coverage is much less")))))

                #Adding interpretations for the difference beween surveyed and reported coverage

                diffDF$comp_thresh<-ifelse(phat2>=thresh, "Yes","No")

                #Making sure output is in either alphabetical/numercial order of IU

                diffDF2<-diffDF[order(district_labels),]

                #Renaming the columns

                colnames(diffDF2) <- c("IU", "Survey Coverage","Reported Coverage","Difference",
                                       "Survey Coverage & \n Reported Coverage \n are Similar",
                                       threshcol)

                #Creating the flextable in order to allow for conditional colorization

                diffFT<-FlexTable(diffDF2, body.par.props = parProperties(padding= 1,
                                                                          text.align = "center"),
                                  header.par.props = parProperties(padding = 3,
                                                                   text.align = "center"),
                                  header.text.props = textProperties(font.weight = "bold",
                                                                     font.family = "Calibri"),
                                  body.text.props = textProperties(font.family = "Calibri"),
                                  header.cell.props = cellProperties(background.color = "#D4D6D8"))

                # diffFT<-addHeaderRow(diffFT, textProperties(font.family = "Calibri",
                #                                             font.weight = 'bold'),
                #                      value ="Reported vs. Survey Coverage", colspan = 6,
                #                      cell.properties = cellProperties(background.color = "#D4D6D8"), first = TRUE)

                vars<-"Difference"

                #Cutoff points were just chosen from the example output, can easily be changed

                for (i in vars) {
                        diffFT[abs(diffDF2[i]) >=0 & abs(diffDF2[i]) <= 10, 5] = cellProperties( background.color = "#0DCF00" )
                        diffFT[abs(diffDF2[i]) > 10 & abs(diffDF2[i]) <= 25, 5] = cellProperties( background.color = "#F7FF00" )
                        diffFT[abs(diffDF2[i]) > 25, 5] = cellProperties( background.color = "#FF0000")
                }

                varsint<-threshcol

                for (w in varsint) {
                        diffFT[diffDF2[w]=="Yes", 6] = cellProperties( background.color = "#0DCF00" )
                        diffFT[diffDF2[w]=="No", 6] = cellProperties( background.color = "#FF0000")
                }

                diffFT

        }

        #-----------------------------------------------------------------------#
        #Making the table that explains the interpretations of the RvsS data    #
        #-----------------------------------------------------------------------#

        Conclusion<-c("Yes; survey coverage validates reported coverage", "Yes; survey and reported coverage are similar", "No; survey coverage is...", "No; survey coverage is much...")
        Interpretation<-c("The reported coverage is contained within the 95% confidence interval around the survey coverage.  This means the reported coverage can be considered be “validated” in that IU; no action or improvements are required to the reporting system.",
                          "The reported coverage is outside the 95% confidence interval around the survey coverage but is still within ± 10 percentage points of the survey coverage, which suggests the reporting system is working well; no action or improvements are required to the reporting system.",
                          "The reported coverage is between ± 10 to 25 percentage points different from the survey coverage.  This suggests there could be a problem with the reporting system and action may be required if resources permit.",
                          "The reported coverage is at least ± 25 percentage points different from the survey coverage. This suggests that there is a real problem with the reported coverage and follow-up action to improve the reporting system in the IU is required.")

        difkeyDF<-data.frame(Conclusion, Interpretation)
        colnames(difkeyDF)<-c("Conclusion","Validation Interpretation")

        difkeyFT<-FlexTable(difkeyDF, body.par.props = parProperties(padding= 1,
                                                                     text.align = "center"),
                            header.par.props = parProperties(padding = 3,
                                                             text.align = "center"),
                            header.text.props = textProperties(font.weight = "bold",
                                                               font.family = "Calibri"),
                            body.text.props = textProperties(font.family = "Calibri"))

        difkeyFT[1,1] = cellProperties( background.color = "#0DCF00")
        difkeyFT[2,1] = cellProperties( background.color = "#0DCF00")
        difkeyFT[3,1] = cellProperties( background.color = "#F7FF00")
        difkeyFT[4,1] = cellProperties( background.color = "#FF0000")

        difkeyFT

        #-----------------------------------------------------------------------#
        #Making the table that explains the interpretations of the RvsS data    #
        #-----------------------------------------------------------------------#

        Conclusion2<-c("Yes", "No")
        Interpretation2<-vector(mode="character",length=2)


        threshDF<-data.frame(Conclusion2, Interpretation2)
        colnames(threshDF)<-c("Conclusion","Threshold Interpretation")

        threshFT<-FlexTable(threshDF, body.par.props = parProperties(padding= 1,
                                                                     text.align = "center"),
                            header.par.props = parProperties(padding = 3,
                                                             text.align = "center"),
                            header.text.props = textProperties(font.weight = "bold",
                                                               font.family = "Calibri"),
                            body.text.props = textProperties(font.family = "Calibri"))

        threshFT[1,1] = cellProperties( background.color = "#0DCF00")
        threshFT[2,1] = cellProperties( background.color = "#FF0000")

        threshFT[1,2]<-pot("The surveyed coverage is at or above the threshold for ",textProperties(font.family = "Calibri"))+
                pot(disease_name,textProperties(font.family = "Calibri"))+
                pot(" (",textProperties(font.family = "Calibri"))+
                pot(thresh,textProperties(font.family = "Calibri"))+
                pot("%)",textProperties(font.family = "Calibri"))
        threshFT[2,2]<-pot("The surveyed coverage is below the threshold for ",textProperties(font.family = "Calibri"))+
                pot(disease_name,textProperties(font.family = "Calibri"))+
                pot(" (",textProperties(font.family = "Calibri"))+
                pot(thresh,textProperties(font.family = "Calibri"))+
                pot("%)",textProperties(font.family = "Calibri"))

        threshFT

        #---------------------------------------#
        #Creating Bar Graph for compliance      #
        #---------------------------------------#

        ######################

        #-------------------------------------------------------------#
        # Calculating phat, 95% CI, and the design effect (DEFF)      #
        #-------------------------------------------------------------#

        if (exists("offer", where=dat2)){

                dat4cicomp<-dat2[,c("district", "cluster","Swallowed", "Offered")]

                dat4cicomp<-subset(dat4cicomp, Offered==1)

                dat4cicomp$compliance<-ifelse(dat4cicomp$Swallowed==1, 1, 0)

                fnsurvcompli<-function(df){

                        design_compli<-svydesign(ids=~cluster,  data=df)
                        result_compli<-svyciprop(~I(compliance==1),
                                                 design_compli, method="logit", level=0.95)
                        ci<-as.vector(attr(result_compli,"ci"))
                        upci<-round(ci[2],3)
                        lowci<-round(ci[1],3)
                        phat<-round(as.vector(result_compli),3)

                        fnsurvcompli<-c(phat, lowci, upci)
                        return(fnsurvcompli)
                }

                #No compliance

                fnsurvnocompli<-function(df){

                        design_compli<-svydesign(ids=~cluster,  data=df)
                        result_compli<-svyciprop(~I(compliance==0),
                                                 design_compli, method="logit", level=0.95)
                        ci<-as.vector(attr(result_compli,"ci"))
                        upci<-round(ci[2],3)
                        lowci<-round(ci[1],3)
                        phat<-round(as.vector(result_compli),3)

                        fnsurvnocompli<-c(phat, lowci, upci)
                        return(fnsurvnocompli)
                }

                #Creating a matrix in order to store the outputs for each district

                #Compliance

                output2 <- matrix(ncol=3, nrow=numdist)

                #No compliance

                output3<-matrix(ncol = 3, nrow = numdist)

                #for loop used to calculate the phat, 95% CI, and DEFF for each of the districts

                for (p in district_labels){

                        sumcompli<-subset(dat4cicomp, district==p & compliance==1)

                        #if there is 0% compliance

                        if(nrow(sumcompli)==0){

                                fakedata1<-data.frame(p,1,9,9,0)

                                sumcompli<-rbind(fakedata1)

                                colnames(sumcompli)<-c("district","cluster","Swallowed",
                                                       "Offered", "compliance")
                        }

                        clustersumcompli<-data.frame((tapply(sumcompli$compliance,
                                                             sumcompli$cluster, sum)))

                        if(sumcompli$Offered==9){
                                clustersumcompli$compliance<-0
                        }else{
                                clustersumcompli$compliance<-1
                        }
                        clustersumcompli$district<-p
                        clustersumcompli$cluster<-sort(unique(sumcompli$cluster))

                        colnames(clustersumcompli)<-c("count","compliance","district", "cluster")


                        sumnocompli<-subset(dat4cicomp, district==p & compliance==0)

                        #Changing the value of those that were not in compliance to 1 in order
                        #to sum up the total

                        if(nrow(sumnocompli)!=0){
                                for(m in 1:length(sumnocompli$compliance)){
                                        if(sumnocompli$compliance[m]==0){
                                                sumnocompli$compliance[m]=1
                                        }
                                }
                        }else{ #if there is 100% compliance
                                fakedata2<-data.frame(p,1,9,9,0)

                                sumnocompli<-rbind(fakedata2)

                                colnames(sumnocompli)<-c("district","cluster","Swallowed",
                                                         "Offered", "compliance")
                        }

                        clustersumnocompli<-data.frame((tapply(sumnocompli$compliance,
                                                               sumnocompli$cluster, sum)))
                        if(sumnocompli$Offered==9){
                                clustersumnocompli$compliance<-1
                        }else{
                                clustersumnocompli$compliance<-0
                        }
                        clustersumnocompli$district<-p
                        clustersumnocompli$cluster <- sort(unique(sumnocompli$cluster))

                        colnames(clustersumnocompli)<-c("count","compliance","district", "cluster")

                        # Concatenate the two data frames together

                        dat_ci95compli<-rbind(clustersumcompli, clustersumnocompli)

                        index_compli<-rep(seq_len(nrow(dat_ci95compli)),times=dat_ci95compli$count)

                        df_compli<<-dat_ci95compli[index_compli,]

                        #placing each district's output into their own row

                        output2[match(p,district_labels),]<-fnsurvcompli(df_compli)

                        print(output2)

                        output3[match(p,district_labels),]<-fnsurvnocompli(df_compli)

                        print(output3)

                }

                cioutputcompli<-data.frame(output2)

                cioutputnocompli<-data.frame(output3)

                # Identifying the ouput information

                colnames(cioutputcompli)<-c("phat", "lowci", "upci")

                colnames(cioutputnocompli)<-c("phat", "lowci", "upci")

                # Creating global variables to use since the previous variables for these
                # were global, and this allows me to avoid having to correct them throughout
                # the code

                compliance<-100*cioutputcompli$phat
                lowci_c<-100*cioutputcompli$lowci
                upci_c<-100*cioutputcompli$upci

                compli<-data.frame(district_labels, compliance, lowci_c, upci_c)

                no_compliance<-100*cioutputnocompli$phat
                lowci_nc<-100*cioutputnocompli$lowci
                upci_nc<-100*cioutputnocompli$upci

                n_compli<-data.frame(district_labels,no_compliance, lowci_nc, upci_nc)

        }

        if (exists("offer", where=dat2)){

                #Finding the lowest lower CI to determine the y-axis range for better reading

                # minci<-range(compli$lowci_c,na.rm = T)
                # yrange<-min(minci)

                #ifelse(yrange>=50, lowval<-50, lowval<-0)

                # ifelse(yrange>=90, lowval<-90, ifelse(yrange>=80, lowval<-80,
                #                                       ifelse(yrange>=70, lowval<-70,
                #                                              ifelse(yrange>=60, lowval<-60,
                #                                                     ifelse(yrange>=50, lowval<-50,
                #                                                            ifelse(yrange>=40, lowval<-40,
                #                                                                   ifelse(yrange>=30, lowval<-30,
                #                                                                          ifelse(yrange>=20, lowval<-20,
                #                                                                                 ifelse(yrange>=10, lowval<-10,
                #                                                                                        ifelse(yrange>=0, lowval<-0)
                #                                                                                 )
                #                                                                          )
                #                                                                   )
                #                                                            )
                #                                                     )
                #                                              )
                #                                       )
                # )
                # )
                #
                # ifelse(lowval==90, ytick<-2, ytick<-10)

                compli$district_labels <- factor(compli$district_labels,
                                                   levels = compli$district_labels[rev(order(compli$compliance))])

                #Plot
                compplot<-ggplot(compli, aes(district_labels, compliance, width=.75))+
                        geom_bar(stat="identity",colour="#F58302",fill="#F58302")+
                        geom_errorbar(aes(ymin=compli$lowci_c, ymax=compli$upci_c),
                                      width=.3,
                                      position=position_dodge(.9))+
                        ggtitle("Compliance rate: \n # individuals who swallowed treatment / # individuals who were offered treatment")+
                        ylab(paste("Compliance (%) for", drug))+
                        xlab("IU")+
                        theme(
                                legend.position="bottom",
                                legend.title = element_blank(),
                                panel.grid.major.x=element_blank(),
                                panel.grid.minor.x=element_blank(),
                                plot.title = element_text(hjust = 0.5, size=10, face="bold"),
                                axis.title.y = element_text(face="bold", colour='black', size=10),
                                axis.title.x = element_text(face="bold", colour='black', size=10))+
                        # geom_hline(aes(yintercept=95, linetype="Compliance Threshold"),
                        #            color="#5A87B2")+
                        scale_y_continuous()+
                        coord_cartesian(ylim=c(0,100))+
                        scale_x_discrete(breaks = district_labels)

                compplot

                #Non-compliance plot

                n_compli$district_labels <-factor(n_compli$district_labels,
                                                  levels = n_compli$district_labels[rev(order(n_compli$no_compliance))])

                #Plot
                n_compplot<-ggplot(n_compli, aes(district_labels, no_compliance, width=.75))+
                        geom_bar(stat="identity",colour="#F58302",fill="#F58302")+
                        geom_errorbar(aes(ymin=n_compli$lowci_nc, ymax=n_compli$upci_nc),
                                      width=.3,
                                      position=position_dodge(.9))+
                        ggtitle("Non-Compliance rate: \n # individuals who did not swallow / # individuals who were offered")+
                        ylab(paste("Non-Compliance (%) for", drug))+
                        xlab("IU")+
                        theme(
                                legend.position="bottom",
                                legend.title = element_blank(),
                                panel.grid.major.x=element_blank(),
                                panel.grid.minor.x=element_blank(),
                                plot.title = element_text(hjust = 0.5, size=10, face="bold"),
                                axis.title.y = element_text(face="bold", colour='black', size=10),
                                axis.title.x = element_text(face="bold", colour='black', size=10))+
                        # geom_hline(aes(yintercept=95, linetype="Compliance Threshold"),
                        #            color="#5A87B2")+
                        scale_y_continuous()+
                        coord_cartesian(ylim=c(0,100))+
                        scale_x_discrete(breaks = district_labels)

                n_compplot

        }

        #-------------------------------------------------------------------------------#
        # Calculating the p_value to see if there is a statistical difference between   #
        # females and males being offered and/or taking the drug                        #
        #-------------------------------------------------------------------------------#

        # #Creating data frames to make matrices, in order to calculate the p_value
        #
        # if (exists("offer", where=dat2)){
        #
        #         togetoff<-data.frame(district_labels, fem_off, fem_nooff, male_off, male_nooff)
        #
        # }
        #
        # togetswall<-data.frame(district_labels, fem_swall, fem_noswall, male_swall, male_noswall)
        #
        # var3<-district_labels
        #
        # #blank data frames to insert p_value into
        #
        # if (exists("offer", where=dat2)){
        #
        #         chsqo<-data.frame(matrix(ncol = 1, nrow = length(district_labels)))
        #
        # }
        #
        # chsqs<-data.frame(matrix(ncol = 1, nrow = length(district_labels)))
        #
        # #Loop that will output the p_value to the respective data frame
        #
        # for (k in var3){
        #
        #         if (exists("offer", where=dat2)){
        #
        #                 mat<-matrix(unlist(togetoff[match(k, district_labels),2:5]), nrow = 2)
        #                 chsqo[match(k, district_labels),]<-chisq.test(mat)$p.value
        #
        #         }
        #
        #         mat2<-matrix(unlist(togetswall[match(k, district_labels),2:5]), nrow = 2)
        #         chsqs[match(k, district_labels),]<-chisq.test(mat2)$p.value
        # }
        #
        # #Renaming the column
        #
        # if (exists("offer", where=dat2)){
        #
        #         colnames(chsqo)<-"p-value"
        #
        # }
        #
        # colnames(chsqs)<-"p-value"

        #------------------------------------------------------------#
        # Calculating the chi-square values for sex reach and treat  #
        #------------------------------------------------------------#

        #Calculation for Offered

        if (exists("offer", where=dat2)){

                chi.offer.multi<-matrix(nrow=length(district_labels), ncol=2)

                for (u in district_labels){

                        dat.district<-as.data.frame(subset(dat2, dat2$district==u))

                        dat.district$sex_ind<-as.factor(dat.district$sex_ind)
                        dat.district$Offered<-as.factor(dat.district$Offered)

                        # table(dat.district$cluster, dat.district$sex_ind)
                        # table(dat.district$cluster, dat.district$Offered)

                        design_offer<-svydesign(ids=~cluster, data=dat.district)
                        chi.offer.multi[match(u,district_labels),]<-as.numeric(svychisq(~Offered + sex_ind, design_offer)[3])

                }

                chi.offer.multi[,1]<-district_labels

                chi.offer.multi<-as.data.frame(chi.offer.multi)

                colnames(chi.offer.multi)<-c("IU","pvalue")

                chi.offer.multi$pvalue<-as.numeric(as.character(chi.offer.multi$pvalue))
        }

        #Calculation for Swallowed

        chi.swallow.multi<-matrix(nrow=length(district_labels), ncol=2)

        for (u in district_labels){

                dat.district<-as.data.frame(subset(dat2, dat2$district==u))

                dat.district$sex_ind<-as.factor(dat.district$sex_ind)
                dat.district$Swallowed<-as.factor(dat.district$Swallowed)

                # table(dat.district$cluster, dat.district$sex_ind)
                # table(dat.district$cluster, dat.district$Swallowed)

                design_swallow<-svydesign(ids=~cluster, data=dat.district)
                chi.swallow.multi[match(u,district_labels),]<-as.numeric(svychisq(~Swallowed + sex_ind, design_swallow)[3])

        }

        chi.swallow.multi[,1]<-district_labels

        chi.swallow.multi<-as.data.frame(chi.swallow.multi)

        colnames(chi.swallow.multi)<-c("IU","pvalue")

        chi.swallow.multi$pvalue<-as.numeric(as.character(chi.swallow.multi$pvalue))

        #Creating a new dataframe in order to display the 'Coverage by Sex' table

        if (exists("offer", where=dat2)){
                cover_sex<-data.frame(district_labels, paste(reach_pfem, "%"), paste(treat_pfem, "%"), paste(reach_pmale,"%"), paste(treat_pmale,"%"))

        }else{
                cover_sex<-data.frame(district_labels, paste(treat_pfem, "%"), paste(treat_pmale,"%"))
        }

        #Fill in cell with 'yes' if significant, otherwise, 'no'

        if (exists("offer", where=dat2)){
                cover_sex$compr_male<-ifelse(chi.offer.multi$pvalue>=.05 ,"No", "Yes")
        }

        cover_sex$compt_male<-ifelse(chi.swallow.multi$pvalue>=.05 ,"No", "Yes")

        #Rearranging columns for aesthetics

        if (exists("offer", where=dat2)){

                cover_sex<-cover_sex[,c(1,2,4,6,3,5,7)]

                colnames(cover_sex) <- c("IU", "% Reached amongst females interviewed (offered drug)",
                                         "% Reached amongst males interviewed (offered drug)",
                                         "Programme Reach for females statistically different from males",
                                         "% Treated amongst females reached (swallowed drug)",
                                         "% Treated amongst males reached (swallowed drug)",
                                         "Treatment Coverage for females statistically different from males")

                covsexFT<-FlexTable(cover_sex, body.par.props = parProperties(padding= 1,
                                                                              text.align = "center"),
                                    header.par.props = parProperties(padding = 3,
                                                                     text.align = "center"),
                                    header.text.props = textProperties(font.weight = "bold",
                                                                       font.family = "Calibri"),
                                    body.text.props = textProperties(font.family = "Calibri"))

                vars2<-c("Programme Reach for females statistically different from males",
                         "Treatment Coverage for females statistically different from males")

                #Filling in the yes/no cell with color based on output

                for (j in vars2) {
                        covsexFT[cover_sex[, j]== "Yes" & reach_pfem>reach_pmale, j] = cellProperties( background.color = "#428EFC")
                        covsexFT[cover_sex[, j]== "Yes" & reach_pfem<reach_pmale, j] = cellProperties( background.color = "#FF9F33")
                        covsexFT[cover_sex[, j]== "Yes" & treat_pfem>treat_pmale, j] = cellProperties( background.color = "#428EFC")
                        covsexFT[cover_sex[, j]== "Yes" & treat_pfem<treat_pmale, j] = cellProperties( background.color = "#FF9F33")
                        covsexFT[cover_sex[, j]== "No", j] = cellProperties( background.color = "#D9DADB")

                        #This was the origial code. Leaving it in just in case the new code for some reason does not work in the future

                        # covsexFT[cover_sex[, j]== "Yes" & cover_sex[, 2]>cover_sex[, 3], j] = cellProperties( background.color = "#94C8F1")
                        # covsexFT[cover_sex[, j]== "Yes" & cover_sex[, 2]<cover_sex[, 3], j] = cellProperties( background.color = "#94F1A0")
                        # covsexFT[cover_sex[, j]== "Yes" & cover_sex[, 5]>cover_sex[, 6], j] = cellProperties( background.color = "#94C8F1")
                        # covsexFT[cover_sex[, j]== "Yes" & cover_sex[, 5]<cover_sex[, 6], j] = cellProperties( background.color = "#94F1A0")
                        # covsexFT[cover_sex[, j]== "No", j] = cellProperties( background.color = "#D9DADB")
                }

                covsexFT

        } else {

                colnames(cover_sex) <- c("IU",
                                         "% Treated amongst females reached (swallowed drug)",
                                         "% Treated amongst males reached (swallowed drug)",
                                         "Treatment Coverage for females statistically different from males")

                covsexFT<-FlexTable(cover_sex, body.par.props = parProperties(padding= 1,
                                                                              text.align = "center"),
                                    header.par.props = parProperties(padding = 3,
                                                                     text.align = "center"),
                                    header.text.props = textProperties(font.weight = "bold",
                                                                       font.family = "Calibri"),
                                    body.text.props = textProperties(font.family = "Calibri"))

                vars2<-c("Treatment Coverage for females statistically different from males")

                #Filling in the yes/no cell with color based on output

                for (j in vars2) {
                        covsexFT[cover_sex[, j]== "Yes" & treat_pfem>treat_pmale, j] = cellProperties( background.color = "#428EFC")
                        covsexFT[cover_sex[, j]== "Yes" & treat_pfem<treat_pmale, j] = cellProperties( background.color = "#FF9F33")
                        covsexFT[cover_sex[, j]== "No", j] = cellProperties( background.color = "#D9DADB")
                }

                covsexFT
        }

        #-----------------------------------------------------------------------#
        #Creating grouped bar plots for reach and treament for men and women    #
        #-----------------------------------------------------------------------#

        if (exists("offer", where=dat2)){

                datreach<-data.frame(district_labels, reach_pmale, reach_pfem)

                meltreach<-melt(datreach, id='district_labels')

        }

        dattreat<-data.frame(district_labels, treat_pmale, treat_pfem)

        melttreat<-melt(dattreat, id='district_labels')

        #plot

        if (exists("offer", where=dat2)){

                reachplot<-ggplot(meltreach, aes(district_labels, value, color=variable,
                                                 fill=factor(variable,
                                                             labels=c("Male",
                                                                      "Female")),width=.5))+
                        geom_bar(position = position_dodge(.5),
                                 stat="identity",colour="#000000", size=0)+
                        ggtitle("Programme Reach by Sex")+
                        ylab("Programme Reach (%)")+
                        xlab("IU")+
                        theme(
                                legend.position="bottom",
                                legend.title = element_blank(),
                                panel.grid.major.x=element_blank(),
                                panel.grid.minor.x=element_blank(),
                                plot.title = element_text(hjust = 0.5,
                                                          size=10, face="bold"),
                                axis.title.y = element_text(face="bold",
                                                            colour='black', size=10),
                                axis.title.x = element_text(face="bold",
                                                            colour='black', size=10))+
                        scale_y_continuous(breaks = seq(0, 100, 10))+
                        coord_cartesian(ylim=c(0, 100))+
                        scale_x_continuous(breaks = district_labels)+
                        scale_fill_manual(values = colors)

                reachplot

        }

        #plot

        treatplot<-ggplot(melttreat, aes(district_labels, value, color=variable,
                                         fill=factor(variable,
                                                     labels=c("Male",
                                                              "Female")), width=.5))+
                geom_bar(position = position_dodge(.5),
                         stat="identity",colour="#000000", size=0)+
                ggtitle("Survey Coverage by Sex")+
                ylab("Survey Coverage (%)")+
                xlab("IU")+
                theme(
                        legend.position="bottom",
                        legend.title = element_blank(),
                        panel.grid.major.x=element_blank(),
                        panel.grid.minor.x=element_blank(),
                        plot.title = element_text(hjust = 0.5,
                                                  size=10, face="bold"),
                        axis.title.y = element_text(face="bold",
                                                    colour='black', size=10),
                        axis.title.x = element_text(face="bold",
                                                    colour='black', size=10))+
                scale_y_continuous(breaks = seq(0, 100, 10))+
                coord_cartesian(ylim=c(0, 100))+
                scale_x_continuous(breaks = district_labels)+
                scale_fill_manual(values = colors)

        treatplot

        #----------------------------------#
        # Creating plot of Coverage by Age #
        #----------------------------------#

        if (exists("age", where=dat2) & length(dat2$age)!=0){

                dat2$swallow_ind<-0

                dat2$swallow_ind<-ifelse(dat2$swallow==1 | dat2$swallow=='YES' | dat2$swallow=="Yes" |
                                                 dat2$swallow=="yes" | dat2$swallow=="Y" | dat2$swallow=="y",
                                         dat2$swallow_ind+1, 0)

                dat2$swallow<-dat2$swallow_ind

                if (disease=="Trachoma"){

                        agedf<-data.frame(matrix(nrow = 9, ncol = 2))

                        colnames(agedf)<-c("Ageint", "Ageswa")

                        agedf$Ageint<-0

                        agedf$Ageswa<-0

                        #Summing the total interviewed in each age group

                        agedf[1,1]<-sum(ifelse(0<=dat2$age & dat2$age<=4 , agedf[1,1]+1, agedf[1,1]+0))

                        agedf[2,1]<-sum(ifelse(5<=dat2$age & dat2$age<=9, agedf[2,1]+1, agedf[2,1]+0))

                        agedf[3,1]<-sum(ifelse(10<=dat2$age & dat2$age<=14, agedf[3,1]+1, agedf[3,1]+0))

                        agedf[4,1]<-sum(ifelse(15<=dat2$age & dat2$age<=19, agedf[4,1]+1, agedf[4,1]+0))

                        agedf[5,1]<-sum(ifelse(20<=dat2$age & dat2$age<=29, agedf[5,1]+1, agedf[5,1]+0))

                        agedf[6,1]<-sum(ifelse(30<=dat2$age & dat2$age<=39, agedf[6,1]+1, agedf[6,1]+0))

                        agedf[7,1]<-sum(ifelse(40<=dat2$age & dat2$age<=49, agedf[7,1]+1, agedf[7,1]+0))

                        agedf[8,1]<-sum(ifelse(50<=dat2$age & dat2$age<=59, agedf[8,1]+1, agedf[8,1]+0))

                        agedf[9,1]<-sum(ifelse(dat2$age>=60, agedf[9,1]+1, agedf[9,1]+0))

                        #Summing the total swallowed in each age group

                        agedf[1,2]<-sum(ifelse(0<=dat2$age & dat2$age<=4 & dat2$swallow==1,
                                               agedf[1,2]+1, agedf[1,2]+0))

                        agedf[2,2]<-sum(ifelse(5<=dat2$age & dat2$age<=9 & dat2$swallow==1,
                                               agedf[2,2]+1, agedf[2,2]+0))

                        agedf[3,2]<-sum(ifelse(10<=dat2$age & dat2$age<=14 & dat2$swallow==1,
                                               agedf[3,2]+1, agedf[3,2]+0))

                        agedf[4,2]<-sum(ifelse(15<=dat2$age & dat2$age<=19 & dat2$swallow==1,
                                               agedf[4,2]+1, agedf[4,2]+0))

                        agedf[5,2]<-sum(ifelse(20<=dat2$age & dat2$age<=29 & dat2$swallow==1,
                                               agedf[5,2]+1, agedf[5,2]+0))

                        agedf[6,2]<-sum(ifelse(30<=dat2$age & dat2$age<=39 & dat2$swallow==1,
                                               agedf[6,2]+1, agedf[6,2]+0))

                        agedf[7,2]<-sum(ifelse(40<=dat2$age & dat2$age<=49 & dat2$swallow==1,
                                               agedf[7,2]+1, agedf[7,2]+0))

                        agedf[8,2]<-sum(ifelse(50<=dat2$age & dat2$age<=59 & dat2$swallow==1,
                                               agedf[8,2]+1, agedf[8,2]+0))

                        agedf[9,2]<-sum(ifelse(dat2$age>=60 & dat2$swallow==1,
                                               agedf[9,2]+1, agedf[9,2]+0))

                        #Calculating the coverage in each age group

                        agedf$Agecov<-0

                        agedf$Agecov<-100*(agedf$Ageswa/agedf$Ageint)

                        #Calculating the upper and lower CI for coverage by age

                        agedf$upci_age<-agedf$Agecov+(1.96*sqrt(agedf$Agecov*(100-agedf$Agecov)/agedf$Ageint))
                        agedf$lowci_age<-agedf$Agecov-(1.96*sqrt(agedf$Agecov*(100-agedf$Agecov)/agedf$Ageint))

                        #Setting the age coverage to zero if there was no one in
                        #that age group

                        for (o in 1:nrow(agedf)){

                                if(is.na(agedf$Agecov[o])){

                                        agedf[o,]<-0

                                }

                        }

                        #Determining the scale of the y-axis

                        maxci_age<-range(agedf$upci_age,na.rm = F)
                        yrange2<-max(maxci_age)

                        ifelse(yrange2>=90, upval_age<-100, ifelse(yrange2>=80, upval_age<-90,
                                                                   ifelse(yrange2>=70, upval_age<-80,
                                                                          ifelse(yrange2>=60, upval_age<-70,
                                                                                 ifelse(yrange2>=50, upval_age<-60,
                                                                                        ifelse(yrange2>=40, upval_age<-50,
                                                                                               ifelse(yrange2>=30, upval_age<-40,
                                                                                                      ifelse(yrange2>=20, upval_age<-30,
                                                                                                             ifelse(yrange2>=10, upval_age<-20,
                                                                                                                    ifelse(yrange2>=0, upval_age<-10)
                                                                                                             )
                                                                                                      )
                                                                                               )
                                                                                        )
                                                                                 )
                                                                          )
                                                                   )
                        )
                        )

                        #plot

                        ageplot<-ggplot(agedf, aes(1:9, agedf$Agecov, width=.5))+
                                geom_bar(stat="identity",colour="#5A87B2",fill="#5A87B2")+
                                ggtitle("Coverage by Age")+
                                ylab(paste("Survey Coverage (%) for", drug))+
                                xlab("Age (years)")+
                                theme(
                                        legend.position="bottom",
                                        legend.title = element_blank(),
                                        panel.grid.major.x=element_blank(),
                                        panel.grid.minor.x=element_blank(),
                                        plot.title = element_text(hjust = 0.5,
                                                                  size=10, face="bold"),
                                        axis.title.y = element_text(face="bold",
                                                                    colour='black', size=10),
                                        axis.title.x = element_text(face="bold",
                                                                    colour='black', size=10))+
                                scale_y_continuous(breaks = seq(0, upval_age, 10))+
                                coord_cartesian(ylim=c(0,upval_age))+
                                scale_x_continuous(breaks = seq(1, 9, 1),labels=c("0-4", "5-9", "10-14",
                                                                                  "15-19", "20-29", "30-39",
                                                                                  "40-49","50-59", "60 & up"))
                        print(ageplot)

                } else {

                        agedf<-data.frame(matrix(nrow = 9, ncol = 2))

                        colnames(agedf)<-c("Ageint", "Ageswa")

                        agedf$Ageint<-0

                        agedf$Ageswa<-0

                        #Summing the total interviewed in each age group

                        agedf[1,1]<-sum(ifelse(2<=dat2$age & dat2$age<=4 , agedf[1,1]+1, agedf[1,1]+0))

                        agedf[2,1]<-sum(ifelse(5<=dat2$age & dat2$age<=9, agedf[2,1]+1, agedf[2,1]+0))

                        agedf[3,1]<-sum(ifelse(10<=dat2$age & dat2$age<=14, agedf[3,1]+1, agedf[3,1]+0))

                        agedf[4,1]<-sum(ifelse(15<=dat2$age & dat2$age<=19, agedf[4,1]+1, agedf[4,1]+0))

                        agedf[5,1]<-sum(ifelse(20<=dat2$age & dat2$age<=29, agedf[5,1]+1, agedf[5,1]+0))

                        agedf[6,1]<-sum(ifelse(30<=dat2$age & dat2$age<=39, agedf[6,1]+1, agedf[6,1]+0))

                        agedf[7,1]<-sum(ifelse(40<=dat2$age & dat2$age<=49, agedf[7,1]+1, agedf[7,1]+0))

                        agedf[8,1]<-sum(ifelse(50<=dat2$age & dat2$age<=59, agedf[8,1]+1, agedf[8,1]+0))

                        agedf[9,1]<-sum(ifelse(dat2$age>=60, agedf[9,1]+1, agedf[9,1]+0))

                        #Summing the total swallowed in each age group

                        agedf[1,2]<-sum(ifelse(2<=dat2$age & dat2$age<=4 & dat2$swallow==1,
                                               agedf[1,2]+1, agedf[1,2]+0))

                        agedf[2,2]<-sum(ifelse(5<=dat2$age & dat2$age<=9 & dat2$swallow==1,
                                               agedf[2,2]+1, agedf[2,2]+0))

                        agedf[3,2]<-sum(ifelse(10<=dat2$age & dat2$age<=14 & dat2$swallow==1,
                                               agedf[3,2]+1, agedf[3,2]+0))

                        agedf[4,2]<-sum(ifelse(15<=dat2$age & dat2$age<=19 & dat2$swallow==1,
                                               agedf[4,2]+1, agedf[4,2]+0))

                        agedf[5,2]<-sum(ifelse(20<=dat2$age & dat2$age<=29 & dat2$swallow==1,
                                               agedf[5,2]+1, agedf[5,2]+0))

                        agedf[6,2]<-sum(ifelse(30<=dat2$age & dat2$age<=39 & dat2$swallow==1,
                                               agedf[6,2]+1, agedf[6,2]+0))

                        agedf[7,2]<-sum(ifelse(40<=dat2$age & dat2$age<=49 & dat2$swallow==1,
                                               agedf[7,2]+1, agedf[7,2]+0))

                        agedf[8,2]<-sum(ifelse(50<=dat2$age & dat2$age<=59 & dat2$swallow==1,
                                               agedf[8,2]+1, agedf[8,2]+0))

                        agedf[9,2]<-sum(ifelse(dat2$age>=60 & dat2$swallow==1,
                                               agedf[9,2]+1, agedf[9,2]+0))

                        #Calculating the coverage in each age group

                        agedf$Agecov<-0

                        agedf$Agecov<-100*(agedf$Ageswa/agedf$Ageint)

                        #Calculating the upper and lower CI for coverage by age

                        agedf$upci_age<-agedf$Agecov+(1.96*sqrt(agedf$Agecov*(100-agedf$Agecov)/agedf$Ageint))
                        agedf$lowci_age<-agedf$Agecov-(1.96*sqrt(agedf$Agecov*(100-agedf$Agecov)/agedf$Ageint))

                        #Setting the age coverage to zero if there was no one in
                        #that age group

                        for (o in 1:nrow(agedf)){

                                if(is.na(agedf$Agecov[o])){

                                        agedf[o,]<-0

                                }

                        }



                        #Determining the scale of the y-axis

                        maxci_age<-range(agedf$upci_age, na.rm = F)
                        yrange2<-max(maxci_age)

                        ifelse(yrange2>=90, upval_age<-100, ifelse(yrange2>=80, upval_age<-90,
                                                                   ifelse(yrange2>=70, upval_age<-80,
                                                                          ifelse(yrange2>=60, upval_age<-70,
                                                                                 ifelse(yrange2>=50, upval_age<-60,
                                                                                        ifelse(yrange2>=40, upval_age<-50,
                                                                                               ifelse(yrange2>=30, upval_age<-40,
                                                                                                      ifelse(yrange2>=20, upval_age<-30,
                                                                                                             ifelse(yrange2>=10, upval_age<-20,
                                                                                                                    ifelse(yrange2>=0, upval_age<-10)
                                                                                                             )
                                                                                                      )
                                                                                               )
                                                                                        )
                                                                                 )
                                                                          )
                                                                   )
                        )
                        )

                        #plot

                        ageplot<-ggplot(agedf, aes(1:9, agedf$Agecov, width=.5))+
                                geom_bar(stat="identity",colour="#5A87B2",fill="#5A87B2")+
                                ggtitle("Coverage by Age")+
                                ylab(paste("Survey Coverage (%) for", drug))+
                                xlab("Age (years)")+
                                theme(
                                        legend.position="bottom",
                                        legend.title = element_blank(),
                                        panel.grid.major.x=element_blank(),
                                        panel.grid.minor.x=element_blank(),
                                        plot.title = element_text(hjust = 0.5,
                                                                  size=10, face="bold"),
                                        axis.title.y = element_text(face="bold",
                                                                    colour='black', size=10),
                                        axis.title.x = element_text(face="bold",
                                                                    colour='black', size=10))+
                                scale_y_continuous(breaks = seq(0, upval_age, 10))+
                                coord_cartesian(ylim=c(0,upval_age))+
                                scale_x_continuous(breaks = seq(1, 9, 1),labels=c("0-4", "5-9", "10-14",
                                                                                  "15-19", "20-29", "30-39",
                                                                                  "40-49","50-59", "60 & up"))
                        print(ageplot)

                }
        }

        #-----------------------------------------------------------------------#
        #Creating grouped bar plot for coverage by school attendance            #
        #-----------------------------------------------------------------------#

        if (exists("school", where=dat2)){

                datattend<-data.frame(district_labels, attendno, attendyes)

                meltattend<-melt(datattend, id='district_labels')

                #plot

                attendplot<-ggplot(meltattend, aes(district_labels, value, color=variable,
                                                   fill=factor(variable,
                                                               labels=c("Non-school attending coverage",
                                                                        "School attending coverage")),
                                                   width=.5))+
                        geom_bar(position = position_dodge(.5),
                                 stat="identity",colour="#000000", size=0)+
                        ggtitle("Survey Coverage by School Attendance")+
                        ylab("Survey Coverage (%)")+
                        xlab("IU")+
                        theme(
                                legend.position="bottom",
                                legend.title = element_blank(),
                                panel.grid.major.x=element_blank(),
                                panel.grid.minor.x=element_blank(),
                                plot.title = element_text(hjust = 0.5,
                                                          size=10, face="bold"),
                                axis.title.y = element_text(face="bold",
                                                            colour='black', size=10),
                                axis.title.x = element_text(face="bold",
                                                            colour='black', size=10))+
                        scale_y_continuous(breaks = seq(0, 100, 10))+
                        coord_cartesian(ylim=c(0, 100))+
                        scale_x_continuous(breaks = district_labels)+
                        scale_fill_manual(values = colors)

                attendplot



        }

        #Creating the flex table

        if (exists("offer", where=dat2) & exists("school", where=dat2)){

                compli2<-round(compliance, 1)

                datschlcom<-data.frame(district_labels, paste(compli2,"%"),
                                       paste(attendyes, "%"), paste(attendno,"%"))

                colnames(datschlcom) <- c("IU", "Compliance",
                                          "School-Attending \n Coverage",
                                          "Non-School \n Attending Coverage")

                schlcomFT<-FlexTable(datschlcom, body.par.props = parProperties(padding= 1,
                                                                                text.align = "center"),
                                     header.par.props = parProperties(padding = 3,
                                                                      text.align = "center"),
                                     header.text.props = textProperties(font.weight = "bold",
                                                                        font.family = "Calibri"),
                                     body.text.props = textProperties(font.family = "Calibri"))

                schlcomFT


        } else if (exists("school", where=dat2)) {

                datschlcom<-data.frame(district_labels, paste(attendyes, "%"),
                                       paste(attendno,"%"))

                colnames(datschlcom) <- c("IU",
                                          "School-Attending \n Coverage",
                                          "Non-School \n Attending Coverage")

                schlcomFT<-FlexTable(datschlcom, body.par.props = parProperties(padding= 1,
                                                                                text.align = "center"),
                                     header.par.props = parProperties(padding = 3,
                                                                      text.align = "center"),
                                     header.text.props = textProperties(font.weight = "bold",
                                                                        font.family = "Calibri"),
                                     body.text.props = textProperties(font.family = "Calibri"))

                schlcomFT

        }

        #################################################
        #       Creating compliance with 95% CI table   #
        #################################################

        if (exists("offer", where=dat2)){
                compli2<-round(compliance, 1)

                datcompli_table<-data.frame(district_labels, paste(compli2,"%"),
                                            paste(lowci_c, "%"), paste(upci_c,"%"))

                colnames(datcompli_table) <- c("IU", "Compliance",
                                               "Lower 95% \n Confidence Interval",
                                               "Upper 95% \n Confidence Interval")

                datcompli_FT<-FlexTable(datcompli_table, body.par.props = parProperties(padding= 1,
                                                                                        text.align = "center"),
                                        header.par.props = parProperties(padding = 3,
                                                                         text.align = "center"),
                                        header.text.props = textProperties(font.weight = "bold",
                                                                           font.family = "Calibri"),
                                        body.text.props = textProperties(font.family = "Calibri"))
        }

        #####################################################
        #       Creating non-compliance with 95% CI table   #
        #####################################################

        if (exists("offer", where=dat2)){
                compli3<-round(no_compliance, 1)

                datnocompli_table<-data.frame(district_labels, paste(compli3,"%"),
                                            paste(lowci_nc, "%"), paste(upci_nc,"%"))

                colnames(datnocompli_table) <- c("IU", "Non-Compliance",
                                               "Lower 95% \n Confidence Interval",
                                               "Upper 95% \n Confidence Interval")

                datnocompli_FT<-FlexTable(datnocompli_table, body.par.props = parProperties(padding= 1,
                                                                                        text.align = "center"),
                                        header.par.props = parProperties(padding = 3,
                                                                         text.align = "center"),
                                        header.text.props = textProperties(font.weight = "bold",
                                                                           font.family = "Calibri"),
                                        body.text.props = textProperties(font.family = "Calibri"))
        }


        #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~#
        #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~#
        #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~#
        #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~#
        #~*~*~*~*~*~*~*~*~*~*~*~*~*          REPORT         *~*~*~*~*~*~*~*~*~*~*~*~*~*~#
        #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~#
        #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~#
        #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~#
        #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~#

        #dev.off()

        #^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^#
        # Creating variables that will be displayed in the report   #
        #^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^#

        #Data Acurracy page variables

        if (exists("r_coverage") & length(r_coverage)!=0){

                statval<-0

                statval<-ifelse(lowci*100-5<=100*r_coverage & 100*r_coverage<=upci*100+5,
                                statval+1, statval+0)

                statval<-sum(statval)

                lowrvs<-0
                hirvs<-0
                conrvs<-0

                lowrvs<-ifelse(100*r_coverage<thresh & lowci*100<= thresh & thresh<= upci*100,
                               lowrvs+1, lowrvs+0)
                hirvs<-ifelse( 100*r_coverage>= thresh & upci*100< thresh, hirvs+1,
                               hirvs+0)
                conrvs<-ifelse(100*r_coverage<thresh & upci*100<thresh |
                                       100*r_coverage>=thresh & lowci*100>=thresh,
                               conrvs+1, conrvs+0)

                lowrvs<-sum(lowrvs)
                hirvs<-sum(hirvs)
                conrvs<-sum(conrvs)

        }

        #Coverage page variables

        lowcov<-0

        lowcov<-ifelse((100*lowci)<thresh,lowcov+1, lowcov+0)

        lowcov<-sum(lowcov)

        hicov<-0

        hicov<-ifelse((100*lowci)>=thresh, hicov+1, hicov+0)

        hicov<-sum(hicov)

        #Determining the districts that require coverage improvement

        improve<-data.frame(district_labels, lowci*100)

        colnames(improve)<-c("IU", "lowci")

        improve<-data.frame(ifelse(improve$lowci<thresh, print(improve$IU), NA))
        improve<-na.omit(improve)
        colnames(improve)<-"IU"

        improve<-unlist(improve)

        ifelse(is_empty(improve), improve<-"No IU meets this criteria", improve)

        improve<-paste(improve)


        #^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^#
        # Creating the format for the word document output, and generating the report   #
        #^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^#

        #This is a template to change fonts

        title.font <- textProperties(color="#000000",font.size = 18,
                                     font.weight = 'bold', font.family = 'Calibri' )

        doc<-docx()

        #-----------#
        #Title page #
        #-----------#

        titlepg<-pot(paste(disease, "Coverage Evaluation Results Summary:", country, ",", format(Sys.time(), '%B %Y')),
                     textProperties(font.weight = "bold", font.family = "Calibri",
                                    font.size = 20))
        titlepg

        doc<-addParagraph(doc, titlepg)
        doc <- addParagraph( doc, '', stylename = 'Normal' )

        sumsent<- pot("This summary reviews the results from coverage evaluation surveys for ",
                      textProperties(font.family = "Calibri"))+
                pot(drug, textProperties(font.weight = "bold", font.family = "Calibri"))+
                pot(" that were conducted in ",textProperties(font.family = "Calibri"))+
                pot(country, textProperties(font.weight = "bold", font.family = "Calibri"))+
                pot(" in ",textProperties(font.family = "Calibri"))+
                pot(format(Sys.time(), "%Y"), textProperties(font.weight = "bold", font.family = "Calibri"))+
                pot(".",textProperties(font.family = "Calibri"))

        doc<-addParagraph(doc, sumsent)
        doc <- addParagraph( doc, '', stylename = 'Normal' )

        kt<-pot("Key Terms", textProperties(underlined = TRUE, font.family = "Calibri"))

        doc<-addParagraph(doc, kt)
        doc <- addParagraph( doc, '', stylename = 'Normal' )


        rc<-pot("Reported Coverage-", textProperties(font.weight = "bold", font.family = "Calibri"))+
                pot(" The coverage calculated from data reported by all drug distributors, with census figures or drug distributor reports used to estimate the population denominator.",
                    textProperties(font.family = "Calibri"))

        doc <- addParagraph( doc, rc)
        doc <- addParagraph( doc, '', stylename = 'Normal' )

        sc<-pot("Survey Coverage-", textProperties(font.weight = "bold", font.family = "Calibri"))+
                pot(" Coverage estimated through the use of population-based survey sampling methods. The denominator is the total number of individuals Survey and the numerator is the total number of individuals Survey who were identified as having ingested the drug.",
                    textProperties(font.family = "Calibri"))

        doc <- addParagraph( doc, sc)
        doc <- addParagraph( doc, '', stylename = 'Normal' )

        pr<-pot("Programme Reach-", textProperties(font.weight = "bold", font.family = "Calibri"))+
                pot(" The proportion of people in the survey area who were given the opportunity to receive the preventive chemotherapy, regardless of whether the drug was ingested.",
                    textProperties(font.family = "Calibri"))

        doc <- addParagraph( doc, pr)
        doc <- addParagraph( doc, '', stylename = 'Normal' )

        com<-pot("Compliance-", textProperties(font.weight = "bold", font.family = "Calibri"))+
                pot(" The proportion of people in the survey area who are offered the drug that also swallow the drug.",
                    textProperties(font.family = "Calibri"))

        doc<-addParagraph(doc, com)
        doc <- addParagraph( doc, '', stylename = 'Normal' )

        iudef<-pot("Implementation Unit (IU)-", textProperties(font.weight = "bold", font.family = "Calibri"))+
                pot(" Designated survey areas. Coverage surveys are typically conducted at the district level; however, in some cases they may be done at the province, county, or zonal level.",
                    textProperties(font.family = "Calibri"))

        doc<-addParagraph(doc, iudef)

        doc<- addPageBreak(doc)


        #----------------------------------------------#
        #Inserting Reported vs. Surveyed Coverage plot #
        #----------------------------------------------#

        #System sleep is, for some reason, required in orde for the code to run
        #on other computers/laptops

        Sys.sleep(1)

        #Creation of the 'District Selection' editable table

        dissel<-data.frame(matrix(nrow = 1, ncol = 1))

        colnames(dissel)<-"Implementation Unit Selection"

        dissel[1:1]<-paste("REPLACE WITH YOUR TEXT
Insert text here explaining how and why these IUs were selected for surveys")

        dissel<-FlexTable(dissel,
                          header.par.props = parProperties(padding = 3, text.align = "center"),
                          header.cell.props = cellProperties(background.color = "#B4D3F1"),
                          header.text.props = textProperties(font.family = "Calibri"))

        dissel[1:1]<- cellProperties( background.color = "#D4D6D8", padding.left = 10,
                                      padding.right = 10)

        dissel[1:1]<- textProperties(font.family = "Calibri", font.style = "italic")

        dissel[1:1]<- parProperties(text.align = "center")

        doc<-addFlexTable(doc, dissel)
        doc<- addParagraph( doc, '', stylename = 'Normal')

        doc<-addPlot(doc, fun=print, x=rvsplot)
        doc<- addPageBreak(doc)

        #--------------------------------------------------------------------#
        # Inserting Survey coverage results by district  table               #
        #--------------------------------------------------------------------#

        table1<-pot(paste("Table 1. Survey coverage results by IU in",
                          country, ",",format(Sys.time(), "%Y"),"."),
                    textProperties(font.family = "Calibri",
                                   font.weight = "bold"))
        doc<-addParagraph(doc, table1)

        doc<-addFlexTable(doc, surcovFT)

        doc<- addPageBreak(doc)

        #--------------------------------------------------------------------#
        #Inserting Difference in Reported vs. Surveyed Coverage Report table #
        #--------------------------------------------------------------------#

        if (exists("r_coverage") & length(r_coverage)!=0){

                titlervs<-pot(value = "VALIDATION OF REPORTED COVERAGE", format=title.font)

                doc<-addParagraph(doc, titlervs)

                valdef<-pot("When there is no significant difference between the reported and survey coverage, or when the two figures are relatively close (indicated by the colors in green) then the survey coverage is considered to “validate” the reported coverage.",
                            textProperties(font.family = "Calibri"))

                doc<-addParagraph(doc, valdef)

                doc<- addParagraph( doc, '', stylename = 'Normal' )

                table2<-pot(paste("Table 2. Interpretation of survey coverage results to determine if the survey coverage validates the reported coverage and whether the target threshold is met by each IU,",
                                  country,",", format(Sys.time(), "%Y"),". The coloring of the cells indicates whether programmatic action is required (Green = on track, no action required; Yellow = caution, improvements can be made; Red = inadequate, action is required)."),
                            textProperties(font.family = "Calibri",
                                           font.weight = "bold"))

                doc<-addParagraph(doc, table2)

                doc<-addFlexTable(doc, diffFT)

                doc<- addPageBreak(doc)

                doc<-addFlexTable(doc, difkeyFT)

                doc<- addParagraph( doc, '', stylename = 'Normal' )

                doc<-addFlexTable(doc, threshFT)

                doc<- addParagraph( doc, '', stylename = 'Normal' )

                valthresh<-pot("The desired outcome is to have the surveyed coverage within 10% of the reported coverage, as well as meet WHO's target coverage threshold for ",
                            textProperties(font.family = "Calibri"))+
                        pot(disease,textProperties(font.family = "Calibri"))+
                        pot(".",textProperties(font.family = "Calibri"))

                doc<-addParagraph(doc,valthresh)

                doc<- addPageBreak(doc)
        }



        #-------------------#
        #Data Accuracy Page #
        #-------------------#

        #Creating the Validation of Reported Coverage table

        if (exists("r_coverage") & length(r_coverage)!=0){

                valrc<-data.frame(matrix(nrow = 1, ncol = 1))

                colnames(valrc)<-"Validation of Reported Coverage"

                valrc[1:1]<-paste("REPLACE WITH YOUR TEXT
One purpose of acoverage survey is to determine whether the reported coverage (and hence the administrative reporting system) appears to be accurate. Based on the previous table results comparing Reported vs. Survey coverage, how do the results compare? Does the administrative reporting system appear to be working well? Describe what you observe.
                                  ")

                valrc<-FlexTable(valrc,
                                 header.par.props = parProperties(padding = 3, text.align = "center"),
                                 header.cell.props = cellProperties(background.color = "#B4D3F1"),
                                 header.text.props = textProperties(font.family = "Calibri"))

                valrc[1:1]<- cellProperties( background.color = "#D4D6D8", padding.left = 10,
                                             padding.right = 10)

                valrc[1:1]<- textProperties(font.family = "Calibri", font.style = "italic")

                valrc[1:1]<- parProperties(text.align = "center")

                valrc

                #Creating Data Accuracy Conclusion table

                daaccon<-data.frame(matrix(nrow = 1, ncol = 1))

                colnames(daaccon)<-"Data Accuracy Conclusion"

                daaccon[1:1]<-paste("
                                    Insert text to characterize the overall accuracy of reported coverage


                                    ")

                daaccon<-FlexTable(daaccon,
                                   header.par.props = parProperties(padding = 3, text.align = "center"),
                                   header.cell.props = cellProperties(background.color = "#B4D3F1"),
                                   header.text.props = textProperties(font.family = "Calibri"))

                daaccon[1:1]<- cellProperties( background.color = "#D4D6D8", padding.left = 10,
                                               padding.right = 10)

                daaccon[1:1]<- textProperties(font.family = "Calibri")

                daaccon[1:1]<- parProperties(text.align = "center")

                #Creating the Data Accuracy Recommendation table

                daacrec<-data.frame(matrix(nrow = 1, ncol = 1))

                colnames(daacrec)<-"Data Accuracy Recommendations"

                daacrec[1:1]<-paste("REPLACE WITH YOUR TEXT
If the reported coverage is similar to the survey coverage, then the reported coverage is saidto be “validated” – meaning that the reporting system is working as desiredand the reported coverage estimates can be relied on for determining if the programme is meeting the target coverage threshold. When they are different, further investigation into the reporting system, with tools such as the Data Quality Self-Assessment, may be required to understand where theinaccuracies are likely taking place. What, if any, next steps will your programme take in response to non-validated (or invalid or poor) coverage results? How do results in the Survey IUs change your interpretation of reported coverage in non-Survey IUs? Insert interpretation.

Insert 1-3 sentences which highlight the specific actions that will be taken to improve data accuracy
                                    "
                )

                daacrec<-FlexTable(daacrec,
                                   header.par.props = parProperties(padding = 3, text.align = "center"),
                                   header.cell.props = cellProperties(background.color = "#E49CA7"),
                                   header.text.props = textProperties(font.family = "Calibri"))

                daacrec[1:1]<- cellProperties( background.color = "#D4D6D8", padding.left = 10,
                                               padding.right = 10)

                daacrec[1:1]<- textProperties(font.family = "Calibri", font.style = "italic")

                daacrec[1:1]<- parProperties(text.align = "center")

                daacrec

                #Format page

                #Inserting editable tables

                doc<-addFlexTable(doc, valrc)
                doc<- addParagraph( doc, '', stylename = 'Normal')
                doc<-addFlexTable(doc, daacrec)

                asterisk<-pot(paste("*Reported coverage fell within 5 percentage points of closest survey coverage CI"),
                              textProperties(font.size = 10, font.family = "Calibri"))

                asterisk2<-pot(paste("**within 95% CI"),textProperties(font.size = 10,
                                                                       font.family = "Calibri"))

                # doc<-addParagraph(doc,asterisk)
                # doc<- addParagraph( doc, asterisk2)

        }

        doc<- addParagraph( doc, '', stylename = 'Normal')

        compWHO<-pot("Comparison with WHO Target Threshold of ", textProperties(font.size = 18,
                                                                                font.family = "Calibri",
                                                                                font.weight = "bold"))+
                pot(thresh,textProperties(font.size = 18, font.family = "Calibri",
                                          font.weight = "bold"))+
                pot("%",textProperties(font.size = 18, font.family = "Calibri",
                                       font.weight = "bold"))

        doc<-addParagraph(doc, compWHO)

        WHOtext<-pot("If the survey coverage falls below the target coverage threshold, it is evidence that the MDA is considered “LOW” and in need of improvement. If the survey coverage is well above the target coverage threshold it is evidence that the MDA was successful and the programme is functioning well.",
                     textProperties(font.family = "Calibri", font.style = "italic"))
        doc<-addParagraph(doc, WHOtext)

        WHOastrk<-pot(paste("*Survey coverage is considered ‘good’ when the lower confidence limit exceeds ",
                            thresh,"%", " (Note that in some cases the point estimate of the survey coverage may exceed",
                            thresh, "%", " but the lower limit may not).  Survey coverage is considered ‘low’ if the lower confidence limit falls below ",
                            thresh,"%."), textProperties(font.family = "Calibri", font.style = "italic"))

        WHOastrk

        #Displaying numbers and percentage for good and poor coverage

        doc<- addParagraph( doc, '', stylename = 'Normal')

        highfrac<-pot(paste(round((100*hicov/numdist),0), "% "),
                      textProperties(font.size = 14, font.family = "Calibri"))+
                pot(" ")+pot(" ")+pot(" ")+
                pot(paste(hicov), textProperties(font.weight = "bold",font.family = "Calibri"))+
                pot(" of ", textProperties(font.family = "Calibri"))+
                pot(numdist, textProperties(font.weight = "bold",font.family = "Calibri"))+
                pot(" IUs had GOOD survey coverage (≥",textProperties(font.family = "Calibri"))+
                pot(thresh, textProperties(font.family = "Calibri"))+
                pot("%)* ", textProperties(font.family = "Calibri"))

        highfrac

        doc<-addParagraph(doc, highfrac)
        doc<- addParagraph( doc, '', stylename = 'Normal')

        lowfrac<-pot(paste(round((100*lowcov/numdist),0), "% "),
                     textProperties(font.size = 14, font.family = "Calibri"))+
                pot(" ")+pot(" ")+pot(" ")+
                pot(paste(lowcov), textProperties(font.weight = "bold",font.family = "Calibri"))+
                pot(" of ", textProperties(font.family = "Calibri"))+
                pot(numdist, textProperties(font.weight = "bold",font.family = "Calibri"))+
                pot(" IUs had LOW survey coverage (<",textProperties(font.family = "Calibri"))+
                pot(thresh, textProperties(font.family = "Calibri"))+
                pot("%) ", textProperties(font.family = "Calibri"))

        lowfrac

        doc<-addParagraph(doc, lowfrac)
        doc<- addParagraph( doc, '', stylename = 'Normal' )

        doc<-addParagraph(doc, WHOastrk)

        doc<- addPageBreak(doc)

        #--------------------------------------------#
        #Inserting Comparative Coverage by Sex table #
        #--------------------------------------------#

        title4<-pot(value = "SURVEY COVERAGE BY SEX", format=title.font)

        doc<-addParagraph(doc, title4)
        doc<- addParagraph( doc, '', stylename = 'Normal' )

        if (exists("offer", where=dat2)){

                table3<-pot(paste("Table 3. Programme reach and survey coverage by sex,",
                                  country, ",", format(Sys.time(),"%Y"), "."),
                            textProperties(font.family = "Calibri",
                                           font.weight = "bold"))
        } else {
                table3<-pot(paste("Table 3. Survey coverage by sex,",
                                  country, ",", format(Sys.time(),"%Y"), "."),
                            textProperties(font.family = "Calibri",
                                           font.weight = "bold"))
        }

        doc<-addParagraph(doc, table3)

        doc<-addFlexTable(doc, covsexFT)

        blue<-pot("BLUE", textProperties(color = "#428EFC", font.weight = "bold", font.family = "Calibri"))+
                pot(" indicates females had a significantly higher percentage")

        doc<-addParagraph(doc, blue)

        green<-pot("ORANGE", textProperties(color = "#FF9F33", font.weight = "bold", font.family = "Calibri"))+
                pot(" indicates males had a significantly higher percentage")

        doc<-addParagraph(doc, green)

        doc<- addPageBreak(doc)

        #-----------------------------------------------#
        # Inserting Reach and Coverage by Sex plots     #
        #-----------------------------------------------#

        if (exists("offer", where=dat2)){

                doc<- addPlot(doc, fun=print, x=reachplot)
                doc<- addParagraph( doc, '', stylename = 'Normal' )

        }

        doc<-addPlot(doc, fun=print, x=treatplot)
        doc<- addParagraph( doc, '', stylename = 'Normal' )

        doc<- addPageBreak(doc)

        #--------------------------------#
        # Inserting Coverage by Age plot #
        #--------------------------------#

        if (exists("age", where=dat2) & length(dat2$age)!=0){

                title5<-pot(value = "SURVEY COVERAGE BY AGE*", format=title.font)

                doc<-addParagraph(doc, title5)

                doc<- addParagraph( doc, '', stylename = 'Normal' )

                doc<-addPlot(doc, fun=print, x=ageplot)

                ageastrk1<-pot("*Survey coverage is for all IU(s) combined.  Note that this figure does not take into consideration the age groups that are ineligible for treatment (e.g., children <5 years when ivermectin is distributed) and thus will be reflected in this figure by low coverage among the age group(s) ineligible for the MDA.",
                               textProperties(font.family = "Calibri", font.style = "italic"))

                ageastrk1

                doc<-addParagraph(doc, ageastrk1)
                doc<- addPageBreak(doc)
        }

        #------------------------------#
        #Inserting Non-Compliance Plot #
        #------------------------------#

        if (exists("offer", where=dat2)){

                title6<-pot(value = "NON-COMPLIANCE", format=title.font)

                doc<-addParagraph(doc, title6)

                doc<- addParagraph( doc, '', stylename = 'Normal' )

                doc<-addPlot(doc, fun=print, x=n_compplot)

                doc<- addPageBreak(doc)

        }

        ###################

        #---------------------------#
        #Inserting Compliance table #
        #---------------------------#

        if (exists("offer", where=dat2)){

                doc<-addParagraph(doc, title6)

                table4b<-pot(paste("Table 4. Non-Compliance in ",
                                   country, ",", format(Sys.time(),"%Y"), "."),
                             textProperties(font.family = "Calibri",
                                            font.weight = "bold"))
                table4b

                doc<-addParagraph(doc, table4b)

                doc<-addFlexTable(doc, datnocompli_FT)

                doc<- addPageBreak(doc)

        }

        #----------------------------------------------------#
        # Survey Coverage by School Attendance plot page     #
        #----------------------------------------------------#

        if (exists("school", where=dat2)){

                title8<-pot("SURVEY COVERAGE BY SCHOOL ATTENDANCE", format=title.font)

                doc<-addParagraph(doc, title8)
                doc<- addParagraph( doc, '', stylename = 'Normal' )

                doc<-addPlot(doc, fun=print, x=attendplot)
                doc<- addPageBreak(doc)
        }

        #----------------------------------------------------#
        # Survey Coverage by School Attendance table page    #
        #----------------------------------------------------#

        if (exists("school", where=dat2)){

                if (exists("offer", where=dat2)){
                        table4<-pot(paste("Table 5. Results for compliance and survey coverage by school attendance, by IU in",
                                          country, ",", format(Sys.time(), "%Y"), "."),
                                    textProperties(font.family = "Calibri",
                                                   font.weight = "bold"))
                } else{
                        table4<-pot(paste("Table 4. Results for survey coverage by school attendance, by IU in",
                                          country, ",", format(Sys.time(), "%Y"), "."),
                                    textProperties(font.family = "Calibri",
                                                   font.weight = "bold"))
                }

                doc<-addParagraph(doc, table4)

                doc<-addFlexTable(doc, schlcomFT)
                doc<- addPageBreak(doc)
        }

        #--------------#
        #Coverage Page #
        #--------------#

        #Creating Potential Reasons for Poor Coverage table

        potpoor<-data.frame(matrix(nrow = 1, ncol = 1))

        colnames(potpoor)<-"Potential Reasons for Poor Coverage"

        potpoor[1:1]<-paste("REPLACE WITH YOUR TEXT
If one or more of your districts had POOR coverage, list some potential reasons why this may be. What were the most common reasons given for not being offered the drug? What were the most common reasons for not swallowing the drug when it was offered? Consider the coverage in the different sub-populations to determine whether any particular group is being left out or has lower than average coverage (e.g., males vs. females, school-attendance, particular subdistricts, ethnic minorities, etc.). ")

        potpoor<-FlexTable(potpoor,
                           header.par.props = parProperties(padding = 3, text.align = "center"),
                           header.cell.props = cellProperties(background.color = "#B4D3F1"),
                           header.text.props = textProperties(font.family = "Calibri"))

        potpoor[1:1]<- cellProperties( background.color = "#D4D6D8", padding.left = 10,
                                       padding.right = 10)

        potpoor[1:1]<- textProperties(font.family = "Calibri", font.style = "italic")

        potpoor[1:1]<- parProperties(text.align = "center")

        potpoor

        #Creating Requiring Coverage Improvement table

        #reported coverage is ± 10 percent from point estimate

        if (exists("r_coverage") & exists("offer", where=dat2)){

                improve2<-data.frame(district_labels, phat2, repcov)

                improve2<-data.frame(ifelse(improve2$repcov>10+improve2$phat2 | improve2$repcov<improve2$phat2-10,
                                            print(improve2$district_labels), NA))
                improve2<-na.omit(improve2)
                colnames(improve2)<-"IU"

                improve2<-unlist(improve2)

                improve2<-paste(improve2)

                ifelse(is_empty(improve2), improve2<-"No IU meets this criteria", improve2)

                improve2

                #Compliance is less than 90%

                improve3<-data.frame(district_labels, compliance, lowci_c, thresh)

                improve3<-data.frame(ifelse(improve3$lowci_c<90,
                                            print(improve3$district_labels), NA))
                improve3<-na.omit(improve3)
                colnames(improve3)<-"IU"

                improve3<-unlist(improve3)

                improve3<-paste(improve3)

                ifelse(is_empty(improve3), improve3<-"No IU meets this criteria", improve3)

                reqcovdist<-c(paste(c(improve2), collapse=","), paste(c(improve), collapse = ", "),paste(c(improve3), collapse = ", ") )
                reqcovrationale<-c("The reported coverage is > ± 10 percentage points from the point estimate of the survey coverage",
                                   paste("The survey coverage suggests the true coverage could be below the WHO target threshold of", thresh),
                                   "The compliance suggests the true compliance could be below 90%")

                reqcovDF<-data.frame(reqcovdist, reqcovrationale)

                colnames(reqcovDF)<-c("IU(s)", "Rationale")

                reqcovFT<-FlexTable(reqcovDF, body.par.props = parProperties(padding= 1,
                                                                             text.align = "center"),
                                    header.par.props = parProperties(padding = 3,
                                                                     text.align = "center"),
                                    header.text.props = textProperties(font.weight = "bold",
                                                                       font.family = "Calibri"),
                                    body.text.props = textProperties(font.family = "Calibri"),
                                    header.cell.props = cellProperties(background.color = "#E49CA7"))

                reqcovFT<-addHeaderRow(reqcovFT, "IU(s) where follow-up actions may be needed",
                                       colspan = 2, textProperties(font.weight = "bold", font.family = "Calibri"),
                                       first = TRUE)

                reqcovFT
        } else if (exists("offer", where=dat2)){

                #Compliance is less than 90%

                improve3<-data.frame(district_labels, compliance, lowci_c, thresh)

                improve3<-data.frame(ifelse(improve3$lowci_c<90,
                                            print(improve3$district_labels), NA))
                improve3<-na.omit(improve3)
                colnames(improve3)<-"IU"

                improve3<-unlist(improve3)

                improve3<-paste(improve3)

                ifelse(is_empty(improve3), improve3<-"No IU meets this criteria", improve3)

                reqcovdist<-c(paste(c(improve), collapse = ", "),paste(c(improve3), collapse = ", ") )
                reqcovrationale<-c(paste("The survey coverage suggests the true coverage could be below the WHO target threshold of", thresh),
                                   "The compliance suggests the true compliance could be below 90%")

                reqcovDF<-data.frame(reqcovdist, reqcovrationale)

                colnames(reqcovDF)<-c("IU(s)", "Rationale")

                reqcovFT<-FlexTable(reqcovDF, body.par.props = parProperties(padding= 1,
                                                                             text.align = "center"),
                                    header.par.props = parProperties(padding = 3,
                                                                     text.align = "center"),
                                    header.text.props = textProperties(font.weight = "bold",
                                                                       font.family = "Calibri"),
                                    body.text.props = textProperties(font.family = "Calibri"),
                                    header.cell.props = cellProperties(background.color = "#E49CA7"))

                reqcovFT<-addHeaderRow(reqcovFT, "IU(s) where follow-up actions may be needed",
                                       colspan=2, textProperties(font.weight = "bold", font.family = "Calibri"),
                                       first = TRUE)

                reqcovFT

        } else if (exists("r_coverage")){

                improve2<-data.frame(district_labels, phat2, repcov)

                improve2<-data.frame(ifelse(improve2$repcov>10+improve2$phat2 | improve2$repcov<improve2$phat2-10,
                                            print(improve2$district_labels), NA))
                improve2<-na.omit(improve2)
                colnames(improve2)<-"IU"

                improve2<-unlist(improve2)

                improve2<-paste(improve2)

                ifelse(is_empty(improve2), improve2<-"No IU meets this criteria", improve2)

                improve2

                reqcovdist<-c(paste(c(improve2), collapse=","), paste(c(improve), collapse = ", "))
                reqcovrationale<-c("The reported coverage is > ± 10 percentage points from the point estimate of the survey coverage",
                                   paste("The survey coverage suggests the true coverage could be below the WHO target threshold of", thresh))

                reqcovDF<-data.frame(reqcovdist, reqcovrationale)

                colnames(reqcovDF)<-c("IU(s)", "Rationale")

                reqcovFT<-FlexTable(reqcovDF, body.par.props = parProperties(padding= 1,
                                                                             text.align = "center"),
                                    header.par.props = parProperties(padding = 3,
                                                                     text.align = "center"),
                                    header.text.props = textProperties(font.weight = "bold",
                                                                       font.family = "Calibri"),
                                    body.text.props = textProperties(font.family = "Calibri"),
                                    header.cell.props = cellProperties(background.color = "#E49CA7"))

                reqcovFT<-addHeaderRow(reqcovFT, "IU(s) where follow-up actions may be needed",
                                       colspan=2, textProperties(font.weight = "bold", font.family = "Calibri"),
                                       first = TRUE)

                reqcovFT

        } else {

                reqcovdist<-c(paste(c(improve), collapse = ", "))
                reqcovrationale<-c(paste("The survey coverage suggests the true coverage could be below the WHO target threshold of", thresh))

                reqcovDF<-data.frame(reqcovdist, reqcovrationale)

                colnames(reqcovDF)<-c("IU(s)", "Rationale")

                reqcovFT<-FlexTable(reqcovDF, body.par.props = parProperties(padding= 1,
                                                                             text.align = "center"),
                                    header.par.props = parProperties(padding = 3,
                                                                     text.align = "center"),
                                    header.text.props = textProperties(font.weight = "bold",
                                                                       font.family = "Calibri"),
                                    body.text.props = textProperties(font.family = "Calibri"),
                                    header.cell.props = cellProperties(background.color = "#E49CA7"))

                reqcovFT<-addHeaderRow(reqcovFT, "IU(s) where follow-up actions may be needed",
                                       colspan=2, textProperties(font.weight = "bold", font.family = "Calibri"), first = TRUE)

                reqcovFT
        }

        #Creating the second tip table

        tip2<-data.frame(matrix(nrow = 1, ncol = 1))

        colnames(tip2)<-"Tip"

        tip2[1:1]<-paste("How do trends in the survey IUs change your
                         interpretation of reported coverage in non-survey districts?


                         ")

        tip2<-FlexTable(tip2,
                        header.par.props = parProperties(padding = 3, text.align = "center"),
                        header.cell.props = cellProperties(background.color = "#B4D3F1"),
                        header.text.props = textProperties(font.family = "Calibri"))

        tip2[1:1]<- cellProperties( background.color = "#D4D6D8", padding.left = 10,
                                    padding.right = 10)

        tip2[1:1]<- textProperties(font.family = "Calibri")

        tip2[1:1]<- parProperties(text.align = "center")

        #Creating Coverage Improvement Recommendations table

        covimp<-data.frame(matrix(nrow = 1, ncol = 1))

        colnames(covimp)<-"Coverage Improvement Recommendations"

        covimp[1:1]<-paste("REPLACE WITH YOUR TEXT
What actions will your programme take to improve coverage in the poor performing IUs? Refer to Appendix A for suggestions.

                           ")

        covimp<-FlexTable(covimp,
                          header.par.props = parProperties(padding = 3, text.align = "center"),
                          header.cell.props = cellProperties(background.color = "#B4D3F1"),
                          header.text.props = textProperties(font.family = "Calibri"))

        covimp[1:1]<- cellProperties( background.color = "#D4D6D8", padding.left = 10,
                                      padding.right = 10)

        covimp[1:1]<- textProperties(font.family = "Calibri", font.style = "italic")

        covimp[1:1]<- parProperties(text.align = "center")

        #Format page

        #Page Title

        if (exists("offer", where=dat2)){

                title7<-pot(value = "ACTIONS TO IMPROVE COVERAGE AND/OR COMPLIANCE", format=title.font)

        }else{
                title7<-pot(value = "ACTIONS TO IMPROVE COVERAGE", format=title.font)
        }

        doc<-addParagraph(doc, title7)
        doc<- addParagraph( doc, '', stylename = 'Normal' )

        doc<-addFlexTable(doc, reqcovFT)
        doc<- addParagraph( doc, '', stylename = 'Normal' )

        doc<-addFlexTable(doc, potpoor)
        doc<- addParagraph( doc, '', stylename = 'Normal' )

        doc<-addFlexTable(doc, covimp)
        doc<- addPageBreak(doc)

        #----------------#
        # Appendix A     #
        #----------------#

        title9<-pot("Appendix A. Interpreting and following up reported and survey coverage results",
                    textProperties(font.weight = "bold", font.family = "Calibri",
                                   font.size = 16))

        doc<-addParagraph(doc, title9)

        apndxpar<-pot("This table is taken from the WHO document,", textProperties(font.family = "Calibri"))+
                pot("“Coverage Evaluation Surveys for Preventive Chemotherapy: Field Guide for Implementation”",
                    textProperties(font.weight = "bold", font.family = "Calibri"))+
                pot(" and is intended to help users in developing their own Action Plan as the result of a coverage survey(s).   This table lists the possible findings that can occur when the survey coverage is compared to both the target coverage threshold and the reported coverage.  The table provides potential causes to investigate and corrective actions that can be taken.",
                    textProperties(font.family = "Calibri"))

        doc<- addParagraph(doc, apndxpar)
        doc<- addParagraph( doc, '', stylename = 'Normal' )

        #Table 1

        finding<-c("1. Comparison of survey coverage to target coverage threshold: To get a better estimate of coverage where there is reason to believe routine reporting is incorrect", "Survey coverage is below the target coverage threshold","","Survey coverage is above the target coverage threshold")
        potcause<-c("","Check the coverage in the different sub-populations to determine whether any particular group is being left out or has lower than average coverage (e.g., males vs. females, SAC, particular sub-districts, ethnic minorities, etc.)",
                    "Check the reasons for why the eligible population was not  offered the drug

                    Check the reasons for the eligible population not swallowing the drug",
                    "Communities and drug distributors are motivated and the programme is functioning well")
        corrective<-c("","Develop and implement targeted social mobilization as required

                      Investigate the reasons why the sub-population(s) are not adequately covered and make the appropriate change in MDA strategy/platform to reach the sub-population(s)

                      Consider using Independent Monitoring or the Coverage Supervision Tool with MDA mop-up during the next round to improve coverage",
                      "Tailor corrective action according to reasons given which include strengthening:
                      - Drug supply chain
                      - Social mobilization and information and education campaigns (look at reasons given by those who did swallow the drug)
                      - Drug delivery platform used
                      - Training, supervision and motivation of drug distributors
                      - Communication on adverse events
                      - Capacity of national and/or district level staff",
                      "Congratulate your teams.  Sustain programme momentum for the next year to maintain coverage levels")

        apndxA1DF<-data.frame(finding,potcause,corrective)

        colnames(apndxA1DF)<-c("Finding or observation", "Potential Causes to Investigate", "Corrective action")

        apndxA1FT<-FlexTable(apndxA1DF, body.par.props = parProperties(padding= 1),
                             header.par.props = parProperties(padding = 3,
                                                              text.align = "center"),
                             header.text.props = textProperties(font.weight = "bold",
                                                                font.family = "Calibri"),
                             body.text.props = textProperties(font.family = "Calibri"))

        apndxA1FT[1,]=cellProperties(background.color = "#B6A0C0")

        apndxA1FT <- spanFlexTableRows( apndxA1FT, j = 1, from = 2, to = 3 )

        apndxA1FT<-spanFlexTableColumns(apndxA1FT, i = 1, from = 1, to = 3)

        doc<- addFlexTable(doc, apndxA1FT)
        #doc<- addParagraph( doc, '', stylename = 'Normal' )

        apndxA1FT



        #Table 2

        finding2<-c("2. Comparison of survey coverage with reported coverage: To check if the data reporting system is working well",
                    "Reported coverage is much higher than survey coverage

                    (i.e., routine reporting is likely overestimating true coverage)","",
                    "Reported coverage is much lower than survey coverage

                    (i.e., routine reporting is likely underestimating true coverage)", "",
                    "Reported coverage and survey coverage are similar")
        potcause2<-c("","Drug distributors are incorrectly reporting on ingestion of the drugs",
                     "The total population figure (e.g.,  the denominator) is incorrect or outdated, or people from outside the survey area are also taking the drugs and are being included in the total treatment tallies",
                     "The total population figure (e.g., the denominator) is incorrect or outdated",
                     "Data are not being correctly aggregated or reported ", "A good reporting system is in place")
        corrective2<-c("","Conduct a Data Quality Self-Assessment or Data Quality Assessment to diagnose where the data reporting system is breaking down

                       Improve the skills and motivation of drug distributors through better training and supervision; consider use of mHealth technologies

                       Make improvements to the tally sheets and/or registers used ", "Determine if more accurate population estimates or projections are available and apply a correction factor to routine coverage estimates as appropriate

                       Ask the drug distributors to record and report non-resident individuals ingesting the drugs separately and do not include them in the numerator for calculating PC coverage",
                       "Refer to corrective actions given for the same problem above",
                       "Conduct a Data Quality Self-Assessment or Data Quality Assessment  to diagnose where the data reporting system is breaking down",
                       "Congratulate your teams. Continue using the current reporting system, with increased confidence that it provides a good estimate of PC coverage.  Less expenditure in future surveys (at least for the current survey area) is required.")

        apndxA2DF<-data.frame(finding2,potcause2,corrective2)

        colnames(apndxA2DF)<-c("Finding or observation", "Potential Causes to Investigate", "Corrective action")

        apndxA2FT<-FlexTable(apndxA2DF, body.par.props = parProperties(padding= 1),
                             header.columns = FALSE,
                             header.par.props = parProperties(padding = 3,
                                                              text.align = "center"),
                             header.text.props = textProperties(font.weight = "bold",
                                                                font.family = "Calibri"),
                             body.text.props = textProperties(font.family = "Calibri"))

        apndxA2FT[1,]=cellProperties(background.color = "#E49CA7")

        apndxA2FT <- spanFlexTableRows( apndxA2FT, j = 1, from = 2, to = 3 )

        apndxA2FT <- spanFlexTableRows( apndxA2FT, j = 1, from = 4, to = 5 )

        apndxA2FT<-spanFlexTableColumns(apndxA2FT, i = 1, from = 1, to = 3)

        doc<- addFlexTable(doc, apndxA2FT)
        #doc<- addParagraph( doc, '', stylename = 'Normal' )

        apndxA2FT


        #Table 3

        finding3<-c("3. Comparison of survey coverage with programme reach: To assess compliance and the success of the programs social mobilization and communication strategy",
                    "Survey coverage is less than programme reach","Survey coverage is greater than programme reach",
                    "Survey coverage is close to or equal to programme reach")
        potcause3<-c("","Check the reasons given for why individuals who were offered the drug did not swallow it",
                     "There may be a problem with the coverage survey data.  Check to see if the survey team implemented the questionnaire correctly; recount results and check for arithmetic errors",
                     "Compliance is high")
        corrective3<-c("","Improved information and education campaigns may be needed prior to the next round to increase compliance",
                       "Greater training may be needed to make sure the survey team correctly understands the difference between being offered the drug(s) and swallowing the drugs and that this difference is conveyed correctly to the respondents",
                       "Congratulate the teams. Continue with the current information and education campaigns, as they appear to be working")

        apndxA3DF<-data.frame(finding3,potcause3,corrective3)

        colnames(apndxA3DF)<-c("Finding or observation", "Potential Causes to Investigate", "Corrective action")

        apndxA3FT<-FlexTable(apndxA3DF, body.par.props = parProperties(padding= 1),
                             header.columns = FALSE,
                             header.par.props = parProperties(padding = 3,
                                                              text.align = "center"),
                             header.text.props = textProperties(font.weight = "bold",
                                                                font.family = "Calibri"),
                             body.text.props = textProperties(font.family = "Calibri"))

        apndxA3FT[1,]=cellProperties(background.color = "#2E93CD")

        apndxA3FT<-spanFlexTableColumns(apndxA3FT, i = 1, from = 1, to = 3)

        doc<- addFlexTable(doc, apndxA3FT)

        apndxA3FT

        return(doc)

}

#
#
#
#
#Function for when reported coverage is not available
#
#
#
#

analyzeData2<- function(dat2, country, numdist, drug, disease, district_labels, cluster_names){

        #Creating vector that houses the colors for males and females for figures

        colors <- c("#FF9F33", "#428EFC")

        #Assiging the value for the WHO threshold based on the disease of interest

        if (disease=="Onchocerciasis" | disease=="Lymphatic Filariasis") {
                thresh=65
        } else if (disease=="Trachoma") {
                thresh=80
        } else thresh=75 #Value for STH and Schistosomiasis

        if (disease=="Onchocerciasis" | disease=="Lymphatic Filariasis") {
                threshcol<-"Meets or Exceeds the \n Target 65% Threshold"
        } else if (disease=="Trachoma") {
                threshcol<-"Meets or Exceeds the \n Target 80% Threshold"
        } else threshcol<-"Meets or Exceeds the \n Target 75% Threshold" #Value for STH and Schistosomiasis


        #-----------------------------------------------------------------------#
        #Dropping any observations that do meet our requirements for coding     #
        #-----------------------------------------------------------------------#

        #Only observations with acceptable values for sex will be passed through

        dat5<-subset(dat2, dat2$sex==1 | dat2$sex==2 | dat2$sex==0 | dat2$sex=="Male" |
                             dat2$sex=="MALE" | dat2$sex=="male" | dat2$sex=="M" | dat2$sex== "m" |
                             dat2$sex=="Female" | dat2$sex=="FEMALE" | dat2$sex=="female" | dat2$sex=="F" |
                             dat2$sex=="f")

        #Only observations with acceptable values for offer will be passed through, if measured

        if (exists("offer", where=dat2)){
                dat5<-subset(dat2, dat2$offer==1 |dat2$offer==2 | dat2$offer==0 | dat2$offer=="YES" |
                                     dat2$offer=="Yes" |dat2$offer=="yes" | dat2$offer=="Y" |
                                     dat2$offer=="y" | dat2$offer=="NO" | dat2$offer=="No" |
                                     dat2$offer=="no" | dat2$offer=="N" | dat2$offer=="n")
        }

        #Only observations with acceptable values for swallow will be passed through

        dat5<-subset(dat2, dat2$swallow==1 | dat2$swallow==2 | dat2$swallow==0 | dat2$swallow=="YES" |
                             dat2$swallow=="Yes" |dat2$swallow=="yes" | dat2$swallow=="Y" |
                             dat2$swallow=="y" | dat2$swallow=="NO" | dat2$swallow=="No" |
                             dat2$swallow=="no" | dat2$swallow=="N" | dat2$swallow=="n")

        dat2<-dat5

        #-------------------------------------------------------#
        #Creating female and males Interviewed variables        #
        #-------------------------------------------------------#

        #Females (currently acceptable values: 0, 2, F, f, FEMALE, Female, female)
        dat2$Females_Interviewed<-0

        dat2$Females_Interviewed<-ifelse(dat2$sex==2 | dat2$sex==0 | dat2$sex=="F" | dat2$sex=="f" | dat2$sex=="FEMALE" | dat2$sex=="Female" | dat2$sex=="female",
                                         dat2$Females_Interviewed+1, 0)

        #Males (currently acceptable values: 1, M, m, MALE, Male, male)
        dat2$Males_Interviewed<-0

        dat2$Males_Interviewed<-ifelse(dat2$sex==1 | dat2$sex== "M" | dat2$sex=="m" | dat2$sex== "MALE" |dat2$sex== "Male" | dat2$sex== "male",
                                       dat2$Males_Interviewed+1, 0)

        #-----------------------------------------------#
        #Creating female and males Offered variables    #
        #-----------------------------------------------#

        if (exists("offer", where=dat2)){

                #Females
                dat2$Females_Offered<-0

                dat2$Females_Offered<-ifelse(dat2$offer==1 & dat2$Females_Interviewed==1 | dat2$offer=="YES" & dat2$Females_Interviewed==1 | dat2$offer=="Yes" & dat2$Females_Interviewed==1 | dat2$offer=="yes" & dat2$Females_Interviewed==1 | dat2$offer=="Y" & dat2$Females_Interviewed==1 | dat2$offer=="y" & dat2$Females_Interviewed==1,
                                             dat2$Females_Offered+1, 0)

                dat2$Females_Nooffer<-0

                dat2$Females_Nooffer<-ifelse(dat2$Females_Offered==0 & dat2$Females_Interviewed==1 | dat2$Females_Offered=="NO" & dat2$Females_Interviewed==1 | dat2$Females_Offered=="No" & dat2$Females_Interviewed==1 | dat2$Females_Offered=="no" & dat2$Females_Interviewed==1 | dat2$Females_Offered=="N" & dat2$Females_Interviewed==1 | dat2$Females_Offered=="n" & dat2$Females_Interviewed==1,
                                             dat2$Females_Nooffer+1, 0)

                #Males
                dat2$Males_Offered<-0

                dat2$Males_Offered<-ifelse(dat2$offer==1 & dat2$Males_Interviewed==1 | dat2$offer=="YES" & dat2$Males_Interviewed==1 | dat2$offer=="Yes" & dat2$Males_Interviewed==1 | dat2$offer=="yes" & dat2$Males_Interviewed==1 | dat2$offer=="Y" & dat2$Males_Interviewed==1 | dat2$offer=="y" & dat2$Males_Interviewed==1,
                                           dat2$Males_Offered+1, 0)

                dat2$Males_Nooffer<-0

                dat2$Males_Nooffer<-ifelse(dat2$Males_Offered==0 & dat2$Males_Interviewed==1 | dat2$Males_Offered=="NO" & dat2$Males_Interviewed==1 | dat2$Males_Offered=="No" & dat2$Males_Interviewed==1 | dat2$Males_Offered=="no" & dat2$Males_Interviewed==1 | dat2$Males_Offered=="N" & dat2$Males_Interviewed==1 | dat2$Males_Offered=="n" & dat2$Males_Interviewed==1,
                                           dat2$Males_Nooffer+1, 0)
        }

        #-----------------------------------------------#
        #Creating female and males Swallowed variables  #
        #-----------------------------------------------#

        #Females
        dat2$Females_Swallowed<-0

        dat2$Females_Swallowed<-ifelse(dat2$swallow==1 & dat2$Females_Interviewed==1 | dat2$swallow=="YES" & dat2$Females_Interviewed==1 |dat2$swallow=="Yes" & dat2$Females_Interviewed==1 |dat2$swallow=="yes" & dat2$Females_Interviewed==1 | dat2$swallow=="Y" & dat2$Females_Interviewed==1 | dat2$swallow=="y" & dat2$Females_Interviewed==1,
                                       dat2$Females_Swallowed+1, 0)

        dat2$Females_Noswallow<-0

        dat2$Females_Noswallow<-ifelse(dat2$Females_Swallowed==0 & dat2$Females_Interviewed==1 | dat2$Females_Swallowed=="NO" & dat2$Females_Interviewed==1 | dat2$Females_Swallowed=="No" & dat2$Females_Interviewed==1 | dat2$Females_Swallowed=="no" & dat2$Females_Interviewed==1 | dat2$Females_Swallowed=="N" & dat2$Females_Interviewed==1 | dat2$Females_Swallowed=="n" & dat2$Females_Interviewed==1,
                                       dat2$Females_Noswallow+1, 0)

        #Males
        dat2$Males_Swallowed<-0

        dat2$Males_Swallowed<-ifelse(dat2$swallow==1 & dat2$Males_Interviewed==1 | dat2$swallow=="YES" & dat2$Males_Interviewed==1 | dat2$swallow=="Yes" & dat2$Males_Interviewed==1 | dat2$swallow=="yes" & dat2$Males_Interviewed==1 | dat2$swallow=="Y" & dat2$Males_Interviewed==1 | dat2$swallow=="y" & dat2$Males_Interviewed==1,
                                     dat2$Males_Swallowed+1, 0)

        dat2$Males_Noswallow<-0

        dat2$Males_Noswallow<-ifelse(dat2$Males_Swallowed==0 & dat2$Males_Interviewed==1 | dat2$swallow=="NO" & dat2$Males_Interviewed==1 | dat2$swallow=="No" & dat2$Males_Interviewed==1 | dat2$swallow=="no" & dat2$Males_Interviewed==1 | dat2$swallow=="N" & dat2$Males_Interviewed==1 | dat2$swallow=="n" & dat2$Males_Interviewed==1,
                                     dat2$Males_Noswallow+1, 0)

        #Creating a sex indicator variable
        dat2$sex_ind<-0

        dat2$sex_ind<-ifelse(dat2$Males_Interviewed==1,dat2$sex_ind+1,0)

        #---------------------------------------------------------------#
        #Creating the variables interviewed, offered, and swallowed     #
        #---------------------------------------------------------------#

        dat2$Interviewed<-dat2$Females_Interviewed + dat2$Males_Interviewed

        if (exists("offer", where=dat2)){

                dat2$Offered <-dat2$Females_Offered + dat2$Males_Offered
                dat2$No_offer<-dat2$Females_Nooffer + dat2$Males_Nooffer

        }

        dat2$Swallowed<-dat2$Females_Swallowed + dat2$Males_Swallowed
        dat2$No_swallow<-dat2$Females_Noswallow + dat2$Males_Noswallow

        #Only observations with acceptable values for school will be passed through, if measured

        if (exists("school", where=dat2)){
                dat_school<-subset(dat2, dat2$school==1 | dat2$school==0 | dat2$school=="YES" |
                                           dat2$school=="Yes" |dat2$school=="yes" | dat2$school=="Y" |
                                           dat2$school=="y" | dat2$school=="NO" | dat2$school=="No" |
                                           dat2$school=="no" | dat2$school=="N" | dat2$school=="n")
        }

        #-----------------------------------------------------------------------#
        # Creating variables to calculate school vs. non-school coverage        #
        #-----------------------------------------------------------------------#

        if (exists("school", where=dat2)){

                #Creating school indicator variable

                dat_school$school_ind<-0

                dat_school$school_ind<-ifelse(dat_school$school==1 | dat_school$school=="YES" |
                                                dat_school$school=="Yes" |
                                                dat_school$school=="yes" |
                                                dat_school$school=="Y" |
                                                dat_school$school=="y",
                                        dat_school$school_ind+1, dat_school$school_ind+0)

                #Placing the results back into the school variable, in order to avoid
                #changing the code in multiple places

                dat_school$school<-dat_school$school_ind

                #Attended school or not

                school_yes<-tapply(dat_school$school==1, dat_school$district, sum)
                school_no<-tapply(dat_school$school==0, dat_school$district, sum)

                #Those treated within the two categories

                schyescov<-tapply(dat_school$school==1 & dat_school$Swallowed==1, dat_school$district, sum)
                schnocov<-tapply(dat_school$school==0 & dat_school$Swallowed==1, dat_school$district, sum)

                #Calculating coverage by school attendance

                attendyes<-round(100*(schyescov/school_yes),1)
                attendno<-round(100*(schnocov/school_no),1)

        }

        #subset data so that we can get the program reach (number offered/interviewed)
        #and survey coverage (number swallow/interviewed) separately

        if (exists("offer", where=dat2)){

                dat_offer<-dat2[,c("district","Offered","No_offer")]
                dat_fem_offer<-dat2[,c("district","Females_Offered","Females_Nooffer")]
                dat_male_offer<-dat2[,c("district","Males_Offered","Males_Nooffer")]

        }

        dat_swallow<-dat2[,c("district","Swallowed","No_swallow")]
        dat_fem_swallow<-dat2[,c("district","Females_Swallowed","Females_Noswallow")]
        dat_male_swallow<-dat2[,c("district","Males_Swallowed","Males_Noswallow")]

        #-----------------------------------------------#
        #This will sum up certain variables by district #
        #-----------------------------------------------#

        intv_dist<-tapply(dat2$Interviewed, dat2$district,sum)
        fem_intv<-tapply(dat2$Females_Interviewed, dat2$district, sum)
        male_intv<-tapply(dat2$Males_Interviewed, dat2$district, sum)

        if (exists("offer", where=dat2)){

                off_dist<-tapply(dat2$Offered, dat2$district, sum)
                nooff_dist<-tapply(dat2$No_offer, dat2$district, sum)
                fem_off<-tapply(dat_fem_offer$Females_Offered,dat_fem_offer$district, sum)
                fem_nooff<-tapply(dat_fem_offer$Females_Nooffer,dat_fem_offer$district, sum)
                male_off<- tapply(dat_male_offer$Males_Offered, dat_male_offer$district, sum)
                male_nooff<- tapply(dat_male_offer$Males_Nooffer, dat_male_offer$district, sum)

        }

        swall_dist<-tapply(dat_swallow$Swallowed,dat_swallow$district, sum)
        fem_swall<-tapply(dat_fem_swallow$Females_Swallowed,dat_fem_swallow$district, sum)
        fem_noswall<-tapply(dat_fem_swallow$Females_Noswallow,dat_fem_swallow$district, sum)
        male_swall<-tapply(dat_male_swallow$Males_Swallowed,dat_male_swallow$district, sum)
        male_noswall<-tapply(dat_male_swallow$Males_Noswallow,dat_male_swallow$district, sum)

        #-------------------------------------------------------------#
        # Calculating phat, 95% CI, and the design effect (DEFF)      #
        #-------------------------------------------------------------#

        dat4ci<-dat2[,c("district", "cluster","Swallowed", "No_swallow")]

        fnsurvcov<-function(df){

                design_swallow<-svydesign(ids=~cluster,  data=df)
                result_swallow<-svyciprop(~I(coverage==1),
                                          design_swallow, method="logit", level=0.95)
                ci<-as.vector(attr(result_swallow,"ci"))
                upci<-round(ci[2],3)
                lowci<-round(ci[1],3)
                phat<-round(as.vector(result_swallow),3)
                #get DEFF
                DEFF<-round(as.data.frame(svymean(~factor(coverage, levels=c(1,0)),
                                                  design_swallow, deff="replace"))[1,3],1)

                fnsurvcov<-c(phat, lowci, upci, DEFF)
                return(fnsurvcov)
        }

        #Creating a matrix in order to store the outputs for each district

        output <- matrix(ncol=4, nrow=numdist)

        #for loop used to calculate the phat, 95% CI, and DEFF for each of the districts

        for (p in district_labels){

                sumswall<-subset(dat4ci, district==p & Swallowed==1)

                clustersumswall<-data.frame((tapply(sumswall$Swallowed, sumswall$cluster, sum)))

                clustersumswall$Coverage<-1
                clustersumswall$district<-p
                clustersumswall$cluster <- sort(unique(sumswall$cluster))

                colnames(clustersumswall)<-c("count","coverage","district", "cluster")


                sumnoswall<-subset(dat4ci, district==p & No_swallow==1)

                clustersumnoswall<-data.frame((tapply(sumnoswall$No_swallow, sumnoswall$cluster, sum)))

                clustersumnoswall$Coverage<-0
                clustersumnoswall$district<-p
                clustersumnoswall$cluster <- sort(unique(sumnoswall$cluster))

                colnames(clustersumnoswall)<-c("count","coverage","district", "cluster")

                # Concatenate the two data frames together

                dat_ci95<-rbind(clustersumswall, clustersumnoswall)

                index_swallow<-rep(seq_len(nrow(dat_ci95)),times=dat_ci95$count)

                df_swallow<<-dat_ci95[index_swallow,]

                #placing each district's output into their own row

                output[match(p,district_labels),]<-fnsurvcov(df_swallow)

                print(output)

        }

        cioutput<-data.frame(output)

        # Identifying the ouput information

        colnames(cioutput)<-c("phat", "lowci", "upci", "DEFF")

        # Creating global variables to use since the previous variables for these
        # were global, and this allows me to avoid having to correct them throughout
        # the code

        phat<-cioutput$phat
        lowci<-cioutput$lowci
        upci<-cioutput$upci
        deff<-cioutput$DEFF

        #-----------------------------------------------#
        # Calculating programme reach for each district #
        #-----------------------------------------------#
        if (exists("offer", where=dat2)){
                reach_dist<-round((off_dist/intv_dist),3)
        }

        if (exists("offer", where=dat2)){

                surcovDF<-data.frame(district_labels, paste(phat*100,"%"),
                                     paste(lowci*100, "%"), paste(upci*100,"%"), deff,
                                     paste(reach_dist*100,"%"))

                #Renaming the columns

                colnames(surcovDF) <- c("IU", "Survey Coverage",
                                        "Lower 95% Confidence Interval", "Upper 95% Confidence Interval",
                                        "Design Effect", "Programme Reach")

                #Creating the flextable in order to allow for conditional colorization

                surcovFT<-FlexTable(surcovDF, body.par.props = parProperties(padding= 1,
                                                                             text.align = "center"),
                                    header.par.props = parProperties(padding = 3,
                                                                     text.align = "center"),
                                    header.text.props = textProperties(font.weight = "bold",
                                                                       font.family = "Calibri"),
                                    body.text.props = textProperties(font.family = "Calibri"),
                                    header.cell.props = cellProperties(background.color = "#D4D6D8"))

                surcovFT
        }else{
                surcovDF<-data.frame(district_labels, paste(phat*100,"%"),
                                     paste(lowci*100, "%"), paste(upci*100,"%"), deff)

                #Renaming the columns

                colnames(surcovDF) <- c("IU","Survey Coverage",
                                        "Lower 95% Confidence Interval", "Upper 95% Confidence Interval",
                                        "Design Effect")

                #Creating the flextable in order to allow for conditional colorization

                surcovFT<-FlexTable(surcovDF, body.par.props = parProperties(padding= 1,
                                                                             text.align = "center"),
                                    header.par.props = parProperties(padding = 3,
                                                                     text.align = "center"),
                                    header.text.props = textProperties(font.weight = "bold",
                                                                       font.family = "Calibri"),
                                    body.text.props = textProperties(font.family = "Calibri"),
                                    header.cell.props = cellProperties(background.color = "#D4D6D8"))
                surcovFT
        }

        #-----------------------------------------#
        # Calcluating % reached among the sexes   #
        #-----------------------------------------#

        if (exists("offer", where=dat2)){

                #Females
                reach_pfem<-(round((100*fem_off/fem_intv),1))

                #Males
                reach_pmale<-(round((100*male_off/male_intv),1))

        }

        #-------------------------------------------------------------------------#
        # Calculating % swallowed the drug that were offered among the sexes      #
        #-------------------------------------------------------------------------#

        #Females
        treat_pfem<-(round((100*fem_swall/fem_intv),1))

        #Males
        treat_pmale<-(round((100*male_swall/male_intv),1))

        #---------------------------------------------------------------------------------#
        #Plotting the surveyed (with 95% CI) and reported coverage with the WHO threshold #
        #---------------------------------------------------------------------------------#

        #Creating a new dataframe in order to better plot the data

        dat3<-data.frame(district_labels,phat,lowci,upci)

        #Orders the phat values from greatest to least

        dat3$district_labels <- factor(dat3$district_labels, levels = dat3$district_labels[(order(dat3$phat))])

        #Plot
        rvsplot<-ggplot(data=dat3, aes(x=district_labels, y=100*phat))+
                geom_hline(aes(yintercept=thresh, linetype="WHO Target \n Coverage Threshold"),
                           color="green4")+
                geom_point(aes(fill="Survey Coverage"))+
                geom_errorbar(aes(ymin=100*lowci, ymax=100*upci),
                              width=.3,
                              position=position_dodge(.9))+
                xlab("IU")+
                ylab(paste("Coverage Results (%) for",drug)) +
                ggtitle(paste("Survey Coverage:", "", country)) +
                theme(
                        legend.position="bottom",
                        plot.title = element_text(hjust = 0.5, size=14, face="bold"),
                        plot.subtitle = element_text(hjust = 0.5, size=14, face="bold"),
                        legend.title = element_blank(),
                        axis.title.y = element_text(face="bold", colour='black', size=10),
                        axis.title.x = element_text(face="bold", colour='black', size=10))+
                scale_y_continuous(breaks = seq(0, 100, 10))+
                coord_cartesian(ylim=c(0,100))

        rvsplot

        #-----------------------------------------------------------------------#
        #Making the table for differences between surveyed and reported (%)     #
        #with conditional coloring for the diff                                #
        #-----------------------------------------------------------------------#

        #Creating new dataframe to manipulate

        phat2<-100*cioutput$phat


        #---------------------------------------#
        #Creating Bar Graph for compliance      #
        #---------------------------------------#

        ########################

        #-------------------------------------------------------------#
        # Calculating phat, 95% CI, and the design effect (DEFF)      #
        #-------------------------------------------------------------#

        if (exists("offer", where=dat2)){

                dat4cicomp<-dat2[,c("district", "cluster","Swallowed", "Offered")]

                dat4cicomp<-subset(dat4cicomp, Offered==1)

                dat4cicomp$compliance<-ifelse(dat4cicomp$Swallowed==1, 1, 0)

                fnsurvcompli<-function(df){

                        design_compli<-svydesign(ids=~cluster,  data=df)
                        result_compli<-svyciprop(~I(compliance==1),
                                                 design_compli, method="logit", level=0.95)
                        ci<-as.vector(attr(result_compli,"ci"))
                        upci<-round(ci[2],3)
                        lowci<-round(ci[1],3)
                        phat<-round(as.vector(result_compli),3)

                        fnsurvcompli<-c(phat, lowci, upci)
                        return(fnsurvcompli)
                }

                #No compliance

                fnsurvnocompli<-function(df){

                        design_compli<-svydesign(ids=~cluster,  data=df)
                        result_compli<-svyciprop(~I(compliance==0),
                                                 design_compli, method="logit", level=0.95)
                        ci<-as.vector(attr(result_compli,"ci"))
                        upci<-round(ci[2],3)
                        lowci<-round(ci[1],3)
                        phat<-round(as.vector(result_compli),3)

                        fnsurvnocompli<-c(phat, lowci, upci)
                        return(fnsurvnocompli)
                }

                #Creating a matrix in order to store the outputs for each district

                #Compliance

                output2 <- matrix(ncol=3, nrow=numdist)

                #No compliance

                output3<-matrix(ncol = 3, nrow = numdist)

                #for loop used to calculate the phat, 95% CI, and DEFF for each of the districts

                for (p in district_labels){

                        sumcompli<-subset(dat4cicomp, district==p & compliance==1)

                        #if there is 0% compliance

                        if(nrow(sumcompli)==0){

                                fakedata1<-data.frame(p,1,9,9,0)

                                sumcompli<-rbind(fakedata1)

                                colnames(sumcompli)<-c("district","cluster","Swallowed",
                                                       "Offered", "compliance")
                        }

                        clustersumcompli<-data.frame((tapply(sumcompli$compliance,
                                                             sumcompli$cluster, sum)))

                        if(sumcompli$Offered==9){
                                clustersumcompli$compliance<-0
                        }else{
                                clustersumcompli$compliance<-1
                        }
                        clustersumcompli$district<-p
                        clustersumcompli$cluster<-sort(unique(sumcompli$cluster))

                        colnames(clustersumcompli)<-c("count","compliance","district", "cluster")


                        sumnocompli<-subset(dat4cicomp, district==p & compliance==0)

                        #Changing the value of those that were not in compliance to 1 in order
                        #to sum up the total

                        if(nrow(sumnocompli)!=0){
                                for(m in 1:length(sumnocompli$compliance)){
                                        if(sumnocompli$compliance[m]==0){
                                                sumnocompli$compliance[m]=1
                                        }
                                }
                        }else{ #if there is 100% compliance
                                fakedata2<-data.frame(p,1,9,9,0)

                                sumnocompli<-rbind(fakedata2)

                                colnames(sumnocompli)<-c("district","cluster","Swallowed",
                                                         "Offered", "compliance")
                        }

                        clustersumnocompli<-data.frame((tapply(sumnocompli$compliance,
                                                               sumnocompli$cluster, sum)))
                        if(sumnocompli$Offered==9){
                                clustersumnocompli$compliance<-1
                        }else{
                                clustersumnocompli$compliance<-0
                        }
                        clustersumnocompli$district<-p
                        clustersumnocompli$cluster <- sort(unique(sumnocompli$cluster))

                        colnames(clustersumnocompli)<-c("count","compliance","district", "cluster")

                        # Concatenate the two data frames together

                        dat_ci95compli<-rbind(clustersumcompli, clustersumnocompli)

                        index_compli<-rep(seq_len(nrow(dat_ci95compli)),times=dat_ci95compli$count)

                        df_compli<<-dat_ci95compli[index_compli,]

                        #placing each district's output into their own row

                        output2[match(p,district_labels),]<-fnsurvcompli(df_compli)

                        print(output2)

                        output3[match(p,district_labels),]<-fnsurvnocompli(df_compli)

                        print(output3)

                }

                cioutputcompli<-data.frame(output2)

                cioutputnocompli<-data.frame(output3)

                # Identifying the ouput information

                colnames(cioutputcompli)<-c("phat", "lowci", "upci")

                colnames(cioutputnocompli)<-c("phat", "lowci", "upci")

                # Creating global variables to use since the previous variables for these
                # were global, and this allows me to avoid having to correct them throughout
                # the code

                compliance<-100*cioutputcompli$phat
                lowci_c<-100*cioutputcompli$lowci
                upci_c<-100*cioutputcompli$upci

                compli<-data.frame(district_labels, compliance, lowci_c, upci_c)

                no_compliance<-100*cioutputnocompli$phat
                lowci_nc<-100*cioutputnocompli$lowci
                upci_nc<-100*cioutputnocompli$upci

                n_compli<-data.frame(district_labels,no_compliance, lowci_nc, upci_nc)

        }

        if (exists("offer", where=dat2)){

                #Finding the lowest lower CI to determine the y-axis range for better reading

                # minci<-range(compli$lowci_c,na.rm = T)
                # yrange<-min(minci)

                #ifelse(yrange>=50, lowval<-50, lowval<-0)

                # ifelse(yrange>=90, lowval<-90, ifelse(yrange>=80, lowval<-80,
                #                                       ifelse(yrange>=70, lowval<-70,
                #                                              ifelse(yrange>=60, lowval<-60,
                #                                                     ifelse(yrange>=50, lowval<-50,
                #                                                            ifelse(yrange>=40, lowval<-40,
                #                                                                   ifelse(yrange>=30, lowval<-30,
                #                                                                          ifelse(yrange>=20, lowval<-20,
                #                                                                                 ifelse(yrange>=10, lowval<-10,
                #                                                                                        ifelse(yrange>=0, lowval<-0)
                #                                                                                 )
                #                                                                          )
                #                                                                   )
                #                                                            )
                #                                                     )
                #                                              )
                #                                       )
                # )
                # )
                #
                # ifelse(lowval==90, ytick<-2, ytick<-10)

                n_compli$district_labels <- factor(compli$district_labels,
                                                   levels = compli$district_labels[rev(order(compli$compliance))])

                #Plot
                compplot<-ggplot(compli, aes(district_labels, compliance, width=.75))+
                        geom_bar(stat="identity",colour="#F58302",fill="#F58302")+
                        geom_errorbar(aes(ymin=compli$lowci_c, ymax=compli$upci_c),
                                      width=.3,
                                      position=position_dodge(.9))+
                        ggtitle("Compliance rate: \n # individuals who swallowed treatment / # individuals who were offered treatment")+
                        ylab(paste("Compliance (%) for", drug))+
                        xlab("IU")+
                        theme(
                                legend.position="bottom",
                                legend.title = element_blank(),
                                panel.grid.major.x=element_blank(),
                                panel.grid.minor.x=element_blank(),
                                plot.title = element_text(hjust = 0.5, size=10, face="bold"),
                                axis.title.y = element_text(face="bold", colour='black', size=10),
                                axis.title.x = element_text(face="bold", colour='black', size=10))+
                        # geom_hline(aes(yintercept=95, linetype="Compliance Threshold"),
                        #            color="#5A87B2")+
                        scale_y_continuous()+
                        coord_cartesian(ylim=c(0,100))+
                        scale_x_discrete(breaks = district_labels)

                compplot

                #Non-compliance plot

                n_compli$district_labels <-factor(n_compli$district_labels,
                                                  levels = n_compli$district_labels[rev(order(n_compli$no_compliance))])

                #Plot
                n_compplot<-ggplot(n_compli, aes(district_labels, no_compliance, width=.75))+
                        geom_bar(stat="identity",colour="#F58302",fill="#F58302")+
                        geom_errorbar(aes(ymin=n_compli$lowci_nc, ymax=n_compli$upci_nc),
                                      width=.3,
                                      position=position_dodge(.9))+
                        ggtitle("Non-Compliance rate: \n # individuals who did not swallow / # individuals who were offered")+
                        ylab(paste("Non-Compliance (%) for", drug))+
                        xlab("IU")+
                        theme(
                                legend.position="bottom",
                                legend.title = element_blank(),
                                panel.grid.major.x=element_blank(),
                                panel.grid.minor.x=element_blank(),
                                plot.title = element_text(hjust = 0.5, size=10, face="bold"),
                                axis.title.y = element_text(face="bold", colour='black', size=10),
                                axis.title.x = element_text(face="bold", colour='black', size=10))+
                        # geom_hline(aes(yintercept=95, linetype="Compliance Threshold"),
                        #            color="#5A87B2")+
                        scale_y_continuous()+
                        coord_cartesian(ylim=c(0,100))+
                        scale_x_discrete(breaks = district_labels)

                n_compplot

        }

        #-------------------------------------------------------------------------------#
        # Calculating the p_value to see if there is a statistical difference between   #
        # females and males being offered and/or taking the drug                        #
        #-------------------------------------------------------------------------------#

        # #Creating data frames to make matrices, in order to calculate the p_value
        #
        # if (exists("offer", where=dat2)){
        #
        #         togetoff<-data.frame(district_labels, fem_off, fem_nooff, male_off, male_nooff)
        #
        # }
        #
        # togetswall<-data.frame(district_labels, fem_swall, fem_noswall, male_swall, male_noswall)
        #
        # var3<-c(district_labels)
        #
        # #blank data frames to insert p_value into
        #
        # if (exists("offer", where=dat2)){
        #
        #         chsqo<-data.frame(matrix(ncol = 1, nrow = numdist))
        #
        # }
        #
        # chsqs<-data.frame(matrix(ncol = 1, nrow = numdist))
        #
        # #Loop that will output the p_value to the respective data frame
        #
        # for (k in var3){
        #
        #         if (exists("offer", where=dat2)){
        #
        #                 mat<-matrix(unlist(togetoff[match(k,district_labels),2:5]), nrow = 2)
        #                 chsqo[match(k,district_labels),]<-chisq.test(mat)$p.value
        #
        #         }
        #
        #         mat2<-matrix(unlist(togetswall[match(k,district_labels),2:5]), nrow = 2)
        #         chsqs[match(k,district_labels),]<-chisq.test(mat2)$p.value
        # }
        #
        # #Renaming the column
        #
        # if (exists("offer", where=dat2)){
        #
        #         colnames(chsqo)<-"p-value"
        #
        # }
        #
        # colnames(chsqs)<-"p-value"

        #------------------------------------------------------------#
        # Calculating the chi-square values for sex reach and treat  #
        #------------------------------------------------------------#

        #Calculation for Offered

        if (exists("offer", where=dat2)){

                chi.offer.multi<-matrix(nrow=length(district_labels), ncol=2)

                for (u in district_labels){

                        dat.district<-as.data.frame(subset(dat2, dat2$district==u))

                        dat.district$sex_ind<-as.factor(dat.district$sex_ind)
                        dat.district$Offered<-as.factor(dat.district$Offered)

                        # table(dat.district$cluster, dat.district$sex_ind)
                        # table(dat.district$cluster, dat.district$Offered)

                        design_offer<-svydesign(ids=~cluster, data=dat.district)
                        chi.offer.multi[match(u,district_labels),]<-as.numeric(svychisq(~Offered + sex_ind, design_offer)[3])

                }

                chi.offer.multi[,1]<-district_labels

                chi.offer.multi<-as.data.frame(chi.offer.multi)

                colnames(chi.offer.multi)<-c("IU","pvalue")

                chi.offer.multi$pvalue<-as.numeric(as.character(chi.offer.multi$pvalue))
        }

        #Calculation for Swallowed

        chi.swallow.multi<-matrix(nrow=length(district_labels), ncol=2)

        for (u in district_labels){

                dat.district<-as.data.frame(subset(dat2, dat2$district==u))

                dat.district$sex_ind<-as.factor(dat.district$sex_ind)
                dat.district$Swallowed<-as.factor(dat.district$Swallowed)

                # table(dat.district$cluster, dat.district$sex_ind)
                # table(dat.district$cluster, dat.district$Swallowed)

                design_swallow<-svydesign(ids=~cluster, data=dat.district)
                chi.swallow.multi[match(u,district_labels),]<-as.numeric(svychisq(~Swallowed + sex_ind, design_swallow)[3])

        }

        chi.swallow.multi[,1]<-district_labels

        chi.swallow.multi<-as.data.frame(chi.swallow.multi)

        colnames(chi.swallow.multi)<-c("IU","pvalue")

        chi.swallow.multi$pvalue<-as.numeric(as.character(chi.swallow.multi$pvalue))

        #Creating a new dataframe in order to display the 'Coverage by Sex' table

        if (exists("offer", where=dat2)){
                cover_sex<-data.frame(district_labels, paste(reach_pfem, "%"), paste(treat_pfem, "%"), paste(reach_pmale,"%"), paste(treat_pmale,"%"))

        }else{
                cover_sex<-data.frame(district_labels, paste(treat_pfem, "%"), paste(treat_pmale,"%"))
        }

        #Fill in cell with 'yes' if significant, otherwise, 'no'

        if (exists("offer", where=dat2)){
                cover_sex$compr_male<-ifelse(chi.offer.multi$pvalue>=.05 ,"No", "Yes")
        }

        cover_sex$compt_male<-ifelse(chi.swallow.multi$pvalue>=.05 ,"No", "Yes")

        #Rearranging columns for aesthetics

        if (exists("offer", where=dat2)){

                cover_sex<-cover_sex[,c(1,2,4,6,3,5,7)]

                colnames(cover_sex) <- c("IU", "% Reached amongst females interviewed (offered drug)",
                                         "% Reached amongst males interviewed (offered drug)",
                                         "Programme Reach for females statistically different from males",
                                         "% Treated amongst females reached (swallowed drug)",
                                         "% Treated amongst males reached (swallowed drug)",
                                         "Treatment Coverage for females statistically different from males")

                covsexFT<-FlexTable(cover_sex, body.par.props = parProperties(padding= 1,
                                                                              text.align = "center"),
                                    header.par.props = parProperties(padding = 3,
                                                                     text.align = "center"),
                                    header.text.props = textProperties(font.weight = "bold",
                                                                       font.family = "Calibri"),
                                    body.text.props = textProperties(font.family = "Calibri"))

                vars2<-c("Programme Reach for females statistically different from males",
                         "Treatment Coverage for females statistically different from males")

                #Filling in the yes/no cell with color based on output

                for (j in vars2) {
                        covsexFT[cover_sex[, j]== "Yes" & reach_pfem>reach_pmale, j] = cellProperties( background.color = "#428EFC")
                        covsexFT[cover_sex[, j]== "Yes" & reach_pfem<reach_pmale, j] = cellProperties( background.color = "#FF9F33")
                        covsexFT[cover_sex[, j]== "Yes" & treat_pfem>treat_pmale, j] = cellProperties( background.color = "#428EFC")
                        covsexFT[cover_sex[, j]== "Yes" & treat_pfem<treat_pmale, j] = cellProperties( background.color = "#FF9F33")
                        covsexFT[cover_sex[, j]== "No", j] = cellProperties( background.color = "#D9DADB")

                        #This was the origial code. Leaving it in just in case the new code for some reason does not work in the future

                        # covsexFT[cover_sex[, j]== "Yes" & cover_sex[, 2]>cover_sex[, 3], j] = cellProperties( background.color = "#94C8F1")
                        # covsexFT[cover_sex[, j]== "Yes" & cover_sex[, 2]<cover_sex[, 3], j] = cellProperties( background.color = "#94F1A0")
                        # covsexFT[cover_sex[, j]== "Yes" & cover_sex[, 5]>cover_sex[, 6], j] = cellProperties( background.color = "#94C8F1")
                        # covsexFT[cover_sex[, j]== "Yes" & cover_sex[, 5]<cover_sex[, 6], j] = cellProperties( background.color = "#94F1A0")
                        # covsexFT[cover_sex[, j]== "No", j] = cellProperties( background.color = "#D9DADB")
                }

                covsexFT

        } else {

                colnames(cover_sex) <- c("IU",
                                         "% Treated amongst females reached (swallowed drug)",
                                         "% Treated amongst males reached (swallowed drug)",
                                         "Treatment Coverage for females statistically different from males")

                covsexFT<-FlexTable(cover_sex, body.par.props = parProperties(padding= 1,
                                                                              text.align = "center"),
                                    header.par.props = parProperties(padding = 3,
                                                                     text.align = "center"),
                                    header.text.props = textProperties(font.weight = "bold",
                                                                       font.family = "Calibri"),
                                    body.text.props = textProperties(font.family = "Calibri"))

                vars2<-c("Treatment Coverage for females statistically different from males")

                #Filling in the yes/no cell with color based on output

                for (j in vars2) {
                        covsexFT[cover_sex[, j]== "Yes" & treat_pfem>treat_pmale, j] = cellProperties( background.color = "#428EFC")
                        covsexFT[cover_sex[, j]== "Yes" & treat_pfem<treat_pmale, j] = cellProperties( background.color = "#FF9F33")
                        covsexFT[cover_sex[, j]== "No", j] = cellProperties( background.color = "#D9DADB")
                }

                covsexFT
        }
        #-----------------------------------------------------------------------#
        #Creating grouped bar plots for reach and treament for men and women    #
        #-----------------------------------------------------------------------#

        if (exists("offer", where=dat2)){

                datreach<-data.frame(district_labels, reach_pmale, reach_pfem)

                meltreach<-melt(datreach, id='district_labels')

        }

        dattreat<-data.frame(district_labels, treat_pmale, treat_pfem)

        melttreat<-melt(dattreat, id='district_labels')

        #plot

        if (exists("offer", where=dat2)){

                reachplot<-ggplot(meltreach, aes(district_labels, value, color=variable,
                                                 fill=factor(variable,
                                                             labels=c("Male",
                                                                      "Female")),
                                                 width=.5))+
                        geom_bar(position = position_dodge(.5),
                                 stat="identity",colour="#000000", size=0)+
                        ggtitle("Programme Reach by Sex")+
                        ylab("Programme Reach (%)")+
                        xlab("IU")+
                        theme(
                                legend.position="bottom",
                                legend.title = element_blank(),
                                panel.grid.major.x=element_blank(),
                                panel.grid.minor.x=element_blank(),
                                plot.title = element_text(hjust = 0.5,
                                                          size=10, face="bold"),
                                axis.title.y = element_text(face="bold",
                                                            colour='black', size=10),
                                axis.title.x = element_text(face="bold",
                                                            colour='black', size=10))+
                        scale_y_continuous(breaks = seq(0, 100, 10))+
                        coord_cartesian(ylim=c(0, 100))+
                        scale_x_continuous(breaks = district_labels)+
                        scale_fill_manual(values = colors)

                reachplot

        }

        #plot

        treatplot<-ggplot(melttreat, aes(district_labels, value, color=variable,
                                         fill=factor(variable,
                                                     labels=c("Male",
                                                              "Female")),
                                         width=.5))+
                geom_bar(position = position_dodge(.5),
                         stat="identity",colour="#000000", size=0)+
                ggtitle("Survey Coverage by Sex")+
                ylab("Survey Coverage (%)")+
                xlab("IU")+
                theme(
                        legend.position="bottom",
                        legend.title = element_blank(),
                        panel.grid.major.x=element_blank(),
                        panel.grid.minor.x=element_blank(),
                        plot.title = element_text(hjust = 0.5,
                                                  size=10, face="bold"),
                        axis.title.y = element_text(face="bold",
                                                    colour='black', size=10),
                        axis.title.x = element_text(face="bold",
                                                    colour='black', size=10))+
                scale_y_continuous(breaks = seq(0, 100, 10))+
                coord_cartesian(ylim=c(0, 100))+
                scale_x_continuous(breaks = district_labels)+
                scale_fill_manual(values = colors)

        treatplot

        #----------------------------------#
        # Creating plot of Coverage by Age #
        #----------------------------------#

        if (exists("age", where=dat2) & length(dat2$age)!=0){

                dat2$swallow_ind<-0

                dat2$swallow_ind<-ifelse(dat2$swallow==1 | dat2$swallow=='YES' | dat2$swallow=="Yes" |
                                                 dat2$swallow=="yes" | dat2$swallow=="Y" | dat2$swallow=="y",
                                         dat2$swallow_ind+1, 0)

                dat2$swallow<-dat2$swallow_ind

                if (disease=="Trachoma"){

                        agedf<-data.frame(matrix(nrow = 9, ncol = 2))

                        colnames(agedf)<-c("Ageint", "Ageswa")

                        agedf$Ageint<-0

                        agedf$Ageswa<-0

                        #Summing the total interviewed in each age group

                        agedf[1,1]<-sum(ifelse(0<=dat2$age & dat2$age<=4 , agedf[1,1]+1, agedf[1,1]+0))

                        agedf[2,1]<-sum(ifelse(5<=dat2$age & dat2$age<=9, agedf[2,1]+1, agedf[2,1]+0))

                        agedf[3,1]<-sum(ifelse(10<=dat2$age & dat2$age<=14, agedf[3,1]+1, agedf[3,1]+0))

                        agedf[4,1]<-sum(ifelse(15<=dat2$age & dat2$age<=19, agedf[4,1]+1, agedf[4,1]+0))

                        agedf[5,1]<-sum(ifelse(20<=dat2$age & dat2$age<=29, agedf[5,1]+1, agedf[5,1]+0))

                        agedf[6,1]<-sum(ifelse(30<=dat2$age & dat2$age<=39, agedf[6,1]+1, agedf[6,1]+0))

                        agedf[7,1]<-sum(ifelse(40<=dat2$age & dat2$age<=49, agedf[7,1]+1, agedf[7,1]+0))

                        agedf[8,1]<-sum(ifelse(50<=dat2$age & dat2$age<=59, agedf[8,1]+1, agedf[8,1]+0))

                        agedf[9,1]<-sum(ifelse(dat2$age>=60, agedf[9,1]+1, agedf[9,1]+0))

                        #Summing the total swallowed in each age group

                        agedf[1,2]<-sum(ifelse(0<=dat2$age & dat2$age<=4 & dat2$swallow==1,
                                               agedf[1,2]+1, agedf[1,2]+0))

                        agedf[2,2]<-sum(ifelse(5<=dat2$age & dat2$age<=9 & dat2$swallow==1,
                                               agedf[2,2]+1, agedf[2,2]+0))

                        agedf[3,2]<-sum(ifelse(10<=dat2$age & dat2$age<=14 & dat2$swallow==1,
                                               agedf[3,2]+1, agedf[3,2]+0))

                        agedf[4,2]<-sum(ifelse(15<=dat2$age & dat2$age<=19 & dat2$swallow==1,
                                               agedf[4,2]+1, agedf[4,2]+0))

                        agedf[5,2]<-sum(ifelse(20<=dat2$age & dat2$age<=29 & dat2$swallow==1,
                                               agedf[5,2]+1, agedf[5,2]+0))

                        agedf[6,2]<-sum(ifelse(30<=dat2$age & dat2$age<=39 & dat2$swallow==1,
                                               agedf[6,2]+1, agedf[6,2]+0))

                        agedf[7,2]<-sum(ifelse(40<=dat2$age & dat2$age<=49 & dat2$swallow==1,
                                               agedf[7,2]+1, agedf[7,2]+0))

                        agedf[8,2]<-sum(ifelse(50<=dat2$age & dat2$age<=59 & dat2$swallow==1,
                                               agedf[8,2]+1, agedf[8,2]+0))

                        agedf[9,2]<-sum(ifelse(dat2$age>=60 & dat2$swallow==1,
                                               agedf[9,2]+1, agedf[9,2]+0))

                        #Calculating the coverage in each age group

                        agedf$Agecov<-0

                        agedf$Agecov<-100*(agedf$Ageswa/agedf$Ageint)

                        #Calculating the upper and lower CI for coverage by age

                        agedf$upci_age<-agedf$Agecov+(1.96*sqrt(agedf$Agecov*(100-agedf$Agecov)/agedf$Ageint))
                        agedf$lowci_age<-agedf$Agecov-(1.96*sqrt(agedf$Agecov*(100-agedf$Agecov)/agedf$Ageint))

                        #Setting the age coverage to zero if there was no one in
                        #that age group

                        for (o in 1:nrow(agedf)){

                                if(is.na(agedf$Agecov[o])){

                                        agedf[o,]<-0

                                }

                        }

                        #Determining the scale of the y-axis

                        maxci_age<-range(agedf$upci_age,na.rm = T)
                        yrange2<-max(maxci_age)

                        ifelse(yrange2>=90, upval_age<-100, ifelse(yrange2>=80, upval_age<-90,
                                                                   ifelse(yrange2>=70, upval_age<-80,
                                                                          ifelse(yrange2>=60, upval_age<-70,
                                                                                 ifelse(yrange2>=50, upval_age<-60,
                                                                                        ifelse(yrange2>=40, upval_age<-50,
                                                                                               ifelse(yrange2>=30, upval_age<-40,
                                                                                                      ifelse(yrange2>=20, upval_age<-30,
                                                                                                             ifelse(yrange2>=10, upval_age<-20,
                                                                                                                    ifelse(yrange2>=0, upval_age<-10)
                                                                                                             )
                                                                                                      )
                                                                                               )
                                                                                        )
                                                                                 )
                                                                          )
                                                                   )
                        )
                        )

                        #plot

                        ageplot<-ggplot(agedf, aes(1:9, agedf$Agecov, width=.5))+
                                geom_bar(stat="identity",colour="#5A87B2",fill="#5A87B2")+
                                ggtitle("Coverage by Age")+
                                ylab(paste("Survey Coverage (%) for", drug))+
                                xlab("Age (years)")+
                                theme(
                                        legend.position="bottom",
                                        legend.title = element_blank(),
                                        panel.grid.major.x=element_blank(),
                                        panel.grid.minor.x=element_blank(),
                                        plot.title = element_text(hjust = 0.5,
                                                                  size=10, face="bold"),
                                        axis.title.y = element_text(face="bold",
                                                                    colour='black', size=10),
                                        axis.title.x = element_text(face="bold",
                                                                    colour='black', size=10))+
                                scale_y_continuous(breaks = seq(0, upval_age, 10))+
                                coord_cartesian(ylim=c(0,upval_age))+
                                scale_x_continuous(breaks = seq(1, 9, 1),labels=c("0-4", "5-9", "10-14",
                                                                                  "15-19", "20-29", "30-39",
                                                                                  "40-49","50-59", "60 & up"))
                        print(ageplot)

                } else {

                        agedf<-data.frame(matrix(nrow = 9, ncol = 2))

                        colnames(agedf)<-c("Ageint", "Ageswa")

                        agedf$Ageint<-0

                        agedf$Ageswa<-0

                        #Summing the total interviewed in each age group

                        agedf[1,1]<-sum(ifelse(2<=dat2$age & dat2$age<=4 , agedf[1,1]+1, agedf[1,1]+0))

                        agedf[2,1]<-sum(ifelse(5<=dat2$age & dat2$age<=9, agedf[2,1]+1, agedf[2,1]+0))

                        agedf[3,1]<-sum(ifelse(10<=dat2$age & dat2$age<=14, agedf[3,1]+1, agedf[3,1]+0))

                        agedf[4,1]<-sum(ifelse(15<=dat2$age & dat2$age<=19, agedf[4,1]+1, agedf[4,1]+0))

                        agedf[5,1]<-sum(ifelse(20<=dat2$age & dat2$age<=29, agedf[5,1]+1, agedf[5,1]+0))

                        agedf[6,1]<-sum(ifelse(30<=dat2$age & dat2$age<=39, agedf[6,1]+1, agedf[6,1]+0))

                        agedf[7,1]<-sum(ifelse(40<=dat2$age & dat2$age<=49, agedf[7,1]+1, agedf[7,1]+0))

                        agedf[8,1]<-sum(ifelse(50<=dat2$age & dat2$age<=59, agedf[8,1]+1, agedf[8,1]+0))

                        agedf[9,1]<-sum(ifelse(dat2$age>=60, agedf[9,1]+1, agedf[9,1]+0))

                        #Summing the total swallowed in each age group

                        agedf[1,2]<-sum(ifelse(2<=dat2$age & dat2$age<=4 & dat2$swallow==1,
                                               agedf[1,2]+1, agedf[1,2]+0))

                        agedf[2,2]<-sum(ifelse(5<=dat2$age & dat2$age<=9 & dat2$swallow==1,
                                               agedf[2,2]+1, agedf[2,2]+0))

                        agedf[3,2]<-sum(ifelse(10<=dat2$age & dat2$age<=14 & dat2$swallow==1,
                                               agedf[3,2]+1, agedf[3,2]+0))

                        agedf[4,2]<-sum(ifelse(15<=dat2$age & dat2$age<=19 & dat2$swallow==1,
                                               agedf[4,2]+1, agedf[4,2]+0))

                        agedf[5,2]<-sum(ifelse(20<=dat2$age & dat2$age<=29 & dat2$swallow==1,
                                               agedf[5,2]+1, agedf[5,2]+0))

                        agedf[6,2]<-sum(ifelse(30<=dat2$age & dat2$age<=39 & dat2$swallow==1,
                                               agedf[6,2]+1, agedf[6,2]+0))

                        agedf[7,2]<-sum(ifelse(40<=dat2$age & dat2$age<=49 & dat2$swallow==1,
                                               agedf[7,2]+1, agedf[7,2]+0))

                        agedf[8,2]<-sum(ifelse(50<=dat2$age & dat2$age<=59 & dat2$swallow==1,
                                               agedf[8,2]+1, agedf[8,2]+0))

                        agedf[9,2]<-sum(ifelse(dat2$age>=60 & dat2$swallow==1,
                                               agedf[9,2]+1, agedf[9,2]+0))

                        #Calculating the coverage in each age group

                        agedf$Agecov<-0

                        agedf$Agecov<-100*(agedf$Ageswa/agedf$Ageint)

                        #Calculating the upper and lower CI for coverage by age

                        agedf$upci_age<-agedf$Agecov+(1.96*sqrt(agedf$Agecov*(100-agedf$Agecov)/agedf$Ageint))
                        agedf$lowci_age<-agedf$Agecov-(1.96*sqrt(agedf$Agecov*(100-agedf$Agecov)/agedf$Ageint))

                        #Setting the age coverage to zero if there was no one in
                        #that age group

                        for (o in 1:nrow(agedf)){

                                if(is.na(agedf$Agecov[o])){

                                        agedf[o,]<-0

                                }

                        }

                        #Determining the scale of the y-axis

                        maxci_age<-range(agedf$upci_age,na.rm = T)
                        yrange2<-max(maxci_age)

                        ifelse(yrange2>=90, upval_age<-100, ifelse(yrange2>=80, upval_age<-90,
                                                                   ifelse(yrange2>=70, upval_age<-80,
                                                                          ifelse(yrange2>=60, upval_age<-70,
                                                                                 ifelse(yrange2>=50, upval_age<-60,
                                                                                        ifelse(yrange2>=40, upval_age<-50,
                                                                                               ifelse(yrange2>=30, upval_age<-40,
                                                                                                      ifelse(yrange2>=20, upval_age<-30,
                                                                                                             ifelse(yrange2>=10, upval_age<-20,
                                                                                                                    ifelse(yrange2>=0, upval_age<-10)
                                                                                                             )
                                                                                                      )
                                                                                               )
                                                                                        )
                                                                                 )
                                                                          )
                                                                   )
                        )
                        )

                        #plot

                        ageplot<-ggplot(agedf, aes(1:9, agedf$Agecov, width=.5))+
                                geom_bar(stat="identity",colour="#5A87B2",fill="#5A87B2")+
                                ggtitle("Coverage by Age")+
                                ylab(paste("Survey Coverage (%) for", drug))+
                                xlab("Age (years)")+
                                theme(
                                        legend.position="bottom",
                                        legend.title = element_blank(),
                                        panel.grid.major.x=element_blank(),
                                        panel.grid.minor.x=element_blank(),
                                        plot.title = element_text(hjust = 0.5,
                                                                  size=10, face="bold"),
                                        axis.title.y = element_text(face="bold",
                                                                    colour='black', size=10),
                                        axis.title.x = element_text(face="bold",
                                                                    colour='black', size=10))+
                                scale_y_continuous(breaks = seq(0, upval_age, 10))+
                                coord_cartesian(ylim=c(0,upval_age))+
                                scale_x_continuous(breaks = seq(1, 9, 1),labels=c("0-4", "5-9", "10-14",
                                                                                  "15-19", "20-29", "30-39",
                                                                                  "40-49","50-59", "60 & up"))
                        print(ageplot)

                }
        }


        #-----------------------------------------------------------------------#
        #Creating grouped bar plot for coverage by school attendance            #
        #-----------------------------------------------------------------------#

        if (exists("school", where=dat2)){

                datattend<-data.frame(district_labels, attendno, attendyes)

                meltattend<-melt(datattend, id='district_labels')

                #plot

                attendplot<-ggplot(meltattend, aes(district_labels, value, color=variable,
                                                   fill=factor(variable,
                                                               labels=c("Non-school attending coverage",
                                                                        "School attending coverage")),
                                                   width=.5))+
                        geom_bar(position = position_dodge(.5),
                                 stat="identity",colour="#000000", size=0)+
                        ggtitle("Survey Coverage by School Attendance")+
                        ylab("Survey Coverage (%)")+
                        xlab("IU")+
                        theme(
                                legend.position="bottom",
                                legend.title = element_blank(),
                                panel.grid.major.x=element_blank(),
                                panel.grid.minor.x=element_blank(),
                                plot.title = element_text(hjust = 0.5,
                                                          size=10, face="bold"),
                                axis.title.y = element_text(face="bold",
                                                            colour='black', size=10),
                                axis.title.x = element_text(face="bold",
                                                            colour='black', size=10))+
                        scale_y_continuous(breaks = seq(0, 100, 10))+
                        coord_cartesian(ylim=c(0, 100))+
                        scale_x_continuous(breaks = district_labels)+
                        scale_fill_manual(values = colors)

                attendplot

                #Creating the flex table

                if (exists("offer", where=dat2)){
                        compli2<-round(compliance, 1)

                        datschlcom<-data.frame(district_labels, paste(compli2,"%"),
                                               paste(attendyes, "%"), paste(attendno,"%"))

                        colnames(datschlcom) <- c("IU", "Compliance",
                                                  "School-Attending \n Coverage",
                                                  "Non-School \n Attending Coverage")

                        schlcomFT<-FlexTable(datschlcom, body.par.props = parProperties(padding= 1,
                                                                                        text.align = "center"),
                                             header.par.props = parProperties(padding = 3,
                                                                              text.align = "center"),
                                             header.text.props = textProperties(font.weight = "bold",
                                                                                font.family = "Calibri"),
                                             body.text.props = textProperties(font.family = "Calibri"))
                } else {
                        datschlcom<-data.frame(district_labels, paste(attendyes, "%"),
                                               paste(attendno,"%"))

                        colnames(datschlcom) <- c("IU",
                                                  "School-Attending \n Coverage",
                                                  "Non-School \n Attending Coverage")

                        schlcomFT<-FlexTable(datschlcom, body.par.props = parProperties(padding= 1,
                                                                                        text.align = "center"),
                                             header.par.props = parProperties(padding = 3,
                                                                              text.align = "center"),
                                             header.text.props = textProperties(font.weight = "bold",
                                                                                font.family = "Calibri"),
                                             body.text.props = textProperties(font.family = "Calibri"))
                }

                schlcomFT

        }

        #####################################################
        #       Creating non-compliance with 95% CI table   #
        #####################################################

        if (exists("offer", where=dat2)){
                compli3<-round(no_compliance, 1)

                datnocompli_table<-data.frame(district_labels, paste(compli3,"%"),
                                              paste(lowci_nc, "%"), paste(upci_nc,"%"))

                colnames(datnocompli_table) <- c("IU", "Non-Compliance",
                                                 "Lower 95% \n Confidence Interval",
                                                 "Upper 95% \n Confidence Interval")

                datnocompli_FT<-FlexTable(datnocompli_table, body.par.props = parProperties(padding= 1,
                                                                                            text.align = "center"),
                                          header.par.props = parProperties(padding = 3,
                                                                           text.align = "center"),
                                          header.text.props = textProperties(font.weight = "bold",
                                                                             font.family = "Calibri"),
                                          body.text.props = textProperties(font.family = "Calibri"))
        }

        #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~#
        #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~#
        #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~#
        #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~#
        #~*~*~*~*~*~*~*~*~*~*~*~*~*          REPORT         *~*~*~*~*~*~*~*~*~*~*~*~*~*~#
        #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~#
        #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~#
        #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~#
        #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~#

        #dev.off()

        #^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^#
        # Creating variables that will be displayed in the report   #
        #^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^#

        #Data Acurracy page variables

        #Coverage page variables

        lowcov<-0

        lowcov<-ifelse((100*lowci)<thresh,lowcov+1, lowcov+0)

        lowcov<-sum(lowcov)

        hicov<-0

        hicov<-ifelse((100*lowci)>=thresh, hicov+1, hicov+0)

        hicov<-sum(hicov)

        #Determining the districts that require coverage improvement

        improve<-data.frame(district_labels, lowci*100)

        colnames(improve)<-c("District", "lowci")

        improve<-data.frame(ifelse(improve$lowci<thresh, print(improve$IU), NA))
        improve<-na.omit(improve)
        colnames(improve)<-"IU"

        improve<-unlist(improve)

        ifelse(is_empty(improve), improve<-"No IU meets this criteria", improve)

        improve<-paste(improve)

        #^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^#
        # Creating the format for the word document output, and generating the report   #
        #^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^#

        #This is a template to change fonts

        title.font <- textProperties(color="#000000",font.size = 18,
                                     font.weight = 'bold', font.family = 'Calibri' )

        doc<-docx()

        #-----------#
        #Title page #
        #-----------#

        titlepg<-pot(paste(disease, "Coverage Evaluation Results Summary:", country, ",", format(Sys.time(), '%B %Y')),
                     textProperties(font.weight = "bold", font.family = "Calibri",
                                    font.size = 20))
        titlepg

        doc<-addParagraph(doc, titlepg)
        doc <- addParagraph( doc, '', stylename = 'Normal' )

        sumsent<- pot("This summary reviews the results from coverage evaluation surveys for ",
                      textProperties(font.family = "Calibri"))+
                pot(drug, textProperties(font.weight = "bold", font.family = "Calibri"))+
                pot(" that were conducted in ",textProperties(font.family = "Calibri"))+
                pot(country, textProperties(font.weight = "bold", font.family = "Calibri"))+
                pot(" in ",textProperties(font.family = "Calibri"))+
                pot(format(Sys.time(), "%Y"), textProperties(font.weight = "bold", font.family = "Calibri"))+
                pot(".",textProperties(font.family = "Calibri"))

        doc<-addParagraph(doc, sumsent)
        doc <- addParagraph( doc, '', stylename = 'Normal' )

        kt<-pot("Key Terms", textProperties(underlined = TRUE, font.family = "Calibri"))

        doc<-addParagraph(doc, kt)
        doc <- addParagraph( doc, '', stylename = 'Normal' )


        rc<-pot("Reported Coverage-", textProperties(font.weight = "bold", font.family = "Calibri"))+
                pot(" The coverage calculated from data reported by all drug distributors, with census figures or drug distributor reports used to estimate the population denominator.",
                    textProperties(font.family = "Calibri"))

        doc <- addParagraph( doc, rc)
        doc <- addParagraph( doc, '', stylename = 'Normal' )

        sc<-pot("Survey Coverage-", textProperties(font.weight = "bold", font.family = "Calibri"))+
                pot(" Coverage estimated through the use of population-based survey sampling methods. The denominator is the total number of individuals Survey and the numerator is the total number of individuals Survey who were identified as having ingested the drug.",
                    textProperties(font.family = "Calibri"))

        doc <- addParagraph( doc, sc)
        doc <- addParagraph( doc, '', stylename = 'Normal' )

        pr<-pot("Programme Reach-", textProperties(font.weight = "bold", font.family = "Calibri"))+
                pot(" The proportion of people in the survey area who were given the opportunity to receive the preventive chemotherapy, regardless of whether the drug was ingested.",
                    textProperties(font.family = "Calibri"))

        doc <- addParagraph( doc, pr)
        doc <- addParagraph( doc, '', stylename = 'Normal' )

        com<-pot("Compliance-", textProperties(font.weight = "bold", font.family = "Calibri"))+
                pot(" The proportion of people in the survey area who are offered the drug that also swallow the drug.",
                    textProperties(font.family = "Calibri"))

        doc<-addParagraph(doc, com)
        doc <- addParagraph( doc, '', stylename = 'Normal' )

        iudef<-pot("Implementation Unit (IU)-", textProperties(font.weight = "bold", font.family = "Calibri"))+
                pot(" Designated survey areas. Coverage surveys are typically conducted at the district level; however, in some cases they may be done at the province, county, or zonal level.",
                    textProperties(font.family = "Calibri"))

        doc<-addParagraph(doc, iudef)

        doc<- addPageBreak(doc)


        #----------------------------------------------#
        #Inserting Reported vs. Surveyed Coverage plot #
        #----------------------------------------------#

        Sys.sleep(1)

        #Creation of the 'District Selection' editable table

        dissel<-data.frame(matrix(nrow = 1, ncol = 1))

        colnames(dissel)<-"District Selection"

        dissel[1:1]<-paste("REPLACE WITH YOUR TEXT
Insert text here explaining how and why these IUs were selected for surveys")

        dissel<-FlexTable(dissel,
                          header.par.props = parProperties(padding = 3, text.align = "center"),
                          header.cell.props = cellProperties(background.color = "#B4D3F1"),
                          header.text.props = textProperties(font.family = "Calibri"))

        dissel[1:1]<- cellProperties( background.color = "#D4D6D8", padding.left = 10,
                                      padding.right = 10)

        dissel[1:1]<- textProperties(font.family = "Calibri", font.style = "italic")

        dissel[1:1]<- parProperties(text.align = "center")

        doc<-addFlexTable(doc, dissel, align="center")
        doc<- addParagraph( doc, '', stylename = 'Normal')

        doc<-addPlot(doc, fun=print, x=rvsplot)
        doc<- addPageBreak(doc)

        #--------------------------------------------------------------------#
        # Inserting Survey coverage results by district  table               #
        #--------------------------------------------------------------------#

        table1<-pot(paste("Table 1. Survey coverage results by IU in",
                          country, ",",format(Sys.time(), "%Y"),"."),
                    textProperties(font.family = "Calibri",
                                   font.weight = "bold"))
        doc<-addParagraph(doc, table1)

        doc<-addFlexTable(doc, surcovFT)

        doc<- addPageBreak(doc)

        #-------------------#
        #Data Accuracy Page #
        #-------------------#

        compWHO<-pot("Comparison with WHO Target Threshold of ", textProperties(font.size = 18,
                                                                                font.family = "Calibri",
                                                                                font.weight = "bold"))+
                pot(thresh,textProperties(font.size = 18, font.family = "Calibri",
                                          font.weight = "bold"))+
                pot("%",textProperties(font.size = 18, font.family = "Calibri",
                                       font.weight = "bold"))

        doc<-addParagraph(doc, compWHO)

        WHOtext<-pot("If the survey coverage falls below the target coverage threshold, it is evidence that the MDA is considered “LOW” and in need of improvement. If the survey coverage is well above the target coverage threshold it is evidence that the MDA was successful and the programme is functioning well.",
                     textProperties(font.family = "Calibri", font.style = "italic"))
        doc<-addParagraph(doc, WHOtext)

        WHOastrk<-pot(paste("*Survey coverage is considered ‘good’ when the lower confidence limit exceeds ",
                            thresh,"%", " (Note that in some cases the point estimate of the survey coverage may exceed",
                            thresh, "%", " but the lower limit may not).  Survey coverage is considered ‘low’ if the lower confidence limit falls below ",
                            thresh,"%."), textProperties(font.family = "Calibri", font.style = "italic"))

        WHOastrk

        #Displaying numbers and percentage for good and poor coverage

        doc<- addParagraph( doc, '', stylename = 'Normal')

        highfrac<-pot(paste(round((100*hicov/numdist),0), "% "),
                      textProperties(font.size = 14, font.family = "Calibri"))+
                pot(" ")+pot(" ")+pot(" ")+
                pot(paste(hicov), textProperties(font.weight = "bold",font.family = "Calibri"))+
                pot(" of ", textProperties(font.family = "Calibri"))+
                pot(numdist, textProperties(font.weight = "bold",font.family = "Calibri"))+
                pot(" IUs had GOOD survey coverage (≥",textProperties(font.family = "Calibri"))+
                pot(thresh, textProperties(font.family = "Calibri"))+
                pot("%)* ", textProperties(font.family = "Calibri"))

        highfrac

        doc<-addParagraph(doc, highfrac)
        doc<- addParagraph( doc, '', stylename = 'Normal')

        lowfrac<-pot(paste(round((100*lowcov/numdist),0), "% "),
                     textProperties(font.size = 14, font.family = "Calibri"))+
                pot(" ")+pot(" ")+pot(" ")+
                pot(paste(lowcov), textProperties(font.weight = "bold",font.family = "Calibri"))+
                pot(" of ", textProperties(font.family = "Calibri"))+
                pot(numdist, textProperties(font.weight = "bold",font.family = "Calibri"))+
                pot(" IUs had LOW survey coverage (<",textProperties(font.family = "Calibri"))+
                pot(thresh, textProperties(font.family = "Calibri"))+
                pot("%) ", textProperties(font.family = "Calibri"))

        lowfrac

        doc<-addParagraph(doc, lowfrac)
        doc<- addParagraph( doc, '', stylename = 'Normal' )

        doc<-addParagraph(doc, WHOastrk)

        doc<- addPageBreak(doc)

        #--------------------------------------------#
        #Inserting Comparative Coverage by Sex table #
        #--------------------------------------------#

        title4<-pot(value = "SURVEY COVERAGE BY SEX", format=title.font)

        doc<-addParagraph(doc, title4)
        doc<- addParagraph( doc, '', stylename = 'Normal' )

        if (exists("offer", where=dat2)){

                table3<-pot(paste("Table 2. Programme reach and survey coverage by sex,",
                                  country, ",", format(Sys.time(),"%Y"), "."),
                            textProperties(font.family = "Calibri",
                                           font.weight = "bold"))
        } else {
                table3<-pot(paste("Table 2. Survey coverage by sex,",
                                  country, ",", format(Sys.time(),"%Y"), "."),
                            textProperties(font.family = "Calibri",
                                           font.weight = "bold"))
        }

        doc<-addParagraph(doc, table3)

        doc<-addFlexTable(doc, covsexFT)

        blue<-pot("BLUE", textProperties(color = "#428EFC", font.weight = "bold", font.family = "Calibri"))+
                pot(" indicates females had a significantly higher percentage")

        doc<-addParagraph(doc, blue)

        green<-pot("ORANGE", textProperties(color = "#FF9F33", font.weight = "bold", font.family = "Calibri"))+
                pot(" indicates males had a significantly higher percentage")

        doc<-addParagraph(doc, green)

        doc<- addPageBreak(doc)

        #-----------------------------------------------#
        # Inserting Reach and Coverage by Sex plots     #
        #-----------------------------------------------#

        if (exists("offer", where=dat2)){

                doc<- addPlot(doc, fun=print, x=reachplot)
                doc<- addParagraph( doc, '', stylename = 'Normal' )

        }

        doc<-addPlot(doc, fun=print, x=treatplot)
        doc<- addParagraph( doc, '', stylename = 'Normal' )

        doc<- addPageBreak(doc)

        #--------------------------------#
        # Inserting Coverage by Age plot #
        #--------------------------------#

        if (exists("age", where=dat2) & length(dat2$age)!=0){

                title5<-pot(value = "SURVEY COVERAGE BY AGE*", format=title.font)

                doc<-addParagraph(doc, title5)

                doc<- addParagraph( doc, '', stylename = 'Normal' )

                doc<-addPlot(doc, fun=print, x=ageplot)

                ageastrk1<-pot("*Survey coverage is for all IUs combined.  Note that this figure does not take into consideration the age groups that are ineligible for treatment (e.g., children <5 years when ivermectin is distributed) and thus will be reflected in this figure by low coverage among the age group(s) ineligible for the MDA.",
                               textProperties(font.family = "Calibri", font.style = "italic"))

                ageastrk1

                doc<-addParagraph(doc, ageastrk1)
                doc<- addPageBreak(doc)
        }

        #------------------------------#
        #Inserting Non-Compliance Plot #
        #------------------------------#

        if (exists("offer", where=dat2)){

                title6<-pot(value = "NON-COMPLIANCE", format=title.font)

                doc<-addParagraph(doc, title6)

                doc<- addParagraph( doc, '', stylename = 'Normal' )

                doc<-addPlot(doc, fun=print, x=n_compplot)

                doc<- addPageBreak(doc)

        }

        #-------------------------------#
        #Inserting Non-Compliance table #
        #-------------------------------#

        if (exists("offer", where=dat2)){

                doc<-addParagraph(doc, title6)

                table4b<-pot(paste("Table 3. Non-Compliance in ",
                                   country, ",", format(Sys.time(),"%Y"), "."),
                             textProperties(font.family = "Calibri",
                                            font.weight = "bold"))
                table4b

                doc<-addParagraph(doc, table4b)

                doc<-addFlexTable(doc, datnocompli_FT)

                doc<- addPageBreak(doc)

        }


        #----------------------------------------------------#
        # Survey Coverage by School Attendance plot page     #
        #----------------------------------------------------#

        if (exists("school", where=dat2)){

                title8<-pot("SURVEY COVERAGE BY SCHOOL ATTENDANCE", format=title.font)

                doc<-addParagraph(doc, title8)
                doc<- addParagraph( doc, '', stylename = 'Normal' )

                doc<-addPlot(doc, fun=print, x=attendplot)
                doc<- addPageBreak(doc)
        }

        #----------------------------------------------------#
        # Survey Coverage by School Attendance table page    #
        #----------------------------------------------------#
        if (exists("school", where=dat2)){

                if (exists("offer", where=dat2)){
                        table4<-pot(paste("Table 4. Results for compliance and survey coverage by school attendance, by IU in",
                                          country, ",", format(Sys.time(), "%Y"), "."),
                                    textProperties(font.family = "Calibri",
                                                   font.weight = "bold"))
                } else{
                        table4<-pot(paste("Table 3. Results for survey coverage by school attendance, by IU in",
                                          country, ",", format(Sys.time(), "%Y"), "."),
                                    textProperties(font.family = "Calibri",
                                                   font.weight = "bold"))
                }

                doc<-addParagraph(doc, table4)

                doc<-addFlexTable(doc, schlcomFT)
                doc<- addPageBreak(doc)
        }

        #--------------#
        #Coverage Page #
        #--------------#

        #Creating Potential Reasons for Poor Coverage table

        potpoor<-data.frame(matrix(nrow = 1, ncol = 1))

        colnames(potpoor)<-"Potential Reasons for Poor Coverage"

        potpoor[1:1]<-paste("REPLACE WITH YOUR TEXT
If one or more of your IUs had POOR coverage, list some potential reasons why this may be. What were the most common reasons given for not being offered the drug? What were the most common reasons for not swallowing the drug when it was offered? Consider the coverage in the different sub-populations to determine whether any particular group is being left out or has lower than average coverage (e.g., males vs. females, school-attendance, particular subdistricts, ethnic minorities, etc.). ")

        potpoor<-FlexTable(potpoor,
                           header.par.props = parProperties(padding = 3, text.align = "center"),
                           header.cell.props = cellProperties(background.color = "#B4D3F1"),
                           header.text.props = textProperties(font.family = "Calibri"))

        potpoor[1:1]<- cellProperties( background.color = "#D4D6D8", padding.left = 10,
                                       padding.right = 10)

        potpoor[1:1]<- textProperties(font.family = "Calibri", font.style = "italic")

        potpoor[1:1]<- parProperties(text.align = "center")

        potpoor

        #Creating Requiring Coverage Improvement table

        #reported coverage is ± 10 percent from point estimate

        ################

        if (exists("offer", where=dat2)){

                #Compliance is less than 90%

                improve3<-data.frame(district_labels, compliance, lowci_c, thresh)

                improve3<-data.frame(ifelse(improve3$lowci_c<90,
                                            print(improve3$district_labels), NA))

                improve3<-na.omit(improve3)

                colnames(improve3)<-"District"

                improve3<-unlist(improve3)

                improve3<-paste(improve3)

                ifelse(is_empty(improve3), improve3<-"No IUs meets this criteria", improve3)

                reqcovdist<-c(paste(c(improve), collapse = ", "),paste(c(improve3), collapse = ", ") )
                reqcovrationale<-c(paste("The survey coverage suggests the true coverage could be below the WHO target threshold of", thresh),
                                   "The compliance estimate suggests the true compliance could be below 90%")

                reqcovDF<-data.frame(reqcovdist, reqcovrationale)

                colnames(reqcovDF)<-c("IU(s)", "Rationale")

                reqcovFT<-FlexTable(reqcovDF, body.par.props = parProperties(padding= 1,
                                                                             text.align = "center"),
                                    header.par.props = parProperties(padding = 3,
                                                                     text.align = "center"),
                                    header.text.props = textProperties(font.weight = "bold",
                                                                       font.family = "Calibri"),
                                    body.text.props = textProperties(font.family = "Calibri"),
                                    header.cell.props = cellProperties(background.color = "#E49CA7"))

                reqcovFT<-addHeaderRow(reqcovFT, "IU(s) where follow-up actions may be needed",
                                       colspan=2, textProperties(font.weight = "bold", font.family = "Calibri"), first = TRUE)

                reqcovFT

        } else {

                reqcovdist<-c(paste(c(improve), collapse = ", "))
                reqcovrationale<-c(paste("The survey coverage suggests the true coverage could be below the WHO target threshold of", thresh))

                reqcovDF<-data.frame(reqcovdist, reqcovrationale)

                colnames(reqcovDF)<-c("IU(s)", "Rationale")

                reqcovFT<-FlexTable(reqcovDF, body.par.props = parProperties(padding= 1,
                                                                             text.align = "center"),
                                    header.par.props = parProperties(padding = 3,
                                                                     text.align = "center"),
                                    header.text.props = textProperties(font.weight = "bold",
                                                                       font.family = "Calibri"),
                                    body.text.props = textProperties(font.family = "Calibri"),
                                    header.cell.props = cellProperties(background.color = "#E49CA7"))

                reqcovFT<-addHeaderRow(reqcovFT, "IU(s) where follow-up actions may be needed",
                                       colspan=2, textProperties(font.weight = "bold", font.family = "Calibri"), first = TRUE)

                reqcovFT
        }

        #Creating the second tip table

        tip2<-data.frame(matrix(nrow = 1, ncol = 1))

        colnames(tip2)<-"Tip"

        tip2[1:1]<-paste("How do trends in the survey IUs change your
                         interpretation of reported coverage in non-survey IUs?


                         ")

        tip2<-FlexTable(tip2,
                        header.par.props = parProperties(padding = 3, text.align = "center"),
                        header.cell.props = cellProperties(background.color = "#B4D3F1"),
                        header.text.props = textProperties(font.family = "Calibri"))

        tip2[1:1]<- cellProperties( background.color = "#D4D6D8", padding.left = 10,
                                    padding.right = 10)

        tip2[1:1]<- textProperties(font.family = "Calibri")

        tip2[1:1]<- parProperties(text.align = "center")

        #Creating Coverage Improvement Recommendations table

        covimp<-data.frame(matrix(nrow = 1, ncol = 1))

        colnames(covimp)<-"Coverage Improvement Recommendations"

        covimp[1:1]<-paste("REPLACE WITH YOUR TEXT
What actions will your programme take to improve coverage in the poor performing IUs? Refer to Appendix A for suggestions.

                           ")

        covimp<-FlexTable(covimp,
                          header.par.props = parProperties(padding = 3, text.align = "center"),
                          header.cell.props = cellProperties(background.color = "#B4D3F1"),
                          header.text.props = textProperties(font.family = "Calibri"))

        covimp[1:1]<- cellProperties( background.color = "#D4D6D8", padding.left = 10,
                                      padding.right = 10)

        covimp[1:1]<- textProperties(font.family = "Calibri", font.style = "italic")

        covimp[1:1]<- parProperties(text.align = "center")

        #Format page

        #Page Title

        if (exists("offer", where=dat2)){

                title7<-pot(value = "ACTIONS TO IMPROVE COVERAGE AND/OR COMPLIANCE", format=title.font)

        }else{
                title7<-pot(value = "ACTIONS TO IMPROVE COVERAGE", format=title.font)
        }

        doc<-addParagraph(doc, title7)
        doc<- addParagraph( doc, '', stylename = 'Normal' )

        doc<-addFlexTable(doc, reqcovFT)
        doc<- addParagraph( doc, '', stylename = 'Normal' )

        doc<-addFlexTable(doc, potpoor)
        doc<- addParagraph( doc, '', stylename = 'Normal' )

        doc<-addFlexTable(doc, covimp)
        doc<- addPageBreak(doc)

        #----------------#
        # Appendix A     #
        #----------------#

        title9<-pot("Appendix A. Interpreting and following up reported and survey coverage results",
                    textProperties(font.weight = "bold", font.family = "Calibri",
                                   font.size = 16))

        doc<-addParagraph(doc, title9)

        apndxpar<-pot("This table is taken from the WHO document,", textProperties(font.family = "Calibri"))+
                pot("“Coverage Evaluation Surveys for Preventive Chemotherapy: Field Guide for Implementation”",
                    textProperties(font.weight = "bold", font.family = "Calibri"))+
                pot(" and is intended to help users in developing their own Action Plan as the result of a coverage survey(s).   This table lists the possible findings that can occur when the survey coverage is compared to both the target coverage threshold and the reported coverage.  The table provides potential causes to investigate and corrective actions that can be taken.",
                    textProperties(font.family = "Calibri"))

        doc<- addParagraph(doc, apndxpar)
        doc<- addParagraph( doc, '', stylename = 'Normal' )

        # hyperlink<-pot("Click here", textProperties(font.weight = "bold", font.family = "Calibri",
        #                                             color = "blue"), hyperlink = "http://www.ntdsupport.org/sites/default/files/uploads/docs/resources/Coverage%20Evaluation%20Guidelines%20Final%20Draft_Nov%202016.pdf")+
        #         pot(" and go to table 3 on page 39 for interpreations and follow-up suggestions",
        #             textProperties(font.family = "Calibri"))
        #
        # doc<- addParagraph( doc, '', stylename = 'Normal')
        # doc<-addParagraph(doc, hyperlink)

        #Table 1

        finding<-c("1. Comparison of survey coverage to target coverage threshold: To get a better estimate of coverage where there is reason to believe routine reporting is incorrect", "Survey coverage is below the target coverage threshold","","Survey coverage is above the target coverage threshold")
        potcause<-c("","Check the coverage in the different sub-populations to determine whether any particular group is being left out or has lower than average coverage (e.g., males vs. females, SAC, particular sub-districts, ethnic minorities, etc.)",
                    "Check the reasons for why the eligible population was not  offered the drug

                    Check the reasons for the eligible population not swallowing the drug",
                    "Communities and drug distributors are motivated and the programme is functioning well")
        corrective<-c("","Develop and implement targeted social mobilization as required

                      Investigate the reasons why the sub-population(s) are not adequately covered and make the appropriate change in MDA strategy/platform to reach the sub-population(s)

                      Consider using Independent Monitoring or the Coverage Supervision Tool with MDA mop-up during the next round to improve coverage",
                      "Tailor corrective action according to reasons given which include strengthening:
                      - Drug supply chain
                      - Social mobilization and information and education campaigns (look at reasons given by those who did swallow the drug)
                      - Drug delivery platform used
                      - Training, supervision and motivation of drug distributors
                      - Communication on adverse events
                      - Capacity of national and/or district level staff",
                      "Congratulate your teams.  Sustain programme momentum for the next year to maintain coverage levels")

        apndxA1DF<-data.frame(finding,potcause,corrective)

        colnames(apndxA1DF)<-c("Finding or observation", "Potential Causes to Investigate", "Corrective action")

        apndxA1FT<-FlexTable(apndxA1DF, body.par.props = parProperties(padding= 1),
                             header.par.props = parProperties(padding = 3,
                                                              text.align = "center"),
                             header.text.props = textProperties(font.weight = "bold",
                                                                font.family = "Calibri"),
                             body.text.props = textProperties(font.family = "Calibri"))

        apndxA1FT[1,]=cellProperties(background.color = "#B6A0C0")

        apndxA1FT <- spanFlexTableRows( apndxA1FT, j = 1, from = 2, to = 3 )

        apndxA1FT<-spanFlexTableColumns(apndxA1FT, i = 1, from = 1, to = 3)

        doc<- addFlexTable(doc, apndxA1FT)
        #doc<- addParagraph( doc, '', stylename = 'Normal' )

        apndxA1FT



        #Table 2

        finding2<-c("2. Comparison of survey coverage with reported coverage: To check if the data reporting system is working well",
                    "Reported coverage is much higher than survey coverage

                    (i.e., routine reporting is likely overestimating true coverage)","",
                    "Reported coverage is much lower than survey coverage

                    (i.e., routine reporting is likely underestimating true coverage)", "",
                    "Reported coverage and survey coverage are similar")
        potcause2<-c("","Drug distributors are incorrectly reporting on ingestion of the drugs",
                     "The total population figure (e.g.,  the denominator) is incorrect or outdated, or people from outside the survey area are also taking the drugs and are being included in the total treatment tallies",
                     "The total population figure (e.g., the denominator) is incorrect or outdated",
                     "Data are not being correctly aggregated or reported ", "A good reporting system is in place")
        corrective2<-c("","Conduct a Data Quality Self-Assessment or Data Quality Assessment to diagnose where the data reporting system is breaking down

                       Improve the skills and motivation of drug distributors through better training and supervision; consider use of mHealth technologies

                       Make improvements to the tally sheets and/or registers used ", "Determine if more accurate population estimates or projections are available and apply a correction factor to routine coverage estimates as appropriate

                       Ask the drug distributors to record and report non-resident individuals ingesting the drugs separately and do not include them in the numerator for calculating PC coverage",
                       "Refer to corrective actions given for the same problem above",
                       "Conduct a Data Quality Self-Assessment or Data Quality Assessment  to diagnose where the data reporting system is breaking down",
                       "Congratulate your teams. Continue using the current reporting system, with increased confidence that it provides a good estimate of PC coverage.  Less expenditure in future surveys (at least for the current survey area) is required.")

        apndxA2DF<-data.frame(finding2,potcause2,corrective2)

        colnames(apndxA2DF)<-c("Finding or observation", "Potential Causes to Investigate", "Corrective action")

        apndxA2FT<-FlexTable(apndxA2DF, body.par.props = parProperties(padding= 1),
                             header.columns = FALSE,
                             header.par.props = parProperties(padding = 3,
                                                              text.align = "center"),
                             header.text.props = textProperties(font.weight = "bold",
                                                                font.family = "Calibri"),
                             body.text.props = textProperties(font.family = "Calibri"))

        apndxA2FT[1,]=cellProperties(background.color = "#E49CA7")

        apndxA2FT <- spanFlexTableRows( apndxA2FT, j = 1, from = 2, to = 3 )

        apndxA2FT <- spanFlexTableRows( apndxA2FT, j = 1, from = 4, to = 5 )

        apndxA2FT<-spanFlexTableColumns(apndxA2FT, i = 1, from = 1, to = 3)

        doc<- addFlexTable(doc, apndxA2FT)
        #doc<- addParagraph( doc, '', stylename = 'Normal' )

        apndxA2FT


        #Table 3

        finding3<-c("3. Comparison of survey coverage with programme reach: To assess compliance and the success of the programs social mobilization and communication strategy",
                    "Survey coverage is less than programme reach","Survey coverage is greater than programme reach",
                    "Survey coverage is close to or equal to programme reach")
        potcause3<-c("","Check the reasons given for why individuals who were offered the drug did not swallow it",
                     "There may be a problem with the coverage survey data.  Check to see if the survey team implemented the questionnaire correctly; recount results and check for arithmetic errors",
                     "Compliance is high")
        corrective3<-c("","Improved information and education campaigns may be needed prior to the next round to increase compliance",
                       "Greater training may be needed to make sure the survey team correctly understands the difference between being offered the drug(s) and swallowing the drugs and that this difference is conveyed correctly to the respondents",
                       "Congratulate the teams. Continue with the current information and education campaigns, as they appear to be working")

        apndxA3DF<-data.frame(finding3,potcause3,corrective3)

        colnames(apndxA3DF)<-c("Finding or observation", "Potential Causes to Investigate", "Corrective action")

        apndxA3FT<-FlexTable(apndxA3DF, body.par.props = parProperties(padding= 1),
                             header.columns = FALSE,
                             header.par.props = parProperties(padding = 3,
                                                              text.align = "center"),
                             header.text.props = textProperties(font.weight = "bold",
                                                                font.family = "Calibri"),
                             body.text.props = textProperties(font.family = "Calibri"))

        apndxA3FT[1,]=cellProperties(background.color = "#2E93CD")

        apndxA3FT<-spanFlexTableColumns(apndxA3FT, i = 1, from = 1, to = 3)

        doc<- addFlexTable(doc, apndxA3FT)

        apndxA3FT

        return(doc)

}


