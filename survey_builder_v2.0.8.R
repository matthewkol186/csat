#///////////////////////////////////////////////////////////////#
# Program: Coverage Survey Analysis Tool for a Single District  #
# using the Coverage Survey Builder tool                        #
# Date Created: 03/14/17                                        #
# Date Updated: 11/9/18                                         #
#                                                               #
# Version:2.0.8                                                 #
#                                                               #
# Original Programmer: Lucas Trower 662846587/2230275           #
#                                                               #
# Params:                                                       #
#                                                               #
#   csv                                                         #
#   country_name                                                #
#   implementation_unit_header                                  #
#   drug_name                                                   #
#   disease_name                                                #
#   reported_coverage_header                                    #
#                                                               #
#///////////////////////////////////////////////////////////////#
# install.packages("readr")
# install.packages("dplyr")
library(readr)
library(survey)
library(ggplot2)
library(plyr)
library(ReporteRs)
library(purrr)
library(reshape)
library(PropCIs)
library(dplyr)

#Purpose: Give a visual output of CSA Tool for easier interpretation

wordDoc <- function() {
        dat <- read_csv(csv_url)
        doc <- analyzeData(dat)
        return(doc)
}

# set Using either analyzeData1 or analyzeData2 if reported coverage is measured or not

analyzeData<- function(dat) {
        
        #-------------------------------------------------------------------------------------------#
        #Creating variable to change title of document for Country, Implementation Unit, & Disease  #
        #-------------------------------------------------------------------------------------------#
        
        country<-country_name
        
        IU<-implementation_unit_header
        
        disease<-disease_name
        
        drug<-drug_name
        
        #Checking to make sure the drug packages are selected with the appropriate disease
        
        if (disease=="Lymphatic Filariasis"){
                
                if (drug=="Ivermectin" | drug=="Albendazole" | drug=="ALB & IVM" | drug=="ALB & DEC" | drug=="ALB, DEC, & IVM"){
                        
                }else{
                        stop("The drug/drug package selected is not used in treatment for the disease selected")
                }
                
        }
        
        
        if (disease=="Onchocerciasis"){
                
                if (drug=="Ivermectin"){
                        
                }else{
                        stop("The drug/drug package selected is not used in treatment for the disease selected")
                }
                
        }
        
        if (disease=="Trachoma"){
                
                if (drug=="Azithromycin"){
                        
                }else{
                        stop("The drug/drug package selected is not used in treatment for the disease selected")
                }
                
        }
        
        if (disease=="STH"){
                
                if (drug=="Albendazole" | drug=="Mebendazole"){
                        
                }else{
                        stop("The drug/drug package selected is not used in treatment for the disease selected")
                }
                
        }
        
        if (disease=="Schistosomiasis"){
                
                if (drug=="Praziquantel"){
                        
                }else{
                        stop("The drug/drug package selected is not used in treatment for the disease selected")
                }
                
        }
        
        sub_num<-as.numeric(number_of_subunits)
        
        if(exists('reported_coverage_header')){
                reported_coverage_header<-as.numeric(reported_coverage_header)
                r_coverage<-reported_coverage_header*100
        }
        
        #Removed the error message, but can be reinstated after discussion
        
        # if (exists('r_coverage')){
        # 
        #         if (0<=r_coverage & r_coverage<=100){
        #                 
        #         }else{
        #                 stop("Reported coverage was not reported as a decimal between 0 & 1")
        #         }
        #         
        # }
        
        #Date: 2/18/2015  **revised 12/9/2016
        #Programmer: Katie Gass
        #Purpose: 
        #1) To import cluster summary survey data from csv
        #2) TO reshape these data into long files (by offered vs. swallowed drug, for males, females and all) 
        #3) To calculate the program reach and survey coverage
        #4) To output this information into csv file that can automatically populate a webfrom
        #FUTURE GOALS: to create user dashboard (to feed into website) with nice, interpretable graphics 
        
        #Note: first the user must install the package 'survey' on their computer
        
        #this tells R that certainty units - single stratum PSU - should not contribute to variance
        options(survey.lonely.psu = "certainty")
        
        #we are using the "Ultimate Cluster" approach because we need the tool to be general and cannot assume that 
        #users will have the information required to account for each stage of sampling (FPC)
        options(survey.ultimate.cluster = "TRUE")
        
        
        #Checking to make sure the version of the CSB is the most recent
        
        version_check<-read.csv(csv_url, skip=3)
        version_check<-version_check[,1:10]
        
        version_col_names<-list("Strata", "Subunit_.Name", "No_Females.Interviewed", 
                                "No_Males.Interviewed", "No_.Females.Offered", 
                                "No_Males.Offered", "No_Females.Swallowed", 
                                "No_Males.Swallowed", "Remarks", "Selected.Subunits")
        
        version_col_names2<-list("Strata", "Subunit.Name", "No..Females.Interviewed", 
                                "No..Males.Interviewed", "No..Females.Offered", 
                                "No..Males.Offered", "No..Females.Swallowed", 
                                "No..Males.Swallowed", "Remarks", "Selected.Subunits")
        
        for (b in 1:dim(version_check)[2]){
        
                if (colnames(version_check)[b] == version_col_names[b] | colnames(version_check)[b] == version_col_names2[b]){
                        
                }else{
                        stop("Dataset uploaded does not match the most recent version of the Coverage Survey Builder output. Please copy and paste the following URL (https://www.ntdsupport.org/resources/coverage-survey-builder-coverage-evaluations) into your browser, or utilize the template provided on this website.")
                }
        }
        
        #point R to the location of the csv file
        
        dat<-read.csv(csv_url, skip=3, nrow=sub_num, header = TRUE)
        
        dat<-dat[,1:10]
        
        colnames(dat)<-c("Strata","Subunit_Name","Females_Interviewed", 
                         "Males_Interviewed","Females_Offered","Males_Offered",
                         "Females_Swallowed","Males_Swallowed","Remarks","Subunit")
         
        datcheck<-dat[1:sub_num,3:8]
        
        for (r in 1:dim(datcheck)[1]){
                for (c in 1:dim(datcheck)[2]){
                        if(!is.numeric(datcheck[r,c]) | is.na(datcheck[r,c])){
                                stop(paste("There is a non-numeric or blank value where a numeric value is expected. Please look through your file to check for an error."))
                        }
                }
        }
        
        #trim off unnecessary columns
        
        dat2<-dat[,c(1,3:8,10)]
        
        # Number interviewed must be >= number offered >= number swallowed
        
        for (a in 1:dim(dat2)[1]){
                
        #Female

                if (dat2$Females_Offered[a] > dat2$Females_Interviewed[a]){
                        stop("One or more of the subunits has more females offered the drug than interviewed")
                }
                        
                if (dat2$Females_Swallowed[a] > dat2$Females_Interviewed[a]){
                        stop("One or more of the subunits has more females swallowing the drug than interviewed")
                }
                        
                if (dat2$Females_Swallowed[a] > dat2$Females_Offered[a]){
                        stop("One or more of the subunits has more females swallowing the drug than offered")
                }
                
        #Male
                
                if (dat2$Males_Offered[a] > dat2$Males_Interviewed[a]){
                        stop("One or more of the subunits has more males offered the drug than interviewed")
                }
                
                if (dat2$Males_Swallowed[a] > dat2$Males_Interviewed[a]){
                        stop("One or more of the subunits has more males swallowing the drug than interviewed")
                }
                
                if (dat2$Males_Swallowed[a] > dat2$Males_Offered[a]){
                        stop("One or more of the subunits has more males swallowing the drug than offered")
                }
        
        }
        
        dat2<-dat2[complete.cases(dat2),]

        if (exists("r_coverage")){
                doc<-try(analyzeData1(dat2, country, IU, disease, drug, r_coverage, sub_num))
        } else{ 
                doc<-try(analyzeData2(dat2, country, IU, disease, drug, sub_num))
        }
        return(doc)
        
}

analyzeData1<-function(dat2, country, IU, disease, drug, r_coverage, sub_num){
        
        
        #Creating vector that houses the colors for males and females for figures
        
        colors <- c("#FF9F33", "#428EFC")
        
        #Setting the y axis limit for figures
        
        y_limit<-ifelse(r_coverage>100, r_coverage+10, 100)
        
        #-----------------------------------------------------------------------------------------------#
        #Creating variable thresh that will insert the WHO threshold for figures based on the NTD       # 
        #the user chose. Onchocerciasis is used as a test, but it will need to be generated from the    # 
        #-----------------------------------------------------------------------------------------------#
        
        if (disease=="Lymphatic Filariasis" | disease=="Onchocerciasis") {
                thresh=65
        } else if (disease=="Trachoma") {
                thresh=80
        } else thresh=75 #Value for STH and Schistosomiasis
        
        if (disease=="Onchocerciasis" | disease=="Lymphatic Filariasis") {
                threshcol<-"Meets or Exceeds the \n Target 65% Threshold"
        } else if (disease=="Trachoma") {
                threshcol<-"Meets or Exceeds the \n Target 80% Threshold"
        } else threshcol<-"Meets or Exceeds the \n Target 75% Threshold" #Value for STH and Schistosomiasis
        
        #first add columns for number not swallowed and not offered 
        dat2$Females_Noswallow<-dat2$Females_Interviewed - dat2$Females_Swallowed
        dat2$Females_Nooffer<-dat2$Females_Interviewed - dat2$Females_Offered
        dat2$Males_Noswallow<-dat2$Males_Interviewed - dat2$Males_Swallowed
        dat2$Males_Nooffer<-dat2$Males_Interviewed - dat2$Males_Offered
        
        #now get total interviewed, offered and swallowed
        dat2$Interviewed<-dat2$Females_Interviewed + dat2$Males_Interviewed
        dat2$Offered <-dat2$Females_Offered + dat2$Males_Offered
        dat2$Swallowed<-dat2$Females_Swallowed + dat2$Males_Swallowed
        dat2$No_offer<-dat2$Females_Nooffer + dat2$Males_Nooffer
        dat2$No_swallow<-dat2$Females_Noswallow + dat2$Males_Noswallow
        
        #subset data so that we can get the program reach (#offered/total) and survey coverage (#swallow/total) separately
        dat_offer<-dat2[,c(1,8,14,16)]
        dat_swallow<-dat2[,c(1,8,15,17)]
        dat_fem_offer<-dat2[,c(1,8,4,10)]
        dat_fem_swallow<-dat2[,c(1,8,6,9)]
        dat_male_offer<-dat2[,c(1,8,5,12)]
        dat_male_swallow<-dat2[,c(1,8,7,11)]
        
        #data is currently in wide format, need to transform to long
        #there's definitely a prettier way to condense this code - but for the sake of time and simplicity I've written it all out
        
        #first do for "offered" the drug
        long_offer<-reshape(dat_offer,direction="long",idvar="Subunit",varying=c("Offered","No_offer"),
                            times=c("Offered","No_offer"),v.names="COUNT",timevar="COVERAGE")
        
        index_offer<-rep(seq_len(nrow(long_offer)),times=long_offer$COUNT)
        ds_offer<<-long_offer[index_offer,]
        
        #Now do for "swallowed" the drug
        long_swallow<-reshape(dat_swallow,direction="long",idvar="Subunit",varying=c("Swallowed","No_swallow"),
                              times=c("Swallowed","No_swallow"),v.names="COUNT",timevar="COVERAGE")
        
        index_swallow<-rep(seq_len(nrow(long_swallow)),times=long_swallow$COUNT)
        ds_swallow<<-long_swallow[index_swallow,]
        
        #Female "offered" the drug
        long_offer_f<-reshape(dat_fem_offer,direction="long",idvar="Subunit",varying=c("Females_Offered","Females_Nooffer"),
                              times=c("Offered","No_offer"),v.names="COUNT",timevar="COVERAGE")
        
        index_offer_f<-rep(seq_len(nrow(long_offer_f)),times=long_offer_f$COUNT)
        ds_offer_f<<-long_offer_f[index_offer_f,]
        
        ds_offer_f$sex<-"female"
        
        #Female "swallowed" the drug
        long_swallow_f<-reshape(dat_fem_swallow,direction="long",idvar="Subunit",varying=c("Females_Swallowed","Females_Noswallow"),
                                times=c("Swallowed","No_swallow"),v.names="COUNT",timevar="COVERAGE")
        
        index_swallow_f<-rep(seq_len(nrow(long_swallow_f)),times=long_swallow_f$COUNT)
        ds_swallow_f<<-long_swallow_f[index_swallow_f,]
        
        ds_swallow_f$sex<-"female"
        
        #Male "offered" the drug
        long_offer_m<-reshape(dat_male_offer,direction="long",idvar="Subunit",varying=c("Males_Offered","Males_Nooffer"),
                              times=c("Offered","No_offer"),v.names="COUNT",timevar="COVERAGE")
        
        index_offer_m<-rep(seq_len(nrow(long_offer_m)),times=long_offer_m$COUNT)
        ds_offer_m<<-long_offer_m[index_offer_m,]
        
        ds_offer_m$sex<-"male"
        
        #Male "swallowed" the drug 
        long_swallow_m<-reshape(dat_male_swallow,direction="long",idvar="Subunit",varying=c("Males_Swallowed","Males_Noswallow"),
                                times=c("Swallowed","No_swallow"),v.names="COUNT",timevar="COVERAGE")
        
        index_swallow_m<-rep(seq_len(nrow(long_swallow_m)),times=long_swallow_m$COUNT)
        ds_swallow_m<<-long_swallow_m[index_swallow_m,]
        
        ds_swallow_m$sex<-"male"
        
        ds_offer_combined<-rbind(ds_offer_f,ds_offer_m)
        ds_swallow_combined<-rbind(ds_swallow_f,ds_swallow_m)
        
        
        #now calculate the coverage for offered and swallowed
        
        
        #____________DESIORANGE OUTPUTS__________________________
        
        #Program Reach
        #Total program reach + 95% CI
        #Female program reach + 95% CI
        #Male program reach + 95% CI
        
        #Survey Coverage
        #Total survey coverage + 95% CI + DEFF  (to validate the reported coverage)
        #Female survey coverage + 95% CI
        #Male survey coverage + 95% CI
        
        #Total survey coverage + lower 1-sided CI (to compare with threshold)
        #Total survey coverage + lower 1-sided CI (to compare with threshold) *note that not powered for comparison
        #Total survey coverage + lower 1-sided CI (to compare with threshold) *note that not powered for comparison
        
        #Assumptions: we are assuming that exactly 30 clusters will always be used 
        # Strata information is retained from Coverage Survey Builder (with certainty units in their own strata) and 
        #all other subunits are formed into pseudostrata according to geographic proximity
        #Ultimate Cluster design is used, whereby no finite population corrections are entered
        #THe logit method is used to calculate CI (because it is an improvement over the Wald and can be cited in literature)
        
        
        #first calculate coverage for program reach (#offer/total)
        
        #specify survey design (one-stage cluster with implicit stratification)
        fn_program_reach<-function(ds){
                
                design_offer<-svydesign(ids=~Subunit, strata=~Strata, data=ds)
                result_offer<-svyciprop(~I(COVERAGE=="Offered"),design_offer,method="logit",level=0.95)
                ci<-as.vector(attr(result_offer,"ci"))
                upperci<-round(ci[2],4)
                lowerci<-round(ci[1],4)
                phat<-round(as.vector(result_offer),4)
                
                program_reach<-c(phat, lowerci, upperci)
                
                return(program_reach)
        }
        
        #Next calculate survey coverage 
        #return: 2-sided 95% CI (for validation with reported coverage), design effect, and lower 1-sided CI (for threshold)
        
        #specify survey design (one-stage cluster with implicit stratification)
        fn_survey_coverage<-function(ds){
                
                design_swallow<-svydesign(ids=~Subunit, strata=~Strata, data=ds)
                result_swallow<-svyciprop(~I(COVERAGE=="Swallowed"),design_swallow,method="logit",level=0.95)
                ci<-as.vector(attr(result_swallow,"ci"))
                upperci<-round(ci[2],4)
                lowerci<-round(ci[1],4)
                phat<-round(as.vector(result_swallow),4)
                #get DEFF
                DEFF<-round(as.data.frame(svymean(~factor(COVERAGE, levels=c("Swallowed","No_swallow")), design_swallow, deff="replace"))[1,3],1)
                
                #get 1-sided lower 95% limit
                result_swallow90<-svyciprop(~I(COVERAGE=="Swallowed"),design_swallow,method="logit",level=0.90)
                ci90<-as.vector(attr(result_swallow90,"ci"))
                lowerlimit<-round(ci90[1],4)
                survey_coverage<-c(phat, lowerci, upperci,DEFF,lowerlimit) 
                return(survey_coverage)
        }
        
        total_pr<-fn_program_reach(ds_offer)
        female_pr<-fn_program_reach(ds_offer_f)
        male_pr<-fn_program_reach(ds_offer_m)
        
        total_sc<-fn_survey_coverage(ds_swallow)
        female_sc<-fn_survey_coverage(ds_swallow_f)
        male_sc<-fn_survey_coverage(ds_swallow_m)
        
        #now concatenate data
        final<-as.data.frame(rbind(total_pr,female_pr,male_pr,total_sc,female_sc,male_sc))
        
        #-------------------------------------#
        #Showing Results for Programme Reach  #
        #-------------------------------------#
        
        #Reconfiguring final in order to have the variables as variables (columns) and not observations (rows)
        
        total_pr2 = c(final[1:1,1:1],final[1:1,2:2] ,final[1:1,3:3] ) 
        male_pr2 = c(final[3:3,1:1],final[3:3,2:2] ,final[3:3,3:3]) 
        female_pr2 = c(final[2:2,1:1],final[2:2,2:2] ,final[2:2,3:3]) 
        final_3 = data.frame(total_pr2, male_pr2, female_pr2)
        
        plot1<- ggplot(data=final_3*100, aes(x=c(5,15,25), y=c(total_pr2[1:1],male_pr2[1:1],female_pr2[1:1])))+
                geom_errorbar(aes(ymin=c(total_pr2[2:2],male_pr2[2:2], female_pr2[2:2]), 
                                  ymax=c(total_pr2[3:3],male_pr2[3:3], female_pr2[3:3])),
                              width=.3,
                              position=position_dodge(.9))+
                geom_point(shape=c(21,24,22),fill=c("#000000","#FF9F33","#428EFC"),
                           stat="identity",color=c("black","black","black"),size=3)+
                xlab("")+
                ylab(paste("Proportion of respondents who reported being offered \n",drug)) +
                ggtitle("Estimated Programme Reach by Gender")+
                theme(plot.title = element_text(hjust = 0.5, size=16))+
                scale_x_continuous(breaks = c(5,15,25), limits = c(0, 30), labels=c("Overall","Male","Female"))+
                theme(
                        panel.grid.major.x=element_blank(),
                        panel.grid.minor.x=element_blank(),
                        legend.position =  "bottom",
                        legend.title = element_blank(),
                        axis.title.y = element_text(face="bold", colour='black', size=10),
                        axis.text.x = element_text(face="bold", color='black', size=10),
                        plot.title = element_text(lineheight=.8, face="bold"))+
                scale_y_continuous(breaks = seq(0, 100, 10))+
                coord_cartesian(ylim=c(0,100))+
                geom_text(label=paste(format(round(total_pr2[1:1]*100,1),nsmall=1),"%"), x=3, y=total_pr2[1:1]*100)+
                geom_text(label=paste(format(round(male_pr2[1:1]*100,1),nsmall=1),"%"), x=13, y=male_pr2[1:1]*100)+
                geom_text(label=paste(format(round(female_pr2[1:1]*100,1),nsmall=1), "%"), x=23, y=female_pr2[1:1]*100)+
                scale_color_discrete(breaks="WHO \nTarget \nCoverage \nThreshold")
        
        plot1
        
        #-------------------------------------#
        #Showing Results for Survey Coverage  #
        #-------------------------------------#
        
        # Reconfiguring final in order to have the variables as variables (columns) 
        # and not observations (rows), and creating a subset of data from final for 
        # survey coverage in order for the graph to no include DEFF and lowerlimit
        
        total_sc2 = c(final[4:4,1:1],final[4:4,2:2] ,final[4:4,3:3] ) 
        male_sc2 = c(final[6:6,1:1],final[6:6,2:2] ,final[6:6,3:3]) 
        female_sc2 = c(final[5:5,1:1],final[5:5,2:2] ,final[5:5,3:3]) 
        final_2 = data.frame(total_sc2, male_sc2, female_sc2)
        
        lowci<-final_2[2,1]*100
        upci<-final_2[3,1]*100
        
        plot2<- ggplot(data=final_2*100, aes(x=c(5,15,25), y=c(total_sc2[1:1],male_sc2[1:1],female_sc2[1:1])))+
                geom_hline(aes(yintercept=thresh, linetype="WHO \nTarget \nCoverage \nThreshold"),
                           color="red3")+
                geom_errorbar(aes(ymin=c(total_sc2[2:2],male_sc2[2:2], female_sc2[2:2]), 
                                  ymax=c(total_sc2[3:3],male_sc2[3:3],female_sc2[3:3])),
                              width=.3,
                              position=position_dodge(.9))+
                geom_point(shape=c(21,24,22),fill=c("#000000","#FF9F33","#428EFC"),
                           stat="identity",color=c("black","black","black"), size=3)+
                xlab("")+
                geom_point(aes(5,r_coverage), color='black', size=2, shape=8)+       #Y-value will need to be pulled from the information the user enters on the homepage of the tool. Create a variable that will change the value of 90 based on this number
                geom_text(label='\nReported \nCoverage', x=8, y=r_coverage, size=3)+   #Y-value will need to be pulled from the information the user enters on the homepage of the tool. Create a variable that will change the value of 90 based on this number
                ylab(paste("Proportion of respondents who reported swallowing \n",drug)) +
                ggtitle("Estimated Survey Coverage by Gender")+
                theme(plot.title = element_text(hjust = 0.5, size=16))+
                scale_x_continuous(breaks = c(5,15,25), limits = c(0, 30), labels=c("Overall","Male", "Female"))+
                theme(
                        panel.grid.major.x=element_blank(),
                        panel.grid.minor.x=element_blank(),
                        legend.position =  "bottom",
                        legend.title = element_blank(),
                        axis.title.y = element_text(face="bold", colour='black', size=10),
                        axis.text.x = element_text(face="bold", color='black', size=10),
                        plot.title = element_text(lineheight=.8, face="bold"))+
                scale_y_continuous(breaks = seq(0, y_limit, 10))+
                coord_cartesian(ylim=c(0,y_limit))+
                geom_text(label=paste(format(round(total_sc2[1:1]*100,1),nsmall=1),"%"), x=3, y=total_sc2[1:1]*100)+
                geom_text(label=paste(format(round(male_sc2[1:1]*100,1),nsmall=1),"%"), x=13, y=male_sc2[1:1]*100)+
                geom_text(label=paste(format(round(female_sc2[1:1]*100,1),nsmall=1), "%"), x=23, y=female_sc2[1:1]*100)+
                scale_color_discrete(breaks="WHO \nTarget \nCoverage \nThreshold") #Only allows "WHO" 
        
        plot2
        
        # plots12<-grid.arrange(plot1, plot2, ncol=2)
        
        total_sc3 = final[4:4,1:1]*100
        lower_sc3=final[4:4,2:2]*100
        upper_sc3=final[4:4,3:3]*100
        valid_sc=data.frame(total_sc3, lower_sc3, upper_sc3)
        
        thresh_sc=final[4:4, 1:1]
        thresh_lowerci=final[4:4,5:5]
        fin_thresh=data.frame(thresh_sc, thresh_lowerci)
        
        #-----------------------------------------------#
        #Showing Results for Surve Coverage by Subunit  #
        #-----------------------------------------------#
        
        ds_swallow2<-ds_swallow
        
        ds_swallow2$coverage<-ifelse(ds_swallow2$COVERAGE=="Swallowed", 1, 0)
        
        fnsurvcovsub<-function(df){
                
                design_swallow<-svydesign(ids=~1,  data=df)
                result_swallow<-svyciprop(~I(coverage==1),
                                          design_swallow, method="logit", level=0.95)
                ci<-as.vector(attr(result_swallow,"ci"))
                upci<-round(ci[2],3)
                lowci<-round(ci[1],3)
                phat<-round(as.vector(result_swallow),3)
                
                fnsurvcovsub<-c(phat, lowci, upci)
                return(fnsurvcovsub)
        }
        
        #Creating a matrix in order to store the outputs for each district
        
        output <- matrix(ncol=3, nrow=nrow(dat2))
        
        output<-data.frame(output)
        
        #for loop used to calculate the phat, 95% CI, and DEFF for each of the districts
        
        for (p in 1:nrow(dat2)){
                
                dat4covci<-subset(ds_swallow2, Subunit==p)
                
                #placing each district's output into their own row
                
                output[p,]<-fnsurvcovsub(dat4covci)
                
                print(output)
                
        }
        
        covcioutput<-data.frame(output*100)
        
        #Confidence intervals for clusters/subunits in single district analysis
        
        dat_treat.conf.lim<-dat2[,c("Interviewed","Swallowed","Subunit")]
        
        dat_treat.conf.lim$uci<-0
        dat_treat.conf.lim$lci<-0
        
        for (b in 1:sub_num){
                
                #Loops through each subunit to output the upper and lower confidence intervals
                
                dat_treat.conf.lim[b,4]<-100*as.numeric(unlist(scoreci(dat_treat.conf.lim[b,2], 
                                                                       dat_treat.conf.lim[b,1], conf.level = .95)))[2]
                dat_treat.conf.lim[b,5]<-100*as.numeric(unlist(scoreci(dat_treat.conf.lim[b,2], 
                                                                       dat_treat.conf.lim[b,1], conf.level = .95)))[1]
                
        }
        
        dat_treat.conf.lim$Treated<-100*(dat_treat.conf.lim$Swallowed/dat_treat.conf.lim$Interviewed)
        
        dat_treat.conf.lim$Subunit <- factor(dat_treat.conf.lim$Subunit, 
                                             levels = dat_treat.conf.lim$Subunit[rev(order(dat_treat.conf.lim$Treated))])
        
        
        plot5<- ggplot(data=dat_treat.conf.lim, aes(x=Subunit, y=Treated))+
                geom_hline(aes(yintercept=thresh, linetype="WHO \nTarget \nCoverage \nThreshold"),
                           color="red3")+
                geom_point(stat="identity")+
                geom_errorbar(aes(ymin=dat_treat.conf.lim$uci, ymax=dat_treat.conf.lim$lci),
                              width=.2,
                              position=position_dodge(.9))+
                xlab("Cluster")+
                ylab(paste("Proportion of respondents who reported swallowing \n",drug)) +
                ggtitle("Plot of Survey Coverage by Subunit: Greatest to Least Coverage") +
                theme(
                        plot.title = element_text(hjust = 0.5, size=12, face="bold"),
                        legend.title = element_blank(),
                        legend.position =  "bottom",
                        axis.title.y = element_text(face="bold", colour='black', size=10),
                        axis.title.x = element_text(face="bold", colour='black', size=10))+
                scale_y_continuous(breaks = seq(0, 100, 10))+
                coord_cartesian(ylim=c(0,100))
        
        plot5
        

        
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
        
        titlepg<-pot(paste(disease, "Coverage Evaluation Results Summary:", IU,",", country,",", format(Sys.time(), '%B %Y')),
                     textProperties(font.weight = "bold", font.family = "Calibri",
                                    font.size = 20))
        titlepg
        
        doc<-addParagraph(doc, titlepg)
        doc <- addParagraph( doc, '', stylename = 'Normal' )
        
        sumsent<- pot("This summary reviews the results from coverage evaluation surveys for ",
                      textProperties(font.family = "Calibri"))+
                pot(drug, textProperties(font.weight = "bold", font.family = "Calibri"))+
                pot(" that were conducted in ",textProperties(font.family = "Calibri"))+
                pot(IU, textProperties(font.weight = "bold", font.family = "Calibri"))+
                pot(", ", textProperties(font.weight = "bold", font.family = "Calibri"))+
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
        
        doc<-addPlot(doc, fun=print, x=plot1)
        doc<-addPlot(doc, fun=print, x=plot2)
        
        # doc<- addPageBreak(doc)
        
        #--------------------------------------------------------------------#
        # Inserting Survey coverage results by district table                #
        #--------------------------------------------------------------------#
        
        table1<-pot(paste("Table 1. Survey coverage results for",IU,",",
                          country, ",",format(Sys.time(), "%Y"),"."),
                    textProperties(font.family = "Calibri",
                                   font.weight = "bold"))
        
        doc<- addParagraph( doc, '', stylename = 'Normal' )
        
        doc<-addParagraph(doc, table1)
        
        #-----------------------------------------------#
        # Calculating programme reach for each district #
        #-----------------------------------------------#
        if (exists("Offered", where=dat2)){
                reach_dist<-round(((sum(dat2$Offered))/(sum(dat2$Offered)+sum(dat2$No_offer)))*100,2)
        }
        
        if (exists("Offered", where=dat2)){
                
                surcovDF<-data.frame(IU, paste(r_coverage,"%"), paste(total_sc[1]*100,"%"),
                                     paste(total_sc[2]*100, "%"), paste(total_sc[3]*100,"%"),
                                     total_sc[4], paste(reach_dist,"%"))
                
                #Renaming the columns
                
                colnames(surcovDF) <- c("Implementation Unit", "Reported Coverage","Survey Coverage",
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
                surcovDF<-data.frame(IU, paste(r_coverage,"%"), paste(total_sc[1]*100,"%"),
                                     paste(total_sc[2]*100, "%"), paste(total_sc[3]*100,"%"),
                                     total_sc[4])
                
                #Renaming the columns
                
                colnames(surcovDF) <- c("Implementation Unit", "Reported Coverage","Survey Coverage",
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
                
                #-----------------------------------------------------------------------#
                #Making the table for differences between surveyed and reported (%)     #
                #with conditional coloring for the diff                                #
                #-----------------------------------------------------------------------#
                
                #Creating new dataframe to manipulate
                
                if (exists("r_coverage") & length(r_coverage)!=0){
                        
                        
                        diffDF<-data.frame(IU, total_sc[1]*100, r_coverage, total_sc[1]*100-r_coverage)
                        
                        #Adding interpretations for the difference beween surveyed and reported coverage
                        
                        diffDF$interp<-ifelse(lowci<= r_coverage & r_coverage<= upci, "Yes; survey coverage validates \n reported coverage",
                                              ifelse(abs(diffDF[,4])>=0 & abs(diffDF[,4])<=10,"Yes; survey and reported coverage are similar",
                                                     ifelse (diffDF[,4]>10 & diffDF[,4]<= 25, "No; survey coverage is greater", 
                                                             ifelse(diffDF[,4]>25,"No; survey coverage is much greater",
                                                                    ifelse(diffDF[,4]>-25 & diffDF[,4]<= -10, "No; survey coverage is less",
                                                                           "No; survey coverage is much less")))))
                        
                        
                        #Adding interpretations for the difference beween surveyed and reported coverage
                        
                        diffDF$comp_thresh<-ifelse(total_sc[1]*100>=thresh, "Yes","No")
                        
                        #Renaming the columns
                        
                        colnames(diffDF) <- c("IU", "Survey Coverage","Reported Coverage","Difference", 
                                               "Survey Coverage & \n Reported Coverage \n are Similar", 
                                               threshcol)
                        
                        #Creating the flextable in order to allow for conditional colorization
                        
                        diffFT<-FlexTable(diffDF, body.par.props = parProperties(padding= 1,
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
                                diffFT[abs(diffDF[i]) >=0 & abs(diffDF[i]) <= 10, 5] = cellProperties( background.color = "#0DCF00" )
                                diffFT[abs(diffDF[i]) > 10 & abs(diffDF[i]) <= 25, 5] = cellProperties( background.color = "#F7FF00" )
                                diffFT[abs(diffDF[i]) > 25, 5] = cellProperties( background.color = "#FF0000")
                        }
                        
                        varsint<-threshcol
                        
                        for (w in varsint) {
                                diffFT[diffDF[w]=="Yes", 6] = cellProperties( background.color = "#0DCF00" )
                                diffFT[diffDF[w]=="No", 6] = cellProperties( background.color = "#FF0000")
                        }
                        
                        diffFT
                        
                }
                
                
                doc<-addFlexTable(doc, diffFT)
                
                doc<-addPageBreak(doc)
                
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
                
                doc<-addFlexTable(doc, difkeyFT)
                
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
                
                doc<- addParagraph( doc, '', stylename = 'Normal' )
                
                doc<-addFlexTable(doc,threshFT)
                
                doc<- addParagraph( doc, '', stylename = 'Normal' )
                
                valthresh<-pot("The desired outcome is to have the surveyed coverage within 10% of the reported coverage, as well as meet WHO's target coverage threshold for ",
                               textProperties(font.family = "Calibri"))+
                        pot(disease,textProperties(font.family = "Calibri"))+
                        pot(".",textProperties(font.family = "Calibri"))
                
                doc<-addParagraph(doc,valthresh)
                
                doc<- addPageBreak(doc)
        }
        
        ############################################################# 
        # Adding in the remainder of the plots from original report #
        #############################################################
        
        doc<-addPlot(doc, fun=print, x=plot5)
        
        doc<- addPageBreak(doc)
        
        #-------------------------------------------------------------------------------#
        # Calculating the p_value to see if there is a statistical difference between   #
        # females and males being offered and/or taking the drug                        #
        #-------------------------------------------------------------------------------#
        
        title4<-pot(value = "SURVEY COVERAGE BY SEX", format=title.font)
        
        doc<-addParagraph(doc, title4)
        doc<- addParagraph( doc, '', stylename = 'Normal' )
        
        if (exists("Offered", where=dat2)){
                
                table3<-pot(paste("Table 3. Programme reach and survey coverage by sex:",IU,",",
                                  country, ",", format(Sys.time(),"%Y"), "."), 
                            textProperties(font.family = "Calibri",
                                           font.weight = "bold"))
        } else {
                table3<-pot(paste("Table 3. Survey coverage by sex:",IU,",",
                                  country, ",", format(Sys.time(),"%Y"), "."), 
                            textProperties(font.family = "Calibri",
                                           font.weight = "bold"))
        }
        
        doc<-addParagraph(doc, table3)
        
        #Creating data frames to make matrices, in order to calculate the p_value
        
        # if (exists("Offered", where=dat2)){
        #         
        #         togetoff<-data.frame(IU, sum(dat2$Females_Offered), sum(dat2$Females_Nooffer), 
        #                              sum(dat2$Males_Offered), sum(dat2$Males_Nooffer))
        #         
        # }
        # 
        # togetswall<-data.frame(IU, sum(dat2$Females_Swallowed), sum(dat2$Females_Noswallow), 
        #                        sum(dat2$Males_Swallowed), sum(dat2$Males_Noswallow))
        # 
        # var3<-IU
        # 
        # #blank data frames to insert p_value into
        # 
        # if (exists("Offered", where=dat2)){
        #         
        #         chsqo<-data.frame(matrix(ncol = 1, nrow = length(var3)))
        #         
        # }
        # 
        # chsqs<-data.frame(matrix(ncol = 1, nrow = length(var3)))
        # 
        # #Loop that will output the p_value to the respective data frame
        # 
        # for (k in var3){
        #         
        #         if (exists("Offered", where=dat2)){
        #                 
        #                 mat<-matrix(unlist(togetoff[match(k, IU),2:5]), nrow = 2)
        #                 chsqo[match(k, IU),]<-chisq.test(mat)$p.value
        #                 
        #         }
        #         
        #         mat2<-matrix(unlist(togetswall[match(k, IU),2:5]), nrow = 2)
        #         chsqs[match(k, IU),]<-chisq.test(mat2)$p.value
        # }
        # 
        # #Renaming the column
        # 
        # if (exists("Offered", where=dat2)){
        #         
        #         colnames(chsqo)<-"p-value"
        #         
        # }
        # 
        # colnames(chsqs)<-"p-value"
        
        #Creating a new dataframe in order to display the 'Coverage by Sex' table
        
        if (exists("Offered", where=dat2)){
                cover_sex<-data.frame(IU, paste(round(sum(dat2$Females_Offered/sum(dat2$Females_Interviewed))*100,2), "%"), 
                                      paste(round(sum(dat2$Females_Swallowed)/sum(dat2$Females_Interviewed)*100,2), "%"), 
                                      paste(round(sum(dat2$Males_Offered/sum(dat2$Males_Interviewed))*100,2), "%"), 
                                      paste(round(sum(dat2$Males_Swallowed)/sum(dat2$Males_Interviewed)*100,2), "%"))
                
        }else{
                cover_sex<-data.frame(IU, paste(round(sum(dat2$Females_Swallowed)/sum(dat2$Females_Interviewed)*100,2), "%"),
                                      paste(round(sum(dat2$Males_Swallowed)/sum(dat2$Males_Interviewed)*100,2), "%"))
        }
        
        #Obtaining the p-values for the chi-square test between the sexes for reach and treatment
        
        #Offer p-value calculation
        
        design_offer<-svydesign(ids=~Subunit, strata=~Strata, data=ds_offer_combined)
        
        chi.offer<-as.numeric(svychisq(~COVERAGE + sex, design_offer)[3])
        
        #Swallow p-value calculation
        
        design_swallow<-svydesign(ids=~Subunit, strata=~Strata, data=ds_swallow_combined)
        
        chi.swallow<-as.numeric(svychisq(~COVERAGE + sex, design_swallow)[3])
        
        #Fill in cell with 'no' if not statistically significant, otherwise, 'yes'
        
        if (exists("Offered", where=dat2)){
                cover_sex$compr_male<-ifelse(chi.offer>=.05 ,"No", "Yes")
        }
        
        cover_sex$compt_male<-ifelse(chi.swallow>=.05 ,"No", "Yes")
        
        #Rearranging columns for aesthetics
        
        if (exists("Offered", where=dat2)){
                
                cover_sex<-cover_sex[,c(1,2,4,6,3,5,7)]
                
                colnames(cover_sex) <- c("Implementation Unit", "% Reached amongst females interviewed (offered drug)",
                                         "% Reached amongst males interviewed (offered drug)",
                                         "Programme Reach for females statistically different from males",
                                         "% Treated amongst females interviewed (swallowed drug)",
                                         "% Treated amongst males interviewed (swallowed drug)",
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
                        covsexFT[cover_sex[, j]== "Yes" & (sum(dat2$Females_Offered/sum(dat2$Females_Interviewed))*100)>(sum(dat2$Males_Offered/sum(dat2$Males_Interviewed))*100), j] = cellProperties( background.color = "#428EFC")
                        covsexFT[cover_sex[, j]== "Yes" & (sum(dat2$Females_Offered/sum(dat2$Females_Interviewed))*100)<(sum(dat2$Males_Offered/sum(dat2$Males_Interviewed))*100), j] = cellProperties( background.color = "#FF9F33")
                        covsexFT[cover_sex[, j]== "Yes" & (sum(dat2$Females_Swallowed)/sum(dat2$Females_Offered)*100)>(sum(dat2$Males_Swallowed)/sum(dat2$Males_Offered)*100), j] = cellProperties( background.color = "#428EFC")
                        covsexFT[cover_sex[, j]== "Yes" & (sum(dat2$Females_Swallowed)/sum(dat2$Females_Offered)*100)<(sum(dat2$Males_Swallowed)/sum(dat2$Males_Offered)*100), j] = cellProperties( background.color = "#FF9F33")
                        covsexFT[cover_sex[, j]== "No", j] = cellProperties( background.color = "#D9DADB")
                }
                
                covsexFT
                
        } else {
                
                colnames(cover_sex) <- c("Implementation Unit",
                                         "% Treated amongst females interviewed (swallowed drug)",
                                         "% Treated amongst males interviewed (swallowed drug)",
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
                        covsexFT[cover_sex[, j]== "Yes" & (sum(dat2$Females_Swallowed)/sum(dat2$Females_Offered)*100)>(sum(dat2$Males_Swallowed)/sum(dat2$Males_Offered)*100), j] = cellProperties( background.color = "#428EFC") 
                        covsexFT[cover_sex[, j]== "Yes" & (sum(dat2$Females_Swallowed)/sum(dat2$Females_Offered)*100)<(sum(dat2$Males_Swallowed)/sum(dat2$Males_Offered)*100), j] = cellProperties( background.color = "#FF9F33")
                        covsexFT[cover_sex[, j]== "No", j] = cellProperties( background.color = "#D9DADB")
                }
                
                covsexFT
        }
        
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
        
        #-----------------------------------------------------------------------#
        #Creating grouped bar plots for reach and treament for men and women    #
        #-----------------------------------------------------------------------#
        
        if (exists("Offered", where=dat2)){
                
                datreach<-data.frame(IU, (sum(dat2$Males_Offered)/sum(dat2$Males_Interviewed))*100, 
                                     (sum(dat2$Females_Offered)/sum(dat2$Females_Interviewed))*100)
                
                meltreach<-melt(datreach, id='IU')
                
        }
        
        dattreat<-data.frame(IU, (sum(dat2$Males_Swallowed)/sum(dat2$Males_Interviewed))*100, 
                             (sum(dat2$Females_Swallowed)/sum(dat2$Females_Interviewed))*100)
        
        melttreat<-melt(dattreat, id='IU')
        
        #plot
        
        if (exists("Offered", where=dat2)){
                
                reachplot<-ggplot(meltreach, aes(IU, value, color=variable,
                                                 fill=factor(variable,
                                                             labels=c("Male",
                                                                      "Female")),
                                                 width=.5))+
                        geom_bar(position = position_dodge(.5),
                                 stat="identity",colour="#000000", size=0)+
                        ggtitle("Programme Reach by Sex")+
                        ylab("Programme Reach (%)")+
                        xlab("Implementation Unit")+
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
                        scale_x_discrete(breaks = IU)+
                        scale_fill_manual(values = colors)
                
                reachplot
                
        }
        
        #plot
        
        treatplot<-ggplot(melttreat, aes(IU, value, color=variable,
                                         fill=factor(variable,
                                                     labels=c("Male",
                                                              "Female")),
                                         width=.5))+
                geom_bar(position = position_dodge(.5),
                         stat="identity",colour="#000000", size=0)+
                ggtitle("Survey Coverage by Sex")+
                ylab("Survey Coverage (%)")+
                xlab("Implementation Unit")+
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
                scale_x_discrete(breaks = IU)+
                scale_fill_manual(values = colors)
        
        treatplot
        
        if (exists("Offered", where=dat2)){
                
                doc<- addPlot(doc, fun=print, x=reachplot)
                doc<- addParagraph( doc, '', stylename = 'Normal' )
                
        }
        
        doc<-addPlot(doc, fun=print, x=treatplot)
        doc<- addParagraph( doc, '', stylename = 'Normal' )
        
        doc<- addPageBreak(doc)
        
        #-----------------------------#
        # Non-/Compliance plot by sex #
        #-----------------------------# 
        
        if (exists("Offered", where=dat2)){
                
                #Creating data for compliance plot
                
                dat_comp.conf.lim<-dat2[,c("Females_Offered","Females_Swallowed", 
                                           "Males_Offered","Males_Swallowed","Subunit")]
                
                sex.compli.df<-matrix(nrow = 2,ncol = 4)
                
                colnames(sex.compli.df)<-c("sex","compliance","lowerci","upperci")
                
                sex.compli.df[2,1]<-"Female"
                sex.compli.df[1,1]<-"Male"
                
                #Compliance
                
                sex.compli.df[2,2]<-round(100*sum(dat_comp.conf.lim$Females_Swallowed)/sum(dat_comp.conf.lim$Females_Offered),1)
                sex.compli.df[1,2]<-round(100*sum(dat_comp.conf.lim$Males_Swallowed)/sum(dat_comp.conf.lim$Males_Offered),1)
                
                #Lower CI
                
                sex.compli.df[2,3]<-unlist(scoreci(sum(dat_comp.conf.lim$Females_Swallowed),sum(dat_comp.conf.lim$Females_Offered),conf.level = .95))[1]
                sex.compli.df[1,3]<-unlist(scoreci(sum(dat_comp.conf.lim$Males_Swallowed),sum(dat_comp.conf.lim$Males_Offered),conf.level = .95))[1]
                
                #Upper CI
                
                sex.compli.df[2,4]<-unlist(scoreci(sum(dat_comp.conf.lim$Females_Swallowed),sum(dat_comp.conf.lim$Females_Offered),conf.level = .95))[2]
                sex.compli.df[1,4]<-unlist(scoreci(sum(dat_comp.conf.lim$Males_Swallowed),sum(dat_comp.conf.lim$Males_Offered),conf.level = .95))[2]
                
                sexcompli<-as.data.frame(sex.compli.df)
                sexcompli$IU<-IU
                
                #Converting the factors to numeric
                
                sexcompli$compliance<-as.numeric(levels(sexcompli$compliance))[sexcompli$compliance]
                sexcompli$lowerci<-as.numeric(levels(sexcompli$lowerci))[sexcompli$lowerci]
                sexcompli$upperci<-as.numeric(levels(sexcompli$upperci))[sexcompli$upperci]
                
                #plot        
                
                sexcompliplot<-ggplot(sexcompli, aes(IU, compliance, color=sex,
                                                     fill=factor(sex, levels = rev(levels(sex))),
                                                     width=.5))+
                        geom_bar(position = position_dodge(.5),
                                 stat="identity",colour="#000000", size=0)+
                        ggtitle("Compliance by Sex")+
                        ylab("Compliance (%)")+
                        xlab("Implementation Unit")+
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
                        scale_x_discrete(breaks = IU)+
                        scale_fill_manual(values = colors)
                
                sexcompliplot
                
                #----------------------------#
                # Non-Compliance plot by sex #
                #----------------------------# 
                
                dat_ncomp.conf.lim<-as.data.frame(dat2[,c("Males_Offered","Males_Swallowed",
                                            "Females_Offered","Females_Swallowed", 
                                            "Subunit")])
                
                sex.ncompli.df<-matrix(nrow = 2,ncol = 4)
                
                colnames(sex.ncompli.df)<-c("sex","ncompli","lowerci","upperci")
                
                sex.ncompli.df[1,1]<-"Male"
                sex.ncompli.df[2,1]<-"Female"
                
                #Creating data for Non-Compliance plot
                
                sex.ncompli.df[1,2]<-round(100*sum(dat_ncomp.conf.lim$Males_Offered-dat_ncomp.conf.lim$Males_Swallowed)/sum(dat_ncomp.conf.lim$Males_Offered),1)
                sex.ncompli.df[2,2]<-round(100*sum(dat_ncomp.conf.lim$Females_Offered-dat_ncomp.conf.lim$Females_Swallowed)/sum(dat_ncomp.conf.lim$Females_Offered),1)
                
                #Lower CI
                
                sex.ncompli.df[1,3]<-unlist(scoreci(sum(dat_ncomp.conf.lim$Males_Offered-dat_ncomp.conf.lim$Males_Swallowed),sum(dat_ncomp.conf.lim$Males_Offered),conf.level = .95))[1]*100
                sex.ncompli.df[2,3]<-unlist(scoreci(sum(dat_ncomp.conf.lim$Females_Offered-dat_ncomp.conf.lim$Females_Swallowed),sum(dat_ncomp.conf.lim$Females_Offered),conf.level = .95))[1]*100
                
                #Upper CI
                
                sex.ncompli.df[1,4]<-unlist(scoreci(sum(dat_ncomp.conf.lim$Males_Offered-dat_ncomp.conf.lim$Males_Swallowed),sum(dat_ncomp.conf.lim$Males_Offered),conf.level = .95))[2]*100
                sex.ncompli.df[2,4]<-unlist(scoreci(sum(dat_ncomp.conf.lim$Females_Offered-dat_ncomp.conf.lim$Females_Swallowed),sum(dat_ncomp.conf.lim$Females_Offered),conf.level = .95))[2]*100
                
                sexnoncompli<-as.data.frame(sex.ncompli.df)
                sexnoncompli$IU<-IU
                
                #Converting the factors to numeric
                
                sexnoncompli$ncompli<-as.numeric(levels(sexnoncompli$ncompli))[sexnoncompli$ncompli]
                sexnoncompli$lowerci<-as.numeric(levels(sexnoncompli$lowerci))[sexnoncompli$lowerci]
                sexnoncompli$upperci<-as.numeric(levels(sexnoncompli$upperci))[sexnoncompli$upperci]
                
                #Changing the values into percents
                

                
                #plot        
                
                sexnoncompliplot<-ggplot(sexnoncompli, aes(IU, ncompli, color=sex,
                                                           fill=factor(sex, levels = rev(levels(sex))),
                                                           width=.5))+
                        geom_bar(position = position_dodge(.5),
                                 stat="identity",colour="#000000", size=0)+
                        ggtitle("Non-Compliance by Sex")+
                        ylab("Non-Compliance (%)")+
                        xlab("Implementation Unit")+
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
                        scale_x_discrete(breaks = IU)+
                        scale_fill_manual(values = colors)
                
                sexnoncompliplot
                
        }
        
        if (exists("Offered", where=dat2)){
                
                doc<- addPlot(doc, fun=print, x=sexnoncompliplot)
                doc<- addParagraph( doc, '', stylename = 'Normal' )
                doc<- addPageBreak(doc)
                
        }
        
        #Sex Non-Compliance Table
        
        if (exists("Offered", where=dat2)){
                
                #Creating data for Non-Compliance plot
                
                sex.ncompli.df[1,2]<-paste(round(100*sum(dat_ncomp.conf.lim$Males_Offered-dat_ncomp.conf.lim$Males_Swallowed)/sum(dat_ncomp.conf.lim$Males_Offered),1),"%")
                sex.ncompli.df[2,2]<-paste(round(100*sum(dat_ncomp.conf.lim$Females_Offered-dat_ncomp.conf.lim$Females_Swallowed)/sum(dat_ncomp.conf.lim$Females_Offered),1),"%")
                
                #Lower CI
                
                sex.ncompli.df[1,3]<-paste(round(unlist(scoreci(sum(dat_ncomp.conf.lim$Males_Offered-dat_ncomp.conf.lim$Males_Swallowed),sum(dat_ncomp.conf.lim$Males_Offered),conf.level = .95))[1]*100,1), "%")
                sex.ncompli.df[2,3]<-paste(round(unlist(scoreci(sum(dat_ncomp.conf.lim$Females_Offered-dat_ncomp.conf.lim$Females_Swallowed),sum(dat_ncomp.conf.lim$Females_Offered),conf.level = .95))[1]*100,1),"%")
                
                #Upper CI
                
                sex.ncompli.df[1,4]<-paste(round(unlist(scoreci(sum(dat_ncomp.conf.lim$Males_Offered-dat_ncomp.conf.lim$Males_Swallowed),sum(dat_ncomp.conf.lim$Males_Offered),conf.level = .95))[2]*100,1),"%")
                sex.ncompli.df[2,4]<-paste(round(unlist(scoreci(sum(dat_ncomp.conf.lim$Females_Offered-dat_ncomp.conf.lim$Females_Swallowed),sum(dat_ncomp.conf.lim$Females_Offered),conf.level = .95))[2]*100,1),"%")
                
                sexnoncompli<-as.data.frame(sex.ncompli.df)
                sexnoncompli$IU<-IU
                
                colnames(sex.ncompli.df) <- c("Sex", "Non-Compliance",
                                                 "Lower 95% \n Confidence Interval",
                                                 "Upper 95% \n Confidence Interval")
                
                datnocompli_FT<-FlexTable(sex.ncompli.df, body.par.props = parProperties(padding= 1,
                                                                                            text.align = "center"),
                                          header.par.props = parProperties(padding = 3,
                                                                           text.align = "center"),
                                          header.text.props = textProperties(font.weight = "bold",
                                                                             font.family = "Calibri"),
                                          body.text.props = textProperties(font.family = "Calibri"))
        
                table4b<-pot(paste("Table 4. Non-Compliance in ",
                                   IU,",",country, ",", format(Sys.time(),"%Y"), "."),
                             textProperties(font.family = "Calibri",
                                            font.weight = "bold"))
                table4b
                
                doc<-addParagraph(doc, table4b)
                
                doc<-addFlexTable(doc, datnocompli_FT)
                
                doc<- addPageBreak(doc)
                
        }
        
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

analyzeData2<-function(dat2, country, IU, disease, drug, sub_num){
        
        #Creating vector that houses the colors for males and females for figures
        
        colors <- c("#FF9F33", "#428EFC")
        
        #-----------------------------------------------------------------------------------------------#
        #Creating variable thresh that will insert the WHO threshold for figures based on the NTD       # 
        #the user chose. Onchocerciasis is used as a test, but it will need to be generated from the    # 
        #-----------------------------------------------------------------------------------------------#
        
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
        
        #first add columns for number not swallowed and not offered 
        dat2$Females_Noswallow<-dat2$Females_Interviewed - dat2$Females_Swallowed
        dat2$Females_Nooffer<-dat2$Females_Interviewed - dat2$Females_Offered
        dat2$Males_Noswallow<-dat2$Males_Interviewed - dat2$Males_Swallowed
        dat2$Males_Nooffer<-dat2$Males_Interviewed - dat2$Males_Offered
        
        #now get total interviewed, offered and swallowed
        dat2$Interviewed<-dat2$Females_Interviewed + dat2$Males_Interviewed
        dat2$Offered <-dat2$Females_Offered + dat2$Males_Offered
        dat2$Swallowed<-dat2$Females_Swallowed + dat2$Males_Swallowed
        dat2$No_offer<-dat2$Females_Nooffer + dat2$Males_Nooffer
        dat2$No_swallow<-dat2$Females_Noswallow + dat2$Males_Noswallow
        
        #subset data so that we can get the program reach (#offered/total) and survey coverage (#swallow/total) separately
        dat_offer<-dat2[,c(1,8,14,16)]
        dat_swallow<-dat2[,c(1,8,15,17)]
        dat_fem_offer<-dat2[,c(1,8,4,10)]
        dat_fem_swallow<-dat2[,c(1,8,6,9)]
        dat_male_offer<-dat2[,c(1,8,5,12)]
        dat_male_swallow<-dat2[,c(1,8,7,11)]
        
        #data is currently in wide format, need to transform to long
        #there's definitely a prettier way to condense this code - but for the sake of time and simplicity I've written it all out
        
        #first do for "offered" the drug
        long_offer<-reshape(dat_offer,direction="long",idvar="Subunit",varying=c("Offered","No_offer"),
                            times=c("Offered","No_offer"),v.names="COUNT",timevar="COVERAGE")
        
        index_offer<-rep(seq_len(nrow(long_offer)),times=long_offer$COUNT)
        ds_offer<<-long_offer[index_offer,]
        
        #Now do for "swallowed" the drug
        long_swallow<-reshape(dat_swallow,direction="long",idvar="Subunit",varying=c("Swallowed","No_swallow"),
                              times=c("Swallowed","No_swallow"),v.names="COUNT",timevar="COVERAGE")
        
        index_swallow<-rep(seq_len(nrow(long_swallow)),times=long_swallow$COUNT)
        ds_swallow<<-long_swallow[index_swallow,]
        
        #Female "offered" the drug
        long_offer_f<-reshape(dat_fem_offer,direction="long",idvar="Subunit",varying=c("Females_Offered","Females_Nooffer"),
                              times=c("Offered","No_offer"),v.names="COUNT",timevar="COVERAGE")
        
        index_offer_f<-rep(seq_len(nrow(long_offer_f)),times=long_offer_f$COUNT)
        ds_offer_f<<-long_offer_f[index_offer_f,]
        
        ds_offer_f$sex<-"female"
        
        #Female "swallowed" the drug
        long_swallow_f<-reshape(dat_fem_swallow,direction="long",idvar="Subunit",varying=c("Females_Swallowed","Females_Noswallow"),
                                times=c("Swallowed","No_swallow"),v.names="COUNT",timevar="COVERAGE")
        
        index_swallow_f<-rep(seq_len(nrow(long_swallow_f)),times=long_swallow_f$COUNT)
        ds_swallow_f<<-long_swallow_f[index_swallow_f,]
        
        ds_swallow_f$sex<-"female"
        
        #Male "offered" the drug
        long_offer_m<-reshape(dat_male_offer,direction="long",idvar="Subunit",varying=c("Males_Offered","Males_Nooffer"),
                              times=c("Offered","No_offer"),v.names="COUNT",timevar="COVERAGE")
        
        index_offer_m<-rep(seq_len(nrow(long_offer_m)),times=long_offer_m$COUNT)
        ds_offer_m<<-long_offer_m[index_offer_m,]
        
        ds_offer_m$sex<-"male"
        
        #Male "swallowed" the drug 
        long_swallow_m<-reshape(dat_male_swallow,direction="long",idvar="Subunit",varying=c("Males_Swallowed","Males_Noswallow"),
                                times=c("Swallowed","No_swallow"),v.names="COUNT",timevar="COVERAGE")
        
        index_swallow_m<-rep(seq_len(nrow(long_swallow_m)),times=long_swallow_m$COUNT)
        ds_swallow_m<<-long_swallow_m[index_swallow_m,]
        
        ds_swallow_m$sex<-"male"
        
        ds_offer_combined<-rbind(ds_offer_f,ds_offer_m)
        ds_swallow_combined<-rbind(ds_swallow_f,ds_swallow_m)
        
        #now calculate the coverage for offered and swallowed
        
        
        #____________DESIORANGE OUTPUTS__________________________
        
        #Program Reach
        #Total program reach + 95% CI
        #Female program reach + 95% CI
        #Male program reach + 95% CI
        
        #Survey Coverage
        #Total survey coverage + 95% CI + DEFF  (to validate the reported coverage)
        #Female survey coverage + 95% CI
        #Male survey coverage + 95% CI
        
        #Total survey coverage + lower 1-sided CI (to compare with threshold)
        #Total survey coverage + lower 1-sided CI (to compare with threshold) *note that not powered for comparison
        #Total survey coverage + lower 1-sided CI (to compare with threshold) *note that not powered for comparison
        
        #Assumptions: we are assuming that exactly 30 clusters will always be used 
        # Strata information is retained from Coverage Survey Builder (with certainty units in their own strata) and 
        #all other subunits are formed into pseudostrata according to geographic proximity
        #Ultimate Cluster design is used, whereby no finite population corrections are entered
        #THe logit method is used to calculate CI (because it is an improvement over the Wald and can be cited in literature)
        
        
        #first calculate coverage for program reach (#offer/total)
        
        #specify survey design (one-stage cluster with implicit stratification)
        fn_program_reach<-function(ds){
                
                design_offer<-svydesign(ids=~Subunit, strata=~Strata, data=ds)
                result_offer<-svyciprop(~I(COVERAGE=="Offered"),design_offer,method="logit",level=0.95)
                ci<-as.vector(attr(result_offer,"ci"))
                upperci<-round(ci[2],4)
                lowerci<-round(ci[1],4)
                phat<-round(as.vector(result_offer),4)
                
                program_reach<-c(phat, lowerci, upperci)
                
                return(program_reach)
        }
        
        #Next calculate survey coverage 
        #return: 2-sided 95% CI (for validation with reported coverage), design effect, and lower 1-sided CI (for threshold)
        
        #specify survey design (one-stage cluster with implicit stratification)
        fn_survey_coverage<-function(ds){
                
                design_swallow<-svydesign(ids=~Subunit, strata=~Strata, data=ds)
                result_swallow<-svyciprop(~I(COVERAGE=="Swallowed"),design_swallow,method="logit",level=0.95)
                ci<-as.vector(attr(result_swallow,"ci"))
                upperci<-round(ci[2],4)
                lowerci<-round(ci[1],4)
                phat<-round(as.vector(result_swallow),4)
                #get DEFF
                DEFF<-round(as.data.frame(svymean(~factor(COVERAGE, levels=c("Swallowed","No_swallow")), design_swallow, deff="replace"))[1,3],1)
                
                #get 1-sided lower 95% limit
                result_swallow90<-svyciprop(~I(COVERAGE=="Swallowed"),design_swallow,method="logit",level=0.90)
                ci90<-as.vector(attr(result_swallow90,"ci"))
                lowerlimit<-round(ci90[1],4)
                survey_coverage<-c(phat, lowerci, upperci,DEFF,lowerlimit) 
                return(survey_coverage)
        }
        
        total_pr<-fn_program_reach(ds_offer)
        female_pr<-fn_program_reach(ds_offer_f)
        male_pr<-fn_program_reach(ds_offer_m)
        
        total_sc<-fn_survey_coverage(ds_swallow)
        female_sc<-fn_survey_coverage(ds_swallow_f)
        male_sc<-fn_survey_coverage(ds_swallow_m)
        
        #now concatenate data
        final<-as.data.frame(rbind(total_pr,female_pr,male_pr,total_sc,female_sc,male_sc))
        
        #-------------------------------------#
        #Showing Results for Programme Reach  #
        #-------------------------------------#
        
        #Reconfiguring final in order to have the variables as variables (columns) and not observations (rows)
        
        total_pr2 = c(final[1:1,1:1],final[1:1,2:2] ,final[1:1,3:3] ) 
        male_pr2 = c(final[3:3,1:1],final[3:3,2:2] ,final[3:3,3:3]) 
        female_pr2 = c(final[2:2,1:1],final[2:2,2:2] ,final[2:2,3:3]) 
        final_3 = data.frame(total_pr2, male_pr2, female_pr2)
        
        plot1<- ggplot(data=final_3*100, aes(x=c(5,15,25), y=c(total_pr2[1:1],male_pr2[1:1],female_pr2[1:1])))+
                geom_errorbar(aes(ymin=c(total_pr2[2:2],male_pr2[2:2], female_pr2[2:2]), 
                                  ymax=c(total_pr2[3:3],male_pr2[3:3], female_pr2[3:3])),
                              width=.3,
                              position=position_dodge(.9))+
                geom_point(shape=c(21,24,22),fill=c("#000000","#FF9F33","#428EFC"),
                           stat="identity",color=c("black","black","black"),size=3)+
                xlab("")+
                ylab(paste("Proportion of respondents who reported being offered \n",drug)) +
                ggtitle("Estimated Programme Reach by Gender")+
                theme(plot.title = element_text(hjust = 0.5, size=16))+
                scale_x_continuous(breaks = c(5,15,25), limits = c(0, 30), labels=c("Overall","Male","Female"))+
                theme(
                        panel.grid.major.x=element_blank(),
                        panel.grid.minor.x=element_blank(),
                        legend.position =  "bottom",
                        legend.title = element_blank(),
                        axis.title.y = element_text(face="bold", colour='black', size=10),
                        axis.text.x = element_text(face="bold", color='black', size=10),
                        plot.title = element_text(lineheight=.8, face="bold"))+
                scale_y_continuous(breaks = seq(0, 100, 10))+
                coord_cartesian(ylim=c(0,100))+
                geom_text(label=paste(format(round(total_pr2[1:1]*100,1),nsmall=1),"%"), x=3, y=total_pr2[1:1]*100)+
                geom_text(label=paste(format(round(male_pr2[1:1]*100,1),nsmall=1),"%"), x=13, y=male_pr2[1:1]*100)+
                geom_text(label=paste(format(round(female_pr2[1:1]*100,1),nsmall=1), "%"), x=23, y=female_pr2[1:1]*100)+
                scale_color_discrete(breaks="WHO \nTarget \nCoverage \nThreshold")
        
        plot1
        
        #-------------------------------------#
        #Showing Results for Survey Coverage  #
        #-------------------------------------#
        
        # Reconfiguring final in order to have the variables as variables (columns) 
        # and not observations (rows), and creating a subset of data from final for 
        # survey coverage in order for the graph to no include DEFF and lowerlimit
        
        total_sc2 = c(final[4:4,1:1],final[4:4,2:2] ,final[4:4,3:3] ) 
        male_sc2 = c(final[6:6,1:1],final[6:6,2:2] ,final[6:6,3:3]) 
        female_sc2 = c(final[5:5,1:1],final[5:5,2:2] ,final[5:5,3:3]) 
        final_2 = data.frame(total_sc2, male_sc2, female_sc2)
        
        plot2<- ggplot(data=final_2*100, aes(x=c(5,15,25), y=c(total_sc2[1:1],male_sc2[1:1],female_sc2[1:1])))+
                geom_hline(aes(yintercept=thresh, linetype="WHO \nTarget \nCoverage \nThreshold"),
                           color="red3")+
                geom_errorbar(aes(ymin=c(total_sc2[2:2],male_sc2[2:2], female_sc2[2:2]), 
                                  ymax=c(total_sc2[3:3],male_sc2[3:3],female_sc2[3:3])),
                              width=.3,
                              position=position_dodge(.9))+
                geom_point(shape=c(21,24,22),fill=c("#000000","#FF9F33","#428EFC"),
                           stat="identity",color=c("black","black","black"), size=3)+
                xlab("")+
                # geom_point(aes(5,r_coverage), color='black', size=2, shape=8)+       #Y-value will need to be pulled from the information the user enters on the homepage of the tool. Create a variable that will change the value of 90 based on this number
                # geom_text(label='\nReported \nCoverage', x=8, y=r_coverage, size=3)+   #Y-value will need to be pulled from the information the user enters on the homepage of the tool. Create a variable that will change the value of 90 based on this number
                ylab(paste("Proportion of respondents who reported swallowing \n",drug)) +
                ggtitle("Estimated Survey Coverage by Gender")+
                theme(plot.title = element_text(hjust = 0.5, size=16))+
                scale_x_continuous(breaks = c(5,15,25), limits = c(0, 30), labels=c("Overall","Male", "Female"))+
                theme(
                        panel.grid.major.x=element_blank(),
                        panel.grid.minor.x=element_blank(),
                        legend.position =  "bottom",
                        legend.title = element_blank(),
                        axis.title.y = element_text(face="bold", colour='black', size=10),
                        axis.text.x = element_text(face="bold", color='black', size=10),
                        plot.title = element_text(lineheight=.8, face="bold"))+
                scale_y_continuous(breaks = seq(0, 100, 10))+
                coord_cartesian(ylim=c(0,100))+
                geom_text(label=paste(format(round(total_sc2[1:1]*100,1),nsmall=1),"%"), x=3, y=total_sc2[1:1]*100)+
                geom_text(label=paste(format(round(male_sc2[1:1]*100,1),nsmall=1),"%"), x=13, y=male_sc2[1:1]*100)+
                geom_text(label=paste(format(round(female_sc2[1:1]*100,1),nsmall=1), "%"), x=23, y=female_sc2[1:1]*100)+
                scale_color_discrete(breaks="WHO \nTarget \nCoverage \nThreshold") #Only allows "WHO" 
        
        plot2
        
        total_sc3 = final[4:4,1:1]*100
        lower_sc3=final[4:4,2:2]*100
        upper_sc3=final[4:4,3:3]*100
        valid_sc=data.frame(total_sc3, lower_sc3, upper_sc3)
        
        thresh_sc=final[4:4, 1:1]
        thresh_lowerci=final[4:4,5:5]
        fin_thresh=data.frame(thresh_sc, thresh_lowerci)
        
        #-----------------------------------------------#
        #Showing Results for Surve Coverage by Subunit  #
        #-----------------------------------------------#
        
        ds_swallow2<-ds_swallow
        
        ds_swallow2$coverage<-ifelse(ds_swallow2$COVERAGE=="Swallowed", 1, 0)
        
        fnsurvcovsub<-function(df){
                
                design_swallow<-svydesign(ids=~1,  data=df)
                result_swallow<-svyciprop(~I(coverage==1),
                                          design_swallow, method="logit", level=0.95)
                ci<-as.vector(attr(result_swallow,"ci"))
                upci<-round(ci[2],3)
                lowci<-round(ci[1],3)
                phat<-round(as.vector(result_swallow),3)
                
                fnsurvcovsub<-c(phat, lowci, upci)
                return(fnsurvcovsub)
        }
        
        #Creating a matrix in order to store the outputs for each district
        
        output <- matrix(ncol=3, nrow=nrow(dat2))
        
        output<-data.frame(output)
        
        #for loop used to calculate the phat, 95% CI, and DEFF for each of the districts
        
        for (p in 1:nrow(dat2)){
                
                dat4covci<-subset(ds_swallow2, Subunit==p)
                
                #placing each district's output into their own row
                
                output[p,]<-fnsurvcovsub(dat4covci)
                
                print(output)
                
        }
        
        covcioutput<-data.frame(output*100)
        
        #Confidence intervals for clusters/subunits in single district analysis
        
        dat_treat.conf.lim<-dat2[,c("Interviewed","Swallowed","Subunit")]
        
        dat_treat.conf.lim$uci<-0
        dat_treat.conf.lim$lci<-0
        
        for (b in 1:sub_num){
                
                #Loops through each subunit to output the upper and lower confidence intervals
                
                dat_treat.conf.lim[b,4]<-100*as.numeric(unlist(scoreci(dat_treat.conf.lim[b,2], 
                                                                       dat_treat.conf.lim[b,1], conf.level = .95)))[2]
                dat_treat.conf.lim[b,5]<-100*as.numeric(unlist(scoreci(dat_treat.conf.lim[b,2], 
                                                                       dat_treat.conf.lim[b,1], conf.level = .95)))[1]
                
        }
        
        dat_treat.conf.lim$Treated<-100*(dat_treat.conf.lim$Swallowed/dat_treat.conf.lim$Interviewed)
        
        dat_treat.conf.lim$Subunit <- factor(dat_treat.conf.lim$Subunit, 
                                             levels = dat_treat.conf.lim$Subunit[rev(order(dat_treat.conf.lim$Treated))])
        
        
        plot5<- ggplot(data=dat_treat.conf.lim, aes(x=Subunit, y=Treated))+
                geom_hline(aes(yintercept=thresh, linetype="WHO \nTarget \nCoverage \nThreshold"),
                           color="red3")+
                geom_point(stat="identity")+
                geom_errorbar(aes(ymin=dat_treat.conf.lim$uci, ymax=dat_treat.conf.lim$lci),
                              width=.2,
                              position=position_dodge(.9))+
                xlab("Cluster")+
                ylab(paste("Proportion of respondents who reported swallowing \n",drug)) +
                ggtitle("Plot of Survey Coverage by Subunit: Greatest to Least Coverage") +
                theme(
                        plot.title = element_text(hjust = 0.5, size=12, face="bold"),
                        legend.title = element_blank(),
                        legend.position =  "bottom",
                        axis.title.y = element_text(face="bold", colour='black', size=10),
                        axis.title.x = element_text(face="bold", colour='black', size=10))+
                scale_y_continuous(breaks = seq(0, 100, 10))+
                coord_cartesian(ylim=c(0,100))
        
        plot5
        
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
        
        titlepg<-pot(paste(disease, "Coverage Evaluation Results Summary:", IU,",", country,",", format(Sys.time(), '%B %Y')),
                     textProperties(font.weight = "bold", font.family = "Calibri",
                                    font.size = 20))
        titlepg
        
        doc<-addParagraph(doc, titlepg)
        doc <- addParagraph( doc, '', stylename = 'Normal' )
        
        sumsent<- pot("This summary reviews the results from coverage evaluation surveys for ",
                      textProperties(font.family = "Calibri"))+
                pot(drug, textProperties(font.weight = "bold", font.family = "Calibri"))+
                pot(" that were conducted in ",textProperties(font.family = "Calibri"))+
                pot(IU, textProperties(font.weight = "bold", font.family = "Calibri"))+
                pot(", ", textProperties(font.weight = "bold", font.family = "Calibri"))+
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
        
        doc<-addPlot(doc, fun=print, x=plot1)
        doc<-addPlot(doc, fun=print, x=plot2)
        
        # doc<- addPageBreak(doc)
        
        #--------------------------------------------------------------------#
        # Inserting Survey coverage results by district table                #
        #--------------------------------------------------------------------#
        
        table1<-pot(paste("Table 1. Survey coverage results for",IU,",",
                          country, ",",format(Sys.time(), "%Y"),"."),
                    textProperties(font.family = "Calibri",
                                   font.weight = "bold"))
        
        doc<- addParagraph( doc, '', stylename = 'Normal' )
        
        doc<-addParagraph(doc, table1)
        
        #-----------------------------------------------#
        # Calculating programme reach for each district #
        #-----------------------------------------------#
        if (exists("Offered", where=dat2)){
                reach_dist<-round(((sum(dat2$Offered))/(sum(dat2$Offered)+sum(dat2$No_offer)))*100,2)
        }
        
        if (exists("Offered", where=dat2)){
                
                surcovDF<-data.frame(IU, paste(total_sc[1]*100,"%"),
                                     paste(total_sc[2]*100, "%"), paste(total_sc[3]*100,"%"),
                                     total_sc[4], paste(reach_dist,"%"))
                
                #Renaming the columns
                
                colnames(surcovDF) <- c("Implementation Unit", "Survey Coverage",
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
                surcovDF<-data.frame(IU, paste(total_sc[1]*100,"%"),
                                     paste(total_sc[2]*100, "%"), paste(total_sc[3]*100,"%"),
                                     total_sc[4])
                
                #Renaming the columns
                
                colnames(surcovDF) <- c("Implementation Unit","Survey Coverage",
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
        
        doc<-addFlexTable(doc, surcovFT)
        
        doc<- addPageBreak(doc)
        
        #--------------------------------------------------------------------#
        #Inserting Difference in Reported vs. Surveyed Coverage Report table #
        #--------------------------------------------------------------------#
        
        if (exists("r_coverage")){
                
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
                
                #-----------------------------------------------------------------------#
                #Making the table for differences between surveyed and reported (%)     #
                #with conditional coloring for the diff                                #
                #-----------------------------------------------------------------------#
                
                #Creating new dataframe to manipulate
                
                if (exists("r_coverage")){
                        
                        
                        diffDF<-data.frame(IU, total_sc[1]*100, r_coverage, total_sc[1]*100-r_coverage)
                        
                        #Adding interpretations for the difference beween surveyed and reported coverage
                        
                        diffDF$interp<-ifelse(total_sc[2]*100<= r_coverage & r_coverage<=total_sc[3]*100, "Validated",
                                              ifelse(abs(diffDF[,4])>=0 & abs(diffDF[,4])<=10,"Good",
                                                     ifelse (abs(diffDF[,4])>10 & abs(diffDF[,4])<= 25, "Caution", "Action Required")))
                        
                        #Renaming the columns
                        
                        colnames(diffDF) <- c("Implementation Unit", "Survey Coverage","Reported Coverage","Difference", "Interpretation")
                        
                        #Creating the flextable in order to allow for conditional colorization
                        
                        diffFT<-FlexTable(diffDF, body.par.props = parProperties(padding= 1,
                                                                                 text.align = "center"),
                                          header.par.props = parProperties(padding = 3,
                                                                           text.align = "center"),
                                          header.text.props = textProperties(font.weight = "bold",
                                                                             font.family = "Calibri"),
                                          body.text.props = textProperties(font.family = "Calibri"))
                        
                        diffFT<-addHeaderRow(diffFT, textProperties(font.family = "Calibri",
                                                                    font.weight = 'bold'),
                                             value ="Reported vs. Survey Coverage", colspan = 5,
                                             first = T,
                                             cell.properties = cellProperties(background.color = "#D4D6D8"))
                        
                        vars<-c("Difference")
                        
                        #Cutoff points were just chosen from the example output, can easily be changed
                        
                        for (i in vars) {
                                diffFT[abs(diffDF[, i]) >=0 & abs(diffDF[, i]) <= 10, ] = cellProperties( background.color = "#0DCF00" )
                                diffFT[abs(diffDF[, i]) > 10 & abs(diffDF[, i]) <= 25, ] = cellProperties( background.color = "#F7FF00" )
                                diffFT[abs(diffDF[, i]) > 25] = cellProperties( background.color = "#FF0000" )
                        }
                        
                        diffFT
                        
                }
                
                
                doc<-addFlexTable(doc, diffFT)
                
                doc<- addParagraph( doc, '', stylename = 'Normal' )
                
                #-----------------------------------------------------------------------#
                #Making the table that explains the interpretations of the RvsS data    #
                #-----------------------------------------------------------------------#
                
                Conclusion<-c("Validated", "Good", "Caution", "Action Required")
                Interpretation<-c("The reported coverage is contained within the 95% confidence interval around the survey coverage.  This means the reported coverage can be considered be “validated” in that Implementation Unit; no action or improvements are required to the reporting system.",
                                  "The reported coverage is outside the 95% confidence interval around the survey coverage but is still within ± 10 percentage points of the survey coverage, which suggests the reporting system is working well; no action or improvements are required to the reporting system.",
                                  "The reported coverage is between ± 10 to 25 percentage points different from the survey coverage.  This suggests there could be a problem with the reporting system and action may be required if resources permit.",
                                  "The reported coverage is at least ± 25 percentage points different from the survey coverage. This suggests that there is a real problem with the reported coverage and follow-up action to improve the reporting system in the Implementation Unit is required.")
                
                difkeyDF<-data.frame(Conclusion, Interpretation)
                
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
                
                doc<-addFlexTable(doc, difkeyFT)
                
                doc<- addPageBreak(doc)
        }
        
        ############################################################# 
        # Adding in the remainder of the plots from original report #
        #############################################################
        
        doc<-addPlot(doc, fun=print, x=plot5)
        
        doc<- addPageBreak(doc)
        
        #-------------------------------------------------------------------------------#
        # Calculating the p_value to see if there is a statistical difference between   #
        # females and males being offered and/or taking the drug                        #
        #-------------------------------------------------------------------------------#
        
        title4<-pot(value = "SURVEY COVERAGE BY SEX", format=title.font)
        
        doc<-addParagraph(doc, title4)
        doc<- addParagraph( doc, '', stylename = 'Normal' )
        
        if (exists("Offered", where=dat2)){
                
                table3<-pot(paste("Table 3. Programme reach and survey coverage by sex:",IU, ",",
                                  country, ",", format(Sys.time(),"%Y"), "."), 
                            textProperties(font.family = "Calibri",
                                           font.weight = "bold"))
        } else {
                table3<-pot(paste("Table 3. Survey coverage by sex:",IU,",",
                                  country, ",", format(Sys.time(),"%Y"), "."), 
                            textProperties(font.family = "Calibri",
                                           font.weight = "bold"))
        }
        
        doc<-addParagraph(doc, table3)
        
        # #Creating data frames to make matrices, in order to calculate the p_value
        # 
        # if (exists("Offered", where=dat2)){
        #         
        #         togetoff<-data.frame(IU, sum(dat2$Females_Offered), sum(dat2$Females_Nooffer), 
        #                              sum(dat2$Males_Offered), sum(dat2$Males_Nooffer))
        #         
        # }
        # 
        # togetswall<-data.frame(IU, sum(dat2$Females_Swallowed), sum(dat2$Females_Noswallow), 
        #                        sum(dat2$Males_Swallowed), sum(dat2$Males_Noswallow))
        # 
        # var3<-IU
        # 
        # #blank data frames to insert p_value into
        # 
        # if (exists("Offered", where=dat2)){
        #         
        #         chsqo<-data.frame(matrix(ncol = 1, nrow = length(var3)))
        #         
        # }
        # 
        # chsqs<-data.frame(matrix(ncol = 1, nrow = length(var3)))
        # 
        # #Loop that will output the p_value to the respective data frame
        # 
        # for (k in var3){
        #         
        #         if (exists("Offered", where=dat2)){
        #                 
        #                 mat<-matrix(unlist(togetoff[match(k, IU),2:5]), nrow = 2)
        #                 chsqo[match(k, IU),]<-chisq.test(mat)$p.value
        #                 
        #         }
        #         
        #         mat2<-matrix(unlist(togetswall[match(k, IU),2:5]), nrow = 2)
        #         chsqs[match(k, IU),]<-chisq.test(mat2)$p.value
        # }
        # 
        # #Renaming the column
        # 
        # if (exists("Offered", where=dat2)){
        #         
        #         colnames(chsqo)<-"p-value"
        #         
        # }
        # 
        # colnames(chsqs)<-"p-value"
        
        #Creating a new dataframe in order to display the 'Coverage by Sex' table
        
        if (exists("Offered", where=dat2)){
                cover_sex<-data.frame(IU, paste(round(sum(dat2$Females_Offered/sum(dat2$Females_Interviewed))*100,2), "%"), 
                                      paste(round(sum(dat2$Females_Swallowed)/sum(dat2$Females_Interviewed)*100,2), "%"), 
                                      paste(round(sum(dat2$Males_Offered/sum(dat2$Males_Interviewed))*100,2), "%"), 
                                      paste(round(sum(dat2$Males_Swallowed)/sum(dat2$Males_Interviewed)*100,2), "%"))
                
        }else{
                cover_sex<-data.frame(IU, paste(round(sum(dat2$Females_Swallowed)/sum(dat2$Females_Interviewed)*100,2), "%"),
                                      paste(round(sum(dat2$Males_Swallowed)/sum(dat2$Males_Interviewed)*100,2), "%"))
        }
        
        #Fill in cell with 'no' if not significant, otherwise, 'yes'
        
        #Obtaining the p-values for the chi-square test between the sexes for reach and treatment
        
        #Offer
        
        design_offer<-svydesign(ids=~Subunit, strata=~Strata, data=ds_offer_combined)
        
        chi.offer<-as.numeric(svychisq(~COVERAGE + sex, design_offer)[3])
        
        #Swallow
        
        design_swallow<-svydesign(ids=~Subunit, strata=~Strata, data=ds_swallow_combined)
        
        chi.swallow<-as.numeric(svychisq(~COVERAGE + sex, design_swallow)[3])
        

        
        if (exists("Offered", where=dat2)){
                cover_sex$compr_male<-ifelse(chi.offer>=.05 ,"No", "Yes")
        }
        
        cover_sex$compt_male<-ifelse(chi.swallow>=.05 ,"No", "Yes")
        
        
        
        #Rearranging columns for aesthetics
        
        if (exists("Offered", where=dat2)){
                
                cover_sex<-cover_sex[,c(1,2,4,6,3,5,7)]
                
                colnames(cover_sex) <- c("Implementation Unit", "% Reached amongst females interviewed (offered drug)",
                                         "% Reached amongst males interviewed (offered drug)",
                                         "Programme Reach for females statistically different from males",
                                         "% Treated amongst females interviewed (swallowed drug)",
                                         "% Treated amongst males interviewed (swallowed drug)",
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
                        covsexFT[cover_sex[, j]== "Yes" & (sum(dat2$Females_Offered/sum(dat2$Females_Interviewed))*100)>(sum(dat2$Males_Offered/sum(dat2$Males_Interviewed))*100), j] = cellProperties( background.color = "#428EFC")
                        covsexFT[cover_sex[, j]== "Yes" & (sum(dat2$Females_Offered/sum(dat2$Females_Interviewed))*100)<(sum(dat2$Males_Offered/sum(dat2$Males_Interviewed))*100), j] = cellProperties( background.color = "#FF9F33")
                        covsexFT[cover_sex[, j]== "Yes" & (sum(dat2$Females_Swallowed)/sum(dat2$Females_Offered)*100)>(sum(dat2$Males_Swallowed)/sum(dat2$Males_Offered)*100), j] = cellProperties( background.color = "#428EFC")
                        covsexFT[cover_sex[, j]== "Yes" & (sum(dat2$Females_Swallowed)/sum(dat2$Females_Offered)*100)<(sum(dat2$Males_Swallowed)/sum(dat2$Males_Offered)*100), j] = cellProperties( background.color = "#FF9F33")
                        covsexFT[cover_sex[, j]== "No", j] = cellProperties( background.color = "#D9DADB")
                }
                
                covsexFT
                
        } else {
                
                colnames(cover_sex) <- c("Implementation Unit",
                                         "% Treated amongst females interviewed (swallowed drug)",
                                         "% Treated amongst males interviewed (swallowed drug)",
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
                        covsexFT[cover_sex[, j]== "Yes" & (sum(dat2$Females_Swallowed)/sum(dat2$Females_Offered)*100)>(sum(dat2$Males_Swallowed)/sum(dat2$Males_Offered)*100), j] = cellProperties( background.color = "#428EFC") 
                        covsexFT[cover_sex[, j]== "Yes" & (sum(dat2$Females_Swallowed)/sum(dat2$Females_Offered)*100)<(sum(dat2$Males_Swallowed)/sum(dat2$Males_Offered)*100), j] = cellProperties( background.color = "#FF9F33")
                        covsexFT[cover_sex[, j]== "No", j] = cellProperties( background.color = "#D9DADB")
                }
                
                covsexFT
        }
        
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
        
        #-----------------------------------------------------------------------#
        #Creating grouped bar plots for reach and treament for men and women    #
        #-----------------------------------------------------------------------#
        
        if (exists("Offered", where=dat2)){
                
                datreach<-data.frame(IU, (sum(dat2$Males_Offered)/sum(dat2$Males_Interviewed))*100, 
                                     (sum(dat2$Females_Offered)/sum(dat2$Females_Interviewed))*100)
                
                meltreach<-melt(datreach, id='IU')
                
        }
        
        dattreat<-data.frame(IU, (sum(dat2$Males_Swallowed)/sum(dat2$Males_Offered))*100, 
                             (sum(dat2$Females_Swallowed)/sum(dat2$Females_Offered))*100)
        
        melttreat<-melt(dattreat, id='IU')
        
        #plot
        
        if (exists("Offered", where=dat2)){
                
                reachplot<-ggplot(meltreach, aes(IU, value, color=variable,
                                                 fill=factor(variable,
                                                             labels=c("Male",
                                                                      "Female")),
                                                 width=.5))+
                        geom_bar(position = position_dodge(.5),
                                 stat="identity",colour="#000000", size=0)+
                        ggtitle("Programme Reach by Sex")+
                        ylab("Programme Reach (%)")+
                        xlab("Implementation Unit")+
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
                        scale_x_discrete(breaks = IU)+
                        scale_fill_manual(values = colors)
                
                reachplot
                
        }
        
        #plot
        
        treatplot<-ggplot(melttreat, aes(IU, value, color=variable,
                                         fill=factor(variable,
                                                     labels=c("Male",
                                                              "Female")),
                                         width=.5))+
                geom_bar(position = position_dodge(.5),
                         stat="identity",colour="#000000", size=0)+
                ggtitle("Survey Coverage by Sex")+
                ylab("Survey Coverage (%)")+
                xlab("Implementation Unit")+
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
                scale_x_discrete(breaks = IU)+
                scale_fill_manual(values = colors)
        
        treatplot
        
        if (exists("Offered", where=dat2)){
                
                doc<- addPlot(doc, fun=print, x=reachplot)
                doc<- addParagraph( doc, '', stylename = 'Normal' )
                
        }
        
        doc<-addPlot(doc, fun=print, x=treatplot)
        doc<- addParagraph( doc, '', stylename = 'Normal' )
        
        doc<- addPageBreak(doc)
        
        #------------------------#
        # Compliance plot by sex #
        #------------------------# 
        
        if (exists("Offered", where=dat2)){
                
        #Confidence intervals for clusters/subunits in single district analysis
        
        dat_comp.conf.lim<-dat2[,c("Females_Offered","Females_Swallowed", 
                                   "Males_Offered","Males_Swallowed","Subunit")]
        
        sex.compli.df<-matrix(nrow = 2,ncol = 4)
        
        colnames(sex.compli.df)<-c("sex","compliance","lowerci","upperci")
        
        sex.compli.df[2,1]<-"female"
        sex.compli.df[1,1]<-"male"
        
        #Compliance
        
        sex.compli.df[2,2]<-round(100*sum(dat_comp.conf.lim$Females_Swallowed)/sum(dat_comp.conf.lim$Females_Offered),1)
        sex.compli.df[1,2]<-round(100*sum(dat_comp.conf.lim$Males_Swallowed)/sum(dat_comp.conf.lim$Males_Offered),1)
        
        #Lower CI
        
        sex.compli.df[2,3]<-unlist(scoreci(sum(dat_comp.conf.lim$Females_Swallowed),sum(dat_comp.conf.lim$Females_Offered),conf.level = .95))[1]
        sex.compli.df[1,3]<-unlist(scoreci(sum(dat_comp.conf.lim$Males_Swallowed),sum(dat_comp.conf.lim$Males_Offered),conf.level = .95))[1]
        
        #Upper CI
        
        sex.compli.df[2,4]<-unlist(scoreci(sum(dat_comp.conf.lim$Females_Swallowed),sum(dat_comp.conf.lim$Females_Offered),conf.level = .95))[2]
        sex.compli.df[1,4]<-unlist(scoreci(sum(dat_comp.conf.lim$Males_Swallowed),sum(dat_comp.conf.lim$Males_Offered),conf.level = .95))[2]

        sexcompli<-as.data.frame(sex.compli.df)
        sexcompli$IU<-IU
        
        #Converting the factors to numeric
        
        sexcompli$compliance<-as.numeric(levels(sexcompli$compliance))[sexcompli$compliance]
        sexcompli$lowerci<-as.numeric(levels(sexcompli$lowerci))[sexcompli$lowerci]
        sexcompli$upperci<-as.numeric(levels(sexcompli$upperci))[sexcompli$upperci]
        
        #plot        
        
                sexcompliplot<-ggplot(sexcompli, aes(IU, compliance, color=sex,
                                                                fill=factor(sex,
                                                                            labels=c("Male",
                                                                                     "Female")),
                                                                width=.5))+
                        geom_bar(position = position_dodge(.5),
                                 stat="identity",colour="#000000", size=0)+
                        ggtitle("Compliance by Sex")+
                        ylab("Compliance (%)")+
                        xlab("Implementation Unit")+
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
                        scale_x_discrete(breaks = IU)+
                        scale_fill_manual(values = colors)
                
                sexcompliplot
                
                #----------------------------#
                # Non-Compliance plot by sex #
                #----------------------------# 
                
                dat_ncomp.conf.lim<-dat2[,c("Females_Offered","Females_Noswallow", 
                                            "Males_Offered","Males_Noswallow","Subunit")]
                
                sex.ncompli.df<-matrix(nrow = 2,ncol = 4)
                
                colnames(sex.ncompli.df)<-c("sex","ncompli","lowerci","upperci")
                
                sex.ncompli.df[2,1]<-"female"
                sex.ncompli.df[1,1]<-"male"
                
                #Creating data for Non-Compliance plot
                
                sex.ncompli.df[2,2]<-round(100*sum(dat_ncomp.conf.lim$Females_Noswallow)/sum(dat_ncomp.conf.lim$Females_Offered),1)
                sex.ncompli.df[1,2]<-round(100*sum(dat_ncomp.conf.lim$Males_Noswallow)/sum(dat_ncomp.conf.lim$Males_Offered),1)
                
                #Lower CI
                
                sex.ncompli.df[2,3]<-unlist(scoreci(sum(dat_ncomp.conf.lim$Females_Noswallow),sum(dat_ncomp.conf.lim$Females_Offered),conf.level = .95))[1]*100
                sex.ncompli.df[1,3]<-unlist(scoreci(sum(dat_ncomp.conf.lim$Males_Noswallow),sum(dat_ncomp.conf.lim$Males_Offered),conf.level = .95))[1]*100
                
                #Upper CI
                
                sex.ncompli.df[2,4]<-unlist(scoreci(sum(dat_ncomp.conf.lim$Females_Noswallow),sum(dat_ncomp.conf.lim$Females_Offered),conf.level = .95))[2]*100
                sex.ncompli.df[1,4]<-unlist(scoreci(sum(dat_ncomp.conf.lim$Males_Noswallow),sum(dat_ncomp.conf.lim$Males_Offered),conf.level = .95))[2]*100
                
                sexnoncompli<-as.data.frame(sex.ncompli.df)
                sexnoncompli$IU<-IU
                
                #Converting the factors to numeric
                
                sexnoncompli$ncompli<-as.numeric(levels(sexnoncompli$ncompli))[sexnoncompli$ncompli]
                sexnoncompli$lowerci<-as.numeric(levels(sexnoncompli$lowerci))[sexnoncompli$lowerci]
                sexnoncompli$upperci<-as.numeric(levels(sexnoncompli$upperci))[sexnoncompli$upperci]
                
                #plot        
                
                sexnoncompliplot<-ggplot(sexnoncompli, aes(IU, ncompli, color=sex,
                                                           fill=factor(sex,
                                                                       labels=c("Male",
                                                                                "Female")),
                                                           width=.5))+
                        geom_bar(position = position_dodge(.5),
                                 stat="identity",colour="#000000", size=0)+
                        ggtitle("Non-Compliance by Sex")+
                        ylab("Non-Compliance (%)")+
                        xlab("Implementation Unit")+
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
                        scale_x_discrete(breaks = IU)+
                        scale_fill_manual(values = colors)
                
                sexnoncompliplot
                
        }
        
        if (exists("Offered", where=dat2)){
                
                doc<- addPlot(doc, fun=print, x=sexnoncompliplot)
                doc<- addParagraph( doc, '', stylename = 'Normal' )
                doc<- addPageBreak(doc)
                
        }
        
        #Sex Non-Compliance Table
        
        if (exists("Offered", where=dat2)){
                
                #Creating data for Non-Compliance plot
                
                sex.ncompli.df[1,2]<-paste(round(100*sum(dat_ncomp.conf.lim$Males_Offered-dat_ncomp.conf.lim$Males_Swallowed)/sum(dat_ncomp.conf.lim$Males_Offered),1),"%")
                sex.ncompli.df[2,2]<-paste(round(100*sum(dat_ncomp.conf.lim$Females_Offered-dat_ncomp.conf.lim$Females_Swallowed)/sum(dat_ncomp.conf.lim$Females_Offered),1),"%")
                
                #Lower CI
                
                sex.ncompli.df[1,3]<-paste(round(unlist(scoreci(sum(dat_ncomp.conf.lim$Males_Offered-dat_ncomp.conf.lim$Males_Swallowed),sum(dat_ncomp.conf.lim$Males_Offered),conf.level = .95))[1]*100,1), "%")
                sex.ncompli.df[2,3]<-paste(round(unlist(scoreci(sum(dat_ncomp.conf.lim$Females_Offered-dat_ncomp.conf.lim$Females_Swallowed),sum(dat_ncomp.conf.lim$Females_Offered),conf.level = .95))[1]*100,1),"%")
                
                #Upper CI
                
                sex.ncompli.df[1,4]<-paste(round(unlist(scoreci(sum(dat_ncomp.conf.lim$Males_Offered-dat_ncomp.conf.lim$Males_Swallowed),sum(dat_ncomp.conf.lim$Males_Offered),conf.level = .95))[2]*100,1),"%")
                sex.ncompli.df[2,4]<-paste(round(unlist(scoreci(sum(dat_ncomp.conf.lim$Females_Offered-dat_ncomp.conf.lim$Females_Swallowed),sum(dat_ncomp.conf.lim$Females_Offered),conf.level = .95))[2]*100,1),"%")
                
                sexnoncompli<-as.data.frame(sex.ncompli.df)
                sexnoncompli$IU<-IU
                
                colnames(sex.ncompli.df) <- c("Sex", "Non-Compliance",
                                              "Lower 95% \n Confidence Interval",
                                              "Upper 95% \n Confidence Interval")
                
                datnocompli_FT<-FlexTable(sex.ncompli.df, body.par.props = parProperties(padding= 1,
                                                                                         text.align = "center"),
                                          header.par.props = parProperties(padding = 3,
                                                                           text.align = "center"),
                                          header.text.props = textProperties(font.weight = "bold",
                                                                             font.family = "Calibri"),
                                          body.text.props = textProperties(font.family = "Calibri"))
                
                table4b<-pot(paste("Table 4. Non-Compliance in ",
                                   IU,",",country, ",", format(Sys.time(),"%Y"), "."),
                             textProperties(font.family = "Calibri",
                                            font.weight = "bold"))
                table4b
                
                doc<-addParagraph(doc, table4b)
                
                doc<-addFlexTable(doc, datnocompli_FT)
                
                doc<- addPageBreak(doc)
                
        }
        
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