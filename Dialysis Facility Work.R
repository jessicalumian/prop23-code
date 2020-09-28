## Need to subset the facility data by showing only CA dialysis facility centres
## 

View(DialysisFacilityCompare)

DialysisFacilityCompare$State

install.packages("dplyr")
library(tidyverse)
library(ggplot2)
install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
library("ggmap")

## Want to create a subset of the dataset that only shows CA Dialysis Facility Centres, given that the dataset shows for all states
## Also, subsetting this data set with columns that are relevant, such as 
CA_Facility = subset(DialysisFacilityCompare, State == "CA", select = c("State", "Facility Name", "Five Star", "City", "County", "Profit or Non-Profit", "# of Dialysis Stations", "Percentage of Medicare patients with Hgb<10 g/dL", "Percentage of Medicare patients with Hgb>12 g/dL", "Percent of Adult HD patients with Kt/V >= 1.2", "Percentage of Adult PD PTS with Kt/V >= 1.7","Percentage of Pediatric HD patients with Kt/V >= 1.2", "Percentage of Adult patients with hypercalcemia (serum calcium greater than 10.2 mg/dL)","Percentage of Adult patients with serum phosphorus less than 3.5 mg/dL", "Percentage of Adult patients with serum phosphorus between 3.5-4.5 mg/dL", "Percentage of Adult patients with serum phosphorus between 4.6-5.5 mg/dL", "Percentage of Adult patients with serum phosphorus between 5.6-7.0 mg/dL", "Percentage of Adult patients with serum phosphorus greater than 7.0 mg/dL", "Patient Hospitalization category text", "Patient Hospital Readmission Category", "Patient Survival Category Text", 'Number of patients included in hospitalization summary', "Number of hospitalizations included in hospital readmission summary", "Number of patients included in survival summary", "Mortality Rate (Facility)", "Readmission Rate (Facility)", 'Hospitalization Rate (Facility)', "Patient Infection category text", "Transfusion Rate (Facility)", "Fistula Rate (Facility)", 'Percentage of Adult patients with long term catheter in use', "SWR category text", "PPPW category text", "Percentage of Prevalent Patients Waitlisted"))

View(CA_Facility)

## An issue with this data set is that the columns are all characters, need to change them into factors so I can split the data accordingly
class("Profit or Non-Profit")
profit_updated = as.factor("Profit or Non-Profit")
class("Profit or Non-Profit")

str(CA_Facility) ## all columns are characters, need to be updated to factors 
CA_Facility_Factored = as.data.frame(unclass(CA_Facility))
str(CA_Facility_Factored) ## Now returns the structure of the data set to be factored, so will make splitting the data in accordance with factor levels a lot easier

class(CA_Facility_Factored$Profit.or.Non.Profit) ## Now returns as a factor

## Questions I want to understand and what I'm going to do to understand them:
## 1. Quality of care (according to five star-rating) in profit and non profit: Will create a chart with fator levels of star rating 
    ## Also want to create a map to understand where these centres are specifically located
## 2. Hospitalization rates, mortality rates, re-admission rates across profits and non-profits; these factors can be indicative of the nature of the service being provided
    ## Also want to understand this in relation to quality of care based off of the five-star survey
## 3.  Comparison of # of dialysis stations within profits and non-profits 
    ##    Does # of dialysis stations within the facility impact quality of care? And does there seem to be associations/correlations in mortality rates, re-admission rates, hospitalization rates
    ##    and the percentage of patients wait-listed?

## 1. Quality of Care in relation to Profit and Non-Profit

split(CA_Facility_Factored$Profit.or.Non.Profit, CA_Facility_Factored$Five.Star) ## shows number of ratings in accordance with the 

## To remove the missing values of "Not Available":
CA_Facility_Factored.clean = subset(CA_Facility_Factored, Five.Star != "Not Available")
CA_Facility_Factored.clean ## this will now plot the data having removed the 'not available' data of the surveys to create a cleaner bar plot

## Creating bar plot to show the number of five star ratings in accordance with 
ggplot(CA_Facility_Factored.clean, aes(x = CA_Facility_Factored.clean$Profit.or.Non.Profit)) + geom_bar(aes(fill = CA_Facility_Factored.clean$Five.Star))
## adding appropriate labels, space apart, etc.

## We can see that quality of care in profits tends to be mostly be 3, 4, 5 

library(ggmap)

## 2. Rates across profits and non-profits
## Need to convert the appropriate columns to numerical columns first

CA_Facility_Factored$Mortality.Rate..Facility. <- as.numeric(as.character(CA_Facility_Factored$Mortality.Rate..Facility.))
class(CA_Facility_Factored$Mortality.Rate..Facility.)
CA_Facility_Factored$Hospitalization.Rate..Facility. <- as.numeric(as.character(CA_Facility_Factored$Hospitalization.Rate..Facility.))
CA_Facility_Factored$Readmission.Rate..Facility. <- as.numeric(as.character(CA_Facility_Factored$Readmission.Rate..Facility.))

str(CA_Facility_Factored)
colnames(CA_Facility_Factored)

Mortality_PNP <- ggplot(CA_Facility_Factored) +  aes(CA_Facility_Factored$Profit.or.Non.Profit, CA_Facility_Factored$Mortality.Rate..Facility.) 
Mortality_PNP
Mortality_PNP + geom_jitter()

Hospitalisation_PNP <- ggplot(CA_Facility_Factored) +  aes(CA_Facility_Factored$Profit.or.Non.Profit, CA_Facility_Factored$Hospitalization.Rate..Facility.) 
Hospitalisation_PNP + geom_jitter()
## 

Readmission_PNP <- ggplot(CA_Facility_Factored) +  aes(CA_Facility_Factored$Profit.or.Non.Profit, CA_Facility_Factored$Readmission.Rate..Facility.) 
Readmission_PNP + geom_jitter()

Readmission_PNP + geom_jitter(aes(colour = CA_Facility_Factored$Five.Star)) ## This shows non-profits vs profits Re-admission rates, coloured by Five-Star rating to answer
## whether centres with higher ratings have lower re-admission rates 
