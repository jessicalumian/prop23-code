# ****************************************************
# ==== U.S. Chronic Disease Indicators (CDI) data ====
# ****************************************************

# ==== MAKE SURE TO EDIT CODE BASED ON DATA FILES USED ====
# - Need to save files of datasets to the Prop 23 Github Repo (depending on your data extraction method)
# - Edit code and directories based on where files are stored on Github Repo
# - If able to connect to API edit your code to account for this
# - DOCUMENT YOUR WORK FLOW HERE
# - Clean you code for legibility and "reproducibility"
# ^ ==== CONTINUE HERE ====

# URL: https://healthdata.gov/dataset/us-chronic-disease-indicators-cdi
# Data Dictionary: https://chronicdata.cdc.gov/Chronic-Disease-Indicators/U-S-Chronic-Disease-Indicators-CDI-/g4ie-h725
# Diabetes & CKD connection: https://www.cdc.gov/kidneydisease/prevention-risk/make-the-connection.html

library(tidyverse) # For data wrangling w/ dplyr and ggplot2 data viz
library(jsonlite) # For API connection bc data is in JSON format
library(httr) # For API connection; GET()
library(skimr) # For elegant data summaries
library(RSocrata) # To retrieve Socrata dataset as R object


# ==== API Connection ====

# Try to connect to CDI API using JSON url
#CDI <- fromJSON(txt = )
# ^ Fix issue w/ pulling only 1000 observations

# *** TRY TO CONNECT TO API THRU DATA DICTIONARY URL ***


# ==== DATA WRANGLING & EXPLORATION ====

# Import CDI csv file locally
#US_CDI <- read_csv("U.S._Chronic_Disease_Indicators__CDI_.csv")

# Import CDI data as an R data frame using Socrata method 
# CDI is comes from a Socrata Open Data API (SODA)
US_CDI <- read.socrata(url = "https://chronicdata.cdc.gov/resource/g4ie-h725.json")
# ^ This method takes time to run bc very large dataset? (800K plus observations)


#View(US_CDI) # View raw CDI data in spreadsheet format
#skim_without_charts(US_CDI) # Use skim to create data summary
#glimpse(US_CDI) # Preview the object's structure
# May have to change "YearStart" and "YearEnd" variables to date data type

# Check the unique chronic disease health topic values in the dataset
#unique(US_CDI$Topic)

# Obtain California data on Diabetes, Cardiovascular, & Chronic Kidney Disease (CKD)
# Arrange data by Topic (A-Z), YearStart (descending = most recent), YearEnd (descend)
US_CDI_CA <- US_CDI %>%
  filter(LocationDesc == "California" & Topic %in% c("Diabetes", "Cardiovascular Disease",
                                                     "Chronic Kidney Disease")) %>%
  arrange(Topic, desc(YearStart), desc(YearEnd))

#View(US_CDI_CA) # View CA data in spreadsheet format
#skim_without_charts(US_CDI_CA) # Create data summary
#glimpse(US_CDI_CA) # Preview object's structure

# Check unique values of "Question" variable for Cardiovascular Disease observations
US_CDI_CA %>% filter(Topic == "Cardiovascular Disease") %>% distinct(Question)
# ^ Not much data related to hypertension / high blood pressure -> Focus on Diabetes and CKD data?

# Check unique values of "Question" variable for Diabetes observations
US_CDI_CA %>% filter(Topic == "Diabetes") %>% distinct(Question)
# ^ Diabetes Prevalence Data from CA Data Portal may suffice for Diabetes data (See CA_Diabetes.R)

# Check unique values of "Question" variable for CKD observations
US_CDI_CA %>% filter(Topic == "Chronic Kidney Disease") %>% distinct(Question)
# ^ Create graphs conveying "Prevalence of chronic kidney disease among adults aged >= 18 years",
# "Incidence of treated end-stage renal disease attributed to diabetes"
# "Incidence of treated end-stage renal disease" (ESRD)


# Create object containing CKD data in CA only filtered by questions of interest 
# and grab OVERALL rates only
US_CDI_CA_CKD <- US_CDI_CA %>%
  filter(Topic == "Chronic Kidney Disease" & 
           Question %in% c("Prevalence of chronic kidney disease among adults aged >= 18 years",
                           "Incidence of treated end-stage renal disease attributed to diabetes",
                           "Incidence of treated end-stage renal disease") &
           StratificationCategory1 == "Overall") %>% 
  arrange(desc(Question), DataValueType)

# View data in spreadsheet format
#View(US_CDI_CA_CKD)
#glimpse(US_CDI_CA_CKD) # Get glimpse of object's structure



# ==== CKD PREVALENCE ====

# ***Create a bar chart of OVERALL adult CKD prevalence over time***
# Facet by DataValueType (i.e. Crude Prevalence & Age-adjusted prevalence)
# ggplot(data = US_CDI_CA_CKD %>% 
#          filter(Question == "Prevalence of chronic kidney disease among adults aged >= 18 years")) +
#   geom_col(mapping = aes(x = as_factor(YearEnd), y = DataValue), fill = "dodgerblue", color = "black") +
#   facet_wrap(facets = ~DataValueType, ncol = 2) +
#   theme_bw() +
#   labs(x = "Year", y = "Percent (%)", 
#        title = "Chronic Kidney Disease Prevalence among adults in California aged 18 years or older, 2011-2018",
#        caption = "Centers for Disease Control and Prevention, 
#        National Center for Chronic Disease Prevention and Health Promotion, 
#        Division of Population Health")
# Add subtitle for commentary and caption to cite data source
# - Need to study dataset more for better interpretation of Age-adjusted prevalence
#   - Likely not needed for main idea: convey general CKD prevalence in CA in recent time


# Column chart of OVERALL adult CKD CRUDE prevalence in CA
CKD.crudeOVR <- ggplot(data = US_CDI_CA_CKD %>%
                         filter(Question == "Prevalence of chronic kidney disease among adults aged >= 18 years" &
                                  DataValueType == "Crude Prevalence")) +
  geom_col(mapping = aes(x = as_factor(YearEnd), y = DataValue), 
           fill = "dodgerblue", color = "black")

# Layer additional aesthetics onto CKD prevalence plot
CKD.crudeOVR <- CKD.crudeOVR  +
  geom_text(mapping = aes(x = as_factor(YearEnd), y = DataValue, label = DataValue), vjust = -0.5) +
  theme_bw() +
  labs(x = "Year", y = "Percent (%)", 
       title = "Chronic Kidney Disease Prevalence among adults in California aged 18 years or older, 2011-2018",
       subtitle = "Data depicts overall crude prevalence rates",
       caption = "Centers for Disease Control and Prevention,
       National Center for Chronic Disease Prevention and Health Promotion,
       Division of Population Health")
# ^ *** KEEP PLOT ***
# - If prevalence here is measured as %, is the prevalence interpreted as
#   "Cases per 100 adults" ???
# ^ ==== TRY TO VERIFY THIS ====

# Preview CKD column chart; Determine how to interpret data (percent for prevalence rates?)
CKD.crudeOVR



# ==== ESRD TREATMENT INCIDENCE ====

# ESRD = End-stage Renal Disease

# ***Create bar chart of OVERALL Incidence of treated end-stage renal disease***
# Facet by DataValueType for crude vs adjusted rates; Assuming "Number" observations is Crude prevalence
# Cases are per 1,000,000
# ggplot(data = US_CDI_CA_CKD %>% filter(Question == "Incidence of treated end-stage renal disease")) +
#   geom_col(mapping = aes(x = as_factor(YearEnd), y = DataValue)) +
#   facet_wrap(facets = ~DataValueType, nrow = 2) +
#   theme_bw() +
#   labs(x = "Year", y = "Cases per 100,000,000", 
#        title = "Incidence of treated end-stage renal disease in California, 2010-2015")
# *** Create a plot of just adjusted data and zoom in to see trends ***

# Bar chart of OVERALL ESRD treatment age-adjusted rates
# ggplot(data = US_CDI_CA_CKD %>% 
#          filter(Question == "Incidence of treated end-stage renal disease" &
#                   DataValueType == "Adjusted by age, sex, race and ethnicity")) +
#   geom_col(mapping = aes(x = as_factor(YearEnd), y = DataValue)) +
#   theme_bw() +
#   labs(x = "Year", y = "Cases per 100,000,000", 
#        title = "Incidence of treated end-stage renal disease in California, 2010-2015",
#        subtitle = "Incidence Rates are adjusted by age, sex, and race/ethnicity",
#        caption = "Data provided by: Centers for Disease Control and Prevention,
#        National Center for Chronic Disease Prevention and Health Promotion,
#        Division of Population Health")
# ^ SHOWING ADJUSTED RATES MAY BE SUPERFLUOUS AND COULD TAKE AWAY FROM MAIN IDEA
# Would need to study dataset more for insightful commentary on adjusted rates


# Bar chart of OVERALL ESRD treatment incidence CRUDE rates
ESRD.incid <- ggplot(data = US_CDI_CA_CKD %>% 
         filter(Question == "Incidence of treated end-stage renal disease" & 
                  DataValueType == "Number")) +
  geom_col(mapping = aes(x = as_factor(YearEnd), y = DataValue), 
           fill = "firebrick", color = "black") +
  geom_text(mapping = aes(x = as_factor(YearEnd), y = DataValue, label = DataValue), vjust = -0.5) +
  theme_bw() +
  labs(x = "Year", y = "Cases per 100,000,000", 
       title = "Overall incidence of treated end-stage renal disease in California, 2010-2015",
       subtitle = "Data depicts crude incidence rates",
       caption = "Centers for Disease Control and Prevention,
       National Center for Chronic Disease Prevention and Health Promotion,
       Division of Population Health")
# ^ *** KEEP PLOT ***

# View ESRD treament incidence plot
ESRD.incid


# ==== ESRD TREATMENT INCIDENCE ATTRIBUTED TO DIABETES ====

# ***Create bar chart of OVERALL incidence rates of treated end-stage renal disease attributed to diabetes***
# Facet by DataValueType for crude vs adjusted rates; Assuming "Number" observations is Crude prevalence
# Cases are per 1,000,000
# ggplot(data = US_CDI_CA_CKD %>% 
#          filter(Question == "Incidence of treated end-stage renal disease attributed to diabetes")) +
#   geom_col(mapping = aes(x = as_factor(YearEnd), y = DataValue)) +
#   facet_wrap(facets = ~DataValueType, nrow = 2) +
#   theme_bw() +
#   labs(x = "Year", y = "Cases per 100,000,000", 
#        title = "Incidence of treated end-stage renal disease attributed to diabetes, 2010-2015")
# *** Create a plot of just adjusted data and zoom in to see trends ***


# Column chart of OVERALL incidence rates of treated end-stage renal disease attributed to diabetes
ESRD.diab.incid <- ggplot(data = US_CDI_CA_CKD %>%
         filter(Question == "Incidence of treated end-stage renal disease attributed to diabetes" &
                  DataValueType == "Number")) +
  geom_col(mapping = aes(x = as_factor(YearEnd), y = DataValue), 
           fill = "firebrick", color = "black") +
  geom_text(mapping = aes(x = as_factor(YearEnd), y = DataValue, label = DataValue), vjust = -0.5) +
  theme_bw() +
  labs(x = "Year", y = "Cases per 100,000,000", 
       title = "Overall incidence of treated end-stage renal disease attributed to diabetes, 2010-2015",
       subtitle = "Data depicts crude incidence rates",
       caption = "Centers for Disease Control and Prevention,
       National Center for Chronic Disease Prevention and Health Promotion,
       Division of Population Health")
# ^ *** KEEP PLOT *** 

# View ESRD treatment incidence attributed to diabetes
ESRD.diab.incid


# ==== COMBINED PLOTS ====

# Combine plots onto a single page based on dataset and topic
# CKD and ESRD plots
#gridExtra::grid.arrange(CKD.crudeOVR, ESRD.incid, ESRD.diab.incid, ncol = 3)
# ^ Will need to edit individual plots if you place them all on one page
