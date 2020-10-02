# *****************************************
# ==== CA Adults with Diabetes Per 100 ====
# *****************************************

# ==== MAKE SURE TO EDIT CODE BASED ON DATA FILES USED ====
# - Need to save files of datasets to the Prop 23 Github Repo (depending on your data extraction method)
# - Edit code and directories based on where files are stored on Github Repo
# - If able to connect to API edit your code to account for this
# - DOCUMENT YOUR WORK FLOW HERE
# - Clean you code for legibility and "reproducibility"
# ^ ==== CONTINUE HERE ====


# ==== GOAL(S) & IMPORTANT INFO ====

# - Create a data vizualization that highlights Diabetes Prevalence rates in CA
#   - Convey prevalence of diabetes as it relates to Dialysis clinics and kidney care
#   - Communicate prevalence rates according to specific strata
#   - Compliments Dialysis Clinic map made by Andrew and Gloria

# 1.  This prevalence rate does not include pre-diabetes, or gestational diabetes. 
# 2.  This is based on the question: "Has a doctor, or nurse or other health professional ever told you that you have diabetes?" 
# 3.  The sample size for 2014 was 8,832. NOTE: Denominator data and weighting was taken from the California Department of Finance, not U.S. Census. 
#       - Values may therefore differ from what has been published in the national BRFSS data tables by the Centers for Disease Control and Prevention (CDC) or other federal agencies.

# Statewide data of Adults (>= 18 y.o.) with Diabetes per 100

# SOURCE OF DATASET USED: https://data.ca.gov/dataset/adults-with-diabetes-per-100-lghc-indicator
# Let's Get Healthy California (LGHC): https://letsgethealthy.ca.gov/goals/living-well/decreasing-diabetes-prevalence/
# - Good source for background information on Diabetes in CA
# - Use website to find a reference point for the prevalence rates
# - DATA IS PRESENTED IN UNITS OF COUNTS, NOT PERCENTAGES
#   - Interpret Prevalence rates as counts per 100 adults



# ==== WORKFLOW / PROCESS DOCUMENTATION ====

# - Data was obtained from California Open Data Portal: https://data.ca.gov/dataset/adults-with-diabetes-per-100-lghc-indicator
#   - Data originally used for Let's Get Healthy California Diabetes Indicator
#   - Data was collected by the California Behavioral Risk Factor Surveillance Survey (BRFSS)

# - Data was downloaded as a CSV file and stored locally on Earl's device
#   - Raw data did not require data cleaning
#   - Dataset was broken down according to Diabetes Prevalence topic and strata
#     - 



# ==== DATA WRANGLING AND EXPLORATION ==== 

library(tidyverse) # For data wrangling w/ dplyr, importing data w/ readr, & viz w/ ggplot2
library(jsonlite) # For API Connection if data is in JSON format
library(httr) # For API connection; GET()
library(skimr) # For elegant data summary
#library(plotly) # For interactive plots; useful for Shiny dashboard

# Request and Connect to Data API <- *TRY CONNECTING TO API AFTER CREATING DATA VIZ
# result <- GET(url = "https://data.ca.gov/api/3/action/datastore_search")
# result

# Import LGCH diabetes data locally
# ^ *** IF UNABLE TO REQUEST DATA API, PUSH DATASET TO Jessica's Github repo and adapt code ***
adults_diabetes_per_100 <- read_csv("adults-with-diabetes-per-100-lghc-indicator-23.csv")
#View(adults_diabetes_per_100) # View the imported diabetes CSV file in spreadsheet format

# Obtain dataset's structure and produce summary of data
glimpse(adults_diabetes_per_100)
skim_without_charts(adults_diabetes_per_100)
# Will need to change "Year" variable/column to date data type, or convert to factor for data viz

# library(lubridate) # Package for handling Date values
# 
# # Change "Year" variable from numeric/integer (i.e. double value) to 
# adults_diabetes_per_100$Year <- year(adults_diabetes_per_100$Year)


#################################
# ==== DATA VISUALIZATIONS ==== #
#################################

# 1. Plot prevalence rates in total population
# 2. Prevalence by Age and Year
# 3. Prevalence by Income and Year




# ==== Diabetes Prevalence Rates in Total Population over time ==== 

# Create a dataframe containing Total Population data only
diabetes_TotPop <- adults_diabetes_per_100 %>%
   filter(Strata == "Total population", `Strata Name` == "Total population")

#glimpse(diabetes_TotPop) # Check if data filtering worked

# Create column chart of rates over time w/ ggplot2
Tot.Prev <- ggplot(data = diabetes_TotPop) +
  geom_col(mapping = aes(x = as_factor(Year), y = Percent), fill = "firebrick", color = "black")

# Add additional aesthetics layers 
# (i.e. text of data points, line for LGHC target, titles, labels, captions)
Tot.Prev <- Tot.Prev + 
  geom_text(mapping = aes(x = as_factor(Year), y = Percent, label = Percent), vjust = -0.5,
                      position = position_dodge(width = 1)) +
  geom_hline(yintercept = 7.0) + # Add line at LGHC target diabetes prevalence rate for CA adult population
  geom_label(mapping = aes(x = 3, y = 7.0, label = "LGHC Target: 7.0"), color = "black") + # Add text label to line
  theme_bw() +
  labs(x = "Year", y = "Cases per 100 adults", 
      title = "Diabetes Prevalence among total population of adults in California, 2012-2018",
      subtitle = "CA has the most new cases of diabetes in the US according to Let's Get Health California Task Force",
      caption = "Data Sourced from CA Open Data Portal: https://data.ca.gov")
# ^ *** KEEP PLOT *** 

# View Diabetes Prevalence in Total Population
Tot.Prev

# ** Unused code for error bars **
#geom_errorbar(mapping = aes(x = as_factor(Year), y = Percent, ymin = `Lower 95% CL`, 
#                            ymax = `Upper 95% CL`), width = 0.2) +

# Create column chart of rates over time w/ plotly
# diabetes_TotPop %>%
#   plot_ly(x = ~as_factor(Year), y = ~Percent, type = "bar", 
#           error_y = list(array = ~`Standard Error`, color = "black")) # <- first arg in 
# array = ~c(`Lower 95% CL`, `Upper 95% CL`) yields extremely long error bars s.t. lower CL value is used
# ^ ** UNUSED CODE FOR PLOTLY GRAPH **



# ==== Diabetes Prevalence Rates by Age Group and Year ==== 

# Create dataframe containing Age Group strata only
diabetes_Age <- adults_diabetes_per_100 %>% filter(Strata == "Age")
# glimpse(diabetes_Age) # Quick glimpse of dataframe structure

# Column chart of diabetes prevalence by year and age group in GGPlot2
Age.Prev <- ggplot(data = diabetes_Age) +
  geom_col(mapping = aes(x = as_factor(Year), y = Percent, fill = `Strata Name`), 
           position = "dodge", color = "black") +
  scale_fill_brewer(palette = "Blues") + # Consistent coloring scheme of bars based on Age group
  theme_bw() +
  labs(x = "Year", y = "Cases per 100 adults", 
       title = "Diabetes Prevalence among adults in California by Age Group, 2012-2018",
       subtitle = "Rates of diabetes grow as age increases",
       caption = "Sourced from CA Open Data Portal: https://data.ca.gov",
       fill = "Age Group")
# ^ *** KEEP PLOT *** 

# Preview diabetes prevalence by age group
Age.Prev


# Column chart of diabetes prevalence by year and age group using Plotly
# diabetes_Age %>%
#   plot_ly(x = ~as_factor(Year), y = ~Percent, type = "bar", color = ~`Strata Name`) %>%
#   layout(xaxis = list(title = "Year", yaxis = list(title = "Percent per 100")))



# ==== Diabetes Prevalence Rates by Income Strata and Year ==== 

# Create a dataframe containing only Income data
diabetes_Inc <- adults_diabetes_per_100 %>% filter(`Strata` == "Income")
#glimpse(diabetes_Inc) # Get a glimpse of dataframe's structure

# Column chart of diabetes prevalence by Income and Year using ggplot2
Inc.Prev <- ggplot(data = diabetes_Inc) +
  geom_col(mapping = aes(x = as_factor(Year), y = Percent, fill = as_factor(`Strata Name`)), 
           position = "dodge", color = "black") +
  scale_fill_brewer(palette = "Greens") + # Create consistent coloring scheme by Income Strata
  theme_bw() +
  labs(x = "Year", y = "Cases per 100 adults", 
       title = "Diabetes Prevalence among adults in California by Income, 2012-2018",
       subtitle = "Individuals with less income tend to have a greater burden of diabetes",
       caption = "Sourced from CA Open Data Portal: https://data.ca.gov",
       fill = "Income Range")
# ^ *** KEEP PLOT *** 

# View Diabetes X Income plot
Inc.Prev
# Determine how income is measured (median household income? individual income?)

# Column chart of diabetes prevalence by year and age group using Plotly
# diabetes_Inc %>%
#   plot_ly(x = ~as_factor(Year), y = ~Percent, type = "bar", color = ~`Strata Name`) %>%
#   layout(xaxis = list(title = "Year", yaxis = list(title = "Percent per 100")))
# ^ try to add accurate error bars if possible



# ==== COMBINED PLOTS ====

# Combine plots onto a single page based on dataset and topic; use grid.arrange from grid.Extra
#gridExtra::grid.arrange(Tot.Prev, Age.Prev, Inc.Prev, ncol = 3)
# ^ Will need to edit individual plots if you place them all on one page



# ==== Diabetes Prevalence Rates by Race/Ethnicity and Year ==== 

# Create a dataframe filtered by Race/Ethnicity
# diabetes_RaceEth <- adults_diabetes_per_100 %>%
#   filter(Strata == "Race-Ethnicity")
# #glimpse(diabetes_RaceEth) # preview dataframe's structure
# 
# # Column chart of diabetes prevalence by Race/Ethnicity and year using ggplot2
# ggplot(data = diabetes_RaceEth) +
#   geom_col(mapping = aes(x = as_factor(Year), y = Percent, fill = `Strata Name`), position = "dodge") +
#   theme_bw() +
#   labs(x = "Year", y = "Cases per 100 adults", 
#        title = "Diabetes Prevalence among Adults in California by Race/Ethnicity, 2012-2018",
#        #subtitle = "Rates of diabetes decline as income rises",
#        caption = "Sourced from CA Open Data Portal: https://data.ca.gov")
# 
# # Column chart of diabetes prevalence by Year and Race/Ethnicity using Plotly
# diabetes_RaceEth %>%
#   plot_ly(x = ~as_factor(Year), y = ~Percent, type = "bar", color = ~`Strata Name`) %>%
#   layout(xaxis = list(title = "Year"), yaxis = list(title = "Percent per 100"),
#          title = "Diabetes Prevalence among Adults in California by Race/Ethnicity, 2012-2018",
#          annotations = list(text = "Sourced from CA Open Data Portal: https://data.ca.gov",
#                             x = 1, y = -0.1, showarrow = FALSE, xref='paper', yref='paper', 
#                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
#                             font=list(size=15))
#          )
