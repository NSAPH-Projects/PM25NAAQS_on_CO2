################################################################################
## The impact of PM2.5 National Ambient Air Quality Standards on CO2 emissions #
## Veronica Ballerini, Marina Bottomley, Michelle L. Bell, Francesca Dominici ##
################### Code author: Veronica Ballerini ############################
################## Last modified: November 13, 2026 ############################
## This code builds the dataset used to produce results in the manuscript. #####
################################################################################

# clean environment
rm(list=ls())

# set the seed
set.seed(123)

# set the working directory
project.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project.dir)

# upload all useful libraries
library(dplyr)
library(tidyr)
library(tidybayes)
library(tidyverse)

library(readxl)
library(readr)
library(purrr)
library(stringr)

################################################################################
########################### Data upload ########################################
################################################################################
destfile <- "co2_sector.xlsx"

## Load the "Electric power" CO2 emissions sheet and skip the first row, which is empty
data <- read_excel(destfile, sheet = 6, skip = 1)
head(data)

## Transpose the dataset for convenience
CO2 <-as.data.frame(t(data[,-1]))
head(CO2)

n_until2015<-56 #number of periods until 2015
CO2<-CO2[c(1:n_until2015),] #cut the series at 2015

colnames(CO2)<-as.character(data[[1]]) # rename the columns

names(CO2)

# Exclude Hawaii, Alaska, and DC
CO2 <- select(CO2, -AK, -DC, -HI, -US)

#Wrangle dataset so that you can combine with merged_df
#Add a year column
CO2$Year <- as.numeric(rownames(CO2))

# Pivot data to long format
CO2_long <- CO2 %>%
  pivot_longer(
    cols=-Year,
    names_to = "State",
    values_to= "CO2"
  )

#########Combining covariates into one dataset

## Load the "Commercial" CO2 emissions sheet and skip the first row, which is empty
data_commercial <- read_excel(destfile, sheet = 3, skip = 1)
head(data_commercial)

## Transpose the dataset for convenience
CO2_commercial <-as.data.frame(t(data_commercial[,-1]))
CO2_commercial<-CO2_commercial[c(1:n_until2015),] #cut the series at 2015
colnames(CO2_commercial)<-as.character(data_commercial[[1]]) # rename the columns

# Exclude Hawaii, Alaska, and DC
CO2_commercial <- select(CO2_commercial, -AK, -DC, -HI, -US)

#Wrangle dataset so that you can combine with merged_df
#Add a year column
CO2_commercial$Year <- as.numeric(rownames(CO2_commercial))

# Pivot data to long format
CO2_commercial_long <- CO2_commercial %>%
  pivot_longer(
    cols=-Year,
    names_to = "State",
    values_to= "CO2"
  )
names(CO2_commercial_long)[3]<-"CO2_commercial"

# Function to load and label one theme directory
combine_theme_data <- function(folder_path, suffix = "") {
  theme_name <- basename(folder_path)
  files <- list.files(folder_path, full.names = TRUE, pattern = "\\.csv$")
  
  data_list <- lapply(files, function(file) {
    df <- read.csv(file)
    
    # Get filename without extension and strip suffix
    file_base <- tools::file_path_sans_ext(basename(file))
    state_name <- sub(paste0(suffix, "$"), "", file_base)
    
    colnames(df)[2] <- str_to_title(theme_name)  # rename to "Oil", "Population"
    df$State <- state_name
    df
  })
  
  do.call(rbind, data_list)
}

Population <- combine_theme_data(paste(project.dir,"/covariates/Population/",sep=""), 
                                 suffix = "POP")

#Change date format to display just year
Population$observation_date <- format(as.Date(Population$observation_date),"%Y")
Population <- Population[-which(Population$observation_date<1960|
                                  Population$observation_date>2015),]

#Change column name from Observation_date to year
Population <- Population %>%
  rename(Year= observation_date)

#Upload more covariates, temperature and oil prices

# Upload Climate data by State 
# Upload excel sheet with weather data 
# Of all data, only select up to year 2015
# keep_years <- as.character(1960:2015)

#Load Temperature and Precipitation data
Temperature1 <- read_excel(paste(project.dir,"/covariates/StateClimate.xlsx",sep=""), 
                           sheet=1, skip=1)
# Temperature1 <- select(Temperature1, 1, all_of(keep_years))
head(Temperature1)

Precipitation1 <- read_excel(paste(project.dir,"/covariates/StateClimate.xlsx",
                                   sep=""),sheet=2, skip=1)
# Precipitation1 <- select(Precipitation1, 1, all_of(keep_years))
head(Precipitation1)

#Wrangle data 

#Pivot Temperature Data
Temperature <- Temperature1 %>%
  pivot_longer(
    col= -1, 
    names_to = "Year",
    values_to= "Temperature",
  ) %>%
  rename(State=1) %>%
  mutate(Year= as.numeric(Year))

#Pivot Precipitation Data 
Precipitation <- Precipitation1 %>%
  pivot_longer(
    col= -1,
    names_to= "Year",
    values_to= "Precipitation",
  ) %>%
  rename(State=1) %>%
  mutate(Year= as.numeric(Year))

#Join Precipitation and Temperature data into one Climate dataset  
Climate <- left_join(Temperature, Precipitation, by= c("State", "Year"))

Climate <- Climate %>% 
  select(Year, State, Temperature, Precipitation) #reorder columns so year is first

#Load GDP data
# gdp <- read_excel(paste(project.dir,
#                         "/covariates/P_Data_Extract_From_World_Development_Indicators.xlsx",
#                         sep=""), 
#                   sheet=1)
gdp <- read_excel(paste(project.dir,
                        "/covariates/GDPC1.xlsx",
                        sep=""), 
                  sheet=2)
gdp <- gdp[-c(1:13),]
gdp$observation_date<-as.numeric(substr(gdp$observation_date,1,4))
gdp$GDPC1<-as.numeric(gdp$GDPC1)
head(gdp)
colnames(gdp)<-c("Year","gdp")

#Upload oil prices data for US
fileoil_path <- paste(project.dir,"/covariates/oil-prices-inflation-adjusted.csv", 
                      sep="")
Oil_price <- read_csv(fileoil_path)

## Wrangle Oil Data
#Delete unnecessary columns
Oil_price$Entity <- NULL
Oil_price$Code <- NULL

# Rename variables 
variables2<- c("Year", "Oil_Price")
names(Oil_price)<-variables2 
# Oil_price<-Oil_price[-which(Oil_price$Year<1960|Oil_price$Year>2015),]
Oil_price<-Oil_price[-which(Oil_price$Year<1960),]

#Upload natural gas prices data for US
filegas_path <- paste(project.dir,"/covariates/N9190US3a.xls", 
                      sep="")
Naturalgas_price <- read_excel(filegas_path,sheet=2, skip=2)
Naturalgas_price <- Naturalgas_price[-c(1:38,92:104),]
Naturalgas_price$Date<-substr(Naturalgas_price$Date,1,4)

# Rename variables 
colnames(Naturalgas_price)<-c("Year","Natural_gas")

# Ensure Year is the same variable type in each dataset: numeric
Population$Year <- as.numeric(Population$Year)
Oil_price$Year <- as.numeric(Oil_price$Year)
Temperature$Year <- as.numeric(Temperature$Year)
Naturalgas_price$Year <- as.numeric(Naturalgas_price$Year)

# Ensure Year is the same variable type in each dataset: numeric
Population$Year <- as.numeric(Population$Year)
Oil_price$Year <- as.numeric(Oil_price$Year)
Temperature$Year <- as.numeric(Temperature$Year)
Naturalgas_price$Year <- as.numeric(Naturalgas_price$Year)

#Load renewable data
renew <- read.csv(paste(project.dir,"/covariates/MER_T10_01.csv",sep=""))
str(renew)
unique(renew$Description)
renew$YYYYMM <- as.character(renew$YYYYMM)
renew$Year <- substr(renew$YYYYMM, 1, 4)
renew$tot <- ifelse(substr(renew$YYYYMM, 5, 6)=="13",1,0)

renew_cons <- renew[which(renew$tot==1&renew$Description=="Total Renewable Energy Consumption"),
                    -c(1,2,4:6,8)]
head(renew_cons)
colnames(renew_cons)<-c("renew","Year")
renew_cons$Year <- as.numeric(renew_cons$Year)
renew_cons$renew <- as.numeric(renew_cons$renew)

# Merge national data for oil and temperature with the merged_df for each state, 
# by year
merged_df <- Population %>% 
  left_join( Oil_price, by= "Year" )

merged_df <- merged_df %>%
  left_join(gdp, by= c("Year"))

merged_df <- merged_df %>%
  left_join(renew_cons, by = "Year")

merged_df <- merged_df %>%
  left_join(Temperature, by= c("Year", "State"))

merged_df <- merged_df %>%
  left_join(Precipitation, by= c("Year", "State"))

merged_df <- merged_df %>%
  left_join(Naturalgas_price, by = c("Year"))

merged_df <- CO2_commercial_long %>%
  left_join(merged_df, by= c("Year", "State"))

# Merge covariates with CO2 data
final_data <- CO2_long %>%
  left_join(merged_df, by= c("Year", "State"))

#### SAVE
save(final_data,n_until2015,file=paste0(project.dir,"/final_data_electricpower.RData"))
