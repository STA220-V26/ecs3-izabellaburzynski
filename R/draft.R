install.packages(c("tidyverse", "janitor", "fs", "curl"))


library(tidyverse)
library(janitor)
library(fs)
library(curl)

#STEP 2: PATIENT DATA

#ERIK"S CODE
#library(tidyverse)
#library(data.table)
#patients <-
#readr::read_csv(unz("data.zip", "data-fixed/patients.csv")) |>
#setDT() |>
#setkey(id

#patients <- janitor::remove_empty(patients, quiet = FALSE)
# A column with only one constant value is also not very interesting
#patients <- janitor::remove_constant(patients, quiet = FALSE)


#MY CODE 
# Download the dataset archive if it is not already present in the project folder
if (!fs::file_exists("data.zip")) {
  curl::curl_download(
    url = "https://github.com/eribul/cs/raw/refs/heads/main/data.zip",
    destfile = "data.zip",
    quiet = FALSE
  )
}

# Read only the patient table directly from the zip archive
# This avoids reading the full thing when we only need one file
patients <- readr::read_csv(
  unz("data.zip", "data-fixed/patients.csv"),
  show_col_types = FALSE
)

# Remove the rows and columns that are completely empty
patients <- patients %>%
  janitor::remove_empty(which = c("rows", "cols"))

# Remove columns where all values are identical
patients <- patients %>%
  janitor::remove_constant()

# Inspect the cleaned patient dataset
glimpse(patients)

#PART 3 Expectations/validations
#Now we're running some checks based on what we expect from the data
#for example: dates should be realistic, death should not be before birth

#install.packages("pointblank")
library(pointblank)

#see the collumn names
names(patients)
# we have id, birthdate, deathdate, ssn, drivers, passport, etc

#get a glimpse of what the data looks like
glimpse(patients)

#make sure the date columns are real dates 
patients <- patients %>%
  mutate(
    birthdate = as.Date(birthdate),
    deathdate = as.Date(deathdate)
  )

#write the validation code

#load pointblank package, this is important for validaitng data
#allows us to define rules/expectations for what the data should look like
library(pointblank)
library(dplyr)

#start by creating a validation agent
#the agent is an object that stores all the validation checks we define
checks <- patients %>% 
  
  #create_agent() initialized the validation workflow
  #label describes what the validation is for
  create_agent(label = "Validation checks for patient data") %>%
  
  #Check 1: Validate date ranges
  #this checks that birthdate and deathdate fall within a reasonable range of time
  
  col_vals_between(
    columns = c(birthdate, deathdate),
    
    #no patient should have a birth date that is before the date 1900 Jan 1 
    left = as.Date("1900-01-01"),
    
    #the dates should not be in the future 
    right = Sys.Date(),
    
    #but missing values are allowed, for example if patient is still alive
    na_pass = TRUE,
    label = "Birth and death dates should be between 1900-01-01 and today"
  ) %>%
  
  #CHECK 2: logical consistency of dates 
  #patients death date should never be ealrier than their birth date

  #compares two columns, deathdate must be bigger or equal to birthday 
  col_vals_gte(
    columns = deathdate,
    value = vars(birthdate),

    
    label = "Death date must be on or after birth date"
  ) %>%
  
  #CHECK 3: Calidate SSN format

  #the regex pattern forces the structure 
#000-00-0000

  #[0-9]{3} 3 digits
  #[0-9]{2} 2 digits
  #[0-9]{4} 4 digits 

  # 6 and $ make sure the ENTIRE string follows this format 

  col_vals_regex(
    columns = ssn,
    regex = "^[0-9]{3}-[0-9]{2}-[0-9]{4}$",

    
    label = "SSN must follow the format 000-00-0000"
  ) %>%
  
  #Check 4: Patient ID must be an integer 
  #each patient should have a unique numeric identifier

  #id only has interger values, if accidently read as characters or decimals, this check would fail 
  col_is_integer(
    columns = id,
    label = "Patient ID must be stored as an integer"
  ) %>%
  
  #Additional Check 1: Gender 

  #gender should only contain the values M and F 
  col_vals_in_set(
    columns = gender,

    #define the set of allowed values
    set = c("M", "F"),

  
    label = "Gender should only contain M or F"
  ) %>%
  
  #Additional Check 2: 

  # marital status collumn containes:
  #S= Single
  #M=Married
  #D=Divorced
  #W=Widowed 

  #If A, or unknown exists, it will be flagged

  #this rule checks that only these valid codes appear
  col_vals_in_set(
    columns = marital,

    #define allowed set
    set = c("S", "M", "D", "W"),

    
    label = "Marital status should only contain the expected coded values"
  ) %>%
  
  
  #Additional check 3: SSN are unique for each person (no repeats)
  rows_distinct(
    #go through IDS and check if they are distinct
    columns = ssn,
    label = "Each SSN should uniquely identify a patient"
  ) %>%
  
  interrogate()

#see the report in R 
checks

#export the report as a html file
export_report(checks, "patient_validation.html")

#check that the file was actually created
file.exists("patient_validation.html")
