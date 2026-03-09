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


#PART 3.1
#check how many of each category we have
patients %>% 
  count(marital)
#results:
#marital     n
#  <chr>   <int>
#1 D         635
#2 M        2772
#3 S         906
#4 W         205
#5 NA       2333

#change the reasonable things into factors with labels

patients <- patients %>%
  mutate(
    
    # change marital status codes to descriptive categories
    marital = factor(
      marital,
      levels = c("S", "M", "D", "W"),
      labels = c("Single", "Married", "Divorced", "Widowed")
    ),
    
    # change gender codes to readable labels
    gender = factor(
      gender,
      levels = c("M", "F"),
      labels = c("Male", "Female")
    )
  )


#check the labels now
patients %>% count(marital)
patients %>% count(gender)

#find all other reasonable collumns

#find the candidate collumns
#find the character columns with less than 10 unqiue values

fctr_candidates <- patients %>%
  select(where(is.character)) %>%
  summarise(across(everything(), n_distinct)) %>%
  pivot_longer(everything()) %>%
  filter(value < 10) %>%
  pull(name)

#show all the unqiue values for those variables
patients %>%
  select(all_of(fctr_candidates)) %>%
  summarise(across(everything(), ~ paste(unique(.), collapse = ", "))) %>%
  glimpse()

#the results:
#$ prefix    <chr> "Mr., Mrs., NA, Ms."
#$ suffix    <chr> "PhD, NA, JD, MD"
#$ race      <chr> "white, black, native, hawaiian, asian, other"
#$ ethnicity <chr> "nonhispanic, hispanic"
#$ state     <chr> "Arkansas, Alaska, California, Massachusetts,…


#convert those variables into factors:
patients <- patients %>%
  mutate(across(all_of(fctr_candidates), as.factor))

#part 3.2: unusual combinations
#the purpose here is to check whetehr some combinations of variables are very rare

#for example, one or two patients bleong to certain combination of gender race, state, then those individuals are easier to identify

#this important for ethics! especticlaly is reported later in figures

#count how many patients belong to each combination of gender, erace and state
combination_counts <- patients %>%
  count(gender, race, state, name = "N") %>%
  
  # sort from the smallest groups to the largest groups
  arrange(N)

# print the result 
# we can inspect whether some combinations are very rare
combination_counts

#the results
  #gender race     state             N
  # <fct>  <fct>    <fct>         <int>
 #1 Male   native   Alabama           1
 #2 Male   native   Massachusetts     1
 #3 Female hawaiian Alabama           2
 #4 Female hawaiian Arkansas          3
 #5 Female native   Arkansas          4
 #6 Female other    Arkansas          4
 #7 Male   other    California        5
 #8 Female native   Massachusetts     5

#collect all 
rare_combinations <- combination_counts %>%
  filter(N <= 5)

rare_combinations


#combine the rare race groups
#basically we want to recladdify the race variable to reduce the number of rare gorups
#fct_lump_prop() keeps categories that make up at least 5% of the observations and combines smaller categries into "Other"

#this reduces discloser risk and mmakes later summaries easier to interpret
patients <- patients %>%
  mutate(
    race = forcats::fct_lump_prop(race, prop = 0.05)
  )

#Part 4
#load the payer_transitions dataset from inside the zip archive
#this dataset contains information about when patients moved 
#between insurance payers over tine

payer_transitions <- readr::read_csv(
  unz("data.zip", "data-fixed/payer_transitions.csv"),
  show_col_types = FALSE
)

#determine the assumed dataset extract date
#the extract date should be the latest start_date in the payer-transitions dataset

extract_date <- payer_transitions %>%
  #take the maximum start date 
  summarise(last_date = max(start_date, na.rm = TRUE)) %>%
  pull(last_date) %>%
  as.Date()

extract_date
#result: [1] "2025-10-01 10:33:29 UTC"

#next we want to caluclate patient age based on the dataset extract date
#age is computer as the number of days between the extract date and the birthdate, divided by 365.25 to approximate the year 
#floor() ensures we get the age in completed years 
patients <- patients %>%
  mutate(
    age = floor(as.numeric(extract_date - birthdate) / 365.25)
  )


#DECEASED PATIENTS
#the deceased patients should not get an age value 
patients <- patients %>%
  mutate(
    age = if_else(
      is.na(deathdate),
      floor(as.numeric(extract_date - birthdate) / 365.25),
      NA_real_
    )
  )

#check the result
patients %>% 
  select(birthdate, deathdate, age) %>% 
  glimpse()


#make a histogram
patients %>%
  #first, remove all the deceased patients 
  filter(is.na(deathdate)) %>%
  
  #we want to plot age
  ggplot(aes(x = age)) +
  
  #group it into 5 year bins
  geom_histogram(binwidth = 5) +
  labs(
    title = "Age distribution of living patients at data extract date",
    x = "Age (years)",
    y = "Number of patients"
  )
