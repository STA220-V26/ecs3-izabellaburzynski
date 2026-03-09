#install.packages(c("tidyverse", "janitor", "fs", "curl"))


library(tidyverse)
library(janitor)
library(fs)
library(curl)

#############################################################################
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

#############################################################################
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


#############################################################################
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

#############################################################################
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

#the results (wouldnt usually put this in for privacy reason)
#just put it in for pedagological reasons 
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

#############################################################################
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


#############################################################################
#PART 5: NAMES
#we want to replace missing prefix and middle values with empty strings
#create a full_name from prefix, first, miffle, last and optional suffix
#trim trailing spcaes ofr character collumns
#remoce duplicated spaces inside full_name
#delete the old name columns afterwards

#when you combine name parts with past(), missing or empty pieces can create ugly resutls like:
#extra spaces at the start or end
#double spaces in the middle
#awakrd suffix handlking

#goal: build one clean full_name collumn for later use

patients <- patients %>%
  mutate(
    # replace missing prefix and middle values with empty strings so they do not appear as NA inside the final name
    prefix = tidyr::replace_na(as.character(prefix), ""),
    middle = tidyr::replace_na(as.character(middle), ""),
    suffix = tidyr::replace_na(as.character(suffix), "")
  ) %>%
  mutate(
    # combine name parts into one string
    # if suffix exists, add it with a comma
    full_name = if_else(
      suffix != "",
      paste(prefix, first, middle, last, paste0(", ", suffix)),
      paste(prefix, first, middle, last)
    ),
  

   # collapse repeated spaces using regex
full_name = stringr::str_replace_all(full_name, " +", " "),

# remove leading and trailing spaces
full_name = stringr::str_trim(full_name)
  )


#check the cleaned names
patients %>% select(full_name) %>% head(10)

#remove original name columns
patients <- patients %>%
  select(-prefix, -first, -middle, -last, -suffix, -maiden)

#check
names(patients) #no longer see prefeix, first, etc


#############################################################################
#Part 6: Necessary Data
#original data set has drivers collumn
#However, with the information currently available, the only
# relevant thing seems to be whether the patient has a driver's
# license or not.


patients <- patients %>%
  mutate(
    driver = !is.na(drivers)
  ) %>%
  #remove the original drivers column after createing the logical collumn
  select(-drivers)

#check trua nd false
patients %>% count(driver)

#playing around with leaflet
install.packages("leaflet")
library(leaflet)
leaflet(patients) |>
  addTiles() |>
  addCircleMarkers(~lon, ~lat, radius = 2)

#Based on this data, would it be reasonable to make statistical inference for the whole of USA? For
#the whole world?
#Not all of the US, it's mainly the south and Alaska


#Part 7: Linkage
#if were interested in reasons for procedures performed on adults over the years, for exmaple
#we need to:
#load the procedures data
#keep only collumns needed
#extract the year of each procedure
#join procedures with the patients table using patient ID
#calculate whetehr the patient was 18 or older at teh time of the procedure
#count procedure reasons by year 

#we need from procedures:
#"patients" for linkage
#"reasoncode_icd10" for the reason
#"start" to calculate age and summary by year

#we need to drop rows where "reasoncode_icd10" is missing 
#then it joins "procedures" to "patients" on 
#"procedures$patient" and "patients$id"

#trying it just with dplyr:
#read the procedures file
procedures <- readr::read_csv(
  unz("data.zip", "data-fixed/procedures.csv"),
  show_col_types = FALSE
)

#keep only columns i need
#linkage key
#reason code
#date
procedures <- procedures %>%
  select(patient, reasoncode_icd10, start) %>%
  #remove rows without reason code
  filter(!is.na(reasoncode_icd10))

#create a year variable
procedures <- procedures %>%
  mutate(
    start = as.Date(start),
    year = lubridate::year(start)
  ) %>%
  #remove start column 
  select(-start)

#join procedures with patient birthdate 
proc_linked <- procedures %>%
  left_join(
    patients %>% select(id, birthdate),
    by = c("patient" = "id")
  )

#calculate age at procedure and keep adults 
#adults are people who were atleast 18 at teh time of the proceudre
proc_n_adults <- proc_linked %>%
  mutate(
    age_at_procedure = year - lubridate::year(birthdate)
  ) %>%
  filter(age_at_procedure >= 18) %>%
  count(reasoncode_icd10, year, name = "N")

#now proc_n_adults
#contains reasoncode_icd10, year, N
#where N is the number of procedures with that reaosn among adults in that year
#the same patient might have had more than one prodcuere so these are procedure counts, not unique patient counts 

#Add a human-readable ICD-10 descriptions 
#the variable reasoncode_icd10 ontains only ICD-10 codes,
# which are not very informative for readers.

#The decoder package contains a lookup table (icd10se)
# that maps ICD-10 codes to descriptive condition names.

# I join this table to our procedure counts so that each
# ICD-10 code is accompanied by a readable description.
install.packages("decoder")
library(decoder)
cond_by_year <- proc_n_adults %>%
  left_join(
    decoder::icd10se %>% 
      mutate(key = as.character(key)),
    by = c("reasoncode_icd10" = "key")
  )

#visualization: five most common coditions overall:

#The cond_by_year table contains:
# reasoncode_icd10 = ICD-10 code
#  year             = calendar year
# N                = number of procedures
#  value            = human-readable condition description

#first we identify the 5 most common conditionns overall by summing procedure counts across all years 

#get the top 5 conditions overall
top5 <- cond_by_year %>%
  group_by(value) %>%

  #add up the counts across all years for each condition
  summarise(N = sum(N), .groups = "drop") %>%
  
  #sort from most common to least common
  arrange(desc(N)) %>%
  
  #keep only the five most common
  slice_head(n = 5) %>%
  #extract condiction names into a vector
  pull(value)

#plot only those top 5 
cond_by_year %>%
  filter(value %in% top5) %>%
  ggplot(aes(x = year, y = N, color = value)) +

  #draw one line per condition over time
  geom_line() +
  
  #legend to bottom 
  theme(legend.position = "bottom") +
  
  #legent formatting, into one collumn 
  guides(color = guide_legend(ncol = 1)) +
  
  #wrap condition labels 
  scale_color_discrete(
    labels = function(x) stringr::str_wrap(x, width = 40)
  ) +
  labs(
    title = "Five most common conditions overall",
    x = "Year",
    y = "Number of procedures",
    color = "Condition"
  )

#questions for 7

#Wath happens in the end? Is it really reasonable to include tha last year (we were looking at
#the assumed data extraction date earlier)
#Not necessarily. The last year may be incomplete because the dataset was extracted before the end of the year, so procedure counts may appear artificially lower.

#What happened early in history? 
# The earliest years may contain incomplete or less reliable records. 
# Lower counts in early years may reflect missing or poorly recorded data rather than fewer procedures.


#Does this visualisation tell us anything? 
#It shows how the number of procedures changed over time, 
# but it only reflects raw counts and does not necessarily indicate changes in disease prevalence.

#Do we need to standardize the numbers?
#Yes. To properly compare across years, counts should ideally be standardized by population size or the number of patients in the dataset.


#Are patients sicker today or do they get more treatment for conditions which might have been
#undertreated in the past?
#Not necessarily. Higher procedure counts may reflect improved diagnosis, better access to healthcare, or increased treatment rather than greater disease severity.
