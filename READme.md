# Program Purpose

# Dependancy packages
readxl
tidyverse (dpylr, lubridate, stringr)
fuzzyjoin

# Preprocessing steps that need to happen in Excel (but should be added to the program)
- change dates into YYYY-MM-DD format
- change times into military HH:MM:SS format

# Components of the processing in R - part 1 & part 2

# Data Inputs

potential script problems based on data input 
- there are inconsistancies in how the actigraphy data formats participant IDs
- the delimeter of the actigraphy data may either be a comma or a tab
- in some files, '1899-12-31' shows up instead of NAN or NA for dates

# Data Outputs

#Improvements that need to be made
- one file for baseline and follow up
