library(readxl)
library(lubridate)
library(dplyr)
library(stringr)
library(fuzzyjoin)

`%--%` <- function(x, y) {
  
  do.call(sprintf, c(list(x), y))
  
}

file <- read.csv(file = 'march-2021/original_data/baseline TSS Covid participants_partial_3-2-21.csv', sep = ",", header = TRUE)
head(file)

#remove daily summary from activity types
file <- file %>%
  filter(interval_type != "DAILY")

#remove any non-numeric characters from the subject_id column
file$subject_id <- str_extract(file$subject_id, "\\d+")
file$subject_id <- as.integer(file$subject_id)
#file$subject_id <- as.character(file$subject_id) /// get rid of this line?

###create activity intervals 
#start activity datetime
file$activity_start_datetime <- paste(file$start_date, file$start_time, sep = " ")
file$activity_start_datetime <- as_datetime(file$activity_start_datetime)
file$activity_start_datetime <- as.POSIXct(file$activity_start_datetime, origin = file$activity_start_datetime, tz = "EST")

#end activity datetime
file$activity_end_datetime <- paste(file$end_date, file$end_time, sep = " ")
file$activity_end_datetime <- as_datetime(file$activity_end_datetime)
file$activity_end_datetime <- as.POSIXct(file$activity_end_datetime, origin = file$activity_end_datetime, tz = "EST")

#create new day start time based on when a subject wakes up on the current day (end_time of rest on the rest end date)
file$interval_start <- paste(file$end_date, file$end_time, sep = " ") #consider moving this to the sub dfs, not the main file 
file$interval_start <- as_datetime(file$interval_start)
file$interval_start <- as.POSIXct(file$interval_start, tz = "EST") #consider moving this to the sub dfs, not the main file

#create a unique id for events based on the subject id and interval number
file$activityuid <- paste("subject", file$subject_id, file$interval_type, file$activity_start_datetime, sep = "_")

names(file)

df <- file

#indicate the largest of the subjects listed in the data file. later figure out how to get user input here. 
#note = largest subject id is more important than the n of subject ids
subject_id_summary <- df %>%
  group_by(subject_id) %>%
  summarise(n = n())
subject_id_summary <- as.data.frame(subject_id_summary)
write.csv(subject_id_summary, "march-2021/data_summaries/number_of_rows_per_subject.csv")
number_of_subjects <- 110
#make the number of subjects a range to use in the for loop 
subject_range <- 1:as.integer(number_of_subjects)

#create a seperate dataframe for each of the subjects and filter to show only the rest intervals
#and create a list containing the names of all of the created dfs
for (s in subject_range) {
  assign(paste("subjectdf_", s, sep = ""), 
         df  %>% 
           group_by(subject_id) %>%
           filter(subject_id == s & interval_type == "REST"))
}

subjects_dfs_list <- mget(ls(pattern="subjectdf_"))

#add the interval_end column to each subject's dataframe
subjects_dfs_list <- lapply(subjects_dfs_list, function(x){
  x$interval_end <- lead(x$interval_start)
  return(x)
}) 

test <- as.data.frame(subjects_dfs_list[10])


############move this to another stage after df is merged back together 
#create an interval value for each of the 
#df_list <- lapply(df_list, function(x){
#  x$sleep_based_day_id <- interval(x$interval_start, x$interval_end)
#  return(x)
#})

#remove empty lists
subjects_dfs_list <- subjects_dfs_list[sapply(subjects_dfs_list, function(x) dim(x)[1]) > 0]

#create a unique identifier for each of the new day ranges
subjects_dfs_list <- lapply(subjects_dfs_list, function(x){
  x$sleep_based_day_id <- seq(nrow(x)) 
  return(x)
})

#parse down the number of cols in the day indexing df
subjects_dfs_list <- lapply(subjects_dfs_list, function(x)
  select(x, subject_id, interval_start, interval_end, sleep_based_day_id, activityuid))


subjects_dfs_list[9]
subjectdf_1


#break dfs out of list (updating the subjectdf_s in the global environment, but maintaining the list itself)
list2env(subjects_dfs_list, envir=.GlobalEnv)

#bind all of the listed dfs into one df
day_intervals_df <- bind_rows(subjects_dfs_list)

joined_df <- fuzzy_left_join(df, day_intervals_df, by = c("subject_id" = "subject_id", 
                                                          "activity_start_datetime" = "interval_start",
                                                          "activity_end_datetime" = "interval_end"),
                             match_fun = list(`==`, `>=`, `<=`)
)

'''
#index incomplete first day of activity tracking as sleep_based_day_id "0"
joined_df <- joined_df %>%
mutate(sleep_based_day_id = ifelse(date(activity_start_datetime) == date(data_start_date), "0", joined_df$sleep_based_day_id))


#index incomplete last day of activity tracking as sleep_based_day_id "99" these are the only currently existing NA values in this columns
joined_df[, "sleep_based_day_id"][is.na(joined_df[, "sleep_based_day_id"])] <- 99
'''

#intermediate data file creation:
write.csv(joined_df, "march-2021/processed_data/3-4-contains_sleep_based_day_ids.csv")

joined_df_smaller <- select(joined_df, subject_id.x, sleep_based_day_id, interval_type, activity_start_datetime, activity_end_datetime, interval_start.y, interval_end)
#joined_df <- joined_df[complete.cases(joined_df), ]

joined_df_sorted <- arrange(joined_df_smaller, subject_id.x, sleep_based_day_id)
write.csv(joined_df_sorted, "march-2021/processed_data/3-4-contains_sleep_based_day_ids_SMALLER_SORTED.csv")

naps <- joined_df_smaller %>%
  group_by(subject_id.x, sleep_based_day_id) %>%
  filter(interval_type == 'CUSTOM') %>%
  summarize(n()) 

write.csv(naps, "march-2021/data_summaries/2021-03-04_baseline-nap-counts.csv")



