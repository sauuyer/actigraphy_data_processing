#data read
file2 <- read.csv("march-2021/processed_data/contains_sleep_based_day_ids.csv")
file2 %>% group_by(subject_id.x, sleep_based_day_id)%>%
  summarise(n())
#file2 <- file2 %>% group_by(subject_id.x, sleep_based_day_id)


#datatype (dates) conhersion 
file2$interval_start.y <- ymd_hms(file2$interval_start.y)
file2$interval_end <- ymd_hms(file2$interval_end)
file2$interval_duration_hrs <- file2$interval_end - file2$interval_start.y
file2$interval_duration_hrs <- as.numeric(file2$interval_duration_hrs)
#as.numeric(file2$sleep_based_day_id)
file2$activity_start_datetime <- ymd_hms(file2$activity_start_datetime)
file2$activity_day <- date(file2$activity_start_datetime)

new <- file2

#mark omitted data (data with NA activity start datetime)
new <- new %>% mutate(sleep_based_day_id = ifelse(is.na(activity_start_datetime), 9999, sleep_based_day_id))

###### subset problem data
#greater than 30 hour interval durations 
str(new$interval_duration_hrs)
oneohonerange <- new %>%
  filter(interval_duration_hrs > 30)

#partial days at start and end of study
partialdays <- new %>%
  filter(is.na(sleep_based_day_id))

#days with more than 20% excluded data
new$activity_start_datetime <- ymd_hms(new$activity_start_datetime)
new$activity_end_datetime <- ymd_hms(new$activity_end_datetime)
#time duration is in minutes for activities
new$activity_duration <- new$activity_end_datetime - new$activity_start_datetime
new$activity_duration <- as.numeric(new$activity_duration)/60
new$interval_duration_hrs

toomuchexcludeddata <- new %>%
  filter(interval_type == "EXCLUDED") %>%
  select(subject_id.x, sleep_based_day_id, interval_duration_hrs, activity_duration) %>%
  mutate(twenty.percent.of.interval.time = interval_duration_hrs * .20) %>%
  group_by(subject_id.x, sleep_based_day_id) %>%
  summarise(excluded.hours.per.day = sum(activity_duration), .groups = "keep") %>%
  filter(!is.na(sleep_based_day_id))

excluded.hours.per.day.calculation <- fuzzy_left_join(new, toomuchexcludeddata, 
                                                      by = c("subject_id.x"="subject_id.x","sleep_based_day_id" ="sleep_based_day_id"),
                                                      match_fun = list(`==`, `==`))

toomuchexcludedtime <- excluded.hours.per.day.calculation %>%
  filter(excluded.hours.per.day > interval_duration_hrs * .2) %>%
  select()

nrow(filter(new, is.na(sleep_based_day_id)))

write.csv(oneohonerange, "march-2021/processed_data/problem_data/oneohonerange.csv")
write.csv(partialdays, "march-2021/processed_data/problem_data/partialdays.csv")
write.csv(toomuchexcludeddata, "march-2021/processed_data/problem_data/toomuchexcludeddata.csv")

#combined problem data subsets
allproblems <-rbind(oneohonerange, partialdays, toomuchexcludedtime)

#group by subject id, order by activity date time
allproblems$start_date <- ymd(allproblems$start_date)
allproblemdays <- allproblems %>% group_by(subject_id.x) %>%
  distinct(start_date) %>%
  arrange(start_date, .by_group = TRUE) %>%
  mutate(problem_sleep_based_day_id = row_number() + 100)

#add these new ranges to the new sheet
problemsjoined <- fuzzy_left_join(new, allproblemdays, by = c("subject_id.x" = "subject_id.x", 
                                                              "activity_day" = "start_date"),
                                  match_fun = list(`==`, `==`))




#if problem_sleep_based_day_ids is not null, override sleepbased day id with it in the joined sheet.
problemsjoined <- problemsjoined %>% mutate(sleep_based_day_id = ifelse(!is.na(problem_sleep_based_day_id), problem_sleep_based_day_id, sleep_based_day_id))
#joined <- joined %>% mutate(sleep_based_day_id = ifelse(is.na(activity_start_datetime), "no-activity-start-datetime", 
#                                                       as.character(sleep_based_day_id)))

#add 99s to cases that are activities not followed by a rest period
#if lead within a subject id group is not rest, active, sleep or custom
#ninetynines <- problemsjoined %>%
#  filter(interval_type == "ACTIVE")

write.csv(problemsjoined, "reprocessed-data/correct-august-outputs/2020-08-13-followup-completed-2.csv")
