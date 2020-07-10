# Rodrigo Dal Ben, 14/05/2020
# script to output raw data from D'Souza et al. (2020, doi: 10.1098/rsos.180191) 
# D'Souza OSF: https://osf.io/53gh2/
# D'Souza Dryad: https://datadryad.org/stash/dataset/doi:10.5061/dryad.3n5tb2rc6 

# summary:
# 1) matlab:
# raw events and data (x and y axis) have been pulled from .mat files (10.5061/dryad.3n5tb2rc6),
# the authors collected data on 3 (independent?) buffers: 1. events, 2. time, 3. samples. 
# the event buffer had its' own timeline; time and samples buffers were combined following the authors' matlab script;
# for each participant, one samples file (with a timeline) and one events file (with a timeline) was genereted (02_input_csv folder).

# 2) R:
# a single dataframe for events and another for samples were created and id was added as a variable.
# the usual `dplyr::left_join` didn't work for joining these dfs as the timelines between them do not exactly match. 
# an join baed on time interval was done using `fuzzyjoin::fuzzy_left_join`.
# this process created a gigantic vector of size greater than 12.5Gb, so my notebook could not handle it.
# the dfs were broken into lists of smaller dataframes;
# lists were iterated using `purrr::map2`;
# some 14 hours later, the full dataset was ready. 
# rename "data" for "samples" for clarity
# add participant log to dataset - 95 ids match; we miss 7 participants
# SES = Social Economic Status
# Exposure = anything below 90 is coded as Bilingual
# 18/06/2020: e-mail by D'Souza with updated participant info.
# 19/06/2020: update participant info, A90 is missing; 
# A90 retrieved from MATLAB (1 file for exp1 and another for exp2 and 3; they were being overwritten)
# added A90, now we have the full 102-infants sample
# moved all the "old/exploratory" code to the end of the script


# load library
library(tidyverse)
library(here)
library(fuzzyjoin)
library(janitor)

# list all .csv from folder and merge them
## list all .csv files
setwd(here("/02_input_csv")) 

list_events <- list.files(pattern = "*events.csv")
list_samples <- list.files(pattern = "*data.csv")

## read files
read_events <- lapply(list_events, read.csv)
read_samples <- lapply(list_samples, read.csv)

## rename with participant id
names(read_events) <- gsub(list_events, 
                           pattern = "_.*",   #find characters after the underscore
                           replacement = "")

names(read_samples) <- gsub(list_samples, 
                         pattern = "_.*", 
                         replacement = "")

## merge rows and add id (from file name) as identifier
events <- dplyr::bind_rows(read_events, .id = "id")
samples <- dplyr::bind_rows(read_samples, .id = "id")
head(events)

# adding relevant information (parameters from article and matlab script)
## events
events_clean <- 
  events %>% 
  tidyr::separate(event,  
                  c("experiment", "counter_balance", "block", "trial", "reward_side", "event")
                  ) %>%
  dplyr::mutate(
    event = if_else(counter_balance %in% c("Start", "End"), counter_balance, 
                    if_else(!reward_side %in% c("Right", "Left") & !is.na(reward_side), reward_side, 
                            if_else(experiment == "Experiment2", counter_balance,
                                    event
                                    )
                            )
                    ),
    counter_balance = if_else(counter_balance %in% c("Start", "End") | experiment == "Experiment2", 
                              as.character(NA), counter_balance),
    trial = if_else(experiment == "Experiment2", block, trial),
    block = if_else(experiment == "Experiment2", as.character(NA), block),
    reward_side = if_else(experiment == "Experiment1", as.character(NA), reward_side)
    )

write_csv(events_clean, path = here("03_output/01_wrangling/events.csv"))

## samples (parameters from matlab script)
## replace NaN with NA
is.nan.samples.frame <- function(x) do.call(cbind, lapply(x, is.nan))
samples[is.nan(samples)] = NA

samples_clean <- 
  samples %>% 
  dplyr::mutate(
    look_away = 
      dplyr::case_when(
        is.na(x_axis_1) ~ T,
        is.na(x_axis_2) ~ T,
        is.na(y_axis_1) ~ T,
        is.na(y_axis_2) ~ T,
        TRUE ~ F
        ),
    
    look_direction =
      dplyr::case_when(
        rowMeans(.[,4:5]) < 0.33 ~ "left",
        rowMeans(.[,4:5]) > 0.33 & rowMeans(.[,4:5]) < 0.66 ~ "center",
        rowMeans(.[,4:5]) > 0.66 ~ "right"
        )
    )
      
write_csv(samples_clean, path = here("03_output/01_wrangling/samples.csv"))

# merge datasets
# join without exact match.
## add "end_time"  (onset time of the next event -1)
events_join <- 
  events_clean %>% 
  dplyr::mutate(
    start_time = time,
    end_time = if_else(time == tail(time, 1), 
                       time, 
                       dplyr::lead(start_time) - 1 # disambiguate end from start
                       )
    ) %>% 
  dplyr::select(-unknown, -time)

## sanity check
events_join$start_time[2] - events_join$end_time[1] # 1
tail(samples_clean$time, 1) - tail(events_join$end_time, 1) # events is 11ms longer (11201)

## fuzzyjoin::fuzzy_left_join works beautifully on smaller subsets;
## but exceeds the 12.3 Gb available memory in my notebook;

## slice events and samples into lists 
events_list <- split(events_join, events_join$id)
samples_list <- split(samples_clean, samples_clean$id)

## join each dataframe with purrr::map2
### I am also assuming that samples before the "Start" event are due to calibration procedures
### takes ~14h to run on 20Gb ram
dataset <- 
  purrr::map2_df(samples_list, events_list, 
                 fuzzyjoin::fuzzy_left_join, by = c("time"="start_time", "time"="end_time"),
                 match_fun=list(`>=`, `<=`)
  ) %>% 
  dplyr::rename(id = id.x) %>% 
  dplyr::select(-id.y, -end_time, -start_time) %>% 
  dplyr::mutate(
    event = as.factor(if_else(is.na(event), "calibration", as.character(event))) # assuming calibration
  )

write_csv(dataset, path = here("03_output/01_wrangling/dataset.csv"))

# define target/reward side
## from article:
  # The reward was always displayed on the same side of the screen during the pre-switch trials and on the other side 
  # during the post-switch trials. The order of the cue type (AAB, ABB) and reward side (left, right) was
  # counterbalanced (i.e. each child completed one of four presentation orders). The procedure lasted around 2 min.
  # (page 6)

## Krista: 
  # "the baby should basically 100% be looking at the target once it appears on the screen, (...)
  # use this to figure out which the target side is (...)"

## subset experiment 1
data_exp1 <- 
  dataset %>% 
  dplyr::filter(experiment == "Experiment1")

## "cue type (AAB, ABB) and reward side (left, right)" was counterbalanced
unique(data_exp1$counter_balance) # 4 counterbalances

## reward/target side defined by visual inspection
## cue type? 
data_exp1 <- 
  data_exp1 %>% 
  dplyr::mutate(
    reward_side = case_when(
      counter_balance %in%  c(1, 3) & block == 1 ~ "left", 
      counter_balance %in%  c(1, 3) & block == 2 ~ "right",
      counter_balance %in%  c(2, 4) & block == 1 ~ "right", 
      counter_balance %in%  c(2, 4) & block == 2 ~ "left",
      TRUE ~ as.character(reward_side)
    )
  )

write_csv(data_exp1, here("03_output/01_wrangling/dataset_exp1.csv"))

### temporary load of data
data_exp1 <- read.csv(here("03_output/01_wrangling/dataset_exp1.csv"))

# add participant information
p_info <- read.csv(here("02_input_csv/p_info.csv"))

p_info_exp1 <- 
  p_info %>% 
  dplyr::filter(Exp.1.1 == 1)

## several checks on participant consistency information were ran (check "old code" section)

## from article: "We collected data from 102 infants (seven to nine months of age), of whom 51 were raised in ‘bilingual’ homes and 51 in ‘monolingual’ homes." (p. 4)
## Is this the final count? After excluding infants for lack o attention? 
## "Participants were recruited and tested until, for each task, we had useable data from 51 bilingual and 51 monolingual infants. 
## We defined useable data as eye-tracking data (gaze patterns) from at least 75% of the trials in the task."

## 18/06/2020 - Update information by D'Souza e-mail:
## A02 = m1102170
## A08 = m2602170
## A135b = A135
## a167b = A167 (but too old by the time the infant came to the lab - they were supposed to come earlier!)
## a70 = A70
## b149180 OR b1409170 = A52 (this is recorded as b1409170 elsewhere. Looks like my RA made a typo. This is why I have gone back to using a simpler coding system!)
## b2908170 = A51
## b300917 = A50
## m1608170 = A53

## I will upd p_info and gender to have the codes from the experimental data (not touching the experimental data)

# check if upd labels are in p_info and p_gender lists
upd_id <- c("m1102170", "m2602170", "A135", "A167", 
            "A70", "A52", "A51", "A50", "A53")

upd_id %in% p_info_exp1$Code1 # True, except A167, too old
upd_id %in% p_gender$id # True, except A02 and A08 

# merge data
## load & upd/clean gender 
p_gender <- read.csv("02_input_csv/participants_gender_from_word.csv")

p_gender_clean <- 
  p_gender %>% 
  mutate(
    id = case_when(
      id == "A135" ~ "A135b",
      id == "A167" ~ "a167b",
      id == "A70" ~ "a70",
      id == "A52" ~ "b149180",
      id == "A51" ~ "b2908170",
      id == "A50" ~ "b300917",
      id == "A53" ~ "m1608170",
      TRUE ~ as.character(id)
      ),
    id = as.factor(id)
    )

p_gender_clean$id %in% data_exp1$id

## clean participant log
p_info_exp1_clean <- 
  p_info_exp1 %>% 
  mutate(
    id = case_when(
      Code1 == "m1102170" ~ "A02",
      Code1 == "m2602170" ~ "A08",
      Code1 == "A135" ~ "A135b",
      #Code1 == "A167" ~ "a167b", # not in p_info (too old)
      Code1 == "A70" ~ "a70",
      Code1 == "A52" ~ "b149180",
      Code1 == "A51" ~ "b2908170",
      Code1 == "A50" ~ "b300917",
      Code1 == "A53" ~ "m1608170",
      TRUE ~ as.character(Code1)
    ),
    id = as.factor(id)
  ) %>% 
  clean_names(case = "snake") %>% 
  select(id, group, exposure, age_in_days, exclude, ses_score) 

p_info_exp1_clean$id %in% data_exp1$id

# merge gender & p_info
p_info_exp1_v2 <- dplyr::left_join(p_info_exp1_clean, p_gender_clean, by = "id")

## 1 row more: confirm duplicate
p_info_exp1_v2 %>% janitor::get_dupes(id) 

## clean duplicate
p_info_exp1_v3 <- p_info_exp1_v2 %>% distinct()

## keep only data that have participant info
data_exp1_clean2 <- 
  data_exp1 %>% 
  filter(id %in% p_info_exp1_v3$id)

length(unique(data_exp1_clean2$id)) # experimental after exclusions = 102

## A90 was missing, but I found it (see old code section)

## backup data
save(data_exp1_clean2, file = here("03_output/01_wrangling/data_exp1_clean.rda"))
save(p_info_exp1_v3, file = here("03_output/01_wrangling/participant_info_exp1_v3.rda"))
write_csv(data_exp1_clean2, here("03_output/01_wrangling/dataset_exp1_clean.csv"))
write_csv(p_info_exp1_v3, here("03_output/01_wrangling/participant_info_exp1_v3.csv"))    

load("03_output/01_wrangling/data_exp1_clean.rda")
load("03_output/01_wrangling/participant_info_exp1_v3.rda")

## merge
dataset_exp1 <- dplyr::left_join(data_exp1_clean2, p_info_exp1_v3, by = "id") # 95 participants

length(unique(dataset_exp1$id)) # 102

# descriptive statistics + data formatting
## format variables to match our analysis script
dataset_exp1_v2 <- 
  dataset_exp1 %>% 
  mutate(
    language = if_else(group == "M", "Monolinguals", "Bilinguals"),
    gender = if_else(gender == "F", 1, 0),
    trial_type = case_when(
      block == 1 ~ "pre-switch", 
      block == 2 ~ "post-switch",
      TRUE ~ as.character(block)
      )
  ) %>% 
  select(-group, -block) # get rid of transformed variables

length(unique(dataset_exp1_v2$id)) # 102

save(dataset_exp1_v2, file = here("/03_output/01_wrangling/dataset_exp1_v2.rda")) # backup

## additional variables from CogControl analysis script
trial_name_levels <- c("pre-switch_1", "pre-switch_2", "pre-switch_3", 
                       "pre-switch_4", "pre-switch_5", "pre-switch_6", 
                       "pre-switch_7", "pre-switch_8", "pre-switch_9", 
                       "post-switch_1", "post-switch_2", "post-switch_3", 
                       "post-switch_4", "post-switch_5", "post-switch_6", 
                       "post-switch_7", "post-switch_8", "post-switch_9")

dataset_exp1_v3 <- 
  dataset_exp1_v2 %>% 
  rename(trial_num = trial) %>% 
  mutate(
    circle = case_when(as.character(look_direction) == "center" ~ 1,  TRUE ~ 0),
    target = case_when(as.character(look_direction) == as.character(reward_side) ~ 1, TRUE ~ 0),
    distractor = case_when(as.character(look_direction) != as.character(reward_side) & as.character(look_direction) != "center" ~ 1, 
                           TRUE ~ 0),
    trackloss = case_when(target == 1 | distractor == 1 | circle == 1 ~ FALSE, TRUE ~ TRUE), # there is divergence between gaze coordinates & trackloss column from matlab; building our own
    ) %>% 
  filter(!event %in% c("Start", "End")) %>% # get rid of NA in the beginning and end
  mutate(trial_name = factor(str_c(trial_type, trial_num, sep = "_"), levels = trial_name_levels)) %>% 
  group_by(id, trial_name) %>%
  mutate(
    time = round(time/1000), # get the rounded ms
    trial_from_zero = time - min(time) # make time stamps start from 0 for each trial
    ) %>% 
  ungroup() %>% 
  mutate(
    language = factor(language, levels = c("Monolinguals", "Bilinguals")),
    trial_type = factor(trial_type, levels = c("pre-switch", "post-switch")),
    gender = factor(gender, levels = c(0, 1)),
    id = as.factor(id)
  )

length(unique(dataset_exp1_v3$id)) # 102

save(dataset_exp1_v3, file = here("/03_output/01_wrangling/dataset_exp1_v3.rda")) 
write_csv(dataset_exp1_v3, here("03_output/01_wrangling/dataset_exp1_v3.csv")) # backup


## Define minimum looking and minimum trial numbers for this study
## from "load_merge.R", added in 09/07/2020
MIN_LOOK_PROPORTION <- .5 # 50% of looking
MIN_NUMBER_TRIALS <- 5 # out of 9

data_anticipation <- 
  dataset_exp1_v3 %>%
  mutate(
    look_any = case_when(target == 1 | distractor == 1 | circle == 1 ~ 1, TRUE ~ 0), #looks to either target, distractor, or circle  
    trial_unique = as.factor(str_c(id, trial_name, sep = "_")) #get a trial identifier that is unique to each participant, trial number, and trial type
    ) %>% 
  filter(trial_from_zero >= 3200 & trial_from_zero <= 4200) %>% #D'souza aniticipatory period, which is 150 ms after the offset of the 2000 ms cue, and lasts for 1000 ms
  group_by(trial_unique) %>%
  mutate(prop_fixations = mean(look_any)) %>%
  filter(prop_fixations >= MIN_LOOK_PROPORTION) %>% #removes trials where the child did not fixate on anything for at least 50% of the anticipation period
  group_by(id, trial_type) %>%
  mutate(num_good_trials = length(unique(trial_num))) %>%
  filter(num_good_trials >= MIN_NUMBER_TRIALS) %>% #removes babies who don't have the minimum number of trials 
  group_by(id) %>%
  mutate(num_trial_types = length(unique(trial_type))) %>%
  filter(num_trial_types == 2) %>% #removes babies who don't have data in both trial types
  ungroup()


save(data_anticipation, file = here("03_output/01_wrangling/data_anticipation.rda"))
write_csv(data_anticipation, here("03_output/01_wrangling/data_anticipation.csv")) # backup

#used for creating data_full_trials object for plotting whole trial
final_sample_ids <- 
  data_anticipation %>% 
  select(id) %>%
  unique() # 51 left

# we don't need the "final_sample_mslist" df as it only adds vocab measures

# this is the full time series data, but only for infants that made it through to the final anticipation-period sample. Will be used for ploting time series data of whole trial
data_full_trials <- 
  inner_join(dataset_exp1_v3, final_sample_ids, by = "id") %>%
  mutate(id = as.factor(id)) 


save(data_full_trials, file = here("03_output/01_wrangling/data_full_trials.rda"))
write_csv(data_full_trials, here("03_output/01_wrangling/data_full_trials.csv")) # backup


# for future wranglings
load(file = here("/03_output/01_wrangling/dataset_exp1_v2.rda"))


########################################################################### OLD CODE ###########################################################################

## this should be easy, but "time" does not match between datasets!
dataset <- dplyr::left_join(samples_clean, events_clean, by = c("id", "time"))

## further checking
### Is samples total time greater than events total time?
### We just need 1 participant
samples_A02 <- 
  samples_clean %>% 
  dplyr::filter(id == "A02")

events_A02 <- 
  events_clean %>% 
  dplyr::filter(id == "A02")

## samples timeline vs event timeline
tail(samples_A02$time, 1) > tail(events_A02$time, 1) # False
head(samples_A02$time, 1) - head(events_A02$time, 1) # samples starts 65.287.108 (65sec & 287ms)  before samples = calibration? 
tail(samples_A02$time, 1) - tail(events_A02$time, 1) # events is 11.201 (11ms) longer at the end = pratically together.

## the "unkonown" maybe an adjustment between timelines?
## try differences between time and "unknown" variable in events - IGNORE Unknown
tail(events_A02$unknown, 1) # 23559839187 ms 
sprintf("%.1f", tail(events_A02$time, 1) - tail(events_A02$unknown, 1)) # events time - events unknown = 1509424791634423 ms
tail(samples_A02$time, 1) - (tail(events_A02$time, 1) - tail(events_A02$unknown, 1)) # unknown has nothing to do with anything

# fuzzy left join
dataset_raw <- 
  fuzzyjoin::fuzzy_left_join(samples_clean, events_join, 
                             by=c("id"="id", "time"="start_time", "time"="end_time"),
                             match_fun=list(`==`, `>=`, `<=`)
  ) %>% 
  dplyr::rename(id = id.x) %>% 
  dplyr::select(-id.y, -end_time, -start_time)


## ids are consistent between data and log?
id_exp1 <- levels(data_exp1$id)
id_log_code1 <- levels(p_info_exp1$Code1) 
id_log_code2 <- levels(p_info_exp1$Code2)

## Participants in exp data but NOT in log
## Use Code 1 or Code 2?
## code 1
sum(!id_exp1 %in% id_log_code1) # 10 entries do not match
not_in_code1 <- setdiff(id_exp1, id_log_code1) 

## code 2
sum(!id_exp1 %in% id_log_code2) # 46 entries do not match
not_in_code2 <- setdiff(id_exp1, id_log_code2) 

## stick with code 1
## can we find missing Code 1 in Code 2?
miss_log <- 
  p_info_exp1 %>% 
  filter(Code2 %in% not_in_code1) # A02, A08 are not missing anymore

not_in_code1_2 <- not_in_code1[3:10] # still missing

## Participants in log but NOT in exp data
setdiff(id_log_code1, id_exp1) # 26
setdiff(id_log_code2, id_exp1) # 62

## check exclusions 
## each entry from the "Laboratory log v2" (from OSF):
# "28 Nov 2017: Two infants we recruited have erroneously been given the same code: b1504170. 
# Data from these two infants will be stored in separate places, to avoid confusion.
# "12 Dec 2018: Data from Participant A79 collected using the “main” script was erroneously 
# coded as A78 and overwrote the previous data. They have been recoded. 
# The Gap data were coded correctly from the start and unaffected by this error."

excl_osf <- c("m0403170", "A100", "A162", "A167", "A170")
to_exclude <- intersect(excl_osf, id_exp1) # to exclude latter
sum(excl_osf %in% not_in_code1_2) # 0 none match the missing ones

## "Main data errouneously coded as A78"
"A79" %in% id_exp1 # False

## check if our ids match with matlab (processed) ids
mat_exp1 <- read.csv(here("extras/Expt1Output_20200514T090509.csv"))
mat_id <- unique(mat_exp1$Participant.ID) # 158 ids

setdiff(id_exp1, mat_id) # no difference between our id and mat id
setdiff(mat_id, id_exp1) # 4 differences
# inspection of id events file: A125 = Exp2 & 3; A156 = Exp3; A81 = Exp2 & 3; A90 = Exp2 & 3;

intersect(mat_id, not_in_code1_2) # all missing ids are also on mat id

## When inspecting the R script from D'Souza it seems that they have an "intermediary" script for further processing the data from matlab before running the analysis;
## For instance, they analyze grouping variables that are absence in the matlab output data. 

## I will exclude the participants with missing information
exclude = c(to_exclude, not_in_code1_2) # 11 participants = 6 for missing information + 5 from OSF exclusion criteria 

data_exp1_clean <- 
  data_exp1 %>% 
  filter(!id %in% exclude)

## excluded samples
nrow(data_exp1) - nrow(data_exp1_clean)

## final number of participants
length(unique(data_exp1_clean$id)) # experimental after exclusions = 143

## compare to 102 from participants log
length(p_info_exp1$Code1)  

# In search of A90
setdiff(p_info_exp1_v3$id, data_exp1$id) # A90 is missing

## in search of "A90"
x = read.csv(here("03_output/01_wrangling/dataset.csv")) # full dataset
"A90" %in% x$id # true
y = x %>% filter(id == "A90") 
"Experiment1" %in% y$experiment # False! A90 is NOT a participant of Exp1
