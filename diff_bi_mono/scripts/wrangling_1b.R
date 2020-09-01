# Article: Iliaei, S. P., Killam, H., Dal Ben, R., & Byers-Heinlein, K. (under review). Bilingualism affects infant cognition: Insights from new and open data
# Author: Rodrigo Dal Ben
# Last update: 08/19/2020 
# Aim: script to create raw datasets from D'Souza et al. (2020) individual csv files.
# See the "step-by-step" to recreate this analysis. 
#   In summary, if you want to follow every step, you will need to begin with the .mat files available on D'Souza et al. Dryad repository (link below).
#   If you want to check the final raw dataset, you will find it on D'Souza et al. OSF repository (link below).
# Feedback and suggestions: <dalbenwork@gmail.com>

# References: 
# D'Souza et al. (2020)
# Article: https://doi.org/10.1098/rsos.180191
# OSF: https://osf.io/53gh2/
# Dryad: https://doi.org/10.5061/dryad.3n5tb2rc6 

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

## fuzzyjoin::fuzzy_left_join works beautifully on smaller subsets;
## but exceeds the 12.3 Gb available memory in the current computer;

## slice events and samples into lists 
events_list <- split(events_join, events_join$id)
samples_list <- split(samples_clean, samples_clean$id)

## join each dataframe with purrr::map2
### Assume that samples before the "Start" event are due to calibration procedures
### takes ~14h to run on 20Gb ram
dataset <- 
  purrr::map2_df(samples_list, events_list, 
                 fuzzyjoin::fuzzy_left_join, by = c("time"="start_time", "time"="end_time"),
                 match_fun=list(`>=`, `<=`)
  ) %>% 
  dplyr::rename(id = id.x) %>% 
  dplyr::select(-id.y, -end_time, -start_time) %>% 
  dplyr::mutate(
    event = as.factor(if_else(is.na(event), "fixation", as.character(event))) 
  )

write_csv(dataset, path = here("03_output/01_wrangling/dataset.csv"))

# define target/reward side
## from article:
  # The reward was always displayed on the same side of the screen during the pre-switch trials and on the other side 
  # during the post-switch trials. The order of the cue type (AAB, ABB) and reward side (left, right) was
  # counterbalanced (i.e. each child completed one of four presentation orders). The procedure lasted around 2 min.
  # (page 6)

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

# add participant information
p_info <- read.csv(here("02_input_csv/p_info.csv"))

p_info_exp1 <- 
  p_info %>% 
  dplyr::filter(Exp.1.1 == 1)

## several checks on participant consistency information were ran (check "old code" section)

## from article: "We collected data from 102 infants (seven to nine months of age), of whom 51 were raised in bilingual homes and 51 in monolingual homes..." 
## "Participants were recruited and tested until, for each task, we had useable data from 51 bilingual and 51 monolingual infants..." 
## "We defined useable data as eye-tracking data (gaze patterns) from at least 75% of the trials in the task..." (page 4)

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
dataset_exp1 <- dplyr::left_join(data_exp1_clean2, p_info_exp1_v3, by = "id") 

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
  filter(!event %in% c("Start", "End")) %>% # delete samples at "Start" and "End" messages (from each phase)
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

# make sure that the antcipation period is aligned for all participants
## because of the "fuzzyjoin" between samples' and events' timelines.
## start by taking some statistics
ap <- 
  dataset_exp1_v3 %>% 
  filter(event == "anticipOnset") %>% 
  select(id, event, trial_from_zero, trial_name) %>% 
  group_by(id, event, trial_name) %>% 
  summarise(
    ap_start = min(trial_from_zero),
    ap_end = max(trial_from_zero),
    duration = ap_end - ap_start
  ) %>% 
  ungroup()

## start
min(ap$ap_start) # 3008
max(ap$ap_start) # 3099
mean(ap$ap_start) # 3028
median(ap$ap_start) # 3033

## end
min(ap$ap_end) # 4007
max(ap$ap_end) # 4141
mean(ap$ap_end) # 4035
median(ap$ap_end) # 4041

## duration
min(ap$duration) # 983
max(ap$duration) # 1042
mean(ap$duration) # 1006
median(ap$duration) # 1008

# align anticipation period (first "anticipOnset" message) start at 3000
dataset_exp1_v3 <- 
  dataset_exp1_v3 %>% 
  group_by(id, trial_name, event) %>% 
  mutate(
    sample_num = row_number(),
    diff_timeline = if_else(event == "anticipOnset" & sample_num == 1,
                            3000 - trial_from_zero,
                            as.numeric(NA)
                            )
  ) %>% 
  ungroup() %>% 
  group_by(id, trial_name) %>% 
  mutate(time_align = trial_from_zero - abs(mean(diff_timeline, na.rm = T))) %>% 
  select(-sample_num, -diff_timeline) %>% # comment this line for sanity check
  ungroup()

length(unique(dataset_exp1_v3$id)) # 102

data_full_trials_unfiltered_1b <- dataset_exp1_v3

save(data_full_trials_unfiltered_1b, file = here("/output/data/data_full_trials_unfiltered_1b.rda")) 

## Define minimum looking and minimum trial numbers for this study
## from "load_merge.R", added in 09/07/2020
MIN_LOOK_PROPORTION <- .5 # 50% of looking
MIN_NUMBER_TRIALS <- 5 # out of 9
ANTICIP_START_OFF <- 3000 + 150 # anticipation start + offset 
ANTICIP_END_OFF <- 4000 + 150 # anticipation start + offset
BOTH_PHASES <- 2 # data in Training and Test 

data_anticipation <- 
  dataset_exp1_v3 %>%
  mutate(
    look_any = case_when(target == 1 | distractor == 1 | circle == 1 ~ 1, TRUE ~ 0), #looks to either target, distractor, or circle  
    trial_unique = as.factor(str_c(id, trial_name, sep = "_")) #get a trial identifier that is unique to each participant, trial number, and trial type
    ) %>% 
  filter(time_align >= ANTICIP_START_OFF & time_align <= ANTICIP_END_OFF) %>% #D'souza aniticipatory period, which is 150 ms after the offset
  group_by(trial_unique) %>%
  mutate(prop_fixations = mean(look_any)) %>%
  filter(prop_fixations >= MIN_LOOK_PROPORTION) %>% #removes trials where the child did not fixate on anything for at least 50% of the anticipation period
  group_by(id, trial_type) %>%
  mutate(num_good_trials = length(unique(trial_num))) %>%
  filter(num_good_trials >= MIN_NUMBER_TRIALS) %>% #removes babies who don't have the minimum number of trials 
  group_by(id) %>%
  mutate(num_trial_types = length(unique(trial_type))) %>%
  filter(num_trial_types == BOTH_PHASES) %>% #removes babies who don't have data in both trial types
  ungroup()

data_anticipation_filtered_1b <- data_anticipation

save(data_anticipation_filtered_1b, file = here("output/data/data_anticipation_filtered_1b.rda"))

# used for creating data_full_trials object for plotting whole trial
final_sample_ids <- 
  data_anticipation %>% 
  select(id) %>%
  unique() # 53 left (49 gone)

# this is the full time series data, but only for infants that made it through to the final anticipation-period sample
data_full_trials <- 
  inner_join(dataset_exp1_v3, final_sample_ids, by = "id") %>%
  mutate(id = as.factor(id))

data_full_trials_filtered_1b <- data_full_trials

save(data_full_trials_filtered_1b, file = here("output/data/data_full_trials_filtered_1b.rda"))

###### THE END ######