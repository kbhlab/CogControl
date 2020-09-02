# Article: Iliaei, S. P., Killam, H., Dal Ben, R., & Byers-Heinlein, K. (under review). Bilingualism affects infant cognition: Insights from new and open data
# Author: Hilary Killam
# Last update: 08/19/2020 
# Aim: script to create raw datasets from Kalashnikova et al. (2020) data.
# To recreate this analysis, you will need to download the data available on 
# Kalashnikova et al. (2020) OSF repository as of 04/14/2020

# Feedback and suggestions: <kbh.coordinator@concordia.ca>

# References: 
# Kalashnikova et al. (2020)
# article: https://doi.org/10.1111/desc.13011
# OSF: https://osf.io/k3e9z/

# Setup
library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(tidylog)
library(here)

`%notin%` <- Negate(`%in%`)

##Load Data
baby_info <- 
  read_excel(here("input/Metadata.xlsx")) %>% 
  clean_names() %>%
  mutate(group = case_when(group == "monolingual sp" ~ "monolingual",
                           group == "monoligual" ~ "monolingual",
                           TRUE ~ group),
         visual_tl = case_when(id == "5314" ~ auditory_tl, #looks like this kid had a mistake in the baby info sheet... they have data for auditory trackloss but no data for visual trackloss, yet they only participated in the visual session. I double checked the trackloss number for the visual session, and it matches what was put under auditory, so I think they should be swapped.
                               TRUE ~ visual_tl),
         auditory_tl = case_when(id == "5314" ~ NA_real_,
                                 TRUE ~ auditory_tl),
         # calculate age
         do_t_s2 = excel_numeric_to_date(as.numeric(do_t_s2)),
         do_b = lubridate::ymd(do_b),
         do_b = case_when(id == 5629 ~ ymd(2019-07-29),
                          TRUE ~ do_b),
         do_t_s1 = lubridate::ymd(do_t_s1),
         do_t_s2 = lubridate:: ymd(do_t_s2),
         age_interval_s1 = lubridate::interval(do_b, do_t_s1),
         age_interval_s2 = lubridate::interval(do_b, do_t_s2),
         age_at_session_1 = lubridate::as.period(age_interval_s1),
         age_at_session_2 = lubridate::as.period(age_interval_s2),
         months_at_session_1 = lubridate::month(age_at_session_1),
         days_at_session_1 = lubridate::day(age_at_session_1),
         months_at_session_2 = lubridate::month(age_at_session_2),
         days_at_session_2 = lubridate::day(age_at_session_2),
         age_auditory = case_when(task_order == "Aud" ~ age_at_session_1, 
                                  TRUE ~ age_at_session_2),
         age_visual = case_when(task_order == "Vis" ~ age_at_session_1,
                                TRUE ~ age_at_session_2),
         months_auditory = case_when(task_order == "Aud" ~ months_at_session_1,
                                     TRUE ~ months_at_session_2),
         days_auditory = case_when(task_order == "Aud" ~ as.numeric(days_at_session_1),
                                   TRUE ~ as.numeric(days_at_session_2)),
         age_in_days_auditory = 30.436875 * months_auditory + days_auditory,
         months_visual = case_when(task_order == "Vis" ~ months_at_session_1,
                                   TRUE ~ months_at_session_2),
         days_visual = case_when(task_order == "Vis" ~ as.numeric(days_at_session_1),
                                 TRUE ~ as.numeric(days_at_session_2)),
         age_in_days_visual = 30.436875 * months_visual + days_visual)
  
exclusions <- 
  read_excel(here::here("input", "Metadata.xlsx"), sheet = 2) %>% 
  clean_names() %>%
  mutate(id = as.character(id))

aud_left <- 
  read_csv(here("input", "auditory_left_FA.csv")) %>% 
  clean_names() %>% 
  mutate(target_side = case_when(parse_number(trial_label) <= 12 ~ "left",
                                 TRUE ~ "right"),
         orig_file = "aud_left") %>% 
  select(-x1)

aud_right <- 
  read_csv(here("input", "auditory_right_FA.csv")) %>% 
  clean_names() %>% 
  mutate(target_side = case_when(parse_number(trial_label) <= 12 ~ "right",
                                 TRUE ~ "left"),
         orig_file = "aud_right") %>% 
  select(-x1)

#looks like trials 9 and 17 have extra data from an attention getter so have to shift time stamps for those trials
vis_left <- 
  read_csv(here("input", "visual_left_FA.csv")) %>% 
  clean_names() %>% 
  mutate(target_side = case_when(parse_number(trial_label) <= 12 ~ "left",
                                 TRUE ~ "right"),
         orig_file = "vis_left") %>% 
  select(-x1, -ia_4_id) %>%
  rename(original_start_time = bin_start_time) %>%
  mutate(bin_start_time = case_when(trial_label == "Trial: 9" | trial_label == "Trial: 17" ~ original_start_time - 2110,                                   
                                    TRUE ~ original_start_time))

vis_right <- 
  read_csv(here("input", "visual_right_FA.csv")) %>% 
  clean_names() %>% 
  mutate(target_side = case_when(parse_number(trial_label) <= 12 ~ "right",
                                 TRUE ~ "left"),
         orig_file = "vis_right") %>% 
  select(-x1) %>%
  mutate(original_start_time = bin_start_time) #add this column so can merge with vis_left later

## Prep data
###-----------------------------------------------------------PREPARE UNFILTERED VISUAL DATA

vis_all <- 
  vis_left %>%
  dplyr::union(vis_right) %>%
  filter(bin_start_time > 0 & bin_start_time < 6740) %>%
  mutate(id = str_remove(id, "icv"),
         id = str_remove(id, "ica"),
         id = case_when(str_detect(id, "5332") ~ "5532", #typo in eyetracking ID
                        TRUE ~ id),
         trial_num = parse_number(trial_label),
         trial_type = case_when(trial_num <= 12 ~ "pre-switch",
                                TRUE ~ "post-switch"),
         trial_num = case_when(trial_num > 12 ~ trial_num - 12,
                               TRUE ~ trial_num),
         trial_type = factor(trial_type, levels = c("pre-switch", "post-switch")),
         target = case_when(target_side == "left" ~ left,
                            target_side == "right" ~ right),
         distractor = case_when(target_side == "left" ~ right,
                                target_side == "right" ~ left),
         trial_name = str_c(trial_type, trial_num, sep = "_"),
         trial_unique = str_c(id, trial_name, sep = "_")) %>%
  inner_join(baby_info, by = "id") %>%
  mutate(id = as.factor(id),
         trial_num = as.numeric(trial_num),
         trial_name = as.factor(trial_name),
         group = factor(group, levels = c("monolingual", "bilingual"))) %>%
  filter(id %notin% exclusions$id) %>%
  filter(visual_tl <= 0.6) %>% #kalashnikova's original trackloss threshold
  mutate(trackloss = loss,
         circle = center,
         age_in_days = age_in_days_visual,
         gender = factor(
           case_when(sex == "FEMALE" ~ 1,
                            TRUE ~ 0)),
         language = factor(paste0(str_to_title(group), "s"), levels = c("Monolinguals", "Bilinguals"))
         )

#check for babies missing info:

vis_all %>% select(id) %>% distinct() %>% anti_join(baby_info, by = "id")
baby_info %>% anti_join((vis_all %>% select(id) %>% distinct()), by = "id")

#two kids in eyetracking data but not info data

###-----------------------------------------------------------PREPARE FILTERED VISUAL DATA
MIN_LOOK_PROPORTION <- .5 # 50% of looking
MIN_NUMBER_TRIALS <- 7 # out of 12
ANTICIP_START_OFF <- 3500 + 150 # anticipation start + offset 
ANTICIP_END_OFF <- 4500 + 150 # anticipation start + offset 
BOTH_PHASES <- 2 # data in Training and Test 

vis_all_filtered <- 
  vis_all %>%
  mutate(look_any = case_when(target == TRUE | distractor == TRUE | circle == TRUE ~ 1, TRUE ~ 0)) %>% 
  filter(bin_start_time >= ANTICIP_START_OFF & bin_start_time <= ANTICIP_END_OFF) %>%
  group_by(trial_unique) %>%
  mutate(prop_fixations = mean(look_any)) %>% # trackloss column
  filter(prop_fixations >= MIN_LOOK_PROPORTION) %>% #removes trials where the child did not fixate on anything for at least 50% of the anticipation period
  group_by(id, trial_type) %>%
  mutate(num_good_trials = length(unique(trial_num))) %>%
  filter(num_good_trials >= MIN_NUMBER_TRIALS) %>% #removes babies who don't have the minimum number of trials 
  group_by(id) %>%
  mutate(num_trial_types = length(unique(trial_type))) %>%
  filter(num_trial_types == BOTH_PHASES) %>% #removes babies who don't have data in both trial types
  ungroup()


### PROBLEM HERE - 1 BILINGUAL IS MISSING COME BACK HERE!!! 
vis_all_filtered %>% 
select(id, group) %>%
  distinct() %>%
  count(group)

###-----------------------------------------------------------SAVE VISUAL DATA

data_full_trials_filtered_1c <- vis_all_filtered
save(data_full_trials_filtered_1c, file = here("output/data/data_full_trials_filtered_1c.rda"))

data_full_trials_unfiltered_1c <- vis_all
save(data_full_trials_unfiltered_1c, file = here("output/data/data_full_trials_unfiltered_1c.rda"))

###-----------------------------------------------------------PREPARE UNFILTERED AUDITORY DATA
aud_all <- 
  aud_left %>%
  dplyr::union(aud_right) %>% #merge left and right dataframes
  filter(bin_start_time < 5400 & bin_start_time > 0) %>% #don't need anything after this point, since most trials have no data afterward 
  mutate(id = str_remove(id, "ica"), #fix some id matching problems
         id = str_remove(id, "ic"),
         id = str_remove(id, "ca"),
         id = case_when(id == "524401" ~ "5244",
                        id == "53002" ~ "5300",
                        id == "5588a" ~ "5588",
                        TRUE ~ id),
         trial_num = parse_number(trial_label), #get trial number
         trial_type = case_when(trial_num <= 12 ~ "pre-switch",
                                TRUE ~ "post-switch"),
         trial_num = case_when(trial_num > 12 ~ trial_num - 12, #make 2 sets of 12 trials, instead of 1 to 24
                               TRUE ~ trial_num),
         trial_type = factor(trial_type, levels = c("pre-switch", "post-switch")),
         target = case_when(target_side == "left" ~ left, #set AOI columns
                            target_side == "right" ~ right),
         distractor = case_when(target_side == "left" ~ right,
                                target_side == "right" ~ left),
         trial_name = str_c(trial_type, trial_num, sep = "_"),
         trial_unique = str_c(id, trial_name, sep = "_")) %>%
  inner_join(baby_info, by = "id") %>% #merge with participant info
  mutate(id = as.factor(id),
         trial_num = as.numeric(trial_num),
         trial_name = as.factor(trial_name),
         group = factor(group, levels = c("monolingual", "bilingual"))) %>%
  filter(id %notin% exclusions$id) %>% #get rid of any kids that match an ID in the exclusions file
  filter(auditory_tl <= 0.6) %>% #filter for Kalashnikova's trackloss
  mutate(trackloss = loss,
         circle = center,
         age_in_days = age_in_days_visual,
         gender = factor(
           case_when(sex == "FEMALE" ~ 1,
                     TRUE ~ 0)),
         language = factor(paste0(str_to_title(group), "s"), levels = c("Monolinguals", "Bilinguals"))  
         )

###-----------------------------------------------------------PREPARE FILTERED AUDITORY DATA
MIN_LOOK_PROPORTION <- .5 # 50% of looking
MIN_NUMBER_TRIALS <- 7 # out of 12
ANTICIP_START_OFF <- 2200 + 150 # anticipation start + offset 
ANTICIP_END_OFF <- 3200 + 150 # anticipation start + offset 
BOTH_PHASES <- 2 # data in Training and Test 

aud_all_filtered <- 
  aud_all %>%
  mutate(look_any = case_when(target == TRUE | distractor == TRUE | circle == TRUE ~ 1, TRUE ~ 0)) %>% 
  filter(bin_start_time >= ANTICIP_START_OFF & bin_start_time <= ANTICIP_END_OFF) %>%
  group_by(trial_unique) %>%
  mutate(prop_fixations = mean(look_any)) %>% # trackloss column
  filter(prop_fixations >= MIN_LOOK_PROPORTION) %>% #removes trials where the child did not fixate on anything for at least 50% of the anticipation period
  group_by(id, trial_type) %>%
  mutate(num_good_trials = length(unique(trial_num))) %>%
  filter(num_good_trials >= MIN_NUMBER_TRIALS) %>% #removes babies who don't have the minimum number of trials 
  group_by(id) %>%
  mutate(num_trial_types = length(unique(trial_type))) %>%
  filter(num_trial_types == BOTH_PHASES) %>% #removes babies who don't have data in both trial types
  ungroup()

# double check filtering
aud_all_filtered %>% 
  select(id, group) %>%
  distinct() %>%
  count(group)

###-----------------------------------------------------------SAVE AUDITORY DATA

data_full_trials_filtered_1d <- aud_all_filtered
save(data_full_trials_filtered_1d, file = here("output/data/data_full_trials_filtered_1d.rda"))

data_full_trials_unfiltered_1d <- aud_all
save(data_full_trials_unfiltered_1d, file = here("output/data/data_full_trials_unfiltered_1d.rda"))

##### THE END #####