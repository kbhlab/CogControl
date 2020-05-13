

## Load packages
library(tidyverse)
library(readxl)
library(stringr)
library(tidyr)
library(ggplot2)
library(car)
library(ez)
library(snakecase)
library(janitor)
library(lme4)
library(effects)
library(emmeans)
library(apaTables)
library(here)
library(eyetrackingR)
library(ggeffects)
library(interactions)
library(tidylog)
options(scipen = 999)

kovacs09 <- "Kovács & Mehler 2009"

## Define minimum looking and minimum trial numbers for this study
MIN_LOOK_PROPORTION <- .5
MIN_NUMBER_TRIALS <- 5

###-----------------------------------------------------------LOAD DATA--------------------------------------------------------

load(here("data", "tobii_full_export.Rda")) #full export data is saved as RDA file since the csv is too large for GitHub

##parent education data is separate
edu_7 <- read_csv(here("data", "cogcontrol_7_parent_ed.csv")) %>%
  mutate(recording_name = case_when(recording_name == "CogControl7_S16_45233" ~ "CogControl7_S16_45314",
                                    recording_name == "CogControl7_S28_45244" ~ "CogControl7_S28_45241",
                                    recording_name == "CogControl7_S108_48416" ~ "CogControl7_S108_49416",
                                    TRUE ~ recording_name))
edu_20 <- read_csv(here("data", "cogcontrol_20_parent_ed.csv")) %>%
  mutate(recording_name = case_when(recording_name == "CogControl20_S116_48735" ~ "CogControl20_S116_48375",
                                    TRUE ~ recording_name))

edu_all <- bind_rows(edu_7, edu_20)

## Read in MSL data
ms_list_all <- read_csv(here("data", "2019_CogControl_MSL.csv"), na = "-") %>%
  separate(lang.group,into = c("language", "group")) %>%
  clean_names() %>%
  left_join(edu_all, by = "recording_name") %>%
  mutate(mother_edu = case_when(parent_A_gender == "F" ~ parent_A_education,
                                parent_B_gender == "F" ~ parent_B_education,
                                TRUE ~ NA_character_)) %>%
  mutate(mother_edu = case_when(mother_edu == "some_university" | mother_edu == "trade_school" | mother_edu == "college" | mother_edu == "professional" | mother_edu == "attestation_of_college" ~ "post-secondary",
                                mother_edu == "masters" | mother_edu == "doctoral" ~ "advanced_degree",
                                TRUE ~ mother_edu))

ms_list <- ms_list_all %>%
  filter(keeper == 1) 


###--------------------------------------------------------PREP MSL DATA------------------------------------------------------

ms_list <- ms_list %>%  
  mutate(age_group = fct_rev(as.factor(age_group))) %>%
  mutate(language = fct_rev(as.factor(language))) %>%
  mutate(total_vocab_prod = as.numeric(total_vocab_prod)) %>%
  mutate(total_concept_prod = as.numeric(total_concept_prod)) %>%
  mutate(recording_name = as_factor(recording_name)) %>%
  mutate(gender = as.factor(gender)) %>%
  mutate(do_birth = excel_numeric_to_date(as.numeric(as.character(do_birth)), date_system = "modern")) %>%
  mutate(do_participation = excel_numeric_to_date(as.numeric(as.character(do_participation)), date_system = "modern")) %>%
  mutate(age_days_r = do_participation - do_birth) %>%
  mutate(num_cdis = case_when(is.na(cdi_prod_eng) & is.na(cdi_prod_fr) ~ 0,
                              is.na(cdi_prod_eng) | is.na(cdi_prod_fr) ~ 1,
                              TRUE ~ 2)) %>%
  mutate(cdi_prod_fr = as.numeric(cdi_prod_fr)) %>%
  mutate(cdi_tot_vocab_prod = case_when(language == "Monolinguals" & dom_lang == "french" ~ cdi_prod_fr,
                                        language == "Monolinguals" & dom_lang == "english" ~ cdi_prod_eng,
                                        language == "Bilinguals" & num_cdis == 2 ~ total_vocab_prod))


###-----------------------------------------------------------------PREP TOBII DATA----------------------------------------------

###-------------------------------------------------------------------set factor levels:

trial_name_levels <- c("pre-switch_1", "pre-switch_2", "pre-switch_3", "pre-switch_4", "pre-switch_5", "pre-switch_6", "pre-switch_7", "pre-switch_8", "pre-switch_9", "post-switch_1", "post-switch_2", "post-switch_3", "post-switch_4", "post-switch_5", "post-switch_6", "post-switch_7", "post-switch_8", "post-switch_9")


data_tobii_cleaned <- tobii_full_export %>%
  #head(n = 2500) %>% # uncomment this line to test code changes on a subset of the full dataset since running the whole thing takes awhile
  select(StudioTestName, RecordingName, RecordingDuration, MediaName, RecordingTimestamp, StudioEvent, FixationIndex, GazeEventType, GazeEventDuration, `GazePointX (ADCSpx)`, `GazePointY (ADCSpx)`, ValidityLeft, ValidityRight) %>% # choose only needed columns
  mutate(RecordingName = case_when(RecordingName == "Cogcontrol20_S116_48375" ~ "CogControl20_S116_48375",
                                   RecordingName == "CogControl7_S108_48416" ~ "CogControl7_S108_49416",
                                   TRUE ~ RecordingName)) %>% #fix typo from Tobii
  rename(gaze_point_X = `GazePointX (ADCSpx)`, gaze_point_y = `GazePointY (ADCSpx)`, timestamp = `RecordingTimestamp`, order = StudioTestName) %>% #fix names (newname = oldname)
  mutate(MediaName = gsub(".wmv", "", MediaName)) %>% # drops the .wmv part.
  mutate(trial_num = parse_number(MediaName)) %>% #get trial number
  mutate(trial_type = factor(case_when(str_detect(MediaName, "Training") ~ "pre-switch", #get trial type
                                       str_detect(MediaName, "Test") ~ "post-switch"), levels = c("pre-switch", "post-switch"))) %>% #get trial type
  mutate(trial_name = factor(str_c(trial_type, trial_num, sep = "_"), levels = trial_name_levels)) %>% #combine trial number and trial type into a trial name (for example, "post-switch_3"")
  mutate(trial_unique = as.factor(str_c(RecordingName, trial_name, sep = "_"))) %>% #get a trial identifier that is unique to each participant, trial number, and trial type
  filter(RecordingName != "test") %>% #gets rid of a "test" recording that somehow got exported
  mutate(MediaName = as.factor(MediaName)) %>%
  group_by(trial_unique) %>%
  mutate(trial_start = min(timestamp)) %>%
  mutate(trial_end = max(timestamp)) %>%
  mutate(trial_from_zero = timestamp - trial_start) %>% #make time stamps start from 0 for each trial
  ungroup() %>%
  filter(!is.na(ValidityLeft) & !is.na(ValidityRight)) %>% #gets rid of Movie Start and Movie End rows that duplicate some info and don't have meaningful validity data or gaze point info
  mutate(trackloss = case_when(is.na(gaze_point_X) ~ TRUE, #creates a trackloss column that will be used in the eyetrackingR functions
                               is.na(gaze_point_y) ~ TRUE,
                               ValidityLeft > 1 & ValidityRight > 1 ~ TRUE,
                               TRUE ~ FALSE)) %>%
  clean_names() %>% #makes everything snake case 
  
  ###-------------------------------------------------------------------make target aoi coordinates:
  
  mutate(aoi_target_x_left = case_when(order == "Order 1" & trial_type == "pre-switch" ~ 10,
                                       order == "Order 1" & trial_type == "post-switch" ~ 1260,
                                       order == "Order 2" & trial_type == "pre-switch" ~ 1260,
                                       order == "Order 2" & trial_type == "post-switch" ~ 10)) %>%
  mutate(aoi_target_x_length = case_when(!is.na(trial_type) ~ 645)) %>%
  mutate(aoi_target_x_right = aoi_target_x_left + aoi_target_x_length) %>%
  mutate(aoi_target_y_top = case_when(!is.na(trial_type) ~ 270)) %>%
  mutate(aoi_target_y_length = case_when(!is.na(trial_type) ~ 650)) %>%
  mutate(aoi_target_y_bottom = aoi_target_y_top + aoi_target_y_length) %>%
  
  ###-------------------------------------------------------------------make distractor aoi coordinates:
  
  mutate(aoi_distractor_x_left = case_when(order == "Order 1" & trial_type == "pre-switch" ~ 1260,
                                           order == "Order 1" & trial_type == "post-switch" ~ 10,
                                           order == "Order 2" & trial_type == "pre-switch" ~ 10,
                                           order == "Order 2" & trial_type == "post-switch" ~ 1260)) %>%
  mutate(aoi_distractor_x_length = case_when(!is.na(trial_type) ~ 645)) %>%
  mutate(aoi_distractor_x_right = aoi_distractor_x_left + aoi_distractor_x_length) %>%
  mutate(aoi_distractor_y_top = case_when(!is.na(trial_type) ~ 270)) %>%
  mutate(aoi_distractor_y_length = case_when(!is.na(trial_type) ~ 650)) %>%
  mutate(aoi_distractor_y_bottom = aoi_distractor_y_top + aoi_distractor_y_length) %>%
  
  ###-------------------------------------------------------------------make circle aoi coordinates:
  
  mutate(aoi_circle_x_left = case_when(!is.na(trial_type) ~ 758)) %>%
  mutate(aoi_circle_x_length = case_when(!is.na(trial_type) ~ 385)) %>%
  mutate(aoi_circle_x_right = case_when(!is.na(trial_type) ~ aoi_circle_x_left + aoi_circle_x_length)) %>%  
  mutate(aoi_circle_y_top = case_when(!is.na(trial_type) ~ 400)) %>%
  mutate(aoi_circle_y_length = case_when(!is.na(trial_type) ~ 385)) %>%
  mutate(aoi_circle_y_bottom = case_when(!is.na(trial_type) ~ aoi_circle_x_left + aoi_circle_x_length)) %>%  
  filter(!is.na(media_name)) %>% #remove lines with no media name
  filter(!is.na(trial_unique)) %>% #remove NA unique trials (gets rid of times outside our trial windows that aren't interesting to us)
  filter(is.na(studio_event)) %>% #get rid of MovieStart and MovieEnd lines since they have duplicate timestamps
  mutate(remove_row = case_when(timestamp == 66398 & trial_unique == "CogControl20_S01_42270_Test_Target_Right_4.wmv" & is.na(validity_left) ~ "remove",
                                timestamp == 85782 & trial_unique == "CogControl20_S01_42270_Test_Target_Right_7.wmv" & is.na(validity_left) ~ "remove",
                                timestamp == 35816 & trial_unique == "CogControl7_S24_44771_Training_Target_Left_7.wmv" & is.na(validity_left) ~ "remove",
                                TRUE ~ "keep"))%>% #this identifies 3 rows with duplicate timestamps, but inexplicably without the fixation gaze data, that prevent the eyetrackingR code from running correctly
  filter(remove_row == "keep") %>% #get rid of those 3 weird rows from above
  
  ###-------------------------------------------------------------------get AOI look per time point:
  
  mutate(target = case_when(gaze_point_x > aoi_target_x_left & gaze_point_x < aoi_target_x_right & gaze_point_y > aoi_target_y_top & gaze_point_y < aoi_target_y_bottom ~ 1,
                            TRUE ~ 0)) %>% #looks to target
  mutate(distractor = case_when(gaze_point_x > aoi_distractor_x_left & gaze_point_x < aoi_distractor_x_right & gaze_point_y > aoi_distractor_y_top & gaze_point_y < aoi_distractor_y_bottom ~ 1,
                                TRUE ~ 0)) %>% #looks to distractor
  mutate(circle = case_when(gaze_point_x > aoi_circle_x_left & gaze_point_x < aoi_circle_x_right & gaze_point_y > aoi_circle_y_top & gaze_point_y < aoi_circle_y_bottom ~ 1,
                            TRUE ~ 0)) %>% #looks to circle
  mutate(look_any = case_when(target == 1 | distractor == 1 | circle == 1 ~ 1,
                              TRUE ~ 0)) #looks to either target, distractor, or circle


save(data_tobii_cleaned, file = here("data", "data_tobii_cleaned.Rda"))



###--------------------------------------------------------------------GET ANTICIPATION PERIOD KEEPERS---------------------------

data_anticipation <- data_tobii_cleaned %>%
  filter(trial_from_zero >= 2150 & trial_from_zero <= 3150) %>% #filter to just the aniticpatory period, which is 150 ms after the offset of the 2000 ms cue, and lasts for 1000 ms
  group_by(trial_unique) %>%
  mutate(prop_fixations = mean(look_any)) %>%
  filter(prop_fixations >= MIN_LOOK_PROPORTION) %>% #removes trials where the child did not fixate on anything for at least 50% of the anticipation period
  group_by(recording_name, trial_type) %>% 
  mutate(num_good_trials = length(unique(trial_num))) %>%
  filter(num_good_trials >= MIN_NUMBER_TRIALS) %>% #removes babies who don't have the minimum number of trials 
  group_by(recording_name) %>%
  mutate(num_trial_types = length(unique(trial_type))) %>%
  filter(num_trial_types == 2) %>% #removes babies who don't have data in both trial types
  ungroup()

final_sample_ids <- data_anticipation %>% #used for creating data_full_trials object for plotting whole trial
  select(recording_name) %>%
  unique()

final_sample_mslist <- ms_list %>%
  inner_join(final_sample_ids, by = "recording_name") %>%
  group_by(age_group, language) %>%
  mutate(median_vocab = median(cdi_tot_vocab_prod, na.rm = TRUE)) %>%
  mutate(vocab_group = ifelse(median_vocab > cdi_tot_vocab_prod,
                              "Low", 
                              "High")) %>%
  ungroup() %>%
  mutate(vocab_group = factor(vocab_group, levels = c("Low", "High")),
         vocab_centred = scale(cdi_tot_vocab_prod, center = TRUE, scale = FALSE),
         vocab_scaled = scale(cdi_tot_vocab_prod, center = TRUE, scale = TRUE))

save(final_sample_mslist, file = here("data", "final_sample_mslist.Rda"))


###----------------------------------------------------------------------CREATE MERGED FULL DATA----------------------------------

data_full_trials <- inner_join(data_tobii_cleaned, final_sample_mslist, by = "recording_name") %>%
  mutate(recording_name = as.factor(recording_name)) # this is the full time series data, but only for infants that made it through to the final anticipation-period sample. Will be used for ploting time series data of whole trial


save(data_full_trials, file = here("data", "data_full_trials.Rda"))

###----------------------------------------------------------------------CREATE MERGED ANTICIPATION DATA----------------------------------

save(data_anticipation, file = here("data", "data_anticipation.Rda"))

###----------------------------------------------------------------------CREATE EXCLUSION DATA----------------------------------

exclusions <-read_csv(here("data", "2019_CogControl_MSL.csv"), na = "-") %>%
  separate(lang.group,into = c("language", "group")) %>%
  clean_names() %>%
  full_join(data_tobii_cleaned, by = "recording_name") %>%
  filter(trial_from_zero >= 2150 & trial_from_zero <= 3150) %>% #filter to just the aniticpatory period, which is 150 ms after the offset of the 2000 ms cue, and lasts for 1000 ms
  group_by(trial_unique) %>%
  mutate(prop_fixations = mean(look_any),
         prop_fixationskeep = case_when(prop_fixations >= MIN_LOOK_PROPORTION ~ "keep_trial",
                                    TRUE ~ "exclude_trial"))  %>%
  select(recording_name, trial_num, trial_type, trial_unique, age_group, language, total_age_days_excel, gender, per_eng, per_fr, per_other1, per_other2, excl_reason, prop_fixationskeep, keeper, comments) %>%
  distinct() %>%
  group_by(recording_name, trial_type) %>% 
  mutate(num_good_trials = sum(prop_fixationskeep == "keep_trial")) %>%
  mutate(num_good_trialskeep = case_when(num_good_trials >= MIN_NUMBER_TRIALS ~ paste0("keep_trial_type_", trial_type),
                                     TRUE ~ paste0("exclude_trial_type_", trial_type))) %>%  
  ungroup() %>%
  select(-trial_num, -num_good_trials, -trial_unique, -prop_fixationskeep) %>%
  distinct() %>%
  group_by(recording_name) %>%
  mutate(keep_baby = str_sub(num_good_trialskeep, 1, 4)) %>%
  select(-trial_type, -num_good_trialskeep) %>%
  distinct() %>%
  mutate(excl = sum(keep_baby == "excl"),
         keep_baby = case_when(excl == 1 ~ "exclude",
                               TRUE ~ "keep")) %>%
  distinct() %>%
  ungroup() %>%
  mutate(excl_reason = case_when(excl_reason == "none" & keep_baby == "exclude" ~ "inattentive",
                                 TRUE ~ excl_reason))


save(exclusions, file = here("data", "exclusions.Rda"))

