#
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("car")
#install.packages("ez")


## Clear workspace
rm(list = ls())

## Load packages
library(tidyverse)
library(readxl)
library(stringr)
library(tidyr)
library(ggplot2)
library(car)
library(ez)

## Read in tobii data from Excel 
Mydata <- read_excel("E:/Spring 2018/cogcontrol/April26th-WRITING.xlsx", na="-") 

## Read in master_subject_list
m.s.list <- read_excel("E:/Spring 2018/cogcontrol/m.s.list.xlsx", na="-") %>%
separate(lang.group,into = c("language", "group")) %>%
filter(keeper == 1) 

## Clean Mydata
names(Mydata) <- gsub("Total Fixation Duration_Test", "Test", names(Mydata))
names(Mydata) <- gsub("Total Fixation Duration_Training", "Training", names(Mydata))
names(Mydata) <- gsub("Circle_Sum", "Circle", names(Mydata))
names(Mydata) <- gsub("Target_Sum", "Target", names(Mydata))
names(Mydata) <- gsub("Distractor_Sum", "Distractor", names(Mydata))
names(Mydata) <- gsub("Test", "Test_", names(Mydata))
names(Mydata) <- gsub("Training", "Training_", names(Mydata))
Mydata[is.na(Mydata)] <- 0 

## Convert data from wide to long, and then from long to wide
data_set <- Mydata %>%
  select(recording.name, contains("Circle"), contains("Distractor"), contains("Target")) %>%
  gather(AOI, looking_time, Test_1_Circle:Training_9_Target) %>%
  separate(AOI,into = c("trial_type", "trial_number", "AOI")) %>%
  spread(AOI, looking_time) %>%
  
##calculating the total looking time & good baby criteria
  mutate(All_looking_time = Circle + Distractor + Target) %>%
  mutate(good_trial = ifelse(All_looking_time >0.50, "good", "bad")) %>%
  filter(good_trial == "good") %>% 
  group_by(recording.name, trial_type) %>% 
  mutate(num_good_trial = length(recording.name)) %>%
  filter(num_good_trial >=5) %>%
  group_by(recording.name) %>% 
  mutate(good_baby = length(unique(trial_type))) %>%
  filter(good_baby == 2) %>%
  
##calculating 3 types of anticipation
  mutate(correct_anticipation = ifelse(Target > Distractor, 1, 0)) %>%
  mutate(incorrect_anticipation = ifelse(Distractor > Target, 1, 0)) %>%
  mutate(no_anticipation = ifelse(Target + Distractor == 0, 1, 0))
 
## merge data_set with master.subject.list
data_merge <- merge(data_set, m.s.list, by="recording.name")

#Count the number of participants in each group
count <- data_merge %>%
filter(age.group == "7 months") %>%
filter(language == "Bilinguals") %>%
mutate(num_participants = length(unique(recording.name)))

## calculate proportion of correct anticipation for figure 1
  proportion_correct <- data_merge %>%
  group_by(age.group, language, trial_type, trial_number) %>%
  filter(no_anticipation == 0) %>%
  summarise(proportion = mean(correct_anticipation,na.rm=T))
  
##calculate proportion of incorrect incorrect anticipation for fig.1.2 
  proportion_incorrect <- data_merge %>%
    group_by(age.group, language, trial_type, trial_number) %>%
    filter(no_anticipation == 0) %>%
    summarise(proportion = mean(incorrect_anticipation,na.rm=T))
  
  
####calculate proportion of no anticipation for fig. 1.3
  proportion_no_anticipation <- data_merge %>%
    group_by(age.group, language, trial_type, trial_number) %>%
    summarise(proportion = mean(no_anticipation,na.rm=T))
  
####calculate proportion of no anticipation for fig. 1.4
  proportion4 <- data_merge %>%
    group_by(age.group, language, trial_type, trial_number) %>%
    summarise(proportion = mean(correct_anticipation,na.rm=T))
  
# ?? here if we do not filter, then not sure zeros mean 
  # incorrect or no aniticipation?

##figure 1
fig_data <- proportion_correct %>%  
  ungroup() %>%
  mutate(trial_number = as.numeric(trial_number)) %>%
  mutate(age.group = as.factor(age.group)) %>%
  mutate(language = as.factor(language)) %>%
  mutate(trial_type = as.factor(trial_type)) %>%
  group_by(trial_number, age.group, trial_number, language) 


fig_data$age.group <-  fct_rev(fig_data$age.group)
fig_data$trial_type <- fct_rev(fig_data$trial_type)


fig_data%>%
  filter(age.group == "20 months") %>% 
  ggplot (aes(color = language, 
              shape = language, 
              x = trial_number, 
              y = proportion)) +
  geom_point(size= 4) +
  scale_x_discrete(limits=c(1:9))+
  stat_smooth(method = "lm", se = FALSE) +
  facet_grid(age.group ~ trial_type) +
  theme_gray((base_size=22)) +
  xlab("Trial number") +
  ylab("Proportion anticipation") 

##ggsave("fig_data.pdf") saves in this PC/documents
## to save with more similarity to the original figure 
ggsave("fig_data.pdf")

## change dimentions 
ggsave("fig_data.pdf",height = 12.5,width = 27,units = "cm")


##figure 1.2
fig_data <- proportion_incorrect %>%  
  ungroup() %>%
  mutate(trial_number = as.numeric(trial_number)) %>%
  mutate(age.group = as.factor(age.group)) %>%
  mutate(language = as.factor(language)) %>%
  mutate(trial_type = as.factor(trial_type)) %>%
  group_by(trial_number, age.group, trial_number, language) 


fig_data$age.group <-  fct_rev(fig_data$age.group)
fig_data$trial_type <- fct_rev(fig_data$trial_type)


fig_data%>%
  #filter(age.group == "20 months") %>% 
  ggplot (aes(color = language, 
              shape = language, 
              x = trial_number, 
              y = proportion)) +
  geom_point(size= 4) +
  scale_x_discrete(limits=c(1:9))+
  stat_smooth(method = "lm", se = FALSE) +
  facet_grid(age.group ~ trial_type) +
  theme_gray((base_size=22)) +
  xlab("Number of Trials") +
  ylab("Proportion anticipation") 


##figure 1.3
fig_data <- proportion_no_anticipation %>%  
  ungroup() %>%
  mutate(trial_number = as.numeric(trial_number)) %>%
  mutate(age.group = as.factor(age.group)) %>%
  mutate(language = as.factor(language)) %>%
  mutate(trial_type = as.factor(trial_type)) %>%
  group_by(trial_number, age.group, trial_number, language) 


fig_data$age.group <-  fct_rev(fig_data$age.group)
fig_data$trial_type <- fct_rev(fig_data$trial_type)


fig_data%>%
  filter(age.group == "7 months") %>% 
  ggplot (aes(color = language, 
              shape = language, 
              x = trial_number, 
              y = proportion)) +
  geom_point(size= 4) +
  scale_x_discrete(limits=c(1:9))+
  stat_smooth(method = "lm", se = FALSE) +
  facet_grid(age.group ~ trial_type) +
  theme_gray((base_size=22)) +
  xlab("Trial number") +
  ylab("Proportion no anticipation") 

ggsave("fig_data.pdf")

## change dimentions 
ggsave("fig_data.pdf",height = 15,width = 27,units = "cm")

# Change data types to proper ones
data_clean <- data_merge %>%
  mutate(age.group = as.factor(age.group)) %>%
  mutate(language = as.factor(language)) %>%
  mutate(trial_type = as.factor(trial_type)) %>%
  mutate(recording.name = as.factor(recording.name))

ggsave("fig_data.pdf")

## change dimentions 
ggsave("fig_data.pdf",height = 11,width = 27,units = "cm")

#  filter(no_anticipation == 0)

##figure 1.4
fig_data <- proportion4 %>%  
  ungroup() %>%
  mutate(trial_number = as.numeric(trial_number)) %>%
  mutate(age.group = as.factor(age.group)) %>%
  mutate(language = as.factor(language)) %>%
  mutate(trial_type = as.factor(trial_type)) %>%
  group_by(trial_number, age.group, trial_number, language) 


fig_data$age.group <-  fct_rev(fig_data$age.group)
fig_data$trial_type <- fct_rev(fig_data$trial_type)


fig_data%>%
  #filter(age.group == "20 months") %>% 
  ggplot (aes(color = language, 
              shape = language, 
              x = trial_number, 
              y = proportion)) +
  geom_point(size= 4) +
  scale_x_discrete(limits=c(1:9))+
  stat_smooth(method = "lm", se = FALSE) +
  facet_grid(age.group ~ trial_type) +
  theme_gray((base_size=22)) +
  xlab("Number of Trials") +
  ylab("Proportion anticipation") 

ggsave("fig_data.pdf")

## change dimentions 
ggsave("fig_data.pdf",height = 15,width = 27,units = "cm")

#  filter(no_anticipation == 0)
  
## creating the blocks
data_blocks <- data_clean %>%
  mutate(blocks_num = as.factor((as.numeric(trial_number)+2) %/% 3)) %>%
  group_by(recording.name,
           age.group,
           language, 
           trial_type, 
           blocks_num,
           total.vocab.prod, 
           total.concept.prod) %>%
  summarise(proportion_looking = mean(correct_anticipation,na.rm=T))
 
## figure.2 showing blocks 
data_fig_blocks <- data_clean %>%
  mutate(blocks_num = as.factor((as.numeric(trial_number)+2) %/% 3)) %>%
  group_by(age.group, language, trial_type, blocks_num) %>%
  summarise(proportion_looking = mean(correct_anticipation,na.rm=T))
data_fig_blocks$age.group <-  fct_rev(data_fig_blocks$age.group)
data_fig_blocks$trial_type <- fct_rev(data_fig_blocks$trial_type)

# geom_point 
ggplot(data=data_fig_blocks, aes(x=blocks_num, y=proportion_looking, group=language, shape=language)) +
  geom_line(size=1.2) + 
  geom_point(size=3, fill="red") +
  scale_shape_manual(values=c(22,21)) +
  scale_x_discrete(limits=c(1:3))+
  facet_grid(age.group ~ trial_type) 

# geom_bar 
ggplot(data=data_fig_blocks, aes(x=blocks_num, y=proportion_looking, group=language, fill=language)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_shape_manual(values=c(22,21)) +
  scale_x_discrete(limits=c(1:3))+
  facet_grid(age.group ~ trial_type)


## median for each language group
data_blocks_summary <- data_blocks %>%
  group_by(language) %>%
  summarise(median= median(total.concept.prod,na.rm=T))
  
## calculating High & Low vocab size

data_vocab <- data_blocks %>%
  group_by(language) %>%
  mutate(median.byGroup= median(total.concept.prod,na.rm=T)) %>%
  mutate(vocab.size = ifelse(median.byGroup>total.concept.prod,"Low", "High")) %>%
  mutate(vocab.size = as.factor(vocab.size))
  
## mean of vocab size for figure 2
  mean.vocab <- data_vocab %>%
  filter(age.group == "20 months") %>%
  group_by(language, vocab.size) %>%
  summarise(mean.vocabulary = mean(total.concept.prod,na.rm=T)) 
  
## fig 3. bar
  mean.vocab$language <-  fct_rev(mean.vocab$language)
  
  fig_vocab <- mean.vocab
    ggplot(data=fig_vocab, aes(x=language, y=mean.vocabulary, fill=vocab.size)) +
    geom_bar(stat="identity", position=position_dodge()) +
    scale_fill_manual(values=c("#999999", "#E69F00"))

 ## fig 3. geom_point
  fig.vocab2 <- mean.vocab 
  ggplot(data=fig.vocab2, aes(x=vocab.size, y=mean.vocabulary, group=language, shape=language)) +
    geom_line(size=1.2) + 
    geom_point(size=3, fill="green") +
    scale_shape_manual(values=c(22,21))
  
  
## fig 4. Blocks with interaction of language x vocab
  
  data_fig_blocks <- data_clean %>%
    mutate(blocks_num = as.factor((as.numeric(trial_number)+2) %/% 3)) %>%
    filter(age.group == "20 months") %>%
    group_by(language, trial_type, blocks_num) %>%
    mutate(median.byGroup= median(total.concept.prod,na.rm=T)) %>%
    mutate(vocab.size = ifelse(median.byGroup>total.concept.prod,"low", "high")) %>%
    mutate(vocab.size = as.factor(vocab.size))

  
 #Trials with vocab size 
  fig_40 <- data_fig_blocks %>%
    group_by(language, trial_type, trial_number, vocab.size) %>%
    summarise(proportion_looking = mean(correct_anticipation, na.rm=T)) 
  
  fig_40$trial_type <- fct_rev(fig_40$trial_type)
  
  
  ggplot(data=fig_40, aes(x=trial_number, 
                         y=proportion_looking, 
                         group=interaction(language,vocab.size), 
                         linetype=vocab.size,
                         shape=language)) +
    geom_line(size=1.2) + 
    geom_point(size=3, fill="turquoise") +
    scale_shape_manual(values=c(22,21)) +
    scale_x_discrete(limits=c(1:9))+
    #stat_smooth(method = "lm", se = FALSE) +
    facet_grid(language ~ trial_type) + 
    xlab("Trial number") +
    ylab("Proportion looking")
  
  
  ggsave("fig_40.pdf")
  
  ggsave("fig_40.pdf",height = 20,width = 28,units = "cm") 
  
  

  # fig.4 geom_point 
  fig_4 <- data_fig_blocks %>%
  group_by(language, trial_type, blocks_num, vocab.size) %>%
  summarise(proportion_looking = mean(correct_anticipation, na.rm=T)) 
    
  fig_4$trial_type <- fct_rev(fig_4$trial_type)
  
  ggplot(data=fig_4, aes(x=blocks_num, 
                                   y=proportion_looking, 
                                   group=interaction(language,vocab.size), 
                                   linetype=vocab.size,
                                   shape=language)) +
                                   geom_line(size=1.2) + 
                                   geom_point(size=3, fill="red") +
                                   scale_shape_manual(values=c(22,21)) +
                                   scale_x_discrete(limits=c(1:3))+
                                   facet_grid(language ~ trial_type) + 
                                   xlab("Block number") +
                                   ylab("Proportion looking")
  ggsave("fig_4.pdf")
  
  ggsave("fig_4.pdf",height = 20,width = 28,units = "cm") 
  
  
  ## ANOVA for 7-month-olds  
  test.7 <- data_blocks %>% 
    filter(age.group == "7 months") %>%
    filter(trial_type == "Training") 
  
  options(scipen = 999)
  test.anova.7 <- ezANOVA(data = test.7, 
                           dv = proportion_looking,
                           wid = recording.name,
                           within = blocks_num,
                           between = .(language), 
                           detailed = TRUE,
                           type = 3)
  
  
## ANOVA for 20-month-olds
  test.20 <- data_blocks %>% 
    filter(age.group == "20 months") %>%
    filter(trial_type == "Test") 
  
  
##3X2X2 ANOVA for 20.m (block, language group, vocab size)
  options(scipen = 999)
  test.anova.20 <- ezANOVA(data = test.20, 
                           dv = proportion_looking,
                           wid = recording.name,
                           within = blocks_num,
                           between = .(language), 
                           detailed = TRUE,
                           type = 3)

  
## 2 X2 ANOVA for 2l0.m (language group and vocab size)
  options(scipen = 999)
  test.anova.20.between <- ezANOVA(data = test.20, 
                                   dv = proportion_looking,
                                   wid = recording.name,
                                   between = vocab.size, language,
                                   detailed = TRUE,
                                   type = 2)
  
  
  
     