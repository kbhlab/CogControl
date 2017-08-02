# comment to see how git works

# this sign is used to write explanations in R
# whenever comes at the beginning of a sentence, it shows in green.
install.packages("readxl")
library(readxl)
myexcel <- read_excel("E:/ExampleR/myexcel.xlsx")
View(myexcel)
# note that the direction of / is opposite to that of csv
# for read.csv always use "<-", NOT ">-"

str(myexcel) # shows the structure of an object
edit(myexcel) # prepares in a separate sheet for fixing data
summary(myexcel) # gives mean, media, for each column 
data(package = "datasets") # lists all the datasets in a package
head(myexcel) # shows some of (usually the first 6) rows [for all the variables ; here all the 81 v) in a dataset 
help(myexcel) # access help about variables in the dataset
# creates box plots for two differnet groups
plot(language_group ~ Test1_Distractor, data = myexcel)
boxplot() # shows in boxes
## retrieve the score values for each lang_group and save the results
## into two object
language_group1 = myexcel$Test1_Distractor[myexcel$language_group == 1] #
language_group2 = myexcel$Test1_Distractor[myexcel$language_group == 2] #

mean(language_group1) #
mean(language_group2) #

## one sample t test. test : H0: mean of population is equal 0.5
##                           H1: mean of population is not equal 0.5
t.test(x = langauge_group1, y = NULL , mu = 0.5)

install.packages("tidyr") # useful for gathering and spreading
# gather makes wide data look long
# Test_Training looking_time ra xodeman be onvane title midahim
myexcel3 <- gather(myexcel, Test_Training, looking_time, Test1_circle:averageblock3_Training) #

View(myexcel3) #

  
  # tibbles are one of the unifying features of the tidyverse. 
  # Most other R packages use regular data frames, 
  # so you might want to coerce a data frame to a tibble. 
  # You can do that with as_tibble():
  # isntall and library(tibble)
  as_tibble(iris) #
  # this is a collection of flower
  as_tibble(table1) # or
  table1 # this is another tibble in tidyverse
  
  # You can create a new tibble from individual vectors with
  tibble() #
  # tibble(
  # x = 1:5,).... bexatere , yani faqat sotoun darim az 1 ta 5
  
  # In order for R to show table 1 
  # We have to 1st install package named tidyverse
  # to Compute rate per 10,000
  table1 %>% #
    mutate(rate = cases / population * 10000) #
  
  mydata %>%
    group_by(BabyID, trial_number, trial_type) %>%
    summarize(total_look_trial = sum(looking_time)) %>%
    filter(total_look_trial >= .5)
  
  # I 1st installed "tidyverse" package [because wanted to use gather code, 
  # and that function is in tidyr package under tidyverse], 
  # then opened my dataframe, for this had to install anther package,
  # And then I wrote the code "gather", but
  # there was an error saying, [could not find function "gather"],
  # to fix this,
  # I again installed tidyr and ran by library), 
  # This time the code worked!
  # Thus, because I had installed another package [for opening the dataframe]
  # there was a need to recall the tidyr once again!
  #These are sime of the packages under tidyverse
  #library(tidyverse)
 # Loading tidyverse: ggplot2
 # Loading tidyverse: tibble
 # Loading tidyverse: tidyr
 # Loading tidyverse: readr
 # Loading tidyverse: purrr
 # Loading tidyverse: dplyr
  
  # Merging two datasets in R
  # Final_data <- merge(myExcel, myExcel_new, by="BabyID")
  
  # Here, merge only binds those observations
  # at the intersection of both datasets
  # Mesal:
  
  # data_set1    data_set2
  #Name Age      Name  Weight
  #Bob  23       Bob    70 
  #BIlly 25      Ben    95
  
  #after merge, it shows:
  # Name  Age Weight
  # Bob   23   70     & no info for the rest
  
  #to fix this, we can use this function
  # merge(data_set1, data_set2, by="Name",all.x=TRUE)
  
  #So it will get all data in data_set1 but
  # Billy's weight will be NA, so it looks like:
  # Name Age Weight
  # Billy 25   NA 
  #  Bob  23   70
  
  #To have all observations in data_set2 as well
  # merge(data_set1, data_set2, by="Name",all.y=TRUE)
  
  #Finally to have all data_sets 1&2 together:
  #merge(data_set1, data_set2, by="Name",all=TRUE)
  #Which looks like:
  #Name Age Weight
  #Billy 25    NA
  #Bob   23    70
  #Ben   NA    95
  
  #As you see "Bob" is shown only once because it is common betw
  #the two datasets

  # Merging based on class 
  # Final_data <- merge(myExcel, myExcel_new, by=C("Class", "Section', "BabyID"))
  
  # In dplyr, with this function you can easily
  #get the mean of a variable called "Var2" inonce column
  #
  #(df$Var2)
  
  #to add up all v\observations in column 1 and 2:
  # df$Var3 <- df$Var1 + df$Var2
  
  #Agar bexahim az dplyr estefade konim,
  #mitavan ba in function nevesht:
  
  #left_join(dataset1, dataset2, by = "name")
  # here, dataset1 is called "table to augment" 
  #& dataset2 is called "table to augment with"
  #name is called "key column name as a character string"
  
  #to select variables which start with a particualr name
  #mydata_2 <- myexcel2 %>% select(starts_with("Test"))
  #vaqti dar help search dadim ba starts_with,
  #option contains ham amad!
  
  #STEPS TO TIDY COGCONTROL TOBII DATA + MERGING WITH M.S.L
  # Trialtype, Trial#, AOI
  
  #library(tidyverse)
  #install.packages("readxl")
  #library(readxl)
  #my_excel <- read_excel("E:/ExampleR/my.excel.xlsx")
  # In order to renamine variables we need "stringr"
  # library(stringr), This is to rename variable Test1 and put an _ between Test & 1
  #names(my_excel) <- gsub("Test", "Test_", names(my_excel))
  #names(my_excel) <- gsub("Training", "Training_", names(my_excel))
  #mydata <- my_excel %>% select(BabyID, contains("circle"), contains("Distractor"), contains("Target"))
  #mydata1 <- gather(mydata1, AOI, looking_time, Test_1_circle:Training_9_Target)
  #mydata2 <- separate(mydata1, AOI, into = c("trial_type", "trial_number", "AOI"))
  #import master subject list:
  # m.s.l <- read.csv ("C:\\Users\\User\\Desktop\\CogControl_Master Subject List.csv", header = TRUE)
  # to merge mydata2 with m.s.l, they should have one common variable
  # rename BabyID in mydata2 to recording.name in m.s.l
  #names(mydata2) <- gsub("BabyID", "recording.name", names(mydata2))
  #now merge the two:
  #final.data <- merge(mydata2, m.s.l, by="recording.name")
  # Advanced imputation, to fix - and change it with NA:
  #mydata2[ mydata2 == "-" ] = NA

  
  # you can update by tidyverse_update()
  
  # three data packages from outside the tidyverse:
  # install.packages(c("nycflights13", "gapminder", "Lahman"))
  
  # the >, called the prompt
  
  # If Google doesn't help, try stackoverflow.
  
  # creating a scatterplot with ggplot, suppose your 
  #dataset is called mpg
  # first load ggplot2 by library(tidyverse)
  #ggplot(data = mpg) +
  # + geom_point(mapping = aes(x = , y = ))
  
  # Hadley chap.2 to change color in scatterplot
  # > ggplot(data = mpg) +
    + geom_point(mapping = aes(x = displ, y = hwy, colour = class))
  
  # ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")
  # all dots will turn blue, don't forget ""
  
  # if it's a +, it means that R doesn't think you've typed 
  # a complete expression and it's waiting for you to finish it
  
  # bar plot
  # 
  # ggplot(data = diamonds) +
    + geom_bar(mapping = aes(x = cut))
  
  # agar bexay bejaye default count az yek variable y dige 
  #estefade kone
  # ggplot(data = demo) +
  # geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")
  
  # to see in proportion
  #ggplot(data = diamonds) +
    + geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))
  
  #ggplot(data = diamonds) +
  # + stat_summary(mapping = aes(x = cut, y = depth), 
  # fun.ymin = min, fun.ymax =max, fun.y = median)
  
  #color a bar chart
  # ggplot(data = diamonds) +
  + geom_bar(mapping = aes(x = cut, fill = cut))
  
  # box plot
  # > ggplot(data = mpg) +
  + geom_boxplot(mapping = aes(x = class, y = hwy)) 
  
  # yek ravesh digar neveshtan
  # ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()
  
  # question 3.3.1
  #ggplot(data = mpg) +
  + geom_point(mapping = aes(x = class, y = hwy), color = "blue")
  
  # put color inside outside brackets
  ggplot(data = mpg) +
    + geom_point(mapping = aes(x = cyl, y = hwy, color = hwy))
  
  # get github account
  
  df <- data.frame(Month = 1:12, Year = c(2000, rep(NA, 11)))
df %>% fill(Year)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
