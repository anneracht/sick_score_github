getwd()
library(dplyr)
library(tibble)
library(tidyr)
library(magrittr)
library(ggplot2)
library(reshape2)
library(tm)
library(base)
library(stringr)
library(RColorBrewer)
library(purrr)
library(stringr)
library(ggpubr)


require(gdata)

#Pull data from desktop folder
df = read.xls(("sick_score_xls.xls"), sheet = 1, header = TRUE)
sick_score <- rename(df)
sick_score <- data.frame(sick_score, stringsAsFactors = FALSE)
head(sick_score)
#open up the spreadsheet on R
view(sick_score)
# Inspecting data. View the no. of variables and levels for each variable.
sick_score %>% tbl_vars()
str(sick_score)
str(sick_score$Race)

#convert all missing values ("", '') in sick_score to NA, and count the missing values
sick_score[sick_score == ""] <- NA

#count the number of missing values
sum(is.na(sick_score))

#simple density plot
ggplot(sick_score, aes(BMI.Surgical.Tracking, fill = BMI.Surgical.Tracking, colour = BMI.Surgical.Tracking)) +
  geom_density(alpha = 0.1) +
  xlim(20,100)

summary(sick_score$BMI.Surgical.Tracking)

#I wanna see the distribution of n based on age, race, gender, and health insurance status
ggdensity(sick_score, x = "Age",
          add = "mean", rug = TRUE,
          color = "Gender", fill = "Gender",
          palette = c("#0073C2FF", "#FC4E07"))

#density plot for BMI based on Race
ggdensity(sick_score, x = "BMI.Surgical.Tracking",
          add = "mean", rug = TRUE,
          color = "ASA", fill = "ASA",
          title = "Density Plot of BMI based on ASA",
          palette = c("#0073C2FF", "#FC4E07"))


#reorder the plot
sick_score %>% count(Race) %>% mutate(Race = reorder(Race, n, desc = TRUE)) %>%
ggplot(aes(x = Race, y = n)) + geom_bar(stat = 'identity') + scale_y_continuous(breaks = seq(0,1200,50))

#density plot for each variable (I'm going to examine all 49 if I have to)

#stacked histogram for distribution of ASA based on races
ggplot() + geom_bar(data = sick_score,
           aes(x = factor(Race),fill = factor(ASA)),
           position = "fill") + labs(title="Stacked Histogram for ASA distribution based on Race",
                                   subtitle="#Race")
#stacked histogram without 100%
ggplot() + geom_bar(data = sick_score,
                    aes(reorder(x = factor(Race)),fill = factor(ASA))) + labs(title="Stacked Histogram for ASA distribution based on Race",
                                              subtitle="#Race")


#stacked histogram for distribution of LOS based on Age
ggplot(sick_score, aes(Race, fill = ASA)) + geom_bar()+
  labs(title = "Stacked Bar Chart", x = "Race", y = "ASA")


#density plot of 2 or more variables,  x= age , y = any variables you wanna see
ggplot(sick_score, aes(Age)) + geom_density(aes(fill=factor(LOS)), alpha=0.8) +
  labs(title="Density plot",
       subtitle="# Bariatric surgery pts grouped based on Insurance",
       x="# Age",
       fill="LOS")

levels(sick_score$Type.of.Surgery)

ggplot(sick_score, aes(Age, BMI.Surgical.Tracking))+
  geom_raster(aes(fill = LOS)) +
  labs(title ="Heat Map", x = "Age", y = "BMI") +
  scale_fill_continuous(name = "length of stay")


ggplot(sick_score, aes(Age, BMI.Surgical.Tracking)) +
  geom_tile(aes(fill = LOS),colour = "lightblue") + scale_fill_gradient(low = "white",high = "steelblue") +
  labs(title ="Heat Map of Age,BMI,LOS", x = "Age", y = "BMI")

#scatterplot for BMI and age
ggplot(sick_score, aes(Age, BMI.Surgical.Tracking)) + geom_point() + labs(title ="Scatterplot : BMI vs Age")

#scatterplot for BMI & age, divided based n ASA
ggplot(sick_score, aes(Age, BMI.Surgical.Tracking)) + geom_point(aes(color = ASA)) +
  scale_x_continuous("Age", breaks = seq(10,80,10))+
  scale_y_continuous("BMI", breaks = seq(20,100,10))+
  theme_bw() + labs(title="Scatterplot: BMI vs Age stratified on ASA") + facet_wrap( ~ ASA)

#scatterplot for BMI & age, divided based n Race
ggplot(sick_score, aes(Age, BMI.Surgical.Tracking)) + geom_point(aes(color = HLD)) +
  scale_x_continuous("Age", breaks = seq(10,80,5))+
  scale_y_continuous("BMI", breaks = seq(20,100,5))+
  theme_bw() + labs(title="Scatterplot: BMI vs Age stratified on HLD status") + facet_wrap( ~ HLD)


#abbreviate the Race to AA, C, LA, ME, AZN, AIAN, O
levels(sick_score$Race)[levels(sick_score$Race)=="A Indian/Alaskan Native"] <- "AIAN"
levels(sick_score$Race)[levels(sick_score$Race)=="African American"] <- "AA"
levels(sick_score$Race)[levels(sick_score$Race)=="Asian"] <- "AZN"
levels(sick_score$Race)[levels(sick_score$Race)=="Caucasian"] <- "C"
levels(sick_score$Race)[levels(sick_score$Race)=="latin American"] <- "LA"  #I repeated this for latin and Latin
levels(sick_score$Race)[levels(sick_score$Race)=="Middle Eastern"] <- "ME"
levels(sick_score$Race)[levels(sick_score$Race)=="Other"] <- "O"


#let's come up with the most essential, ie. Table 1
#total variables 49. I'll modify the CKD , CAD, CHF, CVA, liver data to reduce it to 1 variable column.
#but first, I'll convert the obvious binaries into 1 and 0 . The edited spreadsheet is still named sick_score for simplicity purpose.
#to filter a variable column into specific level
sick_score %>% select(Race, Type.of.Surgery) %>% filter(str_detect(Race, "African"))
#I want to count which race gets which type of surgery. But first I need to clean up the data on the types
#of surgery
str(sick_score$Type.of.Surgery, list.len = 700)

#to list all the levels with these character occurrences
grep("G*", sick_score$Type.of.Surgery, value=TRUE)

sum(sick_score$Type.of.Surgery == "Gastric Sleeve") #35
sum(sick_score$Type.of.Surgery == "Realize Band") #1
sum(sick_score$Type.of.Surgery == "RnY") #1


str_count(sick_score$Type.of.Surgery, "G* S*") %>% mutate(count = n())


#list all types of surgeries
#count all of these surgeries and mutate into a new column, and the a graph
#sapply(unique(sick_score$Type.of.Surgery), function(x) str_count(sick_score$Type.of.Surgery,x))
sick_score %>%
  group_by(Type.of.Surgery, Race) %>%
  tally()

#use tally to get a glimpse of all the levels of variables , and discuss w team on what to edit later on
#I want to count the types of surgery based on Race , and find any missing data
sick_score %>% count(Race, Type.of.Surgery, sort = TRUE) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


#group race, type of surgery, and health insurance status
sick_score %>%
  group_by(Type.of.Surgery, Race ,Health.Insurance.Status) %>%
  tally() %>%
  arrange(desc(n)) %>% #arrange in descending order
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


#time to look at the comorbids !
sick_score %>%
  group_by() %>%
  tally() %>%
  arrange(desc(n))

summary(sick_score$Health.Insurance.Status)
str(sick_score)
