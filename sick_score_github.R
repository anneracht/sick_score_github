getwd()
library(dplyr)
library(tibble)
library(tidyr)
library(magrittr)
library(ggplot2)
library(reshape2)
library(igraph)
library(DiagrammeR)
library(tm)
library(base)
library(stringr)
library(syuzhet) # for Anne sentiment experiment
library(RColorBrewer)
library(wordcloud)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(ggpubr)


require(gdata)

#Pull data from desktop folder
df = read.xls(("sick_score_xls.xls"), sheet = 1, header = TRUE)
sick_score <- rename(df)
head(sick_score)
#open up the spreadsheet on R
view(sick_score)
# Inspecting data. View the no. of variables and levels for each variable.
sick_score %>% tbl_vars()
str(sick_score)
str(sick_score$ASA)

#convert all missing values ("", '') in sick_score to NA, and count the missing values
#sick_score[sick_score == ""] <- NA
view(sick_score)

#count the number of missing values
sum(is.na(sick_score$ASA))


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




#reorder the plot
sick_score %>% count(Race) %>% mutate(Race = reorder(Race, n, desc = TRUE)) %>%
ggplot(aes(x = Race, y = n)) + geom_bar(stat = 'identity') + scale_y_continuous(breaks = seq(0,1200,50))

#density plot for each variable (I'm going to examine all 49 if I have to)
sick_score %>% count(Race) %>% mutate(Race = reorder(Race, n, desc = TRUE)) %>%
  ggplot(aes(Race)) + geom_density()

#distribution plot for non-numeric data
sum(sick_score$ASA


#density plot of 2 or more variables,  x= age , y = any variables you wanna see
ggplot(sick_score, aes(BMI.Surgical.Tracking)) + geom_density(aes(fill=factor(Age)), alpha=0.8) +
  labs(title="Density plot",
       subtitle="# Bariatric surgery pts grouped based on Insurance",
       x="# Age",
       fill="ASA")





summary(sick_score)

#abbreviate the Race to AA, C, LA, ME, AZN, AIAN, O
levels(sick_score$Race)[levels(sick_score$Race)=="A Indian/Alaskan Native"] <- "AIAN"
levels(sick_score$Race)[levels(sick_score$Race)=="African American"] <- "AA"
levels(sick_score$Race)[levels(sick_score$Race)=="Asian"] <- "AZN"
levels(sick_score$Race)[levels(sick_score$Race)=="Caucasian"] <- "C"
levels(sick_score$Race)[levels(sick_score$Race)=="Latin American"] <- "LA"  #I repeated this for latin and Latin
levels(sick_score$Race)[levels(sick_score$Race)=="Middle Eastern"] <- "ME"
levels(sick_score$Race)[levels(sick_score$Race)=="Other"] <- "O"


#let's come up with the most essential, ie. Table 1
#total variables 49. I'll modify the CKD , CAD, CHF, CVA, liver data to reduce it to 1 variable column.
#but first, I'll convert the obvious binaries into 1 and 0 . The edited spreadsheet is still named sick_score for simplicity purpose.
#to filter a variable column into specific level
sick_score %>% select(Race, Type.of.Surgery) %>% filter(str_detect(Race, "African"))
#I want to count which race gets which type of surgery

