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
  geom_raster(aes(fill = LOS))+
  labs(title ="Heat Map", x = "Age", y = "BMI")+
  scale_fill_continuous(name = "length of stay")

Read more at: https://www.tatvic.com/blog/7-visualizations-learn-r/?utm_source=copytext&utm_medium=text&utm_campaign=textshare


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
#I want to count which race gets which type of surgery

summary(sick_score$Health.Insurance.Status)
levels(sick_score$Race)
