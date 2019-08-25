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
sick_score[sick_score == ""] <- "No"

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
  group_by(Type.of.Surgery) %>%
  tally()

#use tally to get a glimpse of all the levels of variables , and discuss w team on what to edit later on
#I want to count the types of surgery based on Race , and find any missing data
sick_score %>% count(Race, Type.of.Surgery, sort = TRUE) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


#group race, type of surgery, and health insurance status
sick_score %>%
  group_by(Race ,Health.Insurance.Status) %>%
  tally() %>%
  arrange(desc(n)) %>% #arrange in descending order
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


#time to look at the comorbids !
sick_score %>%
  group_by(BMI.Surgical.Tracking, OSA) %>%
  tally() %>%
  arrange(desc(n)) %>% kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


summary(sick_score$Health.Insurance.Status)
str(sick_score)

sick_score %>%
  group_by(CAD, METs...4.listed.) %>% tally()%>% arrange(desc(n)) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

#I'm interested in seeing how much missing data there is. This has implications on study validity (both int and ext)
sum(is.na(sick_score[21]))

sick_score %>% group_by(HTN.continued) %>% tally()

colnames(sick_score[34])

#clean up CVA [7] . Hx of CVA wit residu
levels(sick_score$CVA)[levels(sick_score$CVA) == "Carotid Artery Imaging positive for stenosis" ] <- "CAI_Positive"
#clean up CAD [9]. I've divided this to stress positive test with no cath, cath w/o PCI, then all PCI's. All
#PCI's requite cath, mentioning cath with PCI together is redundancy
levels(sick_score$CAD)[levels(sick_score$CAD) == "CathNoStent" ] <- "CathNoPCI"
#clean up PVD [11]. MedsNoAngio = cilostazol without peripheral angio,
levels(sick_score$PVD)[levels(sick_score$PVD) == "Positive Peripheral Angiogram with Angioplasty or Stent placement" ] <- "Angio_endovascular_procedure"
#clean up CHF [12]. Curious about CHF with preserved ejection fraction category.
levels(sick_score$CHF)[levels(sick_score$CHF) == "No CHF / Not Charted" ] <- "No"
#clean up CHF.continued. and .continued.1 [12].
levels(sick_score$CHF.continued)[levels(sick_score$CHF.continued) == "No BNP >500 w/o CKD" ] <- "No_BNP_No_CKD"
levels(sick_score$CHF.continued.1)[levels(sick_score$CHF.continued.1) == "" ] <- "No"
levels(sick_score$CHF.continued.2)[levels(sick_score$CHF.continued.2) == "History of Ventricular Tachycardia or Ventricular Fibrillation or Implanted Cardiac Device or Pacemaker in place" ] <- "VT/VF/ICD/Pacemaker"

#clean up DM data. It's not the best. Is BG a range taken over a period of time or one off reading?
#It's very different from HbA1c. For simplicity sake Im going to categorize this to no DM, controlled and uncontrolled DM
#controlled DM = Blood Glucose (BG) 120-149 or glycated hemoglobin (HbA1C within 3mths of surgery) 6-6.9 . Beyond is uncontrolled.
levels(sick_score$DM)[levels(sick_score$DM) == "Unontrolled_DM" ] <- "Uncontrolled_DM"
#cleaning up hypertension data. Do LVH / end organ damage patients have meds too? Assuming they do, I'm slightly confused.
#why do we classify HTN on meds as a category on its own.
sick_score %>% group_by(HTN) %>% tally()
levels(sick_score$HTN)[levels(sick_score$HTN) == "HTN_end_organ_damage" ] <- "HTN_uncontrolled"
levels(sick_score$HTN.continued)[levels(sick_score$HTN.continued) == "Number 1 with Left Ventricular Hypertrophy (LVH) on electrocardiogram (EKG) or echocardiogram (ECHO)" ] <- "LVH"
#cleaning up hyperlipidemia data
sick_score %>% group_by(HLD) %>% tally()
levels(sick_score$HLD)[levels(sick_score$HLD) == "" ] <- "No"
#cleaning up COPD data. RVSP > 40 is classified into dilated RV with or without EF compromise.
#PFTs suggestive of Obstructive Disease or No PFTs on Bronchodilator Therapy <- dont get it.
sick_score %>% group_by(COPD) %>% tally()
levels(sick_score$COPD)[levels(sick_score$COPD) == "No Bronchodilator Therapy" ] <- "No_meds"
#clean up OSA data
sick_score %>% group_by(OSA) %>% tally()
levels(sick_score$OSA)[levels(sick_score$OSA) == "Positive Sleep Study/ Diagnosis of OSA - not compliant with CPAP" ] <- "CPAP_noncompliant"
#clean up CKD data 22,23,24,25,26
sick_score %>% group_by(CKD.continued.1) %>% tally()
levels(sick_score$CKD)[levels(sick_score$CKD) == "No CKD / Not Charted" ] <- "No"
levels(sick_score$CKD.continued.1)[levels(sick_score$CKD.continued.1) == "No Electrolyte abnormality" ] <- "electrolyte_normal"
sick_score %>% group_by(CKD.continued.2) %>% tally()
levels(sick_score$CKD.continued.2)[levels(sick_score$CKD.continued.2) == "N/A" ] <- "Not_anemic"
sick_score %>% group_by(CKD.continued.4) %>% tally()
levels(sick_score$CKD.continued.4)[levels(sick_score$CKD.continued.4) == "N/A" ] <- "BUN_normal"
#clean up Liver Disease col 27. fatty_liver includes NAFLD and AFLD
sick_score %>% group_by(Liver.Disease) %>% tally()
levels(sick_score$Liver.Disease)[levels(sick_score$Liver.Disease) == "Cirrhosis" ] <- "cirrhosis"
sick_score %>% group_by(Liver.Disease.other.factors.2) %>% tally()
levels(sick_score$Liver.Disease.other.factors.1)[levels(sick_score$Liver.Disease.other.factors.1) == "Transaminitis" ] <- "transaminitis"
#not sure about liver disease factors 2. Albumin >2 criteria? INR without anticoag, shortened to INR <1.5 or >1.5
sick_score %>% group_by(Liver.Disease.other.factors.5) %>% tally()
levels(sick_score$Liver.Disease.other.factors.3)[levels(sick_score$Liver.Disease.other.factors.3) == "International Normalized Ration (INR) >1.5 without concomitant Anticoagulation" ] <- "INR_1.5_above"
levels(sick_score$Liver.Disease.other.factors.4)[levels(sick_score$Liver.Disease.other.factors.4) == "Not Charted" ] <- "no_hypoglycemia"
#omit column 32
#clean up alcohol history column 34.
sick_score %>% group_by(Alcohol) %>% tally()
levels(sick_score$Alcohol)[levels(sick_score$Alcohol) == "No history of Alcohol abuse" ] <- "No_abuse"
#clean up tobacco info
colnames(sick_score[35])
sick_score %>% group_by(Tobacco) %>% tally()
levels(sick_score$Tobacco)[levels(sick_score$Tobacco) == "Active Smoker" ] <- "Smoker"
#clean up drug use info
colnames(sick_score[36])
sick_score %>% group_by(Illicit.Drug.Use) %>% tally()
levels(sick_score$Illicit.Drug.Use)[levels(sick_score$Illicit.Drug.Use) == "History of Intravenous Drug Abuse (IVDA) without health complications" ] <- "Yes"
# Illicit..Drug.Use , i'll tidy this up later
colnames(sick_score[37])
sick_score %>% group_by(Illicit..Drug.Use) %>% tally()
# narcotic use
colnames(sick_score[38])
sick_score %>% group_by(Narcotic) %>% tally()
levels(sick_score$Narcotic)[levels(sick_score$Narcotic) == "Patient on 50 -90 MME/day" ] <- "50_90_MME"
# METS data
colnames(sick_score[39])
sick_score %>% group_by(METs...4.listed.) %>% tally()
# ASA
colnames(sick_score[40])
sick_score %>% group_by(ASA) %>% tally()
#city ! yay . count them. So much we can do with this data.
colnames(sick_score[41])
levels(sick_score$City)
sick_score %>% count(City, sort=TRUE)

#
(sick_score[46])
names(sick_score)[46]<-"ED.Visit.30.Day"

#count the complications . Seems like ~ 7-8% had 30 day readmission for something
levels(sick_score$ED.Visit.30.Day)
sick_score %>% count(ED.Visit.30.Day , sort=TRUE)

# ~30 patients were readmitted
colnames(sick_score[47])
levels(sick_score$Readmit.30.Day)
sick_score %>% count(Readmit.30.Day , sort=TRUE)

#
colnames(sick_score[48])
names(sick_score)[48]<-"Reoperation.30.Day"
#types of reoperation done
sick_score %>% count(Reoperation.30.Day , sort=TRUE)

#clean up data on city. replace Eastpointe for East Pointe. Replace for Dearborn Hghts as well
levels(sick_score$City)
levels(sick_score$City)[levels(sick_score$City) == "Macomb" ] <- "Macomb Twp."

# populate the longitude and latitude for each of the cities using a for loop


#define the lat and long for each city

for (row in 1:nrow(stock)) {
  price <- stock[row, "apple"]
  date  <- stock[row, "date"]

  if(price > 117) {
    print(paste("On", date,
                "the stock price was", price))
  }
}




write.csv(sick_score, file = "sick_score/sick_score_github2.csv", row.names = FALSE)
write.csv(sick_score,"/Users/Desktop/sick_score_github.csv", row.names = FALSE)

#to replace NA with No or 0
sick_score$METs...4.listed.[is.na(sick_score$METs...4.listed.)] <- "No_METS_charted"
