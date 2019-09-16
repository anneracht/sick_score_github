#1. find cutoff values to split it into intervals. It requires non-parametric LOESS smoothing
# technique to describe the relationship between the variable and the outcome.

#2.



#amazing references
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5690964/
#https://cran.r-project.org/web/packages/tableone/vignettes/smd.html
# https://www.ncbi.nlm.nih.gov/pubmed/22521443

#comorbidity analysis according to age , BMI , gender in obese patients
sss #contains the data for analysis. variables will be numbered.

#calculate the top comorbidities
sss %>% count(CVA , sort=TRUE)
sss %>% count(CAD, sort = TRUE)
sss %>% count(PVD, sort = TRUE)
sss %>% count(CHF, sort=TRUE)
sss %>% count(DM, sort=TRUE)
sss %>% count(HTN, sort=TRUE)
sss %>% count(HLD, sort=TRUE)
sss %>% count(COPD, sort=TRUE)
sss %>% count(OSA, sort=TRUE)
sss %>% count(CKD, sort=TRUE)
sss %>% count(Liver.Disease, sort=TRUE)
sss %>% count(Alcohol, sort=TRUE)
sss %>% count(Tobacco, sort=TRUE)

#delete columns from sss first
sss$CHF.continued <- NULL
sss$CHF.continued.1 <- NULL
sss$CHF.continued.2 <- NULL
sss$Liver.Disease.other.factors.5 <- NULL
sss$Study.Number <- NULL
sss$Age <- NULL
sss$Health.Insurance.Status <- NULL
sss$Gender <- NULL
sss$CVA.continued <- NULL
sss$CAD.Continued <- NULL
sss$HTN.continued <- NULL
sss$CKD.continued.3 <- NULL
sss$CKD.continued.4 <- NULL
sss$Liver.Disease.other.factors.1 <- NULL
sss$Liver.Disease.other.factors.2 <- NULL
sss$Liver.Disease.other.factors.3 <- NULL
sss$Liver.Disease.other.factors.4 <- NULL
sss$Type.of.Surgery <- NULL
sss$Race <- NULL
sss$BMI.Surgical.Tracking <- NULL
sss$ASA.2 <- NULL
sss$Reoperation.30.Day <- NULL
sss$Readmit.30.Day <- NULL
sss$ED.Visit.30.Day<- NULL
sss$LOS <- NULL
sss$Discharge.Date <- NULL
sss$Surgery.Date <- NULL
sss$State <- NULL
sss$City <- NULL
sss$ASA <- NULL
ss_comorbidity <- as.matrix(sss)

# Matrix multiplication for cooccurrence counts
#convert the dataframe to boolean matrix
ss_comorbidity[ss_comorbidity== "NA" ] <- as.numeric(0)
replace(ss_comorbidity, is.na(ss_comorbidity), 0)
ss_comorbidity[ss_comorbidity>=1] <- 1
ss_comorbidity[is.na(ss_comorbidity)] <- 0
ss_comorbidity <- as.matrix(ss_comorbidity, fill = T)

ss_comorbidity_df <- as.data.frame(ss_comorbidity, stringsAsFactors = FALSE)
#convert to matrix and numeric
ss_comorbidity_1 <- as.matrix(ss_comorbidity_df)
head(ss_comorbidity_1)
#check if matrix contents are character, and convert it to numeric
is.character(ss_comorbidity_1)
class(ss_comorbidity_1) <- "numeric"
head(ss_comorbidity_1)

ss_comorbidity_cor <- t(ss_comorbidity_1) %*% ss_comorbidity_1


#heatmap(ss_comorbidity_cor)
temp = sapply(colnames(ss_comorbidity), function(x)
  sapply(colnames(ss_comorbidity), function(y)
    sum(rowSums(ss_comorbidity_1[,c(x, y)]) == 2)))



#comorbidity network of co-occurrences
ss_comorbidity_df
ss_comorbidity[,1:16]
dat <- ss_comorbidity[,1:16]
cor(ss_comorbidity_1)



#reference
#http://www.medsci.org/v13p0099.htm#T3
#https://www.nature.com/articles/s41598-018-36973-1
