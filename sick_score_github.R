getwd()
library(dplyr)
require(gdata)

#Pull data from desktop folder
df = read.xls(("sick_score_xls.xls"), sheet = 1, header = TRUE)
sick_score_original <- rename(df)
head(sick_score)

#convert variables to diff names

