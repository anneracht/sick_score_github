getwd()
library(dplyr)
require(gdata)


df = read.xls(("sick_score_xls.xls"), sheet = 1, header = TRUE)
sick_score_original <- rename(df)
head(sick_score)


