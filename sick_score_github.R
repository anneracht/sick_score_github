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

require(gdata)

#Pull data from desktop folder
df = read.xls(("sick_score_xls.xls"), sheet = 1, header = TRUE)
sick_score_original <- rename(df)
head(sick_score)
#open up the spreadsheet on R
view(sick_score)
# Inspecting data. View the no. of variables and levels for each variable.
sick_score %>% tbl_vars()
str(sick_score)
#total variables 49. I'll modify the CKD , CAD, CHF, CVA, liver data to reduce it to 1 variable column.
#but first, I'll convert the obvious binaries into 1 and 0 . The edited spreadsheet is still named sick_score for simplicity purpose.


