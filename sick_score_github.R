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

#convert variables to


