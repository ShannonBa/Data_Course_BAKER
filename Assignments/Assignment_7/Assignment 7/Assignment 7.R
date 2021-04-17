library(tidyverse)
library(readxl)

d2 <- read_xlsx("./")




library(tidyr)
library(dplyr)
utah <- read.csv("Data/Utah_Religions_by_County.csv")
names(utah)
utah_long = gather(utah,key = Religion, value = Proportion, -c(1:3) )