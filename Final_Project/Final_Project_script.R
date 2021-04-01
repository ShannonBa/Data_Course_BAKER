################## Final Project ##########################

## load packages
library(tidyverse)
library(janitor)
library(dplyr)
library(deseq2)
library(corncob)
#helper functions testing for abundance (log-fold-changes are actually different from eachtoher)
#https://github.com/gzahn/tools/blob/master/bbdml_helper.R


### load data

df <- read_csv("./41586_2020_2975_MOESM11_ESM.csv", skip=3)
names(df)
names(df) <- names(df) %>% str_replace_all(" ","_")
nam <- "logFC (12 mo (+OSK)/12 mo (-OSK))"
nam %>% str_replace_all(" ","_")


# p-value are in three categories
sigs <- df<0.05
df[sigs] <- "Blue"
#OR
cut(df,3) #makes three categorical groups
df %>%
  mutate(
    dotcolor= case_when(df<0.05~"Blue",
                        TRUE~"Red"))
df %>%
      mutate(
        dotcolor = case_when(FC < 0.05 ~ "Blue",
                             TRUE ~ "Red"))

#could make heatmap that compare the gene expression of 464 genes
#and compares the old to the young mice
#heatmap is a matrix (all the same, numeric)
mat <- matrix(rnorm(1000),nrow = 10,ncol = 10)
heatmap(mat, Colv=NA,RowSideColors = c(rep("Blue",4),rep("Red",6)))
heatmap(mat, Colv=NA)
#make log comparison of treated and untreated, yound and old