################## ----Final Project---- ##########################

## load packages
library(tidyverse)
library(janitor)
library(dplyr)
library(corncob)
library(gplots)
BiocManager::install("DESeq2")
#helper functions testing for abundance (log-fold-changes are actually different from eachtoher)
#https://github.com/gzahn/tools/blob/master/bbdml_helper.R


### load data

df <- read_csv("./41586_2020_2975_MOESM11_ESM.csv", skip=3)
names(df)
names(df) <- names(df) %>% str_replace_all(" ","_")
nam <- "logFC (12 mo (+OSK)/12 mo (-OSK))"
nam %>% str_replace_all(" ","_")


#make log comparison of treated and untreated, young and old


#### try to make the heatmap myself #### 
heatdf <- df[ ,c(                
            "5_mo_rep2"  ,                      
            "5_mo_rep3"   ,                     
            "5_mo_rep4"    ,                    
            "5_mo_rep5"     ,                   
            "12_mo_rep1" ,                      
            "12_mo_rep2"  ,                     
            "12_mo_rep3"  ,                     
            "12_mo_rep4"   ,                    
            "12_mo_rep5"    ,                   
            "12_mo_rep6"     ,                  
            "12_mo_(-OSK)_rep1",                
            "12_mo_(-OSK)_rep2" ,               
           "12_mo_(-OSK)_rep3"  ,              
            "12_mo_(-OSK)_rep4"   ,             
            "12_mo_(-OSK)_rep5"    ,            
            "12_mo_(+OSK)_rep1"     ,           
            "12_mo_(+OSK)_rep2" ,               
            "12_mo_(+OSK)_rep3"  ,              
            "12_mo(+OSK)_rep4")]
heatmap.2(as.matrix(heatdf),
          margins=c(4,15),
          density.info = "none",
          trace = "none")

#another version

heatmap.2(as.matrix(heatdf),
          Rowv = FALSE, Colv = FALSE,
          density.info = "none",
          trace = "none")
 dev.off()
# trying to make heatmap column groups
 
 ha <- HeatmapAnnotation(
   cyl = heatdf$cyl, am = mtcars$am, mpg = mtcars$mpg,
   col = col
 )
df %>% group_by(df, starts_with(df, "5_")) %>% 
heatdf[starts_with(heatdf, "5_"),] 

#heat map that works so far
heatmap(as.matrix(heatdf), Colv = NA)
dev.off()

class(heatdf)
heatdf <- as.matrix(heatdf)

heatmap.2(as.matrix(heatdf), Rowv = NA, Colv = NA), Colv=NA, RowSideColors = c(rep("Blue",4),rep("Red",6)))
heatmap(mat, Colv=NA)

class(heatdf$`12_mo_reps5`)
heatdf <- as.numeric(heatdf)

apply(heatdf, 2, as.numeric)



#### heatmap with ggplot ####
library(tidyr)
library(ggplot2)

mine.long <-pivot_longer(data = heatdf, 
             cols = starts_with("5_"), 
             names_to = "5_month_mice", 
             values_to = "expression")


mine.long <-pivot_longer(data = heatdf, 
                         cols = starts_with(c("5_","12_mo_rep","12_mo_(-OSK)","12_mo_(+OSK)")), 
                         names_to = "mice", 
                         values_to = "expression") 

mine.heatmap 
mutate(mine.heatmap, age = starts_with("5_"), "5 months")

mine.heatmap %>%
  
# in class look at heatmap
heatmatrix <-   heat_matrix(as(heatdf, "matrix"))
  rownames(heat_matrix) <- df$gene
  heatmap(heat_matrix, Colv = NA)
  #stick different mice heatmap together with 
  #semperate heatmap (4 hatmaps) for each of the mice, then stick them all together
  #distance phylogeny is only on the first one (young) but not on the other ones
  #vector of names
  
heat_matrix <- as(heatdf, "matrix")
rownames(heat_matrix) <- df$gene
  mousenames <- c(rep("Young", 5),
                  rep("Old",6),
                  rep("Old(-OSK)", 5),
                  rep("Old(+OSK)",4))
#do pattern matching and replace
mousecolors <-   plyr::mapvalues(mousenames,from=unique(mousenames), to= c("blue","red","gray","black"))
  
  heatmap(heat_matrix, Colv = NA, 
          ColSideColors=mousecolors, 
          margins = c(10,10),
          col=gene_colors)
  
  col <- colorRampPalette(brewer.pal(464, "RdYlBu"))(256)
gene_colors <-   viridis::cividis(dim(heat_matrix)[1])  #need viridis packages

 # select(starts_with("5_")) %>%
  mutate(
    age = starts_with("5_"))

  geom_tile() +
  xlab(label = "mice") +
  #facet_grid(~ group_rows(c("5_","12_mo_rep","12_mo_(-OSK)","12_mo_(+OSK)"))) ## this is not working
  

mine.heatmap

names(heatdf)
names(heatdf)

#### volcano plot ####
names(df)
volcanodf <- df[ ,c(                  
  "logFC_(12_mo_(-OSK)/12_mo)",       
   "PValue"                    ,
  "QValue",                           
  "Ageing" ,
  "logFC_(12m/5m)"        ,
   "PValue_1" ,
  "QValue_1"     ,                    
   "OSK"          ,
  "logFC_(12_mo_(+OSK)/12_mo_(-OSK))",
   "PValue_2"           ,
  "QValue_2"  )]

df <- data.frame(log10FC = log10(EM$Fold.changes),
                 logpv = -log2(EM$pvalues))

names(df)
#make new df
df <- df%>%
  mutate(ageing_sig = case_when(QValue < 0.05 ~ TRUE),
         all_sig = case_when(QValue_1 < 0.05 ~ TRUE),
         both_sig = case_when(QValue_2 < 0.05 ~ TRUE)) 
df <- df %>%
  pivot_longer(cols = ends_with("_sig"), 
               names_to = "significance")

#This just about makes the plot I want, but I need the colors right
ggplot(df, aes(x=`logFC_(12m/5m)`, y=`logFC_(12_mo_(+OSK)/12_mo_(-OSK))`)) +
  geom_point(aes(color=both_sig))
   
dev.off()
names(df)
