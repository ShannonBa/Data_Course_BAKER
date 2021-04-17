library(tidyverse)
library(Rpdb)
library(janitor)
```{r,message=FALSE
```
BROC <- read.pdb("./1HXB hiv b with sqv_NO_BROC")
df<-BROC[!(BROC1=="BROC",]

```{r,message=FALSE,warning=FALSE}
df <- read.csv("./Data/Juniper_Oils.csv")
names(df) <- names(df)%>% make_clean_names()
names(df)

compounds <- names(df)[11:33]
df%>% pivot_longer(compounds)
long <- df %>%
  filter(amplicon == "16S" )%>%
  pivot_longer(compounds,names_to="compound",values_to="yield")

gplimpse(long)
```
#now that cleaned up a bit, we can plot the compound yields over time since burn...





















