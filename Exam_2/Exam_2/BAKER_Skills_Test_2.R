####################BAKER_Skills_Test_2.R##########################

library(tidyverse)
library (carData)
library(janitor)


#### Task I: ####
#Load the landdata-states.csv file into R
# Re-create the graph shown in "fig1.png"
landdata <- read_csv("./BIOL3100_Exams/Exam_2/landdata-states.csv")
names(landdata)
names(landdata) <- names(landdata) %>% make_clean_names()
ggplot(landdata, aes(x=year, y = land_value, color=region))+
  geom_smooth() +
  labs(y= "Land Value (USD)", x= "Year", color = "Region") + 
  theme_minimal()+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
ggsave("fig1.png")

#### Task II: ####
##What is "NA Region?"

which(is.na(landdata), arr.ind=TRUE)
landdata %>%
  rowid_to_column() %>%
  filter(is.na(region))
newdataset1<-landdata[which(is.na(landdata$region)),]

#region refers to the region of the United States the state is in. The only 
#state that in the "NA Region" is DC, which is the capitol of the US, so it is 
#not designated a region. I suppose that is to be neutral between the states. 




#### Task III: ####
#The rest of the test uses another data set. The unicef-u5mr.csv data. 
#Get it loaded and take a look.
#It's not exactly tidy. You had better tidy it!

three <- read_csv("./BIOL3100_Exams/Exam_2/unicef-u5mr.csv")
names(three)
three_change <- three %>%
  pivot_longer(cols= c("U5MR.1950" ,  "U5MR.1951",   "U5MR.1952" ,  "U5MR.1953" ,  "U5MR.1954"  ,
                       "U5MR.1955" ,  "U5MR.1956"  , "U5MR.1957" ,  "U5MR.1958",   "U5MR.1959",   "U5MR.1960" , 
                        "U5MR.1961" ,  "U5MR.1962",   "U5MR.1963" ,  "U5MR.1964",   "U5MR.1965",   "U5MR.1966" , 
                        "U5MR.1967"  , "U5MR.1968" ,  "U5MR.1969"  , "U5MR.1970" ,  "U5MR.1971" ,  "U5MR.1972"  ,
                       "U5MR.1973"  , "U5MR.1974" ,  "U5MR.1975",   "U5MR.1976"   ,"U5MR.1977"   ,"U5MR.1978"  ,
                        "U5MR.1979"  , "U5MR.1980" ,  "U5MR.1981",   "U5MR.1982",   "U5MR.1983",   "U5MR.1984"  ,
                        "U5MR.1985" ,  "U5MR.1986"  , "U5MR.1987" ,  "U5MR.1988" ,  "U5MR.1989" ,  "U5MR.1990"  ,
                        "U5MR.1991"  , "U5MR.1992",   "U5MR.1993"  , "U5MR.1994"  , "U5MR.1995"  , "U5MR.1996"  ,
                        "U5MR.1997" ,  "U5MR.1998" ,  "U5MR.1999"   ,"U5MR.2000"   ,"U5MR.2001",   "U5MR.2002"  ,
                        "U5MR.2003"  , "U5MR.2004"  , "U5MR.2005",   "U5MR.2006",   "U5MR.2007" ,  "U5MR.2008"  ,
                        "U5MR.2009" ,  "U5MR.2010"  , "U5MR.2011" ,  "U5MR.2012" ,  "U5MR.2013"  , "U5MR.2014"  ,
                        "U5MR.2015" ),
               names_to= "year",
               values_to="U5MR")
#remov 'U5MR.' from year number
stopwords = c("U5MR.")
three_change$year <- gsub(paste0(stopwords,collapse = "|"),"", three_change$year)




#### Task IV: ####     
#Re-create the graph shown in fig2.png
#Export it to your Exam_2 folder as LASTNAME_Fig_2.jpg (note, that's a jpg, not a png)
class(three_change$year)
three_change$year <- as.numeric(three_change$year)

ggplot()
ggplot(three_change, aes(x=(year), y=U5MR, color=Continent))+
  geom_point(size=2) +
  theme_minimal() +
  labs(x = "Year", y= "MortalityRate", key= "Continent" ) 
ggsave("BAKER_Fig_2.jpg")




#### Task V: ####
#Re-create the graph shown in fig3.png
#Note: This is a line graph of average mortality rate over time for each continent 
#(i.e., all countries in each continent, yearly average), this is NOT a geom_smooth() 
#Export it to your Exam_2 folder as LASTNAME_Fig_3.jpg (note, that's a jpg, not a png)


average <- three_change %>%
  group_by(Continent, year) %>%
  summarise( U5MR = mean(U5MR, na.rm = TRUE))

class(average$year)
average$year <- as.numeric(average$year)

 ggplot(average, aes(x=year, y=U5MR, col=Continent,  group=Continent))+
  geom_line(size=2) +
  theme_minimal() +
  labs(x = "Year", y= "Mean Mortality Rate (deaths per 1000 live births)", key= "Continent" ) 
ggsave("BAKER_Fig_3.jpg")
 


 







#### Task V: ####
#Re-create the graph shown in fig4.png
#Note: The y-axis shows proportions, not raw numbers
#This is a scatterplot, faceted by region
#Export it to your Exam_2 folder as LASTNAME_Fig_4.jpg (note, that's a jpg, not a png)
ggplot(three_change, aes(x=year, y=(U5MR/1000)))+
  geom_point(col="blue", size=.25) +
  geom_smooth(method="lm", size=.25) + 
  labs(x = "Year", y= "Mortality Rate", key= "Continent" ) +
 facet_wrap('Region') +
 theme_minimal() +
  theme(strip.background = element_rect( colour="black", size=1))
ggsave("BAKER_Fig_4.jpg")


