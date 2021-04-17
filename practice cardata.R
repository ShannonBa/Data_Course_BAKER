library(tidyverse)
library (carData)
library(janitor)
data("MplsStops")
data("MplsDemo")

#join datasets

full <- full_join(MplsStops, MplsDemo, by="neighborhood")
names(MplsStops)

ggplot(full, aes(x=lat,y=long,color=poverty))+
  geom_boxplot()


full$race %>% unique

ggplot(full, aes(x=lat,y=long))+
  geom_point(aes(color=race))+
  geom_point(aes(color=poverty),alpha=.2)


ggplot(full, aes(x=race))+
    geom_bar()

full %>% 
    filter (race %in% c("White","Black")) %>%
  ggplot(aes(x=race,fill=black))+
  geom_bar()+
  facet_wrap(~cut(black))

df <- read_csv("../../Data/Bird_Measurements.csv")

#anything that ends in N can be thrown out (that's just the number of counts)
  #F is female, M is male
#this data is messy
#need column for sex
#mass,wing,tarsus, and tail are all different body parts and 
  #could all be their own column
#clutch is fine, egg_mass, mating system are fine

df3 <- read_csv("./Data/cleaned_bird_data.csv")
#cleaned bird data that can be compared to


#any time you want to change stylisic stuff, to the 
  #'theme(axis.text.x= element_text(angle=90, hjust=1))
  #'that should make the text vertical so it is easier to read
  #'
  #If there is a date, say 'as.Date()' ; time zones and everything will
  #be taken into account
day1 <-  as.Date('11-11-2020') #day-month-year
day2 <- as.Date("15-11-2020")
difftime(day1,day2) #tells how many days between day1 and day2

as.POSIXct() # is the unix way of counting dates 

weekdays(day1)#tells yuo what day of the week this day was
co2

#### clean
names(df) <- names(df) %>% make_clean_names()
df_s_w <- df %>%
  filter( grepl("^Soil|Water$", sample_id)) %>%
  pivot_longer(c(m_mass, f_mass),
               names_prefix = "_mass",
               values_to= "sex",
               names_to= "mass")
names(df)
df2 <- df %>%
  pivot_longer(c(m_mass,  f_mass, unsexed_mass),
               #names_prefix = "M_",
               values_to= "mass",
               names_to= "sex")

df2 <- df2 %>%
  pivot_longer(c(m_wing, f_wing, unsexed_wing),
             #name_suffix = "_mass",
             values_to= "wing",
             names_to= "w_sex") 
df2 <- df2 %>%
  pivot_longer(c(m_tarsus, f_tarsus, unsexed_tarsus),
               values_to= "tarsus",
               names_to= "tarsus_sex") 
df2 <- df2 %>%
  pivot_longer(c(m_bill, f_bill, unsexed_bill),
               values_to= "bill",
               names_to= "bill_sex") 
df2 <- df2 %>%
  pivot_longer(c(m_tail, f_tail, unsexed_tail),
               values_to="tail",
               names_to= "tail_sex")
names(df2) # oh, just do the join thing... maybe.. they'll have different names, but it's still by the sex...
            #full_join(x, y, by = "id")
?filter()
mutate(df2, gender = "sex")

df4 <- df %>%
  pivot_longer(c(m_tarsus),
               values_to = "tarsus",
               names_to= "male") 
df4 <- df4 %>%
  pivot_longer(f_tarsus,
               values_to = "tarsus1",
               names_to = "female")
mutate(df4, male = "male" )
df4%>%
full_join(tarsus, tarsus1, by = "id")
df4%>%
  #union(tarsus, tarsus1)
full_join(male, female, by = "tarsus")




#tidy together
names(df) <- names(df) %>% make_clean_names()

df %>%
    select (starts_with("m")) %>%
  names()
male <-  df %>%
  select(c("family","species_number","species_name","english_name", 
           "clutch_size","egg_mass", "mating_system", starts_with ("m")))%>%
  select(-ends_with("n")) %>%
  mutate(sex= "male")        

names(male) <- names(male) %>% str_remove_all ("m_")

female <-  df %>%
  select(c("family","species_number","species_name","english_name", 
           "clutch_size","egg_mass", "mating_system", starts_with ("f")))%>%
  select(-ends_with("n")) %>%
  mutate(sex= "female")        

names(female) <- names(female) %>% str_remove_all ("f_")

unsexed <-  df %>%
  select(c("family","species_number","species_name","english_name", 
           "clutch_size","egg_mass", "mating_system", starts_with ("unsexed")))%>%
  select(-ends_with("n")) %>%
  mutate(sex= "unsexed")        

names(unsexed) <- names(unsexed) %>% str_remove_all ("unsexed_")

#join them back togehter

names(unsexed)
names(male)
names(female)
identical(names(unsexed),
          names(male))
 full <-  rbind(unsexed,male,female)
full$mating_system <-  as.factor(full$mating_system)
 
#explore clean data #### 
 
names(full)
plot(full$mass) #just see the distribution
ggplot (full, aes(x=mass)) + geom_density()

ggplot (full, aes(x=clutch_size)) + geom_histogram()
ggplot (full, aes(x=tarsus)) + geom_density()
ggplot (full, aes(x=wing)) + geom_density()


#super cleaned, put all the data together: 

long <- full %>%
    pivot_longer(cols= c("mass","wing" ,"tarsus", "bill", "tail", "egg_mass" ),
                 names_to= "measurement",
                 values_to="value")
ggplot(long, aes(x=value)) + 
  geom_density() + 
  facet_wrap(~measurement, scales ="free")

saveRDS(full, "./full_cleaned_bird_data.RDS")

#can we analyze egg_mass by spceis??? conclusion: no, too many individual species

table(full$species_name)
 #three for each specie, but that is because f, m , and unsexed

full %>% 
    filter(english_name == "Ostrich")

#can we analyze egg_mass by mating_system

table(full$mating_system)

ggplot(full, aes(x=mating_system, y=egg_mass))+
  geom_boxplot()+
  lims(y=c(0,500))

# get rid of ostrich outlier so added limit (lims)


#test whether 5 really is bigger than 4 (statistics)

#analasis of variance = aov()  
  # formula is always wirtten as response ~ var1 + var2 ....
#dependent variable is egg_mass
     mod1 <-  aov(data= full,
          formula = egg_mass ~ mating_system)
summary (mod1)                
# df is degrees of freedoms, Sum of squares, mean squared, f value, 
# probablility of f (Pr(>F)  is the p-value)      
#tells significance
# "missingness" is the NA data
# this data tells us that the mating system has an effect on egg_mass
    # isn't telling us 4 from 5, just overall, there is a difference


