library(tidyverse)
df = read.csv("../Data/BioLog_Plate_Data.csv")
df <- read_csv("C:/Users/shann/Desktop/Data_Course_BAKER/Data/BioLog_Plate_Data.csv")
library(dplyr)
library(janitor)
glimpse(df)
?plyr::mapvalues()

# write a command the subsets the BioLog data to Clear_Creek samples, with dilution of 0.01, and only "Glycogen"
names(df)

df_sub <- df %>%
  filter(`Sample ID` == "Clear_Creek") %>%
  filter ( Dilution==0.01) %>%
  filter (Substrate==("Glycogen"))


# Now plot those three replicates over time
df_time <- df_sub %>%
  pivot_longer(c(hr_24, hr_48, hr_144),
               names_prefix = "hr_",
               values_to= "absorption",
               names_to= "hr")
ggplot(df_time, aes(x=hr, y=Rep))+
  geom_point()+
  labs(title="Replicates over time", 
       x="time", y="replicates" ) +
  theme_minimal()


class(df_time$hr)
df_time$hr <- as.numeric(df_time$hr)
class(df_time$Rep)

#ggplot(df, aes(x=(c("Hr_24","Hr_48","Hr_144")), y="Rep")+
 #                    geom_point()+
  #       labs(title="Replicates over time", 
   #                y="time", x="replicates" ) +
    #            theme_minimal())
#####################plot#######################################
ggplot(df_all, aes(y=hr, x=rep))+
         geom_point()+
         labs(title="Replicates over time", 
              y="time", x="replicates" ) +
         theme_minimal()

# Make a plot of Tween 80 utilization over time for ALL the samples, colored by Sample.ID
df_tween <- df %>%
  filter(substrate == "Tween 80") %>%
  pivot_longer(c(hr_24, hr_48,hr_144),
               names_prefix = "hr_",
               values_to= "absorption",
               names_to= "hr")
ggplot(df_tween, aes(y=absorption, x= hr))+
  geom_point(aes(color=sample_id),size=2)+
  labs(title="Tween 80 Samples", 
       y="absportion", x="time (hr)" )


#changing the class to numeric doesn't seem to help with graph
#class(df_tween$hr) <- as.numeric(df_tween$hr)
class(df_s_w$absorption)
  # geom_point()

  
ggplot(df_all, aes(x=(subtrate == `Tween 80`), y=hr, color="Sample ID"))

# Now, same plot, but combine both soils and both waters into soil and water groups and color by soil vs water
df_s_w <- df %>%
  filter( grepl("^Soil|Water$", sample_id)) %>%
  pivot_longer(c(hr_24, hr_48, hr_144),
               names_prefix = "hr_",
               values_to= "absorption",
               names_to= "hr")
ggplot(df_s_w, aes(y=absorption, x= hr))+
  geom_point(aes(color=sample_id),size=2)+
  labs(title="Soil and Water Samples", 
       y="absportion", x="time (hr)" )

class(df_s_w$hr) <- as.character(df_s_w$hr)
class(df_s_w$absorption)

#together in class



  #class(df$sample_id)
  #df[grepl("*Soil" & "water*", df$sample_id), ]
 # filter(df, substr(sample_id,1,1) == "Soil")
#  filter(df, sample_id== "Soil*" & "*Water")
#  filter(df, grepl("^Soil", sample_id),grepl("Water", sample_id)) &
#    filter(df, grepl("Water", sample_id))

##  filter(df, grepl("^Soil|Water$", sample_id))
  #filter(Substrate ==( starts_with("Soil") & ends_with("Water"))) %>%
  pivot_longer(c(Hr_24, Hr_48, Hr_144),
               names_prefix = "Hr_",
               values_to= "absorption",
               names_to= "hr")
#ggplot(dr_s_w, aes(y=(("absorption")), x= "hr", color= "Sample ID"))+
 # geom_point()
?filter()
# Make a table of summary statistics: for each combination of Sample.ID and Substrate, give:
# -- Number of observations
names(df) <- names(df) %>% make_clean_names()


df_all <- df %>%
  pivot_longer(c(hr_24, hr_48, hr_144),
               names_prefix = "hr_",
               values_to= "absorption",
               names_to= "hr")




df_all %>%
  group_by(sample_id, substrate) %>%
  summarise(number_abspoption_observations = n())

# -- Mean absorbance value

df_all %>%
  group_by(sample_id, substrate) %>%
  summarise(mean_absoption = mean(absorption))

# together
df_all %>%
  group_by(sample_id, substrate) %>%
  summarise(number_abspoption_observations = n(),
            mean_absoption = mean(absorption))



# Example output ....

# Sample.ID     Substrate                       N Mean
# <fct>         <fct>                       <int> <dbl>
# 1 Clear_Creek 2-Hydroxy Benzoic Acid         27 0.0562
# 2 Clear_Creek 4-Hydroxy Benzoic Acid         27 0.247 
# 3 Clear_Creek D-Cellobiose                   27 0.403 
# 4 Clear_Creek D-Galactonic Acid γ-Lactone    27 0.314 
# 5 Clear_Creek D-Galacturonic Acid            27 0.385 
# 6 Clear_Creek D-Glucosaminic Acid            27 0.154 
# 7 Clear_Creek D.L -α-Glycerol Phosphate      27 0.0335
# 8 Clear_Creek D-Mallic Acid                  27 0.170 
# 9 Clear_Creek D-Mannitol                     27 0.346 
# 10 Clear_Creek D-Xylose                       27 0.0323
# 




#### all together in class


df_clean <- df %>%
  pivot_longer(c(hr_24, hr_48, hr_144),
               names_prefix = "hr_",
               values_to= "absorption",
               names_to= "hr") %>%
  mutate(hr= as.numeric(hr),
         sample_type= case_when(sample_id %in%
                                  c("Clear_Creek", "Waste_Water")~ "water",
                                sample_id %in% c("Soil_1","Soil_2")~"soil"),
         nonsense_variable= case_when((dilution < 0.05 ~ "VeryDiluted") ,
                                       dilution > 0.05 ~ "Less Diluted"))
  

df_clean %>%
  filter(dilution == 0.001) %>%
  ggplot ( aes(x=hr, y=absorption, color = sample_id))+
  geom_smooth(se=FALSE)+
  facet_wrap(substrate)
df_clean$sample_id %>% unique()

#"Clear_Creek" ...water
#"Soil_1" ,,,soil
#"Soil_2"  ....soil
#"Waste_Water" ... water


df_clean %>%
  filter(dilution == 0.001) %>%
  ggplot ( aes(x=hr, y=absorption, color = sample_type))+
  geom_smooth(se=FALSE)+
  facet_wrap(~substrate)







