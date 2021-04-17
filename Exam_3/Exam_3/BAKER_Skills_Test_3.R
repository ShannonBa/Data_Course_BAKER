################ BAKER_Skills_Test_3 ######################


#### Load libraries ####

library(tidyverse)
library(janitor)
library(broom)
library(modelr)


#### Task I. #### 

df <- read.csv("./FacultySalaries_1995.csv")
names(df) <- make_clean_names(names(df))
names(df)

df <- df %>%
  pivot_longer(c(avg_full_prof_salary,avg_assoc_prof_salary, avg_assist_prof_salary),
                names_prefix = "_salary", 
               values_to = "salary",
               names_to = "rank")
df <- df[!df$tier %in% "VIIB", ]


ggplot(df, aes(x= rank, y = salary, fill =rank)) +
  geom_boxplot( size = 1, outlier.size=3) +
  facet_wrap(~tier)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45)) +
  labs(y="Salary",x="Rank", fill = "Rank" )+
  scale_fill_discrete(name = "Rank", labels = c("Assist", "Assoc", "Full"))+
  scale_x_discrete(labels=c("avg_full_prof_salary" = "Full", 
                            "avg_assoc_prof_salary" = "Assoc",
                            "avg_assist_prof_salary" = "Assist"))
ggsave("BAKER_Fig_1.jpg")


#ggsave("BAKER_Fig_1.jpg")
#everything looks good except my graphs are wider....


#### Task II. #### 
# influence of "State", "Tier", and "Rank" on "Salary" 

aov.df <- aov(salary ~ tier + rank + state,  data = df)
summary(aov.df)
a <- summary(aov.df)
capture.output(a, file= "Salary_ANOVA_Summary.txt") 



#### Task III. ####

df2 <- read.csv("./Juniper_Oils.csv")
names(df2)
df2 <- df2 %>%
  pivot_longer(c("alpha.pinene","para.cymene","alpha.terpineol","cedr.9.ene","alpha.cedrene",
                 "beta.cedrene","cis.thujopsene","alpha.himachalene","beta.chamigrene",
                 "cuparene","compound.1","alpha.chamigrene","widdrol","cedrol","beta.acorenol",
                 "alpha.acorenol","gamma.eudesmol","beta.eudesmol","alpha.eudesmol",
                 "cedr.8.en.13.ol","cedr.8.en.15.ol","compound.2","thujopsenal"),
             values_to = "Concentration",
             names_to = "ChemicalID")



#### Task IV. ####


ggplot(df2, aes(x= YearsSinceBurn, y = Concentration)) +
  geom_smooth() +
  facet_wrap(~ChemicalID, scales = "free_y") +
  theme_minimal()
ggsave("BAKER_Fig_2.jpg")


#### Task V. ####

mod1 <- glm(data = df2, Concentration ~ YearsSinceBurn * ChemicalID)

summary(mod1)
tidy(mod1) 
tidy(mod1) %>%
  filter(p.value <= 0.05) %>%
  mutate(term = term %>% str_remove_all("ChemicalID"))


#### Task VII. ####

# added a knitted html file called 'index.Rmd' of Exam 3: https://shannonba.github.io/Exam_3/

