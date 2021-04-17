library(tidyverse)
library(tidyverse)
library (carData)
library(janitor)
library(modelr)
library(broom)
library(tidyverse)
library(fitdistrplus)

df <- read_csv("../../Data/GradSchool_Admissions.csv")

glimpse(df)
df$admit <- as.logical(df$admit) #changes 1's and 0's to TRUE and FALSE
df$rank <- as.factor(df$rank)

mod1 <- glm(data=df,
            formula= admit~ gre*gpa*rank,
            family = "binomial") #glm model is the ONLY model that will let you do "family='binomial'"
#logistic regression(S-shaped curve) is better than a linear regression (straight line)

summary(mod1)


add_predictions(df, mod1, type= "response") %>%#'binomial' and 'response' make the s-shape; we get a percentage
  ggplot(aes(x=gpa, y=pred, color=rank)) +
  geom_smooth()

add_predictions(df, mod1, type= "response") %>%
  ggplot(aes(x=gre, y=pred, color=rank)) +
  geom_smooth()

df %>%
  filter(gpa>3.9) %>%
  filter (gre>750) #it is odd how some people are not admitted, but it 
                  #could be for a reason we do not see

#How far the the points from our best fit line?
sqrt(mean(residuals(mod1)^2))
#we may need more variables to compare to
#there are differences based on race, gender, where your from, program may all affect whether someone got in or not
#the more predictors, the more accurate your predictions will be

library(lmerTest) #linear mixed effects regresstion
lmer(data=df, #lmer is similar to glm, but allows you to specifiy is omething is nested insite another
     formula = admit ~ gre*gpa*rank, # can determine is a variable is fixed or not)
     family= "binomial") #if TRUE/FALSE, then use binomial family!!


#took data from Assignment_8
mod2 <- lmer(data=three_change,
             formula = U5MR ~ (1 |Continent)* year) #this didn't work, but can check 'how to use lmer' on class website

summary(mod2)

add_predictions(three_change,mod2) %>%
  ggplot(aes(x=year,color=Continent)) +
  geom_point(aes(y=U5MR),alpha=.2, color="black")+
  geom_point(aes(y=pred))





