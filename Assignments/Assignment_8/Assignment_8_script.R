############## Assignment 8  ########################
#load packages
library(modelr)
library(broom)
library(tidyverse)
library(fitdistrplus)

#### loads the “/Data/mushroom_growth.csv” data set #### 
mushroom <- read.csv("../../Data/mushroom_growth.csv")

# creates several plots exploring relationships between the response and predictors #### 


mod1 = lm(GrowthRate ~ Nitrogen, data = mushroom)

ggplot(mushroom, aes(x=Nitrogen,y=GrowthRate)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()
#Nitrogen has a non-linear regression line
ggplot(mushroom, aes(x=Nitrogen,y=GrowthRate)) + 
  geom_point() + 
  geom_smooth() +
  theme_minimal()


mod2 = lm(GrowthRate ~ Light, data = mushroom)

ggplot(mushroom, aes(x=Light,y=GrowthRate)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()

mod3 = lm(GrowthRate ~ Temperature, data = mushroom)

ggplot(mushroom, aes(x=Temperature,y=GrowthRate)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()

mod4 = lm(GrowthRate ~ Humidity, data = mushroom)

ggplot(mushroom, aes(x=Humidity,y=GrowthRate)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()

mod5 = lm(GrowthRate ~ Species, data = mushroom)

ggplot(mushroom, aes(x=Species,y=GrowthRate)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()

ggpairs (mushroom)

ggplot (mushroom, aes(y=GrowthRate, x=Light, color=Species)) +
  geom_point() +
  geom_smooth(method="lm")+
  facet_wrap(~Humidity)

#defines at least 2 models that explain the dependent variable “GrowthRate”#### 

mod6 = lm(GrowthRate ~ Species+Light+Humidity+Temperature+Nitrogen, data = mushroom)

mod7 = lm(GrowthRate ~ Species+Light+Humidity, data = mushroom)

mod8 = lm(GrowthRate ~ Light+Humidity+Temperature+Nitrogen, data = mushroom)

mod9 = aov(GrowthRate ~ Species+Humidity, data = mushroom)

mod10 = aov(GrowthRate ~ Species+Light+Humidity+Temperature+Nitrogen, data = mushroom)

mod11 = aov(GrowthRate ~ Light+Humidity+Temperature+Nitrogen, data = mushroom)

mod12 = lm(GrowthRate ~ Species*Light*Humidity*Temperature*Nitrogen, data = mushroom)

mod13 = lm(GrowthRate ~ Species*Light*Humidity+Temperature+Nitrogen, data = mushroom)

mod14 = lm(GrowthRate ~ Species*Light*Humidity, data = mushroom)

mod15 = lm(GrowthRate ~ Species*Light*Humidity:Temperature:Nitrogen, data = mushroom)

  #stepAIC to find the best model
step <- stepAIC(mod12)
mod12#widdles down your variables to the best model possible

step$call #gives the model that was decidedly the best

mod16 <- glm(data=df,
            formula = step$call)

#calculates the mean sq. error of each model####
mean(mod6$residuals^2)

mean(mod7$residuals^2)

mean(mod8$residuals^2)

mean(mod9$residuals^2)

mean(mod10$residuals^2)

mean(mod11$residuals^2)

mean(mod12$residuals^2)

mean(mod13$residuals^2)

mean(mod14$residuals^2)

mean(mod15$residuals^2)

mean(mod16$residuals^2)


#selects the best model you tried ####
    ##The best model was the mod12 that took into account ALL of 
    ##the different variables, although the mod13 removed 
    ##Temperature and Nitrogen and still did pretty good
    ##the mod16 was the stepAIC model, it is was really close to the mod12 value
    ##so it was also good


#adds predictions based on new hypothetical values for the independent variables used in your model ####
#hypothetical data

hypo <- read.csv("./hypothetical_data.csv")

predictions <- add_predictions(hypo, mod12)


#plots these predictions alongside the real data ####

toge <- full_join(mushroom,predictions, by=c("Species",'Light','Temperature','Humidity','Nitrogen'))


#just the original mushroom data and the mod12 predictions
#Did different graphs to compare the data more

pred_mushroom <- add_predictions(mushroom, mod12)


ggplot (pred_mushroom, aes(x=Nitrogen)) +
  geom_point(aes(y=pred),color="Red") +
  geom_point(aes(y=GrowthRate),color="Black")+
  facet_wrap(~Species)
  
ggplot (pred_mushroom, aes(x=Light)) +
  geom_point(aes(y=pred),color="Red") +
  geom_point(aes(y=GrowthRate),color="Black")+
  facet_wrap(~Temperature)

ggplot (pred_mushroom, aes(x=Humidity)) +
  geom_point(aes(y=pred),color="Red") +
  geom_point(aes(y=GrowthRate),color="Black")+
  facet_wrap(~Light)

ggplot (pred_mushroom, aes(x=Light)) +
  geom_point(aes(y=pred),color="Red") +
  geom_point(aes(y=GrowthRate),color="Black")+
  facet_wrap(~Humidity)

summary(mod16)
summary(mod12)
summary(predictions)




#model for linear model ### text questions ####
new <- read.csv("../../Data/non_linear_relationship.csv")

#option 1
ggplot (new, aes(y=response, x=predictor)) +
  geom_point() +
  geom_smooth(method="lm")

#option 2
ggplot (new, aes(y=response, x=predictor)) +
  geom_point() +
  geom_smooth()

#option 3
ggplot (new, aes(y=response, x=predictor)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3))

#option 4
ggplot (new, aes(y=response, x=predictor)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE))

