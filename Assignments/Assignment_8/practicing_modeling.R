library(tidyverse)
library(modelr)


# Some simulated data
sim1
ggplot(sim1,aes(x,y)) + geom_point() +# x is numeric
  geom_smooth(se=FALSE, method="lm")
#behind the scenes, r calculated a regression line
#y=mx+b
#'m' is the slope, x is the independent variable, and 'b' is the y-intercept

sim2
ggplot(sim2,aes(x,y)) + geom_point() +# x is categorical
  geom_smooth(method="glm") #no slope because this data is categorical
#r does not know what to do with categorical data

sim2 %>%
  group_by(x) %>%
  summarize(N=n(),
            meanY = mean(y))
#because it is categorical, the mean is the best way to look at this data


sim3
ggplot(sim3,aes(x1,y,color=x2)) + geom_point() # categorical AND numeric predictors
#dependent variable=y, independent variable=rep, x1, and x2

#model accounts for the affect of x1 on y within the category of x2





#Fit a linear model for each
mod1 <- lm(data = sim1,formula=y~x)

mod2 <- lm(data = sim2, formula= y~x)

mod3a <- lm(data = sim3,formula = y~x1+x2) # + sign, y-in can vary; slope is the same
mod3b <- lm(data = sim3,formula = y~x1*x2) # * sign, y-int AND slope can VARY


#which model is better fit for these data? stat test, measure residuals



#fake data
species <- c(rep("Human",10),rep("Chimpanzee",10),rep("Dog",10),rep("Goldfish",10))
sex <- rep(c(rep("Male",5),rep("Female",5)),4)
sp_mass <- c(rnorm(5,69,15),rnorm(5,63,15),
             rnorm(5,60,7),rnorm(5,58,5),
             rnorm(5,10,9),rnorm(5,8,3),
             rnorm(5,0.010,0.005),rnorm(5,0.015,0.002))

fake_data <- data.frame(species,sex,sp_mass)

ggplot(fake_data,aes(x=species,y=sp_mass)) +
  geom_boxplot(alpha=.25) +
  geom_point()

mod4 <- lm(data = fake_data, formula = sp_mass~species*sex)
#mod3a <- lm(data = sim3,formula = y~x1+x2)

#summary of mod4
summary(mod4) #alphabetically, 'f' comes before 'm', so the data for is based off of the female data

add_predictions(fake_data,mod4)
