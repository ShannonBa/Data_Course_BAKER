############## Assignment 8 practice  ########################


#### Practice first: ####

library(modelr)
library(broom)
library(tidyverse)
library(fitdistrplus)

data("mtcars")
glimpse(mtcars)

mod1 = lm(mpg ~ disp, data = mtcars)
summary(mod1)

ggplot(mtcars, aes(x=disp,y=mpg)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()

mod2 = lm(mpg ~ qsec, data = mtcars)
ggplot(mtcars, aes(x=disp,y=qsec)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()

#which is the better model? compare mean-squared-errors. The smaller the better
mean(mod1$residuals^2)

mean(mod2$residuals^2) # much larger mean-squared-error, so not a great model to use

df <- mtcars %>% 
  add_predictions(mod1) 
df[,c("mpg","pred")] %>% head()


# Make a new dataframe with the predictor values we want to assess
# mod1 only has "disp" as a predictor so that's what we want to add here
newdf = data.frame(disp = c(500,600,700,800,900)) # anything specified in the model needs to be here with exact matching column names

# making predictions
pred = predict(mod1, newdata = newdf)

# combining hypothetical input data with hypothetical predictions into one new data frame
hyp_preds <- data.frame(disp = newdf$disp,
                        pred = pred)

# Add new column showing whether a data point is real or hypothetical
df$PredictionType <- "Real"
hyp_preds$PredictionType <- "Hypothetical"

# joining our real data and hypothetical data (with model predictions)
fullpreds <- full_join(df,hyp_preds)


# plot those predictions on our original graph
ggplot(fullpreds,aes(x=disp,y=pred,color=PredictionType)) +
  geom_point() +
  geom_point(aes(y=mpg),color="Black") +
  theme_minimal()


