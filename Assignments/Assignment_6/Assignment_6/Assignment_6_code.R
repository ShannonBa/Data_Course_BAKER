data (mpg)
mpg <- mpg

library(tidyverse)

ggplot(mpg, aes(x=displ, y=hwy,color = factor(cyl)) )+
  geom_point(alpha=.25)+
  geom_smooth(method="lm")+
  facet_wrap (~drv)

names(mpg)

# used ' factor(cyl)' to get rid of gradient

ggplot(mpg, aes(x=displ, y=cty, color = factor(year)) )+
  geom_point(alpha=.25)+
  geom_smooth(method="lm")+
  theme_minimal()+
  #facet_wrap(~year)+
  labs(y= "mpg in the city", x="displacement of chamber in engine", 
       legend= "year",caption="decreasing mpg over larger displacement, regardless of vehicle year" )

#2.subsets the mtcars dataframe to include only automatic transmissions
#3.saves this new subset as a new file called “automatic_mtcars.csv” in your Assignment_6 directory

data("mtcars")
str(mtcars)
?mtcars
automatic_subset <- mtcars[mtcars$am == 0,]

write.csv(automatic_subset, "automatic_mtcars.csv")


#4.plots the effect of horsepower on miles-per-gallon using ggplot2 (update plot to have meaningful labels and title)
#5.saves this plot as a png image called “mpg_vs_hp_auto.png” in your Assignment_6 directory

ggplot (automatic_subset, aes(x=hp, y=mpg))+
  geom_point()+
  geom_smooth()+
  labs(title="The Effects of Horsepower on miles-per-gallon",
       y= "miles-per-gallon",
       x= "Horsepower")
ggsave("mpg_vs_hp_auto.png")

#6.plots the effect of weight on miles-per-gallon (with improved labels, again)
#7.saves this second plot as a tiff image called “mpg_vs_wt_auto.tiff” in your Assignment_6 directory

ggplot (automatic_subset, aes(x=wt, y=mpg))+
  geom_point()+
  geom_smooth()+
  labs(title="The effect of Weight on miles-per-gallon",
       y="miles-per-gallon",
       x="weight")
ggsave("mpg_vs_wt_auto.tiff")


#8.subsets the original mtcars dataframe to include only cars with displacements less than or equal to 200 cu.in.
#9.saves that new subset as a csv file called mtcars_max200_displ.csv


mtcars_max200_displ_subset <- mtcars[mtcars$disp<=200 ,]
write.csv(mtcars_max200_displ_subset, "mtcars_max200_displ.csv")

#10.includes code to calculate the maximum horsepower for each of the three dataframes 
  #(original, automatic, max200)
max(mtcars$hp)
max(automatic_subset$hp)
max(mtcars_max200_displ_subset$hp)

#11. prints these calculations (from task 10) in a readable format to a new plaintext 
  #file called hp_maximums.txt
hp_maximums.txt <- c(max(mtcars$hp), max(automatic_subset$hp) , max(mtcars_max200_displ_subset$hp))
write.table(hp_maximums.txt,"hp_maximums.txt")

#combines the following 3 plots into one image using the patchwork package 
  #(all 3 plots use the full un-subsetted mtcars data)
    #Scatterplot + trendline of the effect of weight on mpg 
      #(points and linear trendlines colored by the number of cylinders)
    #Violin chart of the distributions of mpg for cars, separated and colored by the number of cylinders
    #Scatterplot + trendline of the effect of horsepower on mpg 
      #(points and linear trendlines colored by the number of cylinders)
library(ggplot2)
library(patchwork)

#Scatterplot + trendline of the effect of weight on mpg 
#(points and linear trendlines colored by the number of cylinders)

p1 <- ggplot (mtcars, aes(x=wt, y=mpg ))+
  geom_point(aes(col=factor(cyl)))+
  geom_smooth(method="lm", aes(color=factor(cyl))) +
  labs(title="The effect of Weight on miles-per-gallon",
       y="miles-per-gallon",
       x="weight")

#Violin chart of the distributions of mpg for cars, separated and colored by the number of cylinders
p2 <- ggplot(mtcars, aes( y=mpg, x=factor(cyl)))+
  geom_violin(aes(fill=factor(cyl)))+
  labs(title="The distributions of mpg for cars",
       y="miles-per-gallon",
       x="car cylinders")
names(mtcars)

#Scatterplot + trendline of the effect of horsepower on mpg 
#(points and linear trendlines colored by the number of cylinders)
p3 <- ggplot (mtcars, aes(x=hp, y=mpg ))+
  geom_point(aes(col=factor(cyl)))+
  geom_smooth(method="lm", aes(color=factor(cyl))) +
  labs(title="The effect of horsepower on mpg",
       y="miles-per-gallon",
       x="horsepower")

#all together
p1 + p2 + p3

#13.saves that combined figure as a single png image file called 
  #combined_mtcars_plot.png in your Assignment_6 directory
p1 + p2 + p3
ggsave("combined_mtcars_plot.png")


