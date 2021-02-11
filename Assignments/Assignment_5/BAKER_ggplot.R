####################BAKER_ggplot.R      Assignment 5#######################
iris<- iris

#1
png(filename = "./iris_fig1.png")
ggplot(iris, aes(x=Sepal.Length, y=Petal.Length)) + geom_point()  +
  geom_point(aes(color=Species),size=2) + 
  geom_smooth(method="lm", aes(color=Species)) +
  labs(title="Sepal length vs petal length", subtitle="for three iris species",
       y="Petal.Length", x="Sepal.Length" ) +
  theme_minimal()
dev.off()

#2
png(filename = "./iris_fig2.png")
ggplot(iris, aes(x=Petal.Width, fill=Species))  +
  geom_density(alpha = .5) +
  labs(title="Distribution of Petal Width", subtitle="for three iris species",
       y="density", x="Petal Width" ) +
  theme_minimal()
dev.off()

#3
png(filename = "./iris_fig3.png")
new_y <- (iris$Petal.Width/iris$Sepal.Width)
ggplot(iris, aes(x=Species, y=new_y,fill=Species))  +
  geom_boxplot()+
  labs(title="Sepal- to Petal-Width Ratio", subtitle="for three iris species",
       y="Ratio of Sepal Width to Petal Width", x="Species" ) +
  theme_minimal()
dev.off()

#4
mean <- mean(iris$Sepal.Length)
deviance <- (iris$Sepal.Length-mean)
class(deviance)

#png(filename = "./iris_fig4.png")
#ggplot(iris, aes( x=order((deviance)), fill=Species))  +
#  geom_histogram(bins=99)+
#  labs(title="Sepal length deviance from the mean of all observations",
#       x="Deviance from the Mean", y=NULL, caption="Note: Deviance = Sepal.Length - mean(Sepal.Length)" ) +
#  theme_minimal()+
#  coord_flip()
#dev.off()

iris <- iris
iris2 <- iris[order(iris$Sepal.Length-mean(iris$Sepal.Length)),]


png(filename = "./iris_fig4.png")
ggplot(iris2, aes(x=1:150, y=Sepal.Length-mean(Sepal.Length), fill=Species))+
  geom_bar(stat="identity") +
  coord_flip()+
  theme_minimal()+
  labs(title="Sepal length deviance from the mean of all observations",
       y="Deviance from the Mean", x="", y=NULL, caption="Note: Deviance = Sepal.Length - mean(Sepal.Length)" )
dev.off()




