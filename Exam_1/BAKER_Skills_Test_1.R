###########Exam 1############

#I.
library(readr)
library(tidyverse)
csv <- read.csv("./DNA_Conc_by_Extraction_Date.csv")

#Katy's DNA concentration histogram
ggplot ( csv, aes(x= DNA_Concentration_Katy))+
  geom_histogram() +
  labs(title= "Katy's DNA concentration", y="DNA concentration in ng/uL", x="Katy's DNA concentration" )


png(filename = "./Katys_DNA_concentration_histogram.png")
ggplot ( csv, aes(x= DNA_Concentration_Katy))+
  geom_histogram() +
  labs(title= "Katy's DNA concentration", y="DNA concentration in ng/uL", x="Katy's DNA concentration" )
dev.off()


#Ben's DNA concentration histogram
ggplot ( csv, aes(x= DNA_Concentration_Ben))+
  geom_histogram() +
  labs(title= "Ben's DNA concentration", y="DNA concentration in ng/uL", x="Ben's DNA concentration" )


png(filename = "./Bens_DNA_concentration_histogram.png")
ggplot ( csv, aes(x= DNA_Concentration_Ben))+
  geom_histogram() +
  labs(title= "Ben's DNA concentration", y="DNA concentration in ng/uL", x="Ben's DNA concentration" )
dev.off()

#II. and III.


class(csv$DNA_Concentration_Katy)
year <- as.factor(csv$Year_Collected)

plot(x=year, y=csv$DNA_Concentration_Katy,
     main = "Katy's Extractions",
     xlab = "YEAR",
     ylab = "DNA Concentration",
     col=FALSE)

lot(x=year, y=csv$DNA_Concentration_Ben,
    main = "Ben's Extractions",
    xlab = "YEAR",
    ylab = "DNA Concentration",
    col=FALSE)



png(filename = "./BAKER_Plot1.jpeg")
plot(x=year, y=csv$DNA_Concentration_Katy,
     main = "Katy's Extractions",
     xlab = "YEAR",
     ylab = "DNA Concentration",
     col=FALSE)
dev.off()

png(filename = "./BAKER_Plot2.jpeg")
plot(x=year, y=csv$DNA_Concentration_Ben,
     main = "Ben's Extractions",
     xlab = "YEAR",
     ylab = "DNA Concentration",
     col=FALSE)
dev.off()

#IV.
#Your task here is to write some code that tells us: in which 
#extraction YEAR, was Ben's performance the
#lowest RELATIVE TO Katy's performance?

Katy <- mean(csv$DNA_Concentration_Katy)
Ben <- mean(csv$DNA_Concentration_Ben)
Ben>Katy
K_B <- c(Katy,Ben)
plot(x=K_B)

Ben_means <- aggregate(x= csv$DNA_Concentration_Ben,
          by= list(csv$Year_Collected),
          FUN=mean)

Katy_means <- aggregate(x= csv$DNA_Concentration_Katy,
          by= list(csv$Year_Collected),
          FUN=mean)

means_combined <- cbind(Ben_means$Group.1, Ben_means$x,Katy_means$x)
colnames(means_combined) <- c("Year","Ben_mean","Katy_mean")

ggplot(x=means_combined$Ben_means, y= means_combined$Katy_means)

plot(x=(means_combined$Ben_means & means_combined$Katy_means), y=means_combined$Year))
plot(y=Ben_means$x , x= Ben_means$Group.1)
plot(y=Katy_means$x , x= Katy_means$Group.1)

means$Ben_mean <- Ben_means
means$Katy_mean <- Katy_means
names(means)
ggplot(means, aes(y=Ben_mean & Katy_mean, x=TRUE))
ggplot(Katy_means $ Ben_means, aes(y=Ben_means$x & Katy_means$x, x= Ben_means$Group.1 &Katy_means$Group.1 ))

ggplot(NULL, aes(x=Ben_means$x,  y=  Katy_means$x, col=year))

ggplot (means_combined, aes(x= Ben_mean, y= Katy_mean, col=Year))+
          geom_point()

means_combined$Ben_mean / means_combined$Katy_mean

means_combined$difference <- means_combined$Ben_mean - means_combined$Katy_mean


class(means_combined$Ben_mean)

rlang::last_error()

#V.

class(csv$Date_Collected)
downstairs <- csv$Lab %in% "Downstairs" 
Ben_downstairs <- csv[downstairs,]
x <- as.POSIXct((Ben_downstairs$Date_Collected))

plot(x=x, y=Ben_downstairs$DNA_Concentration_Ben,
     xlab = "Date_Collected",
     ylab = "DNA_Concentration_Ben")

png(filename = "./Ben_DNA_over_time.jpg")
    plot(x=x, y=Ben_downstairs$DNA_Concentration_Ben,
         xlab = "Date_Collected",
         ylab = "DNA_Concentration_Ben")
dev.off()


#VI.

Ben_Average_Conc <- aggregate(x= csv$DNA_Concentration_Ben,
          by= list(csv$Year_Collected),
          FUN=mean)
colnames(Ben_Average_Conc) <- c("Year","Ben_means")

Ben_Average_Conc[which((Ben_Average_Conc$Ben_means) == max(Ben_Average_Conc$Ben_means)), ]

write.csv(Ben_Average_Conc, "Ben_Average_Conc.csv")


