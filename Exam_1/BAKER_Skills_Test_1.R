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


class(means_combined)
means_combined <- as.data.frame(means_combined)


#min is when Ben did the worst relative to Katy; found by dividing Ben's values by Katy's values
min(means_combined$Ben_mean / means_combined$Katy_mean)
means_combined[which((means_combined$Ben_mean / means_combined$Katy_mean) == min(means_combined$Ben_mean / means_combined$Katy_mean)), ]

print("it seems that in year 2010, Ben did his worst compared to Katy, but he did his max DNA extraction in 2007")


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


