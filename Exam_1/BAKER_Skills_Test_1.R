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
Katy <- mean(csv$DNA_Concentration_Katy)
Ben <- mean(csv$DNA_Concentration_Ben)
Ben>Katy
K_B <- c(Katy,Ben)
plot(x=K_B)

#Your task here is to write some code that tells us: in which 
#extraction YEAR, was Ben's performance the
#lowest RELATIVE TO Katy's performance?



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

Just_Ben<- csv[Date_Collected,]




