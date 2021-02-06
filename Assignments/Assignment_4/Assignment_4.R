################Practice:########################
?read.table() #This brings up the help file
df <-  
read.csv(file="../../../Data/landdata-states.csv") # why did I change to read.csv ???
class(df) # what type of object is df?
## [1] "data.frame"
head(df) # shows the first 6 elements of an object (first 6 rows if you give it a data frame)
class(df$State)
as.numberic(df$State)
state_factor <- as.factor(df$State)
as.character(df$State)
class(state_factor)
as.factor(df$State)
dim(df)# dimensions of the data frame (rows, columns)
str(df)# another nice way to glimpse a data frame
summary(df) # summary() works differently for different data classes. Note how it summarizes factors vs numerics
summary(df$Home.Value)
names(df)[4] 
names(df[4] )
hist(df$Land.Value) # histogram showing number of times each numeric value was seen in the vector "Land.Value"
# If you want to look at land value by region, you could do this:
plot(x=df$region,y=df$Land.Value)
# Land value by year
plot(x=df$Year,y=df$Land.Value)
plot(x=df$Year,y=df$Land.Value,
     col=df$region)
plot(x=df$Year,y=df$Land.Value,col=df$region)


##########Assignment 4##################
ITS_mapping <- read.csv(file="../../../Data/ITS_mapping.csv", header = TRUE, sep = "")
dim(ITS_mapping)# dimensions of the data frame (rows, columns)
str(ITS_mapping)# another nice way to glimpse a data frame
summary(ITS_mapping) # summary() works differently for different data classes. Note how it summarizes factors vs numerics
names(ITS_mapping[1])
summary.Date(ITS_mapping)
dimnames(ITS_mapping)

#boxplot
as.character(ITS_mapping$Ecosystem)
Eco <- as.factor(ITS_mapping$Ecosystem)
class(Eco)
Latf <- as.factor(ITS_mapping$Lat)
class(Latf)
plot(x=ITS_mapping$Ecosystem,y=ITS_mapping$Lat)
plot(x=Eco,y=Latf)
class(ITS_mapping$Ecosystem)
as.numeric(ITS_mapping$Ecosystem)
class(ITS_mapping$Lat)
?boxplot()
boxplot(x=ITS_mapping$Ecosystem, range = ITS_mapping$Lat)
boxplot(ITS_mapping$Ecosystem~ITS_mapping$Lat)
