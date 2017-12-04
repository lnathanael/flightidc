
library(ggplot2)
library(plotrix)

setwd("C:/Users/User/Documents/MBA/semester 1/Bussiness Data Analysics/data/flights")
df  = read.csv(file="DelayedFlights.csv",header=TRUE,as.is=TRUE,na.strings=c("NA",".",""))



set.seed(111)
bound <- floor((nrow(df)/4)*3)
d <- df[sample(nrow(df)), ]
df.train <- d[1:bound, ]
df.test <- d[(bound+1):nrow(df), ]


convert_time_to_min <-function(time_HH_MM){

 time_in_min <- floor(time_HH_MM/100)*60+ (time_HH_MM%%100)
 return(time_in_min)

}

df.train$DepTimeInMin    = convert_time_to_min(df.train$DepTime)
df.train$CRSDepTimeInMin = convert_time_to_min(df.train$CRSDepTime)
df.train$ArrTimeInMin    = convert_time_to_min(df.train$ArrTime)
df.train$CRSArrTimeInMin = convert_time_to_min(df.train$CRSArrTime)



(nrow(df.train ) - nrow(df.train [complete.cases(df.train ),]))/nrow(df.train )

df.train.complete  <- df.train [complete.cases(df.train ),]
sam.df.train.complete <- df.train.complete [sample(nrow(df.train.complete  ), 10000),]

ggplot(sam.df.train.complete, aes(x=CRSElapsedTime, y=ActualElapsedTime))+geom_point(alpha=0.3, colour="blue")+geom_abline(colour="red", intercept=0, slope=1)
ggplot(df.train , aes(x=CRSElapsedTime, y=ActualElapsedTime))+geom_point(alpha=0.3, colour="blue")+geom_abline(colour="red", intercept=0, slope=1)

ggplot(df.train.complete , aes(x=Month, y=ActualElapsedTime))+geom_point(alpha=0.3, colour="blue")
ggplot(sam.df.train.complete , aes(x=Month, y=WeatherDelay))+geom_point(alpha=0.3, colour="blue")

pairs(sam.df.train.complete[30:34])
cor(sam.df.train.complete[30:34])

#df.train[which((df.train$DepTimeInMin-df.train$CRSDepTimeInMin + 100)< 0),]$DepTimeInMin =df.train[which((df.train$DepTimeInMin-df.train$CRSDepTimeInMin+100)<0),]$DepTimeInMin +24*60
#df.train[which((df.train$ArrTimeInMin-df.train$CRSArrTimeInMin + 100)< 0),]$ArrTimeInMin =df.train[which((df.train$ArrTimeInMin-df.train$CRSArrTimeInMin+100)<0),]$ArrTimeInMin +24*60
+
  
summary(df.train$ArrDelay)
summary(df.train.complete$ArrDelay)
summary(df.train$ArrTimeInMin-df.train$CRSArrTimeInMin)

summary(df.train$DepTimeInMin-df.train$CRSDepTimeInMin)

hist (df.train.complete$ArrDelay)


hist(df.train$DepTimeInMin-df.train$CRSDepTimeInMin)
hist(df.train$ArrTimeInMin-df.train$CRSArrTimeInMin)

sum_cancelled= sum(df.train$Cancelled)
sum_Diverted = sum(df.train$Diverted)

num_of_NAS_only     = length(which(df.train.complete$NASDelay!=0 & df.train.complete$SecurityDelay==0 & df.train.complete$WeatherDelay==0 & df.train.complete$CarrierDelay ==0 & df.train.complete$LateAircraftDelay==0))
num_of_securit      = length(which(df.train.complete$NASDelay==0 & df.train.complete$SecurityDelay!=0 & df.train.complete$WeatherDelay==0 & df.train.complete$CarrierDelay ==0 & df.train.complete$LateAircraftDelay==0)) 
num_of_WeatherDelay = length(which(df.train.complete$NASDelay==0 & df.train.complete$SecurityDelay==0 & df.train.complete$WeatherDelay!=0 & df.train.complete$CarrierDelay ==0 & df.train.complete$LateAircraftDelay==0)) 
num_of_CarrierDelay = length(which(df.train.complete$NASDelay==0 & df.train.complete$SecurityDelay==0 & df.train.complete$WeatherDelay==0 & df.train.complete$CarrierDelay !=0 & df.train.complete$LateAircraftDelay==0)) 
num_of_LateAircraf  = length(which(df.train.complete$NASDelay==0 & df.train.complete$SecurityDelay==0 & df.train.complete$WeatherDelay==0 & df.train.complete$CarrierDelay ==0 & df.train.complete$LateAircraftDelay!=0)) 




num_of_else         = length(which((df.train.complete$NASDelay!=0 | df.train.complete$SecurityDelay!=0 | df.train.complete$WeatherDelay!=0 | df.train.complete$LateAircraftDelay!=0) & df.train.complete$CarrierDelay ==0 ))
num_of_Carrier      = length(which(df.train.complete$CarrierDelay !=0))

summary(df.train.complete[which(df.train.complete$CarrierDelay !=0),]$DepDelay)

summary(df.train.complete[which((df.train.complete$NASDelay!=0 | df.train.complete$SecurityDelay!=0 | df.train.complete$WeatherDelay!=0 | df.train.complete$LateAircraftDelay!=0) & df.train.complete$CarrierDelay ==0 ),]$DepDelay)

slices <- c(num_of_WeatherDelay,num_of_NAS_only,num_of_securit ,num_of_CarrierDelay,num_of_LateAircraf)
lbls   <- c("Weather","NAS_only","Security","Carrier","Late Air craft")

pie(slices, labels = lbls, main="Pie Chart delay")

#slices <- c(10, 12, 4, 16, 8) 
#lbls <- c("US", "UK", "Australia", "Germany", "France")
pie3D(slices,labels=lbls,explode=0.2,
      main="Pie Chart of Cancelled ")



lm()

non_char_list=which(lapply(df.train,typeof)!="character")

lapply(df.train[6:9], convert_time_to_min)

lapply(as.numeric(i), hist_attrition) 

summary(IndexesDF$Distance)


pairs(df.train[as.numeric(i)])

hist(as.numeric(as.factor(df.train$Origin)))
cor(df.train$Cancelled==1,as.numeric(as.factor(df.train$Origin)))

sum(IndexesDF$Cancelled[which(IndexesDF$Month==10)])
Cancelled=IndexesDF$Cancelled
Distance=IndexesDF$Distance
CvD=tapply(Cancelled, Distance, sum)

cor(Cancelled,Distance)
