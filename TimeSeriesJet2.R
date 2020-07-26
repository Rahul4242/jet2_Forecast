setwd("C:/Users/rahul.namjoshi/Dropbox/DS/jet2")
library(data.table)
library(tsintermittent) 
library(forecast)
library(tstools)
library(randtests)
library(stats)
library(aTSA)
library("pracma")
library(ForeCA) #to Check forecastabiliy
library(mltools)

# Import file with demand pattern for all spare parts (In source file the part names  are appended with character A)

A<-read.csv("J2F.csv")     
c<-A
d<- transpose(c)
e<-(d[-1,])
e[is.na(e)] <- 0
h<-as.numeric(e)

# Calculate Entropy for all parts -- Start
#  A lower Entrop means the series is not fit to forecast using conventional time series method, threshold can be put as 40%
Entropy<-c()
for (i in names(e)){
  skip_to_next <- FALSE
  g<-as.numeric(e[,i])
  #g<-diff(g)
  tryCatch(Entropy<-c(Entropy,Omega(g)), error = function(e) { skip_to_next <<- TRUE})
  if(skip_to_next) 
  { 
    #return("error")
    Entropy<-c(Entropy,0)
    next } 
}
# Calculate Entropy for all parts -- End ################33

# Calculate percentage of zero demand  in last 50 weeks -- Start
##Assumption: In case the percentge of zeros  last 50 weeks is 100% thn it might mean that the product is no longer in demand

e1<-e[157:206,]

nonZero<-c()
for (i in names(e)){
  g<-as.numeric(e1[,i])
  nonZero<-c(nonZero,(sum(g>0)/length(g)))
}
View(nonZero)
 
############ Calculate percentage of zeros in last 50 week ---Done

############Calculate CV2  for all parts -- Start
### Used to check for demnd patters, A CV2>0.49 means that the demand is either Lumpy or erratic

 CV2<-c()
 for (i in names(e)){
   g<-as.numeric(e[,i])
   X<-sd(e[,i])/mean(as.numeric(e[,i]))
   CV2<-c(CV2,(X*X))
 }

 

###########3 #Calculate CV2 for all parts -- End

#### Create Data for forecast with calculated parameters 
 
 csv<- rbind(h,Entropy,nonZero)

 csv2<-transpose(csv)
 View(csv2)
 names(csv2)[207]<- "Entropy"
 names(csv2)[208]<- "non-zero in last 50"


### Forecasting Starts 
 ### The demand pattern in EDA doesnt show smooth pattern thus the below logic is used 
 # If Last 50 weeks have zero sales the forecast value is put as zero (96 Spare parts out of 337 )
 # If Last 50 weeks have zero sales for 90% (40) weeks then Naïveforecast is used by putting mean of last 50 weeks (90 Spare parts out of 337)
 # For remaining parts (151 out of 337 parts) entropy of the demand is calculated which is  less than 60% for all. Also the squared coefficient of variation (CV2 ) is greater than 0.49 which makes the demand pattern as erratic or Lumpy. Thus the Croston forecasting is used for these parts.

## Split into test and train (last 48 weeks of 2019 as test) to check accuracy

Train <- h[1:(nrow(h)-48),] 
Test<-h[(nrow(h)-47):nrow(h),]
View(Test)
nrow(Test)
Predicted <- data.frame(matrix(ncol=ncol(Test), nrow=nrow(Test)))
colnames(Predicted)<-colnames(Test) 
Forecasted <- data.frame(matrix(ncol=ncol(h), nrow=12))
colnames(Forecasted)<-colnames(h)
#View(Predicted)
#### Forecast on Test Data based on EDA and calculation of Accuracy


for (i in 1:length(names(Train)))
{
  if(nonZero[i]<0.001)
    
  {
    Predicted[,i]<-0 # forecast as zero
    print("forecast of zero is used")
  }  
  
  else if(nonZero[i]<0.15 & nonZero[i]>0.001)
    {
    
    Predicted[,i]<-mean(as.numeric(Train[98:158,i]))# forecast as mean of last 50
    print("constant mean of last 50 weeks is used")
    }
   
  else
  {
    H1<-as.numeric(Train[,i]) 
    print(i)
    tryCatch({
              t<-crost(H1,type='croston',h=48,outplot = TRUE)
              Predicted[,i]<-t$frc.out
             
              },
             error=function(error_message) {
               message("The Train period alone is not suitable thus use complete period for forecast")
               message(error_message)
               Predicted[,i]<-NA
               return(NA)
             }
             
             )
     
     
     
  }
}


################## check Accuracy for all parts for Test Data

Acc <- data.frame(matrix(ncol=ncol(Test), nrow=1))
colnames(Acc)<-colnames(Test)

for (i in 1:length(names(Test)))
{
  #x4<-sum((as.numeric(Predicted[,i])- as.numeric(Test[,i]))^2)/48    
  Acc[i]<-100*(sum(abs((as.numeric(Predicted[,i])-as.numeric(Test[,i]))/as.numeric(Test[,i]))))/48
  
  
  #print(Acc[i])
}

is.na(Acc)<-sapply(Acc, is.infinite)
Acc[is.na(Acc)]<-100

View(Acc)
##################Accuracy on Test --End #####################3

##### Forecast on complete data for 12 week

for (i in 1:length(names(h)))
{
  if(nonZero[i]<0.001)
    
  {
    Forecasted[,i]<-0 # forecast as zero
  }  
  
  else if(nonZero[i]<0.15 & nonZero[i]>0.001)
  {
    
    Forecasted[,i]<-mean(as.numeric(h[(nrow(h)-50:nrow(h)),i]))# forecast as mean of last 50
  }
  
  else
  {
    H1<-as.numeric(h[,i]) 
    print(i)
    tryCatch({
      t<-crost(H1,type='croston',h=12,outplot = TRUE)
      Forecasted[,i]<-t$frc.out
      
    },
    error=function(error_message) {
      message("Cannot Forecast Check the part data for manual intervention")
      message(error_message)
      Forecasted[,i]<-NA
      return(NA)
    }
    
    )
    
    
    
  }
}



#### Forecast for 12 Weeks based on complete data --- END

############## Export the complete output in CSV format

csv3<- rbind(h,Acc,Forecasted,Entropy,nonZero)

csv_final<-transpose(csv3)
View(csv_final)
names(csv_final)[207]<- "Accuracy"
names(csv_final)[208:219]<- "Forecast"
names(csv_final)[220]<- "Entropy"
names(csv_final)[221]<- "non-zero in last 50"
write.csv(csv_final,"final_J2Forecast.csv")



