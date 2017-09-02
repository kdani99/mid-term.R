#mydata1<-read.csv("train_2016_v2.csv")
#mydata1
#mydata2<-read.csv("properties_2016.csv")
#mydata2
#mydata<-merge(mydata1,mydata2,by="parcelid",all= FALSE)#merge two datasets 2016; mydata only contains 2016 observations
#summary(mydata)
#######generate the 2016 dataset
#write.table(mydata, "/Users/andy/desktop/data1.txt", sep="\t")

setwd("C:/Users/LIU/Desktop/Bootcamp");getwd()
mydata2016<-read.table("data2016.txt",header=TRUE)
######deleting variables whose missing values are bigger than 50%.
mydata2016$airconditioningtypeid <- NULL
mydata2016$architecturalstyletypeid <- NULL
mydata2016$basementsqft <- NULL
mydata2016$buildingclasstypeid <- NULL
mydata2016$decktypeid <- NULL
mydata2016$threequarterbathnbr<- NULL
mydata2016$finishedfloor1squarefeet <- NULL
mydata2016$finishedsquarefeet6 <- NULL
mydata2016$finishedsquarefeet13 <- NULL
mydata2016$finishedsquarefeet15 <- NULL
mydata2016$finishedsquarefeet50 <- NULL
mydata2016$fireplacecnt <- NULL
mydata2016$fips <- NULL
mydata2016$garagecarcnt <- NULL
mydata2016$garagetotalsqft <- NULL
mydata2016$hashottuborspa <- NULL
mydata2016$latitude <- NULL
mydata2016$longitude <- NULL
mydata2016$numberofstories <- NULL
mydata2016$poolcnt <- NULL
mydata2016$poolsizesum <- NULL
mydata2016$pooltypeid10 <- NULL
mydata2016$pooltypeid2 <- NULL
mydata2016$pooltypeid7 <- NULL
mydata2016$rawcensustractandblock <- NULL
mydata2016$censustractandblock<- NULL
mydata2016$regionidcounty <- NULL
mydata2016$regionidzip <- NULL
mydata2016$regionidneighborhood <- NULL
mydata2016$storytypeid <- NULL
mydata2016$typeconstructiontypeid<- NULL
mydata2016$yardbuildingsqft17 <- NULL
mydata2016$yardbuildingsqft26 <- NULL
mydata2016$assessmentyear <- NULL
mydata2016$taxdelinquencyyear <- NULL
###take out a few variables for the for loop 
mydata2016$transactiondate <- NULL
mydata2016$parcelid <- NULL
mydata2016$fireplaceflag <- NULL

####deleting all observations with missing values
mydata2016_noNA <- na.omit(mydata2016)
mydata2016_noNA
dim(mydata2016_noNA)
summary(mydata2016_noNA)
attach(mydata2016_noNA)
######tease out outliers:Q3+1.5IQR
lapply(mydata2016_noNA,class)###find classes for all columns

ThresholdforOutlier <- rep(0,24)
order=c(1:10,12,14:21) #exclude the factor variables

for (i in order){
  t1 <- quantile(mydata2016_noNA[,i], 0.75)
  t2 <- IQR(mydata2016_noNA[,i], 0.75)
  ThresholdforOutlier[i] <- t1 + 1.5*t2
}

for (i in 1:50652)
  for (j in order){
    if (mydata2016_noNA[i,j] > ThresholdforOutlier[j]) mydata2016_noNA[i,j] <- NA
  }

summary(mydata2016_noNA)
####mydata contains clean data with all 22 variables and 32575 observations
mydata <- na.omit(mydata2016_noNA)
dim(mydata)

### delete 
mydata$propertycountylandusecode<- NULL
mydata$propertyzoningdesc<- NULL

###build a new variable:age
mydata$age = 2016-mydata$yearbuilt##age= 2016-year when it built
mydata$yearbuilt<- NULL
summary(mydata)
###turn factor variable taxdelinquencyflag into numberic variable: 0 means No, 1 means YES.
mydata$taxdelinquencyflag_new <- as.numeric(mydata$taxdelinquencyflag)#turn factor variable into numeric variable
mydata$taxdelinquencyflag_newDUMMY = mydata$taxdelinquencyflag_new-1##create a 0/1 dummy variable for taxdelinquencyflag
mydata$taxdelinquencyflagY<- NULL
mydata$taxdelinquencyflag_new<- NULL
mydata$taxdelinquencyflag<- NULL
######


####create a training and test data
data.train = mydata[sample(nrow(mydata),22803),]
data.test = mydata[-sample(nrow(mydata),22803),]
####generate train and test datasets
write.table(data.train, "C:/Users/LIU/Desktop/Bootcamp/data.train.txt", sep="\t")
write.table(data.test, "C:/Users/LIU/Desktop/Bootcamp/data.test.txt", sep="\t")
# double check
summary(data.train)
dim(data.train)
summary(data.test)
dim(data.test)

#mse
mse <- function(sm) 
  mean(sm$residuals^2)


#### linear regression
lm0<-lm(logerror~.,data=data.train)
summary(lm0)

mselm0 <- mse(lm0)
mselm0

cor(roomcnt,unitcnt,method = c("pearson"))
cor(roomcnt,bedroomcnt,method = c("pearson"))
cor(unitcnt,bedroomcnt,method = c("pearson"))

#### improve the linear regression based on p-value and correlation coefficient test
lm1<-lm(logerror ~ bedroomcnt+calculatedfinishedsquarefeet+regionidcity+structuretaxvaluedollarcnt+taxvaluedollarcnt+taxamount+age,data=data.train)
summary(lm1)

mselm1 <- mse(lm1)
mselm1


#### polynomial regression

# find the optimal polyn within 3-order on each feature
# the features are selected based on the linear regression

porder=3;
msepm <- array(0,dim=c(porder,porder,porder,porder,porder,porder,porder))

for(i1 in 1:porder){
  for (i2 in 1:porder){
    for(i3 in 1:porder){
      for (i4 in 1:porder){
        for (i5 in 1:porder){
          for (i6 in 1:porder){
            for (i7 in 1:porder){
              temp.fit=lm(logerror ~ poly(bedroomcnt,i1)+poly(calculatedfinishedsquarefeet,i2)+poly(regionidcity,i3)+poly(structuretaxvaluedollarcnt,i4)+poly(taxvaluedollarcnt,i5)+poly(taxamount,i6)+poly(age,i7), data = data.train);
              msepm[i1,i2,i3,i4,i5,i6,i7] <- mse(temp.fit)
            }
          }
        }
      }
    }
  }
}

#find the optimal tuning parameters
which(msepm == min(msepm), arr.ind = TRUE)

# to get the optimal polyn model format, mannually tune the polyn model based on the p-value
pm.fit=lm(logerror ~ I(bedroomcnt^1)+I(bedroomcnt^3)+poly(calculatedfinishedsquarefeet,1)+poly(regionidcity,2)+I(structuretaxvaluedollarcnt^1)+I(structuretaxvaluedollarcnt^3)+poly(taxvaluedollarcnt,3)+poly(taxamount,3)+poly(age,2), data = data.train);

summary(pm.fit)
msepm=mse(pm.fit)

print(pm.fit)

# predict
preds1=predict(pm.fit,newdata=data.test,type="response") 





