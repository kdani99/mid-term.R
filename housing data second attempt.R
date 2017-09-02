#mydata1<-read.csv("train_2016_v2.csv")
#mydata1
#mydata2<-read.csv("properties_2016.csv")
#mydata2
#mydata<-merge(mydata1,mydata2,by="parcelid",all= FALSE)#merge two datasets 2016; mydata only contains 2016 observations
#summary(mydata)
#######generate the 2016 dataset
#write.table(mydata, "/Users/andy/desktop/data1.txt", sep="\t")

setwd("/Users/andy/desktop");getwd()
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
######create a training and test data
data.train = mydata[sample(nrow(mydata),22803),]
data.test = mydata[-sample(nrow(mydata),22803),]
####generate train and test datasets
write.table(data.train, "/Users/andy/desktop/data.train.txt", sep="\t")
write.table(data.test, "/Users/andy/desktop/data.test.txt", sep="\t")
# double check
summary(data.train)
dim(data.train)
summary(data.test)
dim(data.test)

#######
#######A Linear Regression Model
#######


####Check collinearity among variables. 
#landtaxvaluedollarcnt & Other variables
cor(landtaxvaluedollarcnt,calculatedbathnbr)#0.3632316
cor(landtaxvaluedollarcnt,finishedsquarefeet12)#0.4497032
cor(landtaxvaluedollarcnt,fullbathcnt)#0.3632316
####unitcnt & Other variables: no high correlation
####roomcnt & Other variables: no high correlation
####fullbathcnt & Other variables.
cor(fullbathcnt,finishedsquarefeet12)###0.7862645; take out finishedsquarefeet12
cor(fullbathcnt,calculatedbathnbr)###1: only keep one variable fullbathcnt
####finishedsquarefeet12 & calculatedbathnbr 
cor(finishedsquarefeet12,calculatedbathnbr)#0.7862645
#######build a linear regression model based on test dataset
fit<-lm(logerror~.-calculatedbathnbr-finishedsquarefeet12-landtaxvaluedollarcnt-fullbathcnt-roomcnt-unitcnt,data=data.train)
fitfinal<-step(fit,direction="both")#use forward and backward selections to get our final model.
summary(fitfinal)
############
############Decision Tree
############
library(rpart)
##grow tree 
fitDT<-rpart(logerror~bedroomcnt+buildingqualitytypeid+calculatedfinishedsquarefeet+regionidcity+structuretaxvaluedollarcnt+taxvaluedollarcnt+taxvaluedollarcnt+taxamount+age+taxdelinquencyflag_newDUMMY,method="anova",na.action = na.rpart,data=data.train)
summary(fitDT)
######
printcp(fitDT)#display the results
plotcp(fitDT)#visualize cross-validation results
summary(fitDT)#detailed summary of splits
par(mfrow=c(1,2))#two plots on one page
rsq.rpart(fitDT)
plot(fitDT,uniform=TRUE,main="Housing Price for 2017");text(fitDT,use.n=TRUE, all=TRUE,cex=0.6)##fitted model is just a root, not a tree.
############
############Random Forest
############
setwd("/Users/andy/desktop");getwd()
data.train<-read.table("data.train",header=TRUE)
install.packages("randomForest")
library(randomForest)
fitRF<-randomForest(logerror~bedroomcnt+buildingqualitytypeid+calculatedfinishedsquarefeet+regionidcity+structuretaxvaluedollarcnt+taxvaluedollarcnt+taxvaluedollarcnt+taxamount,method="anova",data=data.train)
print(fitRF)
importance(fitRF)
