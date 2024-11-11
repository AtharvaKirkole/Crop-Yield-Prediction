#Script file for Bethany, Carolina, Atharva
#"Modeling the Impact of Drought and Heat Stress on Maize Anthesis-Silking Interval"
#using publicly available Genomes to Fields data from diverse regions of the US
#genetic, phenomic, and environmental data

#set working directory
setwd("C:/Users/addie/Desktop/MSU/UGS202H/ALL_G2F_CURATED/")
#read in weather data -- THIS IS YOUR X VALUES
ecov<-read.csv("ECOV.csv")
#change column name
colnames(ecov)[1]<-"YearLocation"
#read in your phenotypic data -- part of this will be the Y VALUES
pheno<-read.csv("PHENO.csv")
#average to one ASI value per year x location
asi<-aggregate(pheno$ASI,by=list(pheno$year_loc),FUN="mean")
#change column names
colnames(asi)<-c("YearLocation","ASI")
#do same for anthesis and silking
anthesis<-aggregate(pheno$anthesis,by=list(pheno$year_loc),FUN="mean")
silking<-aggregate(pheno$silking,by=list(pheno$year_loc),FUN="mean")
#change column names
colnames(anthesis)<-c("YearLocation","Anthesis")
colnames(silking)<-c("YearLocation","Silking")

#SVM, RF, PLS, KNN
#think about classification vs regression

#look at data
hist(asi$ASI)
summary(asi$ASI)
hist(anthesis$Anthesis)
hist(silking$Silking)

# #if we do classification, need to set categories
# asi$class<-NA
# asi[which(asi$ASI<0),3]<-"low"
# asi[which(asi$ASI>=0 & asi$ASI<1.5),3]<-"mid"
# asi[which(asi$ASI>=1.5),3]<-"high"

#try different algorithms, both for class. and reg.
#goals: good model (accuracy and precision - either R2 and MSE or precision/recall, e.g.)
# suggested package caret: https://topepo.github.io/caret/available-models.html

#split train-test 4-fold CV
nobs<-136
ind_train<-factor(sample(x=rep(1:4,each=round(nobs/4)),size=nobs))
summary(ind_train)
df_list<-list(asi,anthesis,silking,ecov)
alldata<-Reduce(function(x, y) merge(x, y, all=TRUE), df_list)


#try one first
train_indices<-as.vector(which(ind_train!=4))
test_indices<-as.vector(which(ind_train==4))
alltrain<-alldata[c(train_indices),]
alltest<-alldata[c(test_indices),]

#Addie will set up a loop of Anthesis, Silking, ASI across 4xCV
#for predicting year_locs (136 total obs)
#then output accuracy and error for each fold and trait
#24 numbers to report for accuracy and error (3 traits, 4 folds, train/test)
#importance scores for 3 traits x 4 folds = 12 total

install.packages("randomForest")
library(randomForest)
alltrain.rf <- randomForest(y=alltrain$Anthesis, x=alltrain[,5:206],
                            xtest=alltest[,5:206],
                            ytest=alltest[,"Anthesis"],
                            importance = TRUE,
                            proximity = TRUE)
summary(alltrain.rf)
summary(alltrain.rf$predicted)
print(alltrain.rf)
alltrain.rf <- randomForest(y=alltrain$Silking, x=alltrain[,5:206],
                            xtest=alltest[,5:206],
                            ytest=alltest[,"Silking"],
                            importance = TRUE,
                            proximity = TRUE)
summary(alltrain.rf)
summary(alltrain.rf$predicted)
print(alltrain.rf)
plot(alltrain.rf$test$predicted,alltest[,4])
plot(alltrain.rf$predicted,alltrain[,4])

alltrain.rf$importance













#ideas
#try other algorithms
#compare predicting ASI to predicting Anthesis/Silking and getting interval
#maybe try Anthesis/Silking GDD


#put it in the loop
for (fold in c(1:4)){
  train_indices<-as.vector(which(ind_train!=fold))
  test_indices<-as.vector(which(ind_train==fold))
  alltrain<-alldata[c(train_indices),]
  alltest<-alldata[c(test_indices),]
  
}









#what impacts flowering time most?
#pick a subset
pheno20<-pheno[which(pheno$year==2020),]
pheno21<-pheno[which(pheno$year==2021),]
smallpheno<-rbind(pheno20,pheno21)

summary(lm(smallpheno$ASI~smallpheno$year_loc))
summary(lm(smallpheno$anthesis~smallpheno$year_loc))
summary(lm(smallpheno$silking~smallpheno$year_loc))
summary(lm(smallpheno$anthesis~smallpheno$year_loc+smallpheno$genotype))

#rf model
#use ecov and genotype info to predict
#withold year_loc so we are testing predictability of unobs. environ.
#there are 35 year_loc in 2020-2021 dataset
#use leave one out (like a 35x CV)

#NOPE

#filter out year_loc with low obs
lowobs <- names(which(table(smallpheno$year_loc) < 100))
#only 3: "2020-NYH1" "2021-NYH1" "2021-SCH1L"
smallpheno<-smallpheno[which(smallpheno$year_loc!="2020-NYH1"),]
smallpheno<-smallpheno[which(smallpheno$year_loc!="2021-NYH1"),]
smallpheno<-smallpheno[which(smallpheno$year_loc!="2021-SCH1L"),]
#add ecov
smallpheno<-smallpheno[,c(1:4,17:19)]
smallpheno<-merge(smallpheno,ecov,by.x="year_loc",by.y="YearLocation")

for (fold in levels(factor(smallpheno$year_loc))){
  alltrain<-smallpheno[which(smallpheno$year_loc!=fold),]
  alltest<-smallpheno[which(smallpheno$year_loc==fold),]
  alltrain.rf <- randomForest(y=alltrain$anthesis, x=alltrain[,8:209],
                              xtest=alltest[,8:209],
                              ytest=alltest[,"anthesis"],
                              importance = TRUE,
                              proximity = TRUE)
  plot(alltrain.rf$test$predicted,alltest[,5])
  plot(alltrain.rf$predicted,alltrain[,5])
  print(fold)
  print(alltrain.rf)
  alltrain.rf$importance
}

alltrain<-smallpheno[which(smallpheno$year_loc!="2020-DEH1"),]
alltest<-smallpheno[which(smallpheno$year_loc=="2020-DEH1"),]
alltrain.rf <- randomForest(y=alltrain$anthesis, x=alltrain[,c(4,26:34)],
                            xtest=alltest[,c(4,26:34)],
                            ytest=alltest[,"anthesis"],
                            importance = TRUE,
                            proximity = TRUE)
plot(alltrain.rf$test$predicted,alltest[,5])
plot(alltrain.rf$predicted,alltrain[,5])
print(alltrain.rf)
alltrain.rf$importance

















#try pls first
library(pls)
#run plsr with leave-one-out validation
asi1<-plsr(alltrain$ASI ~ as.matrix(alltrain[,3:204]),
           ncomp = 10, 
           validation = "LOO")
summary(asi1)
plot(RMSEP(asi1))
plot(asi1,plottype="loadings",comps=1:4)
asi1$loadings
