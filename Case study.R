install.packages("mlbench")
library(mlbench)
library(readr)
library(caTools)
Data <- read_csv("C:/Users/pm2500925/Desktop/MS BAPM/Predective modelling - 5604/Travelers Case study/Train.csv")

#Data cleaning
#Removing rows with cancel values -1

Data <- subset(Travelers, cancel!=-1)

#Removing NA
Data <- na.omit(Data)

#Travelers$ni.age=xyz
#Travelers$bins=cut(Travelers$ni.age,5,include.lowest = TRUE)
#Travelers$ni.age <- ifelse(Travelers$ni.age>=80, 80, Travelers$ni.age)
# summary(Travelers$ni.age)

# Travelers=Travelers[,-c(19,20)]

#Applying Binary encoding to categorical variables
Data = data.frame(model.matrix(~., data = Data, contrasts.arg = NULL))

#Data$zip.code <- as.factor(Data$zip.code)
#Data$ni.marital.status <- as.factor(Data$ni.marital.status)
#Data$sales.channel <- as.factor(Data$sales.channel)
#Data$credit <- as.factor(Data$credit)


#Splitting Data into Training and Validation Datasets
split <- sample.split(Data,SplitRatio = 0.8)

Training <- subset(Data,split==TRUE)
Testing <- subset(Data,split==FALSE)

#creating model

model = glm(cancel~.-year-
              house.coloryellow-
              house.colorwhite-
              house.colorred-
              dwelling.typeTenant-
              dwelling.typeHouse-
              coverage.typeB-
              coverage.typeC-
              premium-
              ni.genderM-
              id, data = Training, family = "binomial")

summary(model)

#Predicting on Testing dataset
Prediction <- predict(model, Testing)

table(Testing$cancel, Prediction>0.5)


pred_input <- prediction(Prediction,Testing$cancel)
AUC <- performance(pred_input,"auc")
print(AUC@y.values)

