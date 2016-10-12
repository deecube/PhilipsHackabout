#Read Dataset
DATA <- read.csv("obesity_updated.csv", header = TRUE)

summary(DATA )

#------------------------------------------------------------


#Splitting data
set.seed(1000)
library(caTools)
split = sample.split(DATA$GROUP, SplitRatio = 0.7)
train <- subset(DATA, split == T)
Test_Data <- subset(DATA, split == F)


library(stringr)

#..........................................................................

#Classifying SYNTAX scores in TRAIN DATA

train.NEWGRP <- data.frame(SYNGRP = rep("None", nrow(train)), train[,])

train.NEWGRP$SYNGRP<- ifelse(train.NEWGRP$SYNTAX < 22 , 1,ifelse(train.NEWGRP$SYNTAX > 32 ,3,2))

#Classifying SYNTAX scores in TEST DATA

Test_Data.NEWGRP <- data.frame(SYNGRP = rep("None", nrow(Test_Data)), Test_Data[,])

Test_Data.NEWGRP$SYNGRP<- ifelse(Test_Data.NEWGRP$SYNTAX < 22 , 1,ifelse(Test_Data.NEWGRP$SYNTAX > 32 ,3,2))

#------------------------------------------------


grp4 <- train[which(str_detect(train$GROUP, "4")),]
grp3 <- train[which(str_detect(train$GROUP, "3")),]
grp2 <- train[which(str_detect(train$GROUP, "2")),]
grp1 <- train[which(str_detect(train$GROUP, "1")),]

# division of groups into grp 1 2
grp12<-rbind(grp1,grp2)

#test group division into group 34
grp34<-rbind(grp3,grp4)


Test_Data$GROUP<-as.character(Test_Data$GROUP)

#dividing test data into groups 12 and 34

grpTEST4 <- Test_Data[which(str_detect(Test_Data$GROUP, "4")),]
grpTEST3 <- Test_Data[which(str_detect(Test_Data$GROUP, "3")),]
grpTEST2 <- Test_Data[which(str_detect(Test_Data$GROUP, "2")),]
grpTEST1 <- Test_Data[which(str_detect(Test_Data$GROUP, "1")),]

grpTEST12<-rbind(grpTEST1,grpTEST2)
grpTEST34<-rbind(grpTEST3,grpTEST4)


train.NEWGRP.IR <- data.frame(IR_NIR = rep("None",nrow(train.NEWGRP)),train.NEWGRP[,])
train.NEWGRP.IR$IR_NIR <- ifelse(train.NEWGRP.IR$HOMA_IR > 4.21,1,0 )
train.NEWGRP.IR$GROUPMAIN <- as.integer(train.NEWGRP.IR$GROUPMAIN)

train.NEWGRP.CADR <- data.frame(CADRISK = rep("None",nrow(train.NEWGRP)),train.NEWGRP[,])
train.NEWGRP.CADR$CADRISK <- ifelse(train.NEWGRP.CADR$hsCRP > 3.00,1,0 )
train.NEWGRP.CADR$GROUPMAIN <- as.integer(train.NEWGRP.CADR$GROUPMAIN)

#-----------------------------------------------------------------------
#chi-squared testing for HOMA_IR

library("MASS")

class.predictor <- data.frame(train.NEWGRP.IR$IR_NIR,train.NEWGRP.IR$SYNGRP)

class.predictor = table(train.NEWGRP.IR$IR_NIR,train.NEWGRP.IR$SYNGRP) 

print(class.predictor)

print(chisq.test(class.predictor))

#chi-squared test

#chi-squared test for hsCRP

class.predictor.1 <- data.frame(train.NEWGRP.CADR$CADRISK,train.NEWGRP.CADR$SYNGRP)

class.predictor.1 = table(train.NEWGRP.CADR$CADRISK,train.NEWGRP.CADR$SYNGRP) 

print(class.predictor.1)

print(chisq.test(class.predictor.1))

#chi-squared test for hsCRP

#correlation between variables
train <- subset( train, select = -FI )
cor(train$hsCRP,train$SYNTAX)
#confidence matrix
library(corrplot)
confidence.matrix <- cor(train)
corrplot(confidence.matrix,method = "square")

#confidence matrix

#-------------------------------------

#----------plots for self understanding-----------------


library(ggplot2)
# BMI LEVELS 18-24 ARE NORMAL
#WE CAN SEE THAT GRP 3,4 ARE OVER WEIGHT(PROOF)

train$BMI <- as.factor(train$BMI)
ggplot(train, aes(x = BMI, )) +
  geom_bar() +
  facet_wrap(~GROUP) + 
  ggtitle("BMI CHART") +
  xlab("BMI") +
  ylab("COUNT") +
  labs(fill = "BMI")






train$HC <- as.factor(train$FPG)
ggplot(train, aes(x = FPG, )) +
  geom_bar() +
  facet_wrap(~GROUP) + 
  ggtitle("FPGCHART") +
  xlab("FPG") +
  ylab("COUNT") +
  labs(fill = "FPG")

train$HC <- as.factor(train$HbA1c)
ggplot(train, aes(x = HbA1c, )) +
  geom_bar() +
  facet_wrap(~GROUP) + 
  ggtitle("HbA1c CHART") +
  xlab("HbA1c") +
  ylab("COUNT") +
  labs(fill = "HbA1c")


train$HC <- as.factor(train$TC)
ggplot(train, aes(x = TC, )) +
  geom_bar() +
  facet_wrap(~GROUP) + 
  ggtitle("TC CHART") +
  xlab("TC") +
  ylab("COUNT") +
  labs(fill = "TC")

train$HC <- as.factor(train$TG)
ggplot(train, aes(x = TG, )) +
  geom_bar() +
  facet_wrap(~GROUP) + 
  ggtitle("TG CHART") +
  xlab("TG") +
  ylab("COUNT") +
  labs(fill = "TG")

train$HC <- as.factor(train$HDL)
ggplot(train, aes(x = HDL, )) +
  geom_bar() +
  facet_wrap(~GROUP) + 
  ggtitle("HDL CHART") +
  xlab("HDL") +
  ylab("COUNT") +
  labs(fill = "HDL")

train$HC <- as.factor(train$LDL)
ggplot(train, aes(x = LDL, )) +
  geom_bar() +
  facet_wrap(~GROUP) + 
  ggtitle("LDL CHART") +
  xlab("LDL") +
  ylab("COUNT") +
  labs(fill = "LDL")

train$HC <- as.factor(train$CH_HDL)
ggplot(train, aes(x = CH_HDL, )) +
  geom_bar() +
  facet_wrap(~GROUP) + 
  ggtitle("CH_HDL CHART") +
  xlab("CH_HDL") +
  ylab("COUNT") +
  labs(fill = "CH_HDL")

train$HC <- as.factor(train$FI)
ggplot(train, aes(x = FI, )) +
  geom_bar() +
  facet_wrap(~GROUP) + 
  ggtitle("FI CHART") +
  xlab("FI") +
  ylab("COUNT") +
  labs(fill = "FI")

train$HC <- as.factor(train$HOMA_IR)
ggplot(train, aes(x = HOMA_IR, )) +
  geom_bar() +
  facet_wrap(~GROUP) + 
  ggtitle("HOMA_IR CHART") +
  xlab("HOMA_IR") +
  ylab("COUNT") +
  labs(fill = "HOMA_IR")





train$HC <- as.factor(train$hsCRP)
ggplot(train, aes(x = hsCRP, )) +
  geom_bar() +
  facet_wrap(~GROUP) + 
  ggtitle("hsCRP CHART") +
  xlab("hsCRP") +
  ylab("COUNT") +
  labs(fill = "hsCRP")



train$HC <- as.factor(train$GENSINI)
ggplot(train, aes(x = GENSINI, )) +
  geom_bar() +
  facet_wrap(~GROUP) + 
  ggtitle("GENSINI CHART") +
  xlab("GENSINI") +
  ylab("COUNT") +
  labs(fill = "GENSINI")




train$HC <- as.factor(train$SYNTAX)
ggplot(train, aes(x = SYNTAX, )) +
  geom_bar() +
  facet_wrap(~GROUP) + 
  ggtitle("SYNTAX CHART") +
  xlab("SYNTAX") +
  ylab("COUNT") +
  labs(fill = "SYNTAX")



train$HC <- as.factor(train$Age)
ggplot(train, aes(x = Age, )) +
  geom_bar() +
  facet_wrap(~GROUP) + 
  ggtitle("Age CHART") +
  xlab("Age") +
  ylab("COUNT") +
  labs(fill = "Age")





train$HC <- as.factor(train$WC)
ggplot(train, aes(x = WC, )) +
  geom_bar() +
  facet_wrap(~GROUP) + 
  ggtitle("WC CHART") +
  xlab("WC") +
  ylab("COUNT") +
  labs(fill = "WC")



#----------------------------------------------------------------------

#TRAINING OF MODEL based on SYN GROUP---------------------------------------

library(randomForest)
rf.label1 <- as.factor(train.NEWGRP$SYNGRP)
rf.train.2 <- train.NEWGRP[,c("TG","WC","HbA1c")]



set.seed(1234)
rf_model1 <- randomForest(x = rf.train.2, y = rf.label1, importance = TRUE, ntree = 11111)
rf_model1
varImpPlot(rf_model1)



predicted_rf <- predict(rf_model1, newdata = Test_Data)
rf_predictedVsactual = table(predicted_rf,Test_Data.NEWGRP$SYNGRP)

View(rf_predictedVsactual)

rf_accuracy <- sum(diag(rf_predictedVsactual))/sum(rf_predictedVsactual)

#-------------------------------------------------
#Training and Prediction of Groups 
#-----------------------------------GRP 1&2 ----------------------------
#Train model based on BMI
rf.label1 <- as.factor(grp12$GROUP)
rf.train.2 <- grp12[,c("HDL","Age")]



set.seed(1234)
rf_model1 <- randomForest(x = rf.train.2, y = rf.label1, importance = TRUE, ntree = 1000)
rf_model1
varImpPlot(rf_model1)



predicted_rf <- predict(rf_model1, newdata = grpTEST12)
rf_predictedVsactual = table(predicted_rf,grpTEST12$GROUP)

View(rf_predictedVsactual)

rf_accuracy <- sum(diag(rf_predictedVsactual))/sum(rf_predictedVsactual)


#-------------GRP 3& 4-----------------------------
rf.label1 <- as.factor(grp34$GROUP)
rf.train.2 <- grp34[,c("HbA1c","Age")]



set.seed(1234)
rf_model2 <- randomForest(x = rf.train.2, y = rf.label1, importance = TRUE, ntree = 1000)
rf_model2
varImpPlot(rf_model2)



predicted_rf <- predict(rf_model2, newdata = grpTEST34)
rf_predictedVsactual = table(predicted_rf,grpTEST34$GROUP)

View(rf_predictedVsactual)

rf_accuracy <- sum(diag(rf_predictedVsactual))/sum(rf_predictedVsactual)
#------------------------------------------------

#-------------------------------------
# Finding association of various variables with SYNTAX scores
#PO SEGREGATION

PO <- DATA[which(DATA$BMI>24),]

summary(PO$SYNTAX)

#MO SEGREGATION

grp4 <- DATA[which(str_detect(DATA$GROUP, "4")),]

grp2 <- DATA[which(str_detect(DATA$GROUP, "2")),]


MO<-rbind(grp2,grp4)

summary(MO$SYNTAX)



#-------------------------------------------END OF CODE-----------------------------------------


