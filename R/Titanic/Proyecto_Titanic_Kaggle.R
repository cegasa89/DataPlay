# library(gmodels)
# library(ggplot2)
# 
# 
# 
# setwd("C:/Users/cgallego/Desktop/Ciencia de datos y aprendizaje automatico/Proyecto Final Kaggle")
# 
# 
# train <- read.csv("train.csv",stringsAsFactors = FALSE) 
# test <- read.csv("test.csv",stringsAsFactors = FALSE)
# 
# #comprobamos que hemos hecho la carga correcta
# str(train)
# 
# str(test)
# 
# 
# #comprobamos de forma absoluta y relativa la proporcion 
# #de supervivientes y de muertos
# 
# table(train$Survived) 
# 
# prop.table(table(train$Survived)) 
# 
# 
# #todos mueren
# 
# #test$Survived <- rep(0, 418)
# #submit <- data.frame(PassengerId= test$PassengerId, Survived= test$Survived) 
# #write.csv(submit, file = "allDead.csv", row.names = FALSE)
# 
# 
# #muertos y vivos segun genero.
# prop.table(table(train$Survived, train$Sex))*100 
# 
# 
# #viven todas las mujeres, mueren todos los hombres
# 
# #nueva columna y hacemos un if para asignar a superviviente si es mujer
# #el valor 1 y si es hombre el valor 0
# test$Survived <- ifelse(test$Sex=="female", 1, 0)
# submit <- data.frame(PassengerId= test$PassengerId, Survived= test$Survived) 
# write.csv(submit, file = "allMenDead.csv", row.names = FALSE)
# 
# #edad, genero y supervivientes
# prop.table(table(train$Age, train$Sex, train$Survived))*100
# aggregate(Survived ~ Age + Sex, data=train, FUN= function(x) {sum(x)})
# 
# 
# #discretizar la variable y almacenarlo en una nueva columna
# train$FareBin[train$Fare < 10] <- '<10'
# train$FareBin[train$Fare >= 10 & train$Fare <= 20 ] <- '10-20'
# train$FareBin[train$Fare > 20 & train$Fare <= 30 ] <- '20-30'
# train$FareBin[train$Fare > 30] <- '>30'
# 
# #supervivientes segund genero claro y precio del billete
# aggregate(Survived ~ FareBin + Pclass + Sex, data=train, FUN= function(x) {sum(x)/length(x)})
# 
# 
# test$Survived <- ifelse(test$Sex=="female",ifelse(test$Pclass == 3 & test$Fare>20, 0, 1), 0)
# submit <- data.frame(PassengerId= test$PassengerId, Survived= test$Survived) 
# write.csv(submit, file = "allMenAndFemale320Dead.csv", row.names = FALSE)
# 
# 
# setwd("C:/Users/cgallego/Desktop/Ciencia de datos y aprendizaje automatico/Proyecto Final Kaggle")
# 
# train <- read.csv("train.csv",stringsAsFactors = FALSE) 
# test <- read.csv("test.csv",stringsAsFactors = FALSE) 
# test$Survived <- NA 
# all <- rbind(train, test)
# 
# train$Name[1:10]
# 
# #https://www.kaggle.com/janlauge/glmnet-with-feature-engineering
# 
# all$Title <- sapply(all$Name, FUN = function(x) gsub(" ", "", {strsplit(x, split='[,.]')[[1]][2]}))
# 
# all$Title<- ifelse(all$Title=="Mr" | all$Title=="Mrs" | all$Title=="Miss" | all$Title=="Master",all$Title, "Otros")
# 
# table(all$Title)
# 
# 
# all$FamilySize <- 1+ all$SibSp + all$Parch
# 
# 
# ggplot(all, aes(x = FamilySize, fill = factor(Sex)))+
#        geom_bar(stat='count', position='dodge')+
#        labs(x = 'Numero de familiares') + facet_grid(.~Survived)+
#        theme_minimal()
# 
# 
# 
# 
# 
# all2 <- rbind(train)
# #nos quedamos solo con las mujeres para ver las viudas
# all2 <- all2[all2$Sex=="female",]
# #mujeres que tiene 1 conyuge o 1 hermano a bordo
# all2 <- all2[all2$Sex=="female" & all2$SibSp!=0,]
# 
# #mujeres con 1 conyuge o 1 hermano y mayores de 18 suponiendo esta edad minima para estar casada
# all2 <- all2[all2$Sex=="female" & all2$SibSp==1 & all2$Age>=18, ]
# #eliminamos filas con valores nulos
# all2 <- na.omit(all2)
# 
# 
# 
# all2 <- rbind(train)
# all2$FamilySize <- 1+ all2$SibSp + all2$Parch
# ggplot(all2, aes(x = FamilySize, fill = factor(Pclass)))+
#      geom_bar(stat='count', position='dodge')+
#      labs(x = 'Numero de familiares') + facet_grid(.~Survived)+
#      theme_minimal()
# 
# 
# 
# 
# 
# #install.packages(Amelia) 
# library(Amelia) 
# #para ver en una grafica los valores faltantes con na
# missmap(all)
# 
# #sustituir valores na por la media
# all$Age <- ifelse(is.na(all$Age), mean(all$Age, na.rm = TRUE), all$Age)
# all$Fare <- ifelse(is.na(all$Fare), mean(all$Fare, na.rm = TRUE), all$Fare)






library(gmodels)
library(ggplot2)
library(caret)
library(randomForest)
library(C50)
library(e1071)
library(nnet)
library(klaR)
library(kernlab)
library(keras)
library(rpart)
library(party)
library(gbm)



setwd(getwd())


train <- read.csv("train.csv",stringsAsFactors = FALSE)
test <- read.csv("test.csv",stringsAsFactors = FALSE)
#comprobamos que hemos hecho la carga correcta
str(train)
str(test)

test$Survived <- NA

all <- rbind(train, test)
all$Survived <- factor(all$Survived) 
train <- all[1:891,]
test <- all[892:1309,]

set.seed(288) #Semilla 

train$Title <- sapply(train$Name, FUN = function(x) gsub(" ", "", {strsplit(x, split='[,.]')[[1]][2]}))
train$Title<- ifelse(train$Title=="Mr" | train$Title=="Mrs" | train$Title=="Miss" | train$Title=="Master",train$Title, "Otros")
test$Title <- sapply(test$Name, FUN = function(x) gsub(" ", "", {strsplit(x, split='[,.]')[[1]][2]}))
test$Title<- ifelse(test$Title=="Mr" | test$Title=="Mrs" | test$Title=="Miss" | test$Title=="Master",test$Title, "Otros")
train$FamSize <- 1+ train$SibSp + train$Parch
test$FamSize <- 1+ test$SibSp + test$Parch
train$Age <- ifelse(is.na(train$Age), mean(train$Age, na.rm = TRUE), train$Age)
train$Fare <- ifelse(is.na(train$Fare), mean(train$Fare, na.rm = TRUE), train$Fare)
test$Age <- ifelse(is.na(test$Age), mean(test$Age, na.rm = TRUE), test$Age)
test$Fare <- ifelse(is.na(test$Fare), mean(test$Fare, na.rm = TRUE), test$Fare)

train$Marry <- ifelse((train$Title=="Mr" | train$Title=="Mrs") & train$SibSp>=1 , 1, 0)
test$Marry <- ifelse((test$Title=="Mr" | test$Title=="Mrs") & test$SibSp>=1 , 1, 0)

#train$SecondName <- sapply(train$Name, FUN = function(x) gsub(" ", "", {strsplit(x, split='[,.]')[[1]][1]}))
#test$SecondName <- sapply(test$Name, FUN = function(x) gsub(" ", "", {strsplit(x, split='[,.]')[[1]][1]}))




train_control <- trainControl(method="repeatedcv",number = 100) #Cross-Validation 
#RF <- train(Survived ~ Title + Pclass, data=train, trControl = train_control,importance=TRUE, method="rf") 
#RF <- train(Survived ~ Sex + Title + FamSize + Pclass + Age + SibSp + Parch + Fare + Embarked+Marry, data=train, trControl = train_control, method="rf") 



#klaR
#RF<- train(Survived ~ Sex + Title + FamSize + Pclass + Age + SibSp + Parch + Fare + Embarked, data = train, method = "nb", trControl = train_control, verbose = FALSE)

#DTFit_C50 <- train(Survived ~ Sex + Title + FamSize + Pclass + Age + SibSp + Parch + Fare + Embarked, data = train, method = "C5.0", trControl = train_control, verbose = FALSE)

#DTFit_nnet 
#RF<- train(Survived ~ Sex + Title + FamSize + Pclass + Age + SibSp + Parch + Fare + Embarked, data = train, method = "nnet", trControl = train_control, verbose = FALSE)

#DTFit_knn 
#RF <- train(Survived ~ Sex + Title + FamSize + Pclass + Age + SibSp + Parch + Fare + Embarked, data = train, method = "kknn", trControl = train_control, verbose = FALSE)
#DTFit_svmLinear 
#RF <- train(Survived ~ Sex + Title + FamSize + Pclass + Age + SibSp + Parch + Fare + Embarked + Marry, data = train, method = "svmLinear", trControl = train_control, verbose = FALSE)


# seeds <- vector(mode = "list", length = 11)
# 
# seeds[[1]] <- c(530027,46959,963213)
# seeds[[2]] <- c(241405,265555,238179)
# seeds[[3]] <- c(129848,874494,742986)
# seeds[[4]] <- c(25840,452037,412311)
# seeds[[5]] <- c(665356,647161,43604)
# seeds[[6]] <- c(124973,859473,659772)
# seeds[[7]] <- c(600613,296715,422484)
# seeds[[8]] <- c(129501,934855,505637)
# seeds[[9]] <- c(350313,971439,730253)
# seeds[[10]] <- c(589704,450947,243901)
# seeds[[11]] <- c(282418)
# 
# RF <- train(Survived ~ Sex + Title + FamSize + Pclass + Age + SibSp + Parch + Fare + Embarked + Marry, data = train,  method = "cforest",
#             preProc = c("knnImpute"),
#             trControl = trainControl(method = "cv",
#                                      seeds = seeds),
#             na.action = na.pass)


RF <- train(Survived ~ Sex + Title +  Pclass + Age  + Fare + Embarked, data = train, preProcess = c("center", "scale"), method = "gbm", trControl = train_control, verbose = FALSE)



RF #Mostramos el modelo entrenado

#importance <- varImp(RF, scale=FALSE) 
#dev.off()
#plot(importance)

Prediction <- predict(RF, test) 
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction) 
write.csv(submit, file = "ultimo.csv", row.names = FALSE)



