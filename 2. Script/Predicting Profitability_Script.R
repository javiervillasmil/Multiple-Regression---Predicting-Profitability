library(pacman)

p_load(rlang,lattice, ggplot2, caret, rpart, rpart.plot,corrplot,RColorBrewer,randomForest,plotly,data.table,dplyr,rlang,doParallel)

# Prepare Parallel Process
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

#import existing a new products dataset
existingproducts <- read.csv("C:\\Users\\Javier Villasmil\\Desktop\\Ubiqum\\Repositories\\Multiple Regression - Predicting Profitability\\1. Dataset\\existingproductattributes2017.2.csv")
newproducts <- read.csv("C:\\Users\\Javier Villasmil\\Desktop\\Ubiqum\\Repositories\\Multiple Regression - Predicting Profitability\\1. Dataset\\newproductattributes2017.2.csv")

#first look to the existing products dataset
head(existingproducts)
str(existingproducts)
summary(existingproducts)
attributes(existingproducts)
table(existingproducts$ProductType)

#first look to the new products dataset
head(newproducts)
str(newproducts)
summary(newproducts)
table(newproducts$ProductType)

#remove the column BestSellerRank  - it has 15 missing values and the values makes no sense whatosever.
existingproducts$BestSellersRank <- NULL

#remove the column ProductNum  - because it's just an identifier.
existingproducts$ProductNum <- NULL

#random forest - to check meaninful realationships between variables
set.seed(123)
rf <- train(Volume ~ ., data = existingproducts, method = "rf", importance = TRUE)
rf

#save the results of the weighting after the first randomforest
varweights <- varImp(rf) 
varweights

#extract the dataframe with the weights
dfweights <- data.frame(varweights$importance) 
dfweights


#order the index of each variable according the relevance - decreasing
indicesorden <- order(dfweights$Overall, decreasing = TRUE)
indicesorden

#selectiong the the most significant vairiable according to a RF model using all attributes
for (i in indicesorden){
  if (dfweights$Overall[i] == 100){
    mostimportantant <- (rownames(dfweights)[i])
    n <- names(existingproducts)
    nameindex <- which(n == mostimportantant)
  }
}

mostimportantant #relevant feature
existingproducts[,nameindex] #check the values of "5stars" feature

#linearmodel of "THE MOST IMPORTANT VARIABLE" against Volume
lm <- lm(existingproducts[,nameindex] ~ Volume, existingproducts)
summary(lm) #In summary.lm(lm) : essentially perfect fit: summary may be unreliable#


#remove x5StarReviews since its perfectly correlated to volume#
existingproducts$x5StarReviews <- NULL

#remove ProductType because there are few rows and many products, it's not accurate use this variable as a predictor.
existingproducts$ProductType <- NULL

#Check a correlation matrix for possible linear relationships between independent variables.
cormatrix <- cor(existingproducts) 
res2 <- cor.mtest(existingproducts, conf.level = .95)
corrplot(cormatrix, method = "square",p.mat = res2$p, sig.level = .2,tl.col = "black", tl.cex = 0.8, cl.cex = 0.8)
corrplot(cormatrix, method = "number",p.mat = res2$p, sig.level = .2, tl.col = "black", tl.cex = 0.8, cl.cex = 0.8, number.cex = 0.8)

#removing independent variables with high correlation
existingproducts$x3StarReviews <- NULL
existingproducts$x1StarReviews <- NULL
existingproducts$NegativeServiceReview <- NULL

#checking a decision tree
set.seed(123)
rp <- rpart(Volume ~ ., existingproducts, cp =.02)
summary(rp)
rpart.plot(rp, type = 1)

#removing independent variables with no importance in the decistion tree
existingproducts$x3StarReviews <- NULL
existingproducts$x1StarReviews <- NULL
existingproducts$NegativeServiceReview <- NULL
existingproducts$ShippingWeight <- NULL
existingproducts$ProductHeight <- NULL
existingproducts$Price <- NULL
existingproducts$Recommendproduct <- NULL


#Removing Outliers#
p <- plot_ly(data = existingproducts, type = "scatter", x = ~ x4StarReviews, y = ~Volume, mode = 'markers', name = "Reviews") %>% add_trace("scatter", x = ~ x2StarReviews, y = ~Volume,name = "2 Stars Review") %>% add_trace("scatter", x = ~ PositiveServiceReview, y = ~Volume, name = "Positive Service Review") %>% layout(title = 'Styled Scatter',
       yaxis = list(zeroline = FALSE),
       xaxis = list(zeroline = FALSE))
p

#removes volumes over 7000 units
existingproducts <- anti_join(existingproducts, subset(existingproducts, Volume>7000))
#removes repeated warranties
existingproducts <- rbind(existingproducts[1:34,], existingproducts[42:78,])

#train and test sets
set.seed(123)
trainingindices <- createDataPartition(existingproducts$Volume, p = 0.80, list = FALSE)

training <- existingproducts[trainingindices,]
testing  <- existingproducts[-trainingindices,]

#checking quartiles
b <- plot_ly(y = training$Volume, type = "box", name = "Training") %>%
  add_trace(y = testing$Volume, name ="Testing")
b

#Training sets with several variables.
set.seed(123)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
str(existingproducts)
traininglist1 <- existingproducts[c("Volume","x4StarReviews")]
traininglist2 <- existingproducts[c("Volume","x4StarReviews","x2StarReviews")]
traininglist3 <- existingproducts[c("Volume","x4StarReviews","x2StarReviews","PositiveServiceReview")]
traininglist4 <- existingproducts[c("Volume","x4StarReviews","x2StarReviews","PositiveServiceReview","ProductDepth")]
traininglist5 <- existingproducts[c("Volume","x4StarReviews","x2StarReviews","PositiveServiceReview","ProductDepth","ProductWidth")]
traininglist6 <- existingproducts[c("Volume","x4StarReviews","x2StarReviews","PositiveServiceReview","ProductDepth","ProductWidth","ProfitMargin")]

traininglist1
traininglist2
traininglist3
traininglist4
traininglist5
traininglist6

#list with all training sets
traininglist <- list(traininglist1,traininglist2,traininglist3,traininglist4,traininglist5,traininglist6)
traininglist

length(traininglist)
resultsMLR <- c()
resultsRF  <- c()
resultsSVM <- c()
resultsGBT <- c()

#Train Control for the models
control <- trainControl(method="repeatedcv", number=10, repeats=3,verboseIter = TRUE)

#MULTIPLE LINEAR MODEL
for (i in 1:length(traininglist)){
  if (i == 1){
    resultsMLR <- c()
    modelsMLR <-list()
  }
  set.seed(123)
  MLM <- train(Volume ~ ., data = data.frame(traininglist[i]), method = "lm", preProcess=c("center","scale"))
  modelsMLR <- c(modelsMLR,list(MLM))
  prediction1 <- predict(MLM,testing)
  performance1 <- postResample(prediction1,testing$Volume)
  resultsMLR <- cbind(resultsMLR,performance1)
}
colnames(resultsMLR) <- c("MLR Model 1V","MLR Model 2V","MLR Model 3V","MLR Model 4V","MLR Model 5V","MLR Model 6V")
resultsMLR

#RANDOM FOREST
for (i in 1:length(traininglist)){
  if (i == 1){
    resultsRF <- c()
    modelsRF <-list()
  }
  set.seed(123)
  rf <- train(Volume ~ ., data = data.frame(traininglist[i]), method = "rf", trainControl = control, importance = TRUE, preProcess=c("center","scale"), tuneLenght=5)
  modelsRF <- c(modelsRF,list(rf))
  prediction1 <- predict(rf,testing)
  performance1 <- postResample(prediction1,testing$Volume)
  resultsRF <- cbind(resultsRF,performance1)
}
colnames(resultsRF) <- c("RF Model 1V","RF Model 2V","RF Model 3V","RF Model 4V","RF Model 5V","RF Model 6V")
resultsRF
modelsRF

#SVM
for (i in 1:length(traininglist)){
  if (i == 1){
    resultsSVM <- c()
    modelsSVM <-list()
  }
  set.seed(123)
  SVM <- train(Volume ~ ., data = data.frame(traininglist[i]), method = "svmRadial", trainControl = control, importance = TRUE, preProcess=c("center","scale"), tuneLenght=5)
  modelsSVM <- c(modelsSVM,list(SVM))
  prediction1 <- predict(SVM,testing)
  performance1 <- postResample(prediction1,testing$Volume)
  resultsSVM <- cbind(resultsSVM,performance1)
}
colnames(resultsSVM) <- c("SVM Model 1V","SVM Model 2V","SVM Model 3V","SVM Model 4V","SVM Model 5V","SVM Model 6V")
resultsSVM
modelsSVM

#GBT
for (i in 1:length(traininglist)){
  if (i == 1){
    resultsGBT <- c()
    modelsGBT <-c()
  }
  set.seed(123)
  GBT <- train(Volume ~ ., data = data.frame(traininglist[i]), method = "xgbTree", trainControl = control, importance = TRUE, preProcess=c("center","scale"), tuneLenght=5)
  modelsGBT <- c(modelsGBT,list(GBT))
  prediction1 <- predict(GBT,testing)
  performance1 <- postResample(prediction1,testing$Volume)
  resultsGBT <- cbind(resultsGBT,performance1)
}  

colnames(resultsGBT) <- c("GBT Model 1V","GBT Model 2V","GBT Model 3V","GBT Model 4V","GBT Model 5V","GBT Model 6V")
resultsGBT
modelsGBT

#RESULT
###########
resultsMLR
resultsRF
resultsSVM
resultsGBT
###########

#BEST MODELS
randomforestselected <- train(Volume ~ ., data = data.frame(traininglist[3]), method = "rf", trainControl = control, importance = TRUE, preProcess=c("center","scale"), tuneLenght=5)
svmselected <- train(Volume ~ ., data = data.frame(traininglist[3]), method = "svmRadial", trainControl = control, importance = TRUE, preProcess=c("center","scale"), tuneLenght=5)  
gbtselected <- train(Volume ~ ., data = data.frame(traininglist[3]), method = "xgbTree", trainControl = control, importance = TRUE, preProcess=c("center","scale"), tuneLenght=5)

#saving best models
save(randomforestselected, file = "RF_profit.rda")
save(svmselected, file = "SVM_profit.rda")
save(gbtselected, file = "GBT_profit.rda")

#predicting volumes of new products
predictionRF <- predict(randomforestselected,newproducts)
predictionSVM <- predict(svmselected,newproducts)
predictiongbt <- predict(gbtselected,newproducts)

#check predictions
predictionRF
predictionSVM
predictiogbt

#makes a data frame with all the predictions for comparison
newpredictionlist <- c()
newpredictionlist <- cbind(data.frame(newproducts$ProductType),data.frame(newproducts$ProductNum),predictionRF,predictionSVM,predictiongbt)
newpredictionlist


write.csv(newpredictionlist, file="newproducts_prediction.csv", row.names = TRUE)


#END OF CODE #END OF CODE #END OF CODE #END OF CODE #END OF CODE #END OF CODE #END OF CODE #END OF CODE 
########################################################################################################