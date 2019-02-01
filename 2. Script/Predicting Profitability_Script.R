library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(corrplot)
library(RColorBrewer)
library(randomForest)
library(plotly)
library(data.table)
library(dplyr)


#import existing a new products dataset
existingproducts <- read.csv("C:\\Users\\Javier Villasmil\\Desktop\\Ubiqum\\Task 06 - Multiple Regression in R\\existingproductattributes2017.2.csv")
newproducts <- read.csv("C:\\Users\\Javier Villasmil\\Desktop\\Ubiqum\\Task 06 - Multiple Regression in R\\newproductattributes2017.2.csv")

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

mostimportantant
existingproducts[,nameindex]

#linearmodel of "THE MOST IMPORTANT VARIABLE" against Volume
lm <- lm(existingproducts[,nameindex] ~ Volume, existingproducts)
summary(lm)

#In summary.lm(lm) : essentially perfect fit: summary may be unreliable#
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


###################################### not working ###################################################
#control <- rfeControl(functions = rfFuncs, method = "repatedCV", repeats = 3)
#outcome <- "Volume"
#theprediction <-  names(existingproducts)[!names(existingproducts)%in%outcome]
#pred_profile <- rfe(existingproducts[,theprediction],existingproducts[,outcome], rfeControl = control)
###################################### not working ###################################################

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

#Training with several variables.
set.seed(123)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
str(existingproducts)
traininglist1 <- existingproducts[c(7,1)]
traininglist2 <- existingproducts[c(7,1,2)]
traininglist3 <- existingproducts[c(7,1,2,3)]
traininglist4 <- existingproducts[c(7,1,2,3,4)]
traininglist5 <- existingproducts[c(7,1,2,3,4,5)]
traininglist6 <- existingproducts[c(7,1,2,3,4,5,6)]

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
control <- trainControl(method="repeatedcv", number=10, repeats=3)

#MULTIPLE LINEAR MODEL
for (i in 1:length(traininglist)){
  if (i == 1){
    resultsMLR <- c()
    modelsMLR <-c()
  }
  set.seed(123)
  rf <- train(Volume ~ ., data = data.frame(traininglist[i]), method = "lm", preProcess=c("center","scale"))
  modelsMLR <- cbind(modelsMLR,rf)
  prediction1 <- predict(rf,testing)
  performance1 <- postResample(prediction1,testing$Volume)
  resultsMLR <- cbind(resultsMLR,performance1)
}
colnames(resultsMLR) <- c("MLR Model 1V","MLR Model 2V","MLR Model 3V","MLR Model 4V","MLR Model 5V","MLR Model 6V")
colnames(modelsMLR) <- c("RF Model 1V","RF Model 2V","RF Model 3V","RF Model 4V","RF Model 5V","RF Model 6V")
resultsMLR
modelsMLR

#RANDOM FOREST
for (i in 1:length(traininglist)){
  if (i == 1){
    resultsRF <- c()
    modelsRF <-c()
  }
  set.seed(123)
  rf <- train(Volume ~ ., data = data.frame(traininglist[i]), method = "rf", trainControl = control, importance = TRUE, preProcess=c("center","scale"), tuneLenght=5)
  modelsRF <- cbind(modelsRF,rf)
  prediction1 <- predict(rf,testing)
  performance1 <- postResample(prediction1,testing$Volume)
  resultsRF <- cbind(resultsRF,performance1)
}
colnames(resultsRF) <- c("RF Model 1V","RF Model 2V","RF Model 3V","RF Model 4V","RF Model 5V","RF Model 6V")
colnames(modelsRF) <- c("RF Model 1V","RF Model 2V","RF Model 3V","RF Model 4V","RF Model 5V","RF Model 6V")
resultsRF
modelsRF

#SVM
for (i in 1:length(traininglist)){
  if (i == 1){
    resultsSVM <- c()
    modelsSVM <-c()
  }
  set.seed(123)
  rf <- train(Volume ~ ., data = data.frame(traininglist[i]), method = "svmRadial", trainControl = control, importance = TRUE, preProcess=c("center","scale"), tuneLenght=5)
  modelsSVM <- cbind(modelsSVM,rf)
  prediction1 <- predict(rf,testing)
  performance1 <- postResample(prediction1,testing$Volume)
  resultsSVM <- cbind(resultsSVM,performance1)
}
colnames(resultsSVM) <- c("SVM Model 1V","SVM Model 2V","SVM Model 3V","SVM Model 4V","SVM Model 5V","SVM Model 6V")
colnames(modelsSVM) <- c("SVM Model 1V","SVM Model 2V","SVM Model 3V","SVM Model 4V","SVM Model 5V","SVM Model 6V")
resultsSVM
modelsSVM

#GBM
for (i in 1:length(traininglist)){
  if (i == 1){
    resultsGBT <- c()
    modelsGBT <-c()
  }
  set.seed(123)
  rf <- train(Volume ~ ., data = data.frame(traininglist[i]), method = "xgbTree", trainControl = control, importance = TRUE, preProcess=c("center","scale"), tuneLenght=5)
  modelsGBT <- cbind(modelsGBT,rf)
  prediction1 <- predict(rf,testing)
  performance1 <- postResample(prediction1,testing$Volume)
  resultsGBT <- cbind(resultsGBT,performance1)
}  

colnames(resultsGBT) <- c("GBT Model 1V","GBT Model 2V","GBT Model 3V","GBT Model 4V","GBT Model 5V","GBT Model 6V")
colnames(modelsGBT) <- c("GBT Model 1V","GBT Model 2V","GBT Model 3V","GBT Model 4V","GBT Model 5V","GBT Model 6V")
resultsGBT
modelsGBT


resultsMLR
resultsRF
resultsSVM
resultsGBT

#BEST MODELS
randomforestselected <- train(Volume ~ ., data = data.frame(traininglist[3]), method = "rf", trainControl = control, importance = TRUE, preProcess=c("center","scale"), tuneLenght=5)
svmselected <- train(Volume ~ ., data = data.frame(traininglist[3]), method = "svmRadial", trainControl = control, importance = TRUE, preProcess=c("center","scale"), tuneLenght=5)  
gbtselected <- train(Volume ~ ., data = data.frame(traininglist[3]), method = "xgbTree", trainControl = control, importance = TRUE, preProcess=c("center","scale"), tuneLenght=5)


predictionRF <- predict(randomforestselected,newproducts)
predictionSVM <- predict(svmselected,newproducts)
predictiongbt <- predict(gbtselected,newproducts)

predictionRF
predictionSVM
predictiogbt


newpredictionlist <- c()
newpredictionlist <- cbind(data.frame(newproducts$ProductType),data.frame(newproducts$ProductNum),predictionRF,predictionSVM,predictiongbt)
newpredictionlist


write.csv(newpredictionlist, file="C2.T3output.csv", row.names = TRUE)
write.csv(newpredictionlist, file="C2.T3output.csv", row.names = TRUE)




#END OF CODE #END OF CODE #END OF CODE #END OF CODE #END OF CODE #END OF CODE #END OF CODE #END OF CODE 
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################

#RANDOM STUFF

#run a second random forest without the removed variables to check the new weights
set.seed(123)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
rf <- train(Volume ~ ., data = existingproducts, method = "rf", importance = TRUE, preProcess=c("center","scale"))
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

#remove the variables with less than 5% of appeareance
for (i in indicesorden){
  if (dfweights$Overall[i] < 5){
    deletevariable <- (rownames(dfweights)[i])
    n <- names(existingproducts)
    n
    nameindex <- which(n == deletevariable)
    existingproducts[,nameindex] <- NULL
    
  } #else if (dfweights$Overall[i] > 15){
    #deletevariable <- (rownames(dfweights)[i])
    #n <- names(existingproducts)
    #nameindex <- which(n == deletevariable)
    #existingproducts[,nameindex] <- NULL
  #}
}


#selectiong the the most significant vairiable according to a RF model using all attributes
for (i in indicesorden){
  if (dfweights$Overall[i] == 100){
    mostimportantant <- (rownames(dfweights)[i])
    n <- names(existingproducts)
    nameindex <- which(n == mostimportantant)
  }
}

mostimportantant
existingproducts[,nameindex]

#linearmodel of "THE MOST IMPORTANT VARIABLE" against Volume
lm <- lm(existingproducts[,nameindex] ~ Volume, existingproducts)
summary(lm) #no aparent correlation, we keep the variable.




#trying to do reccursive random forest
#run a second random forest without the variables removed to check the new weights
set.seed(123)
rf <- train(Volume ~ ., data = existingproducts, method = "rf", importance = TRUE)
rf


#inicia el data frame#
dfinicial <- data.frame()
data.frame[j] <- existingproducts[1]


existingproducts[1]

dfinicial
dfweights
indicesorden <- order(dfweights$Overall, decreasing = TRUE)
indicesorden

###############
n <- names(existingproducts)
n
appendvariable <- colnames(existingproducts[14])
appendvariable
nameindex <- which(n == appendvariable)
nameindex
dfinicial <- cbind(existingproducts[,nameindex])
dfinicial

###############
for (j in 1:4){
  n <- names(existingproducts)
  appendvariable <- rownames(dfweights)[indicesorden[j]]
  nameindex <- which(n == appendvariable)
  dfinicial <- cbind(dfinicial,existingproducts[,nameindex])
}        

dfinicial <- data.frame(dfinicial)
dfinicial$X1 <- NULL
dfinicial

colnames(dfinicial[1])

rownames(dfweights)[indicesorden[17]]

colnames(dfinicial[1])

setnames(dfinicial, old = c(colnames(dfinicial[1]),colnames(dfinicial[2]),colnames(dfinicial[3]),colnames(dfinicial[4]), new = c('anew','dnew',"pewpew","lalala")))


j <- 0
while (j <= 3){
  for (i in indicesorden){
    n <- names(existingproducts)
    print(n)
    appendvariable <- (rownames(dfweights)[i])
    appendvariable
    nameindex <- which(n == appendvariable)
    nameindex
    dfinicial <- cbind(existingproducts, existingproducts[,nameindex])
    dfinicial
  }
}
    

for (i in indicesorden){
  while (j <= 3){
    n <- names(existingproducts)
    print(n)
    appendvariable <- (rownames(dfweights)[i])
    appendvariable
    nameindex <- which(n == appendvariable)
    nameindex
    dfinicial <- cbind(existingproducts, existingproducts[,nameindex])
    dfinicial
    
    j <- j + 1
  }
}
dfinicial

cbind()

dfweights$Overall[17]
dfweights$Overall[13]
dfweights$Overall[15]



view(existingproducts)

names(existingproducts)



existingproducts$"x5StarReviews"

existingprod

deletevariable

dfweights



plyr::arrange(dfweights, desc(Overall) )



?arrange


dfweights[order(dfweights$Overall)]


dfweights




?order



sort(dfweights
rownames(dfweights)[1]

print(rf)



#desicion tree
set.seed(123)
rp <- rpart(Volume ~ .-x5StarReviews-ProfitMargin-ProductType, existingproducts)
summary(rp)
rpart.plot(rp, type = 1)


#END OF CODE
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################

#checking the data distribution# Basic histogram
for (i in 1:ncol(existingproducts)) { #each columns index
  if is.numeric(existingproducts[,i]){ #si la variable es numerica qqplot y histogram
    
    ggplot(existingproducts, aes(x=Volume)) + 
      geom_histogram(binwidth = 200, color="black", fill="white") + 
      geom_vline(aes(xintercept=mean(Volume)),color="red", linetype="dashed", size=1) +
      geom_density(alpha=.2, fill="#FF6666")
    
    
    
    
    ggplot(existingproducts, aes(x=Volume)) + 
      geom_histogram(binwidth = 200, color="black", fill="white") + 
      geom_vline(aes(xintercept=mean(Volume)),color="red", linetype="dashed", size=1) +
      geom_density(alpha=.2, fill="#FF6666") 
    
    
    # Change the width of bins
    ggplot(existingproducts, aes(x=volume)) + 
      geom_histogram(binwidth=1)
    # Change colors
    p<-ggplot(existingproducts, aes(x=volume)) + 
      geom_histogram(color="black", fill="white")
    p
    
    
    
    # Color and shape depend on factor (categorical variable)
    ggplot(surveydata, aes(x=age, y=salary, color=brand)) + geom_point(size=4, alpha=0.6)
    
    ggplot(surveydata, aes(x=age, y=credit, color=brand)) + geom_point(size=4, alpha=0.6)
    
    ggplot(surveydata, aes(x=car, y=salary, color=brand)) + geom_point(size=4, alpha=0.6)
    
    ggplot(surveydata, aes(x=zipcode, y=salary, color=brand)) + geom_point(size=4, alpha=0.6)
    
    ggplot(surveydata, aes(x=elevel, y=salary, color=brand)) + geom_point(size=4, alpha=0.6)
    
    
    ggplot(surveydata, aes(x=salary)) + 
      geom_histogram(color="white", fill=rgb(0.2,0.7,0.1,0.4))
    
    ggplot(surveydata, aes(x=age)) + 
      geom_histogram(color="white", fill=rgb(0.2,0.7,0.1,0.4))
    
    ggplot(surveydata, aes(x=credit)) + 
      geom_histogram(color="white", fill=rgb(0.2,0.7,0.1,0.4))
    
    ggplot(surveydata, aes(x=elevel)) + 
      geom_histogram(binwidth = 0.2, color="white", fill=rgb(0.2,0.7,0.1,0.4),stat = "count" )
    
    ggplot(surveydata, aes(x=car)) + 
      geom_histogram(binwidth = 0.2, color="white", fill=rgb(0.2,0.7,0.1,0.4),stat = "count" ) 
    
    ggplot(surveydata, aes(x=zipcode)) + 
      geom_histogram(binwidth = 0.2, color="white", fill=rgb(0.2,0.7,0.1,0.4),stat = "count" )
    
    ggplot(surveydata, aes(x=brand)) + 
      geom_histogram(binwidth = 0.2, color="white", fill=rgb(0.2,0.7,0.1,0.4),stat = "count" )
    #checking the data with a decisiton tree and a randomforest
    
    #Since we are doing a regression and we have a categorical variable (ProductType) we have to "dummify the data" to make our factors/levels a numeric dummy.
    
    #ProductType is a factor with 12 levels
    str(existingproducts$ProductType)
    summary(existingproducts$ProductType)
    
    #create list selecting our categorical variables and make a new data.frame joining the dummy features with our existing product dataset. is a factor with 12 levels
    dummylist <- dummyVars("~ .",data = existingproducts)
    duexistingproducts <- data.frame(predict(dummylist, newdata = existingproducts))
    
    str(duexistingproducts)
    summary(duexistingproducts)
    
    
    #correlation matrix 
    
    
    corexisting_dummy <- cor(duexistingproducts) 
    
    corexisting_dummy
    corrplot(corexisting_dummy, method = "square", order="hclust", col = brewer.pal(n = 8, name = "RdYlBu"),cl.ratio = 0.2, cl.align = "r")
    