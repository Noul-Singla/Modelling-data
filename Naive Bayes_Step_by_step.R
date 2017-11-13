getwd()
set.seed(123)
heart.disease <- read.csv("SAheart.data")
str(heart.disease)
heart.disease$chd <- as.factor(heart.disease$chd)

#Creating training set adn test set
sample <- sample(heart.disease$row.names,nrow(heart.disease)/2)
train.set <- heart.disease[sample,]
test.set <- heart.disease[-sample,]

#testing for any null or empty values
anyNA(train.set)
anyNA(test.set)


# using library to get variance in a columns
#install.packages("rmngb")
library(rmngb)

#creating a dataframe with mean and variance for continous variables with output as 0 from train set
n.b.cont.cat0 <- cbind(colMeans(train.set[train.set$chd==0,c(-6,-11)]),(colVars(train.set[train.set$chd==0,c(-6,-11)])))
n.b.cont.cat0 <- t(as.data.frame(n.b.cont.cat0))

#creating a dataframe with mean and variance for continous variables with output as 1 from train set
n.b.cont.cat1 <- cbind(colMeans(train.set[train.set$chd==1,c(-6,-11)]),(colVars(train.set[train.set$chd==1,c(-6,-11)])))
n.b.cont.cat1 <- t(as.data.frame(n.b.cont.cat1))

#creating a dataframe with probability values for categorical predictor with output as 0 from train set
n.b.famhist.cat0 <- table(train.set[train.set$chd==0,c(6)])/length(train.set[train.set$chd==0,c(6)])
n.b.famhist.cat0 <- as.data.frame(n.b.famhist.cat0)

#creating a dataframe with probability values for categorical predictor with output as 0 from train set
n.b.famhist.cat1 <- table(train.set[train.set$chd==1,c(6)])/length(train.set[train.set$chd==1,c(6)])
n.b.famhist.cat1 <- as.data.frame(n.b.famhist.cat1)

#calulcating prior probability of a n outcome from the train set
prior.cat0 <- sum(train.set$chd==0)/length(train.set$chd)
prior.cat1 <- sum(train.set$chd==1)/length(train.set$chd)

#Implementing on train set

n.b.train.output <- train.set[,1:3]*0
names(n.b.train.output) <- c("prob_cat0","prob_cat1","output")


for (i in (1:nrow(train.set))){
  #calculating for output as 0
  # using gaussian naive bayes to use mean and variance get probability on test set for continuous variables 
  predict.cont.cat0 <- 1/sqrt(2*pi*n.b.cont.cat0[2,])*exp((-(train.set[i,c(-6,-11)]-n.b.cont.cat0[1,])^2)/(2*n.b.cont.cat0[2,]))
  # using probability from trained data for categrical variables
  predict.cat.cat0 <- n.b.famhist.cat0[n.b.famhist.cat0$Var1==train.set[i,6],2]
  # multiplying the prior * gaussian probability for continuous and class probability for categorical  
  n.b.train.output[i,1] <- apply(predict.cont.cat0,1,prod) * prior.cat0 *predict.cat.cat0
  
  
  #calculating for output as 1
  # using gaussian naive bayes to use mean and variance get probability on test set for continuous variables 
  predict.cont.cat1 <- 1/sqrt(2*pi*n.b.cont.cat1[2,])*exp((-(train.set[i,c(-6,-11)]-n.b.cont.cat1[1,])^2)/(2*n.b.cont.cat1[2,]))
  # using gaussian naive bayes to use mean and variance get probability on test set for continuous variables 
  predict.cat.cat1 <- n.b.famhist.cat1[n.b.famhist.cat1$Var1==train.set[i,6],2]
  # multiplying the prior * gaussian probability for continuous and class probability for categorical
  n.b.train.output[i,2] <- apply(predict.cont.cat1,1,prod) * prior.cat1 *predict.cat.cat1
  
}

#comparing the probabilities to decide the output category
n.b.train.output$output <- as.numeric(n.b.train.output$prob_cat1 >n.b.train.output$prob_cat0)

#checking if the output has complete info for all test values
length(n.b.train.output$output)==length(train.set[,11])

#Correct predictions
sum(n.b.train.output$output==train.set[,11])
#164 

#incorrect predictions
sum(n.b.train.output$output!=train.set[,11])
#67

#table to compare the correct and incorrect predictions
table(n.b.train.output$output,train.set[,11])

#accuracy on train set
sum(n.b.train.output$output==train.set[,11])/length(train.set[,11])
#0.7099

#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------

#Implementing on test set
n.b.output <- test.set[,1:3]*0
names(n.b.output) <- c("prob_cat0","prob_cat1","output")


for (i in (1:nrow(test.set))){
#calculating for output as 0
# using gaussian naive bayes to use mean and variance get probability on test set for continuous variables 
predict.cont.cat0 <- 1/sqrt(2*pi*n.b.cont.cat0[2,])*exp((-(test.set[i,c(-6,-11)]-n.b.cont.cat0[1,])^2)/(2*n.b.cont.cat0[2,]))
# using probability from trained data for categrical variables
predict.cat.cat0 <- n.b.famhist.cat0[n.b.famhist.cat0$Var1==test.set[i,6],2]
# multiplying the prior * gaussian probability for continuous and class probability for categorical  
n.b.output[i,1] <- apply(predict.cont.cat0,1,prod) * prior.cat0 *predict.cat.cat0


#calculating for output as 1
# using gaussian naive bayes to use mean and variance get probability on test set for continuous variables 
predict.cont.cat1 <- 1/sqrt(2*pi*n.b.cont.cat1[2,])*exp((-(test.set[i,c(-6,-11)]-n.b.cont.cat1[1,])^2)/(2*n.b.cont.cat1[2,]))
# using gaussian naive bayes to use mean and variance get probability on test set for continuous variables 
predict.cat.cat1 <- n.b.famhist.cat1[n.b.famhist.cat1$Var1==test.set[i,6],2]
# multiplying the prior * gaussian probability for continuous and class probability for categorical
n.b.output[i,2] <- apply(predict.cont.cat1,1,prod) * prior.cat1 *predict.cat.cat1

}

#comparing the probabilities to decide the output category
n.b.output$output <- as.numeric(n.b.output$prob_cat1 >n.b.output$prob_cat0)

#checking if the output has complete info for all test values
length(n.b.output$output)==length(test.set[,11])

#Correct predictions
sum(n.b.output$output==test.set[,11])
#170

#incorrect predictions
sum(n.b.output$output!=test.set[,11])
#61

#table to compare the correct and incorrect predictions
table(n.b.output$output,test.set[,11])

#accuracy 
170/231
  #  .735

summary(train.set)
