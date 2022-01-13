###MDA Assignment#6 Decision Tree 2014170856 Jiwon Park
library(tree)
library(rpart)
library(rpart.plot)
library(caret)
library(readr)
library(rattle)
library(RColorBrewer)
library(evtree)


# Performance Evaluation Function -----------------------------------------
perf_eval <- function(cm){
  
  # True positive rate: TPR (Recall)
  TPR <- cm[2,2]/sum(cm[2,])
  # Precision
  PRE <- cm[2,2]/sum(cm[,2])
  # True negative rate: TNR
  TNR <- cm[1,1]/sum(cm[1,])
  # Simple Accuracy
  ACC <- (cm[1,1]+cm[2,2])/sum(cm)
  # Balanced Correction Rate
  BCR <- sqrt(TPR*TNR)
  # F1-Measure
  F1 <- 2*TPR*PRE/(TPR+PRE)
  
  return(c(TPR, PRE, TNR, ACC, BCR, F1))
}

# Performance table
Perf.Table <- matrix(0, nrow = 1, ncol = 6)
colnames(Perf.Table) <- c("TPR", "Precision", "TNR", "Accuracy", "BCR", "F1-Measure")

# Load the data & Preprocessing
heart <- read.csv("heart (4).csv")
input.idx <- c(1:13)
target.idx <- 14

heart.input <- heart[,input.idx]
heart.target <- as.factor(heart[,target.idx])

heart.data <- data.frame(heart.input, heart.target)

#Separate Training and Test Dataset
set.seed(28352)
trn_idx <- sample(1:length(heart.target), round(0.66*length(heart.target)))
trnInputs <- heart.input[trn_idx,]
trnTargets <- heart.target[trn_idx]
valInputs <- heart.input[-trn_idx,]
valTargets <- heart.target[-trn_idx]

heart.trn <- heart.data[trn_idx,]
heart.tst <- heart.data[-trn_idx,]


# Q1. Classification and Regression Tree (CART) -------------------------------
# Training the tree
heart.model <- tree(heart.target ~ ., heart.trn)
summary(heart.model)

# Plot the tree
plot(heart.model)
text(heart.model, pretty = 2)

#Validation before pruning
# Prediction
haert.prey <- predict(heart.model, heart.tst, type = "class")
heart.cfm <- table(heart.tst$heart.target, haert.prey)
heart.cfm

Perf.Table[1,] <- perf_eval(heart.cfm)
Perf.Table


# Q2.Find the best tree
set.seed(12345)
heart.model.cv <- cv.tree(heart.model, FUN = prune.misclass)

# Plot the pruning result
plot(heart.model.cv$size, heart.model.cv$dev, type = "b")
heart.model.cv

# Select the final model
heart.model.pruned <- prune.misclass(heart.model, best = 9)
plot(heart.model.pruned)
text(heart.model.pruned, pretty = 1)

# Prediction
heart.prey <- predict(heart.model.pruned, heart.tst, type = "class")
heart.cfm <- table(heart.tst$heart.target, heart.prey)
heart.cfm

Perf.Table[1,] <- perf_eval(heart.cfm)
Perf.Table


# Performance table
Perf.Table <- matrix(0, nrow = 1, ncol = 6)
colnames(Perf.Table) <- c("TPR", "Precision", "TNR", "Accuracy", "BCR", "F1-Measure")


#Q3. rpart_____________________________________________________________________________________________
#2.2.1 gini and entropy
#gini
heart.rpart <- rpart(heart.target ~., data = heart.trn, method='class') 
# Prediction
heart.prey <- predict(heart.rpart, heart.tst, type = "class")
heart.cfm <- table(heart.tst$heart.target, heart.prey)
heart.cfm
Perf.Table<- rbind(Perf.Table, perf_eval(heart.cfm))
Perf.Table

#entropy
heart.rpart <- rpart(heart.target ~., data = heart.trn, method='class', parms = list(split="entropy"))
# Prediction
heart.prey <- predict(heart.rpart, heart.tst, type = "class")
heart.cfm <- table(heart.tst$heart.target, heart.prey)
heart.cfm
Perf.Table<- rbind(Perf.Table, perf_eval(heart.cfm))
Perf.Table




#2.2.2 minsplit = 10, 20, 30
# Performance table
Perf.Table <- matrix(0, nrow = 1, ncol = 6)
colnames(Perf.Table) <- c("TPR", "Precision", "TNR", "Accuracy", "BCR", "F1-Measure")

for (i in c(10, 20, 30)){
  heart.rpart <- rpart(heart.target ~., data = heart.trn, method='class',control = rpart.control(minsplit = i) )
  fancyRpartPlot(heart.rpart)
  
  # Prediction
  heart.prey <- predict(heart.rpart, heart.tst, type = "class")
  heart.cfm <- table(heart.tst$heart.target, heart.prey)
  heart.cfm
  
  Perf.Table<- rbind(Perf.Table, perf_eval(heart.cfm))
  Perf.Table
  
}
Perf.Table
write.csv(Perf.Table, file = "rpart_perf1.csv",row.names=FALSE)

#2.2.3 maxdepth = 4, 5, 6
# Performance table
Perf.Table <- matrix(0, nrow = 1, ncol = 6)
colnames(Perf.Table) <- c("TPR", "Precision", "TNR", "Accuracy", "BCR", "F1-Measure")

for (i in c(4, 5, 6)){
  heart.rpart <- rpart(heart.target ~., data = heart.trn, method='class',control = rpart.control(maxdepth = i) )
  fancyRpartPlot(heart.rpart)
  
  # Prediction
  heart.prey <- predict(heart.rpart, heart.tst, type = "class")
  heart.cfm <- table(heart.tst$heart.target, heart.prey)
  heart.cfm
  
  Perf.Table<- rbind(Perf.Table, perf_eval(heart.cfm))
  Perf.Table
  
}
Perf.Table
write.csv(Perf.Table, file = "rpart_perf2.csv",row.names=FALSE)

#3. Party package____________________________________________________________________________________
library(party)
Perf.Table <- matrix(0, nrow = 1, ncol = 6)
colnames(Perf.Table) <- c("TPR", "Precision", "TNR", "Accuracy", "BCR", "F1-Measure")

#3.2.1 minsplit = 10, 50, 100
for (i in c(10, 50, 100)){
  heart.party <- ctree(heart.target ~., data = heart.trn, method='class',
                       control = ctree_control(minsplit = i) )
  
  plot(heart.party)
  
  # Prediction
  heart.prey <- predict(heart.party, heart.tst, type = "response")
  heart.cfm <- table(heart.tst$heart.target, heart.prey)
  heart.cfm
  
  Perf.Table<- rbind(Perf.Table, perf_eval(heart.cfm))
  Perf.Table
  
}
Perf.Table
write.csv(Perf.Table, file = "party_perf1.csv",row.names=FALSE)



#3.2.2 mtry = 3, 4, 5
Perf.Table <- matrix(0, nrow = 1, ncol = 6)
colnames(Perf.Table) <- c("TPR", "Precision", "TNR", "Accuracy", "BCR", "F1-Measure")

for (i in c(3, 4, 5)){
  heart.party <- ctree(heart.target ~., data = heart.trn, method='class',
                       control = ctree_control(mtry = i) )
  
  plot(heart.party)
  
  # Prediction
  heart.prey <- predict(heart.party, heart.tst, type = "response")
  heart.cfm <- table(heart.tst$heart.target, heart.prey)
  heart.cfm
  
  Perf.Table<- rbind(Perf.Table, perf_eval(heart.cfm))
  Perf.Table
  
}
Perf.Table
write.csv(Perf.Table, file = "party_perf2.csv",row.names=FALSE)

#4. Evtree package________________________________________________________-
library(evtree)
#4.2.1 alpha = 0.5, 1, 2
Perf.Table <- matrix(0, nrow = 1, ncol = 6)
colnames(Perf.Table) <- c("TPR", "Precision", "TNR", "Accuracy", "BCR", "F1-Measure")
for (i in c(0.5, 1, 2)){
  heart.evtree <- evtree(heart.target ~.,data = heart.trn,
                         control = evtree.control(alpha = i))
  plot(heart.evtree)
  # Prediction
  heart.prey <- predict(heart.evtree, heart.tst, type = "response")
  heart.cfm <- table(heart.tst$heart.target, heart.prey)
  heart.cfm
  
  Perf.Table<- rbind(Perf.Table, perf_eval(heart.cfm))
  Perf.Table
  
}
Perf.Table
write.csv(Perf.Table, file = "evtree_perf1.csv",row.names=FALSE)


#4.2.2 ntrees = 11, 15, 20
Perf.Table <- matrix(0, nrow = 1, ncol = 6)
colnames(Perf.Table) <- c("TPR", "Precision", "TNR", "Accuracy", "BCR", "F1-Measure")
for (i in c(11,15,20)){
  heart.evtree <- evtree(heart.target ~.,data = heart.trn,
                         control = evtree.control(ntrees = i))
  plot(heart.evtree)
  # Prediction
  heart.prey <- predict(heart.evtree, heart.tst, type = "response")
  heart.cfm <- table(heart.tst$heart.target, heart.prey)
  heart.cfm
  
  Perf.Table<- rbind(Perf.Table, perf_eval(heart.cfm))
  Perf.Table

}
Perf.Table
write.csv(Perf.Table, file = "evtree_perf2.csv",row.names=FALSE)

