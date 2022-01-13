library(psych) 
library(ggplot2)
library(corrplot)
library(moments)
library(xlsx)
library(pROC)
library(ROCR)


# Performance Evaluation Function -----------------------------------------
perf_eval2 <- function(cm){
  # True positive rate: TPR (Recall)
  TPR <- cm[2,2]/sum(cm[2,])
  # True negative rate: TNR
  TNR <- cm[1,1]/sum(cm[1,])
  # False positive rate: FPR
  FPR <- cm[2,1]/sum(cm[2,])
  # False negative rate: FNR
  FNR <- cm[1,2]/sum(cm[1,])
  # Simple Accuracy
  ACC <- (cm[1,1]+cm[2,2])/sum(cm)
  # Balanced Correction Rate
  BCR <- sqrt(TPR*TNR)
  # Precision
  PRE <- cm[2,2]/sum(cm[,2])
  # F1-Measure
  F1 <- 2*TPR*PRE/(TPR+PRE)
  
  return(c(TPR,TNR, FPR, FNR, ACC, BCR, PRE, F1))
}





# Initialize the performance matrix1
perf_mat <- matrix(0, 1, 8)
colnames(perf_mat) <- c("TPR","TNR", "FPR", "FNR", "ACC", "BCR", "PRE", "F1")
rownames(perf_mat) <- "Logstic Regression"

# Initialize the performance matrix2
perf_mat2 <- matrix(0, 1, 8)
colnames(perf_mat2) <- c("TPR","TNR", "FPR", "FNR", "ACC", "BCR", "PRE", "F1")
rownames(perf_mat2) <- "Logstic Regression2"


# Load dataset
admission <- read.csv("Admission_Predict.csv")
View(admission)

#Q1 
input_idx <- c(2,3,4,5,6,7,8)
target_idx <- 9

#Q2. Descriptive Statistics for each variable
#save mean, standard deviation, skewness, kurtosis in df
df <- data.frame(matrix(ncol=5, nrow=0))
colnames(df) <- c("variable", "mean", "sd", "skew", "kurtosis")
for (i in 2:8){
  a <- describe(admission[,i])
  df <- rbind(df,data.frame(variable = colnames(admission[i]),mean = a$mean,sd= a$sd , skew=a$skew ,kurtosis= a$kurtosis))
}
df
write.xlsx(df, file="admission_descriptive statistics.xlsx", sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)


#Boxplot for each variable
ggplot(admission, aes(x="", y=GRE.Score)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(admission, aes(x="", y=TOEFL.Score)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(admission, aes(x="", y=University.Rating)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(admission, aes(x="", y=SOP)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(admission, aes(x="", y=LOR)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(admission, aes(x="", y=CGPA)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(admission, aes(x="", y=Research)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()


#Q3. Remove Outliers
for (i in c(2,3,4,5,6,7,8)){
  admission <- admission[admission[,i] > quantile(admission[,i], .25) - 1.5*IQR(admission[,i]) & 
                           admission[,i] < quantile(admission[,i], .75) + 1.5*IQR(admission[,i]), ] #rows
}

#Q4. Scatterplot
pairs(admission[c(2,3,4,5,6,7,8)])

#correlation plot
add_cor <- cor(admission[c(2,3,4,5,6,7,8)], method=c("spearman"))
add_cor
corrplot(add_cor)


#Q5
#change chance_of_admission variable into binary variable
chance_of_Ad <- rep(0, length(admission[,9]))
chance_of_Ad[which(admission[,9] > 0.8)] <- 1
chance_of_Ad
admission[,9] <- chance_of_Ad


# Conduct the normalization
admission_input <- admission[,input_idx]
admission_input <- scale(admission_input, center = TRUE, scale = TRUE)
admission_target <- admission[,target_idx]
admission_data <- data.frame(admission_input, admission_target)




# Split the data into the training/validation sets
set.seed(92735)
trn_idx <- sample(1:nrow(admission_data), round(0.7*nrow(admission_data)))
admission_trn <- admission_data[trn_idx,]
admission_tst <- admission_data[-trn_idx,]

# Train the Logistic Regression Model with all variables
full_lr <- glm(admission_target ~ ., family=binomial, admission_trn)
summary(full_lr)

# Test the logistic regression model and calculate confusion matrix
lr_response <- predict(full_lr, type = "response", newdata = admission_tst)
lr_target <- admission_tst$admission_target
lr_predicted <- rep(0, length(lr_target))
lr_predicted[which(lr_response > 0.8)] <- 1
cm_full <- table(lr_target, lr_predicted)
cm_full

perf_mat[1,] <- perf_eval2(cm_full)
perf_mat


#Q7.AUROC
pred <- prediction(lr_response, lr_target)
roc <- performance(pred,"tpr","fpr")
plot(roc,colorize=TRUE)


x <- lr_response
y <- lr_target
x1 = x[y==1]; n1 = length(x1); 
x2 = x[y==0]; n2 = length(x2);
r = rank(c(x1,x2))  
auc = (sum(r[1:n1]) - n1*(n1+1)/2) / n1 / n2
auc

sum(0.9815331,  0.984127,0.9655052, 0.9756757, 0.9868243)/5


# Helpful function adapted from: https://stat.ethz.ch/pipermail/r-help/2005-September/079872.html


#-----------------------------------------------------------------------------------------------------

# Load dataset
diabetes <- read.csv("diabetes.csv")
View(diabetes)

#Q1 
input_idx2 <- c(1:8)
target_idx2 <- 9

#Q2. Descriptive Statistics for each variable
#save mean, standard deviation, skewness, kurtosis in df
df2 <- data.frame(matrix(ncol=5, nrow=0))
colnames(df2) <- c("variable", "mean", "sd", "skew", "kurtosis")
for (i in 1:8){
  a2 <- describe(diabetes[,i])
  df2 <- rbind(df2,data.frame(variable = colnames(diabetes[i]),mean = a2$mean,sd= a2$sd , skew=a2$skew ,kurtosis= a2$kurtosis))
}
df2
write.xlsx(df2, file="diabetes_descriptive statistics.xlsx", sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)


#Boxplot for each variable
ggplot(diabetes, aes(x="", y=Pregnancies)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(diabetes, aes(x="", y=Glucose)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(diabetes, aes(x="", y=BloodPressure)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(diabetes, aes(x="", y=SkinThickness)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(diabetes, aes(x="", y=Insulin)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(diabetes, aes(x="", y=BMI)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(diabetes, aes(x="", y=DiabetesPedigreeFunction)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(diabetes, aes(x="", y=Age)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()


#Q3. Remove Outliers
for (i in c(1:8)){
  diabetes <- diabetes[diabetes[,i] > quantile(diabetes[,i], .25) - 3*IQR(diabetes[,i]) & 
                           diabetes[,i] < quantile(diabetes[,i], .75) + 3*IQR(diabetes[,i]), ] #rows
}

#Q4. Scatterplot
pairs(diabetes[c(1:8)])

#correlation plot
add_cor2 <- cor(diabetes[c(1:8)], method=c("spearman"))
add_cor2
corrplot(add_cor2)


#Q5
# Conduct the normalization
diabetes_input <- diabetes[,input_idx2]
diabetes_input <- scale(diabetes_input, center = TRUE, scale = TRUE)
diabetes_target <- diabetes[,target_idx2]
diabetes_data <- data.frame(diabetes_input, diabetes_target)



# Split the data into the training/validation sets
set.seed(71526)
trn_idx2 <- sample(1:nrow(diabetes_data), round(0.7*nrow(diabetes_data)))
diabetes_trn <- diabetes_data[trn_idx2,]
diabetes_tst <- diabetes_data[-trn_idx2,]

# Train the Logistic Regression Model with all variables
full_lr2 <- glm(diabetes_target ~ ., family=binomial, diabetes_trn)
summary(full_lr2)

# Test the logistic regression model and calculate confusion matrix
lr_response2 <- predict(full_lr2, type = "response", newdata = diabetes_tst)
lr_target2 <- diabetes_tst$diabetes_target
lr_predicted2 <- rep(0, length(lr_target2))
lr_predicted2[which(lr_response2 > 0.5)] <- 1
cm_full2 <- table(lr_target2, lr_predicted2)
cm_full2

perf_mat2[1,] <- perf_eval2(cm_full)
perf_mat2



#Q7.AUROC
pred2 <- prediction(lr_response2, lr_target2)
roc2 <- performance(pred2,"tpr","fpr")
plot(roc2,colorize=TRUE)


x2 <- lr_response2
y2 <- lr_target2
x2_1 = x2[y2==1]; n2_1 = length(x2_1); 
x2_2 = x2[y2==0]; n2_2 = length(x2_2);
r2 = rank(c(x2_1,x2_2))  
auc2 = (sum(r2[1:n2_1]) - n2_1*(n2_1+1)/2) / n2_1 / n2_2
auc2

sum(0.7797203,	0.8438228,	0.8869464,	0.8105552,	0.8632381)/5

