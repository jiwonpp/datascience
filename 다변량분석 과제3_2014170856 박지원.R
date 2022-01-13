##MDA Assignment#3 MLR for House price data 2014170856
library(psych) 
library(ggplot2)
library(corrplot)
library(moments)

# Performance evaluation function for regression --------------------------
perf_eval_reg <- function(tgt_y, pre_y){
  
  # RMSE
  rmse <- sqrt(mean((tgt_y - pre_y)^2))
  # MAE
  mae <- mean(abs(tgt_y - pre_y))
  # MAPE
  mape <- 100*mean(abs((tgt_y - pre_y)/tgt_y))
  
  return(c(rmse, mae, mape))
  
}

perf_mat <- matrix(0, nrow = 1, ncol = 3)

# Initialize a performance summary
rownames(perf_mat) <- c("kc_house")
colnames(perf_mat) <- c("RMSE", "MAE", "MAPE")
perf_mat


#Load the dataset
house <- read.csv("kc_house_data.csv")

#Split house sold date into year, month, and date
house_soldyr <- as.numeric(substr(house[,2], 1, 4))
house_soldmnth <- as.numeric(substr(house[,2], 5, 6))
house_solddate <- as.numeric(substr(house[,2], 7, 8))

#Q1. Eliminate useless variable and add sold date
house1 <- cbind(house[,-c(1,2,17)], house_soldyr, house_soldmnth, house_solddate)

#Q2. Descriptive Statistics for each variable
#save mean, standard deviation, skewness, kurtosis in df
df <- data.frame(matrix(ncol=5, nrow=0))
colnames(df) <- c("variable", "mean", "sd", "skew", "kurtosis")
for (i in 1:21){
  a <- describe(house1[,i])
  df <- rbind(df,data.frame(variable = colnames(house1[i]),mean = a$mean,sd= a$sd , skew=a$skew ,kurtosis= a$kurtosis))
}
df

#Boxplot for each variable
ggplot(house1, aes(x="", y=price)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(house1, aes(x="", y=bedrooms)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(house1, aes(x="", y=bathrooms)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(house1, aes(x="", y=sqft_living)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(house1, aes(x="", y=sqft_lot)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(house1, aes(x="", y=floors)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(house1, aes(x="", y=waterfront)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(house1, aes(x="", y=view)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(house1, aes(x="", y=condition)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(house1, aes(x="", y=grade)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(house1, aes(x="", y=sqft_above)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(house1, aes(x="", y=sqft_basement)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(house1, aes(x="", y=yr_built)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(house1, aes(x="", y=yr_renovated)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(house1, aes(x="", y=lat)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(house1, aes(x="", y=long)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(house1, aes(x="", y=sqft_living15)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(house1, aes(x="", y=sqft_lot15)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(house1, aes(x="", y=house_soldyr)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(house1, aes(x="", y=house_soldmnth)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()
ggplot(house1, aes(x="", y=house_solddate)) +geom_boxplot(fill='#E69F00', color="black")+theme_classic()


#Q3. Remove Outliers
for (i in c(2,3,4,5,11,12,15,16,17,18)){
  house1 <- house1[house1[,i] > quantile(house1[,i], .25) - 3*IQR(house1[,i]) & 
                     house1[,i] < quantile(house1[,i], .75) + 3*IQR(house1[,i]), ] #rows
}

#Q4. Scatterplot
pairs(house1[c(2,3,4,5,6,9,10,11,12,13,14,15)])

#correlation plot
house_cor <- cor(house1[c(2,3,4,5,6,8,9,10,11,12,13,14,15,16,17,18)], method=c("spearman"))
house_cor


#Q5. MLR model
##Continuous variables to factor variables 
house1$house_soldyr <- as.factor(house1$house_soldyr)
house1$house_soldmnth <- as.factor(house1$house_soldmnth)
house1$house_solddate <- as.factor(house1$house_solddate)
house1$waterfront <- as.factor(house1$waterfront)

#Split the data into the training/validation sets
nHome <- nrow(house1)
nVar <- ncol(house1)
house_trn_idx <- sample(1:nHome, round(0.7*nHome))
house_trn_data <- house1[house_trn_idx,]
house_val_data <- house1[-house_trn_idx,]

# Train the MLR
mlr_house <- lm(price ~ ., data = house_trn_data)
mlr_house
summary(mlr_house)
plot(mlr_house)


# normality test of residuals
house_resid <- resid(mlr_house)

m <- mean(house_resid)
std <- sqrt(var(house_resid))

hist(house_resid, density=20, breaks=50, prob=TRUE, 
     xlab="x-variable", main="normal curve over histogram")

curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

skewness(house_resid)
kurtosis(house_resid)

#Q7. Performance Measure
mlr_house_haty <- predict(mlr_house, newdata = house_val_data)

perf_mat[1,] <- perf_eval_reg(house_val_data$price, mlr_house_haty)
perf_mat


#Q8. Select 7 variables
house2 <- house1[,c(1, 4, 7 ,8, 9, 10,13, 15 )]

#Q9. MLR model with 7 variables
# Split the data into the training/validation sets
nHome2 <- nrow(house2)
nVar2 <- ncol(house2)
house_trn_idx2 <- sample(1:nHome2, round(0.7*nHome2))
house_trn_data2 <- house2[house_trn_idx2,]
house_val_data2 <- house2[-house_trn_idx2,]
# Train the MLR
mlr_house2 <- lm(price ~ ., data = house_trn_data2)
mlr_house2
summary(mlr_house2)
plot(mlr_house2)

perf_mat2 <- matrix(0, nrow = 1, ncol = 3)

# Initialize a performance summary
rownames(perf_mat2) <- c("kc_house")
colnames(perf_mat2) <- c("RMSE", "MAE", "MAPE")
perf_mat2

# Performance Measure
mlr_house_haty2 <- predict(mlr_house2, newdata = house_val_data2)

perf_mat2[1,] <- perf_eval_reg(house_val_data2$price, mlr_house_haty2)
perf_mat2


