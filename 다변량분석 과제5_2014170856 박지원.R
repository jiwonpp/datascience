##MDA Assignment #5 Dimensionality Reduction
# Install necessary packages
# glmnet: Ridge, Lasso, Elastic Net Logistic Regression 
# GA: genetic algorithm
library(glmnet)
library(GA)
library(corrgram)
library(corrplot)
library(moments)
library(gtools)

# Performance Evaluation Function -----------------------------------------
perf_eval<- function(tgt_y, pre_y){
  
  # RMSE
  rmse <- sqrt(mean((tgt_y - pre_y)^2))
  # MAE
  mae <- mean(abs(tgt_y - pre_y))
  # MAPE
  mape <- 100*mean(abs((tgt_y - pre_y)/tgt_y))
  
  return(c(rmse, mae, mape))
  
}
Perf_Table <- matrix(0, nrow = 6, ncol = 3)
rownames(Perf_Table) <- c("All", "Exhaustive Search",  "Forward", "Backward", "Stepwise", "GA")
colnames(Perf_Table) <- c("RMSE", "MAE", "MAPE")



# Load the dataset & Preprocessing
weather <- read.csv("Weather_Ankara.csv")
str(weather)


#Scatterplot
pairs(weather[c(1:9)])

#correlation plot
weather_cor <- cor(weather, method=c("spearman"))
weather_cor
corrplot(weather_cor,tl.col = "black")


weather_input <- weather[,1:9]
weather_input_scaled <- scale(weather_input, center = TRUE, scale = TRUE)
weather_target <- weather$Mean_temperature

weather_data_scaled <- data.frame(weather_input_scaled, weather_target)


#Separate Training and Test Dataset
set.seed(28352)
trn_idx <- sample(1:length(weather_target), round(0.78*length(weather_target)))
trnInputs <- weather_input_scaled[trn_idx,]
trnTargets <- weather_target[trn_idx]
valInputs <- weather_input_scaled[-trn_idx,]
valTargets <- weather_target[-trn_idx]

weather_trn <- weather_data_scaled[trn_idx,]
weather_tst <- weather_data_scaled[-trn_idx,]


######################################################################################################################################
# Variable selection method 0: Logistic Regression with all variables
full_model <- lm(weather_target ~ ., data = weather_trn)
full_model

plot(full_model)
summary(full_model)

full_model_coeff <- as.matrix(full_model$coefficients, 10, 1)
full_model_coeff

# Make prediction
full_model_pred <- predict(full_model, type = "response", newdata = weather_tst)

# Peformance evaluation
Perf_Table[1,] <- perf_eval(weather_tst$weather_target, full_model_pred)
Perf_Table


######################################################################################################################################
# Variable selection method 1: Exhausitve Search
x_idx <- c(1:9)
bestR2 <- 0

start_time <- proc.time()
for (i in 1:9){
  comb <- combinations(9,i,x_idx)
  for (j in 1:dim(comb)[1]){
    x_idx_selected <- comb[j,]
    tmp_x <- paste(colnames(weather_trn[x_idx_selected]), collapse=" + ")
    tmp_xy <- paste("weather_target ~ ", tmp_x, collapse = "")
    as.formula(tmp_xy)
    
    es_model <- lm(tmp_xy, data = weather_trn)
    s <- summary(es_model)
    R2_es <- s$adj.r.squared
    
    if(R2_es >= bestR2){
      bestR2 <- R2_es
      best_formula <- tmp_xy
       }
  }
}
end_time <- proc.time()
end_time - start_time


bestR2
best_formula

es_model <- lm(best_formula, data = weather_trn)
summary(es_model)
# Make prediction
es_model_pred <- predict(es_model, type = "response", newdata = weather_tst)

# Peformance evaluation
Perf_Table[2,] <- perf_eval(weather_tst$weather_target, es_model_pred)
Perf_Table




######################################################################################################################################
# Variable selection method 2: Forward selection
tmp_x <- paste(colnames(weather_trn)[-10], collapse=" + ")
tmp_xy <- paste("weather_target ~ ", tmp_x, collapse = "")
as.formula(tmp_xy)



start_time <- proc.time()
forward_model <- step(lm(weather_target ~ 1, data = weather_trn), 
                      scope = list(upper = as.formula(tmp_xy), lower = weather_target ~ 1), 
                      direction="forward", trace = 1)
end_time <- proc.time()
end_time - start_time


summary(forward_model)
forward_model_coeff <- as.matrix(forward_model$coefficients, 10, 1)
forward_model_coeff

# Make prediction
forward_model_pred <- predict(forward_model, type = "response", newdata = weather_tst)

# Peformance evaluation
Perf_Table[3,] <- perf_eval(weather_tst$weather_target, forward_model_pred)
Perf_Table

######################################################################################################################################
# Variable selection method 3: Backward elimination
tmp_x <- paste(colnames(weather_trn)[-10], collapse=" + ")
tmp_xy <- paste("weather_target ~ ", tmp_x, collapse = "")
as.formula(tmp_xy)

start_time <- proc.time()
backward_model <- step(full_model, 
                       scope = list(upper = as.formula(tmp_xy), lower = weather_target ~ 1),
                       direction = "backward", trace = 1)
end_time <- proc.time()
end_time - start_time

summary(backward_model)
backward_model_coeff <- as.matrix(backward_model$coefficients, 10, 1)
backward_model_coeff

# Make prediction
backward_model_pred <- predict(backward_model, type = "response", newdata = weather_tst)

# Peformance evaluation
Perf_Table[4,] <- perf_eval(weather_tst$weather_target, backward_model_pred)
Perf_Table

######################################################################################################################################
# Variable selection method 4: Stepwise selection
tmp_x <- paste(colnames(weather_trn)[-10], collapse=" + ")
tmp_xy <- paste("weather_target ~ ", tmp_x, collapse = "")
as.formula(tmp_xy)


start_time <- proc.time()

stepwise_model <- step(lm(weather_target ~ 1, data = weather_trn), 
                       scope = list(upper = as.formula(tmp_xy), lower = weather_target ~ 1), 
                       direction="both", trace = 1)

end_time <- proc.time()
end_time - start_time


summary(stepwise_model)
stepwise_model_coeff <- as.matrix(stepwise_model$coefficients, 10, 1)
stepwise_model_coeff


# Make prediction
stepwise_model_pred <- predict(stepwise_model, type = "response", newdata = weather_tst)

# Peformance evaluation
Perf_Table[5,] <- perf_eval(weather_tst$weather_target, stepwise_model_pred)
Perf_Table


######################################################################################################################################
# Variable selection method 5: Genetic Algorithm
# Fitness function: F1 for the training dataset
fit_F1 <- function(string){
  sel_var_idx <- which(string == 1)
  # Use variables whose gene value is 1
  sel_x <- x[, sel_var_idx]
  xy <- data.frame(sel_x, y)
  # Training the model
  GA_lr <- lm(y ~ ., data = xy)
  s <- summary(GA_lr)
  R2 <- s$adj.r.squared
  #GA_lr_pred <- predict(GA_lr, type = "response", newdata = xy)
  
  return(R2)
}

x <- as.matrix(weather_trn[,-10])
y <- weather_trn[,10]

# Variable selection by Genetic Algorithm
start_time <- proc.time()
GA_F1 <- ga(type = "binary", fitness = fit_F1, nBits = ncol(x), 
            names = colnames(x), popSize = 100, pcrossover = 0.5, 
            pmutation = 0.01, maxiter = 100, elitism = 2, seed = 123)
end_time <- proc.time()
end_time - start_time

best_var_idx <- which(GA_F1@solution == 1)

# Model training based on the best variable subset
GA_trn_data <- weather_trn[,c(best_var_idx, 10)]
GA_tst_data <- weather_tst[,c(best_var_idx, 10)]

GA_model <- lm(weather_target ~ ., GA_trn_data)
summary(GA_model)
GA_model_coeff <- as.matrix(GA_model$coefficients, 10, 1)
GA_model_coeff

# Make prediction
GA_model_pred <- predict(GA_model, type = "response", newdata = GA_tst_data)


# Peformance evaluation
Perf_Table[6,] <- perf_eval(GA_tst_data$weather_target, GA_model_pred)
Perf_Table




######################################################################################################################################
# Genetic Algorithm: Change in parameters
x <- as.matrix(weather_trn[,-10])
y <- weather_trn[,10]

# Variable selection by Genetic Algorithm
GA_F1 <- ga(type = "binary", fitness = fit_F1, nBits = ncol(x), 
            names = colnames(x), popSize = 100, pcrossover = 0.5, 
            pmutation = 0.01, maxiter = 200, seed = 357)

best_var_idx <- which(GA_F1@solution == 1)
best_var_idx


