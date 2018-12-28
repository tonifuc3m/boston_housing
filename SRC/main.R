setwd("C:/Users/Usuario1/Documents/R/win-library/3.4/Hackathon_deloitte")
library(data.table)
library(zoo)
library(dplyr)
library(stringr)
library(pracma) # for tic toc

#####
# Load
#####
train_data = as.data.table(read.csv("RAW/train.csv"))  # read csv file
test_data = as.data.table(read.csv("RAW/test.csv"))

all_data = data.frame(rbind(train_data %>% select(-SalePrice), 
                            test_data))

all_variable_names = colnames(train_data)
# Remove ID
all_data$Id <- NULL
train_data$Id <- NULL
test_data$Id <- NULL
#####
# Variables exploration
#####
str(train_data)

# Explore output variable
hist(train_data$SalePrice, breaks=100)

# Explore levels of factor variables
'
summary(train_data$SaleCondition)
summary(train_data$SaleType)
summary(train_data$MiscFeature)
summary(train_data$Fence)
summary(train_data$PoolQC)
summary(train_data$PavedDrive)
summary(train_data$GarageCond)
summary(train_data$GarageQual)
summary(train_data$GarageFinish)
summary(train_data$GarageType)
summary(train_data$FireplaceQu)
summary(train_data$Functional)
summary(train_data$KitchenQual)
summary(train_data$Electrical)
summary(train_data$CentralAir)
summary(train_data$HeatingQC)
summary(train_data$Heating)
summary(train_data$BsmtFinType2)
summary(train_data$BsmtFinType1)
summary(train_data$BsmtExposure)
summary(train_data$BsmtCond)
summary(train_data$BsmtQual)
summary(train_data$Foundation)
summary(train_data$ExterCond)
summary(train_data$ExterQual)
summary(train_data$MasVnrType)
summary(train_data$Exterior1st)
summary(train_data$Exterior2nd)
summary(train_data$RoofMatl)
summary(train_data$RoofStyle)
summary(train_data$HouseStyle)
summary(train_data$BldgType)
summary(train_data$Condition1)
summary(train_data$Condition2)
summary(train_data$Neighborhood)
summary(train_data$Utilities)
summary(train_data$LandContour)
summary(train_data$LotShape)
summary(train_data$Alley)
summary(train_data$Street)
summary(train_data$MSZoning)
'

# EDA of variables (time series)

#####
# NA removal
#####

# Columns of type factor and of type integer
factor_col_name <- names(all_data)[sapply(all_data, is.factor)]
factor_cols <- sapply(all_data, is.factor)
int_col_name <- names(all_data)[sapply(all_data, is.numeric)]
int_cols <- sapply(all_data, is.numeric)

# Columns with NAs
has_na = names(all_data)[sapply(all_data,
                                function(x) any(is.na(x)))]

# Replace NA by real values (mean or most common value)
all_data$LotFrontage[is.na(all_data$LotFrontage)] <- 
  mean(all_data$LotFrontage, na.rm = TRUE)
all_data$MasVnrArea[is.na(all_data$MasVnrArea)] <- 
  mean(all_data$MasVnrArea, na.rm = TRUE)
all_data$GarageYrBlt[is.na(all_data$GarageYrBlt)] <- 
  mean(all_data$GarageYrBlt, na.rm = TRUE)
all_data$BsmtFinSF1[is.na(all_data$BsmtFinSF1)] <- 
  mean(all_data$BsmtFinSF1, na.rm = TRUE)
all_data$BsmtFinSF2[is.na(all_data$BsmtFinSF2)] <- 
  mean(all_data$BsmtFinSF2, na.rm = TRUE)
all_data$BsmtUnfSF[is.na(all_data$BsmtUnfSF)] <- 
  mean(all_data$BsmtUnfSF, na.rm = TRUE)
all_data$TotalBsmtSF[is.na(all_data$TotalBsmtSF)] <- 
  mean(all_data$TotalBsmtSF, na.rm = TRUE)
all_data$BsmtFullBath[is.na(all_data$BsmtFullBath)] <- 
  mean(all_data$BsmtFullBath, na.rm = TRUE)
all_data$BsmtHalfBath[is.na(all_data$BsmtHalfBath)] <- 
  mean(all_data$BsmtHalfBath, na.rm = TRUE)
all_data$GarageArea[is.na(all_data$GarageArea)] <- 
  mean(all_data$GarageArea, na.rm = TRUE)
all_data$GarageCars[is.na(all_data$GarageCars)] <- 
  mean(all_data$GarageCars, na.rm = TRUE)

all_data$Electrical[is.na(all_data$Electrical)] <- 'SBrkr'
all_data$MasVnrType[is.na(all_data$MasVnrType)] <- 'None'
all_data$MSZoning[is.na(all_data$MSZoning)] <- 'RL'
all_data$Utilities[is.na(all_data$Utilities)] <- 'AllPub'
all_data$Exterior1st[is.na(all_data$Exterior1st)] <- 'VinylSd'
all_data$Exterior2nd[is.na(all_data$Exterior2nd)] <- 'VinylSd'
all_data$KitchenQual[is.na(all_data$KitchenQual)] <- 'TA'
all_data$Functional[is.na(all_data$Functional)] <- 'Typ'
all_data$SaleType[is.na(all_data$SaleType)] <- 'WD'


# Replace NA by their meaning
levels(all_data$Alley) = c(levels(all_data$Alley), "None")
all_data$Alley[is.na(all_data$Alley)] <- 'None'
levels(all_data$BsmtQual) = 
  c(levels(all_data$BsmtQual), "None")
all_data$BsmtQual[is.na(all_data$BsmtQual)] <- 'None'
levels(all_data$BsmtCond) = 
  c(levels(all_data$BsmtCond), "None")
all_data$BsmtCond[is.na(all_data$BsmtCond)] <- 'None'
levels(all_data$BsmtExposure) = 
  c(levels(all_data$BsmtExposure), "None")
all_data$BsmtExposure[is.na(all_data$BsmtExposure)] <- 'None'
levels(all_data$BsmtFinType1) = 
  c(levels(all_data$BsmtFinType1), "None")
all_data$BsmtFinType1[is.na(all_data$BsmtFinType1)] <- 'None'
levels(all_data$BsmtFinType2) = 
  c(levels(all_data$BsmtFinType2), "None")
all_data$BsmtFinType2[is.na(all_data$BsmtFinType2)] <- 'None'
levels(all_data$FireplaceQu) = 
  c(levels(all_data$FireplaceQu), "None")
all_data$FireplaceQu[is.na(all_data$FireplaceQu)] <- 'None'
levels(all_data$GarageType) = 
  c(levels(all_data$GarageType), "None")
all_data$GarageType[is.na(all_data$GarageType)] <- 'None'
levels(all_data$GarageFinish) = 
  c(levels(all_data$GarageFinish), "None")
all_data$GarageFinish[is.na(all_data$GarageFinish)] <- 'None'
levels(all_data$GarageQual) = 
  c(levels(all_data$GarageQual), "None")
all_data$GarageQual[is.na(all_data$GarageQual)] <- 'None'
levels(all_data$GarageCond) = 
  c(levels(all_data$GarageCond), "None")
all_data$GarageCond[is.na(all_data$GarageCond)] <- 'None'
levels(all_data$PoolQC) = c(levels(all_data$PoolQC), "None")
all_data$PoolQC[is.na(all_data$PoolQC)] <- 'None'
levels(all_data$Fence) = c(levels(all_data$Fence), "None")
all_data$Fence[is.na(all_data$Fence)] <- 'None'
levels(all_data$MiscFeature) = 
  c(levels(all_data$MiscFeature), "None")
all_data$MiscFeature[is.na(all_data$MiscFeature)] <- 'None'

# Re-divide the all_data datatable into train and test
train_data = cbind(all_data[1:1460,], 
                   train_data$SalePrice)
colnames(train_data)[dim(train_data)[2]] = "SalePrice"
test_data = all_data[1461:2919,]

#####
# Dummy variables and scaling
#####

## Dummy variables
# Get dummy variables
all.numeric <- 
  all_data[, names(all_data) %in% int_col_name]

all.factors <- 
  all_data[, !(names(all_data) %in% int_col_name)]
all.factors <- 
  all.factors[, names(all.factors) != 'SalePrice']

all.dummies <- 
  as.data.frame(model.matrix(~.-1, all.factors))

## Standardize columns of numeric data
all.scaled <- scale(all_data %>% select_if(is.numeric))

# check it is correctly scaled
colMeans(all.scaled)
apply(all.scaled, 2, sd)

# Re-divide the all_data datatable into train and test
all_data = cbind(all.scaled, all.dummies)
train_data = cbind(all.scaled[1:1460,], 
                   all.dummies[1:1460,],
                   train_data$SalePrice)
colnames(train_data)[dim(train_data)[2]] = "SalePrice"
test_data = cbind(all.scaled[1461:2919,], 
                  all.dummies[1461:2919,])

# Remove unuseful variables
rm(all.dummies, all.factors, all.numeric, all.scaled)

#####
# Remove variables highly correlated with others
#####
# Function that eliminates highly correlated variables
correlation_filter <- function(data, threshold){
  tmp <- cor(data)
  tmp[upper.tri(tmp)] <- 0
  diag(tmp) <- 0
  
  data.new <- 
    data[,!apply(tmp,2,
                 function(x) (any(x > threshold) | any(x < -threshold)))]
  return(data.new)
  
}

all_data = correlation_filter(all_data, 0.8)

# Re-divide the all_data datatable into train and test
train_data = cbind(all_data[1:1460,], train_data$SalePrice)
colnames(train_data)[dim(train_data)[2]] = "SalePrice"
test_data = all_data[1461:2919,]

#####
# First model: linear regression with a few correlated variables
#####

# Find highly correlated variables
corr_mat = cor(train_data)
num_var = 3
col = corr_mat[1:(dim(corr_mat)[1] - 1), dim(corr_mat)[1]]
highly_corr_var = names(col[order(-col)][1:num_var])

# Train model & do cross validation
library(caret)

train_control <- trainControl(method="cv", 
                              number=10, 
                              savePredictions = TRUE)
form_hc = paste0('SalePrice~',
              str_c(highly_corr_var, collapse = '+'))
tic(gcFirst=FALSE)
mdl.hc.cv <- train(as.formula(form_hc), 
                   data = train_data, 
                   method = "lm", 
                   trControl=train_control)
toc(echo=TRUE)
summary(mdl.hc.cv)
mdl.hc.cv$results$RMSE

# Make predictions using the test set
test_data$predictions_hc <- 
  unname(predict(mdl.hc.cv, 
                 test_data[, highly_corr_var]))

# Write CSV
aux = data.frame(cbind(1461:2919, test_data$predictions_hc))
colnames(aux) = c('Id', 'SalePrice')
write.csv(aux, file = "RESULTS/highly_corr_results.csv", 
          quote = FALSE, row.names = FALSE)

rm(aux)

#####
# Variable selection with Lasso
#####
library(glmnet)

preds = as.matrix(train_data %>% select(-SalePrice))
y = as.matrix(train_data$SalePrice)
cvfit <- glmnet::cv.glmnet(preds, y)
lasso_coef = coef(cvfit, s = "lambda.1se")

selected_variables = which(lasso_coef != 0)
selected_var_names = 
  rownames(lasso_coef)[selected_variables
                       [2:length(selected_variables)]]
selected_variables = (selected_variables - 1) #remove intercept


all_data = all_data[, (selected_variables)]
train_data = cbind(all_data[1:1460,],
                   train_data$SalePrice)
colnames(train_data)[dim(train_data)[2]] = "SalePrice"
test_data = cbind(all_data[1461:2919,], 
                  test_data$predictions_hc)
colnames(test_data)[dim(test_data)[2]] = "predictions_hc"

# Analyze which variables selected by Lasso are highly 
# correlated with the output
print(selected_var_names)
print(highly_corr_var)
which(selected_var_names %in% highly_corr_var)

#####
# Multi-layer Perceptron
#####

train_control <- trainControl(method="cv",
                              number=10, 
                              savePredictions = TRUE)
form_lasso = paste0('SalePrice~',
              str_c(colnames(all_data), collapse = '+'))

tic(gcFirst=FALSE)
mdl.mlp.cv <- train(as.formula(form_lasso), 
                    data = train_data, 
                    method = "mlp", 
                    trControl=train_control)
toc(echo=TRUE)
mdl.mlp.cv$results$RMSE

# Make predictions using the test set
test_data$predictions_mlp <- 
  unname(predict(mdl.mlp.cv, 
                 test_data[, selected_var_names]))

# Write CSV
aux = data.frame(cbind(1461:2919, test_data$predictions_mlp))
colnames(aux) = c('Id', 'SalePrice')
write.csv(aux, file = "RESULTS/mlp_results.csv", 
          quote = FALSE, row.names = FALSE)

rm(aux)

#####
# Neural Network: ERROR!!!
#####

'
train_control <- trainControl(method="cv", 
                              number=10, 
                              savePredictions = TRUE)
form_lasso = paste0('SalePrice~',
                    str_c(colnames(all_data), collapse = '+'))

tic(gcFirst=FALSE)
mdl.nn.cv <- train(as.formula(form_lasso),
                   data = train_data, 
                   method = "nnet", 
                   trControl=train_control)
toc(echo=TRUE)
mdl.nn.cv$results$RMSE

# Make predictions using the test set
test_data$predictions_nn <- 
  unname(predict(mdl.nn.cv, 
                 test_data[, selected_var_names]))

# Write CSV
aux = data.frame(cbind(1461:2919, test_data$predictions_nn))
colnames(aux) = c('Id', 'SalePrice')
write.csv(aux, file = "RESULTS/nn_results.csv", 
          quote = FALSE, row.names = FALSE)

'
#####
# Random forest
#####

train_control <- trainControl(method="cv", 
                              number=10, 
                              savePredictions = TRUE)
form_lasso = paste0('SalePrice~',
                    str_c(colnames(all_data), collapse = '+'))

tic(gcFirst=FALSE)
mdl.rf.cv <- train(as.formula(form_hc),
                    data = train_data, 
                    method = "rf", 
                    trControl=train_control)
toc(echo=TRUE)
mdl.rf.cv$results$RMSE

# Make predictions using the test set
test_data$predictions_rf <- 
  unname(predict(mdl.rf.cv, 
                 test_data[, selected_var_names]))

# Write CSV
aux = data.frame(cbind(1461:2919, test_data$predictions_rf))
colnames(aux) = c('Id', 'SalePrice')
write.csv(aux, file = "RESULTS/rf_results.csv", 
          quote = FALSE, row.names = FALSE)

#####
# Gradient Boosting Machine (gbm)
#####

train_control <- trainControl(method="cv", 
                              number=10, 
                              savePredictions = TRUE)
form_lasso = paste0('SalePrice~',
                    str_c(colnames(all_data), collapse = '+'))

tic(gcFirst=FALSE)
mdl.gbm.cv <- train(as.formula(form_hc),
                   data = train_data, 
                   method = "gbm", 
                   trControl=train_control)
toc(echo=TRUE)
mdl.gbm.cv$results$RMSE

# Make predictions using the test set
test_data$predictions_gbm <- 
  unname(predict(mdl.gbm.cv, 
                 test_data[, selected_var_names]))

# Write CSV
aux = data.frame(cbind(1461:2919, test_data$predictions_gbm))
colnames(aux) = c('Id', 'SalePrice')
write.csv(aux, file = "RESULTS/gbm_results.csv", 
          quote = FALSE, row.names = FALSE)

#####
# Tune the models!
#####

# Check instead of with lasso variables,
# only with highly correlated variables

# Check this link https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/