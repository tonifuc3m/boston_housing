---
title: "Iowa House Prices dataset"
output:
    html_document:
      keep_md: true
      
---

First we will load the required packages and custom-made functions.

Note that I set my own working directory (I am using Windows), and my custom-made functions are inside a sub-folder in a file called "utils.R".


```r
setwd("C:/Users/toni3/Documents/Development/R_projects/Kaggle/kaggle_iowa_housing")

packages_needed<-c("data.table", "dplyr", "tidyr", "stringr" ,"pracma", "skimr", "ggplot2", "caret", "glmnet", "doParallel", "xgboost")

lapply(packages_needed, require, character.only = TRUE)
source("SRC/utils.R")
```

<br /> 

### Load data from Kaggle csv

I am using the dataset from https://www.kaggle.com/c/iowa-house-price-prediction

We are going to load the train and test data, and to merge both datasets, so that some preprocessing, like ignore some attributes or change the name of a value of another attribute (based only on information from the train dataset), is applied in both data frames.


```r
train_data = data.table::as.data.table(read.csv("RAW/train.csv"))
test_data = data.table::as.data.table(read.csv("RAW/test.csv"))
train_data$Id <- NULL
test_data$Id <- NULL

all_data = data.frame(rbind(train_data %>% select(-SalePrice), 
                            test_data))
```

<br /> 

### Quick data exploration

The output of the skimr package is not thought to be output in a Markdown document. Therefore, the output of this chunk is not shown. It serves us to rapidly know the number of missing values per column, a low-quality histogram, the mean, median, etc. With this info we will decide the preprocessing to be applied. 


```r
#skimr::skim(train_data)
```

<br /> 


<br /> 

#### Closer look at some variables

Some numeric variables showed a skewed or a discrete histogram. We will take a closer look at them to decide what to do with them.

If the code is run on the console, a warning appears indicating that "Removed 267 rows containing non-finite values". However, we should not worry about it, because the real cause is that some variables have a very long tail, and they are not shown in the histograms. In our problem, since this is a quick description of the variables, we should not worry about this issue. 

```r
close_look <- c("BsmtFinSF1", "BsmtFinSF2", "BsmtFullBath", "BsmtHalfBath", "EnclosedPorch", 
                "Fireplaces", "FullBath", "GarageCars", "HalfBath", "KitchenAbvGr", 
                "LotArea", "LotFrontage", "LowQualFinSF", "MasVnrArea", "MiscVal",
                "PoolArea", "ScreenPorch", "WoodDeckSF", "X3SsnPorch", "SalePrice")
close_look_data <- train_data %>% subset(., select=close_look)
ggplot2::ggplot(gather(close_look_data), aes(value)) + 
    geom_histogram(bins = 30) + 
    facet_wrap(~key, scales = 'free_x')
```

![](iowa_housing_files/figure-html/closer_look-1.png)<!-- -->

<br />

### Preprocessing

#### Numerical to categorical

In the histograms, we realized that some numeric variables are actually categorical, since they have discrete numeric values. We will cast them to factor. 


```r
num2cat <- c("BsmtFullBath","BsmtHalfBath", "Fireplaces", "FullBath", "GarageCars", "HalfBath", "KitchenAbvGr")

all_data[names(t) %in% num2cat] <- lapply(all_data[names(t) %in% num2cat], factor)
```

<br /> 


#### Remove those with no variance

Also, some numeric variables present 90% (or even more) of the entries in a specific value. We will remove them since some of the info of this variables is already contained in other categorical variables. For example, PoolArea represents the area of the pool of a house. But since most houses do not have a pool, almost all values contain a zero. And we already have a categorical variable stating whether a house has a pool or not. 

In a more cautious analysis the variables I will remove should be inspected more carefully, but we will skip that step and directly remove them. 

```r
## PoolArea not info (and we already have a categorical one)
rm_no_variance <- c("PoolArea", "X3SsnPorch","BsmtFinSF2", "EnclosedPorch", "LowQualFinSF", "MiscVal", "WoodDeckSF")

all_data[(names(all_data) %in% rm_no_variance)] <- NULL
```


```r
# Re-divide the all_data datatable into train and test
train_data = cbind(all_data[1:1460,], 
                   train_data$SalePrice)
colnames(train_data)[dim(train_data)[2]] = "SalePrice"
test_data = all_data[1461:2919,]
```

<br />

#### NA treatment

NAs are always an interesting part of the preprocessing. 
In this notebook I have followed 4 strategies to treat them:

1/ Remove columns with lots of NAs

2/ Replace NAs by their actual meaning: sometimes, a NA appears but it actually means a zero or None. For example, there are tons of instances with NAs in the attribute "PoolQC". And they correspond to the Houses without a pool (I cross-checked this previously with PoolArea variable).

3/ Replace NAs by the mode of that attribute. Sometimes NAs are impossible to treat. In this cases, a common approach is to replace them by the most common value to minimize the damage they produce. Obviously, this most common value is computed in the train set. I do this in the categorical variables.

4/ Replace NAs by the mean of that attribute. When we have numeric attributes, instead of chosing the mode, we should choose the mean of the median (in this notebook I will use the mean because it is less expensive computationally, despite this is not an issue with our small dataset).


```r
# 1/ Remove Columns with lots of NAs
att_rm = c('Alley', 'Fence', 'FireplaceQu')
all_data[(names(all_data) %in% att_rm)] <- NULL

# 2/ Replace NA by their meaning
na2none_list <- c("PoolQC", "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", 
                  "BsmtFinType2", "GarageType", "GarageFinish", 
                  "GarageQual", "GarageCond", "MiscFeature")

for (i in (na2none_list)){
  c = names(all_data)
  pos <- which(c %in% i)
  levels(all_data[[pos]]) <- c(levels(all_data[[pos]]), "None")
  all_data[[pos]][is.na(all_data[[pos]])] <- "None"
}

factor.names <- names(all_data %>% select(-one_of(na2none_list)))[sapply(all_data %>% select(-one_of(na2none_list)), is.factor)]
numeric.names <- names(all_data %>% select(-one_of(na2none_list)))[sapply(all_data %>% select(-one_of(na2none_list)), is.numeric)]

# 3/ Assign NA entries to the most common category
for (i in factor.names){
  all_data <- na2mode(train_data, all_data, i)
}

# 4/ Replace NA by mean (all nummerical attribute)
for (i in numeric.names){
  all_data <- na2mean(train_data, all_data, i)
}
```


```r
# Re-divide the all_data datatable into train and test
train_data = cbind(all_data[1:1460,], 
                   train_data$SalePrice)
colnames(train_data)[dim(train_data)[2]] = "SalePrice"
test_data = all_data[1461:2919,]
```

<br /> 

#### Get dummies and scale

Once we have our dataset clean, there is one last step before we can proceed to train models. Categorical variables need to be decomposed in to dummy variables and numerical ones need to be scaled.


```r
# Get factor variables
all.factors <- 
  all_data[, names(all_data)[sapply(all_data, is.factor)]]


# Dummies from factors
all.dummies <- 
  as.data.frame(model.matrix(~.-1, all.factors))
```

The scaling I have used is to have zero mean and unit standard deviation. Then, I will substract the mean of the variables in the train set and divide by the standard deviation.

```r
# Get numeric columns 
all.numeric <- 
  all_data[, names(all_data)[sapply(all_data, is.numeric)]]

train.numeric <- 
  train_data[, names(train_data)[sapply(train_data, is.numeric)]]
train.numeric <- 
  train.numeric[, names(train.numeric) != 'SalePrice']

# Standardize numeric data (use means and standard deviations from train data!)
x_mean <- colMeans(train.numeric)
stdev <- apply(train.numeric, 2, sd)
all.center = sweep(all.numeric, 2, x_mean, "-")
all.scaled = sweep(all.center, 2, stdev, "/")
```

We can check the scaling is correct: 

```r
colMeans(all.scaled)
```

```
##    MSSubClass   LotFrontage       LotArea   OverallQual   OverallCond 
##  0.0056845125 -0.0281630361 -0.0349368546 -0.0074067203 -0.0096743011 
##     YearBuilt  YearRemodAdd    MasVnrArea    BsmtFinSF1     BsmtUnfSF 
##  0.0014889340 -0.0291241192 -0.0081534261 -0.0048580155 -0.0146335696 
##   TotalBsmtSF     X1stFlrSF     X2ndFlrSF     GrLivArea  BsmtFullBath 
## -0.0128786411 -0.0078766240 -0.0240734339 -0.0279817284  0.0087647896 
##  BsmtHalfBath      FullBath      HalfBath  BedroomAbvGr  KitchenAbvGr 
##  0.0160314228  0.0053261270 -0.0051890503 -0.0076151245 -0.0092564183 
##  TotRmsAbvGrd    Fireplaces   GarageYrBlt    GarageCars    GarageArea 
## -0.0407801145 -0.0246505740 -0.0154770047 -0.0006719286 -0.0004935772 
##   OpenPorchSF   ScreenPorch        MoSold        YrSold 
##  0.0124748888  0.0179597855 -0.0402537652 -0.0173302240
```

```r
apply(all.scaled, 2, sd)
```

```
##   MSSubClass  LotFrontage      LotArea  OverallQual  OverallCond 
##    1.0051313    0.9677682    0.7901800    1.0194871    1.0002978 
##    YearBuilt YearRemodAdd   MasVnrArea   BsmtFinSF1    BsmtUnfSF 
##    1.0029314    1.0120578    0.9892397    0.9987605    0.9945717 
##  TotalBsmtSF    X1stFlrSF    X2ndFlrSF    GrLivArea BsmtFullBath 
##    1.0045256    1.0149367    0.9820699    0.9630256    1.0108789 
## BsmtHalfBath     FullBath     HalfBath BedroomAbvGr KitchenAbvGr 
##    1.0286911    1.0037274    0.9999726    1.0084766    0.9733310 
## TotRmsAbvGrd   Fireplaces  GarageYrBlt   GarageCars   GarageArea 
##    0.9655381    1.0022693    1.0363973    1.0189730    1.0072639 
##  OpenPorchSF  ScreenPorch       MoSold       YrSold 
##    1.0199147    1.0076573    1.0041188    0.9901132
```

<br />


```r
# Re-divide the all_data datatable into train and test
all_data = cbind(all.scaled, all.dummies)
train_data = cbind(all.scaled[1:1460,], 
                   all.dummies[1:1460,],
                   train_data$SalePrice)
colnames(train_data)[dim(train_data)[2]] = "SalePrice"
test_data = cbind(all.scaled[1461:2919,], 
                  all.dummies[1461:2919,])

# Remove unuseful variables
rm(all.dummies, all.factors, all.numeric, all.scaled, all.center,
   train.numeric, close_look_data)
```

<br />

#### Remove highly correlated variables

Some columns contain redundant information. This is common when we have a variable with the Year and another with the Age, for example. We will apply a custom-made function to replace any variable highly correlated with others. 

I have set the threshold to 0.9.

```r
#####
# Remove variables highly correlated with others
#####
all_data = correlation_filter(all_data, 0.9)
```

<br />


```r
# Re-divide the all_data datatable into train and test
train_data = cbind(all_data[1:1460,], train_data$SalePrice)
colnames(train_data)[dim(train_data)[2]] = "SalePrice"
test_data = all_data[1461:2919,]
```


<br />

## Model testing

After some preprocessing, we are finally here! It is time to play with our models. 

In the further sub-sections I will present code to train a simple linear regression, a Multilayer Perceptron (bit old-fashioned, but I have always like it), a Random Forest and Gradient Boosting Machine. We will not use hyper-parameter tuning, yet, and the BGM is a simple implementation, not a complicated XGBoost of CATGBoost. I will leave these advance techniques for the next steps. 

### First model: linear regression with a few correlated variables

To use a simple linear regression, I will regress only on a few predictors. They are selected based on their correlation with the output variable (in the train set, obviously).

I acknowledge it makes more sense to correlate with the log of the output variable, but for simplicity I have not done it. 

```r
set.seed(0)

# Find highly correlated variables with the output
corr_mat = cor(train_data)
num_var = 3
col = corr_mat[1:(dim(corr_mat)[1] - 1), dim(corr_mat)[1]]
highly_corr_var = names(col[order(-col)][1:num_var])

# Train model & do cross validation
train_control <- caret::trainControl(method="cv", 
                              number=5, 
                              savePredictions = TRUE)
form_hc = paste0('SalePrice~',
              str_c(highly_corr_var, collapse = '+'))
tic(gcFirst=FALSE)
mdl.hc.cv <- caret::train(as.formula(form_hc), 
                   data = train_data, 
                   method = "lm", 
                   trControl=train_control)
toc(echo=TRUE)
```

```
## elapsed time is 0.520000 seconds
```

```r
print(mdl.hc.cv)
```

```
## Linear Regression 
## 
## 1460 samples
##    3 predictor
## 
## No pre-processing
## Resampling: Cross-Validated (5 fold) 
## Summary of sample sizes: 1169, 1168, 1167, 1168, 1168 
## Resampling results:
## 
##   RMSE      Rsquared   MAE     
##   40876.09  0.7398328  27547.19
## 
## Tuning parameter 'intercept' was held constant at a value of TRUE
```

```r
print(summary(mdl.hc.cv))
```

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -340718  -21675   -2085   19500  300177 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   180921       1063  170.18   <2e-16 ***
## OverallQual    37486       1483   25.28   <2e-16 ***
## GrLivArea      26628       1341   19.86   <2e-16 ***
## GarageCars     15917       1350   11.79   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 40620 on 1456 degrees of freedom
## Multiple R-squared:  0.7391,	Adjusted R-squared:  0.7385 
## F-statistic:  1375 on 3 and 1456 DF,  p-value: < 2.2e-16
```

```r
print(mdl.hc.cv$results$RMSE)
```

```
## [1] 40876.09
```
Out Adjusted R2 is 0.7, not bad! 
And the RMSE skyrockets around 405K. We will try to reduce it using other models!

Finally, predict using the test set and submit those predictions to Kaggle

```r
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
```


<br />

## Variable selection

To test more complex models we will use only some of the variables of the dataset, but they will be chosen in a different way: we will use Lasso regression. 
Lasso regression is a linear regression techniques that adds a regularization term. This constraints the search space for the coefficients of the linear regression and many of them are set to zero. 
It is assumed that those coefficients that go to zero correspond to non-informative variables, and thus we should not use them to build our model. 

```r
set.seed(0)

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
```

```
##  [1] "OverallQual"         "YearBuilt"           "BsmtFinSF1"         
##  [4] "TotalBsmtSF"         "X1stFlrSF"           "GrLivArea"          
##  [7] "GarageCars"          "GarageArea"          "NeighborhoodNridgHt"
## [10] "ExterQualTA"
```

```r
print(highly_corr_var)
```

```
## [1] "OverallQual" "GrLivArea"   "GarageCars"
```

```r
which(selected_var_names %in% highly_corr_var)
```

```
## [1] 1 6 7
```

<br />

## Further Model testing

### MLP

```r
set.seed(0)
train_control <- caret::trainControl(method="cv",
                              number=5, 
                              savePredictions = TRUE)
form_lasso = paste0('SalePrice~', str_c(colnames(all_data), collapse = '+'))

tic(gcFirst=FALSE)
mdl.mlp.cv <- caret::train(as.formula(form_lasso), 
                    data = train_data, 
                    method = "mlp", 
                    metric="RMSE",
                    trControl=train_control, 
                    verbose = FALSE)
toc(echo=TRUE)
```

```
## elapsed time is 4.030000 seconds
```

```r
print(mdl.mlp.cv)
```

```
## Multi-Layer Perceptron 
## 
## 1460 samples
##   10 predictor
## 
## No pre-processing
## Resampling: Cross-Validated (5 fold) 
## Summary of sample sizes: 1169, 1168, 1167, 1168, 1168 
## Resampling results across tuning parameters:
## 
##   size  RMSE       Rsquared   MAE      
##   1     124487.21  0.1935925   87530.71
##   3     119216.96  0.3127180  102027.83
##   5      94466.07  0.2737637   72641.34
## 
## RMSE was used to select the optimal model using the smallest value.
## The final value used for the model was size = 5.
```

```r
#print(summary(mdl.mlp.cv))
print(mdl.mlp.cv$results$RMSE)
```

```
## [1] 124487.21 119216.96  94466.07
```
From the first lines of the summary we see how simple our MLP is. 


Finally, predict using the test set and submit those predictions to Kaggle

```r
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
```

<br />

### Random Forest


```r
set.seed(0)
train_control <- caret::trainControl(method="cv", 
                              number=5, 
                              savePredictions = TRUE)
form_lasso = paste0('SalePrice~',
                    str_c(colnames(all_data), collapse = '+'))

tic(gcFirst=FALSE)
mdl.rf.cv <- caret::train(as.formula(form_lasso),
                   data = train_data, 
                   method = "rf", 
                   metric="RMSE", 
                   trControl=train_control,
                   verbose = FALSE)
toc(echo=TRUE)
```

```
## elapsed time is 40.190000 seconds
```

```r
print(mdl.rf.cv)
```

```
## Random Forest 
## 
## 1460 samples
##   10 predictor
## 
## No pre-processing
## Resampling: Cross-Validated (5 fold) 
## Summary of sample sizes: 1169, 1168, 1167, 1168, 1168 
## Resampling results across tuning parameters:
## 
##   mtry  RMSE      Rsquared   MAE     
##    2    32013.13  0.8422857  19284.36
##    6    31662.45  0.8441480  18717.94
##   10    32541.81  0.8357873  19065.48
## 
## RMSE was used to select the optimal model using the smallest value.
## The final value used for the model was mtry = 6.
```

```r
print(summary(mdl.rf.cv))
```

```
##                 Length Class      Mode     
## call               5   -none-     call     
## type               1   -none-     character
## predicted       1460   -none-     numeric  
## mse              500   -none-     numeric  
## rsq              500   -none-     numeric  
## oob.times       1460   -none-     numeric  
## importance        10   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            11   -none-     list     
## coefs              0   -none-     NULL     
## y               1460   -none-     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames            10   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          1   -none-     logical  
## param              1   -none-     list
```

```r
print(mdl.rf.cv$results$RMSE)
```

```
## [1] 32013.13 31662.45 32541.81
```

Finally, predict using the test set and submit those predictions to Kaggle

```r
# Make predictions using the test set
test_data$predictions_rf <- 
  unname(predict(mdl.rf.cv, 
                 test_data[, selected_var_names]))

# Write CSV
aux = data.frame(cbind(1461:2919, test_data$predictions_rf))
colnames(aux) = c('Id', 'SalePrice')
write.csv(aux, file = "RESULTS/rf_results.csv", 
          quote = FALSE, row.names = FALSE)
rm(aux)
```

<br />

### Gradient Boosting Machine (not XGBoost implementation!)

```r
train_control <- caret::trainControl(method="cv", 
                              number=5, 
                              savePredictions = TRUE)
form_lasso = paste0('SalePrice~',
                    str_c(colnames(all_data), collapse = '+'))

tic(gcFirst=FALSE)
mdl.gbm.cv <- caret::train(as.formula(form_lasso),
                   data = train_data, 
                   method = "gbm", 
                   metric="RMSE", 
                   trControl=train_control,
                   verbose=FALSE)
toc(echo=TRUE)
```

```
## elapsed time is 1.530000 seconds
```

```r
print(mdl.gbm.cv)
```

```
## Stochastic Gradient Boosting 
## 
## 1460 samples
##   10 predictor
## 
## No pre-processing
## Resampling: Cross-Validated (5 fold) 
## Summary of sample sizes: 1168, 1169, 1169, 1167, 1167 
## Resampling results across tuning parameters:
## 
##   interaction.depth  n.trees  RMSE      Rsquared   MAE     
##   1                   50      36100.40  0.8098681  23273.94
##   1                  100      33725.44  0.8262858  21233.83
##   1                  150      33168.41  0.8298482  20952.55
##   2                   50      33251.86  0.8310172  20870.79
##   2                  100      31916.96  0.8409857  20027.36
##   2                  150      31898.08  0.8410908  19804.05
##   3                   50      32457.70  0.8383856  20314.12
##   3                  100      31857.84  0.8426858  19785.18
##   3                  150      31921.62  0.8416316  19754.68
## 
## Tuning parameter 'shrinkage' was held constant at a value of 0.1
## 
## Tuning parameter 'n.minobsinnode' was held constant at a value of 10
## RMSE was used to select the optimal model using the smallest value.
## The final values used for the model were n.trees = 100,
##  interaction.depth = 3, shrinkage = 0.1 and n.minobsinnode = 10.
```

```r
print(summary(mdl.gbm.cv))
```

![](iowa_housing_files/figure-html/gbm_model-1.png)<!-- -->

```
##                                     var    rel.inf
## OverallQual                 OverallQual 46.7482695
## GrLivArea                     GrLivArea 21.4219181
## TotalBsmtSF                 TotalBsmtSF  9.2337842
## GarageCars                   GarageCars  8.3665788
## BsmtFinSF1                   BsmtFinSF1  5.5068559
## YearBuilt                     YearBuilt  4.3692153
## X1stFlrSF                     X1stFlrSF  2.6488821
## GarageArea                   GarageArea  1.3372100
## ExterQualTA                 ExterQualTA  0.2626933
## NeighborhoodNridgHt NeighborhoodNridgHt  0.1045929
```

```r
print(mdl.gbm.cv$results$RMSE)
```

```
## [1] 36100.40 33251.86 32457.70 33725.44 31916.96 31857.84 33168.41 31898.08
## [9] 31921.62
```

Finally, predict using the test set and submit those predictions to Kaggle

```r
# Make predictions using the test set
test_data$predictions_gbm <- 
  unname(predict(mdl.gbm.cv, 
                 test_data[, selected_var_names]))

# Write CSV
aux = data.frame(cbind(1461:2919, test_data$predictions_gbm))
colnames(aux) = c('Id', 'SalePrice')
write.csv(aux, file = "RESULTS/gbm_results.csv", 
          quote = FALSE, row.names = FALSE)
rm(aux)
```

<br />

## Hyper-parameter tuning with Parallel Computing

Inside the caret train function there is a little bit of hyper-parameter tuning, as we can see from the output of print(mdl), but in this section we will do a more exhaustive Search.

In addition, we will force our Windows machine to use several cores, to speed up the computations of Random Forest (RF is easy to parallelize since all the trees are created independently from the others).

Note that the CARET implementation of Random Forest only allows us to tune the parameter mtry.


```r
set.seed(0)
# process in parallel
cl <- makeCluster(detectCores(), type="PSOCK")
registerDoParallel(cl)


# prepare training scheme
train_control <- trainControl(method="cv",
                        number=5, 
                        savePredictions = TRUE)

form_lasso = paste0("SalePrice~",
                    str_c(colnames(all_data), collapse = '+'))

n = ncol(train_data)
p = nrow(train_data)
grid <- expand.grid(mtry = seq(floor(n/6),ceil(n/2), 2))


tic(gcFirst=FALSE)
# train the model
mdl.rf.cv.ht <- train(as.formula(form_lasso),
               data = train_data, 
               method="rf", 
               metric="RMSE",
               trControl=train_control,
               tuneGrid = grid,
               verbose = FALSE)
# summarize the model
toc(echo=TRUE)
```

```
## elapsed time is 39.030000 seconds
```

```r
print(mdl.rf.cv.ht)
```

```
## Random Forest 
## 
## 1460 samples
##   10 predictor
## 
## No pre-processing
## Resampling: Cross-Validated (5 fold) 
## Summary of sample sizes: 1169, 1168, 1167, 1168, 1168 
## Resampling results across tuning parameters:
## 
##   mtry  RMSE      Rsquared   MAE     
##   1     35689.16  0.8184006  22313.55
##   3     31638.26  0.8447558  18956.65
##   5     31599.30  0.8449180  18771.13
## 
## RMSE was used to select the optimal model using the smallest value.
## The final value used for the model was mtry = 5.
```

```r
print(summary(mdl.rf.cv.ht))
```

```
##                 Length Class      Mode     
## call               5   -none-     call     
## type               1   -none-     character
## predicted       1460   -none-     numeric  
## mse              500   -none-     numeric  
## rsq              500   -none-     numeric  
## oob.times       1460   -none-     numeric  
## importance        10   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            11   -none-     list     
## coefs              0   -none-     NULL     
## y               1460   -none-     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames            10   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          1   -none-     logical  
## param              1   -none-     list
```

```r
print(mdl.rf.cv.ht$results$RMSE)
```

```
## [1] 35689.16 31638.26 31599.30
```

```r
# turn parallel processing off and run sequentially again:
registerDoSEQ()
```



## XGBoost

```r
set.seed(0)

# process in parallel
cl <- makeCluster(detectCores(), type="PSOCK")
registerDoParallel(cl)

# We transform the data into xgb.DMatrix format, since the algorithm takes only this format
dtrain <- xgboost::xgb.DMatrix(data = as.matrix(train_data[!(names(train_data) %in% "SalePrice")]),
                               label = train_data$SalePrice)

# Train
mdl.xgb <- xgboost::xgboost(dtrain,
                               max_depth = 3,
                               eta = 0.1,
                               nthread = 3,
                               nrounds = 40, 
                               lambda = 0,
                               objective = "reg:linear",
                            verbose = FALSE)

dtest <- as.matrix(test_data[,1:10])
test_data$predictions_xgb <- predict(mdl.xgb, dtest)


# Tune nrounds
# The number of rounds was based on iteratively trying out different values for nround. 
# Let's use the cross validation functionality within xgboost. For convenience, we will move 
# the parameters into its own list

# Cross-validation, is a model validation technique for assessing how the results of a 
# statistical analysis will generalize to an independent data set

param <- list("objective" = "reg:linear",
              "max_depth" = 3, 
              "eta" = 0.1, 
              "lambda" = 0)

cv.nround <- 500
cv.nfold <- 3

mdl.xgb.cv <- xgboost::xgb.cv(param = param,
                                 data = dtrain,
                                 nfold = cv.nfold,
                                 nrounds = cv.nround,
                                 early_stopping_rounds = 200, # training will stop if performance doesn't improve for 200 rounds from the last best iteration
                                 verbose = 0) 

#Let's see
print(mdl.xgb.cv)
```

```
## ##### xgb.cv 3-folds
##     iter train_rmse_mean train_rmse_std test_rmse_mean test_rmse_std
##        1       178666.58      1918.9124      178795.07      4638.721
##        2       161706.71      1674.6734      162015.81      4748.336
##        3       146496.36      1472.1335      146940.22      4737.873
##        4       132854.21      1298.9646      133543.77      4751.264
##        5       120602.07      1130.6486      121547.33      4795.530
## ---                                                                 
##      263        12608.84       306.6407       35030.85      8626.212
##      264        12588.08       305.6012       35030.51      8623.797
##      265        12571.79       288.4780       35036.32      8597.095
##      266        12545.38       287.9068       35034.42      8591.532
##      267        12518.96       284.5421       35038.65      8588.917
## Best iteration:
##  iter train_rmse_mean train_rmse_std test_rmse_mean test_rmse_std
##    67        19318.41       463.3958       34051.65      8165.815
```

```r
# What is the best iteration, with the best evaluation metric value
print(mdl.xgb.cv$best_iteration)
```

```
## [1] 67
```

```r
# Train with nrounds from best iterarion
mdl.xgb <- xgboost(param = param,
                      data = dtrain,
                      nthread = 3, 
                      nrounds = mdl.xgb.cv$best_iteration,
                      verbose = 0)

test_data$predictions_xgb <- predict(mdl.xgb, dtest)

'
# We could further explore the feature importance as we did with random forest
importance <- xgboost::xgb.importance(colnames(train_data[!names(train_data) %in% c("SalePrice")]), model = mdl.xgb)
importance

xgboost::xgb.plot.importance(importance, rel_to_first = TRUE, xlab = "Relative Importance")
'
```

```
## [1] "\n# We could further explore the feature importance as we did with random forest\nimportance <- xgboost::xgb.importance(colnames(train_data[!names(train_data) %in% c(\"SalePrice\")]), model = mdl.xgb)\nimportance\n\nxgboost::xgb.plot.importance(importance, rel_to_first = TRUE, xlab = \"Relative Importance\")\n"
```

```r
# One of the final components here is to tune the other parameters in param. 
# For this we  will have to use the caret package

ntrees <- mdl.xgb.cv$best_iteration

param_grid <- expand.grid(
  nrounds = ntrees,
  eta = seq(2, 24, 2)/ntrees,
  subsample = 1.0,
  colsample_bytree = 1.0,
  max_depth = c(1, 2, 3, 4, 5, 6),
  gamma = 1,
  min_child_weight = 1
)


xgb_control <- caret::trainControl(
  method = "cv",
  number = 5
)

# Find best combination of parameters, using caret package
mdl.xgb.tuned <- caret::train(as.formula(form_lasso),
                                 train_data,
                                 trControl = xgb_control,
                                 tuneGrid = param_grid,
                                 lambda = 0,
                                 method = "xgbTree")

# The best tuning parameters and the final model is given by
mdl.xgb.tuned$bestTune
```

```
##    nrounds max_depth        eta gamma colsample_bytree min_child_weight
## 16      67         4 0.08955224     1                1                1
##    subsample
## 16         1
```

```r
plot(mdl.xgb.tuned)
```

![](iowa_housing_files/figure-html/xgboost_tuning-1.png)<!-- -->

```r
print(mdl.xgb.tuned$results$RMSE)
```

```
##  [1] 52996.84 38700.42 35520.62 34483.43 33380.95 32780.73 32557.19
##  [8] 31635.28 31721.86 31969.97 31943.52 32317.33 44991.64 32014.37
## [15] 29875.07 29459.64 28915.98 29042.19 29580.23 29596.12 29867.15
## [22] 29905.06 29961.21 30080.38 41479.57 29068.25 28393.63 28645.94
## [29] 29173.07 28405.03 29236.61 29360.18 29305.99 30572.13 30127.74
## [36] 30390.00 39670.97 28420.64 27806.65 28553.19 28445.38 28669.56
## [43] 29107.88 28879.78 29260.57 30104.74 29575.81 29013.79 39282.55
## [50] 28711.46 28353.59 28963.76 29069.36 29437.35 29319.70 29636.67
## [57] 29386.05 29866.80 30002.46 30091.76 38883.64 28794.28 28364.10
## [64] 28700.74 28767.13 28484.76 28774.59 29879.84 29461.36 29305.67
## [71] 29829.40 30337.37
```

```r
# turn parallel processing off and run sequentially again:
registerDoSEQ()
```


```r
# Make predictions using the test set
test_data$predictions_xgb <- 
  unname(predict(mdl.xgb.tuned$finalModel, dtest))

# Write CSV
aux = data.frame(cbind(1461:2919, test_data$predictions_xgb))
colnames(aux) = c('Id', 'SalePrice')
write.csv(aux, file = "RESULTS/xgb_results.csv", 
          quote = FALSE, row.names = FALSE)
rm(aux)
```
