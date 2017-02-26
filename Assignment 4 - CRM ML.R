library(bit64)
library(data.table)
library(glmnet)
library(ranger)
library(ggplot2)
library(corrplot)
library(knitr)

load("Customer-Development.RData")

head(crm_DT)
names(crm_DT)

# preset for training sample later (0 = training, 1 = validation)
set.seed(1999)
crm_DT[,training_sample := rbinom(nrow(crm_DT), 1, .5)]



### Data Inspection


## Summarize key aspects of outcome_spend

# What is the purchase incidence?
nrow(crm_DT[outcome_spend != 0,]) / nrow(crm_DT[outcome_spend == 0,])
# 8.26% of customers spend at all

# What is distribution of dollar spend unconditional on purchase?
ggplot(aes(x= outcome_spend), data=crm_DT) + geom_histogram(binwidth=10)
summary(crm_DT[,outcome_spend])
# Min:    $ 0.00
# Median: $ 0.00
# Mean:   $ 8.44
# Max:    $ 1103.00

# What is distrbution of dollar spend conditional on purchase?
ggplot(aes(x= outcome_spend), data=crm_DT[outcome_spend != 0,]) +
  geom_histogram(binwidth=10)
summary(crm_DT[outcome_spend != 0, outcome_spend])
# Min:    $ 13.95
# Median: $ 89.00
# Mean:   $ 110.60
# Max:    $ 1103.00



### Data Pre-Processing


## Identify correlations

# Calculate correlations
cor_matrix <- cor(crm_DT[, !c("customer_id", "mailing_indicator", "outcome_spend"),
with = FALSE])

# Create PDF to show matrix
pdf("Correlation-Matrix.pdf", height = 16, width = 16)

# Plot it (shaded boxes from red to blue to white = -1 to 0 to 1 correlation)
corrplot(cor_matrix, method = "color",
type = "lower", diag = FALSE,
tl.cex = 0.4, tl.col = "gray10")

# Plot it (colored numbers now visualized)
corrplot(cor_matrix, method = "number", number.cex = 0.25, addgrid.col = NA,
type = "lower", diag = FALSE,
tl.cex = 0.4, tl.col = "gray10")
dev.off()
# Some high correlations, a couple high negative correlations

## Eliminate correlations

# Matrix only shows lower half, but upper half exists (which is mirror of lower half)
# so set upper half to n/a (also set diagonal -- colA:colA, colB:colB to n/a)
cor_matrix[upper.tri(cor_matrix, diag = TRUE)] = NA

# create data table of matrix
cor_DT <- data.table(row = rep(rownames(cor_matrix), ncol(cor_matrix)), 
                    col = rep(colnames(cor_matrix), each = ncol(cor_matrix)), 
                    cor = as.vector(cor_matrix))
cor_DT <- cor_DT[is.na(cor) == FALSE]

# isolate cols with absolute correlations > .95
highcor_DT <- cor_DT[abs(cor) > .95]
highcor_v <- unique(highcor_DT$row)

# delete!
crm_DT[, (highcor_v) := NULL]
names(crm_DT)



### Predictive Model Estimation

## separate training data from validation sample
train <- crm_DT[training_sample == 0,]
validate <- crm_DT[training_sample == 1,]

## OLS (regular linear model)

# run model
OLS <- glm(outcome_spend ~ . - customer_id - training_sample, data=train)
s_OLS <- summary(OLS)

# collect results
results <- data.table(input = rownames(s_OLS$coefficients), 
                     est_OLS = s_OLS$coefficients[, 1], 
                     p_OLS = s_OLS$coefficients[, 4])
summary(OLS)

# predict
OLS_predict <- predict(OLS, validate, type="response")
plot(OLS_predict)

# MSE
mse_OLS <- mean((validate[,outcome_spend] - OLS_predict)^2)
# 1434.364


## LASSO

# create matrix
train_MM <- model.matrix(outcome_spend ~ 0 + ., data = train)
y <- train$outcome_spend

# run model
LASSO <- glmnet(x=train_MM, y=y)
plot(LASSO, xvar="lambda")
# hey! I remember these! :)

# cross validation
cv_LASSO <- cv.glmnet(x=train_MM, y=y)

# (don't think I have to do this, but w/e)
cv_LASSO$lambda.min
cv_LASSO$lambda.1se
results[, est_LASSO := coef(cv_LASSO, s = "lambda.min")[,1]]
results[, est_LASSO_1se := coef(cv_LASSO, s = "lambda.1se")[,1]]
coef(cv_LASSO, s = "lambda.min")
plot(cv_LASSO)

# predict
validate_MM <- model.matrix(outcome_spend ~ 0 + ., data = validate)
v_y <- validate$outcome_spend

LASSO_predict <- predict(cv_LASSO, newx=validate_MM, s="lambda.min")

# MSE
mse_LASSO <- mean((validate[,outcome_spend] - LASSO_predict)^2)
# 1432.01


## Elastic Net

# I don't know what I'm doing
# Create folds
folds <- sample(1:10, nrow(train_MM), replace=TRUE)

# Output table
mse_DT <- data.table(alpha = seq(0,1, by=.01), mean_cv_error = rep(0,101))

# Calculate cross-validation error for different alpha values
for (i in 0:100) { 
  cv_i <- cv.glmnet(x = train_MM, y = y, alpha = mse_DT[i+1, alpha], foldid = folds) 
  mse_DT[i+1, mean_cv_error := min(cv_i$cvm)] 
  cat("Iteration", i, " CV error:", mse_DT[i+1, mean_cv_error], "\n") 
}
index_min <- which.min(mse_DT$mean_cv_error)
mse_DT[index_min, alpha]
# unsure if I should have done LASSO or ridge regression
# Doc says LASSO is good when coefficients are large or many are 0 (how do I find that
# out?)

# How do I predict???


## Random Forest

# remove missing values
train_rf <- train[complete.cases(train)]

# estimate random forest
random_forest <- ranger(outcome_spend ~ .,
data=train_rf[, !c("customer_id", "training_sample"), with = FALSE], num.trees = 1000,
# importance = "impurity",
seed = 111)

# predict
rf_predict <- predict(random_forest, data=validate)

# plot
validate[, predicted_spend := rf_predict$predictions]
ggplot(validate, aes(x = outcome_spend, y = predicted_spend)) + 
  geom_line(color = "purple", size = 1) + theme_bw()
# uh...wut? Shouldn't that be a straight line? Or, like, close to it?