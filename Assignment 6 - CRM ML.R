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
plot(validate[,outcome_spend],OLS_predict)

# MSE
mse_OLS <- mean((validate[,outcome_spend] - OLS_predict)^2)
# 1376.841


## LASSO

# create matrix
train_MM <- model.matrix(outcome_spend ~ 0 + . - customer_id - training_sample, data = train)
y <- train$outcome_spend

# run model
LASSO <- glmnet(x=train_MM, y=y)
plot(LASSO, xvar="lambda")
# hey! I remember these! :)

# cross validation
cv_LASSO <- cv.glmnet(x=train_MM, y=y)

# (don't think I have to do this, but w/e)
cv_LASSO$lambda.min # 0.15133
cv_LASSO$lambda.1se # 3.92712
results[, est_LASSO := coef(cv_LASSO, s = "lambda.min")[,1]]
results[, est_LASSO_1se := coef(cv_LASSO, s = "lambda.1se")[,1]]
coef(cv_LASSO, s = "lambda.min")
plot(cv_LASSO)

# predict
validate_MM <- model.matrix(outcome_spend ~ 0 + . - customer_id - training_sample, data = validate)
v_y <- validate$outcome_spend

LASSO_predict <- predict(cv_LASSO, newx=validate_MM, s="lambda.min")
plot(validate$outcome_spend, LASSO_predict)

# MSE
mse_LASSO <- mean((validate[,outcome_spend] - LASSO_predict)^2)
# 1373.139


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
mse_DT[index_min, alpha] # 0.15

mse_DT[alpha==.15,]
#1374.761
# .15 closer to 0 (ridge) than 1 (LASSO)
plot(mse_DT[,mean_cv_error])

# Predit
elastic_net <- cv.glmnet(x=train_MM, y=y, alpha=0.15)
elastic_net_predict <- predict(elastic_net, newx=validate_MM, s="lambda.min") 
mse_elastic_net <- mean((validate[,outcome_spend] - elastic_net_predict)^2)
# 1373.015

plot(validate$outcome_spend, elastic_net_predict)


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
mse_rf <- mean((validate[,outcome_spend] - rf_predict$predictions)^2)
# 891.502

# plot
validate[, predicted_spend := rf_predict$predictions]
ggplot(validate, aes(x = outcome_spend, y = predicted_spend)) + 
  geom_point(color = "purple", size = 1) + theme_bw()

hist(abs(validate$outcome_spend - rf_predict$predictions), breaks=500, xlim=c(0,300))


# table of predicted vs actual results
predict_table <- cbind(validate$outcome_spend, OLS_predict, LASSO_predict, 
                       elastic_net_predict, rf_predict$predictions)
predict_table <- as.data.table(predict_table)
names(predict_table) <- c("Actual", "OLS", "LASSO", "elastic_net", "random_forest")

head(predict_table)

# Need to add Elastic Net and Random Forest predictions 

# Plot predicted vs actual results

ggplot(predict_table, aes(x=Actual, y=OLS))+
  geom_point(color= "dark blue") + 
  xlab("Actual") + 
  ylab("OLS Prediction") +
  ggtitle("Actual vs Predicted: OLS")

ggplot(predict_table, aes(x=Actual, y=LASSO))+
  geom_point(color= "dark blue") + 
  xlab("Actual") + 
  ylab("LASSO Prediction") +
  ggtitle("Actual vs Predicted: LASSO")

ggplot(predict_table, aes(x=Actual, y=elastic_net))+
  geom_point(color= "dark blue") + 
  xlab("Actual") + 
  ylab("Elastic Net Prediction") +
  ggtitle("Actual vs Predicted: Elastic Net")

ggplot(predict_table, aes(x=Actual, y=random_forest))+
  geom_point(color= "dark blue") + 
  xlab("Actual") + 
  ylab("Random Forest Prediction") +
  ggtitle("Actual vs Predicted: Random Forest Prediction")



### Model Validation


## OLS

# Create OLS lift table
OLS_lift_DT <- predict_table[,.(Actual, OLS)]
OLS_lift_DT[, group := 0]

# Cut into groups
OLS_lift_DT[, group := cut_number(OLS_lift_DT$OLS, n=20)]
OLS_lift_DT[, group_no := as.integer(group)]

OLS_mean_actual <- mean(OLS_lift_DT[,Actual])

OLS_lift_table <- OLS_lift_DT[, lift := mean(Actual)/OLS_mean_actual*100, by=group_no] 

ggplot(OLS_lift_table, aes(x = as.factor(group_no), y = lift)) +
  geom_point(shape = 21, color = "gray30", fill = "hotpink", size = 2.5) + theme_bw() +
  geom_hline(aes(yintercept=100)) + ggtitle("OLS Lift")


## LASSO

# Create LASSO lift table
LASSO_lift_DT <- predict_table[,.(Actual, LASSO)]
LASSO_lift_DT[, group := 0]

# Cut into groups
LASSO_lift_DT[, group := cut_number(LASSO_lift_DT$LASSO, n=20)]
LASSO_lift_DT[, group_no := as.integer(group)]

LASSO_mean_actual <- mean(LASSO_lift_DT[,Actual])

LASSO_lift_table <- LASSO_lift_DT[, lift := mean(Actual)/LASSO_mean_actual*100, by=group_no] 

ggplot(LASSO_lift_table, aes(x = as.factor(group_no), y = lift)) +
  geom_point(shape = 21, color = "gray30", fill = "hotpink", size = 2.5) + theme_bw() +
  geom_hline(aes(yintercept=100)) + ggtitle("LASSO Lift")


## Elastic Net

# Create en lift table
en_lift_DT <- predict_table[,.(Actual, elastic_net)]
en_lift_DT[, group := 0]

# Cut into groups
en_lift_DT[, group := cut_number(en_lift_DT$elastic_net, n=20)]
en_lift_DT[, group_no := as.integer(group)]

en_mean_actual <- mean(en_lift_DT[,Actual])

en_lift_table <- en_lift_DT[, lift := mean(Actual)/en_mean_actual*100, by=group_no] 

ggplot(en_lift_table, aes(x = as.factor(group_no), y = lift)) +
  geom_point(shape = 21, color = "gray30", fill = "hotpink", size = 2.5) + theme_bw() +
  geom_hline(aes(yintercept=100)) + ggtitle("Elastic Net Lift")


## Random Forest

# Create Random Forest lift table
rf_lift_DT <- predict_table[,.(Actual, random_forest)]
rf_lift_DT[, group := 0]
head(rf_lift_DT)

# Cut into groups
rf_lift_DT[, group := cut_number(rf_lift_DT$random_forest, n=20)]
rf_lift_DT[, group_no := as.integer(group)]

mean_actual <- mean(rf_lift_DT[,Actual])

rf_lift_table <- rf_lift_DT[, lift := mean(Actual)/mean_actual*100, by=group_no] 

ggplot(rf_lift_table, aes(x = as.factor(group_no), y = lift)) +
  geom_point(shape = 21, color = "gray30", fill = "hotpink", size = 2.5) + theme_bw() +
  geom_hline(aes(yintercept=100))


logit <- glm(outcome_spend ~ . - customer_id - training_sample, data=train,response="binomial")
s_OLS <- summary(OLS)



### Predictive Modeling: Decomposition Method


## Run Logistic Regression

# create column of spend or not
train[, spent_money := 0]
train[outcome_spend > 0, spent_money := 1]
validate[, spent_money := 0]
validate[outcome_spend > 0, spent_money := 1]

# run regression
OLS_logit <- glm(spent_money ~ . - customer_id - training_sample - outcome_spend, 
                 data=train, family=binomial)
z <- summary(OLS_logit)
sigma_2 <- (z$sigma)^2

# predict
OLS_logit_predict <- predict(OLS_logit, validate, type="response")
plot(validate[,spent_money], OLS_logit_predict)

ggplot(aes(x=validate[,spent_money], y=OLS_logit_predict))+
  geom_point(color= "dark blue") + 
  xlab("Actual") + 
  ylab("Logit Prediction") +
  ggtitle("Actual vs Predicted: Logit")

# MSE
mse_logit <- mean((validate[,spent_money] - OLS_logit_predict)^2) # .0678



## Run Non-Logistic Regression on non-zero spend

# grab only the rows who spent money
train_spend <- train[spent_money == 1]

# 

# run regression
nonzero_spend_lm <- glm(log(outcome_spend) ~ . - customer_id - training_sample - spent_money, 
                        data=train_spend)

# predict
nonzero_spend_predict <- predict(nonzero_spend_lm, validate, type="response")
plot(validate[,outcome_spend], exp(nonzero_spend_predict))


## Multiply Predictions
final_predict <- exp(nonzero_spend_predict)*OLS_logit_predict

# plot
plot(validate[,outcome_spend], final_predict)

# MSE
mse_final <- mean((validate[,outcome_spend] - final_predict)^2) # 1426.535

validate[,predict := final_predict]


## Lift

# Create lift table
final_lift_DT <- validate[,.(outcome_spend, predict)]
final_lift_DT[, group := 0]
head(final_lift_DT)

# Cut into groups
final_lift_DT[, group := cut_number(final_lift_DT$predict, n=20)]
final_lift_DT[, group_no := as.integer(group)]

mean_actual <- mean(final_lift_DT[,outcome_spend])

final_lift_table <- final_lift_DT[, lift := mean(outcome_spend)/mean_actual*100, by=group_no] 

ggplot(final_lift_table, aes(x = as.factor(group_no), y = lift)) +
  geom_point(shape = 21, color = "gray30", fill = "hotpink", size = 2.5) + theme_bw() +
  geom_hline(aes(yintercept=100))


logit <- glm(outcome_spend ~ . - customer_id - training_sample, data=train,response="binomial")
s_OLS <- summary(OLS)

