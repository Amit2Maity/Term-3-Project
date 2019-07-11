library(lattice)
library(tidyr)
library(dplyr)
library(car)
library(fastDummies)
library(caret)
library(randomForest)
library(corrplot)

customer_data = read.csv('F:/Capstone/DS3/term3.csv', stringsAsFactors = F)

#Creating dummy variables
cust_df = fastDummies::dummy_cols(customer_data[,c(2:12)], remove_first_dummy = TRUE)

cust_df = cust_df[,-c(1,2,7,8)]

#Feature Engineering

cust_df$customer_rating_per_calls = cust_df$Customer_rating/cust_df$Customer_care_calls

cust_df$prior_purchase_per_calls = cust_df$Prior_purchases/cust_df$Customer_care_calls

cust_df$weight_per_discount = cust_df$Weight_in_gms/cust_df$Discount_offered

cust_df$discount_per_weight = cust_df$Discount_offered/cust_df$Weight_in_gms

#Correlation plot

cust_df$Reached.on.Time_Y.N<- as.numeric(levels(cust_df$Reached.on.Time_Y.N))[cust_df$Reached.on.Time_Y.N]

corrplot(cor(cust_df),type = "upper",tl.col="black",order = "hclust")

# Train test split
set.seed(1234)
split1 <- sample(1:nrow(cust_df), size = 0.7 * nrow(cust_df))
trainSplit <- cust_df[ split1,]
test <- cust_df[-split1,]

set.seed(1234)
split2 <- sample(1:nrow(trainSplit), size = 0.7 * nrow(trainSplit))
train <- trainSplit[ split2,]
Validation <- trainSplit[-split2,]

#VIF calculation

mod1 = lm(Reached.on.Time_Y.N ~ Customer_care_calls+Customer_rating+Cost_of_the_Product+
            Prior_purchases+Discount_offered+Weight_in_gms+
            Warehouse_block_F+Warehouse_block_A+Warehouse_block_B+
            Warehouse_block_C+Mode_of_Shipment_Ship+
            Mode_of_Shipment_Road+Product_importance_medium+
            Product_importance_high+Gender_M, data = train)

vif(mod1)

mod2 = glm(Reached.on.Time_Y.N ~ Customer_care_calls+Customer_rating+Cost_of_the_Product+
             Prior_purchases+Discount_offered+Weight_in_gms+
             Warehouse_block_F+Warehouse_block_A+Warehouse_block_B+
             Warehouse_block_C+Mode_of_Shipment_Ship+
             Mode_of_Shipment_Road+Product_importance_medium+
             Product_importance_high+Gender_M, data = train, family = "binomial")

step(mod2)

model_glm = glm(formula = Reached.on.Time_Y.N ~ Customer_care_calls + Cost_of_the_Product + 
                  Prior_purchases + Discount_offered + Weight_in_gms + Warehouse_block_A + 
                  Product_importance_high + Gender_M, family = "binomial", 
                data = train)

summary(model_glm)

#VIF calculation

mod3 = glm(Reached.on.Time_Y.N ~ Customer_care_calls+Customer_rating+Cost_of_the_Product+
             Prior_purchases+Discount_offered+Weight_in_gms+
             Warehouse_block_F+Warehouse_block_A+Warehouse_block_B+
             Warehouse_block_C+Mode_of_Shipment_Ship+
             Mode_of_Shipment_Road+Product_importance_medium+
             Product_importance_high+Gender_M, family = "binomial", data = Validation)

step(mod3)

model2_glm = glm(formula = Reached.on.Time_Y.N ~ Customer_care_calls + Cost_of_the_Product + 
                   Prior_purchases + Discount_offered + Weight_in_gms + Warehouse_block_A + 
                   Warehouse_block_B, family = "binomial", data = Validation)

summary(model2_glm)

#with common features

model.glm = glm(formula = Reached.on.Time_Y.N ~ Customer_care_calls + Cost_of_the_Product + 
                  Prior_purchases + Discount_offered + Weight_in_gms , family = "binomial", 
                data = train)

pred_y_prob = predict(model.glm, newdata =  test, type="response")

glm.pred_y = ifelse(pred_y_prob>0.6 , 1 ,0)

confusionMatrix(as.factor(test$Reached.on.Time_Y.N), as.factor(glm.pred_y))


########################## Random Forest###################

train$Reached.on.Time_Y.N = as.factor(train$Reached.on.Time_Y.N)

model_rf = randomForest(Reached.on.Time_Y.N ~ Customer_care_calls + Cost_of_the_Product + 
                          Prior_purchases + Discount_offered + Weight_in_gms + Warehouse_block_A + 
          Warehouse_block_B+Product_importance_high + Gender_M,mtry = 2,data = train,replace = TRUE)

importance(model_rf)

varImpPlot(model_rf, main = " Variable Importance") +
  abline(h=3,col="blue")

pred_y = predict(model_rf, newdata = test)

confusionMatrix(as.factor(test$Reached.on.Time_Y.N), pred_y)

##############Random Forest Grid Search#################
set.seed(29)
control <- trainControl(method = "cv", 
                        number = 5)

RFtunegrid <- expand.grid(mtry = c(1:4))
set.seed(123)
rf_gridsearch <- train(Reached.on.Time_Y.N ~ Customer_care_calls + Cost_of_the_Product + 
                         Prior_purchases + Discount_offered + Weight_in_gms, data = train, 
                       method = "rf", metric = "Accuracy",
                       tuneGrid = RFtunegrid, trControl = control)

print(rf_gridsearch)

ggplot(rf_gridsearch)

pred_y = predict(model_rf, newdata = test)

confusionMatrix(as.factor(test$Reached.on.Time_Y.N), pred_y)



############################# Decision Tree #######################
library(rpart)
library(rattle)	
library(rpart.plot)
library(RColorBrewer)
library(party)
install.packages("partykit")
library(partykit)
fit=rpart(factor(Reached.on.Time_Y.N)~., train)
plot(fit)
text(fit)

prp(rxAddInheritance(fit))
fancyRpartPlot(fit)




dtree.pred_y = predict(fit, newdata =  test, type = "class")

confusionMatrix(as.factor(test$Reached.on.Time_Y.N), as.factor(dtree.pred_y))



############################################### XG Boost ##########

set.seed(123)
tune_grid <- expand.grid(
  nrounds = seq(from = 100, to = 1000, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)


tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 5, # with n folds 
  #index = createFolds(tr_treated$Id_clean), # fix the folds
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results 
)


xgb_tune <- caret::train(
  Reached.on.Time_Y.N ~ Customer_care_calls + Cost_of_the_Product + 
    Prior_purchases + Discount_offered + Weight_in_gms, data = train,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = FALSE
)


final <- data.frame(actual = test$Reached.on.Time_Y.N,
                    predict = predict(xgb_tune, newdata = test))

confusionMatrix(as.factor(test$Reached.on.Time_Y.N), as.factor(final$predict) )

################   SVM ############3

library(e1071)

model.svm = svm(formula = Reached.on.Time_Y.N ~ Customer_care_calls + Cost_of_the_Product + 
                  Prior_purchases + Discount_offered + Weight_in_gms, data = train, 
                type = 'C-classification', 
                kernel = 'linear')

final <- data.frame(actual = test$Reached.on.Time_Y.N,
                    predict = predict(model.svm, newdata = test))

confusionMatrix(as.factor(test$Reached.on.Time_Y.N), as.factor(final$predict) )

summary(customer_data)
count(custome)
