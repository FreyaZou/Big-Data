library('RSQLite')
library('data.table')
library('parallel')

# connect to the sqlite file
sqlite = dbDriver("SQLite")
exampledb = dbConnect(sqlite,"pred.sqlite")
alltables = dbListTables(exampledb)

######## Q1

# find all id
idall = dbGetQuery(exampledb,'select demo.ID from demo 
                   inner join pred on demo.ID = pred.ID 
                   inner join outcome on pred.ID = outcome.ID')
fwrite(idall,"idall.csv")

# find the IDs that available only in Table pred and demo but not in Table outcome
idmissing = dbGetQuery(exampledb,'select demo.ID from demo 
                       INNER JOIN pred on demo.ID = pred.ID where pred.Id not in (select ID from outcome)')

fwrite(idmissing,"idmissing.csv")

######## Q2
alldata = dbGetQuery(exampledb,'select * from demo 
                     inner join pred on demo.ID = pred.ID
                     inner join outcome on pred.ID = outcome.ID') 

# delete duplicate id columns
predictors = alldata[,-4][,-109]

xs = model.matrix(O1~.,data = predictors)
Xs = xs[,2:111] # select only Xs (no intercept)

# select O1 as y_t
library(dplyr)
y_t = predictors %>%
  select(O1) %>%
  unlist() %>%
  as.numeric()

# lamda sequence
#grid = 10^seq(10, -2, length = 100)

# longer lamda sequence
grid1 = 10^seq(2, -3, length = 300)


####### ridge regression
library(glmnet)
# use cross validation to choose best lamda
ridge.lam1 = cv.glmnet(Xs,y_t,alpha = 0, lambda = grid1)
best.lam.r1 = ridge.lam1$lambda.min
# "train error"
min(ridge.lam1$cvm)
#coef(ridge.mod, s = best.lam.r1)

####### lasso regression

# use cross validation to choose best lamda
lasso.lam = cv.glmnet(Xs,y_t,alpha = 1, lambda = grid1)
best.lam.ls = lasso.lam$lambda.min

# "cross validation error"
min(lasso.lam$cvm)
lasso.mod = glmnet(Xs,y_t,alpha = 1, lambda = grid1)

## Based on the MSE from this 3 models, the cross validation mse of the lasso 
## sometimes is less than that of the resge regression.
## And considering the number of predictors is relatively large, 
## shrinking the dimention will help improve the computing speed and avoiding overfitting.
## So we choose the lasso to do the prediction in the following analysis.

###### prepare test Xs
test_all = dbGetQuery(exampledb,'select * from demo 
                      INNER JOIN pred on demo.ID = pred.ID where pred.Id not in (select ID from outcome)')
#names(test_all)
test_predictor = test_all[,-4]
t_predictor = model.matrix(~.,data = test_predictor)
t_predictor = t_predictor[,-1]
output1 = predict(lasso.mod, t_predictor,s = best.lam.ls)
fwrite(output1, 'output1.csv')

################Q3
## prepare Ys
train_out = alldata[,111:128]

## function that computes the prediction of missing ids
fit_each_column = function(x = Xs, x1 = t_predictor, y = train_out[,1]){
  fit.lasso = glmnet(x,y,alpha = 1, lambda = grid1)
  fit.cv = cv.glmnet(x,y,alpha = 1, lambda = grid1)
  best.lamb = fit.cv$lambda.min
  
  predict(fit.lasso,x1,s = best.lamb)
}

predct = mclapply(train_out,function(y){fit_each_column(y = y)})
prediction = matrix(unlist(predct),nrow = nrow(t_predictor), ncol = ncol(train_out))

output2 = data.frame(cbind(t_predictor[,1],prediction))
colnames(output2)[1] = 'id'
fwrite(output2, 'output2.csv')

# MSE
mse_cv = function(y = train_out[,1],x = Xs){
  
  fit.cv = cv.glmnet(x,y,alpha = 1, lambda = grid1)
  best.lamb = fit.cv$lambda.min
  min(fit.cv$cvm)
}
mses = mclapply(train_out,function(y){mse_cv(y = y,x = Xs)})
msem = matrix(unlist(mses), nrow = 1, ncol = ncol(train_out))
sum(msem)

################Q4
pred_2 = fread('pred2.csv', header = FALSE)
colnames(pred_2)[1] = 'id'

new_pred = inner_join(predictors[,1:108],pred_2,by=c("id" = "id"))
new_test = inner_join(test_predictor,pred_2,by=c("id" = "id"))

new_xs = model.matrix(~.,new_pred)[,-1]
new_t_xs = model.matrix(~.,new_test)[,-1]

predct_l = mclapply(train_out, function(y){fit_each_column(x = new_xs, x1 = new_t_xs, y=y)})
prediction_l = matrix(unlist(predct_l),nrow = nrow(new_t_xs), ncol = ncol(train_out))
output3 = data.frame(cbind(t_predictor[,1],prediction_l))
colnames(output3)[1] = 'id'
fwrite(output3, 'output3.csv')

## MSE of larger dimension of predictors
mses_l = mclapply(train_out, function(y){mse_cv(x = new_xs, y=y)})
mse_l = matrix(unlist(mses_l),nrow = 1, ncol = ncol(train_out))
sum(mse_l)

# More predictors improve the model fit




