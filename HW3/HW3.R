#Task 1 On the use of distance information for UwaveGesture Recognition Task
#a) Suppose we decided to apply a nearest-neighbor (NN) classifier to find the labels of test instances. For each distance measure alternative,
#use the training data to identify the ideal value of k which minimizes the error of a 10-fold cross-validation.

#Installing required packages
#install.packages('class')
#install.packages('caret')
#installed.packages('FastKNN')
#install.packages('TunePareto')
#installed.packages('data.table')
#install.packages('glmnet')
library(class)
library(caret)
library(FastKNN)
library(TunePareto)
library(data.table)
library(glmnet)


#Read and concatanate the data for the train
colNames <- function(letter, length){
  lst <- rep(1,length)
  for(i in 1:length){
    conc <- paste(letter, toString(i),sep="")
    lst[i] <- conc
  }
  return(lst)
}

# X Train
x_tr = read.table("uWaveGestureLibrary_X_TRAIN")
colnames(x_tr)[1] = "class"
xcol <- colNames("x",dim(x_tr)[2]-1)
colnames(x_tr)[2:316] = xcol

# y Train
y_tr = read.table("uWaveGestureLibrary_y_TRAIN")
y_tr <- y_tr[,-1]
ycol <- colNames("y",dim(y_tr)[2])
colnames(y_tr) = ycol

# z Train
z_tr = read.table("uWaveGestureLibrary_z_TRAIN")
z_tr <- z_tr[,-1]
zcol <- colNames("z",dim(z_tr)[2])
colnames(z_tr) = zcol

#Concatanated data frame for train set
conc_df_tr <- cbind(x_tr, y_tr, z_tr)

#Read and concatanate the data for the test
# x Test
x_te = read.table("uWaveGestureLibrary_X_TEST")
colnames(x_te)[1] = "class"
xcol <- colNames("x",dim(x_te)[2]-1)
colnames(x_te)[2:316] = xcol


# y Test
y_te = read.table("uWaveGestureLibrary_y_TEST")
y_te <- y_te[,-1]
ycol <- colNames("y",dim(y_te)[2])
colnames(y_te) = ycol

# z Test
z_te = read.table("uWaveGestureLibrary_z_TEST")
z_te <- z_te[,-1]
zcol <- colNames("z",dim(z_te)[2])
colnames(z_te) = zcol

#Concatanated data frame for test set
conc_df_te <- cbind(x_te, y_te, z_te)

#Euclidian and Manhattan distances will be used for the KNN

#Euclidian Distance
#With the aid of caret package, knn with 10 fold cross validation carried out.
set.seed(6)
trControl <- trainControl(method  = "cv", number  = 10)
fit <- train(as.factor(class) ~ ., method     = "knn", tuneGrid   = expand.grid(k = 1:10),trControl  = trControl, metric = "Accuracy", data= conc_df_tr)
fit
fit$bestTune
#Models that use 1 and 3 nearest neighbors are the best. But best k = 3.

#Manhattan Distance
#To perform knn with manhattan distance FastKNN package and knn_test_function are used. This function takes distance matrix as an input.
#Con of this function is that it does not have cross validation capability. To overcome this, below function is created.
#10 cv runs are created via TunePareto package and  generateCVRuns function
set.seed(7)
#number of nearest neighbors going to be tried
k=c(1,3,5,7,9)
#calculation of distance matrix
manhattan= as.matrix(dist(conc_df_tr[,-1], method = "manhattan"))
numReplications=1
numFolds=10
indices=generateCVRuns(conc_df_tr[,1],numReplications,numFolds,stratified=TRUE)
cvresult=data.table()
for(i in 1:numReplications) 
{
  curReplication=indices[[i]]
  for(j in 1:numFolds)
  {
    curindices=curReplication[[j]]
    cvtrain=conc_df_tr[-curindices,]        
    cvtest=conc_df_tr[curindices,]
    labels<-as.numeric(rownames(as.data.table(cvtrain)))
    for(y in 1:length(k))
    {
      param_k=k[y]
      predict_knn = knn_test_function(cvtrain, cvtest, manhattan[curindices,labels], conc_df_tr[,1], k = param_k)
      cvresult=rbind(cvresult,data.table(Replication=i,Fold=j,Method='knn',Klev=param_k,TestId=curindices,
                                         Predictions=as.numeric(as.character(predict_knn)),Real=conc_df_tr[curindices,1]))
    }   
  }    
}
cvresult[,list(Accu=mean(Predictions==Real)),by=list(Method,Klev)]
#Model that use 1 and 3 nearest neighbors are the best. But best k = 1.


#b) Using the value of k in part (a) and evaluate your final performance on the test data and present your results in a (8-by-8) confusion matrix, showing
#the counts for actual and predicted labels. In addition, quote the runtime and accuracy for your results.

#predictions of knn that uses euclidian distance, k=3 and runtime
start_time <- Sys.time()
knn.pred1 <- knn(conc_df_tr[,-1], conc_df_te[-1], conc_df_tr[,1] , k = 3)
end_time <- Sys.time()
#Runtime
end_time - start_time
#Confusion matrix
table(knn.pred1 , conc_df_te[,1])
#Accuracy of predictions
mean(knn.pred1==conc_df_te[,1])

#predictions of knn that uses manhattan distance, k=1 and runtime
start_time <- Sys.time()
dist_mat <- Distance_for_KNN_test(conc_df_te[-1],conc_df_tr[,-1])
knn.pred2 = knn_test_function(conc_df_tr[,-1], conc_df_te[-1], dist_mat, conc_df_tr[,1], k = 1)
#Runtime
end_time <- Sys.time()
#Confusion matrix
end_time - start_time
table(knn.pred2 , conc_df_te[,1])
#Accuracy of predictions
mean(knn.pred2==conc_df_te[,1])

#c) The observations from different axes are weighted equally if we compute the distance over each axis and sum them to obtain a final similarity measure.
#Is this reasonable? For example, we can compute the distance by weighing different axes differently. Do you think weighting the distances over different axes
#to obtain a final similarity measure makes sense for classification? Why?

#In my opinion it is logical to give x and y axis more weight tha z axis.The reason is gestures are explained mainly by accelerations in these axis.



#Task 2 Linear models on alternative representations of the data
#Suppose we are willing to perform a binary classification task to identify if a test time series is from Class 3.
#a) Train a logistic regression model on the training data and use the model to make a prediction on the test data. Note that you will obtain probabilistic
#predictions This will require you to select a threshold since 0.5 as a threshold may not work well under this imbalanced class setting. To make things easier,
#use the ratio of Class 3 instances in the training data as threshold. Use the learned model to predict the class for test data. Present your results in a
#(2-by-2) confusion matrix.
conc_df_tr1 <- conc_df_tr
conc_df_tr1$class <- as.numeric(conc_df_tr1$class==3)
conc_df_te1 <- conc_df_te
conc_df_te1$class <- as.numeric(conc_df_te$class==3)
thresh <- sum(conc_df_tr1$class)/length(conc_df_tr1$class)

#penalized regression with lambda = 0 and with binomial family is simply logistic regression
set.seed(8)
logist2 <- glmnet(conc_df_tr1[,-1], conc_df_tr1[,1], family = 'binomial', lambda = 0 )
predicted2 <-as.numeric(predict.glmnet(logist2, as.matrix(conc_df_te[,-1])) >= thresh)
actual <- conc_df_te1$class 
#Confusion matrix
table(predicted2, actual)
#Accuracy of predictions
mean(predicted2==actual)


#b) An alternative way is to train a logistic regression model with lasso penalties.This will require you setting of penalization term.
#Use 10-fold cross-validation to determine your ideal lambda level based on binomial deviance. Once you determine your best lambda value using 10-fold
#cross-validation, perform classification on test data similar to part a and compare your results. Comment on the regression coefficients. Is there
#any interesting information? Try to interpret the model.
set.seed(9)
cv_res <- cv.glmnet(as.matrix(conc_df_tr1[,-1]), conc_df_tr1[,1], family = 'binomial', nfolds = 10)
logist3 <- glmnet(conc_df_tr1[,-1], conc_df_tr1[,1], family = 'binomial', lambda = cv_res$lambda.min )
predicted3 <-as.numeric(predict.glmnet(logist3, as.matrix(conc_df_te[,-1])) >= thresh)
#Confusion matrix
table(predicted3, actual)
#Accuracy of predictions
mean(predicted3==actual)
plot(1:length(coefficients(logist2)),(coefficients(logist2)),xlab = "Coefficient Indices", ylab = "Coefficients")
plot(1:length(coefficients(logist3)),(coefficients(logist3)),xlab = "Coefficient Indices", ylab = "Coefficients")
#Accuracy of the predictions increased and there are lots of coefficients with values of zero in lasso model compared to logistic regression.
#Moreover most of the significant features are from the x-axis in lasso model. It can be seen from the plot above. Most of the covariates with non-zero
#coefficients are coming from first 315 observations. It makes sense because gesture of Class 3 is a straight which is on the x axis. So it makes sense.


#c) Given this information, you are expected to transform your training data to distance information (i.e. N by N matrix). Note that you need to perform a
#similar transformation to your test data. In other words, you need to calculate the distance of each test instance to training instance to obtain a distance
#based representation for your test data. You can use Euclidean distance as your distance measure. Perform the same training and test strategy as in part
#b but use the distances as your new feature matrices. Comment on the regression coefficients. What do they imply under this new representation setting?
dist_train <- Distance_for_KNN_test(conc_df_tr1[,-1],conc_df_tr1[,-1])
dist_test <- Distance_for_KNN_test(conc_df_te1[,-1],conc_df_tr1[,-1])
set.seed(10)
cv_res2 <- cv.glmnet(as.matrix(dist_train), conc_df_tr1[,1], family = 'binomial', nfolds = 10)
logist4 <- glmnet(dist_train, conc_df_tr1[,1], family = 'binomial', weights = NULL, offset = NULL, lambda = 0.0035)
predicted4 <-as.numeric(predict.glmnet(logist4, as.matrix(dist_test)) >= thresh)
#Confusion matrix
table(predicted4, actual)
#Accuracy of the predictions
mean(predicted4==actual)
plot(1:length(coefficients(logist4)),(coefficients(logist4)),xlab = "Coefficient Indices", ylab = "Coefficients")
#Now, regression coefficients are the scalars of the distances between test instance and training instances.
#As seen from the plot, regression coefficients are very small numbers. Because we use distance information and we have high dimensional data.
#As dimensionality increases, distance information become useless. This model is almost an intercept only model.
#Other than that, model has very high prediction accuracy because we can use nonlinear information.


#d) Provide an overall comparison on the results you obtain for each part (over all tasks). You can compare test accuracy of each alternative method you
#developed.
#Accuracy for knn with euclidian distance: 0.946
#Accuracy for knn with manhattan distance: 0.0.956
#Accuracy for logistic regression with no penalty: 0.897
#Accuracy for logistic regression with lasso penalty: 0.942
#Accuracy for the logistic regression with lasso penalty and with distance information as feature matrix: 0.969
#Model with worst performance is logistic regression with no penalty. Logistic regression with lasso penalty do better job than logistic regression without
#penalty because this method has embedded feature selection and tries to find the best model.
#KNN models are better than logistic regression models mentioned above. Because data is nonlinear and knn is a better choice when data is nolinear.
#lastly, when we keep nonlinearity information in the logistic regression by using distance matrix as feature matrix, this model outperforms all models.


logist4
logist3


