# Detection of next holiday location


# Since no data set is provided so I am just telling the algorithm.


# Some given parameters are weather,price, flight time, hotel price and I would like
# to add some more parameters like person's budget so that suitable flight can be 
# allocated to him,also it will have a good corelation with hotel price,
# also whether he would like to go for family holiday which will have a good corelation with flight time and area of hotel.
# Our outcome variable will be Location.

# First of all whatever data set is given , divide it into two parts one for model fittting
# and other for checking the prediction of fitted model.
# use command - 
intrain <- createDataPartition(y = data_set_name$Location,p = 0.7,list = FALSE)
# in above command p=0.7 means we are breaking the data set in the ratio of 70:30
training <- loan_data4[intrain,]
testing <- loan_data4[-intrain,]
# training will be 70% of our data and testing will be 30%
# training will be used only for model fitting and 
# testing will be used for prediction.
# To make any plots you can use qplot() function which is available in ggplot2 package
qplot(x_axis,y_axis,dataset_name)
# Now you can apply some machine learning algorithms like decision tree,
# random forest, gradient boosting,Generalised Additive Model(GAM),etc.

# First let us apply decision tree algorithm
# use train() function, in that use method = "rpart" and subsampling is of crossvalidation
mod1 <- train(Location~.,method = "rpart",data = training,trControl = trainControl(method = "cv"))
# To know details of above model use below command 
mod1
# To make a plot of above splitting use fancyRpartPlot() function which is available in rattle package
fancyRpartPlot(mod1$finalModel)
mod1$finalModel
# Now let us apply prediction of above model fitting using predict() function
pre1 <- predict(mod1,newdata = testing)
# Here pre1 contains the predicted values of above model fitting

# Now let us apply random forest algorithm , just use method = "rf"
mod2 <- train(Location~.,method = "rf",data = training,trControl = trainControl(method = "cv"),prox = TRUE)
pre2 <- predict(mod2,newdata = testing)

# To apply gradient boosting use method = "gbm" in train() function , for using GBM packes like gbm,survival,splines,parallel,plyr shoul be installed
mod4 <- train(Location~.,method = "gbm",data = training,trControl = trainControl(method = "cv"),verbose = FALSE)
pre4 <- predict(mod4,newdata = testing)


# For better predictions you can also combine highly correlated variables using Principal Component Analysis(PCA).

m <- abs(cor(training[,-(column_number_of_outcome_variable)]))
# cor() means finding corelation and abs() means absolute value of it
diag(m) <- 0
# it is showing that we have set corelation of variable with itself as 0, since it will always be 1
which(m > 0.8,arr.ind = TRUE)
# now we are selecting those variables which have m value greater than 0.8
# Now we will try to combine these pairs and make them into their respective one variable
# Suppose we have found that column 3 and 5 are highly correlated i.e their cor() value is freater than 0.8
small1 <- training[,c(5,3)]
prcmp1 <- prcomp(small1)
# we are applying principle component function in small1 to see how we can combine these 2 variables
prcmp1$rotation
# To see the best two combination we have used above command
# Now simply combine those two variables according to PC1 which is giving best combination.
# After combining them into a new variable, remove those variables from the data set

# Now if you will apply Machine Learning algorithms on this new data set, you will observe improved prediction.
