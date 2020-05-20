# Author: Advait Ramesh Iyer
# Case 3
# Financial Analytics
# SUID: 330623485
# NetID: aiyer01

# Look at the objects in the workspace.
objects() # [1] "bank1" "bank2" "DIS"   "GSPC"  "IRX"  

# Check out the training and test datasets. (bank1 for training, and bank2 for test)
head(bank1)
head(bank2)

install.packages("C50")
library(C50)

# Question 1
set.seed(13)
C5.0(STATUS~., data = bank1, trials=10) -> mytrees

# Question 2
predict(mytrees, newdata = bank2[,-1]) -> treepred

# Question 3 & 4
library(caret)
confusionMatrix(treepred,bank2[,1]) -> cf1
cf1

# Question 5
set.seed(13)
train(STATUS~., data = bank1, method='knn',preProcess=c('center','scale'), tuneLength=20) -> myknn

# Question 6
myknn # Optimal k=43

# Question 7
predict(myknn,newdata = bank2[,-1]) -> knnpred

# Question 8 & 9
confusionMatrix(knnpred,bank2[,1]) -> cf2
cf2

# Question 10
train(STATUS~., data = bank1, method='bayesglm') -> myglm

# Question 11
predict(myglm, newdata = bank2[,-1]) -> glmpred

# Question 12
confusionMatrix(glmpred,bank2[,1]) -> cf3
cf3

# Question 15
data.frame(X1=0.4, X2=0.05, X3=-0.01, X4=-0.02, X5=0.3,X6=-0.06,X7=0.1) -> mycompany

# Question 16
predict(mytrees,newdata = mycompany,trials = 10, type = 'prob')
predict(myknn,newdata = mycompany)
predict(myglm,newdata = mycompany, type = 'prob')

