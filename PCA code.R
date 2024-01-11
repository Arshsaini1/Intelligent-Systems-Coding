# load iris data
library(datasets)
data(iris)
# print first 10 rows of iris data
head(iris, n=10)
# scatter plot of iris data
plot(iris, pch=21, bg=c("red","green3","blue")[unclass(iris$Species)])







# load neural net library
library(nnet)
# create train labels: convert the text iris species to numeric class labels
Train_lab <- class.ind(iris$Species)
# set seed for random number generator for repeatable results
set.seed(1)
# Create indexes for training (70%) and validation (30%) data
A <- sort(sample(nrow(iris), nrow(iris)*.7))
# train neural net 
# iris[A,c(1:4)] is to select the first 4 variables as inputs
# size=5 for 5 hidden units, maxit=100 to train for 100 iterations
iris_net <- nnet(iris[A,c(1:2)], Train_lab[A,], size=5, maxit=100, softmax=TRUE)
# test
Yt <- predict(iris_net, iris[-A,c(1:2)], type="class")
# build a confusion matrix
conf.matrix <- table(iris[-A,]$Species, Yt)  
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix))
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix))
print(conf.matrix)
