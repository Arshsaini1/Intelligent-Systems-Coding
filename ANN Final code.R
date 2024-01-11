str(bank.additional)
summary(bank.additional)
bank.additional<-subset(bank.additional[c(1,2,3,5,6,21)])
colnames(bank.additional)
class(bank.additional)
barplot(table(bank.additional$y))
bank.additional$age<-as.numeric(bank.additional$age)
bank.additional$age<-(bank.additional$age-min(bank.additional$age))/(max(bank.additional$age)-min(bank.additional$age))

bank.additional$job<-as.numeric(bank.additional$job)
bank.additional$job<-(bank.additional$job-min(bank.additional$job))/(max(bank.additional$job)-min(bank.additional$job))

bank.additional$marital<-as.numeric(bank.additional$marital)
bank.additional$marital<-(bank.additional$marital-min(bank.additional$marital))/(max(bank.additional$marital)-min(bank.additional$marital))

bank.additional$default<-as.numeric(bank.additional$default)
bank.additional$default<-(bank.additional$default-min(bank.additional$default))/(max(bank.additional$default)-min(bank.additional$default))

#bank.additional$day_of_week<-as.numeric(bank.additional$day_of_week)
#bank.additional$day_of_week<-(bank.additional$day_of_week-min(bank.additional$day_of_week))/(max(bank.additional$day_of_week)-min(bank.additional$day_of_week))

bank.additional$housing<-as.numeric(bank.additional$housing)
bank.additional$housing<-(bank.additional$housing-min(bank.additional$housing))/(max(bank.additional$housing)-min(bank.additional$housing))

#bank.additional$loan<-as.numeric(bank.additional$loan)
#bank.additional$loan<-(bank.additional$loan-min(bank.additional$loan))/(max(bank.additional$loan)-min(bank.additional$loan))

bank.additional$y<-as.numeric(bank.additional$y)
bank.additional$y<-(bank.additional$y-min(bank.additional$y))/(max(bank.additional$y)-min(bank.additional$y))
str(bank.additional)

set.seed(1234)
ind <- sample(2, nrow(bank.additional), replace = TRUE, prob = c(0.7, 0.3))
train <- bank.additional[ind == 1, ]
test <- bank.additional[ind == 2, ]

library(neuralnet)

# Use more hidden layers and neurons for better flexibility
set.seed(1234)
n <- neuralnet(y ~ age + job + marital + default + housing,
               train,
               hidden = c(5, 5),  # Increase the number of neurons in each hidden layer
               err.fct = "sse",
               linear.output = FALSE,
               rep = 10,  # Increase the number of training epochs
               algorithm = "rprop+",
               lifesign = "full")

# Plot the neural network to check for convergence
plot(n)

# Evaluate model on the test set
output_test <- compute(n, test[, -5])
p_test <- output_test$net.result
pred_test <- ifelse(p_test > 0.5, 1, 0)
tab_test <- table(pred_test, test$y)

accuracy_test <- sum(diag(tab_test)) / sum(tab_test)
error_rate_test <- 1 - accuracy_test

# Display test set performance
cat("Test Set Accuracy: ", accuracy_test, "\n")
cat("Test Set Error Rate: ", error_rate_test, "\n")