str(bank.additional)
summary(bank.additional)
library(rpart)
library(randomForest)

# Load marketing data
Mydata <- bank.additional

# Split data into training (80%) and validation (20%)
A <- sort(sample(nrow(Mydata), nrow(Mydata)*.8)) 
Train <- Mydata[A,]  # train data
Val <- Mydata[-A,] # validation data

# Step 2
# load the library randomForest
library(randomForest) 
# build a Random Forest of 10 DTs with outcome y on the train data
rf <- randomForest(y ~ ., data = Train, ntree=30,mtry=20,sampsize=c(900,500))

# plot importance of variables
varImpPlot(rf) 
print(rf)
plot(rf)
print(rf)
# Step 3
Yt=predict(rf,Val)# predict results on the validation data
conf.matrix <- table(Val$y, Yt)  # build a confusion matrix
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix))
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix))
print(conf.matrix)
acc=mean(Val$y==Yt) # accuracy
tp=sum(Val$y=='yes' & Yt=='yes')/sum(Val$y=='yes') # true positive rate
tn=sum(Val$y=='no' & Yt=='no')/sum(Val$y=='no') # true negative rate
fp=sum(Val$y=='no' & Yt=='yes')/sum(Val$y=='no') # false positive rate
fn=sum(Val$y=='yes' & Yt=='no')/sum(Val$y=='yes') # false negative rate
sprintf("Accuracy:%.2f, TP:%.2f, TN:%.2f",acc,tp,tn)# formatted printing
sprintf("Error rates: FP:%.2f, FN:%.2f",fp,fn)

