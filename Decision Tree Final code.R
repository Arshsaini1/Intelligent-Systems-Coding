str(bank.additional)
summary(bank.additional)
# Step 1
# Load marketing data
Mydata <- bank.additional
A = sort(sample(nrow(Mydata), nrow(Mydata)*.8)) 
Train<-Mydata[A,]  # train data
Val<-Mydata[-A,] # validation data

# Step 2
# load the library rpart which is Decision trees (DT)
library(rpart) 
# build a DT using complexity parameter 
complexity=0.005
split_size=700
depth=6
error_costs=matrix(c(0,8,8,0))
mtree <- rpart(y ~ ., data = Train, method="class", control = rpart.control(cp=complexity))
#plot tree 
plot(mtree)
text(mtree, pretty=FALSE, cex=.7)

# Step 3 Confusion Matrix
Yt=predict(mtree,Val,type="class") # predict results on the validation data
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

