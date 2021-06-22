#Required libraries - 
library(rpart)
library(rpart.plot)
library(caret)
library(ggplot2)


#Load the dataset - 
data("iris")
head(iris)
str(iris)
levels(iris$Species)

#Check for any missing values:
colSums(is.na(iris))
#No missing values.

summary(iris)


#Create training and test datasets - 

set.seed(46465)

split=sample(nrow(iris),0.7*nrow(iris))          

training_data = iris[split,]
test_data = iris[-split,]

#Create a decision tree model - 
classifier = rpart(Species ~ . ,training_data, method = "class");classifier

#Visualization - 

plot_tree= prp(classifier,type=0,extra=4,main = "Decision Tree",tweak = 1.2,
               box.palette = list("Red","skyblue","Green"),cex.main=2,yesno = 2,varlen = 0,
               faclen = 0,split.border.col ="black",branch.lwd = 2,legend.cex = 0.94,
               round = 2,nn.cex = 1.1,yesno.yshift = -1.4,split.lwd = 2,branch = 0.1)


#Confusion matrix - 
pred = predict(classifier, test_data[-5],type = "class")
cm_test = confusionMatrix(pred,test_data[,5]);cm_test


#Checking for over-fitting and under-fitting of model - 
#1. Accuracy of the model for training data and test data:

pred2 = predict(classifier, training_data[-5],type = "class")
cm_train = confusionMatrix(pred2,training_data[,5]);cm_train

acc = data.frame("Training Data"=cm_train$overall["Accuracy"],
                 "Testing Data"=cm_test$overall["Accuracy"]);acc


#2.Using Complexity parameter(cp):

##Visual representation of cross-validation results
plotcp(classifier,lty = 5,col="red",upper="splits")

#Optimum no. of splits = 2
#Conclusion - No over-fitting or under-fitting.Pruning of the tree is not needed.


#Predicting the class for an observation - 
set.seed(99432)
a=apply(iris[,-5],2,function(x) sample(seq(min(x),max(x),by=0.1),size=1))
d=data.frame(matrix(a,nrow=1,byrow = TRUE),check.names = FALSE)
names(d)=names(a);d

pr=predict(classifier,d,type="class");pr


#Visualization using Scatter Plot - 
plot_scatter=ggplot(test_data, aes(x=Petal.Length,y=Petal.Width,colour=Species))+
  geom_point(size = 4,shape=18)+
  geom_vline(xintercept = 2.5,colour="orange",size = 1)+
  theme_classic()+
  ggtitle("Scatter Plot")+
  theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5,size = 20),
        legend.title = element_blank(),legend.text = element_text(size = 16),
        axis.title = element_text(size = 16),axis.text = element_text(size = 15))+
  geom_segment(aes(x = 2.5, y = 1.8,xend = 7,yend = 1.8),colour="orange",size=1);plot_scatter



#Conclusion - This decision tree classifier can classify different flowers
#             into 3 species on the basis of variables petal length and petal width   
#             for classification with accuracy 95%.