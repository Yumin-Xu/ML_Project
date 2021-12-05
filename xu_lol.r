> library(caret)
> library(pROC)
> library(rpart)
> library(do)
> library(dplyr)
> library(rpart.plot)
> 
> lol <- read.csv("lol.csv")
> View(lol)
> lol = tibble::rowid_to_column(lol,"id")
> names(lol)=c("id","y","x1","x2","x3","x4","x5","x6","x7","x8","x9","x10"
+               ,"x11","x12","x13","x14","x15","x16","x17","x18","x19","x20"
+               ,"x21","x22","x23","x24","x25","x26","x27","x28","x29","x30"
+               ,"x31","x32","x33","x34","x35","x36","x37","x38")

> lol$y=as.factor(lol$y)
> select<-sample(1:nrow(lol),length(lol$id)*0.7)
> train=lol[select,]
> test=lol[-select,]

> tc<-rpart.control(minsplit = 50,minbucket = 20,maxdepth = 30,xval =10,cp = 0.001)
> formular=y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+
+     x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+
+     x21+x22+x23+x24+x25+x26+x27+x28+x29+x30+
+     x31+x32+x33+x34+x35+x36+x37+x38
> rpart.mod=rpart(formular,data = train,method = "class",
+ parms = list(prior=c(0.6,0.4),loss=matrix(c(0,1,2,0),nrow=2),split="gini"),
+ control = tc)
> rpart.mod$cp
> plotcp(rpart.mod)

> rpart.mod.pru<-prune(rpart.mod,cp=rpart.mod$cptable[which.min(rpart.mod$cptable[,"xerror"])])
> rpart.plot(rpart.mod.pru,branch=1,extra = 102,under = TRUE,faclen = 0,cex = 0.7,main="CART")

> rpart.mod.pru$variable.importance
> 
> test.pre<-predict(rpart.mod.pru,test)
> plot(roc(test$y,test.pre[,2]), print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
+ grid.col=c("green", "red"), max.auc.polygon=TRUE,
+ auc.polygon.col="skyblue", print.thres=TRUE)

> test$pre_p<-predict(rpart.mod.pru,test)[,2]
> test$pre=0
> test$pre[which(test$pre_p>0.356)]=1
> print(cft <- table(test$pre, test$y))
> tp <- cft[2, 2]
> tn <- cft[1, 1]
> fp <- cft[2, 1]
> fn <- cft[1, 2]
> print(accuracy <- (tp + tn)/(tp + tn + fp + fn))

> print(sensitivity <- tp/(tp + fn))

> print(specificity <- tn/(tn + fp))
