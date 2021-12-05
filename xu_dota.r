> library(caret)
> library(pROC)
> library(rpart)
> library(do)
> library(dplyr)
> library(rpart.plot)
> 
> dota <- read.csv("dota.csv")
> View(dota)
> dota = tibble::rowid_to_column(dota,"id")
> names(dota)=c("id","y","x1","x2","x3","x4","x5","x6","x7","x8","x9","x10"
+               ,"x11","x12","x13","x14","x15","x16","x17","x18","x19","x20"
+               ,"x21","x22","x23","x24","x25","x26","x27","x28","x29","x30"
+               ,"x31","x32","x33","x34","x35","x36","x37","x38","x39","x40"
+               ,"x41","x42","x43","x44","x45","x46","x47","x48","x49","x50"
+               ,"x51","x52","x53","x54","x55","x56","x57","x58","x59","x60"
+               ,"x61","x62","x63","x64","x65","x66","x67","x68","x69","x70"
+               ,"x71","x72","x73","x74","x75","x76","x77","x78","x79","x80"
+               ,"x81","x82","x83","x84","x85","x86","x87","x88","x89","x90"
+               ,"x91","x92","x93","x94","x95","x96","x97","x98","x99","x100"
+               ,"x101","x102","x103","x104","x105","x106","x107","x108","x109","x110"
+               ,"x111","x112","x113","x114","x115","x116")
> dota$y <- Replace(data=dota$y,from = c("^-1$"),to = 0)

> dota$y=as.factor(dota$y)
> select<-sample(1:nrow(dota),length(dota$id)*0.7)
> train=dota[select,]
> test=dota[-select,]

> tc<-rpart.control(minsplit = 100,minbucket = 20,maxdepth = 30,xval =10,cp = 0.00005)
> formular=y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+
+     x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+
+     x21+x22+x23+x24+x25+x26+x27+x28+x29+x30+
+     x31+x32+x33+x34+x35+x36+x37+x38+x39+x40+
+     x41+x42+x43+x44+x45+x46+x47+x48+x49+x50+
+     x51+x52+x53+x54+x55+x56+x57+x58+x59+x60+
+     x61+x62+x63+x64+x65+x66+x67+x68+x69+x70+
+     x71+x72+x73+x74+x75+x76+x77+x78+x79+x80+
+     x81+x82+x83+x84+x85+x86+x87+x88+x89+x90+
+     x91+x92+x93+x94+x95+x96+x97+x98+x99+x100+
+     x101+x102+x103+x104+x105+x106+x107+x108+x109+x110+
+     x111+x112+x113+x114+x115+x116
> rpart.mod=rpart(formular,data = train,method = "class",
+ parms = list(prior=c(0.6,0.4),loss=matrix(c(0,1,2,0),nrow=2),split="gini"),
+ control = tc)
> plotcp(rpart.mod)
> 
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
> test$pre[which(test$pre_p>0.394)]=1
> print(cft <- table(test$pre, test$y))
> tp <- cft[2, 2]
> tn <- cft[1, 1]
> fp <- cft[2, 1]
> fn <- cft[1, 2]
> print(accuracy <- (tp + tn)/(tp + tn + fp + fn))

> print(sensitivity <- tp/(tp + fn))

> print(specificity <- tn/(tn + fp))
