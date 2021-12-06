library(readr)
library(class)
install.packages("naivebayes")
install.packages("MLmetrics")
library(naivebayes)
library(MLmetrics)

#Load datasets
dota2_1 <- read_csv("/dota2Train.csv")
dota2_2 <-read_csv("/dota2Test.csv")
dota2_3 <- dota2_2
colnames(dota2_3) <- colnames(dota2_1)
dota2dataset <- rbind(dota2_1, dota2_3)
LoLdataset <- read_csv("/LoLdataset.csv")


ran1 <- sample(1:nrow(dota2dataset), 0.9*nrow(dota2dataset))
ran2 <- sample(1:nrow(LoLdataset), 0.9*nrow(LoLdataset))


nor <- function(x) { (x-min(x))/(max(x)-min(x)) }


dota2_norm <- as.data.frame(lapply(dota2dataset, nor))
LoL_norm <- as.data.frame(lapply(LoLdataset, nor))


summary(dota2_norm)
summary(LoL_norm)

d2Train <- dota2_norm[ran1,]
d2Test <- dota2_norm[-ran1,]
LoLTrain <- LoL_norm[ran2,]
LoLTest <- LoL_norm[-ran2,]


d2_target_category <- dota2dataset[ran1,1]
LoL_target_category <- LoLdataset[ran2,2]


d2_test_category <- dota2dataset[-ran1,1]
LoL_test_category <- LoLdataset[-ran2,2]


pr1 <- knn(d2Train[,-1], d2Test[,-1], cl=d2_target_category[,1], k = 100)
pr2 <- knn(LoLTrain, LoLTest, cl=LoL_target_category, k = 100)


accuracy <- function(x) { sum(diag(x)/(sum(rowSums(x))))*100}


table(pr1, d2_test_category)
tab1 <- table(pr1, d2_test_category)
accuracy(tab1)


table(pr2, LoL_test_category)
tab2 <- table(pr2, LoL_test_category)
accuracy(tab2)




