lol <- read.csv("lol.csv")
> lol <- select(lol,-1) %>% 
+ mutate_at('blueWins',as.factor)

set.seed(123)
train <- strata(lol,'blueWins',size = rev(round(table(lol$blueWins)*0.7)))
select<-sample(1:nrow(lol),length(lol$blueWins)*0.7)
> train=lol[select,]
> test=lol[-select,]

control <- trainControl(method = 'cv',number = 10)
> model <- train(blueWins~.,train,
+ method = 'knn',
+ preProcess = c('center','scale'),
+ trControl = control,
+ tuneLength = 5)
> 
> model

truth <- test$blueWins
> pre <- predict(model,newdata = test)
> knnroc <- roc(test$blueWins, as.numeric(pre))
plot(knnroc, print.auc = TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
+ grid.col=c("green", "red"), max.auc.polygon=TRUE,
+ auc.polygon.col="skyblue", print.thres=TRUE)

print(cft <- table(pre, truth))
tp <- cft[2, 2]
> tn <- cft[1, 1]
> fp <- cft[2, 1]
> fn <- cft[1, 2]
> print(accuracy <- (tp + tn)/(tp + tn + fp + fn))
> print(sensitivity <- tp/(tp + fn))
> print(specificity <- tn/(tn + fp))