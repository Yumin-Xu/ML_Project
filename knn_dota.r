library(caret)
library(pROC)
library(rpart)
library(do)
library(dplyr)
library(rpart.plot)

dota <- read.csv("dota.csv")

dota <- dota[,-grep("X0.19",colnames(dota))]
dota <- dota[,-grep("X0.97",colnames(dota))]

names(dota)=c("y","x1","x2","x3","x4","x5","x6","x7","x8","x9","x10"
               ,"x11","x12","x13","x14","x15","x16","x17","x18","x19","x20"
               ,"x21","x22","x23","x24","x25","x26","x27","x28","x29","x30"
               ,"x31","x32","x33","x34","x35","x36","x37","x38","x39","x40"
               ,"x41","x42","x43","x44","x45","x46","x47","x48","x49","x50"
               ,"x51","x52","x53","x54","x55","x56","x57","x58","x59","x60"
               ,"x61","x62","x63","x64","x65","x66","x67","x68","x69","x70"
               ,"x71","x72","x73","x74","x75","x76","x77","x78","x79","x80"
               ,"x81","x82","x83","x84","x85","x86","x87","x88","x89","x90"
               ,"x91","x92","x93","x94","x95","x96","x97","x98","x99","x100"
               ,"x101","x102","x103","x104","x105","x106","x107","x108","x109","x110"
               ,"x111","x112","x113","x114","x115")

a <- 4*dota$x4+5*dota$x5+6*dota$x6+7*dota$x7+8*dota$x8+9*dota$x9+10*dota$x10+
     11*dota$x11+12*dota$x12+13*dota$x13+14*dota$x14+15*dota$x15+16*dota$x16+17*dota$x17+18*dota$x18+19*dota$x19+20*dota$x20+
     21*dota$x21+22*dota$x22+23*dota$x23+24*dota$x24+25*dota$x25+26*dota$x26+27*dota$x27+28*dota$x28+29*dota$x29+30*dota$x30+
     31*dota$x31+32*dota$x32+33*dota$x33+34*dota$x34+35*dota$x35+36*dota$x36+37*dota$x37+38*dota$x38+39*dota$x39+40*dota$x40+
     41*dota$x41+42*dota$x42+43*dota$x43+44*dota$x44+45*dota$x45+46*dota$x46+47*dota$x47+48*dota$x48+49*dota$x49+50*dota$x50+
     51*dota$x51+52*dota$x52+53*dota$x53+54*dota$x54+55*dota$x55+56*dota$x56+57*dota$x57+58*dota$x58+59*dota$x59+60*dota$x60+
     61*dota$x61+62*dota$x62+63*dota$x63+64*dota$x64+65*dota$x65+66*dota$x66+67*dota$x67+68*dota$x68+69*dota$x69+70*dota$x70+
     71*dota$x71+72*dota$x72+73*dota$x73+74*dota$x74+75*dota$x75+76*dota$x76+77*dota$x77+78*dota$x78+79*dota$x79+80*dota$x80+
     81*dota$x81+82*dota$x82+83*dota$x83+84*dota$x84+85*dota$x85+86*dota$x86+87*dota$x87+88*dota$x88+89*dota$x89+90*dota$x90+
     91*dota$x91+92*dota$x92+93*dota$x93+94*dota$x94+95*dota$x95+96*dota$x96+97*dota$x97+98*dota$x98+99*dota$x99+100*dota$x100+
     101*dota$x101+102*dota$x102+103*dota$x103+104*dota$x104+105*dota$x105+106*dota$x106+107*dota$x107+108*dota$x108+109*dota$x109+110*dota$x110+
     111*dota$x111+112*dota$x112+113*dota$x113+114*dota$x114

dota <- cbind(dota,a)
dota <- data.frame(dota$y,dota$x1,dota$x2,dota$x3,dota$a)
names(dota)=c("y","x1","x2","x3","x4")
dota$y <- Replace(data=dota$y,from = c("^-1$"),to = 0)
dota$y=as.factor(dota$y)

set.seed(123)
select<-sample(1:nrow(dota),length(dota$X1.0)*0.7)
train=dota[select,]
test=dota[-select,]

control <- trainControl(method = 'cv',number = 10)
model <- train(y~x1+x2+x3+x4,train,
method = 'knn',
preProcess = c('center','scale'),
trControl = control,
tuneLength = 100)
 
model

truth <- test$y
pre <- predict(model,newdata = test)
knnroc <- roc(test$y, as.numeric(pre))

plot(knnroc, print.auc = TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
grid.col=c("green", "red"), max.auc.polygon=TRUE,
auc.polygon.col="skyblue", print.thres=TRUE)

print(cft <- table(pre, truth))
tp <- cft[2, 2]
tn <- cft[1, 1]
fp <- cft[2, 1]
fn <- cft[1, 2]
print(accuracy <- (tp + tn)/(tp + tn + fp + fn))
print(sensitivity <- tp/(tp + fn))
print(specificity <- tn/(tn + fp))



    