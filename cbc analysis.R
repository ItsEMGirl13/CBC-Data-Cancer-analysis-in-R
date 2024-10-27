cb= read.csv("CBR.csv")
View(cb)
library(dplyr)
library(caTools)
split= sample.split(cb$Disease, SplitRatio = 0.80)
split
train= subset(cb,split == "TRUE")
nrow(train)
test= subset(cb,split == "FALSE")
nrow(test)
model = glm(Disease~H_HCT+H_PLT+H_PDW+H_HGB, data = train, family = "binomial")
summary(model)
library(car)
vf=vif(model)
vf
mv=vf>5
mv
pred = predict(model,test)
pred
library(pROC)
pred <- unlist(pred)  
y= test$Disease
y <- as.vector(y)     
roc_curve <- roc(y, pred)
plot(roc_curve, main = "ROC Curve")
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))
coords(roc_curve, "best", ret = "threshold")
