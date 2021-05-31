## import dataset
blood_test <- read.table(file="/mnt/c/Users/Nnamdi/Desktop/Bioinformatics/Univariate_data_modelling/Exercises/DATASETS/BLOOD.txt", header  = T, sep=",")
head(blood_test)

### Exploratory data analysis
summary(blood_test)
hist(blood_test$testost)
# remove the missing data
blood.df <- subset(blood_test, blood_test$testost!='999',)
summary(blood.df$testost)
hist(blood.df$testost)

#Simple logistic regression
blood.glm <- glm(case ~ testost, family = binomial(link=logit), data=blood.df)
summary(blood.glm)

# Odds ratio
exp(blood.glm$coefficients)

### Model diagnostics
dev <- (blood.glm$null.deviance - blood.glm$deviance)/blood.glm$null.deviance
dev

### ROC curve
library("ROCR")
predict <- fitted(blood.glm)
pred <- prediction(predict, blood.df$case)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main="sensitivity vs false positive rate",colorize=TRUE)


## Multiple logistic regression
blood.glm2 <- glm(case ~ testost +age, family = binomial(link=logit), data=blood.df)
summary(blood.glm2)

# Odds ratio
exp(blood.glm2$coefficients)

## Model diagnostics - deviance of the model
dev2 <- (blood.glm2$null.deviance - blood.glm2$deviance)/blood.glm2$null.deviance
dev2

# ROC curve
predict <- fitted(blood.glm2)
pred <- prediction(predict, blood.df$case)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main="sensitivity vs false positive rate", colorize=TRUE)

### Comparison of model 1 and model 2
diff.dev <- blood.glm$deviance - blood.glm2$deviance
1-pchisq(diff.dev,1)



