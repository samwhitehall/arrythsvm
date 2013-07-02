library(e1071)

all <- prepare.classes(zero.elements)
all <- ordered.partition(all, 0.7)

x.train <- all$train[,1:279]
y.train <- all$train[,280]

x.test <- all$test[,1:279]
y.test <- all$test[,280]

basic.classifier <- svm(x=x.train, y=y.train,
                        kernel='linear', type='C', scale=TRUE)

predictions <- predict(basic.classifier, newdata=x.test)

pred.tab <- table(pred=predictions, true=y.test)
