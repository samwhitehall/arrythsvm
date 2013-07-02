library(e1071)

all <- ordered.partition(zero.elements, 0.7)

prepare.classes <- function(x) {
    x[x==1] <- 'N'
    x[x!='N'] <- 'A'
    x <- as.factor(x)
    return(x)
}

x.train <- all$train[,1:279]
y.train <- prepare.classes(all$train[,280])

x.test <- all$test[,1:279]
y.test <- prepare.classes(all$test[,280])

basic.classifier <- svm(x=x.train, y=y.train,
                        kernel='linear', type='C', scale=FALSE)

predictions <- predict(basic.classifier, newdata=x.test)

pred.tab <- table(pred=predictions, true=y.test)
