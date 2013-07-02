library(e1071)

prepare.classes <- function(x) {
    x[x==1] <- 'N'
    x[x!='N'] <- 'A'
    x <- as.factor(x)
    return(x)
}

x.train <- zero.elements[,1:279]
y.train <- prepare.classes(zero.elements[,280])

tuned <- tune.svm(x=x.train, y=y.train, kernel="radial", cost=10^(-1:1), gamma=10^(-6:-1))
