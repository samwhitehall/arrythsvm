the.data <- read.table("arrhythmia.data.txt", sep=",", na.strings="?")

# separate into input vector & output class
arrhythmia.data <- the.data[,-280]
arrhythmia.out <- as.factor(the.data[,280])

# the 68/452 rows with full data
only.complete.rows <- na.omit(the.data)

# replace NAs with 0
zero.elements <- as.matrix(the.data)
zero.elements[which(is.na(the.data))] <- 0

# scale each column to similar range; subtract mean, divide by std.dev
normalise.range <- function(x) {
    return(scale(x, center=TRUE, scale=TRUE))
}

# choose records in order, returns x$train and x$test
ordered.partition <- function(x, train.proportion) {
    marker <- round(train.proportion*nrow(x))
    train.set<- x[seq(1,marker),]
    test.set <- x[seq(marker+1,nrow(x)),]
    return(list("train"=train.set, "test"=test.set))
}

# choose random records; returns x$train and x$test
random.partition <- function(x, train.proportion) {
    train.indexes <- sample(1:nrow(x), 
                        round(train.proportion*nrow(x)), 
                        replace=FALSE)
    test.indexes <- setdiff(seq(1,nrow(x)), train.indexes)

    return(list("train"=x[train.indexes,], 
                "test"=x[test.indexes,]))
}

prepare.classes <- function(x) {
    x$V280[x$V280==1] <- 'N' # normal
    x$V280[x$V280!='N'] <- 'A' # arrhythmic
    x$v280 <- as.factor(x$V280) # set to factor type..
    return(x)
}
