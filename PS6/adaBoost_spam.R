# setwd("/media/balint/Storage/Tanulas/Advanced_Computational_Methods/PS6")
# setwd("D:/Tanulas/Advanced_Computational_Methods/PS6")

source("adaBoost.R")
library(gbm)



spam <- read.csv("Spam/spambase.data")

#create training and test sets
set.seed(1113)
train.index <- sample(nrow(spam), 4000)
spam.train <- spam[train.index,]
spam.test <- spam[-train.index,]

#create prediction function (I use the fact that the spam data has 0-s)
predict.adaBoost <- function(trees, alphas, data){
    
    N <- nrow(data)
    noTrees <- length(trees)
    predictions <- matrix(NA, N, noTrees)
    final.classifier <- rep(0, N)
    
    for (m in 1:noTrees){
        cat(m, '\n')
        pred <- as.numeric(as.character(predict(trees[[m]], data, type="class")))
        final.classifier <- final.classifier + alphas[m]*pred
    }
    
    ret <- sign(-final.classifier)
    ret[ret==-1] <- 0
    return(list(predLabels = ret))
    
}

my.test <- rep(NA, 100)
gbm.test <- rep(NA, 100)
errorrates <- data.frame(my.test, gbm.test)
for(iterations in 1:100){
    cat("iterations:", iterations, "\n")
    model.my <- adaBoost(X1 ~., spam.train, 5, iterations)
    pred.my <-predict.adaBoost(trees = model.my$trees, alphas = model.my$alphas, data = spam.test)
    errorrates$my.test[iterations] <- sum(pred.my[[1]]!=spam.test$X1)/length(spam.test$X1)

    model.gbm <- gbm(X1 ~ ., data=spam.train, dist="adaboost", n.tree = iterations, shrinkage=1)
    pred.gbm <- sign(predict(model.gbm, spam.test, n.tree=iterations))
    pred.gbm[pred.gbm==-1] <- 0
    errorrates$gbm.test[iterations] <- sum(pred.gbm!=spam.test$X1)/length(spam.test$X1)
}    


cairo_pdf("adaBoost.pdf")
plot(errorrates$gbmadaboost, col = "red", type="l")
points(errorrates$myadaboost, col = "blue", type="l")
dev.off()
