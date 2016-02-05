library(kknn)

mnist_train <- read.csv("MNIST_training.csv")
mnist_test <- read.csv("MNIST_test.csv")
mnist_train$X4 <- as.factor(mnist_train$X4)

ks <- c(1, 3, 5, 9, 15, 23, 51)
ps <- c(1, 2, 3, 7, 50)

accuracy_table <- matrix(NA, length(ks), length(ps))
length_accuracy_table <- length(accuracy_table)

accuracy <- function(valid, fit){
    sum(valid==fit)/length(valid)
}

for (k in 1:length(ks)){
    for (p in 1:length(ps)){
        knn <- kknn( X4 ~ . , train = mnist_train[1:3000,],
              test = mnist_train[3001:5999,], k = ks[k], distance = ps[p], kernel = "rectangular")
        accuracy_table[k, p] <- accuracy(mnist_train[3001:5999,"X4"], knn$fit)
        cat((k*(p-1)+p)/length_accuracy_table, "\t")
    }
}

which.max(accuracy_table)
mnist_test$X4 <- NA
names(mnist_test)[-257] <- names(mnist_train)[-1]
m <- kknn( X4 ~ . , train = mnist_train,
           test = mnist_test, k = 1, distance = 2, kernel = "rectangular")
write.csv(m$fit, "MNIST_predictions.csv", row.names=FALSE, col.names=FALSE)