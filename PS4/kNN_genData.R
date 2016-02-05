library(reshape2)
library(ggplot2)
source("genData.R")
source("kNN.R")
ss <- genStickSnake(saveData=FALSE, savePlot=FALSE)
ss.knn <- kNN(ss[,1:2], ss[,4], k = 3, p = 2)
ss$predLabels <- ss.knn$predLabels
ss$prob0 <- ss.knn$prob[,1]
ss$prob1 <- ss.knn$prob[,2]

write.csv(ss, "predictions.csv", row.names = FALSE)

x1 <- seq(min(ss$x1), max(ss$x1), length.out = 100)
x2 <- seq(min(ss$x2), max(ss$x2), length.out = 100)
cgrid <- expand.grid(x1=x1, x2=x2)
ss.knn2 <- kNN(cgrid, ss[,3], memory = ss[,1:2], k = 3, p = 2, type="predict")
cgrid$predLabels <- as.numeric(ss.knn2$predLabels)

cairo_pdf("plot.pdf")
ggplot(data = ss) +
    geom_point(aes(x = x1, y = x2, colour=label)) +
    ggtitle("Stick and Snake") +
    xlab("x1") +
    ylab("x2") +
    stat_contour(data = cgrid, aes(x = x1, y = x2, z=predLabels))
dev.off()
