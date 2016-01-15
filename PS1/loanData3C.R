library(mvtnorm)
library(ggplot2)

# create small wrapper functions
sigmaXY <- function(rho, sdX, sdY) {
  covTerm <- rho * sdX * sdY
  VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 
                     2, 2, byrow = TRUE)
  return(VCmatrix)
}

genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
  if(!is.na(seed)) set.seed(seed)
  rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
  return(rdraws)
}

# creating a function for all of this
loanData <- function(noApproved, noDenied, noUndecided,
                     muApproved, muDenied, muUndecided,
                     sdApproved, sdDenied, sdUndecided,
                     rhoApproved, rhoDenied, rhoUndecided, seed=1111) {
  sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
  sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
  sigmaUndecided <- sigmaXY(rho=rhoUndecided, sdX=sdUndecided[1], sdY=sdUndecided[2])
  approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
  denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
  undecided <- genBVN(noUndecided, muUndecided, sigmaUndecided, seed = seed+1)
  loanDf <- as.data.frame(rbind(approved,denied,undecided))
  deny <- c(rep("Approved", noApproved), rep("Denied", noDenied), rep("Undecided", noUndecided))
  targetApproved = c(rep(1, noApproved), rep(0, noDenied), rep(0, noUndecided))
  targetDenied = c(rep(0, noApproved), rep(1, noDenied), rep(0, noUndecided))
  targetUndecided = c(rep(0, noApproved), rep(0, noDenied), rep(1, noUndecided))
  loanDf <- data.frame(loanDf, deny, targetApproved, targetDenied, targetUndecided)
  colnames(loanDf) <- c("PIratio", "solvency", "deny", "targetApproved", "targetDenied", "targetUndecided")
  return(loanDf)
}

loanDf <- loanData(noApproved=50, noDenied=50, noUndecided=25,
                   c(4, 150), c(10, 100), c(7, 170),
                   c(1,20), c(1,20), c(1, 20), -0.3, 0.7, 0.3, 16710)

# analytical solution
X <- as.matrix(cbind(ind=rep(1, nrow(loanDf)), 
                     loanDf[,c("PIratio", "solvency")]))
Y <- as.matrix(loanDf[,c("targetApproved", "targetDenied", "targetUndecided")])
weightsOptim <- solve(t(X)%*%X) %*% t(X) %*% Y

# compute predictions
predictions <- data.frame(X %*% weightsOptim)
names(predictions) <- c("predictionApproved", "predictionDenied", "predictionUndecided")
loanDf <- cbind(loanDf, predictions)

# classify according to the argmax criterion
loanDf$predictedLabels[loanDf$predictionApproved > loanDf$predictionDenied 
                       & loanDf$predictionApproved > loanDf$predictionUndecided] <- "Approved"
loanDf$predictedLabels[loanDf$predictionDenied > loanDf$predictionApproved 
                       & loanDf$predictionDenied > loanDf$predictionUndecided] <- "Denied"
loanDf$predictedLabels[loanDf$predictionUndecided > loanDf$predictionApproved 
                       & loanDf$predictionUndecided > loanDf$predictionDenied] <- "Undecided"

# classification algorithm performance
table(loanDf$deny, loanDf$predictedLabels)

#saving the dataframe
write.csv(loanDf, "./predictions.csv")

#calculating the intersection of the three planes
b <- -weightsOptim[1,]
A <- t(rbind(rep(-1, 3), weightsOptim[-1,]))
intersection <- solve(A, b)

lowsolvency <- 0.9*min(loanDf$solvency)
highsolvency <- 1.1*max(loanDf$solvency)
lowPIratio <- 0.9*min(loanDf$PIratio)
highPIratio <- 1.1*max(loanDf$PIratio)

PIratio_ontheline <- function(solvency, weights1, weights2){
  piratio <- ((weights1["solvency"]-weights2["solvency"])*solvency+weights1["ind"]-weights2["ind"])/(weights2["PIratio"]-weights1["PIratio"])
}

#this function calculates the prediction of a given column of weightsOptim
#for a given solvency and PIratio
prediction <- function(solvency, PIratio, weights){
  r <- weights["ind"]+weights["solvency"]*solvency+weights["PIratio"]*PIratio
}

pilowsolvAD <- PIratio_ontheline(lowsolvency, weightsOptim[,"targetApproved"], weightsOptim[,"targetDenied"])
pilowsolvAU <- PIratio_ontheline(lowsolvency, weightsOptim[,"targetApproved"], weightsOptim[,"targetUndecided"])
pilowsolvDU <- PIratio_ontheline(lowsolvency, weightsOptim[,"targetDenied"], weightsOptim[,"targetUndecided"])
pihighsolvAD <- PIratio_ontheline(highsolvency, weightsOptim[,"targetApproved"], weightsOptim[,"targetDenied"])
pihighsolvAU <- PIratio_ontheline(highsolvency, weightsOptim[,"targetApproved"], weightsOptim[,"targetUndecided"])
pihighsolvDU <- PIratio_ontheline(highsolvency, weightsOptim[,"targetDenied"], weightsOptim[,"targetUndecided"])

if( prediction(lowsolvency, pilowsolvAD, weightsOptim[,"targetApproved"]) > prediction(lowsolvency, pilowsolvAD, weightsOptim[,"targetUndecided"])){
  boundaryDf1 <- data.frame(solvency=c(intersection["solvency"], lowsolvency), PIratio=c(intersection["PIratio"], pilowsolvAD), deny=rep("boundaryAD",2) )
} else {
  boundaryDf1 <- data.frame(solvency=c(intersection["solvency"], highsolvency), PIratio=c(intersection["PIratio"], pihighsolvAD), deny=rep("boundaryAD",2) )
}

if( prediction(lowsolvency, pilowsolvAU, weightsOptim[,"targetUndecided"]) > prediction(lowsolvency, pilowsolvAU, weightsOptim[,"targetDenied"])){
  boundaryDf2 <- data.frame(solvency=c(intersection["solvency"], lowsolvency), PIratio=c(intersection["PIratio"], pilowsolvAU), deny=rep("boundaryAU",2) )
} else {
  boundaryDf2 <- data.frame(solvency=c(intersection["solvency"], highsolvency), PIratio=c(intersection["PIratio"], pihighsolvAU), deny=rep("boundaryAU",2) )
}

if( prediction(lowsolvency, pilowsolvDU, weightsOptim[,"targetDenied"]) > prediction(lowsolvency, pilowsolvDU, weightsOptim[,"targetApproved"])){
  boundaryDf3 <- data.frame(solvency=c(intersection["solvency"], lowsolvency), PIratio=c(intersection["PIratio"], pilowsolvDU), deny=rep("boundaryDU",2) )
} else {
  boundaryDf3 <- data.frame(solvency=c(intersection["solvency"], highsolvency), PIratio=c(intersection["PIratio"], pihighsolvDU), deny=rep("boundaryDU",2) )
}


pdf("./discFunction3C.pdf")
ggplot(data = loanDf, 
       aes(x = solvency, y = PIratio, colour=deny)) + 
  geom_point() +
  xlab("solvency") +
  ylab("PIratio") +
  theme_bw() +
  geom_line(data=boundaryDf1, colour="grey") +
  geom_line(data=boundaryDf2, colour="grey") +
  geom_line(data=boundaryDf3, colour="grey") +
  coord_cartesian(xlim = c(lowsolvency, highsolvency), ylim = c(lowPIratio, highPIratio))
dev.off()
