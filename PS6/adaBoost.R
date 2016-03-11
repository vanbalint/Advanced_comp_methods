# setwd("/media/balint/Storage/Tanulas/Advanced_Computational_Methods/PS6")
# setwd("D:/Tanulas/Advanced_Computational_Methods/PS6")

# install.packages("formula.tools")

#loading libraries
library(formula.tools)
library(rpart)
library(assertthat)

#function 'I' as in the handout
I <- function(formula, tree, data){
    r <- as.numeric(data[,get.vars(lhs(formula))] ==
                   predict(tree, data, type="class"))
    r[r==0] <- -1
    return(r)
}

#the function works with data where the classes are 1-s and 0-s or 1-s and -1-s
adaBoost <- function(formula, data, depth, noTrees){
    
    #checking inputs
    assert_that(is.data.frame(data))
    assert_that(is.scalar(depth))
    assert_that(is.scalar(noTrees))
    
    zeroorminusone <- levels(as.factor(data[,get.vars(lhs(formula))]))[1]
    data[data[,get.vars(lhs(formula))]==0,get.vars(lhs(formula))] <- -1
    
    #initialize
    environment(formula) <- environment()
    N <- nrow(data)
    weights <- rep(1/N, N)
    predictions <- matrix(NA, N, noTrees)
    alphas <- rep(NA, noTrees)
    final.classifier <- 0
    trees <- vector("list", noTrees)
    
    #core loop
    for (m in 1:noTrees){
        cat(m, '\n')
        tree <- rpart(formula, data, weights, method="class", control = rpart.control(maxdepth = depth))
        trees[[m]] <- tree
        pred <- as.numeric(as.character(predict(tree, data, type="class")))
        predictions[,m] <- pred
        error <- sum(weights * I(formula, tree, data)) / sum(weights)
        alphas[m] <- log((1-error)/error)
        final.classifier <- final.classifier + alphas[m]*predictions[,m]
        weights <- weights * exp(alphas[m]*I(formula, tree, data))
    }
    
    #changing -1-s to 0-s in case it is needed
    if(zeroorminusone == "-1"){
        return(list(predLabels = sign(-final.classifier), alphas = alphas, trees = trees, formula = formula))
    }
    if(zeroorminusone == "0"){
        ret <- sign(-final.classifier)
        ret[ret==-1] <- 0
        return(list(predLabels = ret, alphas = alphas, trees = trees, formula = formula))
    }
    
}