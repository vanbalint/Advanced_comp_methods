setwd("/media/balint/Storage/Tanulas/Advanced_Computational_Methods/PS5")
library(assertthat)

# Loss functions definitions
# Each of these functions need a vector of probabilities
MissError <- function(prob) {
    assert_that(is.numeric(prob) & sum(prob)==1)
    MissError <- 1-max(prob)
    return(MissError) 
}

Gini <- function(prob) {
    assert_that(is.numeric(prob) & sum(prob)==1)
    Gini <- sum(prob*(1-prob))
    return(Gini)
}

CrossEntropy <- function(prob) {
    assert_that(is.numeric(prob) & sum(prob)==1)
    for(i in 1:length(prob)){
        if(prob[i]==0){
            prob[i] <-1
        }
    }
    CrossEntropy <- - sum(prob*log(prob))
    return(CrossEntropy)
}

# lets develop a function that will partition the input space, 
# cut the input space exhaustively, count errors for each cut and
# choose the best cut

findThreshold <- function(x, y, costFnc) {
    y <- as.factor(y)
    classes <- levels(as.factor(y))
    noClasses <- length(classes)
    noPoints <- length(x)
    losses <- rep(NA, noPoints-1)
    thresholds <- rep(NA, noPoints-1)
    splitLabels <- matrix(NA, ncol=2, nrow=noPoints-1)
    
    # we go sequentially over each point and cut between that point and the
    # closest neighbor
    for (idx in 1:(noPoints-1)) {
        
        # locate a potential threshold, a split between two points
        potThres <- mean(x[idx:(idx+1)])
        
        # check the classification error, when both sides, 
        # are classified with the most common label
#         predictedClasses <- rep(NA, noPoints)
#         predictedClasses[x < potThres] <- names(table(x[x < potThres]))[which.max(table(x[x < potThres]))]
#         predictedClasses[x > potThres] <- names(table(x[x > potThres]))[which.max(table(x[x > potThres]))]
        
        # loss of this split

        prob1 <- rep(NA, noClasses)
        prob2 <- rep(NA, noClasses)
        for(i in 1:noClasses){
            prob1[i] <- sum(y[x < potThres] == classes[i])/idx
            prob2[i] <- sum(y[x > potThres] == classes[i])/(noPoints-idx)
        }
        
        if(costFnc == "ME"){
            loss <- idx/noPoints*MissError(prob1)+(noPoints-idx)/noPoints*MissError(prob2)
        }
        
        if(costFnc == "Gini"){
            loss <- idx/noPoints*Gini(prob1)+(noPoints-idx)/noPoints*Gini(prob2)
        }
        
        if(costFnc == "Entropy"){
            loss <- idx/noPoints*CrossEntropy(prob1)+(noPoints-idx)/noPoints*CrossEntropy(prob2)
        }

#        misError <- mean(predictedClasses != y)
        
        # recording the accuracy, thresholds and labels of 
        # the splitted interval
        losses[idx] <- loss
        thresholds[idx] <- potThres
        splitLabels[idx,] <- c(names(table(x[x < potThres]))[which.max(table(x[x < potThres]))],
                               names(table(x[x > potThres]))[which.max(table(x[x > potThres]))])
    }
    # print(cbind(errors, thresholds, splitLabels))
    
    # next we find the minimum and the best threshold
    minLoss <- min(losses)
    bestThreshold <- thresholds[which(losses==minLoss)]
    # if more than 1 threshold has the same accuracy we choose one randomly
    bestThreshold <- sample(bestThreshold, 1)
    
    # what are the final labels of the best split?
    labels <- splitLabels[which(thresholds==bestThreshold),]
    # print(cbind(minError, bestThreshold, labels))
    
    return(list(thres = bestThreshold, 
                loss = minLoss, 
                labels = labels))
}


# next we develop the main function that will use findThreshold function
# to find split points locally but decide between them in a greedy way
# based on a global error

cTree <- function(X, Y, depth, minPoints = 1, costFnc = "Entropy") {
    
    # setting up the initial boundaries - whole interval, with each
    # iteration of k this will expand
    boundaries <- c(min(X)-1, max(X))
    
    # iterating for K times, i.e. we iteratively split input space in an 
    # empirically greedy way, keeping only the best split and adding it 
    # to the boundaries, we stop after doing this K times
    for (k in 1:depth) {
        
        # first we subset our input space according to the boundaries 
        # found so far
        intervals <- cut(X, boundaries, include.lowest = TRUE)
        noIntervals <- length(levels(intervals))
        
        # then we go over each subset and see what is the best splitting
        # point locally in this subset, using the findThreshold function
        thresholds <- rep(NA, noIntervals)
        errors <- rep(NA, noIntervals)
        splitLabels <- matrix(NA, ncol=2, nrow=noIntervals)
        for (iter in 1:noIntervals) {
            x <- X[intervals==levels(intervals)[iter]]
            y <- Y[intervals==levels(intervals)[iter]]
            # we skip if there are not enough points in the interval
            # nothing to split there
            if (length(y)>minPoints) {
                # find the local splitting point
                results <- findThreshold(x, y, costFnc)
                thresholds[iter] <- results$thres
                splitLabels[iter,] <- results$labels
                
                # add the potential threshold to our list of boundaries
                boundariesHH <- c(boundaries, abs(results$thres))
                boundariesHH <- sort(boundariesHH)
                
                # add the signs of the new threshold which indicates what 
                # is the label of the of newly splitted interval
                if (k==1) {
                    signsHH <- results$labels
                } else {
                    signsHH <- append(signs, results$labels[1], 
                                      after=which(boundariesHH==results$thres)-2)
                    signsHH[which(boundariesHH==results$thres)] <- 
                        results$labels[2]
                }
                
                # now we compute predictions with new boundaries based on the 
                # potential split
                predictedClasses <- cut(X, boundariesHH)
                levels(predictedClasses) <- signsHH 
                
                # we compute a global, overall error rate for this local
                # modification, we do not use the local error to evaluate 
                # the splitting point
                errors[iter] <- mean(predictedClasses != Y)
            }
        }
        
        # find the best threshold in this iteration, greedy strategy
        minError <- min(errors, na.rm=TRUE)
        bestThreshold <- thresholds[which(errors==minError)]
        bestThreshold <- sample(bestThreshold, 1)
        labels <- splitLabels[which(thresholds==bestThreshold),]
        
        # add the new threshold to our list of boundaries
        boundaries <- c(boundaries, abs(bestThreshold))
        boundaries <- sort(boundaries)
        
        # add the signs of the new threshold which indicates what is the label of the newly splitted interval
        if (k==1) {
            signs <- labels
        } else {
            signs <- append(signs, labels[1], 
                            after=which(boundaries==bestThreshold)-2)
            signs[which(boundaries==bestThreshold)] <- labels[2]
        }
    }
    
    # get the final predicted classes
    predictedClasses <- cut(X, boundaries)
    levels(predictedClasses) <- signs 
    
    # now we evaluate the final accuracy, after K iterations
    misError <- mean(predictedClasses != Y)
    
    
    return(list(predictedClasses = predictedClasses, 
                misError = misError,
                boundaries = boundaries,
                signs = signs))
}


