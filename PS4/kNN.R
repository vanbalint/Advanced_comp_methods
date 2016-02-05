# specify parameters
#     k <- 1  # odd number
#     p <- 2  # Manhattan (1), Euclidean (2) or Chebyshev (Inf)

kNN <- function(features, labels, memory = NULL, 
                k = 1, p = 2, type="train") {
    
    # test the inputs
    library(assertthat)
    library(dplyr)
    not_empty(features); not_empty(labels); 
    if (type == "train") {
        assert_that(nrow(features) == length(labels))
    }
    is.string(type); assert_that(type %in% c("train", "predict"))
    is.count(k); 
    assert_that(p %in% c(1, 2, Inf))
    if (type == "predict") {
        assert_that(not_empty(memory) & 
                        ncol(memory) == ncol(features) & 
                        nrow(memory) == length(labels))
    }
    
    # Compute the distance between each point and all others 
    noObs <- nrow(features)
    labels <- as.factor(labels)
    noLabels <- length(levels(labels))
    # if we are making predictions on the test set based on the memory, 
    # we compute distances between each test observation and observations
    # in our memory
    if (type == "train") {
        distMatrix <- matrix(NA, noObs, noObs)
        for (obs in 1:noObs) {
            
            # getting the probe for the current observation
            probe <- as.numeric(features[obs,])
            probeExpanded <- matrix(probe, nrow = noObs, ncol = 2, 
                                    byrow = TRUE)
            
            # computing distances between the probe and exemplars in the
            # training X
            if (p %in% c(1,2)) {
                distMatrix[obs, ] <- (rowSums((abs(features - 
                                                       probeExpanded))^p) )^(1/p)
            } else if (p==Inf) {
                distMatrix[obs, ] <- apply(abs(features - probeExpanded), 1, max)
            }  
        }
    } else if (type == "predict") {
        noMemory <- nrow(memory)
        distMatrix <- matrix(NA, noObs, noMemory)
        for (obs in 1:noObs) {
            
            # getting the probe for the current observation
            probe <- as.numeric(features[obs,])
            probeExpanded <- matrix(probe, nrow = noMemory, ncol = 2, 
                                    byrow = TRUE)
            
            # computing distances between the probe and exemplars in the memory
            if (p %in% c(1,2)) {
                distMatrix[obs, ] <- (rowSums((abs(memory - 
                                                       probeExpanded))^p) )^(1/p)
            } else if (p==Inf) {
                distMatrix[obs, ] <- apply(abs(memory - probeExpanded), 1, max)
            }  
        }
    }
    
    # Sort the distances in increasing numerical order and pick the first 
    # k elements
    neighbors <- apply(distMatrix, 1, order) %>% t()

    # the most frequent class in the k nearest neighbors and predicted label
    predLabels <- rep(NA, noObs)
    prob <- matrix(NA, noObs, noLabels)
    for (obs in 1:noObs) {
        for(label in 1:noLabels){
            prob[obs, label] <- sum(labels[neighbors[obs, 1:k]]==levels(labels)[label])/k
        }
        predLabels[obs] <- levels(labels)[ which.max( prob[obs,] ) ]
    }
    
    return(list(prob=prob, predLabels=predLabels))
    
}