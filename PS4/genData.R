# ----
# Hrvoje's plain 2D XOR 
# ----

genXOR <- function(noObs=50, seed=1111, saveData=TRUE, savePlot=TRUE) {

    # load the required libraries
    library(assertthat)
    library(mvtnorm)
    library(ggplot2)

    # check the inputs
    assert_that(is.scalar(noObs) && is.double(noObs))
    assert_that(is.scalar(seed) && is.double(seed))
    assert_that(is.scalar(saveData) && is.logical(saveData))
    assert_that(is.scalar(savePlot) && is.logical(savePlot))
    
    # defining a function for generating bivariate normal data
    genBVN <- function(n = 1, muXY = c(0,1), sigmaXY = diag(2)) {
        rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
        return(rdraws)
    }

    # generate XOR data and add some simple names
    set.seed(seed)
    class1 <- rbind(genBVN(noObs, c(1,1), diag(2)),
                    genBVN(noObs, c(10,10), diag(2)) )
    class2 <- rbind(genBVN(noObs, c(1,10), diag(2)),
                    genBVN(noObs, c(10,1), diag(2)) )
    dataset <- rbind(cbind(class1, 0), cbind(class2, 1))
    dataset <- as.data.frame(dataset)
    colnames(dataset) <- c("x1", "x2", "y")

    # save the dataset in a CSV format
    if (saveData) {
        write.csv(dataset, file="dataset.csv", row.names = FALSE)
    }

    # save the plot 
    if (savePlot) {

        # generating the plot
        dataPlot <- 
            ggplot(data = dataset, 
                   aes(x = x1, y = x2, color = factor(y))) + 
            geom_point(size = 2, shape = 4) +
            xlab("Dimension 1") +
            ylab("Dimension 2") +
            theme_bw() +
            theme(text = element_text(family = "Helvetica")) +
            scale_color_manual("Class", 
                values = c("0" = "blue", "1" = "red"))
        
        # saving
        cairo_pdf("dataPlot.pdf")
        print(dataPlot) 
        dev.off()
    }
        
    return(dataset)
}


# ----
# Denitsa's spirals 
# ----

genSpirals <- function(N = 2000,
                    degrees = 570,
                    location = 90,
                    blend = 0.2,
                    saveData = TRUE, 
                    savePlot = TRUE) {
  
    # Generate two-spiral data 
    # idea of the problematic dataset: http://www.benmargolis.com/compsci/ai/two_spirals_problem.htm
    # N - number of observations
    # degrees - length of the spiral 
    # location - how far away from the origin
    # blend<-blending together

    #necessary packages
    if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
    
    # define some variables
    degrees2rad <- (2*pi)/360 #convert degrees to radiant
    location <- location*degrees2rad #how far away from 00 the spiral starts
    
    N1 <- floor(N/2)
    N2 <- N-N1
    
    #spiral 1 
    #we indicate it by 0 in V3
    n <- as.vector(location+sqrt(runif(N1))*degrees*degrees2rad)
    d1 <- t(rbind(-1*n*cos(n)+runif(N1)*blend, sin(n)*n+runif(N1)*blend, rep(0,N1)))
    
    #the second spiral we indicate by 1 in V3
    n <- as.vector(location+sqrt(runif(N1))*degrees*degrees2rad)
    d2 <-t(rbind(n*cos(n)+runif(N1)*blend, -1*sin(n)*n+runif(N1)*blend, rep(1,N1))) 
    
    #combine the data 
    data <- data.frame(rbind(d1, d2))
    names(data) <- c("x1", "x2", "y")

    #create pdf
    if (saveData) {
        write.csv(data, "dataset.csv", row.names = FALSE)
    }

    #create pdf plot
    if (savePlot) {
        cairo_pdf("dataPlot.pdf")
        print(
            ggplot(data = data, 
                   aes(x = x1, y = x2, colour=y)) + 
            scale_colour_continuous(guide = FALSE) +
            geom_point() +
            ggtitle("Spirals") +
            xlab("x1") +
            ylab("x2") +
            theme_bw()
        )
        dev.off()
    }
   
    return(data)
}



# ----
# Domagoj's 3D X problem
#-----

genX3D <- function(obsx=200, obsy=200, obsz=200, 
                   mux=c(2,2,2), 
                   rhox=0.9, 
                   sdx1=2, 
                   saveData=TRUE, 
                   savePlot=TRUE, 
                   seed=1234){
  
    library(mvtnorm)
    if (!require(scatterplot3d)) install.packages("scatterplot3d"); library(scatterplot3d)
    
    set.seed(seed)

    covTerm <- rhox * sdx1^2
    cvc <- matrix(c(sdx1^2,covTerm,covTerm,covTerm,sdx1^2,covTerm,covTerm,covTerm,sdx1^2),ncol = 3)
    x3d <- rmvnorm(obsx,mean = mux,sigma = cvc)

    cvc <- matrix(c(sdx1^2,-covTerm,-covTerm,-covTerm,sdx1^2,covTerm,-covTerm,covTerm,sdx1^2),ncol = 3)
    y3d <- rmvnorm(obsy,mean = mux,sigma = cvc)

    cvc <- matrix(c(sdx1^2,covTerm,-covTerm,covTerm,sdx1^2,-covTerm,-covTerm,-covTerm,sdx1^2),ncol = 3)
    z3d <- rmvnorm(obsz,mean = mux,sigma = cvc)
    
    lab3d <- c(rep(1,obsx),rep(2,obsy),rep(3,obsz))
    lab3da <- c(rep("a",obsx),rep("b",obsy),rep("c",obsz))
    
    dat3d <- data.frame(rbind(x3d,y3d,z3d), y=lab3d, label=lab3da)
    names(dat3d) <- c("x1", "x2", "x3", "y", "label")
    
    if(saveData==TRUE){
        write.csv(dat3d, file = "dataset.csv", row.names=FALSE)
    }
    
    if(savePlot==TRUE){
        cairo_pdf("dataPlot.pdf")
        s3d <- with(dat3d, scatterplot3d(X1, X2, X3, color = as.numeric(lab3d),
                                         col.axis = "blue",col.grid = "lightblue"))
        legend(s3d$xyz.convert(11, 0.7, 0.5), pch = 19, yjust=0,
               legend = levels(dat3d$lab3da), col = seq_along(levels(dat3d$lab3da)))
        dev.off() 
    }
    
    return(dat3d)
}



# ----
# Nicholas' waves
# ----

genWaves <- function(slice = 0.01, 
                     saveData = TRUE, 
                     savePlot = TRUE, 
                     seed = 12345) {

    library("ggplot2")

    get_data <- function(slice, seed = 12345){
        set.seed(seed)
        x <- seq(0,2*pi,slice)
        y <- runif(length(x)) + sin(x)
        z <- runif(length(x)) + cos(x)
        data <- data.frame(x1 = rep(x,2), x2 = c(y,z), 
                           y = c(rep(0,length(y)), rep(1,length(z)) ))
        return(data)
    }

    save_csv <- function(data){
        write.csv(data, file = "dataset.csv", row.names = FALSE)
    }

    get_pdf <- function(data){
        plot <- ggplot(data = data, aes(x=x1, y=x2, color=factor(y))) +
            geom_point() +
            theme_bw()
        cairo_pdf("dataPlot.pdf", family = "Arial", width = 5.5, height = 5)
        print(plot)
        dev.off()
    }

    data <- get_data(slice)
    if (saveData){
        save_csv(data)
    }
    if (savePlot){
        get_pdf(data)
    }
    return(data)
}



# ----
# Aimee - 3D XOR
# ----

genXor3D <- function(n = 100,
                    min.vertex = 1,
                    max.vertex = 2,
                    var = 0.05,
                    saveData = TRUE,
                    savePdf = TRUE) {
  

    if (!require('mvtnorm')) install.packages('mvtnorm')
    library(mvtnorm)
    if (!require('rgl')) install.packages('rgl')
    library(rgl)

    # Inspired by https://archive.ics.uci.edu/ml/datasets/Madelon
    # genData generates data from vertices of 3-d cube
    # with alternating classifications of 0 and 1
    #
    # Arguments:
    # `n`: (approximate) number of observations per vertex
    # `min.vertex` and `max.vertex`:
    #   - The lower corner of the cube is given by (min.vertex, min.vertex, min.vertex)
    #   - The upper corner of the cube is given by (max.vertex, max.vertex, max.vertex)
    # `var`: variance for each vertex
    # `write.csv`: boolean whether to save data points to `dataset.csv`, defaults to TRUE
    # `save.pdf`: boolean whether to save 3Dplot to pdf, defaults to TRUE
    #
    
    # shorthand
    minv <- min.vertex
    maxv <- max.vertex

    numCenters <- 4
    m <- 4 # 3 features + 1 output
    
    zeroes <- matrix(nrow = 0, ncol = m)
    zeroes.means <- matrix(c(minv,minv,minv,maxv,minv,maxv,minv,maxv,maxv,maxv,maxv,minv), ncol = numCenters)
    rdraws <- runif(numCenters*n)
    rdraws.bincount <- as.vector(table(cut(rdraws, b = numCenters)))
    for (idx in 1:length(rdraws.bincount)) {
        # Random draw of distribution
        bin.means <- zeroes.means[,idx]
        bin.count <- rdraws.bincount[idx]
        bin.zeroes <- rmvnorm(bin.count, mean = bin.means, sigma = var*diag(3))
        bin.zeroes <- cbind(bin.zeroes, rep(0, bin.count))
        zeroes <- rbind(zeroes, bin.zeroes)
    }
    
    ones <- matrix(nrow = 0, ncol = m)
    ones.means <- matrix(c(maxv,minv,minv,minv,minv,maxv,minv,maxv,minv,maxv,maxv,maxv), ncol = numCenters)
    rdraws <- runif(numCenters*n)
    rdraws.bincount <- as.vector(table(cut(rdraws, b = numCenters)))
    for (idx in 1:length(rdraws.bincount)) {
        # Random draw of distribution
        bin.means <- ones.means[,idx]
        bin.count <- rdraws.bincount[idx]
        bin.ones <- rmvnorm(bin.count, mean = bin.means, sigma = var*diag(3))
        bin.ones <- cbind(bin.ones, rep(1, bin.count))
        ones <- rbind(ones, bin.ones)
    }
    
    df <- data.frame(rbind(zeroes, ones))
    names(df) <- c("x1", "x2", "x3", "y")

    if (saveData) {
        write.csv(df, file = "dataset.csv", row.names = FALSE)
    }
    if (savePdf) {
        plot3d(df[,1:3], radius = 0.05, type='s', col = df[,4]+2)
        rgl.postscript("dataPlot.pdf","pdf")
    }
    df
}



# ----
# Balint's stick and snake
# ----

genStickSnake <- function(noStick=100, noSnake=100, 
                          min=0, max=100, 
                          gradient=1, amplitude=0.2, wavelength=500, 
                          saveData=TRUE, 
                          savePlot=TRUE) {

    #min, max and gradient are variables for both the stick and the snake
    #amplitude and wavelength are only for the snake
    #Be careful, because if you change the range, amplitude or wavelength,
    #the data sets can easily become ugly.
    
    #calculating the stick
    x <- runif(noStick, min, max)
    y <- gradient*x + rnorm(noStick)
    stick <- data.frame(x, y)
    
    #calculating the snake
    x <- runif(noSnake, min, max)
    y <- gradient*x + rnorm(noSnake) + (max-min)*amplitude*sin((max-min)/wavelength*x)
    snake <- data.frame(x, y)
    
    #joining the stick and the snake
    sas <- rbind(stick, snake)
    stick_or_snake <- c(rep("stick", noStick), rep("snake", noSnake))
    target <- c(rep(0, noStick), rep(1, noSnake))
    sas <- data.frame(sas, target, stick_or_snake)
    names(sas) <- c("x1", "x2", "y", "label")
    
    if(saveData){ 
        write.csv(sas, "dataset.csv", row.names = FALSE)
    }

    if(savePlot){
        plot <- ggplot(data = sas, aes(x=x1, y=x2, color=label)) +
            geom_point() +
            theme_bw()
        cairo_pdf("dataPlot.pdf")
        print(plot)
        dev.off()
    }
    
    return(sas)
}



# ----
# Felix' diagonal 
# ----

genDiagonal <- function(class1 =50, class2 =50, 
                        saveData = TRUE, savePlot=TRUE,
                        rho1 = 1, sdX1  = 1 , sdY1 = 1, 
                        mean1 = c(0,0), seed1 = 1, sigmaXY1 = diag(2),
                        rho2 = 1, sdX2  = 1 , sdY2 = 1, 
                        mean2 = c(15,15), seed2 = 2, sigmaXY2 = diag(2),
                        rho3 = 1, sdX3  = 1 , sdY3 = 1, 
                        mean3 = c(10,10), seed3 = 3, sigmaXY3 = diag(2),
                        rho4 = 1, sdX4  = 1 , sdY4 = 1, 
                        mean4 = c(20,20), seed4 = 4, sigmaXY4 = diag(2)) {

    # Author:       Felix Gutmann
    # Course:       15D012 - Advanced Computational Methods
    # Last update:  15.01.16
    # Content:      Problemset 1 - Exercise 1
    
    ### Load Packages 
    if (!require("mvtnorm")) install.packages("mvtnorm"); library(mvtnorm)
    if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)

    ### Initialize auxilliary function to generate mvn. data / Source: modifiaction from handout 1
    mvsamp <- function(rho, sdX, sdY,
                       n = 1,  mean=c(0,1), 
                       seed, sigmaXY=diag(2)) {
        covTerm  <- rho * sdX * sdY
        VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 2, 2, byrow = TRUE)
        set.seed(seed)
        draws    <- as.data.frame(rmvnorm(n, mean = mean, sigma = sigmaXY))
        colnames(draws) <- c("V1","V2")
        return(draws)
    }


    # Generate data according to specified parameters
    samp      <- cbind(mvsamp(rho1, sdX1, sdY1, class1, mean1, seed1, sigmaXY1),Category="CAT1")
    diag      <- cbind(mvsamp(rho2, sdX2, sdY2, class1, mean2, seed2, sigmaXY2),Category="CAT1")
    up        <- cbind(mvsamp(rho3, sdX3, sdY3, class2, mean3, seed3, sigmaXY3),Category="CAT2")
    right     <- cbind(mvsamp(rho4, sdX4, sdY4, class2, mean4, seed4, sigmaXY4),Category="CAT2")

    # Set up data set
    sim.dat   <- as.data.frame(rbind(samp,diag,up,right))
    names(sim.dat) <- c("x1", "x2", "label")
    sim.dat$y <- ifelse(sim.dat$label=="CAT1", 0, 1)
    
    # OPTION 1: Save data as CSV - TRUE by default
    if(saveData) { 
        write.csv(sim.dat, file = "dataset.csv", row.names=FALSE) 
    }

    # OPTION 2: Save plot of generated data - TRUE by default
    if(savePlot) {
        # Save ggplot graph
        p01 <- ggplot(data = sim.dat, 
                      aes( x = x1, y = x2, colour = label)) + 
               geom_point() +
               xlab("x1") +
               ylab("x2") +
               theme_bw()
        cairo_pdf("dataPlot.pdf")
        print(p01)
        dev.off()
    }
    return(sim.dat)
}



# ----
# Zsuzsa
# ----

genUrban = function(N = 1000, saveData=TRUE, savePlot=TRUE) {

    #Generates random points in a map (e.g. location of houses for sale) and adds a label whether
    #the point is in an urban or in a suburban area. Location and size of suburban areas are also 
    #randomly generated.
    
    set.seed(78696)
    if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
    
    #Max value of latitude and longitude - defines size of the whole area
    latitude_max = 1
    longitude_max = 1
    
    #Number ofsuburban areas
    n = 5
    
    genAreas = function(latitude_max = 1, 
                        longitude_max = 1, 
                        center_size=0.4, 
                        n = 5, 
                        ratio = 0.2) {

        #Generates the suburban areas' center and radius
        #input =  size of the whole area latitude, longitude, size of the center area, 
        #number of suburban areas, overall ratio of size of suburban area
        
        #center of the whole area
        center = c(latitude_max/2, longitude_max/2)
        #overall size of suburban area
        area = latitude_max * longitude_max *ratio
        #size and radius of suburbain areas
        areas = sort(runif(n,min=0, max=area))
        areas = sort(areas - c(0,areas[1:(n-1)]), decreasing=T)
        areas_r = (areas / pi)^(1/2)
        # location of suburban areas
        k=0
        #Initialize location matrix
        l = matrix(rep(NA,1*n),2,n)
        while (k<5) {
            l[1,k+1] = runif(1,min=0,max=latitude_max)
            l[2,k+1] = runif(1,min=0,max=longitude_max)
            #Check if suburban area center is outside the center of the city
            if ( (sum((l[,k+1] - center)^2))^(1/2) > center_size ) {
                #Check if suburban areas do not overlap each other
                distance = c()
                if (k!=0) {
                    for (i in 1:k) {
                        distance = c( distance , sum((l[,k+1] - l[,i])^2)^(1/2) )
                    }
                    if ( sum(distance > areas_r[1:k] + areas_r[k+1]) == k ) {
                        k=k+1
                    }
                } else {
                    k=k+1
                }
            }
        }
        return(list(center_suburban = l, r_suburban = areas_r))
    }
    suburban_areas = genAreas()
    
    #Function to compute distace of some points from a center
    dist <- function(points,center) {
        apply(points , 1 , function(x) sum((x - center)^2)^(1/2) )
    }
    
    #Generating random location data (e.g.: location of houses for sale)
    sample_latitude = runif(N,min=0,max=latitude_max)
    sample_longitude = runif(N, min=0, max=longitude_max)
    data = as.data.frame(cbind(sample_latitude, sample_longitude))
    
    #Initialize catgory variable - False if house is in urban area, True if in suburban
    suburban = rep(FALSE, dim(data)[1])

    #Check if house is in suburban area
    for (i in 1:n) {
        suburban = suburban | dist(data,suburban_areas$center_suburban[,i]) < suburban_areas$r_suburban[i]
    }
    
    #Add category
    data=cbind(data,suburban)

    #Add category labels
    data$suburban_label = rep("urban",nrow(data))
    data$suburban_label[data$suburban==TRUE] = "suburban"
    data$suburban <- data$suburban*1
    names(data) <- c("x1", "x2", "y", "label")

    #Save data
    if (saveData) {
        write.csv(data, file = "dataset.csv", row.names = FALSE)
    }
    
    #Saving plot
    if (savePlot) {
        cairo_pdf("dataPlot.pdf")
        print(
            ggplot(data = data,
                   aes(x = x1, y = x2, colour=label)) +
            geom_point() +
            xlab("latitude") +
            ylab("longitude")  + theme_bw() +
            scale_color_manual("Category", 
                values = c("urban" = "black", "suburban" = "green"))
        )
        dev.off()
    }
    return(data)
}



# ----
# Yiqun's square
# ---- 

genSquare <- function(n = 500, 
                      seed = 123, 
                      rangeX = c(0,1), 
                      rangeY = c(0,1), 
                      saveData = TRUE, 
                      savePlot = TRUE){

    if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)

    #perfect Bayes classifier(DGP)
    classify <- function(obs){
        x <- obs[1]
        y <- obs[2]
        
        if((x-1/4)^2+(y-1/4)^2 <= 1/36){
            lab <- 0
        } else if(x + y <=1){
            lab <-1
        } else{
            lab <- 0
        }
        
        return(lab)
    }


    if(!is.na(seed)) set.seed(seed)
    xdraw <- runif(n, min = rangeX[1], max = rangeX[2])
    ydraw <- runif(n, min = rangeY[1], max = rangeY[2])
    finalData <- data.frame(x = xdraw, y = ydraw)
    finalData$class <- factor(apply(finalData, 1, classify))
    names(finalData) <- c("x1", "x2", "y")
    
    if(saveData){
        write.csv(finalData, file = "dataset.csv", row.names=FALSE)
    }
  
    if(savePlot){
        plot <- 
            ggplot(finalData, aes(x = x1, y = x2, col = y)) +
            geom_point() +
            coord_fixed(ratio = 1) +
            theme_bw()
        cairo_pdf("dataPlot.pdf")
        print(plot)
        dev.off()
    }
  
  return(finalData)
}



# ----
# Miquel's sun
# ----

genSun <- function(n = 200, 
                   features = 2, 
                   seed = NA, mus = NULL, sigma = NULL,
                   saveData = TRUE, 
                   savePlot = TRUE) {

    ####################################################################
    # Inspired by function "mlbench.circle" from package "mlbench"
    # URL: https://cran.r-project.org/web/packages/mlbench/mlbench.pdf
    # (c) Miquel Torrens, 2016.01.11
    # n : number of observation for the dataset
    # features : number of features generated
    # seed : preferred seed
    # mus : means of the features
    # sigma : variance-covariance matrix of the features specified
    # saveData : TRUE if we want to save the dataset (in current working directory)
    # savePlot : TRUE if we want to save the plot (in current working directory)
    ####################################################################
  
    # Libraries
    if (! require(mvtnorm)) { stop('required package not installed: mvtnorm') }
    if (! require(ggplot2)) { stop('required package not installed: ggplot2') }

    # For simplicity we restrict to 2 or 3 dimensions (this can be relaxed)
    if (! as.numeric(features) %in% 2:3) {
        stop('argument "features" must be 2 or 3.')
    }

    # Default values
    if (! is.na(seed)) { set.seed(seed) }
    if (is.null(sigma)) { sigma <- diag(features) }
    if (is.null(mus)) { mus <- rep(0, features) }

    # Simulate points from a bivariate normal
    phi <- rmvnorm(n, mean = mus, sigma = sigma)

    # Decide which belong to each cluster
    rad <- (2 ** (features - 1) * gamma(1 + features / 2) /
           (pi ** (features / 2))) ** (1 / features)
    ones <- apply(phi, 1, function(x) { jitter(sum((x - mus) ** 2)) }) > rad ** 2
    #ones <- apply(phi, 1, function(x) { sum((x - mus) ** 2) }) > rad ** 2    
    category <- rep(0, length = n)
    category[ones] <- 1

    # Build the final data frame
    new.phi <- cbind.data.frame(phi, as.character(category))
    new.phi[, 3] <- as.factor(new.phi[, 3])
    colnames(new.phi) <- c("x1", "x2", 'y')

    # Save the data in a .csv file
    if (saveData) {
        write.csv(new.phi, file = 'dataset.csv', row.names = FALSE)
        cat('Saved file:', paste0(getwd(), '/dataset.csv'), '\n')
    }

    # Plot
    if (savePlot) {
        unlink('dataPlot.pdf')
        cairo_pdf('dataPlot.pdf')
        plot1 <- 
            ggplot(data = new.phi, aes(x = x1, y = x2,
                   colour = y, fill = y)) +
            geom_point() +
            xlab('x1') +
            ylab('x2') +
            theme_bw()
        print(plot1)
        dev.off()
        cat('Saved plot:', paste0(getwd(), '/dataPlot.pdf'), '\n')
    }

    # End
    return(new.phi)
}  # END OF SCRIPT

