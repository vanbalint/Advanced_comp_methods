library(ggplot2)
library(mvtnorm)

# Define server logic required to draw the plot
shinyServer(function(input, output, session) {
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
    
    # correlation is slightly negative
    sigmaXY(rho=-0.1, sdX=1, sdY=20)
    
    # highly positive
    sigmaXY(rho=0.8, sdX=2, sdY=30)
    
    # creating a function for all of this
    loanData <- function(noApproved, noDenied, muApproved, muDenied, sdApproved, 
                         sdDenied, rhoApproved, rhoDenied, seed=1111) {
        sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
        sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
        approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
        denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
        loanDf <- as.data.frame(rbind(approved,denied))
        deny <- c(rep("Approved", noApproved), rep("Denied", noDenied))
        target = c(rep(0, noApproved), rep(1, noDenied))
        loanDf <- data.frame(loanDf, deny, target)
        colnames(loanDf) <- c("PIratio", "solvency", "deny", "target")
        return(loanDf)
    }
  # Combine the selected variables into a new data frame
    selectedData <- reactive({
        loanData(100, 100, c(input$muApprovedy, input$muApprovedx),
            c(input$muDeniedy, input$muDeniedx), 
            c(input$sdApprovedy, input$sdApprovedx), 
            c(input$sdDeniedy, input$sdDeniedx), -0.1, 0.6, 1221)
    })
  
    datafit <- reactive({lm(target ~ solvency + PIratio + 1, data=selectedData())})
    weights <- reactive({coef(datafit())[c("solvency", "PIratio")]})
    bias <- reactive({coef(datafit())[1]})
    intercept <- reactive({(-bias() + 0.5)/weights()["PIratio"]})
    slope <- reactive({-(weights()["solvency"]/weights()["PIratio"])})

    output$plot1 <- renderPlot({
        ggplot(data = selectedData(), 
                aes(x = solvency, y = PIratio, colour=deny, fill=deny)) + 
            geom_point() +
            xlab("solvency") +
            ylab("PIratio") +
            theme_bw() +
            theme(text=element_text(family="Arial")) +
            geom_abline(intercept = intercept(), slope = slope())
  })
    predictedLabels <- reactive({ifelse(predict(datafit()) < 0.5, "Approved", "Denied")})
    output$table <-renderTable({
        table(selectedData()$deny, predictedLabels())
    })
  
})