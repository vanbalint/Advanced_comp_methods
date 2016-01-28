shinyUI(
  
    pageWithSidebar(
    
        headerPanel('Loans Data'),
    
        sidebarPanel(
                 
            numericInput('muApprovedx', 'Mean Solvency of Approved', 4),
            numericInput('muApprovedy', 'Mean PIratio of Approved', 150),
            numericInput('muDeniedx', 'Mean Solvency of Denied', 10),
            numericInput('muDeniedy', 'Mean PIratio of Denied', 100),
            numericInput('sdApprovedx', 'Standard Deviation of Solvency of Approved', 1),
            numericInput('sdApprovedy', 'Standard Deviation of PIratio of Approved', 20),
            numericInput('sdDeniedx', 'Standard Deviation of Solvency of Denied', 2),
            numericInput('sdDeniedy', 'Standard Deviation of PIratio of Denied', 30)
      
        ),
    
        mainPanel(
            tabsetPanel(
                tabPanel("Plot", plotOutput('plot1')),
                tabPanel("Confusion Matrix", tableOutput('table'))
            )
        )
    )
)